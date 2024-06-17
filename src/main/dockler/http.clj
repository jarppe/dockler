(ns dockler.http
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.walk :as walk]
            [jsonista.core :as json])
  (:import (java.util.concurrent.atomic AtomicLong)
           (java.io InputStream
                    OutputStream)
           (java.net URI
                     URLEncoder
                     URLDecoder
                     UnixDomainSocketAddress
                     StandardProtocolFamily)
           (java.nio ByteBuffer)
           (java.nio.channels Channel
                              ByteChannel
                              ReadableByteChannel
                              WritableByteChannel
                              SocketChannel
                              Channels)
           (java.nio.charset StandardCharsets)))


(set! *warn-on-reflection* true)


;;
;; Client:
;;


(defmulti create-client-impl URI/.getScheme)


(defmethod create-client-impl :default [uri]
  (throw (ex-info (str "unsupported uri: " (str uri)) {:uri uri})))


(defmethod create-client-impl "unix" [^URI uri]
  (let [sock    (.getPath uri)
        address (UnixDomainSocketAddress/of sock)]
    (fn []
      (doto (SocketChannel/open StandardProtocolFamily/UNIX)
        (.connect address)
        (.finishConnect)))))


(defn create-client
  ([] (create-client nil))
  ([uri]
   (let [^URI uri (cond
                    (nil? uri) (URI. "unix:///var/run/docker.sock")
                    (instance? URI uri) uri
                    (string? uri) (URI. uri)
                    :else (throw (ex-info (str "unsupported uri: " uri) {:uri uri})))]
     (-> (create-client-impl uri)
         (vary-meta assoc ::client true)))))


(defn client? [v]
  (-> v (meta) ::client))


;;
;; Connection:
;;


(defrecord Connection [^InputStream in ^OutputStream out ^Channel ch]
  java.io.Closeable
  (close [_this]
    (.close ch)))


(defn connect ^Connection [client-or-connection]
  (cond
    (instance? Connection client-or-connection) client-or-connection
    (client? client-or-connection) (let [ch  (client-or-connection)
                                         in  (-> (Channels/newInputStream ^ReadableByteChannel ch)
                                                 (java.io.BufferedInputStream.))
                                         out (-> (Channels/newOutputStream ^WritableByteChannel ch)
                                                 (java.io.BufferedOutputStream.))]
                                     (->Connection in out ch))
    :else (throw (ex-info "not a connection nor a client" {:client-or-connection client-or-connection}))))


;;
;; Mapping data from Go to Clj and back:
;;


(defn- clj->go-kw [k]
  (-> k
      (name)
      (str/replace #"^([^-])" (fn [[_ a]] (str/upper-case a)))
      (str/replace #"([^-])\-([^-])" (fn [[_ a b]] (str a (str/upper-case b))))
      (keyword)))


(defn- go->clj-kw [k]
  (when k
    (let [n (name k)]
      (if (str/includes? k ".")
        n
        (-> n
            (name)
            (str/replace #"^[A-Z]+$" str/lower-case)
            (str/replace #"^([^-])" (fn [[_ a]] (str/lower-case a)))
            (str/replace #"([a-z])([^a-z])" (fn [[_ a b]] (str a "-" (str/lower-case b))))
            (keyword))))))


(defn- clj->go [data]
  (walk/postwalk (fn [v]
                   (if (keyword? v)
                     (clj->go-kw v)
                     v))
                 data))


(defn- go->clj [data]
  (walk/postwalk (fn [v]
                   (if (keyword? v)
                     (go->clj-kw v)
                     v))
                 data))


;;
;; Writing HTTP request:
;;


;; Calva and Kondo don't understand ^byte/1 synatx yet.

(def ^:private ^"[B" EMPTY (byte-array 0))


(defn- str->bytes ^"[B" [^String v]
  (if v
    (.getBytes v StandardCharsets/UTF_8)
    EMPTY))


(def ^:private ^"[B" SP (str->bytes " "))
(def ^:private ^"[B" CRLF (str->bytes "\r\n"))
(def ^:private ^"[B" QUESTION-MARK (str->bytes "?"))
(def ^:private ^"[B" AMPERSAND (str->bytes "&"))
(def ^:private ^"[B" EQUALS (str->bytes "="))
(def ^:private ^"[B" HTTP-1-1 (str->bytes "HTTP/1.1"))
(def ^:private ^"[B" HEADER-SEP (str->bytes ": "))


(defn- write-http-header ^OutputStream [^OutputStream out req]
  (doto out
    (.write (-> req :method (or :get) (name) (str/upper-case) (str->bytes)))
    (.write SP)
    (.write (-> req :uri (str->bytes))))
  (when-let [query (-> req :query-params)]
    (.write out QUESTION-MARK)
    (loop [first          true
           [[k v] & more] query]
      (when-not first
        (.write out AMPERSAND))
      (.write out (-> (name k)
                      (URLEncoder/encode StandardCharsets/UTF_8)
                      (str->bytes)))
      (.write out EQUALS)
      (.write out (let [^String v (cond
                                    (nil? v) ""
                                    (string? v) v
                                    (number? v) (str v)
                                    (keyword? v) (name v)
                                    :else (json/write-value-as-string v))]
                    (-> (URLEncoder/encode v StandardCharsets/UTF_8)
                        (str->bytes))))
      (when (seq more)
        (recur false more))))
  (doto out
    (.write SP)
    (.write HTTP-1-1)
    (.write CRLF))
  (doseq [[k v] (-> req :headers)]
    (doto out
      (.write (-> k (str->bytes)))
      (.write HEADER-SEP)
      (.write (-> v (str) (str->bytes)))
      (.write CRLF)))
  (.write out CRLF)
  out)


;;
;; Reading HTTP response:
;;


(defn- read-resp-line ^String [^InputStream in]
  (let [sb (StringBuilder.)]
    (loop [c (.read in)]
      (if (= c (int \return))
        (do (.read in)
            (.toString sb))
        (do (.append sb (char c))
            (recur (.read in)))))))


(defn- read-http-header [in]
  (let [status-line (read-resp-line in)
        _           (println "header:" (pr-str status-line))
        [_ status]  (re-matches #"HTTP/1.1 (\d+) .*" status-line)]
    [(parse-long status)
     (loop [headers (transient {})
            line    (read-resp-line in)]
       (if (= line "")
         (persistent! headers)
         (let [[_ k v] (re-matches #"([^:]+):\s+(.*)" line)]
           (recur (assoc! headers (str/lower-case k) v)
                  (read-resp-line in)))))]))


(comment
  (let [in (java.io.ByteArrayInputStream. (.getBytes "ff\r\n"))]
    (loop [len 0]
      (let [c (.read in)]
        (println "c =" (char c) (int c))
        (if (= c (int \return))
          (do (.read in)
              len)
          (recur (+ (* len 16) (- (int c) (cond
                                            (<= (int \0) c (int \9)) (int \0)
                                            (<= (int \a) c (int \f)) (- (int \a) 10)
                                            (<= (int \A) c (int \F)) (- (int \A) 10))))))))))


(defn- chunked-input-stream ^InputStream [^InputStream in]
  (let [chunk-left   (volatile! 0)
        available    (fn []
                       (vswap! chunk-left (fn [chunk-left]
                                            (if (pos? chunk-left)
                                              chunk-left
                                              (loop [len 0]
                                                (let [c (.read in)]
                                                  (if (= c (int \return))
                                                    (do (.read in) ; Read the \n 
                                                        ; If len = 0 it means this was the last chunk, so read the end-of-chunk marker
                                                        ; and return 0 indicating the end:
                                                        (when (zero? len)
                                                          (.read in)
                                                          (.read in))
                                                        len)
                                                    (recur (int (+ (* len 16)
                                                                   (- c (cond
                                                                          (<= (int \0) c (int \9)) (int \0)
                                                                          (<= (int \a) c (int \f)) (- (int \a) 10)
                                                                          (<= (int \A) c (int \F)) (- (int \A) 10)))))))))))))
        update-chunk (fn [bytes-read]
                       (vswap! chunk-left
                               (fn [chunk-left c]
                                 (let [chunk-left (- chunk-left c)]
                                   (when (zero? chunk-left)
                                                ; Read the end-of-chunk \r \n
                                     (.read in)
                                     (.read in))
                                   chunk-left))
                               bytes-read))]
    (proxy [InputStream] []
      (read
        ([]
         (if (= (available) 0)
           -1
           (let [c (.read in)]
             (update-chunk 1)
             c)))
        ([^bytes b] (.read ^InputStream this b 0 (alength b)))
        ([^bytes b off len] (let [a (available)]
                              (if (= a 0)
                                -1
                                (let [len (min a len)
                                      c   (.read in b off len)]
                                  (update-chunk c)
                                  c))))))))


(defn- gunzip-input-stream ^InputStream [^InputStream in]
  (java.util.zip.GZIPInputStream. in))


(defn- content-length-input-stream ^InputStream [^InputStream in content-length]
  (let [available (volatile! content-length)]
    (proxy [InputStream] []
      (read
        ([] (if (= @available 0)
              -1
              (do (vswap! available dec)
                  (.read in))))
        ([^bytes b] (.read ^InputStream this b 0 (alength b)))
        ([^bytes b off len] (let [len (min @available len)]
                              (if (= len 0)
                                -1
                                (let [c (.read in b off len)]
                                  (vswap! available - c)
                                  c))))))))


(defn simple-request [client-or-connection req]
  (let [connection (connect client-or-connection)
        ^OutputStream out        (:out connection)
        ^InputStream  in         (:in connection)]
    (try
      (write-http-header out req)
      (when-let [body (:body req)]
        (cond
          (instance? InputStream body) (io/copy ^InputStream body out)
          (string? body) (.write out (str->bytes body))
          (bytes? body) (.write out ^"[B" body)
          :else (json/write-value out (clj->go body))))
      (.flush out)
      (let [[status headers] (read-http-header in)
            content-type     (-> headers (get "content-type" "application/octet-stream"))
            content-length   (-> headers (get "content-length" "0") (parse-long))
            gzip?            (-> headers (get "content-encoding") (case
                                                                   (nil "identity") false
                                                                   "gzip" true))
            chunked?         (-> headers (get "transfer-encoding") (= "chunked"))
            body             (when (or (> content-length 0) chunked?)
                               (let [in   (cond-> in
                                            (> content-length 0) (content-length-input-stream content-length)
                                            chunked? (chunked-input-stream)
                                            gzip? (gunzip-input-stream))
                                     bout (java.io.ByteArrayOutputStream. 4096)]
                                 (io/copy in bout)
                                 (.toByteArray bout)))]
        {:status  status
         :headers headers
         :body    (if (str/starts-with? content-type "application/json")
                    (-> body (json/read-value json/keyword-keys-object-mapper) (go->clj))
                    body)})
      (finally
        (when (client? client-or-connection)
          (.close ^java.io.Closeable connection))))))


(comment
  (def client (create-client))
  (simple-request client {:method  :get
                          :uri     "/version"
                          :headers {"host" "docker.com"}})
  (simple-request client {:method       :get
                          :uri          "/images/json"
                          :query-params {:filters {:label ["com.docker.compose.project=devcontainersdemo"]}}
                          :headers      {"host"            "docker.com"
                                         "accept-encoding" "gzip"}})

  ;
);; (doseq [data ["GET /images/json?filters=" (json/write-value-as-string
;;                                              {:label ["com.docker.compose.project=devcontainersdemo"]})
;;                 " HTTP/1.0\r\n"
;;                 "Host: docker.com\r\n"
;;                 "Accept: application/json\r\n"
;;                 "Content-Length: 0\r\n"
;;                 "\r\n"]]
;;     (.write out (.getBytes ^String data StandardCharsets/UTF_8)))
;;
; HTTP/1.0 400 Bad Request
; Api-Version: 1.45
; Content-Type: application/json
; Date: Sat, 15 Jun 2024 20:29:16 GMT
; Docker-Experimental: false
; Ostype: linux
; Server: Docker/26.1.1 (linux)
; 
; {"message":"invalid filter"}
