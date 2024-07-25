(ns dockler.http
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [cheshire.core :as json]
            [dockler.data :as d]
            [dockler.content-stream :as content])
  (:import (java.io InputStream
                    OutputStream)
           (java.net URLEncoder)
           (java.nio.charset StandardCharsets)
           (java.io InputStream
                    OutputStream)
           (java.net URI
                     UnixDomainSocketAddress
                     StandardProtocolFamily)
           (java.nio.channels Channel
                              ReadableByteChannel
                              WritableByteChannel
                              SocketChannel
                              Channels)))


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
    (with-meta
      (fn []
        (doto (SocketChannel/open StandardProtocolFamily/UNIX)
          (.connect address)
          (.finishConnect)))
      {:host "localhost"})))


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


(def default-client (create-client))


;;
;; Connection:
;;


(defrecord Connection [^InputStream in ^OutputStream out ^Channel ch client host]
  java.io.Closeable
  (close [_this]
    (try (.close in) (catch Exception _))
    (try (.close out) (catch Exception _))
    (try (.close ch) (catch Exception _))))


(defn connect ^Connection [client]
  (let [client (or client default-client)
        ch     (client)
        in     (-> (Channels/newInputStream ^ReadableByteChannel ch)
                   (java.io.BufferedInputStream.))
        out    (-> (Channels/newOutputStream ^WritableByteChannel ch)
                   (java.io.BufferedOutputStream.))
        host   (-> client (meta) :host)]
    (->Connection in out ch client host)))


(defn clone ^Connection [^Connection conn]
  (let [client (-> conn :client)]
    (connect client)))


;;
;; Writing HTTP request:
;;


;; Calva and Kondo don't understand ^byte/1 synatx yet.


(def ^:private ^"[B" SP (d/str->bytes " "))
(def ^:private ^"[B" CRLF (d/str->bytes "\r\n"))
(def ^:private ^"[B" QUESTION-MARK (d/str->bytes "?"))
(def ^:private ^"[B" AMPERSAND (d/str->bytes "&"))
(def ^:private ^"[B" EQUALS (d/str->bytes "="))
(def ^:private ^"[B" HTTP-1-1 (d/str->bytes "HTTP/1.1"))
(def ^:private ^"[B" HEADER-SEP (d/str->bytes ": "))

(def docker-api-version "1.46")
(def docker-api-prefix (str "/v" docker-api-version))


(defn- write-http-header ^OutputStream [^OutputStream out req]
  (doto out
    (.write (-> req :method (or :get) (name) (str/upper-case) (d/str->bytes)))
    (.write SP)
    (.write (->> req :uri (str docker-api-prefix) (d/str->bytes))))
  (when-let [query (-> req :query-params)]
    (.write out QUESTION-MARK)
    (loop [first          true
           [[k v] & more] query]
      (when-not first
        (.write out AMPERSAND))
      (.write out (-> (name k)
                      (URLEncoder/encode StandardCharsets/UTF_8)
                      (d/str->bytes)))
      (.write out EQUALS)
      (.write out (let [^String v (cond
                                    (nil? v) ""
                                    (string? v) v
                                    (number? v) (str v)
                                    (keyword? v) (name v)
                                    :else (json/generate-string v))]
                    (-> (URLEncoder/encode v StandardCharsets/UTF_8)
                        (d/str->bytes))))
      (when (seq more)
        (recur false more))))
  (doto out
    (.write SP)
    (.write HTTP-1-1)
    (.write CRLF))
  (doseq [[k v] (-> req :headers)]
    (doto out
      (.write (-> k (d/str->bytes)))
      (.write HEADER-SEP)
      (.write (-> v (str) (d/str->bytes)))
      (.write CRLF)))
  (.write out CRLF)
  out)


(defn write-request [req ^OutputStream out]
  (let [body  (:body req)
        json? (or (sequential? body) (map? body) (set? body))]
    (write-http-header out (cond-> req
                             body  (update :headers assoc "transfer-encoding" "chunked")
                             json? (update :headers assoc "content-type" "application/json; charset=utf-8")))
    (when body
      (with-open [chunked (content/chunked-output-stream out)]
        (cond
          json? (let [writer (-> (java.io.OutputStreamWriter. chunked)
                                 (java.io.BufferedWriter.))]
                  (-> (d/clj->go body)
                      (json/generate-stream writer)
                      (.flush)))
          (instance? InputStream body) (io/copy body chunked)
          (string? body) (.write chunked (d/str->bytes body))
          (bytes? body) (.write chunked ^bytes body)
          :else (throw (ex-info (str "unsupported body type: " (type body)) {:body body})))))
    (.flush out)
    req))


;;
;; Reading HTTP response:
;;


;;
;; NOTE: Weird behaviour from Docker daemon HTTP API
;;
;; Sometimes the docker daemon returns an extra five bytes at the end of chunked body. 
;; You can verify this with curl:
;;
;;    $ curl -v --unix-socket /var/run/docker.sock http://localhost/v1.46/version
;;    ...normal curl output...
;;    * Leftovers after chunking: 5 bytes 
;;
;; The `Leftovers after chunking: 5 bytes` message denotes that the response had a body with 
;; valid chunked encoding content, but after the valid content the response also contained 5 
;; extra bytes. The extra bytes are always the same: `0\r\n\r\n`. It looks as the daemon 
;; sends empty content encoded as chunked (empty content is encoded as `0\r\n\r\n`).
;;
;; When reading a response from input this implementation tolerates the possible extra bytes.
;; If the first line is `0\r\n` it is assumed that the input has the extra bytes. The line and
;; the following empty line are discarded.
;;


(defn- read-resp-line ^String [^InputStream in]
  (let [sb (StringBuilder.)]
    (loop [c (.read in)]
      (if (= c (int \return))
        (do (.read in)
            (.toString sb))
        (do (.append sb (char c))
            (recur (.read in)))))))


(defn- read-http-header [^InputStream in]
  (let [status-line (read-resp-line in)
        ;; If we get extra end-of-chunk-stream marker, discard it and read 
        ;; status line again: 
        status-line (if (= status-line "0")
                      (do (.read in)
                          (.read in)
                          (read-resp-line in))
                      status-line)
        [_ status]  (re-matches #"HTTP/1.1 (\d+) .*" status-line)]
    [(parse-long status)
     (loop [headers (transient {})
            line    (read-resp-line in)]
       (if (= line "")
         (persistent! headers)
         (let [[_ k v] (re-matches #"([^:]+):\s+(.*)" line)]
           (recur (assoc! headers (str/lower-case k) v)
                  (read-resp-line in)))))]))


(defn- read-json-object [^java.io.InputStream in]
  (-> (java.io.InputStreamReader. in StandardCharsets/UTF_8)
      (json/parse-stream-strict)
      (d/go->clj)))


(defn- read-multiple-json-objects [^java.io.InputStream in]
  (->> (java.io.InputStreamReader. in StandardCharsets/UTF_8)
       (java.io.BufferedReader.)
       (line-seq)
       (mapv (comp d/go->clj json/parse-string))))


(defn read-response [req ^InputStream in]
  (let [[status headers] (read-http-header in)
        content-type     (some-> headers (get "content-type") (str/split #";") (first))
        content-length   (-> headers (get "content-length" "0") (parse-long))
        gzip?            (-> headers (get "content-encoding") (= "gzip"))
        chunked?         (-> headers (get "transfer-encoding") (= "chunked"))
        in               (if chunked?
                           (content/chunked-input-stream in)
                           (content/content-length-input-stream in content-length))
        in               (if gzip?
                           (java.util.zip.GZIPInputStream. in)
                           in)
        body             (when (not= status 101)
                           (case content-type
                             "application/json" (if (-> req :multiple-json-objcts)
                                                  (read-multiple-json-objects in)
                                                  (read-json-object in))
                             "text/plain"       (slurp in)
                             (.readAllBytes ^InputStream in)))]
    (with-meta
      {:status  status
       :headers headers
       :body    body}
      {:req req})))


;;
;; Execute request:
;;


(defn request [conn req]
  (-> req
      (assoc :conn conn)
      (update :headers assoc "host" (:host conn))
      (write-request (:out conn))
      (read-response (:in conn))))


;;
;; Simple requests:
;;


(defn- simple-request [conn method path opts]
  (let [conn' (or conn (connect nil))]
    (try
      (request conn' (assoc opts
                            :method method
                            :uri path))
      (finally
        (when-not conn
          (.close ^java.io.Closeable conn'))))))


;;
;; HTTP methods:
;;


(defn GET
  ([conn path] (GET conn path nil))
  ([conn path opts] (simple-request conn :get path opts)))


(defn POST
  ([conn path] (POST conn path nil))
  ([conn path opts] (simple-request conn :post path opts)))


(defn PUT
  ([conn path] (PUT conn path nil))
  ([conn path opts] (simple-request conn :put path opts)))


(defn DELETE
  ([conn path] (DELETE conn path nil))
  ([conn path opts] (simple-request conn :delete path opts)))


(defn HEAD
  ([conn path] (HEAD conn path nil))
  ([conn path opts] (simple-request conn :head path opts)))


;;
;; Response utilities:
;;


(defn- unexpected-status! [resp]
  (let [error (str "unexpectex status: " (-> resp :status))
        error (if-let [message (-> resp :body :message)]
                (str error ", message: " (pr-str message))
                error)]
    (throw (ex-info error {:resp resp}))))


(defn assert-status! [resp expected-status?]
  (when-not (-> resp :status (expected-status?))
    (unexpected-status! resp))
  resp)


(defn assert-content-type! [resp expected-content-type]
  (when-not (-> resp :headers (get "content-type") (= expected-content-type))
    (throw (ex-info (str "unexpectex content-type: " (-> resp :headers (get "content-type"))) {:resp resp})))
  resp)
