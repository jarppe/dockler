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


(defn- write-http-header ^OutputStream [^OutputStream out req]
  (doto out
    (.write (-> req :method (or :get) (name) (str/upper-case) (d/str->bytes)))
    (.write SP)
    (.write (-> req :uri (d/str->bytes))))
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


(defn- write-request [req ^OutputStream out]
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
        [_ status]  (re-matches #"HTTP/1.1 (\d+) .*" status-line)]
    [(parse-long status)
     (loop [headers (transient {})
            line    (read-resp-line in)]
       (if (= line "")
         (persistent! headers)
         (let [[_ k v] (re-matches #"([^:]+):\s+(.*)" line)]
           (recur (assoc! headers (str/lower-case k) v)
                  (read-resp-line in)))))]))


(defn read-response [_req ^InputStream in]
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
        body             (case content-type
                           "application/octet-stream" (.readAllBytes ^InputStream in)
                           "application/json"         (-> (java.io.InputStreamReader. ^InputStream in StandardCharsets/UTF_8)
                                                          (json/parse-stream)
                                                          (d/go->clj))
                           "text/plain"               (slurp in)
                           nil)]
    {:status  status
     :headers headers
     :body    body}))


;;
;; Execute request:
;;


(defn request [req]
  (let [client (or (:client req) default-client)
        conn   (or (:conn req) (connect client))
        in     (:in conn)
        out    (:out conn)]
    (try
      (-> req
          (update :headers assoc "host" (:host conn))
          (write-request out)
          (read-response in)
          (merge {:in   in
                  :out  out
                  :conn conn}))
      (finally
        (when-not (:conn req)
          (.close ^java.io.Closeable conn))))))


;;
;; Simple requests:
;;


(defn- simple-request [method path opts]
  (request (assoc opts
                  :method method
                  :uri path)))


;;
;; HTTP methods:
;;


(defn GET
  ([path] (GET path nil))
  ([path opts] (simple-request :get path opts)))


(defn POST
  ([path] (POST path nil))
  ([path opts] (simple-request :post path opts)))


(defn PUT
  ([path] (PUT path nil))
  ([path opts] (simple-request :put path opts)))


(defn DELETE
  ([path] (DELETE path nil))
  ([path opts] (simple-request :delete path opts)))


(defn HEAD
  ([path] (HEAD path nil))
  ([path opts] (simple-request :head path opts)))


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
