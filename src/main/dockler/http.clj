(ns dockler.http
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [cheshire.core :as ch]
            [dockler.data :as d]
            [dockler.limited-stream :as ls]
            [dockler.chunked :as c])
  (:import (java.io InputStream
                    OutputStream)
           (java.net URLEncoder)
           (java.nio.charset StandardCharsets)))


(set! *warn-on-reflection* true)


;; Calva and Kondo don't understand ^byte/1 synatx yet.


(def ^:private ^"[B" SP (d/str->bytes " "))
(def ^:private ^"[B" CRLF (d/str->bytes "\r\n"))
(def ^:private ^"[B" QUESTION-MARK (d/str->bytes "?"))
(def ^:private ^"[B" AMPERSAND (d/str->bytes "&"))
(def ^:private ^"[B" EQUALS (d/str->bytes "="))
(def ^:private ^"[B" HTTP-1-1 (d/str->bytes "HTTP/1.1"))
(def ^:private ^"[B" HEADER-SEP (d/str->bytes ": "))


;;
;; Writing HTTP request:
;;


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
                                    :else (ch/generate-string v))]
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
  (let [body (:body req)]
    (write-http-header out (cond-> req
                             (some? body)
                             (update :headers assoc "transfer-encoding" "chunked")

                             (or (sequential? body)
                                 (map? body))
                             (update :headers assoc "content-type" "application/json; charset=utf-8")

                             (-> req :headers (get "host") (nil?))
                             (update :headers assoc "host" (:host req))))
    (when body
      (with-open [out (c/chunked-output-stream out)]
        (cond
          (instance? InputStream body) (io/copy body out)
          (string? body) (.write out (d/str->bytes body))
          (bytes? body) (.write out ^bytes body)
          :else (let [writer (java.io.OutputStreamWriter. out)]
                  (-> (d/clj->go body)
                      (ch/generate-stream writer))
                  (.flush writer)))))
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

(defn read-response [^InputStream in]
  (let [[status headers] (read-http-header in)
        content-type     (some-> headers (get "content-type") (str/split #";") (first))
        content-length   (-> headers (get "content-length" "0") (parse-long))
        gzip?            (-> headers (get "content-encoding") (= "gzip"))
        chunked?         (-> headers (get "transfer-encoding") (= "chunked"))
        in               (if chunked?
                           (c/chunked-input-stream in)
                           (ls/content-length-input-stream in content-length))
        in               (if gzip?
                           (java.util.zip.GZIPInputStream. in)
                           in)
        body             (case content-type
                           "application/octet-stream" (.readAllBytes ^InputStream in)
                           "application/json"         (-> (java.io.InputStreamReader. ^InputStream in StandardCharsets/UTF_8)
                                                          (ch/parse-stream)
                                                          (d/go->clj))
                           "text/plain"               (slurp in)
                           (let [body (.readAllBytes ^InputStream in)]
                             (when (> (alength body) 0)
                               body)))]
    {:status  status
     :headers headers
     :body    body}))
