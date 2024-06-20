(ns dockler.client
  (:require [dockler.http :as http]
            [dockler.stream :as stream]
            [clojure.string :as str])
  (:import (java.io InputStream
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
    (fn
      ([]
       (doto (SocketChannel/open StandardProtocolFamily/UNIX)
         (.connect address)
         (.finishConnect)))
      ([param]
       (case param
         :host "docker.com")))))


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


(defrecord Connection [^InputStream in ^OutputStream out ^Channel ch host]
  java.io.Closeable
  (close [_this]
    (try (.close in) (catch Exception _))
    (try (.close out) (catch Exception _))
    (try (.close ch) (catch Exception _))))


(defn connect ^Connection [client-or-connection]
  (cond
    (instance? Connection client-or-connection) client-or-connection
    (client? client-or-connection) (let [ch  (client-or-connection)
                                         in  (-> (Channels/newInputStream ^ReadableByteChannel ch)
                                                 (java.io.BufferedInputStream.))
                                         out (-> (Channels/newOutputStream ^WritableByteChannel ch)
                                                 (java.io.BufferedOutputStream.))]
                                     (->Connection in out ch (client-or-connection :host)))
    (nil? client-or-connection) (connect (create-client))
    :else (throw (ex-info "not a connection nor a client" {:client-or-connection client-or-connection}))))


;;
;; Execute request:
;;


(defn request [req]
  (let [conn (or (:conn req)
                 (connect (:client req)))
        out  (:out conn)
        in   (:in conn)]
    (try
      (http/write-request req out)
      (merge (http/read-response in)
             {:in     in
              :out    out
              :conn   conn
              :client (:client req)})
      (finally
        (when-not (:conn req)
          (.close ^java.io.Closeable conn))))))


;;
;; Simple requests:
;;


(defn- simple-request [method path opts]
  (request (-> opts
               (assoc :method method
                      :uri path)
               (update :headers merge {"host"            "docker.com"
                                       "accept-encoding" "gzip"}))))

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


(def common-status {200 :ok
                    204 :ok
                    304 :not-modified})


(defn- unexpected! [resp]
  (let [error (str "unexpectex status: " (-> resp :status))
        error (if-let [message (and (-> resp :headers (get "content-type" "") (str/starts-with? "application/json"))
                                    (-> resp :body :message))]
                (str error ", message: " (pr-str message))
                error)]
    (throw (ex-info error {:resp resp}))))


(defn success-body!
  ([resp]
   (when-not (#{200 201 202 204} (:status resp))
     (unexpected! resp))
   (:body resp))
  ([resp expected]
   (let [status (:status resp)]
     (or (expected status)
         (unexpected! resp)))))


;;
;; Attach:
;;


(defn attach
  ([container-id] (attach container-id nil))
  ([container-id opts]
   (let [conn (connect (:client opts))
         resp (request {:conn         conn
                        :method       :post
                        :uri          (str "/containers/" container-id "/attach")
                        :query-params {:stream 1
                                       :stdout (if (:stdout opts) true false)
                                       :stderr (if (:stderr opts) true false)}
                        :headers      {"host"         "docker.com"
                                       "content-type" "application/vnd.docker.raw-stream"
                                       "connection"   "Upgrade"
                                       "upgrade"      "tcp"}})]
     (when (not= (:status resp) 101)
       (throw (ex-info (str "can't attach to container " (pr-str container-id))
                       {:container-id container-id
                        :status       (:status resp)})))
     (stream/stream-resp conn opts resp))))


;;
;; Exec:
;;


(defn exec-start [id body opts]
  (let [conn (connect (:client opts))
        resp (request {:conn    conn
                       :method  :post
                       :uri     (str "/exec/" id "/start")
                       :headers {"host"         "docker.com"
                                 "content-type" "application/vnd.docker.raw-stream"
                                 "connection"   "Upgrade"
                                 "upgrade"      "tcp"}
                       :body    body})]
    (when (not= (:status resp) 101)
      (throw (ex-info (str "can't start exec container " (pr-str id))
                      {:id     id
                       :status (:status resp)})))
    (stream/stream-resp conn opts resp)))

