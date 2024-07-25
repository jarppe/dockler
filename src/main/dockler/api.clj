(ns dockler.api
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [dockler.http :as http]
            [dockler.stream :as stream]
            [dockler.data :as data]))


;;
;; ================================================================================================
;; Client:
;; ================================================================================================
;;


(defn create-client
  ([] (create-client nil))
  ([uri] (http/create-client uri)))


;;
;; ================================================================================================
;; Connection:
;; ================================================================================================
;;


(defn connect
  (^java.io.Closeable [] (connect nil))
  (^java.io.Closeable [client]
   (http/connect client)))


;;
;; ================================================================================================
;; System:
;; ================================================================================================
;;


(defn system-info
  ([conn] (system-info conn nil))
  ([conn opts]
   (-> (http/GET conn "/info" opts)
       (http/assert-status! #{200})
       :body)))


(defn system-version
  ([conn] (system-version conn nil))
  ([conn opts]
   (-> (http/GET conn "/version" opts)
       (http/assert-status! #{200})
       :body)))

;;
;; ================================================================================================
;; Images:
;; ================================================================================================
;;


; https://docs.docker.com/engine/api/v1.46/#tag/Image/operation/ImageList

(defn image-list
  ([conn] (image-list conn nil))
  ([conn query]
   (-> (http/GET conn "/images/json" {:query-params query})
       (http/assert-status! #{200})
       :body)))


; https://docs.docker.com/engine/api/v1.46/#tag/Image/operation/ImageInspect

(defn image-inspect [conn id]
  (-> (http/GET conn (str "/images/" id "/json"))
      (http/assert-status! #{200})
      :body))


; https://docs.docker.com/engine/api/v1.46/#tag/Image/operation/ImageCreate
;
;   The API for this is bit weird. The content-type is `application/json`, but in reality
;   the body contains multiple JSON objects separated by `\r\n` pair. Also, it's important
;   to read all the objects before closing connection, otherwise the image pull operation
;   is cancelled.

(defn image-pull [conn image-tag]
  (-> (http/POST conn "/images/create" {:query-params         {"fromImage" image-tag}
                                        :multiple-json-objcts true})
      (http/assert-status! #{200})
      :body))


;;
;; ================================================================================================
;; Networks:
;; ================================================================================================
;;


; https://docs.docker.com/engine/api/v1.46/#tag/Network/operation/NetworkList

(defn network-list
  ([conn] (network-list conn nil))
  ([conn query]
   (-> (http/GET conn "/networks" {:query-params query})
       (http/assert-status! #{200})
       :body)))


; https://docs.docker.com/engine/api/v1.46/#tag/Network/operation/NetworkDelete

(defn network-inspect
  ([conn id] (network-inspect conn id nil))
  ([conn id query]
   (let [resp (-> (http/GET conn (str "/networks/" id) {:query-params query})
                  (http/assert-status! #{200 404}))]
     (when (= (:status resp) 200)
       (:body resp)))))


; https://docs.docker.com/engine/api/v1.46/#tag/Network/operation/NetworkDelete

(defn network-remove [conn id]
  (-> (http/DELETE conn (str "/networks/" id))
      (http/assert-status! #{204 404})
      :status
      (= 204)))


; https://docs.docker.com/engine/api/v1.46/#tag/Network/operation/NetworkPrune

(defn network-prune [conn filters]
  (-> (http/POST conn "/networks-prune" {:query-params {:filters filters}})
      (http/assert-status! #{200 404})
      :body))


; https://docs.docker.com/engine/api/v1.46/#tag/Network/operation/NetworkCreate

(defn network-create [conn body]
  (-> (http/POST conn "/networks/create" {:body body})
      (http/assert-status! #{201})
      :body
      :id))


; https://docs.docker.com/engine/api/v1.46/#tag/Network/operation/NetworkConnect

(defn network-connect-container
  ([conn network-id container-id] (network-connect-container conn network-id container-id nil))
  ([conn network-id container-id endpoint-config]
   (-> (http/POST conn (str "/networks/" network-id "/connect") {:body {:container       container-id
                                                                        :endpoint-config (or endpoint-config {})}})
       (http/assert-status! #{200})
       :body)))


;;
;; ================================================================================================
;; Containers:
;; ================================================================================================
;;


; https://docs.docker.com/engine/api/v1.46/#tag/Container/operation/ContainerList

(defn container-list
  ([conn] (container-list conn nil))
  ([conn query]
   (-> (http/GET conn "/containers/json" {:query-params query})
       (http/assert-status! #{200})
       :body)))


; https://docs.docker.com/engine/api/v1.46/#tag/Container/operation/ContainerInspect

(defn container-inspect
  ([conn id] (container-inspect conn id nil))
  ([conn id query]
   (-> (http/GET conn (str "/containers/" id "/json") {:query-params query})
       (http/assert-status! #{200})
       :body)))


; https://docs.docker.com/engine/api/v1.46/#tag/Container/operation/ContainerStart

(defn container-start [conn id]
  (-> (http/POST conn (str "/containers/" id "/start"))
      (http/assert-status! #{204 304})
      :status
      {204 :started
       304 :already-started}))


; https://docs.docker.com/engine/api/v1.46/#tag/Container/operation/ContainerCreate
;
; Note: The Docker API takes container data in body except the `name` and `platform` parameters.
;       The `name` and `platform` are given as query params. This API takes all values in `body` 
;       parameter.

(defn container-create [conn body]
  (-> (http/POST conn "/containers/create" {:query-params (select-keys body [:name :platform])
                                            :body         (dissoc body :name :platform)})
      (http/assert-status! #{201})
      :body
      :id))


; Create and start container:

(defn container-create-and-start [conn body]
  (let [id (container-create conn body)]
    (container-start conn id)
    id))


; https://docs.docker.com/engine/api/v1.46/#tag/Container/operation/ContainerStop

(defn container-stop
  ([conn id] (container-stop conn id nil))
  ([conn id query]
   (-> (http/POST conn (str "/containers/" id "/stop") {:query-params query})
       (http/assert-status! #{204 304})
       :status
       {204 :stopped
        304 :already-stopped})))


; https://docs.docker.com/engine/api/v1.46/#tag/Container/operation/ContainerRestart

(defn container-restart
  ([conn id] (container-restart conn id))
  ([conn id query]
   (-> (http/POST conn (str "/containers/" id "/restart") {:query-params query})
       (http/assert-status! #{204}))
   nil))


; https://docs.docker.com/engine/api/v1.46/#tag/Container/operation/ContainerKill

(defn container-kill
  ([conn id] (container-kill conn id nil))
  ([conn id query]
   (-> (http/POST conn (str "/containers/" id "/kill") {:query-params query})
       (http/assert-status! #{204}))
   nil))


; https://docs.docker.com/engine/api/v1.46/#tag/Container/operation/ContainerWait

(defn container-wait
  ([conn id] (container-wait conn id nil))
  ([conn id query]
   (-> (http/POST conn (str "/containers/" id "/wait") {:query-params query})
       (http/assert-status! #{200})
       :body)))


; https://docs.docker.com/engine/api/v1.46/#tag/Container/operation/ContainerDelete

(defn container-delete
  ([conn id] (container-delete conn id nil))
  ([conn id query]
   (-> (http/DELETE conn (str "/containers/" id) {:query-params query})
       (http/assert-status! #{204 409})
       :status
       {204 :ok
        409 :conflict})))


; https://docs.docker.com/engine/api/v1.46/#tag/Container/operation/ContainerPrune
;
; The `filters` must be a map with string keys and string sequences as vals.
; For example:
;   (container-prune {:filters {:label ["some.label" "some.other.label=test"]}})
;

(defn container-prune
  ([conn] (container-prune conn))
  ([conn query]
   (-> (http/POST conn "/containers/prune" {:query-params query})
       (http/assert-status! #{200})
       :body)))


; https://docs.docker.com/engine/api/v1.46/#tag/Container/operation/ContainerAttach
;
; Attach to running container and return java.io.InputStream for stdout and/or stderr.
; Returns a closeable record with `:stdin`, `:stdout` and `:stderr` keys. 
;
; Callers must close the returned record to avoid leaks.
;

(defn container-attach ^java.io.Closeable [conn container-id streams]
  (let [attach-conn (http/clone conn)]
    (-> (http/POST attach-conn (str "/containers/" container-id "/attach") {:query-params {:stream 1
                                                                                           :stdout (if (:stdout streams) true false)
                                                                                           :stderr (if (:stderr streams) true false)}
                                                                            :headers      {"content-type" "application/vnd.docker.raw-stream"
                                                                                           "accept"       "application/vnd.docker.multiplexed-stream"
                                                                                           "connection"   "Upgrade"
                                                                                           "upgrade"      "tcp"}})
        (http/assert-status! #{101})
        (stream/stream-resp streams))))


; https://docs.docker.com/engine/api/v1.46/#tag/Container/operation/ContainerArchiveInfo

(defn container-archive-info [conn id path]
  (-> (http/HEAD conn (str "/containers/" id "/archive") {:query-params {:path path}})
      (http/assert-status! #{200})
      :headers
      (get "x-docker-container-path-stat")
      (data/base64-decode)
      (data/json-parse)
      (data/go->clj)))


; https://docs.docker.com/engine/api/v1.46/#tag/Container/operation/ContainerChanges

(defn container-changes [conn id]
  (-> (http/GET conn (str "/containers/" id "/changes"))
      (http/assert-status! #{200})
      :body))


; https://docs.docker.com/engine/api/v1.46/#tag/Container/operation/ContainerArchive

(defn container-archive ^java.io.InputStream [conn id path]
  (-> (http/GET conn (str "/containers/" id "/archive") {:query-params {:path path}})
      (http/assert-status! #{200})
      (http/assert-content-type! "application/x-tar")
      :in))


; https://docs.docker.com/engine/api/v1.46/#tag/Container/operation/PutContainerArchive

(defn container-extract
  ([conn id path ^java.io.InputStream tar] (container-extract conn id path tar nil))
  ([conn id path ^java.io.InputStream tar opts]
   (-> (http/PUT conn (str "/containers/" id "/archive") {:query-params {"path"       path
                                                                         "copyUIDGID" (:copy-uidgid opts false)}
                                                          :headers      {"content-type"     "application/x-tar"
                                                                         "content-encoding" (:content-encoding opts "identity")}
                                                          :body         tar})
       (http/assert-status! #{200})
       :body)))


(comment

  ;; Create container:

  (with-open [conn (connect)]
    (container-create conn {:name        "test-1234"
                            :cmd         ["bash" "-c" "while true; do date; sleep 1; done"]
                            :image       "debian:12-slim"
                            :host-config {:init true}}))
  ;; "6b4d6c50dff1e906a76795c51f19bee97943ee187d547fce2392829b0e344c28"


  ;; Is it running?

  (with-open [conn (connect)]
    (-> (container-inspect conn "test-1234")
        :state
        :running))
  ;; => false

  ;; Start it:

  (with-open [conn (connect)]
    (container-start conn "test-1234"))
  ;; => :ok

  ;; Is it running now?

  (with-open [conn (connect)]
    (-> (container-inspect conn "test-1234")
        :state
        :running))
  ;; => true

  ;; Attach listener that prints stdout to console:

  (with-open [conn   (connect)
              attach (container-attach conn "test-1234" {:stdout true})]
    (-> (:stdout attach)
        (clojure.java.io/reader)
        (line-seq)
        (first)))
  ;; => "Tue Jun 25 11:42:56 UTC 2024"

  ;; List containers, note that the name starts with "/" for some reason:

  (with-open [conn (connect)]
    (->> (container-list conn)
         (filter (comp (partial = "/test-1234") first :names))
         (count)))
  ;; => 1

  ;; Stop container:

  (with-open [conn (connect)]
    (container-stop conn "test-1234"))
  ;; => :stopped

  ;; Is it still running?

  (with-open [conn (connect)]
    (-> (container-inspect conn "test-1234")
        :state
        :running))
  ;; => false

  ;; List containers:

  (with-open [conn (connect)]
    (->> (container-list conn)
         (filter (comp (partial = "/test-1234") first :names))
         (count)))
  ;; => 0

  ;; List all containers:

  (with-open [conn (connect)]
    (->> (container-list conn {:all true})
         (filter (comp (partial = "/test-1234") first :names))
         (count)))
  ;; => 1

  ;; Delete container:

  (with-open [conn (connect)]
    (container-delete conn "test-1234"))
  ;; => :ok

  ;; Now it's not found anymore:

  (with-open [conn (connect)]
    (->> (container-list conn {:all true})
         (filter (comp (partial = "/test-1234") first :names))
         (count)))
  ;; => 0

  ;
  )


;;
;; ================================================================================================
;; Exec API:
;; ================================================================================================
;;


; https://docs.docker.com/engine/api/v1.46/#tag/Exec/operation/ContainerExec

(defn exec-create [conn id body]
  (-> (http/POST conn (str "/containers/" id "/exec") {:body body})
      (http/assert-status! #{201})
      :body
      :id))


; https://docs.docker.com/engine/api/v1.46/#tag/Exec/operation/ExecInspect

(defn exec-inspect [conn id]
  (-> (http/GET conn (str "/exec/" id "/json"))
      (http/assert-status! #{200})
      :body))


; https://docs.docker.com/engine/api/v1.46/#tag/Exec/operation/ExecStart

(defn exec-start
  (^java.io.Closeable [conn id] (exec-start conn id nil nil))
  (^java.io.Closeable [conn id streams] (exec-start conn id streams nil))
  (^java.io.Closeable [conn id streams body]
   (let [exec-conn (http/clone conn)]
     (-> (http/POST exec-conn (str "/exec/" id "/start") {:headers {"content-type" "application/vnd.docker.raw-stream"
                                                                    "accept"       "application/vnd.docker.multiplexed-stream"
                                                                    "connection"   "Upgrade"
                                                                    "upgrade"      "tcp"}
                                                          :body    (merge {:detach false
                                                                           :tty    false}
                                                                          body)})
         (http/assert-status! #{101})
         (stream/stream-resp streams)))))

;; Helper to running a process in container by creating and starting an 
;; exec container:

(defn exec
  ([conn id cmd] (exec conn id cmd {:stdout true
                                    :stderr true}))
  ([conn id cmd streams]
   (let [exec-id   (exec-create conn id {:attach-stdin  (if (:stdin streams) true false)
                                         :attach-stdout (if (:stdout streams) true false)
                                         :attach-stderr (if (:stderr streams) true false)
                                         :cmd           (cond
                                                          (sequential? cmd) cmd
                                                          (string? cmd) (str/split cmd #"\s+")
                                                          :else (throw (ex-info "invalid cmd" {:cmd cmd})))})]
     (with-open [resp (exec-start conn exec-id streams)]
       (when-let [input (:stdin streams)]
         (io/copy input (:stdin resp)))
       {:stdout    (when-let [stdout (:stdout resp)]
                     (slurp stdout))
        :stderr    (when-let [stderr (:stderr resp)]
                     (slurp stderr))
        :exit-code (-> (exec-inspect conn exec-id)
                       :exit-code)}))))


(comment

  (def base-id (with-open [conn (connect)]
                 (container-create conn {:name        "exec-base"
                                         :image       "debian:12-slim"
                                         :cmd         ["/bin/sleep" "infinity"]
                                         :host-config {:init true}
                                         :start?      true})))


  (with-open [conn (connect)]
    (let [exec-id (exec-create conn base-id {:attach-stdin  false
                                             :attach-stdout true
                                             :attach-stderr false
                                             :cmd           ["/bin/uname" "-a"]})]
      (with-open [exec-output (exec-start conn
                                          exec-id
                                          {:stdin  false
                                           :stdout true
                                           :stderr false})]
        (-> exec-output :stdout (slurp)))))
  ; => "Linux 97bba021a41f 6.6.26-linuxkit #1 SMP Sat Apr 27 04:13:19 UTC 2024 aarch64 GNU/Linux\n"

  (with-open [conn (connect)]
    (-> (exec conn base-id "date")
        :stdout))
  ;; => "Mon Jul 22 15:08:47 UTC 2024\n"
  )

;;
;; ================================================================================================
;; Volumes API:
;; ================================================================================================
;;

; https://docs.docker.com/engine/api/v1.46/#tag/Volume/operation/VolumeList

(defn volume-list
  ([conn] (volume-list conn nil))
  ([conn query]
   (-> (http/GET conn "/volumes" {:query-params query})
       (http/assert-status! #{200})
       :body)))

; https://docs.docker.com/engine/api/v1.46/#tag/Volume/operation/VolumeCreate

(defn volume-create [conn body]
  (-> (http/POST conn "/volumes/create" {:body body})
      (http/assert-status! #{201})
      :body))

; https://docs.docker.com/engine/api/v1.46/#tag/Volume/operation/VolumeInspect

(defn volume-inspect [conn name]
  (-> (http/GET conn (str "/volumes/" name))
      (http/assert-status! #{200})
      :body))

; https://docs.docker.com/engine/api/v1.46/#tag/Volume/operation/VolumeDelete

(defn volume-delete
  ([conn name] (volume-delete conn name nil))
  ([conn name query]
   (-> (http/DELETE conn (str "/volumes/" name) {:query-params query})
       (http/assert-status! #{204}))
   nil))

(comment
  (volume-list nil)
  (volume-create nil {:name "test-volume"})
  (volume-list nil)
  (volume-inspect nil "test-volume")
  (volume-delete nil "test-volume")
  (volume-list nil))
