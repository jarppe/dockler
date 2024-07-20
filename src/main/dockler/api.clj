(ns dockler.api
  (:require [clojure.string :as str]
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
  ([] (system-info nil))
  ([opts]
   (-> (http/GET "/info" opts)
       (http/assert-status! #{200})
       :body)))


(defn system-version
  ([] (system-version nil))
  ([opts]
   (-> (http/GET "/version" opts)
       (http/assert-status! #{200})
       :body)))


;;
;; ================================================================================================
;; Images:
;; ================================================================================================
;;


; https://docs.docker.com/engine/api/v1.45/#tag/Image/operation/ImageList

(defn image-list
  ([] (image-list nil))
  ([query]
   (-> (http/GET "/images/json" {:query-params query})
       (http/assert-status! #{200})
       :body)))


; https://docs.docker.com/engine/api/v1.45/#tag/Image/operation/ImageInspect

(defn image-inspect [id]
  (-> (http/GET (str "/images/" id "/json"))
      (http/assert-status! #{200})
      :body))


; https://docs.docker.com/engine/api/v1.45/#tag/Image/operation/ImageCreate
; The API for this is bit weird. The content-type is `application/json`, but in reality
; the body contains multiple JSON objects separated by `\r\n` pair. Also, it's important
; to read all the objects before closing connection, otherwise the image pull is cancelled.

(defn image-pull
  ([image-tag] (image-pull image-tag nil))
  ([image-tag opts]
   (with-open [conn (or (-> opts :conn)
                        (-> opts :client (http/connect)))]
     (-> (http/POST "/images/create" {:query-params         {"fromImage" image-tag}
                                      :conn                 conn
                                      :multiple-json-objcts true})
         (http/assert-status! #{200})
         :body
         (doall)))))


;;
;; ================================================================================================
;; Networks:
;; ================================================================================================
;;


; https://docs.docker.com/engine/api/v1.45/#tag/Network/operation/NetworkList

(defn network-list
  ([] (network-list nil))
  ([filters]
   (-> (http/GET "/networks" (when (seq filters) {:query-params {:filters filters}}))
       (http/assert-status! #{200})
       :body)))


; https://docs.docker.com/engine/api/v1.45/#tag/Network/operation/NetworkDelete

(defn network-inspect
  ([id] (network-inspect id nil))
  ([id query]
   (-> (http/GET (str "/networks/" id) {:query-params query})
       (http/assert-status! #{200})
       :body)))


; https://docs.docker.com/engine/api/v1.45/#tag/Network/operation/NetworkDelete

(defn network-remove [id]
  (-> (http/DELETE (str "/networks/" id))
      (http/assert-status! #{204}))
  nil)


; https://docs.docker.com/engine/api/v1.46/#tag/Network/operation/NetworkPrune

(defn network-prune [filters]
  (-> (http/POST "/networks-prune" {:query-params {:filters filters}})
      (http/assert-status! #{200 404})
      :body))


; https://docs.docker.com/engine/api/v1.45/#tag/Network/operation/NetworkCreate

(defn network-create [body]
  (-> (http/POST "/networks/create" {:body body})
      (http/assert-status! #{201})
      :body
      :id))


; https://docs.docker.com/engine/api/v1.45/#tag/Network/operation/NetworkConnect

(defn network-connect-container
  ([network-id container-id] (network-connect-container network-id container-id nil))
  ([network-id container-id endpoint-config]
   (-> (http/POST (str "/networks/" network-id "/connect") {:body {:container       container-id
                                                                   :endpoint-config (or endpoint-config {})}})
       (http/assert-status! #{200})
       :body)))


;;
;; ================================================================================================
;; Containers:
;; ================================================================================================
;;


; https://docs.docker.com/engine/api/v1.45/#tag/Container/operation/ContainerList

(defn container-list
  ([] (container-list nil nil))
  ([filters] (container-list filters nil))
  ([filters all?]
   (-> (http/GET "/containers/json" {:query-params {:all     (if all? true false)
                                                    :filters filters}})
       (http/assert-status! #{200})
       :body)))


; https://docs.docker.com/engine/api/v1.45/#tag/Container/operation/ContainerInspect

(defn container-inspect
  ([id] (container-inspect id nil))
  ([id query]
   (-> (http/GET (str "/containers/" id "/json") {:query-params query})
       (http/assert-status! #{200})
       :body)))


; https://docs.docker.com/engine/api/v1.45/#tag/Container/operation/ContainerCreate
; Note: The Docker API takes container data in body except the `name` and `platform`.
;       The `name` and `platform` are given as query params. This API takes all values
;       in `body` argument.

(defn container-create
  ([body] (container-create body nil))
  ([body opts]
   (-> (http/POST "/containers/create" (-> opts
                                           (assoc :query-params (select-keys body [:name :platform]))
                                           (assoc :body (dissoc body :name :platform))))
       (http/assert-status! #{201})
       :body
       :id)))


; https://docs.docker.com/engine/api/v1.45/#tag/Container/operation/ContainerStart

(defn container-start [id]
  (-> (http/POST (str "/containers/" id "/start"))
      (http/assert-status! #{204 304})
      :status
      {204 :started
       304 :already-started}))


; https://docs.docker.com/engine/api/v1.45/#tag/Container/operation/ContainerStop

(defn container-stop
  ([id] (container-stop id nil))
  ([id query]
   (-> (http/POST (str "/containers/" id "/stop") {:query-params query})
       (http/assert-status! #{204 304})
       :status
       {204 :stopped
        304 :already-stopped})))


; https://docs.docker.com/engine/api/v1.45/#tag/Container/operation/ContainerRestart

(defn container-restart
  ([id] (container-restart id nil))
  ([id query]
   (-> (http/POST (str "/containers/" id "/restart") {:query-params query})
       (http/assert-status! #{204}))
   nil))


; https://docs.docker.com/engine/api/v1.45/#tag/Container/operation/ContainerKill

(defn container-kill
  ([id] (container-kill id nil))
  ([id query]
   (-> (http/POST (str "/containers/" id "/kill") {:query-params query})
       (http/assert-status! #{204}))
   nil))


; https://docs.docker.com/engine/api/v1.45/#tag/Container/operation/ContainerWait

(defn container-wait
  ([id] (container-wait id nil))
  ([id query]
   (-> (http/POST (str "/containers/" id "/wait") {:query-params query})
       (http/assert-status! #{200})
       :body)))


; https://docs.docker.com/engine/api/v1.45/#tag/Container/operation/ContainerDelete

(defn container-delete
  ([id] (container-delete id nil))
  ([id query]
   (-> (http/DELETE (str "/containers/" id) {:query-params query})
       (http/assert-status! #{204 409})
       :status
       {204 :ok
        409 :conflict})))


; https://docs.docker.com/engine/api/v1.46/#tag/Container/operation/ContainerPrune
;
; The `filters` must be a map with string keys and string sequences as vals.
; For example:
;   (container-prune {:label ["some.label" "some.other.label=test"]})
;

(defn container-prune [filters]
  (-> (http/POST "/containers/prune" {:query-params {:filters filters}})
      (http/assert-status! #{200})
      :body))


; https://docs.docker.com/engine/api/v1.45/#tag/Container/operation/ContainerAttach
;
; Attach to running container and return java.io.InputStream for stdout and/or stderr.
; Returns a closeable record with `:stdout` and `:stderr` keys. 
;

(defn container-attach
  (^java.io.Closeable [container-id] (container-attach container-id nil))
  (^java.io.Closeable [container-id opts]
   (-> (http/POST
         (str "/containers/" container-id "/attach")
         {:conn         (connect (:client opts))
          :query-params {:stream 1
                         :stdout (if (:stdout opts) true false)
                         :stderr (if (:stderr opts) true false)}
          :headers      {"content-type" "application/vnd.docker.raw-stream"
                         "accept"       "application/vnd.docker.multiplexed-stream"
                         "connection"   "Upgrade"
                         "upgrade"      "tcp"}})
       (http/assert-status! #{101})
       (stream/stream-resp opts))))


; https://docs.docker.com/engine/api/v1.45/#tag/Container/operation/ContainerArchiveInfo

(defn container-archive-info [id path]
  (-> (http/HEAD (str "/containers/" id "/archive") {:query-params {:path path}})
      (http/assert-status! #{200})
      :headers
      (get "x-docker-container-path-stat")
      (data/base64-decode)
      (data/json-parse)
      (data/go->clj)))


; https://docs.docker.com/engine/api/v1.46/#tag/Container/operation/ContainerChanges

(defn container-changes [id]
  (-> (http/GET (str "/containers/" id "/changes"))
      (http/assert-status! #{200})
      :body))


; https://docs.docker.com/engine/api/v1.45/#tag/Container/operation/ContainerArchive

(defn container-archive ^java.io.InputStream [id path]
  (-> (http/GET (str "/containers/" id "/archive") {:query-params {:path path}})
      (http/assert-status! #{200})
      (http/assert-content-type! "application/x-tar")
      :in))


; https://docs.docker.com/engine/api/v1.45/#tag/Container/operation/PutContainerArchive

(defn container-extract
  ([id path ^java.io.InputStream tar] (container-extract id path tar nil))
  ([id path ^java.io.InputStream tar opts]
   (-> (http/PUT
         (str "/containers/" id "/archive")
         {:query-params {"path"       path
                         "copyUIDGID" (:copy-uidgid opts false)}
          :headers      {"content-type"     "application/x-tar"
                         "content-encoding" (:content-encoding opts "identity")}
          :body         tar})
       (http/assert-status! #{200})
       :body)))


(comment

  ;; Create container:

  (container-create {:name        "test-1234"
                     :cmd         ["bash" "-c" "while true; do date; sleep 1; done"]
                     :image       "debian:12-slim"
                     :host-config {:init true}})
  ;; "6b4d6c50dff1e906a76795c51f19bee97943ee187d547fce2392829b0e344c28"

  ;; Is it running?

  (-> (container-inspect "test-1234")
      :state
      :running)
  ;; => false

  ;; Start it:

  (container-start "test-1234")
  ;; => :ok

  ;; Is it running now?

  (-> (container-inspect "test-1234")
      :state
      :running)
  ;; => true

  ;; Attach listener that prints stdout to console:

  (require 'clojure.java.io)

  (with-open [conn (container-attach "test-1234" {:stdout true})]
    (-> (:stdout conn)
        (clojure.java.io/reader)
        (line-seq)
        (first)))
  ;; => "Tue Jun 25 11:42:56 UTC 2024"

  ;; List containers, note that the name starts with "/" for some reason:

  (->> (container-list)
       (filter (comp (partial = "/test-1234") first :names))
       (count))
  ;; => 1

  ;; Stop container:

  (container-stop "test-1234")
  ;; => :ok

  ;; Is it still running?

  (-> (container-inspect "test-1234")
      :state
      :running)
  ;; => false

  ;; List containers:

  (->> (container-list)
       (filter (comp (partial = "/test-1234") first :names))
       (count))
  ;; => 0

  ;; List all containers:

  (->> (container-list nil true)
       (filter (comp (partial = "/test-1234") first :names))
       (count))
  ;; => 1

  ;; Delete container:

  (container-delete "test-1234")
  ;; => :ok

  ;; Now it's not found anymore:

  (->> (container-list nil true)
       (filter (comp (partial = "/test-1234") first :names))
       (count))
  ;; => 0

  ;
  )


;;
;; ================================================================================================
;; Exec API:
;; ================================================================================================
;;


; https://docs.docker.com/engine/api/v1.45/#tag/Exec/operation/ContainerExec

(defn exec-create [id body]
  (-> (http/POST (str "/containers/" id "/exec") {:body body})
      (http/assert-status! #{201})
      :body
      :id))


; https://docs.docker.com/engine/api/v1.45/#tag/Exec/operation/ExecInspect

(defn exec-inspect [id]
  (-> (http/GET (str "/exec/" id "/json"))
      (http/assert-status! #{200})
      :body))


; https://docs.docker.com/engine/api/v1.45/#tag/Exec/operation/ExecStart

(defn exec-start
  (^java.io.Closeable [id] (exec-start id nil))
  (^java.io.Closeable [id opts]
   (-> (http/POST (str "/exec/" id "/start") {:conn    (connect (:client opts))
                                              :headers {"content-type" "application/vnd.docker.raw-stream"
                                                        "accept"       "application/vnd.docker.multiplexed-stream"
                                                        "connection"   "Upgrade"
                                                        "upgrade"      "tcp"}
                                              :body    {:detach false
                                                        :tty    false}})
       (http/assert-status! #{101})
       (stream/stream-resp opts))))


(defn exec-start-detach
  ([id] (exec-start-detach id nil))
  ([id opts]
   (-> (http/POST (str "/exec/" id "/start") {:conn    (connect (:client opts))
                                              :headers {"content-type" "application/vnd.docker.raw-stream"
                                                        "accept"       "application/vnd.docker.multiplexed-stream"
                                                        "connection"   "Upgrade"
                                                        "upgrade"      "tcp"}
                                              :body    {:detach true
                                                        :tty    (:tty opts false)}})
       (http/assert-status! #{200})
       :body)))


;; Helper to running a process in container by creating and starting an 
;; exec container:

(defn exec [id cmd]
  (let [exec-id   (exec-create id {:attach-stdin  false
                                   :attach-stdout true
                                   :attach-stderr true
                                   :cmd           (cond
                                                    (sequential? cmd) cmd
                                                    (string? cmd) (str/split cmd #"\s+")
                                                    :else (throw (ex-info "invalid cmd" {:cmd cmd})))})
        output    (with-open [resp (exec-start exec-id
                                               {:stdin  false
                                                :stdout true
                                                :stderr true})]
                    {:stdout (slurp (:stdout resp))
                     :stderr (slurp (:stderr resp))})
        exit-code (-> (exec-inspect exec-id)
                      :exit-code)]
    (assoc output :exit-code exit-code)))


(comment

  (def base-id (container-create {:name        "exec-base"
                                  :image       "debian:12-slim"
                                  :cmd         ["/bin/sleep" "infinity"]
                                  :host-config {:init true}}))

  (container-start base-id)

  (def exec-id (exec-create base-id {:attach-stdin  false
                                     :attach-stdout true
                                     :attach-stderr false
                                     :cmd           ["/bin/uname" "-a"]}))

  (exec-inspect exec-id)

  (with-open [resp (exec-start exec-id
                               {:stdin  false
                                :stdout true
                                :stderr true})]
    (slurp (:stdout resp)))
  ; => "Linux 97bba021a41f 6.6.26-linuxkit #1 SMP Sat Apr 27 04:13:19 UTC 2024 aarch64 GNU/Linux\n"

  (exec base-id "date")
  ;; => {:stdout "Thu Jun 20 07:38:35 UTC 2024\n"
  ;;     :stderr ""
  ;;     :exit-code 0}
  ;

  (exec base-id "false")
  ;; => {:stdout "" 
  ;;     :stderr "" 
  ;;     :exit-code 1}


  (def network-id (network-create {:name     "docker-test-2"
                                   :internal true
                                   :labels   {"project" "docker-test"}}))

  (def ws (container-create {:name              "ws"
                             :image             "eclipse-temurin:21-jdk"
                             :working-dir       "/app"
                             :cmd               (str/split "jwebserver -b 0.0.0.0 -p 8111" #"\s+")
                             :labels            {"test.id" "base"}
                             :host-config       {:init        true
                                                 :auto-remove true}
                             :networking-config {"docker-test" {:aliases ["foo" "bar"]}}}))

  (container-extract ws "/app" (->))
  (container-start ws)

  (network-connect-container network-id ws {:aliases ["foo" "bar"]})

  (network-inspect network-id)

  ;
  )

