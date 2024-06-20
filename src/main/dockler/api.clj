(ns dockler.api
  (:require [dockler.client :as c]
            [clojure.string :as str]))


; https://docs.docker.com/engine/api/v1.45/#tag/Image/operation/ImageList

(defn image-list
  ([] (image-list nil))
  ([query] (-> (c/GET "/images/json" {:query-params query})
               (c/success-body!))))


; https://docs.docker.com/engine/api/v1.45/#tag/Image/operation/ImageInspect

(defn image-inspect [id] (-> (c/GET (str "/images/" id "/json"))
                             (c/success-body!)))


(comment
  (-> (image-list)
      (first))

  (image-list {:filters {:label ["com.docker.compose.project=devcontainersdemo"]}})

  (image-inspect (-> (image-list)
                     (first)
                     :id)))


; https://docs.docker.com/engine/api/v1.45/#tag/Network/operation/NetworkList

(defn network-list
  ([] (network-list nil))
  ([query] (-> (c/GET "/networks" {:query-params query})
               (c/success-body!))))


; https://docs.docker.com/engine/api/v1.45/#tag/Network/operation/NetworkDelete

(defn network-inspect
  ([id] (network-inspect id nil))
  ([id query] (-> (c/GET (str "/networks/" id) {:query-params query})
                  (c/success-body!))))


(comment
  (network-list)

  (network-list {:filters {:label ["com.docker.compose.project=devcontainersdemo"]}})

  (network-inspect (-> (network-list)
                       (first)
                       :id)))


; https://docs.docker.com/engine/api/v1.45/#tag/Network/operation/NetworkDelete

(defn network-remove [id] (-> (c/DELETE (str "/network/" id))
                              (c/success-body!)))


; https://docs.docker.com/engine/api/v1.45/#tag/Network/operation/NetworkCreate

(defn network-create [body] (-> (c/POST "/networks/create" {:body body})
                                (c/success-body!)))


(defn network-connect-container
  ([network-id container-id] (network-connect-container network-id container-id nil))
  ([network-id container-id endpoint-config]
   (-> (c/POST (str "/networks/" network-id "/connect") {:body {:container       container-id
                                                                :endpoint-config (or endpoint-config {})}})
       (c/success-body!))))

; https://docs.docker.com/engine/api/v1.45/#tag/Container/operation/ContainerList

(defn container-list
  ([] (container-list nil))
  ([query] (-> (c/GET "/containers/json" query)
               (c/success-body!))))


; https://docs.docker.com/engine/api/v1.45/#tag/Container/operation/ContainerInspect

(defn container-inspect
  ([id] (container-inspect id nil))
  ([id query] (-> (c/GET (str "/containers/" id "/json") {:query-params query})
                  (c/success-body!))))


(comment
  (container-list)
  (container-inspect (-> (container-list)
                         (first)
                         :id)))


; https://docs.docker.com/engine/api/v1.45/#tag/Container/operation/ContainerCreate
;
; Note: This implementation combines the query and body expected by 
;       the Docker API into `opts`

(defn container-create
  ([body] (container-create body nil))
  ([body opts]
   (-> (c/POST "/containers/create" (-> opts
                                        (assoc :query-params (select-keys opts [:name :platform]))
                                        (dissoc :name :platform)
                                        (assoc :body body)))
       (c/success-body!))))


; https://docs.docker.com/engine/api/v1.45/#tag/Container/operation/ContainerStart

(defn container-start [id] (-> (c/POST (str "/containers/" id "/start"))
                               (c/success-body! c/common-status)))


; https://docs.docker.com/engine/api/v1.45/#tag/Container/operation/ContainerStop

(defn container-stop
  ([id] (container-stop id nil))
  ([id query] (-> (c/POST (str "/containers/" id "/stop") {:query-params query})
                  (c/success-body! c/common-status))))


; https://docs.docker.com/engine/api/v1.45/#tag/Container/operation/ContainerRestart

(defn container-restart
  ([id] (container-restart id nil))
  ([id query] (-> (c/POST (str "/containers/" id "/restart") {:query-params query})
                  (c/success-body!))))


; https://docs.docker.com/engine/api/v1.45/#tag/Container/operation/ContainerKill

(defn container-kill
  ([id] (container-kill id nil))
  ([id query] (-> (c/POST (str "/containers/" id "/kill") {:query-params query})
                  (c/success-body!))))


; https://docs.docker.com/engine/api/v1.45/#tag/Container/operation/ContainerWait

(defn container-wait
  ([id] (container-wait id nil))
  ([id query] (-> (c/POST (str "/containers/" id "/wait") {:query-params query})
                  (c/success-body!))))


; https://docs.docker.com/engine/api/v1.45/#tag/Container/operation/ContainerDelete

(defn container-delete
  ([id] (container-delete id nil))
  ([id query] (-> (c/DELETE (str "/containers/" id) {:query-params query})
                  (c/success-body! c/common-status))))


; Attach to running container and listen for stdout and stderr streams. When a message is 
; read from container calls `on-message` with two args, first is either :stdout or :stderr, 
; and the second is the string message.
; 
; Returns a future. Interrupt the future to terminate the attachment. The future completed 
; when the attachment is terminated, either by interrupt or when the container is terminated.
;
; https://docs.docker.com/engine/api/v1.45/#tag/Container/operation/ContainerAttach


(defn container-attach
  (^java.io.Closeable [id] (container-attach id {:stdout true
                                                 :stderr true}))
  (^java.io.Closeable [id opts]
   (c/attach id opts)))


(comment

  ;; Create container:

  (container-create {:env               ["MODE=world"]
                     :cmd               ["bash" "-c" "while true; do date; sleep 1; done"]
                     :image             "debian:12-slim"
                     :labels            {"test.id" "foo"}
                     :host-config       {:init true}
                     :networking-config {"devcontainersdemo_default" {}}}
                    {:name "test-1234"})
  ;; {:id "6b4d6c50dff1e906a76795c51f19bee97943ee187d547fce2392829b0e344c28"
  ;;  :warnings []} 

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

  (def fut (container-attach "test-1234"
                             (fn [stream-type message]
                               (println (format "[%s] %s" (name stream-type) (pr-str (String. message java.nio.charset.StandardCharsets/UTF_8)))))))

  ;; Console should show output.

  ;; List containers, note that the name starts with "/" for some reason:

  (->> (container-list)
       (filter (comp (partial = "/test-1234") first :names))
       (count))
  ;; => 1

  ;; Future still pending?

  (realized? fut)
  ;; => false

  ;; You can cancel just the attachment by `(future-cancel fut)`

  ;; Stop it:

  (container-stop "test-1234")
  ;; => :ok

  ;; Future done?

  (realized? fut)
  ;; => true

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

  (->> (container-list {:query-params {:all true}})
       (filter (comp (partial = "/test-1234") first :names))
       (count))
  ;; => 1

  ;; Delete container:

  (container-delete "test-1234")
  ;; => :ok

  ;; Now it's not found anymore:

  (->> (container-list {:query-params {:all true}})
       (filter (comp (partial = "/test-1234") first :names))
       (count))
  ;; => 0

  ;
  )



; https://docs.docker.com/engine/api/v1.45/#tag/Exec/operation/ContainerExec

(defn exec-create [id body]
  (-> (c/POST (str "/containers/" id "/exec") {:body body})
      (c/success-body!)))


; https://docs.docker.com/engine/api/v1.45/#tag/Exec/operation/ExecStart

(defn exec-start [id body]
  (-> (c/POST (str "/exec/" id "/start") {:body body})
      (c/success-body!)))


; https://docs.docker.com/engine/api/v1.45/#tag/Exec/operation/ExecInspect

(defn exec-inspect [id]
  (-> (c/GET (str "/exec/" id "/json"))
      (c/success-body!)))


(defn exec [id cmd]
  (let [exec-id   (-> (exec-create id {:attach-stdin  false
                                       :attach-stdout true
                                       :attach-stderr true
                                       :cmd           (cond
                                                        (sequential? cmd) cmd
                                                        (string? cmd) (str/split cmd #"\s+")
                                                        :else (throw (ex-info "invalid cmd" {:cmd cmd})))})
                      :id)
        output    (with-open [resp (c/exec-start exec-id
                                                 {:detach false
                                                  :tty    false}
                                                 {:stdin  false
                                                  :stdout true
                                                  :stderr true})]
                    {:stdout (slurp (:stdout resp))
                     :stderr (slurp (:stderr resp))})
        exit-code (-> (exec-inspect exec-id)
                      :exit-code)]
    (assoc output :exit-code exit-code)))


(comment

  (def base-id (-> (container-create {:image       "debian:12-slim"
                                      :cmd         ["/bin/sleep" "infinity"]
                                      :host-config {:init true}}
                                     {:name "exec-base"})
                   :id))

  (container-start base-id)

  (def exec-id (-> (exec-create base-id {:attach-stdin  false
                                         :attach-stdout true
                                         :attach-stderr false
                                         :cmd           ["/bin/uname" "-a"]})
                   :id))

  (exec-inspect exec-id)

  (with-open [resp (c/exec-start exec-id
                                 {:detach false
                                  :tty    false}
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


  (def network-id (-> (network-create {:name     "docker-test"
                                       :internal true
                                       :labels   {"project" "docker-test"}})
                      :id))

  (def ws (-> (container-create {:image             "eclipse-temurin:21-jdk"
                                 :cmd               (str/split "jwebserver -b 0.0.0.0 -p 8111" #"\s+")
                                 :labels            {"test.id" "base"}
                                 :host-config       {:init true}
                                 :networking-config {"docker-test" {:aliases ["foo" "bar"]}}}
                                {:name "ws"})
              :id))

  (container-start ws)

  (network-connect-container network-id ws {:aliases ["foo" "bar"]})

  (network-inspect network-id)

  ;
  )

