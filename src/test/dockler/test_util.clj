(ns dockler.test-util
  (:require [dockler.api :as api]))


(defn delete-test-networks []
  (doseq [network (api/network-list {"label" ["dockler-test"]})]
    (api/network-remove (:id network)))
  (doseq [network (api/network-list {"name" ["dockler-test-"]})]
    (api/network-remove (:id network))))


(defn delete-test-containers []
  (doseq [container (api/container-list {"label" ["dockler-test"]} true)
          :let      [id (:id container)]]
    (api/container-stop id)
    (api/container-delete id))
  (doseq [container (api/container-list {"name" ["dockler-test-"]} true)
          :let      [id (:id container)]]
    (api/container-stop id)
    (api/container-delete id)))


(def ^:dynamic *test-id* nil)
(def ^:dynamic *test-network-id* nil)


(defn with-test-setup []
  (fn [f]
    (let [test-id (name (gensym "dockler-test-"))]
      (try
        (binding [*test-id* test-id]
          (f))
        (finally
          (delete-test-containers)
          (delete-test-networks))))))


(defn with-test-network []
  (fn [f]
    (let [test-network-id (api/network-create {:name        (str *test-id* "-network")
                                               :labels      {"dockler-test" *test-id*}
                                               :enable-ipv6 false})]
      (try
        (binding [*test-network-id* test-network-id]
          (f))
        (finally
          (api/network-remove test-network-id))))))


;;
;; Pull cache:
;;   Cache images that we have already pulled so that we do not waste time trying to
;;   pull the same images over and over again.
;;

(defonce pulled-images (atom #{}))


(defn ensure-image-pulled [image]
  (when-not (@pulled-images image)
    (api/image-pull image)
    (swap! pulled-images conj image)))


(def default-container-info {:image       "debian:12-slim"
                             :cmd         ["sleep" "infinity"]
                             :working-dir "/app"
                             :labels      {"dockler-test" "true"}})

(def default-host-config {:init   true
                          :memory (* 6 1024 1024)})


(defn make-test-container [container-info]
  (ensure-image-pulled (:image container-info "debian:12-slim"))
  (api/container-create (-> (merge {:name (str (or *test-id* "dockler-test-0000")
                                               "-"
                                               (gensym "container-"))}
                                   default-container-info
                                   container-info)
                            (update :host-config (partial merge default-host-config)))))


(defmacro with-containers [bindings & body]
  (loop [form                    (list* `do body)
         [[id container] & more] (->> bindings
                                      (partition 2)
                                      (reverse))]
    (if id
      (recur `(let [~id (make-test-container ~container)]
                (try
                  ~form
                  (finally
                    (api/container-stop ~id)
                    (api/container-delete ~id))))
             more)
      form)))


(comment
  (macroexpand-1 '(with-containers [foo {:image "foo"}
                                    bar {:image "bar"}
                                    boz {:image "boz"}]
                    (println "foo:" foo)
                    (println "bar:" bar)
                    (println "boz:" boz)))
  ;; => (let [foo (make-test-container {:image "foo"})]
  ;;      (try
  ;;        (let [bar (make-test-container {:image "bar"})]
  ;;          (try
  ;;            (let [boz (make-test-container {:image "boz"})]
  ;;              (try
  ;;                (do (println "foo:" foo) 
  ;;                    (println "bar:" bar) 
  ;;                    (println "boz:" boz))
  ;;              (finally 
  ;;                (dockler.api/container-stop boz)
  ;;                (dockler.api/container-delete boz))))
  ;;            (finally 
  ;;              (dockler.api/container-stop bar) 
  ;;              (dockler.api/container-delete bar))))
  ;;        (finally 
  ;;          (dockler.api/container-stop foo) 
  ;;          (dockler.api/container-delete foo))))
  )
