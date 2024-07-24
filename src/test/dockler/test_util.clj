(ns dockler.test-util
  (:require [dockler.api :as api]))


(def ^:dynamic *test-id* nil)

(defn with-test-id []
  (fn [f]
    (binding [*test-id* (name (gensym "dockler-test-"))]
      (f))))


(def ^:dynamic *conn* nil)


(defn with-test-conn []
  (fn [f]
    (with-open [conn (api/connect)]
      (binding [*conn* conn]
        (f)))))


;;
;; Pull cache:
;;   Cache images that we have already pulled so that we do not waste time trying to
;;   pull the same images over and over again.
;;


(defonce pulled-images (atom #{}))


(defn ensure-image-pulled [conn image]
  (when-not (@pulled-images image)
    (api/image-pull conn image)
    (swap! pulled-images conj image)))


(def default-container-info {:image       "debian:12-slim"
                             :cmd         ["sleep" "infinity"]
                             :working-dir "/app"
                             :labels      {"dockler-test" "true"}})


(def default-host-config {:init   true
                          :memory (* 6 1024 1024)})


(defn make-test-container [conn container-info]
  (ensure-image-pulled conn (:image container-info "debian:12-slim"))
  (api/container-create conn (-> (merge {:name (str (or *test-id* "dockler-test-0000")
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
    (if (nil? id)
      form
      (recur `(let [~id (make-test-container *conn* ~container)]
                (try
                  ~form
                  (finally
                    (api/container-stop *conn* ~id)
                    (api/container-delete *conn* ~id))))
             more))))


(comment
  (macroexpand-1 '(with-containers [foo {:image "foo"}
                                    bar {:image "bar"}
                                    boz {:image "boz"}]
                    (println "foo:" foo)
                    (println "bar:" bar)
                    (println "boz:" boz)))
  ;; => (let [foo (make-test-container conn {:image "foo"})]
  ;;      (try
  ;;        (let [bar (make-test-container conn {:image "bar"})]
  ;;          (try
  ;;            (let [boz (make-test-container conn {:image "boz"})]
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


(defmacro with-networks [bindings & body]
  (loop [form                  (list* `do body)
         [[id network] & more] (->> bindings
                                    (partition 2)
                                    (reverse))]
    (if (nil? id)
      form
      (recur `(let [~id (api/network-create *conn* ~network)]
                (try
                  ~form
                  (finally
                    (api/network-remove *conn* ~id))))
             more))))


(comment
  (macroexpand-1 '(with-networks [foo {}
                                  bar {}
                                  boz {}]
                    (println "foo:" foo)
                    (println "bar:" bar)
                    (println "boz:" boz)))
  ;; => (let [foo (dockler.api/network-create dockler.test-util/*conn* {})]
  ;;      (try
  ;;        (let [bar (dockler.api/network-create dockler.test-util/*conn* {})]
  ;;          (try
  ;;            (let [boz (dockler.api/network-create dockler.test-util/*conn* {})]
  ;;              (try
  ;;                (do (println "foo:" foo) 
  ;;                    (println "bar:" bar)
  ;;                    (println "boz:" boz))
  ;;                (finally (dockler.api/network-remove boz))))
  ;;            (finally (dockler.api/network-remove bar))))
  ;;        (finally (dockler.api/network-remove foo))))
  )