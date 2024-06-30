(ns dockler.api-test
  (:require [clojure.test :as test :refer [deftest is testing use-fixtures]]
            [clojure.string :as str]
            [matcher-combinators.test]
            [matcher-combinators.matchers :as m]
            [dockler.api :as api]
            [dockler.test-util :as util :refer [with-containers]]
            [clojure.java.io :as io]
            [dockler.data :as data]))


(use-fixtures :once
  (util/with-test-setup)
  (util/with-test-network))


;;
;; Networks:
;;


(deftest create-list-inspect-delete-network-test
  (let [test-id    util/*test-id*
        network-id util/*test-network-id*]
    (testing "network-inspect"
      (is (match? {:name   (str test-id "-network")
                   :id     network-id
                   :labels {"dockler-test" test-id}}
                  (api/network-inspect network-id))))
    (testing "network-list"
      (is (match? (m/seq-of {:id string?})
                  (api/network-list))))
    (testing "network-list with filter"
      (is (match? [{:id network-id}]
                  (api/network-list {:label [(str "dockler-test=" test-id)]}))))))


;;
;; Containers:
;;


(deftest containers-test
  (with-containers [container-id {:cmd ["sleep" "infinity"]}]
    (is (match? {:id     container-id
                 :config {:image "debian:12-slim"
                          :cmd   ["sleep" "infinity"]}
                 :state  {:running false}}
                (api/container-inspect container-id)))
    (api/container-start container-id)
    (is (match? {:id    container-id
                 :state {:running true}}
                (api/container-inspect container-id)))
    (is (match? {:size-rw      integer?
                 :size-root-fs integer?}
                (api/container-inspect container-id {:size true})))))


(deftest containers-attach-to-running-container-test
  (with-containers [container-id {:cmd ["bash" "-c" "while true; do date; sleep 1; done"]}]
    (api/container-start container-id)
    (is (string? (-> (future
                       (with-open [conn (api/container-attach container-id {:stdout true})]
                         (-> (:stdout conn)
                             (io/reader)
                             (line-seq)
                             (first))))
                     (deref 1500 ::timeout))))))


(deftest containers-attach-to-new-container-test
  (testing "get stdout"
    (with-containers [container-id {:cmd ["echo" "hello, world!"]}]
      (let [attach-ready? (promise)
            message       (future
                            (with-open [conn (api/container-attach container-id {:stdout true})]
                              (deliver attach-ready? true)
                              (-> (:stdout conn)
                                  (io/reader)
                                  (line-seq)
                                  (first)
                                  (str/trimr))))]
        @attach-ready?
        (api/container-start container-id)
        (is (= "hello, world!" (deref message 100 ::timeout))))))
  (testing "get stderr"
    ; It would be nice if this would work ["bash" "-c" "echo hello, world!" ">>" "/dev/stderr"]
    (with-containers [container-id {:cmd ["ls" "fofofof"]}]
      (let [attach-ready? (promise)
            message       (future
                            (with-open [conn (api/container-attach container-id {:stderr true})]
                              (deliver attach-ready? true)
                              (-> (:stderr conn)
                                  (io/reader)
                                  (line-seq)
                                  (first))))]
        @attach-ready?
        (api/container-start container-id)
        (is (= "ls: cannot access 'fofofof': No such file or directory"
               (deref message 100 ::timeout))))))
  (testing "redirect stderr to stdout"
    (with-containers [container-id {:cmd ["ls" "fofofof"]}]
      (let [attach-ready? (promise)
            message       (future
                            (with-open [conn (api/container-attach container-id {:stdout true
                                                                                 :stderr :stdout})]
                              (deliver attach-ready? true)
                              (-> (:stdout conn)
                                  (io/reader)
                                  (line-seq)
                                  (first))))]
        @attach-ready?
        (api/container-start container-id)
        (is (= "ls: cannot access 'fofofof': No such file or directory" (deref message 100 ::timeout)))))))


(deftest containers-exec-test
  (with-containers [container-id {:cmd ["sleep" "infinity"]}]
    (api/container-start container-id)
    (let [message "Hello, world!"]
      (is (match? {:stdout    (str message "\n")
                   :stderr    ""
                   :exit-code 0}
                  (api/exec container-id ["echo" message]))))))


(def test-resources (-> (io/file "test-resources") (.getAbsolutePath)))


(deftest containers-binds-test
  (with-containers [container-id  {:cmd         ["cat" "./data/index.html"]
                                   :host-config {:binds [(str test-resources ":/app/data:ro")]}}]
    (let [attach-ready? (promise)
          message       (future
                          (with-open [conn (api/container-attach container-id {:stdout true})]
                            (deliver attach-ready? true)
                            (-> (:stdout conn)
                                (slurp)
                                (str/trimr))))]
      @attach-ready?
      (api/container-start container-id)
      (is (= "<h1>Hello, world!</h1>"
             (deref message 100 ::timeout))))))


(deftest containers-extract-tar-test
  (with-containers [container-id {:cmd ["sleep" "infinity"]}]
    (api/container-start container-id)
    (with-open [in (io/input-stream "./test-resources/foo.tgz")]
      (api/container-extract container-id "/tmp" in {:content-encoding "gzip"}))
    (is (match? {:stdout    "Hello, world!\n"
                 :exit-code 0}
                (api/exec container-id ["cat" "/tmp/foo/bar.txt"])))))


(deftest container-changes-test
  (with-containers [container-id {:cmd ["sleep" "infinity"]}]
    (api/container-start container-id)
    (api/exec container-id ["touch" "/tmp/foo.txt"])
    (is (match? [{:path "/tmp/foo.txt"
                  :kind 1}]
                (filter (comp (partial = "/tmp/foo.txt") :path)
                        (api/container-changes container-id))))))


(deftest container-archive-info-test
  (with-containers [container-id {:cmd ["sleep" "infinity"]}]
    (api/container-start container-id)
    (is (match? {:name  "root"
                 :size  integer?
                 :mode  integer?
                 :mtime string?}
                (api/container-archive-info container-id "/root")))))


(deftest container-networking-test
  (let [;; Get free local port:
        port (let [socket (java.net.ServerSocket. 0)
                   port   (.getLocalPort socket)]
               (.close socket)
               port)
        ;; Function to make a HTTP GET and returning `nil` on connection failure 
        ;; and string on success:
        GET  (let [client       (-> (java.net.http.HttpClient/newBuilder)
                                    (.connectTimeout (java.time.Duration/ofMillis 10))
                                    (.build))
                   request      (-> (java.net.http.HttpRequest/newBuilder)
                                    (.GET)
                                    (.uri (java.net.URI. (str "http://127.0.0.1:" port "/")))
                                    (.timeout (java.time.Duration/ofMillis 10))
                                    (.build))
                   body-handler (java.net.http.HttpResponse$BodyHandlers/ofString)]
               (fn []
                 (try
                   (-> (.send client request body-handler)
                       (.body))
                   (catch java.io.IOException _
                     nil))))]
    ;; Start container with simple Web server that serves static content 
    ;; from /app, mount test-resources to /app. Web server listens port 8000, 
    ;; map local port `port` to 8000:
    (with-containers [container-id {:image         "eclipse-temurin:21-jre-alpine"
                                    :cmd           ["jwebserver" "-b" "0.0.0.0" "-p" "8000"]
                                    :exposed-ports {"8000/tcp" {}}
                                    :host-config   {:memory        (* 512 1024 1024)
                                                    :port-bindings {"8000/tcp" [{:host-ip   "127.0.0.1"
                                                                                 :host-port (str port)}]}
                                                    :binds         [(str test-resources ":/app:ro")]}}]
      (api/container-start container-id)
      ;; Try to make HTTP GET to `port` repeatedly until we get response, or
      ;; timeout is reaced:
      (let [timeout (+ (System/currentTimeMillis) 5000)
            resp    (loop []
                      (let [resp (if (> (System/currentTimeMillis) timeout)
                                   :timeout
                                   (GET))]
                        (if resp
                          resp
                          (recur))))]
        (is (= "<h1>Hello, world!</h1>" resp))))))

;; TODO: Add test for network-connect-container
