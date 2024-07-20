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
  (with-containers [container-id {:cmd ["bash" "-c" "while true; do date +%s; sleep 1; done"]}]
    (api/container-start container-id)
    (let [read-line (fn []
                      (-> (future
                            (with-open [conn (api/container-attach container-id {:stdout true})]
                              (-> (:stdout conn)
                                  (io/reader)
                                  (line-seq)
                                  (first))))
                          (deref 1500 :timeout)))
          out1      (read-line)
          out2      (read-line)]
      (is (match? (m/regex #"\d+") out1))
      (is (match? (m/regex #"\d+") out2))
      (is (< (parse-long out1)
             (parse-long out2))))))


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
        (is (= "hello, world!" (deref message 100 :timeout))))))
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
               (deref message 100 :timeout))))))
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
        (is (= "ls: cannot access 'fofofof': No such file or directory" (deref message 100 :timeout)))))))


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
  (with-containers [container-id {:cmd         ["cat" "./data/index.html"]
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
             (deref message 100 :timeout))))))


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
                          (do (Thread/sleep 10)
                              (recur)))))]
        (is (= "<h1>Hello, world!</h1>" resp))))))


(deftest container-inter-networking-test
  (let [;; Get free local port:
        port       (let [socket (java.net.ServerSocket. 0)
                         port   (.getLocalPort socket)]
                     (.close socket)
                     port)
        ;; Function to make a HTTP GET, retrying on connection failure, returning either
        ;; the successful response or `:timeout`:
        GET        (fn [uri]
                     (let [client       (-> (java.net.http.HttpClient/newBuilder)
                                            (.connectTimeout (java.time.Duration/ofMillis 10))
                                            (.build))
                           request      (-> (java.net.http.HttpRequest/newBuilder)
                                            (.GET)
                                            (.uri (java.net.URI. (str "http://127.0.0.1:" port uri)))
                                            (.timeout (java.time.Duration/ofMillis 10))
                                            (.build))
                           body-handler (java.net.http.HttpResponse$BodyHandlers/ofString)
                           timeout      (+ (System/currentTimeMillis) 5000)]
                       (loop []
                         (let [resp (try
                                      (-> (.send client request body-handler)
                                          (.body))
                                      (catch java.io.IOException _
                                        nil))]
                           (cond
                             resp resp
                             (> (System/currentTimeMillis) timeout) :timeout
                             :else (do (Thread/sleep 10)
                                       (recur)))))))
        *expected* "<h1>Hello, world!</h1>"]
    ;; Start nginx container to serves static content from /usr/share/nginx/html, mount test-resources to that 
    ;; directory. Start another nginx container as a proxy server. Configuration is in `test-resources/nginx.conf`. 
    (with-containers [server-container-id {:name              "server-container-name"
                                           :hostname          "server-hostname"
                                           :image             "nginx:1-alpine-slim"
                                           :cmd               ["nginx" "-g" "daemon off;"]
                                           :host-config       {:binds [(str test-resources
                                                                            ":"
                                                                            "/usr/share/nginx/html"
                                                                            ":ro")]}
                                           :networking-config {:endpoints-config {util/*test-network-id* {}}}}
                      proxy-container-id {:image             "nginx:1-alpine-slim"
                                          :cmd               ["nginx"]
                                          :host-config       {:port-bindings {"80/tcp" [{:host-ip   "127.0.0.1"
                                                                                         :host-port (str port)}]}
                                                              :binds         [(str test-resources "/nginx.conf"
                                                                                   ":"
                                                                                   "/etc/nginx/nginx.conf"
                                                                                   ":ro")]}
                                          :networking-config {:endpoints-config {util/*test-network-id* {}}}}]
      (api/container-start server-container-id)
      (api/container-start proxy-container-id)
      ;; Test that the proxy can reach server container container name and hostname:
      (testing "proxy can reach server using container name"
        (is (= *expected* (GET "/by-container-name"))))
      (testing "proxy can reach server using hostname"
        (is (= *expected* (GET "/by-hostname")))))))
