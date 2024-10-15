(ns dockler.api-test
  (:require [clojure.test :as test :refer [deftest is testing use-fixtures]]
            [clojure.string :as str]
            [matcher-combinators.test]
            [matcher-combinators.matchers :as m]
            [dockler.api :as api]
            [dockler.test-util :as util :refer [*conn* with-containers with-networks with-volumes]]
            [clojure.java.io :as io]))


(use-fixtures :once
  (util/with-test-id)
  (util/with-test-conn))


;;
;; Networks:
;;


(deftest networks-test
  (let [network-name (str util/*test-id* "-network")]
    (with-networks [network-id {:name   network-name
                                :labels {"test-label" util/*test-id*}}]
      (testing "network-inspect with name"
        (is (match? {:name   network-name
                     :id     network-id
                     :labels {"test-label" util/*test-id*}}
                    (api/network-inspect *conn* network-name))))
      (testing "network-inspect with id"
        (is (match? {:name   network-name
                     :id     network-id
                     :labels {"test-label" util/*test-id*}}
                    (api/network-inspect *conn* network-id))))
      (testing "network-list"
        (is (match? (m/seq-of {:id string?})
                    (api/network-list *conn*)))
        (is (some (comp (partial = network-id) :id)
                  (api/network-list *conn*))))
      (testing "network-list with filter"
        (is (match? [{:id network-id}]
                    (api/network-list *conn* {:filters {:label [(str "test-label=" util/*test-id*)]}})))))
    (testing "network is no longer there"
      (is (empty? (api/network-list *conn* {:filters {:label [(str "test-label=" util/*test-id*)]}})))
      (is (nil? (api/network-inspect *conn* network-name)))
      (is (false? (api/network-remove *conn* network-name))))))


;;
;; Containers:
;;


(deftest containers-test
  (with-containers [container-id {:cmd ["sleep" "infinity"]}]
    (is (match? {:id     container-id
                 :config {:image "debian:12-slim"
                          :cmd   ["sleep" "infinity"]}
                 :state  {:running false}}
                (api/container-inspect *conn* container-id)))
    (api/container-start *conn* container-id)
    (is (match? {:id    container-id
                 :state {:running true}}
                (api/container-inspect *conn* container-id)))
    (is (match? {:size-rw      integer?
                 :size-root-fs integer?}
                (api/container-inspect *conn* container-id {:size true})))))


(deftest containers-attach-to-running-container-test
  (with-containers [container-id {:cmd ["bash" "-c" "while true; do date +%s; sleep 1; done"]}]
    (api/container-start *conn* container-id)
    (let [read-line (fn []
                      (-> (future
                            (with-open [conn (api/container-attach *conn* container-id {:stdout true})]
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
                            (with-open [conn (api/container-attach *conn* container-id {:stdout true})]
                              (deliver attach-ready? true)
                              (-> (:stdout conn)
                                  (io/reader)
                                  (line-seq)
                                  (first)
                                  (str/trimr))))]
        @attach-ready?
        (api/container-start *conn* container-id)
        (is (= "hello, world!" (deref message 100 :timeout))))))
  (testing "get stderr"
    ; It would be nice if this would work ["bash" "-c" "echo hello, world!" ">>" "/dev/stderr"]
    (with-containers [container-id {:cmd ["ls" "fofofof"]}]
      (let [attach-ready? (promise)
            message       (future
                            (with-open [conn (api/container-attach *conn* container-id {:stderr true})]
                              (deliver attach-ready? true)
                              (-> (:stderr conn)
                                  (io/reader)
                                  (line-seq)
                                  (first))))]
        @attach-ready?
        (api/container-start *conn* container-id)
        (is (= "ls: cannot access 'fofofof': No such file or directory"
               (deref message 100 :timeout))))))
  (testing "redirect stderr to stdout"
    (with-containers [container-id {:cmd ["ls" "fofofof"]}]
      (let [attach-ready? (promise)
            message       (future
                            (with-open [conn (api/container-attach *conn* container-id {:stdout true
                                                                                        :stderr :stdout})]
                              (deliver attach-ready? true)
                              (-> (:stdout conn)
                                  (io/reader)
                                  (line-seq)
                                  (first))))]
        @attach-ready?
        (api/container-start *conn* container-id)
        (is (= "ls: cannot access 'fofofof': No such file or directory" (deref message 100 :timeout)))))))


(deftest containers-exec-test
  (with-containers [container-id {:cmd ["sleep" "infinity"]}]
    (api/container-start *conn* container-id)
    (let [message "Hello, world!"]
      (is (match? {:stdout    (str message "\n")
                   :stderr    ""
                   :exit-code 0}
                  (api/exec *conn* container-id ["echo" message]))))))


(def test-resources (-> (io/file "test-resources") (.getAbsolutePath)))


(deftest containers-binds-test
  (with-containers [container-id {:cmd         ["cat" "./data/index.html"]
                                  :host-config {:binds [(str test-resources ":/app/data:ro")]}}]
    (let [attach-ready? (promise)
          message       (future
                          (with-open [conn (api/container-attach *conn* container-id {:stdout true})]
                            (deliver attach-ready? true)
                            (-> (:stdout conn)
                                (slurp)
                                (str/trimr))))]
      @attach-ready?
      (api/container-start *conn* container-id)
      (is (= "<h1>Hello, world!</h1>"
             (deref message 100 :timeout))))))


(deftest containers-extract-tar-test
  (with-containers [container-id {:cmd ["sleep" "infinity"]}]
    (api/container-start *conn* container-id)
    (with-open [in (io/input-stream "./test-resources/foo.tgz")]
      (api/container-extract *conn* container-id "/tmp" in {:content-encoding "gzip"}))
    (is (match? {:stdout    "Hello, world!\n"
                 :exit-code 0}
                (api/exec *conn* container-id ["cat" "/tmp/foo/bar.txt"])))))


(deftest container-changes-test
  (with-containers [container-id {:cmd ["sleep" "infinity"]}]
    (api/container-start *conn* container-id)
    (api/exec *conn* container-id ["touch" "/tmp/foo.txt"])
    (is (match? [{:path "/tmp/foo.txt"
                  :kind 1}]
                (filter (comp (partial = "/tmp/foo.txt") :path)
                        (api/container-changes *conn* container-id))))))


(deftest container-archive-info-test
  (with-containers [container-id {:cmd ["sleep" "infinity"]}]
    (api/container-start *conn* container-id)
    (is (match? {:name  "root"
                 :size  integer?
                 :mode  integer?
                 :mtime string?}
                (api/container-archive-info *conn* container-id "/root")))))

;; Get a free server port:

(defn free-port []
  (let [socket (java.net.ServerSocket. 0)
        port   (.getLocalPort socket)]
    (.close socket)
    port))


;; Make a HTTP GET to host 127.0.0.1 with given port and path. Retry every 10ms if 
;; fails. Returns the response body as string, or `:timeout` if not successful in 
;; 5 seconds.

(defn http-get
  ([port] (http-get port nil))
  ([port path]
   (let [client       (-> (java.net.http.HttpClient/newBuilder)
                          (.connectTimeout (java.time.Duration/ofMillis 10))
                          (.build))
         request      (-> (java.net.http.HttpRequest/newBuilder)
                          (.GET)
                          (.uri (java.net.URI. (str "http://127.0.0.1:" port "/" path)))
                          (.timeout (java.time.Duration/ofMillis 10))
                          (.build))
         body-handler (java.net.http.HttpResponse$BodyHandlers/ofString)
         timeout      (+ (System/currentTimeMillis) 5000)]
     (loop []
       (let [resp (try
                    (.send client request body-handler)
                    (catch java.io.IOException _
                      (Thread/sleep 10)
                      nil))]
         (cond
           resp (.body resp)
           (> (System/currentTimeMillis) timeout) :timeout
           :else (recur)))))))


(deftest container-networking-test
  (let [port (free-port)]
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
      (api/container-start *conn* container-id)
      (is (= "<h1>Hello, world!</h1>" (http-get port))))))


(deftest container-inter-networking-test
  (let [port       (free-port)
        *expected* "<h1>Hello, world!</h1>"]
    ;; Start nginx container to serves static content from /usr/share/nginx/html, mount test-resources to that 
    ;; directory. Start another nginx container as a proxy server. Configuration is in `test-resources/nginx.conf`. 
    (with-networks [network-id {:name "container-inter-networking-test"}]
      (with-containers [server-container-id {:name              "server-container-name"
                                             :hostname          "server-hostname"
                                             :image             "nginx:1-alpine-slim"
                                             :cmd               ["nginx" "-g" "daemon off;"]
                                             :host-config       {:binds [(str test-resources
                                                                              ":"
                                                                              "/usr/share/nginx/html"
                                                                              ":ro")]}
                                             :networking-config {:endpoints-config {network-id {}}}}
                        proxy-container-id {:image             "nginx:1-alpine-slim"
                                            :cmd               ["nginx"]
                                            :host-config       {:port-bindings {"80/tcp" [{:host-ip   "127.0.0.1"
                                                                                           :host-port (str port)}]}
                                                                :binds         [(str test-resources "/nginx.conf"
                                                                                     ":"
                                                                                     "/etc/nginx/nginx.conf"
                                                                                     ":ro")]}
                                            :networking-config {:endpoints-config {network-id {}}}}]
        (api/container-start *conn* server-container-id)
        (api/container-start *conn* proxy-container-id)
        ;; Test that the proxy can reach server container container name...
        (testing "proxy can reach server using container name"
          (is (= *expected* (http-get port "/by-container-name"))))
        ;; ...and by hostname:
        (testing "proxy can reach server using hostname"
          (is (= *expected* (http-get port "/by-hostname"))))))))


(deftest volumes-test
  (with-volumes [foo {:name "foo"}
                 bar {}]
    (is (match? {:name   "foo"
                 :driver "local"
                 :scope  "local"}
                (api/volume-inspect *conn* foo)))
    (is (match? {:name   (m/regex #"dockler-test-volume-.*")
                 :driver "local"
                 :scope  "local"}
                (api/volume-inspect *conn* bar))))
  (is (nil? (api/volume-inspect *conn* "foo"))))


(deftest volume-mount-test
  (with-volumes [vol-name {}]
    ; Create container, exec in it to write /data/test.txt
    (with-containers [container-id {:cmd         ["sleep" "infinity"]
                                    :host-config {:mounts [{:source vol-name
                                                            :type   "volume"
                                                            :target "/data"}]}}]
      (api/container-start *conn* container-id)
      (is (match? {:host-config {:mounts [{:source vol-name
                                           :type   "volume"
                                           :target "/data"}]}}
                  (api/container-inspect *conn* container-id)))
      (let [exec-id (api/exec-create *conn* container-id {:attach-stdin true
                                                          :cmd          ["tee" "/data/test.txt"]})]
        (with-open [exec (api/exec-start *conn* exec-id {:stdin true})]
          (doto (:stdin exec)
            (.write (util/str-bytes "hello, world!\n"))
            (.flush))))
      ;; Without some small delay the write is not flushed to volume
      ;; TODO: Figure out if there's a way to flush or wait for i/o
      (Thread/sleep 200))
    ; Create container, exec in it to cat /data/test.txt
    (with-containers [container-id {:cmd         ["sleep" "infinity"]
                                    :host-config {:mounts [{:source vol-name
                                                            :type   "volume"
                                                            :target "/data"}]}}]
      (api/container-start *conn* container-id)
      (is (match? {:host-config {:init   true
                                 :mounts [{:source vol-name
                                           :type   "volume"
                                           :target "/data"}]}}
                  (api/container-inspect *conn* container-id)))
      (is (= "hello, world!\n"
             (let [exec-id (api/exec-create *conn* container-id {:attach-stdout true
                                                                 :cmd           ["cat" "/data/test.txt"]})]
               (with-open [exec (api/exec-start *conn* exec-id {:stdout true})]
                 (slurp (:stdout exec)))))))))


(comment
  (api/volume-create nil {:name "test-volume"})

  (with-open [conn (api/connect)]
    (api/container-create-and-start conn {:image       "debian:12-slim"
                                          :name        "test-1"
                                          :cmd         ["sleep" "infinity"]
                                          :host-config {:init        true
                                                        :auto-remove true
                                                        :mounts      [{:source "test-volume"
                                                                       :type   "volume"
                                                                       :target "/data"}]}})
    (let [exec-id (api/exec-create conn "test-1" {:attach-stdin true
                                                  :cmd          ["tee" "/data/test-2.txt"]})]
      (with-open [exec (api/exec-start conn exec-id {:stdin true})]
        (.write (:stdin exec) (util/str-bytes "hello, world!\n"))
        (.flush (:stdin exec))))
    (Thread/sleep 1000)
    (api/container-stop conn "test-1"))


  (with-open [conn (api/connect)]
    (api/container-create-and-start conn {:image       "debian:12-slim"
                                          :name        "test-2"
                                          :cmd         ["sleep" "infinity"]
                                          :host-config {:init        true
                                                        :auto-remove true
                                                        :mounts      [{:source "test-volume"
                                                                       :type   "volume"
                                                                       :target "/data"}]}})
    (let [exec-id (api/exec-create conn "test-2" {:attach-stdout true
                                                  :cmd           ["cat" "/data/test.txt"]})]
      (with-open [exec (api/exec-start conn exec-id {:stdout true})]
        (println "TEST:" (slurp (:stdout exec)))))
    (api/container-stop conn "test-2"))

  (api/container-stop nil "test-2")

  (api/volume-delete nil "test-volume"))
