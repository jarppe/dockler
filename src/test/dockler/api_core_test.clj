(ns dockler.api-core-test
  (:require [clojure.test :as test :refer [deftest is testing]]
            [matcher-combinators.test]
            [dockler.api :as api]
            [dockler.test-util :as util]))


;;
;; Test minimal functionality that is used in test fixtures
;;


(deftest create-and-delete-network-test
  (let [test-name    (name (gensym "dockler-test-"))
        network-name (str test-name "-network")
        network-id   (api/network-create {:name network-name})]
    (testing "network id is returned"
      (is (string? network-id)))
    (testing "network can be inspected"
      (is (some? (api/network-inspect network-id))))
    (testing "netwotk can be removed"
      (is (nil? (api/network-remove network-id))))
    (testing "network is deleted"
      (is (match? {:resp {:status 404}}
                  (try
                    (api/network-inspect network-id)
                    (catch clojure.lang.ExceptionInfo e
                      (ex-data e))))))))


(deftest delete-dockler-test-networks-test
  (let [test-name      (name (gensym "dockler-test-"))
        network-1-name (str test-name "-network")
        network-1-id   (api/network-create {:name network-1-name})
        network-2-name "some-other-test-network"
        network-2-id   (api/network-create {:name   network-2-name
                                            :labels {"dockler-test" test-name}})]
    (util/delete-test-networks)
    (testing "networks with docker-test-... name are deleted"
      (is (match? {:resp {:status 404}}
                  (try
                    (api/network-inspect network-1-id)
                    (catch clojure.lang.ExceptionInfo e
                      (ex-data e))))))
    (testing "networks with docker-test=... label are deleted"
      (is (match? {:resp {:status 404}}
                  (try
                    (api/network-inspect network-2-id)
                    (catch clojure.lang.ExceptionInfo e
                      (ex-data e))))))))


(deftest create-and-delete-containers-test
  (let [test-name        (name (gensym "dockler-test-"))
        image            "debian:12-slim"
        cmd              ["sleep" "infinity"]
        container-1-name (str test-name "-container")
        container-1-id   (api/container-create {:name        container-1-name
                                                :cmd         cmd
                                                :image       image
                                                :host-config {:init true}})
        container-2-name "some-other-test-container"
        container-2-id   (api/container-create {:name        container-2-name
                                                :cmd         cmd
                                                :image       image
                                                :labels      {"dockler-test" test-name}
                                                :host-config {:init true}})]
    (util/delete-test-containers)
    (testing "containers with docker-test-... name are deleted"
      (is (match? {:resp {:status 404}}
                  (try
                    (api/container-inspect container-1-id)
                    (catch clojure.lang.ExceptionInfo e
                      (ex-data e))))))
    (testing "containers with docker-test=... label are deleted"
      (is (match? {:resp {:status 404}}
                  (try
                    (api/container-inspect container-2-id)
                    (catch clojure.lang.ExceptionInfo e
                      (ex-data e))))))))
