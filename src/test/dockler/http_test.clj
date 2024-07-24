(ns dockler.http-test
  (:require [clojure.test :as test :refer [deftest is are testing]]
            [clojure.string :as str]
            [matcher-combinators.test]
            [dockler.http :as http])
  (:import (java.nio.charset StandardCharsets)))


(defn byte-stream ^java.io.InputStream [^String s]
  (-> (.getBytes s StandardCharsets/UTF_8)
      (java.io.ByteArrayInputStream.)
      (java.io.PushbackInputStream. 8)))


(deftest read-response-valid-input-test
  (let [req      {}
        response (-> (str/join "\r\n" ["HTTP/1.1 200 OK"
                                       "Transfer-Encoding: chunked"
                                       "Content-Type: application/json"
                                       ""
                                       "6"
                                       "{\"foo\""
                                       "5"
                                       ": 42}"
                                       "0"
                                       ""
                                       "$"])
                     (byte-stream))
        resp     (http/read-response req response)]
    (is (match? {:status  200
                 :headers {"transfer-encoding" "chunked"
                           "content-type"      "application/json"}
                 :body    {:foo 42}}
                resp))
    (is (identical? req (-> resp (meta) :req)))
    (is (= (int \$) (.read response)))))


(deftest read-response-excess-bytes-input-test
  (let [req      {}
        response (-> (str/join "\r\n" ["HTTP/1.1 200 OK"
                                       "Transfer-Encoding: chunked"
                                       "Content-Type: application/json"
                                       ""
                                       "6"
                                       "{\"foo\""
                                       "5"
                                       ": 42}"
                                       "0"
                                       ""
                                       "0"
                                       ""
                                       "$"])
                     (byte-stream))
        resp     (http/read-response req response)]
    (is (match? {:status  200
                 :headers {"transfer-encoding" "chunked"
                           "content-type"      "application/json"}
                 :body    {:foo 42}}
                resp))
    (is (identical? req (-> resp (meta) :req)))
    (is (= (int \$) (.read response)))))


(def CRNL "\r\n")

(deftest write-request-test
  (are [req expected] (let [out (java.io.ByteArrayOutputStream.)]
                        (http/write-request req out)
                        (let [actual (-> (.toByteArray out)
                                         (String. StandardCharsets/UTF_8))]
                          (when-not (= expected actual)
                            (println "expected:" (pr-str expected))
                            (println "actual:  " (pr-str actual)))
                          (= expected actual)))
    {:uri "/path"}
    (str "GET " http/docker-api-prefix "/path HTTP/1.1" CRNL
         CRNL)

    {:method :post
     :uri    "/path"}
    (str "POST " http/docker-api-prefix "/path HTTP/1.1" CRNL
         CRNL)

    {:method  :post
     :uri     "/path"
     :headers {"foo" "bar"}}
    (str "POST " http/docker-api-prefix "/path HTTP/1.1" CRNL
         "foo: bar" CRNL
         CRNL)

    {:method       :post
     :uri          "/path"
     :headers      {"foo" "bar"}
     :query-params {"fofo" "baba"}}
    (str "POST " http/docker-api-prefix "/path?fofo=baba HTTP/1.1" CRNL
         "foo: bar" CRNL
         CRNL)

    {:method :post
     :uri    "/path"
     :body   {:foo 42}}
    (str "POST " http/docker-api-prefix "/path HTTP/1.1" CRNL
         "transfer-encoding: chunked" CRNL
         "content-type: application/json; charset=utf-8" CRNL
         CRNL
         "a" CRNL
         "{\"Foo\":42}" CRNL
         "0" CRNL
         CRNL)))

(deftest read-response-test
  (testing "response with normal content"
    (let [input (-> (str "HTTP/1.1 200 OK" CRNL
                         "transfer-encoding: chunked" CRNL
                         "content-type: application/json; charset=utf-8" CRNL
                         CRNL
                         "a" CRNL
                         "{\"Foo\":42}" CRNL
                         "0" CRNL
                         CRNL
                         "$")
                    (byte-stream))]
      (is (match? {:status  200
                   :headers {"transfer-encoding" "chunked"
                             "content-type"      "application/json; charset=utf-8"}
                   :body    {:foo 42}}
                  (http/read-response nil input)))
      (is (= (int \$) (.read input)))
      (is (= -1 (.read input)))))
  (testing "response with extra bytes"
    (let [input (-> (str "HTTP/1.1 200 OK" CRNL
                         "transfer-encoding: chunked" CRNL
                         "content-type: application/json; charset=utf-8" CRNL
                         CRNL
                         "a" CRNL
                         "{\"Foo\":42}" CRNL
                         "0" CRNL
                         CRNL
                         "0" CRNL
                         CRNL
                         "$")
                    (byte-stream))]
      (is (match? {:status  200
                   :headers {"transfer-encoding" "chunked"
                             "content-type"      "application/json; charset=utf-8"}
                   :body    {:foo 42}}
                  (http/read-response nil input)))
      (is (= (int \$) (.read input)))
      (is (= -1 (.read input)))))
  (testing "request is in response metadata"
    (let [req   {}
          input (-> (str "HTTP/1.1 200 OK" CRNL
                         "content-length: 3" CRNL
                         "content-type: text/plain" CRNL
                         CRNL
                         "foo"
                         "$")
                    (byte-stream))
          resp  (http/read-response req input)]
      (is (match? {:status 200
                   :body   "foo"}
                  resp))
      (is (= (int \$) (.read input)))
      (is (= -1 (.read input)))
      (is (identical? req (-> resp (meta) :req))))))
