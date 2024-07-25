(ns dockler.impl.http-test
  (:require [clojure.test :as test :refer [deftest is are testing]]
            [clojure.string :as str]
            [matcher-combinators.test]
            [dockler.impl.http :as http])
  (:import (java.nio.charset StandardCharsets)))


(defn byte-stream ^java.io.InputStream [^String s]
  (-> (.getBytes s StandardCharsets/UTF_8)
      (java.io.ByteArrayInputStream.)))


(def valid-input (str/join "\r\n" ["HTTP/1.1 200 OK"
                                   "Transfer-Encoding: chunked"
                                   "Content-Type: application/json"
                                   ""
                                   "6"
                                   "{\"foo\""
                                   "5"
                                   ": 42}"
                                   "0"
                                   ""
                                   "$"]))


(def leading-end-of-chunk-input (str/join "\r\n" ["0"
                                                  ""
                                                  "HTTP/1.1 200 OK"
                                                  "Transfer-Encoding: chunked"
                                                  "Content-Type: application/json"
                                                  ""
                                                  "6"
                                                  "{\"foo\""
                                                  "5"
                                                  ": 42}"
                                                  "0"
                                                  ""
                                                  "$"]))


(deftest read-response-test
  (testing "Read normal valid HTTP response"
    (let [input    valid-input
          response (byte-stream input)
          req      {}
          resp     (http/read-response req response)]
      (is (match? {:status  200
                   :headers {"transfer-encoding" "chunked"
                             "content-type"      "application/json"}
                   :body    {:foo 42}}
                  resp))
      (is (identical? req (-> resp (meta) :req)))
      (is (= (int \$) (.read response)))))
  (testing "Read input that has end-of-chunk leftover from prev response"
    (let [input    leading-end-of-chunk-input
          response (byte-stream input)
          req      {}
          resp     (http/read-response req response)]
      (is (match? {:status  200
                   :headers {"transfer-encoding" "chunked"
                             "content-type"      "application/json"}
                   :body    {:foo 42}}
                  resp))
      (is (identical? req (-> resp (meta) :req)))
      (is (= (int \$) (.read response))))))


(deftest read-response-excess-bytes-input-test
  (let [req      {}
        response (-> (str/join "\r\n" ["0"
                                       ""
                                       "HTTP/1.1 200 OK"
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
