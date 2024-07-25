(ns dockler.chunked-input-stream-test
  (:require [clojure.test :as test :refer [deftest is are testing]]
            [matcher-combinators.test]
            [dockler.content-stream :as cs]))


(defn byte-stream ^java.io.InputStream [^String s]
  (-> (.getBytes s java.nio.charset.StandardCharsets/UTF_8)
      (java.io.ByteArrayInputStream.)))


(deftest read-chunk-len-test
  (are [input expected] (let [in        (byte-stream input)
                              chunk-len (cs/read-chunk-len in)]
                          (and (= expected chunk-len)
                               (= (int \$) (.read in))))
    "1\r\n$"     0x01
    "12\r\n$"    0x12
    "01\r\n$"    0x01
    ; The terminating chunk is read completly:
    "0\r\n\r\n$"  0x00))


; From https://en.wikipedia.org/wiki/Chunked_transfer_encoding#Encoded_data

(def encoded ["4\r\n" "Wiki" "\r\n"
              "7\r\n" "pedia i" "\r\n"
              "B\r\n" "n \r\nchunks." "\r\n"
              "0\r\n"
              "\r\n"
              ; Extra sentinel to verify stream position 
              "$"])

(def decoded "Wikipedia in \r\nchunks.")

(deftest read-chunked-stream-completely-test
  (let [input   (-> (apply str encoded)
                    (byte-stream))
        chunked (cs/chunked-input-stream input)]
    (testing "Read the chunked data"
      (is (match? (-> chunked
                      (.readAllBytes)
                      (String. java.nio.charset.StandardCharsets/UTF_8))
                  decoded)))
    (testing "The chunked stream is at EOF"
      (is (= -1 (.read chunked))))
    (testing "The underlying stream is positioned after the chunked content"
      (is (= (int \$) (.read input)))
      (is (= -1 (.read input))))))


(deftest read-chunked-stream-completely-byte-by-byte-test
  (let [input   (-> (apply str encoded)
                    (byte-stream))
        chunked (cs/chunked-input-stream input)]
    (testing "Read the chunked data"
      (is (match? (loop [data ""]
                    (let [c (.read chunked)]
                      (if (= -1 c)
                        data
                        (recur (str data (char c))))))
                  decoded)))
    (testing "The chunked stream is at EOF"
      (is (= -1 (.read chunked))))
    (testing "The underlying stream is positioned after the chunked content"
      (is (= (int \$) (.read input)))
      (is (= -1 (.read input))))))


(deftest empty-chunked-stream-test
  (let [input   (byte-stream "0\r\n\r\n$")
        chunked (cs/chunked-input-stream input)]
    (testing "Read the chunked data"
      (is (= 0 (-> chunked
                   (.readAllBytes)
                   (alength)))))
    (testing "The chunked stream is at EOF"
      (is (= -1 (.read chunked))))
    (testing "The underlying stream is positioned after the chunked content"
      (is (= (int \$) (.read input)))
      (is (= -1 (.read input))))))
