(ns dockler.util.explain-image-layers
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [cheshire.core :as json]
            [dockler.api :as api])
  (:import (java.util.zip GZIPInputStream)
           (java.time ZoneId
                      ZonedDateTime)
           (java.time.format DateTimeFormatter)
           (org.apache.commons.compress.archivers.tar TarArchiveInputStream)))

;;
;; This implementation aims to be compatible with Babashka. The commons compress
;; is not included in babashka, so this works only in JVM clojure.
;;


(set! *warn-on-reflection* true)


;;
;; Blob handling:
;;
;; We can not know the contents of the file while we're reading the TAR stream. This
;; peeks into the file contents and checks if it contains a GZIP content by checking
;; the first two bytes. If contents is GZIP, we assume it is a GZIPped TAR ball for
;; Docker layer containing the file-system changes.
;;
;; See https://en.wikipedia.org/wiki/List_of_file_signatures for GZIP magic bytes.
;;

(defn- blob? [^java.io.PushbackInputStream in]
  (let [b1 (.read in)
        b2 (.read in)]
    (.unread in b2)
    (.unread in b1)
    (and (= (bit-and b1 0xff) 0x1f)
         (= (bit-and b2 0xff) 0x8b))))


(defn- read-blob [^java.io.InputStream in]
  (let [in (-> (GZIPInputStream. in)
               (TarArchiveInputStream.))]
    (loop [files []]
      (if-let [entry (.getNextTarEntry in)]
        (recur (conj files {:name (.getName entry)
                            :size (.getSize entry)}))
        files))))


;;
;; JSON handling:
;;
;; Same as with blobs above, this tries to detect the content by checking if it starts with
;; JSON array or object.
;;


(def ^:private start-of-json-data? #{(int \{)
                                     (int \[)})


(defn- json? [^java.io.PushbackInputStream in]
  (let [b1 (.read in)]
    (.unread in b1)
    (start-of-json-data? b1)))


;; Come keys are in CamelCase, some in snake_case. To makes this a bit easier for
;; clojurian eyes this converts the keys to lower-case keywrod. Not perfect, but works
;; for this case.

(def ^:private json-key-fn (comp keyword str/lower-case))


;; The cheshire.core/parse-stream closes the input reader afret the
;; content is parsed to JSON. We need to wrap the input stream to
;; proxy that ignores the close so that we can continue to read the
;; stream.

(defn- non-closeable ^java.io.InputStream [^java.io.InputStream in]
  (proxy [java.io.InputStream] []
    (read
      ([] (.read in))
      ([b] (.read in b))
      ([b off len] (.read in b off len)))
    (close [])))


(defn- read-json [^java.io.InputStream in]
  (-> (non-closeable in)
      (io/reader)
      (json/parse-stream json-key-fn)))


;;
;; Image export handling:
;;

(defn- parse-digest [file-name]
  (when-let [[_ algo hash] (re-matches #"^blobs\/([^/]+)\/([a-z0-9]+)$" file-name)]
    (str algo ":" hash)))


(defn image-export-content-info [^java.io.InputStream in]
  (let [in (TarArchiveInputStream. in)]
    (loop [files {}]
      (if-let [entry (.getNextTarEntry in)]
        (recur (if (.isFile entry)
                 (let [file-name (.getName entry)
                       file-size (.getSize entry)
                       in        (java.io.PushbackInputStream. in 2)]
                   (assoc files file-name (cond
                                            (blob? in) {:type  :blob
                                                        :size  file-size
                                                        :digest (parse-digest file-name)
                                                        :files (read-blob in)}
                                            (json? in) {:type :json
                                                        :size file-size
                                                        :digest (parse-digest file-name)
                                                        :data (read-json in)}
                                            :else  {:type :unknown
                                                    :size file-size})))
                 files))
        files))))


(defn- get-blob [content-info digest]
  (let [[algo hash] (str/split digest #":")
        file-name   (str "blobs/" algo "/" hash)]
    (get content-info file-name)))


(defn content-info-layers
  ([content-info] (content-info-layers content-info nil))
  ([content-info {:keys [os arch]}]
   (let [platform       {:os           (or os "linux")
                         :architecture (or arch (-> (System/getProperty "os.arch")
                                                    {"aarch64" "arm64"
                                                     "amd64"   "amd64"}
                                                    (or (throw (ex-info "unknown os.arch" {})))))}
         manifest       (->> (get content-info "index.json")
                             :data
                             :manifests
                             (first)
                             :digest
                             (get-blob content-info)
                             :data
                             :manifests
                             (some (fn [manifest-info]
                                     (when (-> manifest-info :platform (= platform))
                                       (-> manifest-info :digest))))
                             (get-blob content-info)
                             :data)
         file-layers    (->> manifest
                             :layers
                             (map (fn [layer]
                                    (->> layer
                                         :digest
                                         (get-blob content-info)))))
         config-data    (->> manifest
                             :config
                             :digest
                             (get-blob content-info)
                             :data)
         history-layers (->> config-data 
                             :history)
         #_#_
         config         (->> config-data :config)]
     ;; The history-layers has information of all layers, including layers that don't have any 
     ;; file-system changes. The file-layers has only those layers that have actual files.
     ;;
     ;; This loop iterates over both the history-layers and the files-layers to aggregate information 
     ;; for each layer. If the history indicates that the layer does not have files-layer the 
     ;; files-layers remains unchanged for next loop iteration:
     (loop [layer-infos    []
                     history-layers history-layers
                     file-layers    file-layers]
                (let [history-layer (first history-layers)
                      file-layer    (first file-layers)
                      empty-layer?  (:empty_layer history-layer)]
                  (if (nil? history-layer)
                    layer-infos
                    (let [layer-info (if empty-layer?
                                       history-layer
                                       (merge history-layer (select-keys file-layer [:digest :size :files])))]
                      (recur (conj layer-infos layer-info)
                             (rest history-layers)
                             (if empty-layer?
                               file-layers
                               (rest file-layers))))))))))






(comment
  (def content-info (with-open [in (-> (io/file "docker-overlay-example.tar")
                                       (io/input-stream))]
                      (image-export-content-info in)))
  (-> (content-info-layers content-info)
      (last))
  ;;=> {:created    "2024-10-15T15:49:42.413912838Z"
  ;;    :created_by "RUN /bin/sh -c rm short-lived-file.txt # buildkit"
  ;;    :comment    "buildkit.dockerfile.v0"
  ;;    :digest     "sha256:afe35ecc3b80da0129486041d5a90080bd387bf941b8ab05363d4a8ffaefccb5"
  ;;    :size       145
  ;;    :files      [{:name "hello/"
  ;;                  :size 0}
  ;;                 {:name "hello/.wh.short-lived-file.txt"
  ;;                  :size 0}]}
  )


(defn- humanize-size [size]
  (loop [size           size
         [unit & units] ["B" "kB" "MB" "GB" "TB"]]
    (if (> size 1024)
      (recur (quot size 1024) units)
      [size unit])))


(defn- timestamp-formatter [tz date-format]
  (let [^ZoneId tz (cond
                               (instance? java.time.ZoneId tz) tz
                               (string? tz) (java.time.ZoneId/of tz)
                               (nil? tz) (java.time.ZoneId/systemDefault)
                               :else (throw (ex-info "unknown tz" {})))
        ^DateTimeFormatter date-format (cond
                                         (instance? DateTimeFormatter date-format) date-format
                                         (string? date-format) (DateTimeFormatter/ofPattern date-format)
                                         (nil? date-format) (DateTimeFormatter/ofPattern "yyyy/MM/dd HH:mm:ss")
                                         :else (throw (ex-info "unknown date-format" {})))]
    (fn [time-stamp]
      (-> (ZonedDateTime/parse time-stamp DateTimeFormatter/ISO_OFFSET_DATE_TIME)
          (.withZoneSameInstant tz)
          (.format date-format)))))


(defn explain-layers
  ([layers-info] (explain-layers layers-info nil))
  ([layers-info {:keys [max-files tz date-format columns]}]
   (let [max-files        (or max-files 10)
         format-timestamp (timestamp-formatter tz date-format)
         columns          (or columns
                              (some-> (System/getenv "COLUMNS") (parse-long))
                              120)
         trim             (fn trim
                            ([s] (trim s 0))
                            ([s offset]
                             (let [max-len (- columns offset)]
                               (if (> (count s) max-len)
                                 (str (subs s 0 (- max-len 3)) "...")
                                 s))))]
     (doseq [layer-info layers-info]
       (println (apply str (repeat columns \-)))
       (println "created:" (-> layer-info :created (format-timestamp)))
       (let [command (-> layer-info :created_by)]
         (println "command:" (trim command 9)))
       (when-let [files (-> layer-info :files)]
         (println "digest: " (-> layer-info :digest))
         (println (apply format "size:    %d %s" (-> layer-info :size (humanize-size))))
         (println "files:")
         (doseq [file-info (take max-files files)
                 :let      [file-name    (-> file-info :name)
                            [bytes unit] (-> file-info :size (humanize-size))]]
           (println (format " %4d %-2s  %-40s" bytes unit (trim file-name 10))))
         (let [file-count (count files)]
           (when (> file-count max-files)
             (println " ..." (- file-count max-files) "more files"))))))))


(defn explain-image-layers 
  ([conn image-tag] (explain-image-layers conn image-tag nil))
  ([conn image-tag opts]
   (with-open [in (api/image-export conn image-tag)]
     (-> (image-export-content-info in)
         (content-info-layers opts)
         (explain-layers opts)))))


(comment
  (explain-image-layers nil "example/layers-example:latest" {:columns 80})
  ;
  )