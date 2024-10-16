(ns dockler.util.explain-image-layers
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [cheshire.core :as json]
            [dockler.api :as api])
  (:import (java.util.zip GZIPInputStream)
           (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)
           (java.time ZonedDateTime)
           (java.time.format DateTimeFormatter)
           (org.apache.commons.compress.archivers.tar TarArchiveInputStream
                                                      TarArchiveEntry)))


;;
;; This implementation aims to be compatible with Babashka. The commons compress
;; is not included in babashka, so this works only in JVM clojure.
;;


(defn- entry-seq [^TarArchiveInputStream in]
  (when-let [entry (.getNextTarEntry in)]
    (cons entry (lazy-seq (entry-seq in)))))


(defn- get-fs-layer-file-info [in]
  (->> in
       (GZIPInputStream.)
       (TarArchiveInputStream.)
       (entry-seq)
       (filter (fn [^TarArchiveEntry entry]
                 (or (.isDirectory entry)
                     (.isFile entry))))
       (mapv (fn [^TarArchiveEntry entry]
               [(.getName entry)
                (.getSize entry)]))))


(defn image-layers-info
  ([conn image-name] (image-layers-info conn image-name nil))
  ([conn image-name {:keys [os arch]}]
   (let [platform  {:os           (or os "linux")
                    :architecture (or arch (-> (System/getProperty "os.arch")
                                               {"aarch64" "arm64"
                                                "amd64"   "amd64"}
                                               (or (throw (ex-info "unknown os.arch" {})))))}
         temp-dir  (-> (Files/createTempDirectory "image-export-" (make-array FileAttribute 0))
                       (.toFile))
         open-blob (fn [file-name]
                     (let [blob-name (if (str/starts-with? file-name "sha256:")
                                       (str "blobs/sha256/" (subs file-name (count "sha256:")))
                                       file-name)]
                       (-> (io/file temp-dir blob-name)
                           (io/input-stream))))
         json-blob (fn [file-name]
                     (with-open [in (-> (open-blob file-name)
                                        (io/reader))]
                       (json/parse-stream in (comp keyword str/lower-case))))]
     ;; Get image export stream, expand the context to temp-dir:
     (with-open [in (-> (api/image-export conn image-name)
                        (TarArchiveInputStream.))]
       (loop []
         (when-let [entry (.getNextEntry in)]
           (let [target (io/file temp-dir (.getName entry))]
             (cond
               (.isDirectory entry) (.mkdirs target)
               (.isFile entry)      (io/copy in target)))
           (recur))))
     (let [;; Get index, parse nested blobs to get layer information:
           manifest       (->> (json-blob "index.json")
                               :manifests
                               (first)
                               :digest
                               (json-blob)
                               :manifests
                               (some (fn [manifest-info]
                                       (when (-> manifest-info :platform (= platform))
                                         (-> manifest-info :digest))))
                               (json-blob))
           ;; Get "history" info of the image layers, this includes when layer was created, by 
           ;; what command, etc:
           history-layers (-> manifest
                              :config
                              :digest
                              (json-blob)
                              :history)
           ;; Get file-system layer information, this includes the digest of blob that contains the 
           ;; gzipped tar of the layer files:
           fs-layers      (-> manifest
                              :layers)]
       ;; The history-layers has information of all layers, including layers that don't have any 
       ;; file-system changes. The fs-layers has only those layers that have files system changes.
       ;;
       ;; This loop iterates over both the history-layers and the fs-layers to aggregate information 
       ;; for each layer. If the history indicates that the layer does not have fs-layer the fs-layers
       ;; remains unchanged for next loop iteration:
       (loop [layer-info []
              histories  history-layers
              layers     fs-layers]
         (let [history      (first histories)
               layer        (first layers)
               empty-layer? (:empty_layer history)]
           (if (nil? history)
             layer-info
             (let [info (if empty-layer?
                          history
                          (assoc history
                                 :layer layer
                                 :files (with-open [in (-> layer :digest (open-blob))]
                                          (get-fs-layer-file-info in))))]
               (recur (conj layer-info info)
                      (rest histories)
                      (if empty-layer?
                        layers
                        (rest layers)))))))))))


(defn- humanize-size [size]
  (loop [size           size
         [unit & units] ["B" "kB" "MB" "GB" "TB"]]
    (if (<= size 1024)
      [size unit]
      (recur (quot size 1024) units))))


(defn timestamp-formatter [tz date-format]
  (let [tz          (cond
                      (instance? java.time.ZoneId tz) tz
                      (string? tz) (java.time.ZoneId/of tz)
                      (nil? tz) (java.time.ZoneId/systemDefault)
                      :else (throw (ex-info "unknown tz" {})))
        date-format (cond
                      (instance? DateTimeFormatter date-format) date-format
                      (string? date-format) (DateTimeFormatter/ofPattern date-format)
                      (nil? date-format) (DateTimeFormatter/ofPattern "yyyy/MM/dd HH:mm:ss")
                      :else (throw (ex-info "unknown date-format" {})))]
    (fn [time-stamp]
      (-> (ZonedDateTime/parse time-stamp DateTimeFormatter/ISO_OFFSET_DATE_TIME)
          (.withZoneSameInstant tz)
          (.format date-format)))))


(defn explain-image-layers
  ([layers-info] (explain-image-layers layers-info nil))
  ([layers-info {:keys [max-files tz date-format]}]
   (let [max-files        (or max-files 10)
         format-timestamp (timestamp-formatter tz date-format)]
     (doseq [layer-info layers-info]
       (println "--------------------------------------------------------------------------------")
       (println "created:" (-> layer-info :created (format-timestamp)))
       (let [command (-> layer-info :created_by)]
         (println "command:" (if (> (count command) 68)
                               (str (subs command 0 68) "...")
                               command)))
       (when-let [layer (-> layer-info :layer)]
         (println (apply format "   size: %d %s" (-> layer :size (humanize-size))))
         (println " digest:" (-> layer :digest))
         (println)
         (doseq [[file-name file-size] (->> layer-info :files (take max-files))]
           (println (apply format " %-40s  %4d %s" file-name (humanize-size file-size))))
         (let [file-count (-> layer-info :files (count))]
           (when (> file-count max-files)
             (println " ..." (- file-count max-files) "more files"))))))))


(comment
  (def layer-info (with-open [conn (api/connect)]
                    (image-layers-info conn "docker-overlay-example")))
  (explain-image-layers layer-info)
  ;
  )
