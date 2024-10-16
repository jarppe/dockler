(ns build
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clojure.tools.build.api :as b]
            [deps-deploy.deps-deploy :as deploy]))


(def lib-name 'jarppe/dockler)


(def lib-id (symbol (str "io.github." lib-name)))
(def src-dir     "./src/main")
(def target-dir  "./target")
(def jar-file    (str target-dir "/" (name lib-name) ".jar"))
(def pom-file    (str target-dir "/pom.xml"))


(defn get-next-version []
  (let [f (io/file "version.edn")
        v (-> (slurp f)
              (edn/read-string)
              (update :build inc))]
    (with-open [out (-> (io/file "version.edn")
                        (io/writer))]
      (.write out (pr-str v)))
    (str (:major v) "."
         (:minor v) "."
         (:build v))))


(defn clean [_]
  (b/delete {:path target-dir}))


(defn build-jar [basis]
  (b/jar {:class-dir src-dir
          :jar-file  jar-file})
  (let [version (get-next-version)]
    (b/write-pom {:basis    basis
                  :lib      lib-id
                  :target   target-dir
                  :version  version
                  :scm      {:url                 (str "https://github.com/" lib-name)
                             :connection          (str "scm:git:git://github.com/" lib-name ".git")
                             :developerConnection (str "scm:git:ssh://git@github.com/" lib-name ".git")
                             :tag                 version}
                  :pom-data [[:licenses [:license
                                         [:name "Eclipse Public License 1.0"]
                                         [:url "https://opensource.org/license/epl-1-0/"]
                                         [:distribution "repo"]]]]})
    (println "built jar: version" version)))


;;
;; Tools API:
;;


#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn build [_]
  (doto (b/create-basis)
    (clean)
    (build-jar)))


#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn deploy [_]
  (when-not (and (System/getenv "CLOJARS_USERNAME")
                 (System/getenv "CLOJARS_PASSWORD"))
    (println "error: missing env: CLOJARS_USERNAME and CLOJARS_PASSWORD are required")
    (System/exit 1))
  (build nil)
  (deploy/deploy {:artifact       jar-file
                  :pom-file       pom-file
                  :installer      :remote
                  :sign-releases? false})
  (let [version (->> (io/file "version.edn")
                     (slurp)
                     (edn/read-string)
                     ((juxt :major :minor :build))
                     (str/join "."))]
    (println "deployed:" (str lib-id " {:mvn/version \"" version "\"}"))))
