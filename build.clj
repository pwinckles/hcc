(ns build
  (:require [clojure.tools.build.api :as b]
            [clojure.edn :as edn]))

(def project
  (-> (edn/read-string (slurp "deps.edn"))
      :aliases
      :neil
      :project))
(def lib (:name project))

;; use neil project set version 1.2.0 to update the version in deps.edn

(def version (:version project))
(def class-dir "target/classes")
(def basis (b/create-basis {:project "deps.edn"}))
(def uber-file (format "target/%s-%s-standalone.jar" (name lib) version))
(def jar-file (format "target/%s-%s.jar" (name lib) version))

(defn clean
  [_]
  (b/delete {:path "target"}))

(defn jar
  [_]
  (b/write-pom {:class-dir class-dir,
                :lib       lib,
                :version   version,
                :basis     basis,
                :src-dirs  ["src"]})
  (b/copy-dir {:src-dirs   ["src" "resources"],
               :target-dir class-dir})
  (b/jar {:class-dir class-dir,
          :jar-file  jar-file}))

(defn uber
  [_]
  (clean nil)
  (b/copy-dir {:src-dirs   ["src" "resources"],
               :target-dir class-dir})
  (b/compile-clj {:basis     basis,
                  :src-dirs  ["src"],
                  :class-dir class-dir})
  (b/uber {:class-dir class-dir,
           :uber-file uber-file,
           :basis     basis,
           :main      'com.pwinckles.combo-calculator}))
