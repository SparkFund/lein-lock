(ns leiningen.lock
  (:require [leiningen.core.classpath :as classpath]
            [leiningen.core.project :as project]
            [pandect.algo.sha1 :as pan]
            [clojure.data :as data]
            [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as str]))

(def default-local-repo ;; cf pomegranate/aether.clj
  (io/file (System/getProperty "user.home") ".m2" "repository"))

(defn relativize [project] ;; cf pom.xml
  (let [root (str (:root project) (System/getProperty "file.separator"))]
    (reduce #(update-in %1 [%2]
                        (fn [xs]
                          (if (sequential? xs)
                            (vec (for [x xs]
                                   (.replace x root "")))
                            (and xs (.replace xs root "")))))
            project
            [:target-path :compile-path :source-paths :test-paths
             :resource-paths :java-source-paths])))

(defn jar-info
  "Breaks a jar deps path, assumes they're in a maven structured directory.
  This info comes form (classpath/resolve-dependencies :dependencies project)
  instead of dependency-hierarchy (which doesn't give us the jar files)."
  [file local-repo]
  (let [split-path (fn [file] (loop [f file
                                     acc (list (.getName file))]
                                (if-let [p (.getParentFile f)]
                                  (recur p (cons (.getName p) acc))
                                  acc)))
        local-split (split-path local-repo)
        [abs rel] (split-at (count local-split) (split-path file))
        rev (reverse rel)]
    (assert (= local-split abs) (str "could not relativize dependency " file))
    {:jar-name (nth rev 0) ;; includes artifact and version in the name
     :version (nth rev 1)
     :artifact (nth rev 2)
     :group (str/join "." (reverse (drop 3 rev)))
     :sha (pan/sha1 file)}))

(defn flatten-mapvals
  [graph]
  (if (some? graph)
    (mapcat (fn [[k v]] (cons k (flatten-mapvals v))) graph)
    ()))

(defn parse-dependency-entry
  "Aether gives a short-hand [symbol ver & kwdpairs] we want to parse back into
  a map. groups and artifacts normalized to strings.
   kwargs could include :exclusions and :scope, which will be nil if not present."
  [[name-sym version & kwargs]]
  (let [kws (apply hash-map kwargs)]
    {:version version
     :artifact (name name-sym)
     :group (if (namespace name-sym)
              (namespace name-sym)
              (name name-sym))
     :scope (:scope kws)
     :exclusions (:exclusions kws)}))

(defn merge-dep-info
  "takes two lists of parsed dependency entries and reconciles them 1:1 (to
  unify jar+hash from and :scope+:exclusions info) -- any leftovers or mismatch
  on group+artifact+version means something is missing from one side or the
  other."
  [hierarchy-list resolve-list]
  (let [join-on (juxt :group :artifact :version)]
    (assert (= (count hierarchy-list)
               (count resolve-list)) "Dep lists differ in size")
    (map (fn [h r]
           (assert (or (= (join-on h) (join-on r))
                       (and (re-find #"-SNAPSHOT$" (:version h))  ;; resolve lists get dates as minor version numbers instead of SNAPSHOT, not sure who supplies this?
                            (= (:group h) (:group r))
                            (= (:artifact h) (:artifact r))
                            (= (first (str/split (:version h) #"-"))
                               (first (str/split (:version r) #"-")))))
                   (str "Deps don't align:" {:h h :r r}))
           (merge h r))
         (sort-by join-on hierarchy-list)
         (sort-by join-on resolve-list))))

(defn lock
  "I don't do a lot."
  [project & args]
  ;; Gives nil :(
  (println "get-deps" (apply classpath/get-dependencies :dependencies project))
  #_(prn  (map project/dependency-map (:dependencies project)))
  (let [original-project (-> project meta ::original-project)
        profile-kws (concat
                     (set/difference
                      (set (project/non-leaky-profiles original-project))
                      (set (project/pom-scope-profiles
                            original-project :provided))
                      (set (project/pom-scope-profiles
                            original-project :test))))
        test-project 3 #_(-> original-project
                             (project/unmerge-profiles profile-kws)
                             (project/merge-profiles [:test])
                             relativize)
        deps (:dependencies test-project)]
    (prn original-project #_(map project/dependency-map deps)))
  (let [infos (for [x (classpath/resolve-dependencies :dependencies project)]
                (jar-info x (or (:local-repo project) default-local-repo)))
        infos-vec (->> (for [x (classpath/resolve-dependencies :dependencies project)]
                         ((juxt #(str/join "." (:group %)) :jar-name :sha)
                          (jar-info x (or (:local-repo project) default-local-repo))))
                       (sort))
        flat (map parse-dependency-entry
                  (flatten-mapvals (classpath/dependency-hierarchy :dependencies project)))]
    (prn "resolev:")
    (doall (for [x infos] (prn x)))
    (prn "hier:")
    (doall (for [x flat] (prn x)))
    (prn "both:")
    (prn (merge-dep-info infos flat))
    #_(prn "count" [(count infos) (count flat)]))
  #_(println "deps: " (map (fn [x] [x ]) )) ; This isn't the maven-esque artifacts though, just jar files
  ;; This has scope and exclusions -- have to reverse engineer
  #_(println "tree: " (classpath/dependency-hierarchy :dependencies project)) ; this is a funny symbol nested list
                                        ; this is a funny symbol nested list
  )
