(ns leiningen.lock
  (:require [leiningen.core.classpath :as classpath]
            [leiningen.core.project :as project]
            [leiningen.jar :as jar]
            [pandect.algo.sha1 :as pan]
            [clojure.data :as data]
            [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as str]))

(def default-local-repo ;; cf pomegranate/aether.clj
  (io/file (System/getProperty "user.home") ".m2" "repository"))

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
  "flattens nested output from dependency-hierarchy"
  [graph]
  (if (some? graph)
    (mapcat (fn [[k v]] (cons k (flatten-mapvals v))) graph)
    ()))

(defn parse-dependency-entry
  "Aether via dependency-hierarchy gives a short-hand [symbol ver & kwdpairs] we
  want to parse back into a map. groups and artifacts normalized to strings.
  kwargs could include :exclusions and :scope, which will be nil if not
  present."
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

(defn uberjar-jars
  "cf uberjar.clj"
  [project]
  (let [scoped-profiles (set (project/pom-scope-profiles project :provided))
        default-profiles (set (project/expand-profile project :default))
        provided-profiles (remove
                           (set/difference default-profiles scoped-profiles)
                           (-> project meta :included-profiles))
        project (update-in project [:jar-inclusions]
                           concat (:uberjar-inclusions project))
        whitelisted (select-keys project jar/whitelist-keys)
        project (-> (project/unmerge-profiles project [:default])
                    (merge whitelisted))
        deps (->> (classpath/resolve-dependencies :dependencies project)
                  (filter #(.endsWith (.getName %) ".jar")))]
    (prn "deps:")
    (doall (for [x deps] (prn x)))))

(defn relativize [project] ;; cf pom.clj -- why make these things private?
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

(defn- distinct-key [k xs] ;; cf pom.clj
  ((fn step [seen xs]
     (lazy-seq
      (if (seq xs)
        (let [x (first xs), key (k x)]
          (if (seen key)
            (step seen (rest xs))
            (cons x (step (conj seen key) (rest xs))))))))
   #{} (seq xs)))

(defn pom-deps
  "cf uberjar.clj"
  [project]
  (let [profile-kws (project/non-leaky-profiles project)
        project (-> project
                    (project/unmerge-profiles profile-kws)
                    (vary-meta assoc ::original-project project)
                    relativize)
        original-project (-> project meta ::original-project)
        profile-kws (concat
                     (set/difference
                      (set (project/non-leaky-profiles original-project))
                      (set (project/pom-scope-profiles
                            original-project :provided))
                      (set (project/pom-scope-profiles
                            original-project :test))))
        test-project (-> original-project
                         (project/unmerge-profiles profile-kws)
                         (project/merge-profiles [:test])
                         relativize)
        deps (:dependencies test-project)]
    test-project))

(defn lock
  "I don't do a lot."
  [project & args]
  ;; Gives nil :(
  #_(println "get-deps" (apply classpath/get-dependencies :dependencies project))
  #_(prn  (map project/dependency-map (:dependencies project)))
  (prn "Pom deps:")
  (prn (pom-deps project))
  (let [infos (for [x (classpath/resolve-dependencies :dependencies project)]
                (jar-info x (or (:local-repo project) default-local-repo)))
        infos-vec (->> (for [x (classpath/resolve-dependencies :dependencies project)]
                         ((juxt #(str/join "." (:group %)) :jar-name :sha)
                          (jar-info x (or (:local-repo project) default-local-repo))))
                       (sort))
        flat (map parse-dependency-entry
                  (flatten-mapvals (classpath/dependency-hierarchy :dependencies project)))]
    #_(prn "resolev:")
    #_(doall (for [x infos] (prn x)))
    #_(prn "hier:")
    #_(doall (for [x flat] (prn x)))
    (prn "both:")
    (doall (for [x (merge-dep-info infos flat)]
             (prn ((juxt :group :artifact :version :jar-name :scope :sha) x))))
    #_(prn "count" [(count infos) (count flat)]))
  #_(println "deps: " (map (fn [x] [x ]) )) ; This isn't the maven-esque artifacts though, just jar files
  ;; This has scope and exclusions -- have to reverse engineer
  #_(println "tree: " (classpath/dependency-hierarchy :dependencies project)) ; this is a funny symbol nested list
                                        ; this is a funny symbol nested list
  )
