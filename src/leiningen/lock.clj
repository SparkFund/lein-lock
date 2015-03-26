(ns leiningen.lock
  "checks transitive dependencies against a dependencies.lock file to ensure
  repeatable builds."
  (:require [leiningen.core.classpath :as classpath]
            [leiningen.core.main :as main]
            [leiningen.core.project :as project]
            [leiningen.clean :as clean]
            [leiningen.jar :as jar]
            [leiningen.pom :as pom]
            [clojure.edn :as edn]
            [clojure.data :as data]
            [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.set :as set]
            [clojure.string :as str]
            [pandect.algo.sha1 :as pan]))

(defn local-repo
  "cf pomegranate/aether.clj"
  [project]
  (or (:local-repo project)
      (io/file (System/getProperty "user.home") ".m2" "repository")))

(defn jar-info
  "for jars not in a maven repo structure, eg staged for bundling in an uberjar/war.
  :jar-name and :sha. use m2-jar-info if you can leverage the path higherarchy
  for more info.  (Even though artifact name and version could often be guessed
  from jar name, we don't try it.)"
  [file]
  {:jar-name (.getName file)
   :sha (pan/sha1 file)})

(defn m2-jar-info
  "given a jar io/file in a maven-structured directory.
  This info comes from eg (classpath/resolve-dependencies :dependencies project)
  or uberjar-files -- (i.e. not dependency-hierarchy which doesn't give us the
  jar files, only artifact/group/version etc)."
  [local-repo file]
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

(defn merge-dep-lists
  "a left join, where info from new-list is merged over base-list. If no
  info is in new-list there is an error. (leftovers in new-list is
  ok). Used for getting more accurate :version :scope and :exclusions info from
  eg resolve-dependencies.

  Special logic for only overwriting version number if it's more precise
  w.r.t. SNAPSHOT vs build numbers, otherwise new always overwrites base for
  every key"
  [base-list new-list]
  (let [join-on (juxt :group :artifact)
        new-lookup (into {} (map (juxt join-on identity) new-list))
        snap-check (fn [s o] ;; returns true if s is SNAPSHOT and otherwise matches o (o may have a more precise build number version)
                     (and (re-find #"-SNAPSHOT$" (:version s)) ;; If our base has more precise build number version than SNAPSHOT, don't merge over it with less precise "SNAPSHOT" version
                          (= (first (str/split (:version s) #"-"))
                             (first (str/split (:version o) #"-")))))]
    (for [b base-list]
      (if-let [n (get new-lookup (join-on b))]
        (let [n (if (snap-check n b)
                  (assoc n :version (:version b)) ;; If our base has more precise build number version than SNAPSHOT, don't merge over it with less precise "SNAPSHOT" version
                  n)]
          (assert (or (= (:version b) (:version n)) ;; we can narrow down SNAPSHOT to eg a build number
                      (snap-check b n))
                  (str "Can't unify versions (possibly a :user profile dep is suggesting a different transitive dependency than :dev would, alone? Specifying a version explicitly in :dev might resolve the issue.) " (:artifact b) " b: " (:version b) " and n: " (:version n)))
          (merge b n))
        (throw (Exception. "No extra info found in new-list for " b))))))

(defn uberjar-files
  "cf uberjar.clj, returns fully-qualified list of jar io/files that would be
  included in eg an ubjerar or war (excluding test and plugin deps)"
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
    deps))

(defn relativize [project] ;; cf pom.clj
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

(defn ordered-select-keys
  "returns vec of vec key pairs of a map. suitable for serializing keys in an order"
  [m keys]
  (vec (for [k keys] [k (get m k)])))

(defn dependency-hierarchy-dep-list
  "this is created via classpath/dependency-hierarchy. We learn scope,
  exclusions, and a more accurate version number, but we can't learn the jar
  filename from this. Useful for merging into uberjars merge-dep-lists

  It includes all deps, including testing and plugin deps."
  [project]
  (map parse-dependency-entry
       (flatten-mapvals (classpath/dependency-hierarchy :dependencies project))))

(defn pom-dep-list
  "this is created via classpath/dependency-hierarchy. We learn scope,
  exclusions, and a more accurate version number, but we can't learn the jar
  filename from this.

  It includes test and runtime deps, excluding lein plugins. Can be used for
  pinning test libs in the lockfile, not just runtime libs.

  CAUTION: there is an increased chance you will uncover a transitive dependency
  mismatch, as your :user dependencies will no longer be influencing the
  selection :dev dependencies, so you may need to explicitly state versions for
  on shared transitive deps like tools.namespace in your project.clj :dev profile."
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
                         relativize)]
    (dependency-hierarchy-dep-list test-project)))

(defn resolve-dependencies-dep-list
  "this is created via classpath/resolve-dependencies. We get the jar filenames
  and shas but don't learn SNAPSHOT build version numbers, nor :scope nor :exclusions.

  It includes all deps, including testing and plugin deps."
  [project]
  (for [x (classpath/resolve-dependencies :dependencies project)]
    (m2-jar-info (local-repo project) x)))

(defn uberjar-dep-list
  "this is created via classpath/resolve-dependencies. We get the jar filenames
  and shas but don't learn SNAPSHOT build version numbers, nor :scope nor :exclusions.

  It mimics the logic of uberjar, so will not include plugin or test deps."
  [project]
  (for [x (uberjar-files project)]
    (m2-jar-info (local-repo project) x)))

(defn silent-sh ;; Used only by build-war
  "throws on nonzero return code, returns out otherwise."
  [& args]
  (let [{:keys [exit out]} (apply shell/sh args)]
    (if (= 0 exit)
      out
      (throw (Exception. (str "Process failed, input: " args " out:\n" out))))))

(defn build-war
  "builds war via pom, cf
  https://github.com/pedestal/docs/blob/master/documentation/service-war-deployment.md
  (TODO Probably could be via lein uberjar + web.xml)
  (TODO Split into a different project!)"
  [project]
  (let [target-dir (io/file "target")
        war-staging-dir (io/file target-dir "war")
        inf-dir (io/file war-staging-dir "WEB-INF")
        classdir (io/file inf-dir "classes")
        output-war (io/file target-dir "hello-world-custom.war")]
    (when (.exists war-staging-dir) (clean/delete-file-recursively war-staging-dir))
    (pom/pom project)
    (silent-sh "mvn" "dependency:copy-dependencies" (str "-DoutputDirectory=" (io/as-relative-path (io/file inf-dir "lib"))) "-DincludeScope=runtime")
    (.mkdirs classdir)
    (apply silent-sh "cp" "-R" (map io/as-relative-path (concat (.listFiles (io/file "src"))
                                                                (.listFiles (io/file "config"))
                                                                [classdir])))
    (io/copy (io/file "web.xml") (io/file inf-dir (io/file "web.xml")))
    (silent-sh "jar" "cvf" (io/as-relative-path output-war) "-C" (io/as-relative-path war-staging-dir) (.getName inf-dir))
    (main/info "Wrote " (io/as-relative-path output-war))))

(defn lock
  "'lein lock' checks transitive dependencies against a dependencies.lock file
  to ensure repeatable builds.

  'lein lock freshen' will generate a new dependencies.lock file
  This file should be checked into source control.

  'lein lock echo' will print the contents stdout.

  To customize via project.clj:
  :lock {:lockfile \"different.lock\" ;defaults to dependencies.lock
         :dep-profile :test} ; include :test scoped dependencies in lockfile. defaults to :uberjar
  "
  [project & [command]]
  (let [lockfile (io/file (or (-> project :lock :lockfile) "dependencies.lock"))
        serialize #(ordered-select-keys % [:group :artifact :version :jar-name :sha]) ;; don't include scope as we focus on excluding test deps, which will never have the "test" scope
        [base exts] (let [dp (or (-> project :lock :dep-profile) :uberjar)]
                      (case dp
                        ;; including only uberjar picked deps:
                        :uberjar [(uberjar-dep-list project) (dependency-hierarchy-dep-list project)]
                        ;; vs including test libraries in the lockfile: (more strict, in that transitive dependencies can't resolve any :dev libs differently, even if they aren't going to end up in the final artifact)
                        :test [(pom-dep-list project) (resolve-dependencies-dep-list project)]
                        (throw (Exception. (str "Don't know :dep-profile " dp)))))
        dep-list (merge-dep-lists base exts)
        sorted-deps (sort (map serialize dep-list))]
    (case command
      "freshen" (do (with-open [w (clojure.java.io/writer lockfile)]
                      (doseq [sd sorted-deps]
                        (.write w (str sd))
                        (.newLine w)))
                    (main/info (io/as-relative-path lockfile) "refreshed."))
      "echo" (doseq [x sorted-deps] (main/info (str x)))
      nil (let [lock-entries (with-open [in (java.io.PushbackReader. (io/reader lockfile))]
                               (loop [lock-entries []]
                                 (let [le (edn/read {:eof ::eof} in)]
                                   (if (= ::eof le)
                                     lock-entries
                                     (recur (conj lock-entries le))))))]
            (doall (map (fn [idx sd le]
                          (when (not= sd le) (main/abort "Mismatch on line" (inc idx) "\ncalculated dependency:\n" (str sd) "\ndoes not match lockfile entry:\n" (str le))))
                        (range) sorted-deps lock-entries))
            (main/info "Dependencies match lockfile."))
      "uberwar" (build-war project) ;; Just here for now until we split off to its own lib!!!
      (main/abort "Unknown command:" command "   try `lein lock freshen` or see `lein help lock`"))))
