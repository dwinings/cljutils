(ns wisp.utils.core
  (:use [clojure.string :only [split upper-case]])
  (:require [clojure.java.io :as io])
  (:import (java.nio.file Files Path FileSystems LinkOption)
           (java.nio.file.attribute PosixFilePermission)
           (java.nio.fs.UnixFileSystem)
           (java.io File)))

(defmacro call-if [condition func x]
  "If condition is true, return f(x), else return x"
  `(if ~condition (~func ~x) ~x))

(defmacro vararg [type & args]
  "Doesn't do much, just glosses over the technical bits of java vararg."
  (if (and (= 1 (count args)) (vector? (first args)))
    `(into-array ~type ~@args)
    (if (and (= 1 (count args)) (seq? (first args)))
      `(into-array ~type (into [] ~@args))
      `(into-array ~type (into [] (list ~@args))))))

(defn make-path [filename]
  "It turns out that clojure/java interop is a little odd with varargs"
  (let [split-name (split filename #"/")]
    (..
     FileSystems
     (getDefault)
     (getPath (if (empty? (first split-name))
                "/"
                (first split-name))
              (vararg String (rest split-name))))))

(defn get-permissions [filename]
  "Return a set of permissions for a given filename"
  (let [path (make-path filename)
        pfm-obj (Files/getPosixFilePermissions
                 path
                 (vararg LinkOption LinkOption/NOFOLLOW_LINKS))]
    (set (map #(.name %1) pfm-obj))))

(defn get-attrs [^java.io.File file]
  "Gets the posix file attributes and rolls them into a clojure map."
  (let [attrs (Files/readAttributes (make-path (.getAbsolutePath file))
                                    "unix:*"
                                    (vararg LinkOption LinkOption/NOFOLLOW_LINKS))]
    (reduce #(assoc %1 (.getKey %2) (.getValue %2)) {} attrs)))

(defn -num-children [^java.io.File file]
  (if-not (.isDirectory file) 1
          (->> file
               (file-seq)
               (count)
               (+ 1))))


(defn posix-permissions-string [perm-set]
  "This parses the permissions set into a ls-style string. I used a CL-style cons-construction"
  (let [perm-indices {"OWNER_READ" 0 "OWNER_WRITE" 1 "OWNER_EXECUTE" 2
                      "GROUP_READ" 3 "GROUP_WRITE" 4 "GROUP_EXECUTE" 5
                      "OTHERS_READ" 6 "OTHERS_WRITE" 7 "OTHERS_EXECUTE" 8}
        str-indices (set (vals (select-keys perm-indices (seq perm-set))))]
    (loop [indices (range 8 -1 -1)
           result '()]
      (if (empty? indices) 
        (apply str result)
        (let [idx (first indices)]
          (recur (rest indices)
                 (if (contains? str-indices idx)
                   (cond
                    (= (mod idx 3) 0) (cons \r result)
                    (= (mod idx 3) 1) (cons \w result)
                    (= (mod idx 3) 2) (cons \x result))
                   (cons \- result))))))))

(defn cat [filename]
  (println (slurp filename)))

(defrecord LSFile [permissions links owner group size modified-time name])
(defn ls [dirname & flags]
  "Currently supported flags: :all :reverse :long"
  (let [path (make-path dirname)
        dir (File. dirname)
        flags (set flags)
        files (->> dir
                   (.listFiles)
                   (seq))]
    (loop [files files result '()]
      (if (empty? files) result
          (let [file (first files)
                filename (.getName file)]
            (if (and (= (.charAt filename 0) \.)
                     (not (flags :all)))
              (recur (rest files) result)
              (let [attrs       (get-attrs file)
                    permissions (str (if (attrs "isDirectory") "d" "-")
                                     (posix-permissions-string 
                                      (get-permissions (.getAbsolutePath file))))
                    links       (attrs "nlink")
                    modified    (attrs "lastModifiedTime")
                    owner       (.getName (attrs "owner"))
                    group       (.getName (attrs "group"))
                    size        (attrs "size")
                    name        (if (attrs "isDirectory") 
                                  (str (.getName file) "/") (.getName file))]
                (recur (rest files)
                       (cons (LSFile. permissions links owner group size modified name) result)))))))))

(defn -print-ls [lsfiles & flags]
  (let [flags (set flags)
        lsfiles (call-if (:reverse flags) reverse
                         (sort-by #(upper-case (:name %)) lsfiles))]
    (doseq [file lsfiles]
      (if (:long flags)
        (printf "%s %d\t%s\t%s\t%d\t%s\t%s\n"
                (:permissions file)
                (:links file)
                (:owner file)
                (:group file)
                (:size file)
                (:modified-time file)
                (:name file))))))

(defn head [filename]
  (with-open [rdr (io/reader filename)]
    (->> rdr (line-seq) (take 5) (doall))))

(defn tail [filename]
  "I'm sure this is terribly inefficient... but I'm unsure how to make
  a bufferedReader read backwards."
  (with-open [rdr (io/reader filename)]
    (->> rdr (line-seq) (reverse) (take 5) (reverse))))

(defn wc
  ([filename] (wc filename :chars :words :lines))
  ([filename & flags]
     (with-open [rdr (io/reader filename)]
       (let [flags (set flags)]
         (loop [lines (line-seq rdr)
                num-lines 0
                num-words 0
                longest-line 0]
           (if (empty? lines) 
             {:chars ((get-attrs (File. filename)) "size") ;; cheating...
              :lines num-lines 
              :words num-words 
              :longest longest-line
              :name filename}
             (recur (rest lines)
                    (inc num-lines)
                    (+ num-words (count (split (first lines) #"\s+")))
                    (max longest-line (count (first lines))))))))))

(defn -print-wc
  ([counts] (-print-wc counts :lines :words :chars))
  ([counts & flags]
     (let [flags (set flags)]
       (do
         (if (flags :lines)
           (printf "%d\t" (:lines counts)))
         (if (flags :words)
           (printf "%d\t" (:words counts)))
         (if (flags :chars)
           (printf "%d\t" (:chars counts)))
         (if (flags :longest)
           (printf "%d\t" (:longest counts)))
         (printf "%s\n" (:name counts))))))

;; This one was really easy!
(defn grep [regex input & flags]
  (with-open [rdr (io/reader input)]
    (let [flags (set flags)
          pattern (if (string? regex) (re-pattern regex) regex)]
      (loop [lines (line-seq rdr) result '()]
        (if (empty? lines) result
            (recur (rest lines)
                   (if (re-find pattern (first lines))
                     (cons (first lines) result)
                     result)))))))


(defn tee [input file & flags]
  "Prints input to both a file and stdout."
  (let [flags (set flags)]
    (with-open [wrtr (io/writer file
                                (if (:append flags) {:append true} {:append false}))]
      (cond
       (seq? input) (doseq [elem input]
                      (.write wrtr (str elem))
                      (println (str elem)))
       (string? input) (do
                         (.write wrtr input)
                         (println input))))))
