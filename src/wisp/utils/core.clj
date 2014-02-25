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

(defn make-path [filename]
  "It turns out that clojure/java interop is a little odd with varargs"
  (let [split-name (split filename #"/")]
    (..
     FileSystems
     (getDefault)
     (getPath (if (empty? (first split-name))
                "/"
                (first split-name))
              (into-array String (rest split-name))))))

(defn get-permissions [filename]
  "Return a set of permissions for a given filename"
  (let [path (make-path filename)
        pfm-obj (Files/getPosixFilePermissions path (into-array LinkOption [LinkOption/NOFOLLOW_LINKS]))]
    (set (map #(.name %1) pfm-obj))))

(defn get-attrs [^java.io.File file]
  "Gets the posix file attributes and rolls them into a clojure map."
  (let [attrs (Files/readAttributes (make-path (.getAbsolutePath file))
                                    "unix:*"
                                    (into-array LinkOption [LinkOption/NOFOLLOW_LINKS]))]
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
    (let [result (ref '())]
      (doseq [idx (range 8 -1 -1)]
        (if (contains? str-indices idx)
          (dosync (cond
                   (= (mod idx 3) 0) (ref-set result (cons \r @result))
                   (= (mod idx 3) 1) (ref-set result (cons \w @result))
                   (= (mod idx 3) 2) (ref-set result (cons \x @result))))
          (dosync (ref-set result (cons \- @result)))))
        (apply str @result))))


(defn cat [filename]
  (println (slurp filename)))

(defn ls [dirname & flags]
  "Currently supported flags: :all :reverse :long"
  (let [path (make-path dirname)
        linkops (into-array LinkOption [LinkOption/NOFOLLOW_LINKS])
        dir (File. dirname)
        flags (set flags)
        files (->> dir
                   (.listFiles)
                   (seq)
                   (sort-by #(clojure.string/upper-case (.getName %1)))
                   (call-if (flags :reverse) reverse))]
    (doseq [file files]
      (let [filename (.getName file)]
        (if-not (and (= (.charAt filename 0) \.)
                     (not (flags :all)))
          (if (flags :long)
            (let [attrs       (get-attrs file)
                  permissions (str (if (attrs "isDirectory") "d" "-")
                                   (posix-permissions-string (get-permissions (.getAbsolutePath file))))
                  links       (attrs "nlink")
                  modified    (attrs "lastModifiedTime")
                  owner       (.getName (attrs "owner"))
                  group       (.getName (attrs "group"))
                  size        (attrs "size")
                  name        (if (attrs "isDirectory") (str (.getName file) "/") (.getName file))]
              (printf "%s %d\t%s\t%s\t%d\t%s\t%s\n" permissions links owner group size modified name))
            (println (.getName file))))))))

(defn head [filename]
  (with-open [rdr (io/reader filename)]
    (doseq [s (take 5 (line-seq rdr))]
      (println s))))

(defn tail [filename]
  "I'm sure this is terribly inefficient... but I'm unsure how to make a bufferedReader read backwards."
  (with-open [rdr (io/reader filename)]
    (doseq [s (->> rdr (line-seq) (reverse) (take 5) (reverse))]
      (println s))))

(defn wc
  ([filename] (wc filename :chars :words :lines))
  ([filename & flags]
   (let [flags        (set flags)
         lines        (atom 0)
         words        (atom 0)
         longest-line (atom 0)
         chrs         ((get-attrs (File. filename)) "size")] ;; Why not cheat?
     (with-open [rdr (io/reader filename)]
       (doseq [line (line-seq rdr)]
         (swap! lines inc)
         (swap! longest-line #(max %1 (count line)))
         (swap! words #(+ (count (split line #"\s+")) %1)))
       (if (flags :lines)
         (printf "%d\t" @lines))
       (if (flags :words)
         (printf "%d\t" @words))
       (if (flags :chars)
         (printf "%d\t" chrs))
       (if (flags :longest-line)
         (printf "%d\t" @longest-line))
       (printf "%s\n" filename)))))

