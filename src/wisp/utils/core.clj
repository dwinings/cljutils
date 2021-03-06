(ns wisp.utils.core
  (:use [clojure.string :only [split upper-case]])
  (:require [clojure.java.io :as io])
  (:import (java.nio.file Files Path FileSystems LinkOption)
           (java.nio.file.attribute PosixFilePermission)
           (java.nio.fs.UnixFileSystem)
           (java.io File StringReader BufferedReader)))

(defmacro call-if [condition func x]
  "If condition is true, return f(x), else return x"
  `(if ~condition (~func ~x) ~x))

(defn str-reader [s]
  "Simple function that returns a bufferedReader of the given string."
  (-> s (StringReader.) (BufferedReader.)))

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
       (let [flags (set flags)]
         (with-open [rdr (if (:string flags) (str-reader filename) (io/reader filename))]
           (loop [lines (line-seq rdr)
                  num-lines 0
                  num-words 0
                  longest-line 0]
             (if (empty? lines) 
               {:chars (if (:string flags) (.length filename) ((get-attrs (File. filename)) "size")) ;; cheating...
                :lines num-lines 
                :words num-words 
                :longest longest-line
                :name (if (:string flags) "<String>" filename)}
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
    (with-open [wrtr (io/writer file :append (boolean (:append flags)))]
      (cond
       (or (vector? input)
           (seq? input)) (doseq [elem input]
                           (.write wrtr (str elem "\n"))
                           (println (str elem)))
       (string? input) (do
                         (.write wrtr (str input "\n"))
                         (println input))))))

(defn segment-str [^String str len]
  "I tried this using a vector and no reverse call. There was no speedup."
  (loop [pos 0
         result '()]
    (if (> (.length str) (+ pos len))
      (recur
       (+ pos len)
       (cons (subs str pos (+ len pos)) result))
      ;; Dorun here is a major speedup.
      ;; All of the lazy cons thunks were killing my performance.
      (reverse (cons (subs str pos (.length str)) result)))))

;; 2014-Mar-06 06:56:24 -0500 flexibility INFO [wisp.utils.core] - Profiling: :wisp.utils.core/fold
;;                                           Id  Calls       Min        Max       MAD      Mean   Time% Time
;;               :taoensso.timbre.profiling/map  16163     9.0μs    101.0μs     768ns    14.0μs      47 228.0ms
;;       :taoensso.timbre.profiling/segment-str  16163     8.0μs     99.0μs     704ns    12.0μs      42 202.0ms
;;             :taoensso.timbre.profiling/dorun  16163     3.0μs     68.0μs     190ns     3.0μs      10 48.0ms
;;           :taoensso.timbre.profiling/reverse  16163     826ns     63.0μs      90ns     1.0μs       4 18.0ms
;;         :taoensso.timbre.profiling/len-check  32519     221ns     42.0μs      37ns     291ns       2 9.0ms
;;              :taoensso.timbre.profiling/cons  16356     298ns      3.0μs      50ns     505ns       2 8.0ms
;;         :taoensso.timbre.profiling/next-line  17158     215ns     26.0μs      54ns     296ns       1 5.0ms
;;       :taoensso.timbre.profiling/line-length  17158      91ns     10.0μs      30ns     130ns       0 2.0ms
;; :taoensso.timbre.profiling/make-empty-result  16163      55ns      647ns       6ns      73ns       0 1.0ms
;;                                 Clock Time                                                     100 486.0ms
;;                             Accounted Time                                                     107 522.0ms
(defn fold [filename & flags]
  ;; I need to write a flagify function.
  (let [flags (apply hash-map flags)
        width (if (flags :width) (flags :width) 80)
        splitter-fn (if (:spaces flags)
                      #(split %1 #"\s+")
                      #(doall (segment-str %1 width)))
        reader-fn #(if (:string flags) (str-reader %1) (io/reader %1))]
    (with-open [rdr (reader-fn filename)]
      ;; This would most likely have been cleaner with doseq.
      ;; Also, fold *has* to conflict with a function somewhere.
      ;; Actually, this is faster than my doseq version.
      ;; I profiled this.
      (loop [lines (line-seq rdr)]
        (let [^String line (first lines)
              line-length (.length line)
              remaining (rest lines)
              split-line (splitter-fn line)]
          (doseq [section split-line]
            (println section))
          (if-not (empty? remaining)
            (recur remaining)))))))

;; This was actually no faster.
; (defn fold [filename & flags]
;   (let [flags (set flags)]
;     (with-open [rdr (io/reader filename)]
;       (doseq [line (line-seq rdr)]
;         (loop [line line]
;           (if (> (.length line) 80)
;             (do
;               (println (subs line 0 80))
;               (recur (subs line 80 (.length line))))
;             (println line)))))))
            
             
