(ns wisp.utils.core
  (:use [clojure.string :only [split]])
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
                                    "posix:*"
                                    (into-array LinkOption [LinkOption/NOFOLLOW_LINKS]))]
    (reduce #(assoc %1 (.getKey %2) (.getValue %2)) {} attrs)))


(defn posix-permissions-string [perm-set]
  "This parses the permissions set into a ls-style string. I used a CL-style cons-construction"
  (let [perm-indices {"OWNER_READ" 0 "OWNER_WRITE" 1 "OWNER_EXECUTE" 2
                      "GROUP_READ" 3 "GROUP_WRITE" 4 "GROUP_EXECUTE" 5
                      "OTHERS_READ" 6 "OTHERS_WRITE" 7 "OTHERS_EXECUTE" 8}
        str-indices (set (vals (select-keys perm-indices (seq perm-set))))]
    (def ^:dynamic result)
    (binding [result '()]
      (doseq [idx (range 8 -1 -1)]
        (if (contains? str-indices idx)
          (cond
           (= (mod idx 3) 0) (set! result (cons \r result))
           (= (mod idx 3) 1) (set! result (cons \w result))
           (= (mod idx 3) 2) (set! result (cons \x result)))
          (set! result (cons \- result))))
      (apply str result))))


(defn cat [filename]
  (println (slurp filename)))

(defn ls [dirname & flags]
  "Currently supported flags: :all :reverse :long"
  (let [path (make-path dirname)
        linkops (into-array LinkOption [LinkOption/NOFOLLOW_LINKS])
        dir (File. dirname)
        flags (set flags)
        files (->> dir
                   (file-seq)
                   (sort-by #(.getName %1))
                   (call-if (flags :reverse) reverse))]
    (doseq [file files]
      (let [filename (.getName file)]
        (if-not (and (= (.charAt filename 0) \.)
                     (not (flags :all)))
          (if (flags :long)
            (let [attrs       (get-attrs file)
                  permissions (str (if (attrs "isDirectory") "d" "-")
                                   (posix-permissions-string (get-permissions (.getAbsolutePath file))))
                  modified    (attrs "lastModifiedTime")
                  owner       (.getName (attrs "owner"))
                  group       (.getName (attrs "group"))
                  size        (attrs "size")
                  name        (.getName file)]
              (printf "%s\t%s\t%s\t%s\t%d\t%s\n" permissions modified owner group size name))
            (println (.getName file))))))))


(ls "./" :long)
