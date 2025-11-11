#!/usr/bin/env bb
;; Comprehensive babashka.fs examples
;; Run with: bb examples.clj

(ns fs-examples
  (:require [babashka.fs :as fs]
            [clojure.string :as str]))

(println "\n=== BABASHKA.FS EXAMPLES ===\n")

;; Example 1: Basic file operations
(println "1. Basic File Operations")
(fs/with-temp-dir [tmp {}]
  (let [test-file (fs/path tmp "test.txt")]
    (spit test-file "Hello, babashka.fs!")
    (println "  Created:" test-file)
    (println "  Exists?" (fs/exists? test-file))
    (println "  Is file?" (fs/regular-file? test-file))
    (println "  Size:" (fs/size test-file) "bytes")
    (println "  Content:" (slurp test-file))))

;; Example 2: Directory listing and filtering
(println "\n2. Finding Clojure Source Files")
(let [clj-files (->> (fs/glob "." "*.{clj,md}")
                     (map str)
                     (take 5))]
  (println "  Found files:")
  (doseq [f clj-files]
    (println "   -" f)))

;; Example 3: Creating directory structure
(println "\n3. Creating Directory Structure")
(fs/with-temp-dir [tmp {}]
  (let [nested-dir (fs/path tmp "a" "b" "c")]
    (fs/create-dirs nested-dir)
    (println "  Created nested directories:" nested-dir)
    (println "  Directory exists?" (fs/directory? nested-dir))))

;; Example 4: Copying and moving files
(println "\n4. Copy and Move Operations")
(fs/with-temp-dir [tmp {}]
  (let [src (fs/path tmp "source.txt")
        dest (fs/path tmp "destination.txt")
        moved (fs/path tmp "moved.txt")]
    (spit src "Original content")
    (fs/copy src dest)
    (println "  Copied:" src "→" dest)
    (println "  Both exist?" (and (fs/exists? src) (fs/exists? dest)))
    (fs/move dest moved)
    (println "  Moved:" dest "→" moved)
    (println "  Dest exists?" (fs/exists? dest))
    (println "  Moved exists?" (fs/exists? moved))))

;; Example 5: Working with paths
(println "\n5. Path Manipulation")
(let [path "src/project/core.clj"]
  (println "  Original path:" path)
  (println "  File name:" (fs/file-name path))
  (println "  Extension:" (fs/extension path))
  (println "  Parent:" (fs/parent path))
  (println "  Without ext:" (fs/strip-ext path))
  (println "  Absolute:" (str (fs/absolutize path))))

;; Example 6: File metadata
(println "\n6. File Metadata")
(let [this-file *file*]
  (when (and this-file (fs/exists? this-file))
    (println "  This script:" this-file)
    (println "  Size:" (fs/size this-file) "bytes")
    (println "  Modified:" (fs/last-modified-time this-file))
    (println "  Readable?" (fs/readable? this-file))
    (println "  Writable?" (fs/writable? this-file))
    (println "  Executable?" (fs/executable? this-file))))

;; Example 7: Finding executable in PATH
(println "\n7. Finding Executables")
(when-let [bb-path (fs/which "bb")]
  (println "  Found bb at:" bb-path))
(when-let [git-path (fs/which "git")]
  (println "  Found git at:" git-path))

;; Example 8: Glob patterns
(println "\n8. Glob Pattern Matching")
(fs/with-temp-dir [tmp {}]
  ;; Create some test files
  (doseq [file ["data.json" "config.edn" "test.clj" 
                "README.md" "nested/deep.txt"]]
    (let [path (fs/path tmp file)]
      (fs/create-dirs (fs/parent path))
      (spit path "test")))
  
  (println "  All files:")
  (doseq [f (fs/glob tmp "**/*")]
    (when (fs/regular-file? f)
      (println "   -" (fs/relativize tmp f))))
  
  (println "  Just .clj and .edn files:")
  (doseq [f (fs/glob tmp "**/*.{clj,edn}")]
    (println "   -" (fs/relativize tmp f))))

;; Example 9: Recursive directory walking
(println "\n9. Walking Directory Tree")
(fs/with-temp-dir [tmp {}]
  ;; Create structure
  (doseq [dir ["a" "a/b" "a/b/c"]]
    (fs/create-dirs (fs/path tmp dir))
    (spit (fs/path tmp dir "file.txt") "test"))
  
  (println "  Directory structure:")
  (fs/walk-file-tree tmp
    {:pre-visit-dir (fn [path _]
                      (let [depth (count (fs/components 
                                          (fs/relativize tmp path)))]
                        (println (str (apply str (repeat depth "  "))
                                     "📁 " (fs/file-name path))))
                      :continue)
     :visit-file (fn [path _]
                   (let [depth (count (fs/components 
                                       (fs/relativize tmp path)))]
                     (println (str (apply str (repeat depth "  "))
                                  "📄 " (fs/file-name path))))
                   :continue)}))

;; Example 10: File filtering pipeline
(println "\n10. File Filtering Pipeline")
(fs/with-temp-dir [tmp {}]
  ;; Create test files with different sizes
  (doseq [[name content] [["small.txt" "x"]
                          ["medium.txt" (apply str (repeat 100 "x"))]
                          ["large.txt" (apply str (repeat 1000 "x"))]]]
    (spit (fs/path tmp name) content))
  
  (let [files (->> (fs/list-dir tmp)
                   (filter fs/regular-file?)
                   (map (fn [path]
                          {:name (fs/file-name path)
                           :size (fs/size path)}))
                   (sort-by :size))]
    (println "  Files by size:")
    (doseq [{:keys [name size]} files]
      (println (format "   - %s: %d bytes" name size)))))

;; Example 11: XDG directories (Unix/Linux)
(println "\n11. XDG Base Directories")
(try
  (println "  Config home:" (fs/xdg-config-home))
  (println "  Data home:" (fs/xdg-data-home))
  (println "  Cache home:" (fs/xdg-cache-home))
  (println "  App config:" (fs/xdg-config-home "myapp"))
  (catch Exception _
    (println "  (XDG directories not available on this platform)")))

;; Example 12: Temporary files and cleanup
(println "\n12. Temporary File Management")
(let [temp-file (fs/create-temp-file {:prefix "demo-" 
                                        :suffix ".txt"})]
  (println "  Created temp file:" temp-file)
  (spit temp-file "Temporary data")
  (println "  Content:" (slurp temp-file))
  (fs/delete temp-file)
  (println "  Deleted:" (not (fs/exists? temp-file))))

(println "\n13. Working with temp directory context")
(fs/with-temp-dir [tmp-dir {:prefix "work-"}]
  (println "  Working in:" tmp-dir)
  (let [work-file (fs/path tmp-dir "work.txt")]
    (spit work-file "work data")
    (println "  Created file:" work-file)
    (println "  File exists:" (fs/exists? work-file)))
  (println "  (Directory will be deleted after this block)"))

(println "\n=== All examples completed! ===\n")
