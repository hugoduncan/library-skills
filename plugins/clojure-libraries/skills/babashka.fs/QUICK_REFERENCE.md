# Babashka.fs Quick Reference

## Setup
```clojure
(require '[babashka.fs :as fs])
```

## File Checks
```clojure
(fs/exists? path)           ; Does it exist?
(fs/directory? path)        ; Is it a directory?
(fs/regular-file? path)     ; Is it a regular file?
(fs/sym-link? path)         ; Is it a symbolic link?
(fs/hidden? path)           ; Is it hidden?
(fs/readable? path)         ; Can read?
(fs/writable? path)         ; Can write?
(fs/executable? path)       ; Can execute?
```

## Creating
```clojure
(fs/create-file path)                           ; Empty file
(fs/create-dir path)                            ; Single directory
(fs/create-dirs path)                           ; With parents
(fs/create-temp-file)                           ; Temp file
(fs/create-temp-file {:prefix "x-" :suffix ".txt"})
(fs/create-temp-dir)                            ; Temp directory
(fs/create-sym-link "link" "target")           ; Symbolic link
```

## Reading/Writing
```clojure
(slurp path)                                    ; Read as string
(spit path content)                             ; Write string
(fs/read-all-lines path)                       ; Read lines
(fs/write-lines path ["line1" "line2"])        ; Write lines
(fs/read-all-bytes path)                       ; Read bytes
(fs/write-bytes path byte-array)               ; Write bytes
```

## Copying/Moving/Deleting
```clojure
(fs/copy src dest)                             ; Copy file
(fs/copy src dest {:replace-existing true})    ; Overwrite
(fs/copy-tree src dest)                        ; Copy directory
(fs/move src dest)                             ; Move/rename
(fs/delete path)                               ; Delete
(fs/delete-if-exists path)                     ; Delete (no error)
(fs/delete-tree path)                          ; Recursive delete
(fs/delete-on-exit path)                       ; Delete when JVM exits
```

## Listing
```clojure
(fs/list-dir ".")                              ; List directory
(fs/list-dir "." "*.txt")                      ; With glob
(fs/list-dirs ["dir1" "dir2"] "*.clj")        ; Multiple dirs
```

## Searching
```clojure
(fs/glob "." "**/*.clj")                       ; Recursive search
(fs/glob "." "*.{clj,edn}")                    ; Multiple extensions
(fs/match "." "regex:.*\\.clj" {:recursive true})
```

### Common Glob Patterns
```clojure
"*.txt"           ; Files ending in .txt
"**/*.clj"        ; All .clj files recursively
"**{.clj,.cljc}"  ; Multiple extensions recursive
"src/**/*_test.clj"  ; Test files under src/
"data/*.{json,edn}"  ; JSON or EDN in data/
```

## Path Operations
```clojure
(fs/path "dir" "file.txt")                     ; Join paths
(fs/file-name path)                            ; Get filename
(fs/parent path)                               ; Get parent directory
(fs/extension path)                            ; Get extension ("txt")
(fs/split-ext path)                            ; ["name" "ext"]
(fs/strip-ext path)                            ; Remove extension
(fs/components path)                           ; All path parts
(fs/absolutize path)                           ; Make absolute
(fs/relativize base target)                    ; Relative path
(fs/normalize path)                            ; Normalize (remove ..)
(fs/canonicalize path)                         ; Canonical path
```

## Metadata
```clojure
(fs/size path)                                 ; Size in bytes
(fs/creation-time path)                        ; FileTime
(fs/last-modified-time path)                   ; FileTime
(fs/file-time->millis file-time)              ; Convert to ms
(fs/owner path)                                ; Owner (Unix)
(str (fs/owner path))                          ; Owner name
```

## System Paths
```clojure
(fs/home)                                      ; User home
(fs/temp-dir)                                  ; System temp
(fs/cwd)                                       ; Current directory
(fs/exec-paths)                                ; PATH directories
(fs/which "git")                               ; Find executable
```

## XDG Directories (Linux/Unix)
```clojure
(fs/xdg-config-home)                           ; ~/.config
(fs/xdg-config-home "myapp")                   ; ~/.config/myapp
(fs/xdg-data-home)                             ; ~/.local/share
(fs/xdg-cache-home)                            ; ~/.cache
(fs/xdg-state-home)                            ; ~/.local/state
```

## Archives
```clojure
(fs/zip "archive.zip" ["file1" "file2"])      ; Create zip
(fs/unzip "archive.zip" "dest-dir")           ; Extract all
```

## Walking Trees
```clojure
(fs/walk-file-tree root
  {:visit-file (fn [path attrs]
                 (println path)
                 :continue)
   :max-depth 3
   :follow-links false})
```

## Temporary Files
```clojure
;; Auto-cleanup with temp directory
(fs/with-temp-dir [tmp {}]
  (let [f (fs/path tmp "work.txt")]
    (spit f "data")
    (process f)))
;; tmp deleted here

;; Manual temp file
(let [tmp (fs/create-temp-file)]
  (try
    (spit tmp data)
    (process tmp)
    (finally (fs/delete tmp))))
```

## Common Patterns

### Find files modified in last N days
```clojure
(defn recent? [days path]
  (let [cutoff (- (System/currentTimeMillis)
                  (* days 24 60 60 1000))]
    (> (fs/file-time->millis (fs/last-modified-time path))
       cutoff)))

(->> (fs/glob "." "**/*.clj")
     (filter (partial recent? 7)))
```

### Process all files in directory
```clojure
(doseq [f (fs/glob "data" "*.json")]
  (when (fs/regular-file? f)
    (process-file f)))
```

### Safe file write (atomic)
```clojure
(let [target "important.edn"
      tmp (fs/create-temp-file {:dir (fs/parent target)})]
  (try
    (spit tmp data)
    (fs/move tmp target {:replace-existing true})
    (catch Exception e
      (fs/delete-if-exists tmp)
      (throw e))))
```

### Backup file with timestamp
```clojure
(defn backup [path]
  (let [backup-name (str path ".backup."
                        (System/currentTimeMillis))]
    (fs/copy path backup-name)))
```

### Clean old logs
```clojure
(defn clean-old-logs [dir days]
  (->> (fs/glob dir "*.log")
       (remove (partial recent? days))
       (run! fs/delete)))
```

## Tips

✅ **DO:**
- Use `fs/path` to join paths (cross-platform)
- Use `with-temp-dir` for auto-cleanup
- Check `fs/exists?` before operations
- Use glob for finding files
- Filter early in pipelines

❌ **DON'T:**
- Manually concatenate paths with `/`
- Forget to handle missing files
- Use `list-dir` for large directories (use `directory-stream`)
- Forget to close streams (use `with-open`)

## Error Handling
```clojure
;; Check first
(when (fs/exists? "config.edn")
  (process-config))

;; Try-catch for specific errors
(try
  (process-file path)
  (catch java.nio.file.NoSuchFileException e
    (println "File not found"))
  (catch java.nio.file.AccessDeniedException e
    (println "Access denied")))
```
