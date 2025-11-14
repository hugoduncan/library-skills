---
name: babashka.cli
description: Command-line argument parsing for turning Clojure functions into CLIs
---

# babashka.cli

Command-line argument parsing library for transforming Clojure functions into CLIs with minimal effort.

## Overview

babashka.cli converts command-line arguments into Clojure data structures, supporting both keyword-style (`:opt value`) and Unix-style (`--opt value`) arguments. Designed to minimize friction when creating CLIs from existing Clojure functions.

**Key Features:**
- Automatic type coercion
- Flexible argument syntax (`:foo` or `--foo`)
- Subcommand dispatch
- Validation and error handling
- Boolean flags and negative flags
- Collection handling for repeated options
- Default values

**Artifact:** `org.babashka/cli`
**Latest Version:** 0.8.60
**License:** MIT
**Repository:** https://github.com/babashka/cli

## Installation

Add to `deps.edn`:
```clojure
{:deps {org.babashka/cli {:mvn/version "0.8.60"}}}
```

Or `bb.edn` for Babashka:
```clojure
{:deps {org.babashka/cli {:mvn/version "0.8.60"}}}
```

Since babashka 0.9.160, babashka.cli is built-in.

## Core Concepts

### Parsing vs. Args Separation

- `parse-opts` - Returns flat map of parsed options
- `parse-args` - Separates into `:opts`, `:cmds`, `:rest-args`

### Open World Assumption

Extra arguments don't cause errors by default. Use `:restrict` for strict validation.

### Coercion Strategy

Values are coerced based on specifications, not inferred from values alone. This ensures predictable type handling.

## API Reference

### Parsing Functions

#### parse-opts

Parse command-line arguments into options map.

```clojure
(require '[babashka.cli :as cli])

;; Basic parsing
(cli/parse-opts ["--port" "8080"])
;;=> {:port "8080"}

;; With coercion
(cli/parse-opts ["--port" "8080"] {:coerce {:port :long}})
;;=> {:port 8080}

;; With aliases
(cli/parse-opts ["-p" "8080"]
  {:alias {:p :port}
   :coerce {:port :long}})
;;=> {:port 8080}
```

**Options:**
- `:coerce` - Type coercion map (`:boolean`, `:int`, `:long`, `:double`, `:symbol`, `:keyword`)
- `:alias` - Short name to long name mappings
- `:spec` - Structured option specifications
- `:restrict` - Restrict to specified options only
- `:require` - Required option keys
- `:validate` - Validation predicates
- `:exec-args` - Default values
- `:args->opts` - Map positional args to option keys
- `:no-keyword-opts` - Only accept `--foo` style (not `:foo`)
- `:error-fn` - Custom error handler

#### parse-args

Parse arguments with separation of options, commands, and rest args.

```clojure
(cli/parse-args ["--verbose" "deploy" "prod" "--force"]
  {:coerce {:verbose :boolean :force :boolean}})
;;=> {:cmds ["deploy" "prod"]
;;    :opts {:verbose true :force true}
;;    :rest-args []}
```

Returns map with:
- `:opts` - Parsed options
- `:cmds` - Subcommands (non-option arguments)
- `:rest-args` - Arguments after `--`

#### parse-cmds

Extract subcommands from arguments.

```clojure
(cli/parse-cmds ["deploy" "prod" "--force"])
;;=> {:cmds ["deploy" "prod"]
;;    :args ["--force"]}

;; Without keyword opts
(cli/parse-cmds ["deploy" ":env" "prod"]
  {:no-keyword-opts true})
;;=> {:cmds ["deploy" ":env" "prod"]
;;    :args []}
```

### Coercion

#### Type Keywords

- `:boolean` - True/false values
- `:int` - Integer
- `:long` - Long integer
- `:double` - Floating point
- `:symbol` - Clojure symbol
- `:keyword` - Clojure keyword

#### Collection Coercion

Use empty vector to collect multiple values:

```clojure
(cli/parse-opts ["--path" "src" "--path" "test"]
  {:coerce {:path []}})
;;=> {:path ["src" "test"]}
```

Typed collections:

```clojure
(cli/parse-opts ["--port" "8080" "--port" "8081"]
  {:coerce {:port [:long]}})
;;=> {:port [8080 8081]}
```

#### auto-coerce

Automatic coercion for unspecified options (enabled by default):

```clojure
(cli/parse-opts ["--enabled" "true" "--count" "42" "--mode" ":prod"])
;;=> {:enabled true :count 42 :mode :prod}
```

Converts:
- `"true"`/`"false"` → boolean
- Numeric strings → numbers via `edn/read-string`
- Strings starting with `:` → keywords

### Boolean Flags

```clojure
;; Flag present = true
(cli/parse-opts ["--verbose"])
;;=> {:verbose true}

;; Combined short flags
(cli/parse-opts ["-vvv"])
;;=> {:v true}

;; Negative flags
(cli/parse-opts ["--no-colors"])
;;=> {:colors false}

;; Explicit values
(cli/parse-opts ["--force" "false"]
  {:coerce {:force :boolean}})
;;=> {:force false}
```

### Positional Arguments

#### Basic args->opts

Map positional arguments to named options:

```clojure
(cli/parse-opts ["deploy" "production"]
  {:args->opts [:action :env]})
;;=> {:action "deploy" :env "production"}
```

#### Variable Length Collections

Use `repeat` for collecting remaining args:

```clojure
(cli/parse-opts ["build" "foo.clj" "bar.clj" "baz.clj"]
  {:args->opts (cons :cmd (repeat :files))
   :coerce {:files []}})
;;=> {:cmd "build" :files ["foo.clj" "bar.clj" "baz.clj"]}
```

#### Mixed Options and Arguments

```clojure
(cli/parse-opts ["--verbose" "deploy" "prod" "--force"]
  {:coerce {:verbose :boolean :force :boolean}
   :args->opts [:action :env]})
;;=> {:verbose true :action "deploy" :env "prod" :force true}
```

### Validation

#### Required Options

```clojure
(cli/parse-args ["--name" "app"]
  {:require [:name :version]})
;; Throws: Required option: :version
```

#### Restricted Options

```clojure
(cli/parse-args ["--verbose" "--debug"]
  {:restrict [:verbose]})
;; Throws: Unknown option: :debug
```

#### Custom Validators

```clojure
(cli/parse-args ["--port" "0"]
  {:coerce {:port :long}
   :validate {:port pos?}})
;; Throws: Invalid value for option :port: 0

;; With custom message
(cli/parse-args ["--port" "-1"]
  {:coerce {:port :long}
   :validate {:port {:pred pos?
                      :ex-msg (fn [{:keys [option value]}]
                                (str option " must be positive, got: " value))}}})
;; Throws: :port must be positive, got: -1
```

### Default Values

Provide defaults via `:exec-args`:

```clojure
(cli/parse-args ["--port" "9000"]
  {:coerce {:port :long}
   :exec-args {:port 8080 :host "localhost"}})
;;=> {:opts {:port 9000 :host "localhost"}}
```

### Error Handling

Custom error handler:

```clojure
(defn error-handler [{:keys [type cause msg option]}]
  (when (= type :org.babashka/cli)
    (println "Error:" msg)
    (when option
      (println "Option:" option))
    (System/exit 1)))

(cli/parse-args ["--invalid"]
  {:restrict [:valid]
   :error-fn error-handler})
```

Error causes:
- `:restrict` - Unknown option
- `:require` - Missing required option
- `:validate` - Validation failed
- `:coerce` - Type coercion failed

### Subcommand Dispatch

#### dispatch

Route execution based on subcommands:

```clojure
(defn deploy [opts]
  (println "Deploying to" (:env opts)))

(defn rollback [opts]
  (println "Rolling back" (:version opts)))

(def table
  [{:cmds ["deploy"] :fn deploy :args->opts [:env]}
   {:cmds ["rollback"] :fn rollback :args->opts [:version]}
   {:cmds [] :fn (fn [_] (println "No command specified"))}])

(cli/dispatch table ["deploy" "production"])
;; Prints: Deploying to production

(cli/dispatch table ["rollback" "v1.2.3"])
;; Prints: Rolling back v1.2.3
```

#### Nested Subcommands

```clojure
(def table
  [{:cmds ["db" "migrate"] :fn db-migrate}
   {:cmds ["db" "rollback"] :fn db-rollback}
   {:cmds ["db"] :fn (fn [_] (println "db requires subcommand"))}])

(cli/dispatch table ["db" "migrate" "--env" "prod"])
```

#### Dispatch Options

Pass options to parse-args:

```clojure
(cli/dispatch table args
  {:coerce {:port :long}
   :exec-args {:host "localhost"}})
```

The `:fn` receives enhanced parse-args result:
- `:dispatch` - Matched command path
- `:args` - Remaining unparsed arguments
- `:opts` - Parsed options
- `:cmds` - Subcommands

### Formatting & Help

#### format-opts

Generate help text from spec:

```clojure
(def spec
  {:port {:desc "Port to listen on"
          :default 8080
          :coerce :long}
   :host {:desc "Host address"
          :default "localhost"
          :alias :h}
   :verbose {:desc "Enable verbose output"
             :alias :v}})

(println (cli/format-opts {:spec spec}))
;; Output:
;;   --port       Port to listen on (default: 8080)
;;   --host, -h   Host address (default: localhost)
;;   --verbose, -v Enable verbose output
```

With custom indent:

```clojure
(cli/format-opts {:spec spec :indent 4})
```

#### format-table

Format tabular data:

```clojure
(cli/format-table
  {:rows [["Name" "Type" "Default"]
          ["port" "long" "8080"]
          ["host" "string" "localhost"]]
   :indent 2})
```

#### spec->opts

Convert spec to parse options:

```clojure
(def spec
  {:port {:ref "<port>"
          :desc "Server port"
          :coerce :long
          :default 8080}})

(cli/spec->opts spec)
;;=> {:coerce {:port :long}}

(cli/spec->opts spec {:exec-args true})
;;=> {:coerce {:port :long} :exec-args {:port 8080}}
```

### Option Merging

#### merge-opts

Combine multiple option specifications:

```clojure
(def base-opts
  {:coerce {:verbose :boolean}})

(def server-opts
  {:coerce {:port :long}
   :exec-args {:port 8080}})

(cli/merge-opts base-opts server-opts)
;;=> {:coerce {:verbose :boolean :port :long}
;;    :exec-args {:port 8080}}
```

## Common Patterns

### CLI Application Entry Point

```clojure
#!/usr/bin/env bb

(ns my-app
  (:require [babashka.cli :as cli]))

(defn run [{:keys [port host verbose]}]
  (when verbose
    (println "Starting server on" host ":" port))
  ;; ... server logic
  )

(def spec
  {:port {:desc "Port to listen on"
          :coerce :long
          :default 8080}
   :host {:desc "Host address"
          :default "localhost"}
   :verbose {:desc "Enable verbose output"
             :alias :v
             :coerce :boolean}})

(defn -main [& args]
  (cli/parse-args args
    {:spec spec
     :exec-args (:default spec)
     :error-fn (fn [{:keys [msg]}]
                 (println msg)
                 (println)
                 (println "Usage: my-app [options]")
                 (println (cli/format-opts {:spec spec}))
                 (System/exit 1))}))

(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))
```

### Subcommand CLI

```clojure
#!/usr/bin/env bb

(ns my-cli
  (:require [babashka.cli :as cli]))

(defn build [{:keys [opts]}]
  (println "Building with options:" opts))

(defn test [{:keys [opts]}]
  (println "Running tests with options:" opts))

(defn help [_]
  (println "Commands: build, test"))

(def commands
  [{:cmds ["build"]
    :fn build
    :spec {:target {:coerce :keyword}
           :release {:coerce :boolean}}}
   {:cmds ["test"]
    :fn test
    :spec {:watch {:coerce :boolean}}}
   {:cmds []
    :fn help}])

(defn -main [& args]
  (cli/dispatch commands args))

(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))
```

### Configuration File + CLI Override

```clojure
(require '[clojure.edn :as edn])

(defn load-config [path]
  (when (.exists (io/file path))
    (edn/read-string (slurp path))))

(defn run [args]
  (let [file-config (load-config "config.edn")
        cli-opts (cli/parse-args args
                   {:coerce {:port :long
                             :workers :long}})
        final-config (merge file-config (:opts cli-opts))]
    ;; Use final-config
    ))
```

### Babashka Task Integration

In `bb.edn`:

```clojure
{:tasks
 {:requires ([babashka.cli :as cli])

  test {:doc "Run tests"
        :task (let [opts (cli/parse-opts *command-line-args*
                           {:coerce {:watch :boolean}})]
                (when (:watch opts)
                  (println "Running in watch mode"))
                (shell "clojure -M:test"))}}}
```

### Long Option Syntax Variations

```clojure
;; All equivalent
(cli/parse-opts ["--port" "8080"])
(cli/parse-opts ["--port=8080"])
(cli/parse-opts [":port" "8080"])

;; With coercion
(cli/parse-opts ["--port=8080"] {:coerce {:port :long}})
;;=> {:port 8080}
```

### Repeated Options

```clojure
;; Collect into vector
(cli/parse-opts ["--include" "*.clj" "--include" "*.cljs"]
  {:coerce {:include []}})
;;=> {:include ["*.clj" "*.cljs"]}

;; Count occurrences
(defn inc-counter [m k]
  (update m k (fnil inc 0)))

(cli/parse-opts ["-v" "-v" "-v"]
  {:collect {:v inc-counter}})
;;=> {:v 3}
```

### Rest Arguments

Arguments after `--` are collected as `:rest-args`:

```clojure
(cli/parse-args ["--port" "8080" "--" "arg1" "arg2"]
  {:coerce {:port :long}})
;;=> {:opts {:port 8080}
;;    :rest-args ["arg1" "arg2"]}
```

## Error Handling

### Validation Failure Context

```clojure
(defn validate-port [{:keys [value]}]
  (and (pos? value) (< value 65536)))

(cli/parse-args ["--port" "99999"]
  {:coerce {:port :long}
   :validate {:port {:pred validate-port
                      :ex-msg (fn [{:keys [option value]}]
                                (format "%s must be 1-65535, got %d"
                                        option value))}}})
;; Throws: :port must be 1-65535, got 99999
```

### Graceful Degradation

```clojure
(defn safe-parse [args]
  (try
    (cli/parse-args args {:coerce {:port :long}})
    (catch Exception e
      {:error (ex-message e)
       :opts {}})))
```

### Exit Code Handling

```clojure
(defn -main [& args]
  (let [result (cli/parse-args args
                 {:spec spec
                  :error-fn (fn [{:keys [msg]}]
                              (binding [*out* *err*]
                                (println "Error:" msg))
                              1)})]
    (if (number? result)
      (System/exit result)
      (do-work result))))
```

## Use Cases

### Build Tool CLI

```clojure
(def build-commands
  [{:cmds ["compile"]
    :fn compile-project
    :spec {:target {:coerce :keyword
                    :desc "Compilation target"}
           :optimization {:coerce :keyword
                         :desc "Optimization level"}}}
   {:cmds ["package"]
    :fn package-project
    :spec {:format {:coerce :keyword
                    :desc "Package format"}}}])
```

### Configuration Management

```clojure
(defn read-env-config []
  (reduce-kv
   (fn [m k v]
     (if (str/starts-with? k "APP_")
       (assoc m (keyword (str/lower-case (subs k 4))) v)
       m))
   {}
   (System/getenv)))

(defn merged-config [args]
  (let [env-config (read-env-config)
        cli-config (:opts (cli/parse-args args))]
    (merge env-config cli-config)))
```

### Testing Wrapper

```clojure
(defn test-runner [{:keys [opts]}]
  (let [{:keys [namespace watch]} opts]
    (when watch
      (println "Starting test watcher..."))
    (apply clojure.test/run-tests
           (when namespace [(symbol namespace)]))))

(cli/dispatch
  [{:cmds ["test"]
    :fn test-runner
    :spec {:namespace {:desc "Specific namespace"}
           :watch {:coerce :boolean
                   :desc "Watch mode"}}}]
  *command-line-args*)
```

## Performance Considerations

### Minimize Parsing Overhead

For frequently called operations, parse once and pass options:

```clojure
(defn process-files [opts files]
  (doseq [f files]
    (process-file f opts)))

(let [opts (cli/parse-args args)]
  (process-files (:opts opts) (:cmds opts)))
```

### Coercion Functions

Custom coercion functions are called per-value:

```clojure
;; Efficient: Use keywords for built-in types
{:coerce {:port :long}}

;; Less efficient: Custom function for simple types
{:coerce {:port #(Long/parseLong %)}}
```

### Validation Overhead

Validators run after coercion. Use predicates wisely:

```clojure
;; Good: Simple predicate
{:validate {:port pos?}}

;; Avoid: Complex validation in predicate
{:validate {:port (fn [p]
                     (and (pos? p)
                          (< p 65536)
                          (not (contains? reserved-ports p))))}}
```

## Platform Notes

### Babashka Integration

Since babashka 0.9.160, babashka.cli is built-in. Access via `bb -x`:

```bash
bb -x my-ns/my-fn :port 8080 :verbose true
```

### Clojure CLI Integration

Use with `-X` flag:

```bash
clojure -X:my-alias my-ns/my-fn :port 8080
```

Add metadata to functions for specs:

```clojure
(defn ^{:org.babashka/cli {:coerce {:port :long}}}
  start-server [opts]
  (println "Starting on port" (:port opts)))
```

### JVM vs Native

babashka.cli works identically on JVM Clojure and native Babashka with minimal performance differences in parsing itself.

### Cross-Platform Arguments

Quote handling varies by shell:

```bash
# Unix shells
script --name "My App"

# Windows cmd.exe
script --name "My App"

# PowerShell
script --name 'My App'
```

Use positional args to avoid quoting complexity:

```bash
script deploy production  # Better than: script :env "production"
```
