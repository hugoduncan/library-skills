#!/usr/bin/env bb

;;; Runnable examples for babashka.cli

(require '[babashka.cli :as cli])

(println "=== babashka.cli Examples ===\n")

;;; Basic Parsing

(println "1. Basic option parsing:")
(def result1 (cli/parse-opts ["--port" "8080" "--host" "localhost"]))
(println result1)
(println)

;;; Coercion

(println "2. Type coercion:")
(def result2 (cli/parse-opts ["--port" "8080" "--verbose" "true"]
                              {:coerce {:port :long :verbose :boolean}}))
(println result2)
(println)

;;; Aliases

(println "3. Short aliases:")
(def result3 (cli/parse-opts ["-p" "9000" "-v"]
                              {:alias {:p :port :v :verbose}
                               :coerce {:port :long :verbose :boolean}}))
(println result3)
(println)

;;; Boolean Flags

(println "4. Boolean flags:")
(def result4 (cli/parse-opts ["--verbose" "--no-colors"]))
(println result4)
(println)

;;; Collection Handling

(println "5. Collection handling (repeated options):")
(def result5 (cli/parse-opts ["--path" "src" "--path" "test" "--path" "resources"]
                              {:coerce {:path []}}))
(println result5)
(println)

;;; Positional Arguments

(println "6. Positional arguments:")
(def result6 (cli/parse-opts ["deploy" "production" "--force"]
                              {:args->opts [:action :env]
                               :coerce {:force :boolean}}))
(println result6)
(println)

;;; Variable Length Collections

(println "7. Variable length positional args:")
(def result7 (cli/parse-opts ["build" "foo.clj" "bar.clj" "baz.clj"]
                              {:args->opts (cons :cmd (repeat :files))
                               :coerce {:files []}}))
(println result7)
(println)

;;; parse-args with Commands

(println "8. Parse args with subcommands:")
(def result8 (cli/parse-args ["--verbose" "deploy" "prod" "--force"]
                              {:coerce {:verbose :boolean :force :boolean}}))
(println result8)
(println)

;;; Default Values

(println "9. Default values with exec-args:")
(def result9 (cli/parse-args ["--port" "9000"]
                              {:coerce {:port :long}
                               :exec-args {:port 8080 :host "localhost"}}))
(println result9)
(println)

;;; Auto-coercion

(println "10. Auto-coercion (no explicit coerce needed):")
(def result10 (cli/parse-opts ["--enabled" "true" "--count" "42" "--mode" ":prod"]))
(println result10)
(println)

;;; Validation

(println "11. Validation (valid case):")
(try
  (def result11 (cli/parse-args ["--port" "8080"]
                                 {:coerce {:port :long}
                                  :validate {:port pos?}}))
  (println "Valid:" result11)
  (catch Exception e
    (println "Error:" (ex-message e))))
(println)

(println "12. Validation (invalid case - negative port):")
(try
  (cli/parse-args ["--port" "-1"]
                  {:coerce {:port :long}
                   :validate {:port pos?}})
  (catch Exception e
    (println "Error:" (ex-message e))))
(println)

;;; Required Options

(println "13. Required options (missing required):")
(try
  (cli/parse-args ["--name" "myapp"]
                  {:require [:name :version]})
  (catch Exception e
    (println "Error:" (ex-message e))))
(println)

;;; Subcommand Dispatch

(println "14. Subcommand dispatch:")

(defn deploy-cmd [{:keys [opts cmds]}]
  (str "Deploying to: " (first cmds) ", force=" (:force opts)))

(defn rollback-cmd [{:keys [opts cmds]}]
  (str "Rolling back version: " (first cmds)))

(def dispatch-table
  [{:cmds ["deploy"] :fn deploy-cmd}
   {:cmds ["rollback"] :fn rollback-cmd}
   {:cmds [] :fn (fn [_] "No command specified")}])

(def result14 (cli/dispatch dispatch-table ["deploy" "production" "--force"]
                            {:coerce {:force :boolean}}))
(println result14)
(println)

(def result15 (cli/dispatch dispatch-table ["rollback" "v1.2.3"]))
(println result15)
(println)

;;; Help Generation

(println "15. Help text generation:")

(def spec
  {:port {:desc "Port to listen on"
          :coerce :long
          :default 8080}
   :host {:desc "Host address"
          :default "localhost"
          :alias :h}
   :verbose {:desc "Enable verbose output"
             :alias :v
             :coerce :boolean}})

(println (cli/format-opts {:spec spec}))
(println)

;;; Spec to Opts Conversion

(println "16. Convert spec to parse options:")
(def opts-from-spec (cli/spec->opts spec {:exec-args true}))
(println "Generated opts:" opts-from-spec)
(println)

;;; Long Option Variations

(println "17. Long option syntax variations:")
(def result17a (cli/parse-opts ["--port=8080"] {:coerce {:port :long}}))
(def result17b (cli/parse-opts ["--port" "8080"] {:coerce {:port :long}}))
(def result17c (cli/parse-opts [":port" "8080"] {:coerce {:port :long}}))
(println "All equivalent:" result17a result17b result17c)
(println)

;;; Rest Arguments

(println "18. Rest arguments (after --):")
(def result18 (cli/parse-args ["--port" "8080" "--" "file1.txt" "file2.txt"]
                               {:coerce {:port :long}}))
(println result18)
(println)

;;; Merge Opts

(println "19. Merge option specifications:")
(def base-opts {:coerce {:verbose :boolean}})
(def server-opts {:coerce {:port :long} :exec-args {:port 8080}})
(def merged (cli/merge-opts base-opts server-opts))
(println merged)
(println)

(println "=== Examples Complete ===")
