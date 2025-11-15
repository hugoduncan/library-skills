#!/usr/bin/env bb

(require '[clojure.string :as str])

;; Add Timbre dependency for Babashka
(require '[babashka.deps :as deps])
(deps/add-deps '{:deps {com.taoensso/timbre {:mvn/version "6.8.0"}}})

(require '[taoensso.timbre :as timbre]
         '[taoensso.timbre.appenders.core :as appenders])

(println "\n=== Timbre Logging Examples ===\n")

;; Example 1: Basic Logging Levels
(println "--- Example 1: Basic Logging Levels ---")
(timbre/trace "Trace level - detailed diagnostics")
(timbre/debug "Debug level - debugging information")
(timbre/info "Info level - general information")
(timbre/warn "Warn level - warning message")
(timbre/error "Error level - error occurred")
(timbre/fatal "Fatal level - critical failure")
(timbre/report "Report level - special reporting")

;; Example 2: Formatted Logging
(println "\n--- Example 2: Formatted Logging ---")
(let [username "alice"
      port 8080
      percentage 95.5]
  (timbre/infof "User %s connected" username)
  (timbre/infof "Server started on port %d" port)
  (timbre/infof "Success rate: %.2f%%" percentage))

;; Example 3: Logging with Data
(println "\n--- Example 3: Logging with Data ---")
(timbre/info "User login event"
  {:user-id 123
   :username "bob"
   :ip "192.168.1.100"
   :timestamp (System/currentTimeMillis)})

;; Example 4: Exception Logging
(println "\n--- Example 4: Exception Logging ---")
(try
  (throw (ex-info "Simulated error" {:code 500}))
  (catch Exception e
    (timbre/error e "Operation failed with exception")))

;; Example 5: Spy - Log and Return Value
(println "\n--- Example 5: Spy - Log and Return Value ---")
(defn calculate [x y]
  (* x y))

(let [result (timbre/spy :info (calculate 6 7))]
  (println "Function returned:" result))

;; Example 6: Configuration - Setting Minimum Level
(println "\n--- Example 6: Setting Minimum Level ---")
(println "Current min level:" (:min-level @timbre/*config*))
(timbre/set-min-level! :warn)
(println "After setting to :warn:")
(timbre/debug "This debug message won't appear")
(timbre/warn "This warning will appear")
(timbre/set-min-level! :debug) ; Reset

;; Example 7: Scoped Configuration
(println "\n--- Example 7: Scoped Configuration ---")
(timbre/info "Normal log level")
(timbre/with-min-level :trace
  (timbre/trace "Trace enabled in this scope only"))
(timbre/trace "Back to normal - trace won't show")

;; Example 8: Custom Appender
(println "\n--- Example 8: Custom Appender ---")
(def custom-logs (atom []))

(defn memory-appender
  "Appender that stores logs in memory"
  []
  {:enabled? true
   :async? false
   :fn (fn [data]
         (swap! custom-logs conj
           {:level (:level data)
            :msg (force (:msg_ data))
            :timestamp (force (:timestamp_ data))}))})

(timbre/merge-config!
  {:appenders {:memory (memory-appender)}})

(timbre/info "Message 1 to memory")
(timbre/warn "Message 2 to memory")

(println "Captured logs:" @custom-logs)

;; Restore default config
(timbre/set-config! timbre/default-config)

;; Example 9: Context/MDC Support
(println "\n--- Example 9: Context/MDC Support ---")
(timbre/with-context {:request-id "req-12345" :user-id 456}
  (timbre/info "Processing request")
  (timbre/info "Request completed"))

;; Example 10: Conditional Logging with sometimes
(println "\n--- Example 10: Probabilistic Logging ---")
(println "Attempting 10 sometimes calls (10% probability):")
(dotimes [_ 10]
  (timbre/sometimes 0.1
    (print ".")))
(println " done")

;; Example 11: Rate Limiting
(println "\n--- Example 11: Rate Limiting ---")
(def rate-limited-logs (atom []))

(timbre/merge-config!
  {:appenders
   {:rate-limited
    {:enabled? true
     :rate-limit [[2 1000]] ; Max 2 logs per second
     :fn (fn [data]
           (swap! rate-limited-logs conj (force (:msg_ data))))}}})

(println "Sending 5 messages rapidly:")
(dotimes [i 5]
  (timbre/info (str "Message " (inc i)))
  (Thread/sleep 100))

(println "Messages logged:" (count @rate-limited-logs))
(println "Content:" @rate-limited-logs)

;; Restore default
(timbre/set-config! timbre/default-config)

;; Example 12: Custom Output Function
(println "\n--- Example 12: Custom Output Function ---")
(defn simple-output-fn
  [{:keys [level ?ns-str msg_]}]
  (str (str/upper-case (name level))
       " [" ?ns-str "] "
       (force msg_)))

(timbre/with-merged-config {:output-fn simple-output-fn}
  (timbre/info "Custom formatted message")
  (timbre/error "Another custom format"))

;; Example 13: Namespace Filtering
(println "\n--- Example 13: Namespace Filtering ---")
(timbre/set-ns-min-level!
  {:deny #{"user"} ; Deny current namespace
   :allow #{}})

(timbre/info "This won't appear - namespace filtered")

;; Reset namespace filter
(timbre/set-ns-min-level! {})
(timbre/info "Namespace filter cleared - this appears")

;; Example 14: Log Errors Helper
(println "\n--- Example 14: Log Errors Helper ---")
(defn risky-operation []
  (throw (ex-info "Something went wrong" {:error-code 123})))

(println "Using log-errors (suppresses exception):")
(timbre/log-errors
  (risky-operation))
(println "Execution continued after exception")

;; Example 15: Multiple Appenders
(println "\n--- Example 15: Multiple Appenders ---")
(def console-logs (atom []))
(def file-logs (atom []))

(timbre/set-config!
  {:min-level :debug
   :appenders
   {:console
    {:enabled? true
     :fn (fn [data]
           (swap! console-logs conj (force (:msg_ data))))}
    :file
    {:enabled? true
     :min-level :warn ; Only warnings and above
     :fn (fn [data]
           (swap! file-logs conj (force (:msg_ data))))}}})

(timbre/debug "Debug message")
(timbre/info "Info message")
(timbre/warn "Warning message")
(timbre/error "Error message")

(println "Console appender received:" (count @console-logs) "messages")
(println "File appender received:" (count @file-logs) "messages (warn+)")

;; Example 16: Structured Logging Pattern
(println "\n--- Example 16: Structured Logging Pattern ---")
(timbre/set-config! timbre/default-config)

(defn log-user-action
  [action user-id metadata]
  (timbre/info "User action"
    (merge {:action action
            :user-id user-id
            :timestamp (System/currentTimeMillis)}
           metadata)))

(log-user-action :login 789 {:ip "10.0.0.1" :device "mobile"})
(log-user-action :purchase 789 {:amount 99.99 :currency "USD"})

;; Example 17: Environment-Specific Configuration
(println "\n--- Example 17: Environment-Specific Configuration ---")
(defn config-for-env
  [env]
  (case env
    :dev {:min-level :trace
          :appenders {:println (appenders/println-appender)}}
    :prod {:min-level :info
           :appenders {:println (appenders/println-appender)}}))

(println "Development config:")
(timbre/with-config (config-for-env :dev)
  (timbre/trace "Trace visible in dev"))

(println "Production config:")
(timbre/with-config (config-for-env :prod)
  (timbre/trace "Trace hidden in prod")
  (timbre/info "Info visible in prod"))

;; Example 18: Middleware Example
(println "\n--- Example 18: Middleware Example ---")
(defn add-app-version
  [data]
  (assoc data :app-version "1.0.0"))

(defn add-environment
  [data]
  (assoc data :environment "development"))

(timbre/merge-config!
  {:middleware [add-app-version add-environment]})

(timbre/info "Message with middleware-added data")

;; Reset to default
(timbre/set-config! timbre/default-config)

(println "\n=== Examples Complete ===")
