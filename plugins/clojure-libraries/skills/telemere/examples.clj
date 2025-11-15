#!/usr/bin/env bb

;;; Runnable examples for Telemere

(require '[taoensso.telemere :as t])

(println "=== Telemere Examples ===\n")

;;; Basic Logging

(println "1. Basic logging with levels:")
(t/log! :info "Application started")
(t/log! :warn "Memory usage high")
(t/log! :error "Connection failed")
(println)

;;; Structured Logging

(println "2. Structured logging with data:")
(t/log! {:level :info
         :data {:user-id 123 :action "login"}})
(t/log! :info ["User action:" {:user-id 123 :action "purchase"}])
(println)

;;; Event Logging

(println "3. Event logging:")
(t/event! :user-signup)
(t/event! :payment-processed :info)
(t/event! :cache-miss :warn {:data {:key "user:123"}})
(println)

;;; Tracing

(println "4. Tracing execution:")
(defn add [a b]
  (+ a b))

(def result (t/trace! :addition
              (add 2 3)))
(println "Trace result:" result)
(println)

;;; Nested Tracing

(println "5. Nested tracing:")
(defn outer-fn []
  (t/trace! :outer
    (do
      (t/trace! :inner-1 (+ 1 2))
      (t/trace! :inner-2 (* 3 4)))))

(outer-fn)
(println)

;;; Spy

(println "6. Spy on expressions:")
(def spy-result
  (->> [1 2 3 4 5]
       (map inc)
       (t/spy! :debug)
       (filter even?)
       (reduce +)))
(println "Spy result:" spy-result)
(println)

;;; Error Logging

(println "7. Error logging:")
(t/error! (ex-info "Test error" {:reason :demo}))
(t/error! :api-error (ex-info "API failed" {:status 500}))
(println)

;;; Catch Errors

(println "8. Catch and log errors:")
(defn risky-operation []
  (throw (ex-info "Something went wrong" {})))

(def catch-result
  (t/catch->error! :risky-op
    (risky-operation)))
(println "Catch result (should be nil):" catch-result)
(println)

;;; Minimum Level

(println "9. Minimum level filtering:")
(t/set-min-level! :warn)
(t/log! :debug "This won't appear")
(t/log! :warn "This will appear")
(t/set-min-level! :info)
(println)

;;; With Min Level

(println "10. Temporary level override:")
(t/set-min-level! :warn)
(t/with-min-level :debug
  (t/log! :debug "Appears inside with-min-level"))
(t/log! :debug "Doesn't appear outside")
(t/set-min-level! :info)
(println)

;;; Capturing Signals

(println "11. Capture signal for inspection:")
(def captured
  (t/with-signal
    (t/log! {:level :info
             :id :test-signal
             :data {:x 42}})))
(println "Captured signal ID:" (:id captured))
(println "Captured signal data:" (:data captured))
(println)

;;; Multiple Signals

(println "12. Capture multiple signals:")
(def signals
  (t/with-signals
    (t/log! :info "First")
    (t/log! :warn "Second")
    (t/event! :third)))
(println "Captured" (count signals) "signals")
(println)

;;; Signal with Run

(println "13. Signal with run (deferred execution):")
(t/log! {:level :debug
         :run (do
                (println "  Computing expensive value...")
                {:result 42})}
  "Expensive computation")
(println)

;;; Signal with ID

(println "14. Signals with IDs for filtering:")
(t/log! {:id :user-action
         :level :info
         :data {:user-id 456 :action "logout"}})
(t/event! :db-query :debug {:data {:table "users" :duration-ms 45}})
(println)

;;; Sampling

(println "15. Signal sampling (10%):")
(dotimes [i 20]
  (t/log! {:level :debug
           :sample-rate 0.1
           :data {:iteration i}}
    "Sampled signal"))
(println "  (Only ~2 signals should have appeared)")
(println)

;;; Rate Limiting

(println "16. Rate limiting (2 per second):")
(dotimes [i 10]
  (t/log! {:level :info
           :rate-limit {"demo-limit" [2 1000]}
           :data {:iteration i}}
    "Rate limited signal")
  (Thread/sleep 100))
(println "  (Only first 2 signals should have appeared)")
(println)

;;; Namespace Filtering

(println "17. Namespace filtering:")
(t/set-ns-filter! {:disallow #{"user"}})
(t/log! :info "This might be filtered based on namespace")
(t/set-ns-filter! nil)
(println)

;;; Custom Handler

(println "18. Custom handler:")
(def custom-signals (atom []))

(t/add-handler! :custom-demo
  (fn [signal]
    (swap! custom-signals conj (:id signal))))

(t/event! :custom-1)
(t/event! :custom-2)
(t/event! :custom-3)

(println "Custom handler collected IDs:" @custom-signals)
(t/remove-handler! :custom-demo)
(println)

;;; Handler with Filtering

(println "19. Handler with level filtering:")
(def error-signals (atom []))

(t/add-handler! :errors-only
  (fn [signal]
    (swap! error-signals conj (:level signal)))
  {:min-level :error})

(t/log! :info "Info message")
(t/log! :warn "Warning message")
(t/log! :error "Error message")
(t/log! :fatal "Fatal message")

(println "Error handler collected levels:" @error-signals)
(t/remove-handler! :errors-only)
(println)

;;; Practical Example

(println "20. Practical example - API request handler:")

(defn handle-api-request [request]
  (t/log! :info ["Handling request" {:method (:method request)
                                      :path (:path request)}])
  (t/trace! :request-processing
    (try
      (let [result (t/trace! :validate-request
                     {:valid? true})
            response (t/trace! :process-request
                       {:status 200 :body "Success"})]
        (t/log! :debug {:data {:response response}})
        response)
      (catch Exception e
        (t/error! :request-failed e)
        {:status 500 :body "Error"}))))

(def api-result
  (handle-api-request {:method "GET" :path "/users/123"}))
(println "API result:" api-result)
(println)

;;; Check Interop

(println "21. Check interoperability:")
(def interop-status (t/check-interop))
(println "Interop status:" interop-status)
(println)

(println "=== Examples Complete ===")
