---
name: telemere
description: Structured logging and telemetry for Clojure/Script with tracing and performance monitoring
---

# Telemere

Structured logging and telemetry library for Clojure and ClojureScript. Next-generation successor to Timbre with unified API for logging, tracing, and performance monitoring.

## Overview

Telemere provides a unified approach to application observability, handling traditional logging, structured telemetry, distributed tracing, and performance monitoring through a single consistent API.

**Key Features:**
- Structured data throughout pipeline (no string parsing)
- Compile-time signal elision (zero runtime cost for disabled signals)
- Runtime filtering (namespace, level, ID, rate limiting, sampling)
- Async and sync handler dispatch
- OpenTelemetry, SLF4J, and tools.logging interoperability
- Zero-configuration defaults
- ClojureScript support

**Artifact:** `com.taoensso/telemere`
**Latest Version:** 1.1.0
**License:** EPL-1.0
**Repository:** https://github.com/taoensso/telemere

## Installation

Add to `deps.edn`:
```clojure
{:deps {com.taoensso/telemere {:mvn/version "1.1.0"}}}
```

Or Leiningen `project.clj`:
```clojure
[com.taoensso/telemere "1.1.0"]
```

Import in namespace:
```clojure
(ns my-app
  (:require [taoensso.telemere :as t]))
```

## Core Concepts

### Signals

Signals are structured telemetry events represented as Clojure maps with standardized attributes. They preserve data types throughout the logging pipeline rather than converting to strings.

Signal attributes include: namespace, level, ID, timestamp, thread info, line number, form data, return values, custom data maps.

### Default Configuration

Out-of-the-box settings:
- Minimum level: `:info`
- Handler: Console output to `*out*` or browser console
- Automatic interop with SLF4J, tools.logging when present

### Filtering Philosophy

Two-stage filtering:
1. **Call-time** (compile + runtime): Determines if signal is created
2. **Handler-time** (runtime): Determines which handlers process signal

Effective filtering reduces noise and improves performance.

## API Reference

### Signal Creation

#### log!

Traditional and structured logging.

```clojure
;; Basic logging with level
(t/log! :info "Processing started")
(t/log! :warn "High memory usage")
(t/log! :error "Database connection failed")

;; With message arguments
(t/log! :info ["User logged in:" {:user-id 123}])

;; Structured data
(t/log! {:level :info
         :data {:user-id 123 :action "login"}})

;; With ID for filtering
(t/log! {:id :user-action
         :level :info
         :data {:user-id 123}})
```

**Levels (priority order):**
`:trace` < `:debug` < `:info` < `:warn` < `:error` < `:fatal` < `:report`

**Options:**
- `:level` - Signal level (keyword)
- `:id` - Signal ID for filtering (keyword)
- `:data` - Structured data map
- `:msg` - Message string or vector
- `:error` - Exception/error object
- `:ctx` - Context map
- `:sample-rate` - Signal sampling (0.0-1.0)
- `:rate-limit` - Rate limiting spec
- `:run` - Form to evaluate and include result

#### event!

ID and level-based event logging.

```clojure
;; Simple event
(t/event! :user-signup)
(t/event! :payment-processed)

;; With level
(t/event! :cache-miss :warn)

;; With data
(t/event! :user-signup
  {:data {:user-id 123 :email "user@example.com"}})

;; With level and data
(t/event! :slow-query :warn
  {:data {:duration-ms 1200 :query "SELECT ..."}})
```

Events are filtered by ID, making them ideal for metrics and tracking specific occurrences.

#### trace!

Tracks form execution with nested flow tracking.

```clojure
;; Basic tracing
(t/trace! :fetch-user
  (fetch-user-from-db user-id))

;; Returns form result while logging execution
(def user
  (t/trace! :fetch-user
    (fetch-user-from-db 123)))

;; With data
(t/trace! {:id :process-order
           :data {:order-id 456}}
  (process-order 456))

;; Nested tracing shows parent-child relationships
(t/trace! :outer
  (do
    (t/trace! :inner-1 (step-1))
    (t/trace! :inner-2 (step-2))))
```

Trace signals include execution time and return value. Nested traces maintain parent-child relationships.

#### spy!

Execution tracing with return value capture.

```clojure
;; Spy on expression
(t/spy! :debug
  (+ 1 2 3))
;;=> 6 (also logs the expression and result)

;; Spy in pipeline
(->> data
     (map inc)
     (t/spy! :debug)  ; See intermediate value
     (filter even?))

;; With custom ID
(t/spy! {:id :computation :level :trace}
  (* 42 (expensive-calc)))
```

Spy always returns the form result, making it useful in pipelines.

#### error!

Error logging with exception handling.

```clojure
;; Log error
(t/error! (ex-info "Failed" {:reason :timeout}))

;; With ID
(t/error! :db-error
  (ex-info "Connection lost" {:host "db.example.com"}))

;; With additional data
(t/error! {:id :api-error
           :data {:endpoint "/users" :status 500}}
  (ex-info "API failed" {}))
```

Returns the error object.

#### catch->error!

Catch and log exceptions.

```clojure
;; Basic error catching
(t/catch->error!
  (risky-operation))

;; With ID
(t/catch->error! :db-operation
  (db-query))

;; With data
(t/catch->error! {:id :api-call
                  :data {:endpoint "/users"}}
  (http-request "/users"))

;; Returns nil on error, result on success
(if-let [result (t/catch->error! (fetch-data))]
  (process result)
  (handle-error))
```

Catches exceptions, logs them, and returns nil. Returns form result if no exception.

#### signal!

Low-level signal creation with full control.

```clojure
;; Full signal specification
(t/signal!
  {:kind :log
   :level :info
   :id :custom-event
   :ns (str *ns*)
   :data {:key "value"}
   :msg "Custom message"
   :run (do-something)})
```

Most use cases are better served by higher-level functions.

### Configuration

#### set-min-level!

Set global or namespace-specific minimum level.

```clojure
;; Global minimum level
(t/set-min-level! :warn)

;; Namespace-specific
(t/set-min-level! 'my.app.core :debug)
(t/set-min-level! 'my.app.* :info)

;; Per-namespace map
(t/set-min-level!
  [['my.app.* :info]
   ['my.app.db :debug]
   ['noisy.library.* :error]])
```

Signals below minimum level are filtered at call-time.

#### set-ns-filter!

Configure namespace filtering.

```clojure
;; Allow only specific namespaces
(t/set-ns-filter! {:allow #{"my.app.*"}})

;; Disallow specific namespaces
(t/set-ns-filter! {:disallow #{"noisy.library.*"}})

;; Combined
(t/set-ns-filter!
  {:allow #{"my.app.*"}
   :disallow #{"my.app.test.*"}})
```

Namespace patterns support wildcards (`*`).

#### with-min-level

Temporarily override minimum level.

```clojure
;; Enable debug logging for block
(t/with-min-level :debug
  (t/log! :debug "Debug info")  ; Logged
  (process-data))

;; Nested overrides
(t/with-min-level :warn
  (t/with-min-level :trace  ; Inner level applies
    (t/log! :trace "Trace info")))
```

Scope is thread-local and dynamic.

#### with-signal

Capture last signal for testing.

```clojure
;; Capture signal map
(def sig
  (t/with-signal
    (t/log! {:level :info :data {:x 1}})))

(:level sig)  ;;=> :info
(:data sig)   ;;=> {:x 1}

;; Test signal creation
(let [sig (t/with-signal
            (t/event! :test-event {:data {:y 2}}))]
  (assert (= :test-event (:id sig)))
  (assert (= {:y 2} (:data sig))))
```

Returns signal map instead of nil.

#### with-signals

Capture all signals from form.

```clojure
;; Capture multiple signals
(def sigs
  (t/with-signals
    (t/log! :info "First")
    (t/log! :warn "Second")
    (t/event! :third)))

(count sigs)  ;;=> 3
(map :level sigs)  ;;=> (:info :warn :info)
```

Returns vector of signal maps.

### Handlers

Handlers process signals and route them to destinations (console, files, databases, analytics).

#### add-handler!

Register signal handler.

```clojure
;; Console handler (built-in)
(t/add-handler! :my-console
  (t/handler:console))

;; Custom handler function
(t/add-handler! :custom
  (fn [signal]
    (println "Custom:" (:msg signal))))

;; With filtering
(t/add-handler! :error-only
  (t/handler:console)
  {:min-level :error})

;; With async dispatch
(t/add-handler! :async-log
  (fn [signal] (log-to-db signal))
  {:async {:buffer-size 1024
           :n-threads 2}})

;; With sampling
(t/add-handler! :sampled
  (t/handler:console)
  {:sample-rate 0.1})  ; 10% of signals
```

**Handler Options:**
- `:min-level` - Minimum signal level
- `:ns-filter` - Namespace filter
- `:id-filter` - ID filter
- `:sample-rate` - Sampling rate (0.0-1.0)
- `:rate-limit` - Rate limiting spec
- `:async` - Async dispatch config
- `:middleware` - Transform functions

#### remove-handler!

Remove handler by ID.

```clojure
(t/remove-handler! :my-console)
(t/remove-handler! :custom)
```

#### handler:console

Built-in console handler with formatting.

```clojure
;; Default text format
(t/handler:console)

;; JSON format
(t/handler:console {:format :json})

;; EDN format
(t/handler:console {:format :edn})

;; Custom format function
(t/handler:console
  {:format (fn [signal]
             (pr-str (:data signal)))})
```

#### handler:stream

Output to Java OutputStream or Writer.

```clojure
;; File output
(t/add-handler! :file
  (t/handler:stream
    (io/output-stream "app.log")
    {:format :json}))

;; With rotation (requires additional setup)
(t/add-handler! :rotating-file
  (rotating-file-handler "logs/app.log"))
```

### Filtering Utilities

#### check-min-level

Check if level passes minimum threshold.

```clojure
(t/check-min-level :info)   ;;=> true/false
(t/check-min-level 'my.ns :debug)  ;;=> true/false
```

#### check-ns-filter

Check if namespace passes filter.

```clojure
(t/check-ns-filter 'my.app.core)  ;;=> true/false
```

### Utilities

#### check-interop

Verify interoperability status.

```clojure
(t/check-interop)
;;=> {:slf4j {:present? true :sending->telemere? true}
;;    :tools.logging {:present? true :sending->telemere? true}
;;    :streams {:out :telemere :err :telemere}}
```

Shows which external logging systems are captured.

#### help:filters

Documentation on filtering.

```clojure
t/help:filters
```

#### help:handlers

Documentation on handlers.

```clojure
t/help:handlers
```

## Common Patterns

### Basic Application Logging

```clojure
(ns my-app.core
  (:require [taoensso.telemere :as t]))

;; Set minimum level for production
(t/set-min-level! :info)

;; Disable noisy libraries
(t/set-ns-filter! {:disallow #{"noisy.library.*"}})

(defn process-request [req]
  (t/log! :info ["Processing request" {:path (:uri req)}])
  (try
    (let [result (handle-request req)]
      (t/log! :debug {:data {:result result}})
      result)
    (catch Exception e
      (t/error! :request-error e)
      (throw e))))
```

### Structured Event Tracking

```clojure
;; Track user actions
(defn record-action [user-id action data]
  (t/event! action
    {:data (merge {:user-id user-id} data)}))

(record-action 123 :login {:method "oauth"})
(record-action 123 :purchase {:amount 99.99 :item "widget"})

;; Query-specific tracking
(defn track-slow-query [query duration-ms]
  (when (> duration-ms 1000)
    (t/event! :slow-query :warn
      {:data {:query query :duration-ms duration-ms}})))
```

### Distributed Tracing

```clojure
(defn fetch-user-data [user-id]
  (t/trace! :fetch-user-data
    (let [user (t/trace! :db-query
                 (db/get-user user-id))
          prefs (t/trace! :fetch-preferences
                  (api/get-preferences user-id))]
      (merge user prefs))))

;; Traces show nested execution:
;; :fetch-user-data (parent)
;;   :db-query (child)
;;   :fetch-preferences (child)
```

### Performance Monitoring

```clojure
(defn monitored-operation [data]
  (t/trace! {:id :operation
             :data {:input-size (count data)}}
    (let [result (expensive-processing data)]
      ;; Trace automatically captures execution time
      result)))

;; Check performance
(t/spy! :debug
  (reduce + (range 1000000)))
```

### Error Handling

```clojure
(defn safe-api-call [endpoint]
  (t/catch->error! {:id :api-call
                    :data {:endpoint endpoint}}
    (http/get endpoint)))

;; With fallback
(defn fetch-with-fallback [url]
  (or (t/catch->error! :primary-fetch
        (fetch-primary url))
      (t/catch->error! :fallback-fetch
        (fetch-fallback url))
      (do
        (t/log! :error "All fetch attempts failed")
        nil)))
```

### Rate Limiting

```clojure
;; Limit signal rate
(t/log! {:level :info
         :rate-limit {"my-limit" [10 1000]}}  ; 10/sec
  "High-frequency event")

;; Per-handler rate limiting
(t/add-handler! :limited
  (t/handler:console)
  {:rate-limit {"handler-limit" [100 60000]}})  ; 100/min
```

### Sampling

```clojure
;; Sample 10% of debug signals
(t/log! {:level :debug
         :sample-rate 0.1}
  "Debug info")

;; Sample at handler level
(t/add-handler! :sampled-analytics
  (fn [sig] (send-to-analytics sig))
  {:sample-rate 0.05})  ; 5% to analytics
```

### Multi-Handler Setup

```clojure
;; Console for development
(t/add-handler! :console
  (t/handler:console)
  {:min-level :debug})

;; File for all errors
(t/add-handler! :error-file
  (t/handler:stream (io/output-stream "errors.log"))
  {:min-level :error
   :format :json})

;; Analytics for events
(t/add-handler! :analytics
  (fn [sig]
    (when (= :event (:kind sig))
      (send-to-analytics sig)))
  {:sample-rate 0.1})

;; OpenTelemetry for traces
(t/add-handler! :otel
  (otel-handler)
  {:kind-filter #{:trace}})
```

### Testing with Signals

```clojure
(require '[clojure.test :refer [deftest is]])

(deftest test-logging
  (let [sig (t/with-signal
              (my-function-that-logs))]
    (is (= :info (:level sig)))
    (is (= :expected-id (:id sig)))
    (is (= expected-data (:data sig)))))

(deftest test-multiple-signals
  (let [sigs (t/with-signals
               (process-batch items))]
    (is (= 5 (count sigs)))
    (is (every? #(= :info (:level %)) sigs))))
```

### Dynamic Configuration

```clojure
;; Enable debug logging temporarily
(defn debug-user-request [user-id]
  (t/with-min-level :trace
    (t/set-ns-filter! {:allow #{"my.app.*"}})
    (process-user user-id)))

;; Feature flag integration
(when (feature-enabled? :verbose-logging)
  (t/set-min-level! 'my.app.* :debug))
```

## Error Handling

### Exception Logging

```clojure
;; Automatic exception capture
(try
  (risky-operation)
  (catch Exception e
    (t/error! e)))

;; With context
(try
  (db-operation user-id)
  (catch Exception e
    (t/error! {:id :db-error
               :data {:user-id user-id}}
      e)))

;; Catch helper
(t/catch->error! :operation
  (risky-operation))
```

### Error Context

```clojure
;; Include error in structured data
(t/log! {:level :error
         :id :processing-failed
         :data {:user-id user-id
                :error (ex-message e)
                :cause (ex-cause e)}})

;; Error with trace
(t/trace! {:id :failing-operation
           :data {:input data}}
  (operation-that-might-fail data))
```

## Performance Considerations

### Compile-Time Elision

Signals are compiled away when filtered by minimum level:

```clojure
;; With min-level :info, this compiles to nil (zero cost)
(t/log! :trace "Expensive" (expensive-computation))
```

### Runtime Performance

Benchmark results (2020 Macbook Pro M1):
- Compile-time filtered: 0 ns/call
- Runtime filtered: 350 ns/call
- Enabled with handler: 1000 ns/call

Capacity: ~4.2 million filtered signals/sec

### Optimization Tips

```clojure
;; Defer expensive computations
(t/log! {:level :debug
         :run (expensive-data-builder)})  ; Only runs if logged

;; Use sampling for high-frequency signals
(t/log! {:level :debug
         :sample-rate 0.01}  ; 1%
  "High-frequency event")

;; Async handlers for I/O
(t/add-handler! :db-log
  (fn [sig] (write-to-db sig))
  {:async {:buffer-size 10000
           :n-threads 4}})
```

## Platform-Specific Notes

### Babashka

Telemere fully supports Babashka. All core features work identically.

```clojure
#!/usr/bin/env bb
(require '[taoensso.telemere :as t])

(t/log! :info "Running in Babashka")
```

### ClojureScript

Full ClojureScript support with browser console output.

```clojure
(ns my-app.core
  (:require [taoensso.telemere :as t]))

;; Outputs to browser console
(t/log! :info "ClojureScript logging")

;; Custom handlers for ClojureScript
(t/add-handler! :custom
  (fn [sig]
    (js/console.log "Custom:" (pr-str sig))))
```

### Interoperability

#### SLF4J Integration

Automatically captures SLF4J logging:

```clojure
(t/check-interop)
;;=> {:slf4j {:present? true :sending->telemere? true}}
```

#### tools.logging Integration

Automatically captures tools.logging:

```clojure
(require '[clojure.tools.logging :as log])

;; These route through Telemere
(log/info "Message")
(log/error ex "Error occurred")
```

#### OpenTelemetry

Integration requires additional handler setup (see documentation).

## Migration from Timbre

Telemere includes Timbre compatibility layer:

```clojure
;; Use Timbre API
(require '[taoensso.timbre :as timbre])

;; Routes through Telemere
(timbre/info "Message")
(timbre/error ex "Error")
```

Key differences:
- Telemere emphasizes structured data over string messages
- Filtering is more powerful and flexible
- Tracing is first-class, not an add-on
- Handlers use different configuration format

## Use Cases

### Application Logging

Standard logging for web apps, services, and batch jobs.

### Distributed Tracing

Track request flow through microservices with nested traces.

### Performance Monitoring

Identify bottlenecks with automatic execution timing.

### Error Tracking

Centralized error collection with structured context.

### Audit Logging

Track user actions and system changes with event logging.

### Debugging

Rich contextual debugging with trace and spy.

### Production Observability

Real-time monitoring with filtered, sampled telemetry.

## Resources

- **GitHub:** https://github.com/taoensso/telemere
- **Wiki:** https://github.com/taoensso/telemere/wiki
- **API Docs:** https://cljdoc.org/d/com.taoensso/telemere
- **Videos:**
  - 7-min intro: https://www.youtube.com/watch?v=...
  - 24-min REPL demo: https://www.youtube.com/watch?v=...

## License

Copyright © 2023-2025 Peter Taoussanis
Distributed under the EPL-1.0 (same as Clojure)
