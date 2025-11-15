---
name: timbre
description: Pure Clojure/Script logging library with flexible configuration and powerful features
---

# Timbre

Pure Clojure/Script logging library with zero dependencies, simple configuration, and powerful features like async logging, rate limiting, and flexible appenders.

## Overview

Timbre is a logging library designed for Clojure and ClojureScript applications. Unlike Java logging frameworks requiring XML or properties files, Timbre uses pure Clojure data structures for all configuration.

**Key characteristics:**
- Full Clojure and ClojureScript support
- Single config map - no XML/properties files
- Zero overhead compile-time level/namespace elision
- Built-in async logging and rate limiting
- Function-based appenders and middleware
- Optional tools.logging and SLF4J interop

**Library:** `com.taoensso/timbre`
**Latest Version:** 6.8.0
**License:** EPL-1.0

**Note:** For new projects, consider [Telemere](https://github.com/taoensso/telemere) - a modern rewrite of Timbre. Existing Timbre users have no pressure to migrate.

## Installation

```clojure
;; deps.edn
{:deps {com.taoensso/timbre {:mvn/version "6.8.0"}}}

;; Leiningen
[com.taoensso/timbre "6.8.0"]
```

## Core Concepts

### Log Levels

Seven standard levels in ascending severity:

- `:trace` - Detailed diagnostic information
- `:debug` - Debugging information
- `:info` - Informational messages
- `:warn` - Warning messages
- `:error` - Error messages
- `:fatal` - Critical failures
- `:report` - Special reporting level

### Appenders

Functions that handle log output: `(fn [data]) -> ?effects`

Each appender receives a data map containing:
- `:level` - Log level keyword
- `:?msg` - Log message
- `:timestamp` - When log occurred
- `:hostname` - System hostname
- `:?ns-str` - Namespace string
- `:?file`, `:?line` - Source location
- `:?err` - Exception (if present)
- Additional context data

### Middleware

Functions that transform log data: `(fn [data]) -> ?data`

Applied before appenders receive data, enabling:
- Data enrichment
- Filtering
- Transformation
- Context injection

### Configuration

Timbre uses a single atom `*config*` containing:
- `:min-level` - Minimum log level
- `:ns-filter` - Namespace filtering
- `:middleware` - Middleware functions
- `:timestamp-opts` - Timestamp formatting
- `:output-fn` - Output formatter
- `:appenders` - Appender map

## Basic Usage

### Simple Logging

```clojure
(require '[taoensso.timbre :as timbre])

;; Basic logging at different levels
(timbre/trace "Entering function")
(timbre/debug "Variable value:" x)
(timbre/info "Server started on port" port)
(timbre/warn "Deprecated function used")
(timbre/error "Failed to connect to database")
(timbre/fatal "Critical system failure")
(timbre/report "Daily metrics" {:users 1000 :requests 5000})
```

### Formatted Logging

```clojure
;; Printf-style formatting
(timbre/infof "User %s logged in from %s" username ip)
(timbre/warnf "Cache miss rate: %.2f%%" miss-rate)
(timbre/errorf "Request failed: %d %s" status-code message)
```

### Logging with Exceptions

```clojure
(try
  (risky-operation)
  (catch Exception e
    (timbre/error e "Operation failed")))

;; Multiple values
(timbre/error e "Failed processing" {:user-id 123 :item-id 456})
```

### Spy - Log and Return

```clojure
;; Log value and return it
(let [result (timbre/spy :info (expensive-calculation x y))]
  (process result))

;; With custom message
(timbre/spy :debug
  {:msg "Calculation result"}
  (expensive-calculation x y))
```

## Configuration

### Setting Minimum Level

```clojure
;; Global minimum level
(timbre/set-min-level! :info)

;; Per-namespace level
(timbre/set-ns-min-level! {:deny #{"noisy.namespace.*"}
                            :allow #{"important.namespace.*"}})
```

### Modifying Config

```clojure
;; Replace entire config
(timbre/set-config! new-config)

;; Merge into existing config
(timbre/merge-config! {:min-level :debug
                        :appenders {:println (println-appender)}})

;; Swap with function
(timbre/swap-config! update :min-level (constantly :warn))
```

### Scoped Configuration

```clojure
;; Temporarily change config
(timbre/with-config custom-config
  (timbre/info "Logged with custom config"))

;; Merge temporary changes
(timbre/with-merged-config {:min-level :trace}
  (timbre/trace "Temporarily enabled trace logging"))

;; Temporary level change
(timbre/with-min-level :debug
  (timbre/debug "Debug enabled for this scope"))
```

## Appenders

### Built-in Appenders

#### Console Output

```clojure
(require '[taoensso.timbre.appenders.core :as appenders])

;; println appender (default)
{:appenders {:println (appenders/println-appender)}}
```

#### File Output

```clojure
;; Simple file appender
{:appenders {:spit (appenders/spit-appender
                     {:fname "/var/log/app.log"})}}
```

### Custom Appender

```clojure
(defn my-appender
  "Appender that writes to a custom destination"
  [opts]
  {:enabled? true
   :async? false
   :min-level nil  ; Inherit from config
   :rate-limit nil
   :output-fn :inherit
   :fn (fn [data]
         (let [{:keys [output-fn]} data
               formatted (output-fn data)]
           ;; Custom logic here
           (send-to-custom-system formatted)))})

;; Use custom appender
(timbre/merge-config!
  {:appenders {:custom (my-appender {})}})
```

### Appender Configuration Options

```clojure
{:enabled? true        ; Enable/disable
 :async? false         ; Async logging?
 :min-level :info      ; Appender-specific min level
 :rate-limit [[5 1000]] ; Max 5 logs per 1000ms
 :output-fn :inherit   ; Use config's output-fn
 :fn (fn [data] ...)}  ; Handler function
```

## Advanced Features

### Context (MDC)

```clojure
;; Set context for current thread
(timbre/with-context {:user-id 123 :request-id "abc"}
  (timbre/info "Processing request")
  (do-work))

;; Add to existing context
(timbre/with-context+ {:session-id "xyz"}
  (timbre/info "Additional context"))
```

### Middleware

```clojure
;; Add hostname to all logs
(defn add-hostname-middleware
  [data]
  (assoc data :hostname (get-hostname)))

;; Add timestamp middleware
(defn add-custom-timestamp
  [data]
  (assoc data :custom-ts (System/currentTimeMillis)))

;; Apply middleware
(timbre/merge-config!
  {:middleware [add-hostname-middleware
                add-custom-timestamp]})
```

### Rate Limiting

```clojure
;; Per-appender rate limit
{:appenders
 {:println
  {:enabled? true
   :rate-limit [[10 1000]  ; Max 10 per second
                [100 60000]] ; Max 100 per minute
   :fn (fn [data] (println (:output-fn data)))}}}

;; At log call site
(timbre/log {:rate-limit [[1 5000]]}  ; Max once per 5 seconds
            :info "Rate limited message")
```

### Async Logging

```clojure
;; Enable async for appender
{:appenders
 {:async-file
  {:enabled? true
   :async? true  ; Process logs asynchronously
   :fn (fn [data]
         (spit "/var/log/app.log"
               (str (:output-fn data) "\n")
               :append true))}}}
```

### Conditional Logging

```clojure
;; Log only sometimes (probabilistic)
(timbre/sometimes 0.1  ; 10% probability
  (timbre/info "Sampled log message"))

;; Conditional error logging
(timbre/log-errors
  (risky-operation))  ; Logs if exception thrown

;; Log and rethrow
(timbre/log-and-rethrow-errors
  (risky-operation))  ; Logs then rethrows exception
```

### Exception Handling

```clojure
;; Capture uncaught JVM exceptions (Clojure only)
(timbre/handle-uncaught-jvm-exceptions!)

;; Log errors in futures
(timbre/logged-future
  (risky-async-operation))
```

## Output Formatting

### Custom Output Function

```clojure
(defn my-output-fn
  [{:keys [level ?ns-str ?msg-fmt vargs timestamp_ ?err]}]
  (str
    (force timestamp_) " "
    (str/upper-case (name level)) " "
    "[" ?ns-str "] - "
    (apply format ?msg-fmt vargs)
    (when ?err (str "\n" (timbre/stacktrace ?err)))))

;; Use custom output
(timbre/merge-config! {:output-fn my-output-fn})
```

### Timestamp Configuration

```clojure
{:timestamp-opts
 {:pattern "yyyy-MM-dd HH:mm:ss.SSS"
  :locale (java.util.Locale/getDefault)
  :timezone (java.util.TimeZone/getDefault)}}
```

### Colors

```clojure
(require '[taoensso.timbre :refer [color-str]])

;; ANSI color output
(defn colored-output-fn
  [{:keys [level] :as data}]
  (let [base-output (timbre/default-output-fn data)]
    (case level
      :error (color-str :red base-output)
      :warn  (color-str :yellow base-output)
      :info  (color-str :green base-output)
      base-output)))
```

## Namespace Filtering

```clojure
;; Whitelist/blacklist namespaces
(timbre/merge-config!
  {:ns-filter
   {:deny #{"noisy.lib.*" "chatty.namespace"}
    :allow #{"important.module.*"}}})

;; Per-namespace min levels
(timbre/set-ns-min-level!
  '{my.app.core :trace
    my.app.utils :info
    ["com.external.*"] :warn})
```

## Interoperability

### tools.logging Integration

```clojure
;; Route tools.logging to Timbre
(require '[taoensso.timbre.tools.logging :as tools-logging])

(tools-logging/use-timbre)

;; Now tools.logging calls use Timbre
(require '[clojure.tools.logging :as log])
(log/info "Routed through Timbre")
```

### SLF4J Integration

Timbre can act as an SLF4J backend for Java logging. Requires additional setup with timbre-slf4j-appender or similar.

## Common Patterns

### Application Setup

```clojure
(ns myapp.logging
  (:require [taoensso.timbre :as timbre]
            [taoensso.timbre.appenders.core :as appenders]))

(defn init-logging!
  []
  (timbre/merge-config!
    {:min-level :info
     :ns-filter {:deny #{"verbose.library.*"}}
     :middleware []
     :timestamp-opts {:pattern "yyyy-MM-dd HH:mm:ss"}
     :appenders
     {:println (appenders/println-appender)
      :spit (appenders/spit-appender
              {:fname "/var/log/myapp.log"})}}))

(init-logging!)
```

### Structured Logging

```clojure
;; Log with structured data
(timbre/info "User action"
  {:action :login
   :user-id 123
   :ip "192.168.1.1"
   :timestamp (System/currentTimeMillis)})

;; Custom output for structured logs
(defn json-output-fn
  [{:keys [level msg_ ?ns-str timestamp_ vargs]}]
  (json/write-str
    {:timestamp (force timestamp_)
     :level level
     :namespace ?ns-str
     :message (force msg_)
     :data (first vargs)}))
```

### Environment-Specific Config

```clojure
(defn config-for-env
  [env]
  (case env
    :dev {:min-level :trace
          :appenders {:println (appenders/println-appender)}}
    :staging {:min-level :debug
              :appenders {:spit (appenders/spit-appender
                                  {:fname "/var/log/staging.log"})}}
    :prod {:min-level :info
           :appenders {:spit (appenders/spit-appender
                               {:fname "/var/log/production.log"
                                :async? true})}}))

(timbre/set-config! (config-for-env :prod))
```

### Request Logging

```clojure
(defn wrap-logging
  [handler]
  (fn [request]
    (let [start (System/currentTimeMillis)
          request-id (str (random-uuid))]
      (timbre/with-context {:request-id request-id}
        (timbre/info "Request started" {:method (:request-method request)
                                         :uri (:uri request)})
        (let [response (handler request)
              duration (- (System/currentTimeMillis) start)]
          (timbre/info "Request completed"
            {:status (:status response)
             :duration-ms duration})
          response)))))
```

### Database Query Logging

```clojure
(defn log-query
  [query params]
  (timbre/spy :debug
    {:msg (str "Executing query: " query)}
    (db/execute! query params)))

;; Or with timing
(defn log-slow-queries
  [query params]
  (let [start (System/currentTimeMillis)
        result (db/execute! query params)
        duration (- (System/currentTimeMillis) start)]
    (when (> duration 1000)
      (timbre/warn "Slow query detected"
        {:query query
         :duration-ms duration}))
    result))
```

## Error Handling

### Exception Logging Patterns

```clojure
;; Basic exception logging
(try
  (risky-op)
  (catch Exception e
    (timbre/error e "Operation failed")))

;; With context
(try
  (process-item item)
  (catch Exception e
    (timbre/error e "Failed to process item"
      {:item-id (:id item)
       :item-type (:type item)})))

;; Log and continue
(defn safe-process
  [items]
  (doseq [item items]
    (timbre/log-errors
      (process-item item))))

;; Log and rethrow
(defn critical-operation
  []
  (timbre/log-and-rethrow-errors
    (perform-critical-task)))
```

## Performance Considerations

### Compile-Time Elision

```clojure
;; Set via JVM property or env var
;; Only :info and above will be compiled
;; :trace and :debug calls removed at compile time
;; -Dtimbre.min-level=:info

;; Verify elision
(timbre/debug "This won't be in bytecode if min-level >= :info")
```

### Async Appenders

```clojure
;; Offload I/O to background thread
{:appenders
 {:file
  {:async? true
   :fn (fn [data]
         ;; Expensive I/O operation
         (write-to-file data))}}}
```

### Conditional Evaluation

```clojure
;; Arguments evaluated only if level enabled
(timbre/debug (expensive-debug-string))  ; Not called if debug disabled

;; For very expensive operations, use explicit check
(when (timbre/log? :debug)
  (timbre/debug (very-expensive-operation)))
```

## ClojureScript Usage

```clojure
(ns myapp.core
  (:require [taoensso.timbre :as timbre]))

;; Same API as Clojure
(timbre/info "Running in browser")

;; Configure for browser
(timbre/set-config!
  {:level :debug
   :appenders
   {:console
    {:enabled? true
     :fn (fn [data]
           (let [{:keys [output-fn]} data]
             (.log js/console (output-fn data))))}}})
```

## Testing

### Test Configuration

```clojure
(ns myapp.test
  (:require [clojure.test :refer :all]
            [taoensso.timbre :as timbre]))

;; Disable logging in tests
(use-fixtures :once
  (fn [f]
    (timbre/with-merged-config {:min-level :fatal}
      (f))))

;; Or capture logs for assertions
(defn with-log-capture
  [f]
  (let [logs (atom [])]
    (timbre/with-merged-config
      {:appenders
       {:test {:enabled? true
               :fn (fn [data]
                     (swap! logs conj data))}}}
      (f logs))))
```

## Migration from tools.logging

```clojure
;; Before (tools.logging)
(require '[clojure.tools.logging :as log])
(log/info "Message")
(log/error e "Error occurred")

;; After (Timbre)
(require '[taoensso.timbre :as timbre])
(timbre/info "Message")
(timbre/error e "Error occurred")

;; Or keep tools.logging imports and use bridge
(require '[taoensso.timbre.tools.logging :as tools-logging])
(tools-logging/use-timbre)
;; Now existing tools.logging code routes to Timbre
```

## Troubleshooting

### No Output

Check configuration:
```clojure
;; Verify config
@timbre/*config*

;; Check min level
(:min-level @timbre/*config*)

;; Verify appenders enabled
(->> @timbre/*config* :appenders vals (filter :enabled?))
```

### Missing Logs

```clojure
;; Check namespace filters
(timbre/may-log? :info)  ; In current namespace
(timbre/may-log? :info "some.namespace")  ; Specific namespace
```

### Performance Issues

```clojure
;; Enable async for expensive appenders
{:appenders {:file {:async? true ...}}}

;; Add rate limiting
{:appenders {:email {:rate-limit [[1 60000]] ...}}}

;; Use compile-time elision in production
;; -Dtimbre.min-level=:info
```

## References

- [GitHub Repository](https://github.com/taoensso/timbre)
- [API Documentation](https://taoensso.github.io/timbre/)
- [Wiki](https://github.com/taoensso/timbre/wiki)
- [Telemere (Modern Alternative)](https://github.com/taoensso/telemere)
