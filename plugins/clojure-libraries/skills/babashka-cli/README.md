# babashka.cli

Turn Clojure functions into CLIs with minimal effort.

## Quick Start

```clojure
(require '[babashka.cli :as cli])

;; Parse basic options
(cli/parse-opts ["--port" "8080"] {:coerce {:port :long}})
;;=> {:port 8080}

;; Parse with subcommands
(cli/parse-args ["deploy" "prod" "--force"]
  {:coerce {:force :boolean}})
;;=> {:cmds ["deploy" "prod"] :opts {:force true}}
```

## Key Features

- **Flexible Syntax**: Both `:opt` and `--opt` styles supported
- **Type Coercion**: Automatic or explicit type conversion
- **Subcommands**: Built-in dispatch mechanism
- **Validation**: Required options, restrictions, custom validators
- **Collections**: Handle repeated options naturally
- **Help Generation**: Format specs into help text

## Basic Coercion

```clojure
(cli/parse-opts ["--port" "8080" "--verbose"]
  {:coerce {:port :long :verbose :boolean}})
;;=> {:port 8080 :verbose true}
```

## Aliases

```clojure
(cli/parse-opts ["-p" "8080" "-v"]
  {:alias {:p :port :v :verbose}
   :coerce {:port :long :verbose :boolean}})
;;=> {:port 8080 :verbose true}
```

## Positional Arguments

```clojure
(cli/parse-opts ["deploy" "production"]
  {:args->opts [:action :env]})
;;=> {:action "deploy" :env "production"}
```

## Validation

```clojure
(cli/parse-args ["--port" "8080"]
  {:coerce {:port :long}
   :require [:port :host]
   :validate {:port pos?}})
;; Throws if :host missing or :port not positive
```

## Subcommand Dispatch

```clojure
(defn deploy [opts]
  (println "Deploying to" (get-in opts [:opts :env])))

(cli/dispatch
  [{:cmds ["deploy"] :fn deploy}]
  ["deploy" "--env" "prod"])
```

## Using the Skill

Invoke with:
```
claude-code --skill clojure-libraries:babashka.cli
```

The skill provides comprehensive documentation including:
- Complete API reference for all functions
- Coercion types and collection handling
- Validation and error handling patterns
- Subcommand routing and dispatch
- Help text generation
- Common CLI patterns and recipes

## Learning Path

1. Start with `parse-opts` for simple option parsing
2. Use `parse-args` when you need positional arguments
3. Add validation with `:require`, `:restrict`, `:validate`
4. Implement subcommands with `dispatch`
5. Generate help text with `format-opts`

## Resources

- **Repository**: https://github.com/babashka/cli
- **Documentation**: https://cljdoc.org/d/org.babashka/cli
- **Blog Post**: https://blog.michielborkent.nl/babashka-cli.html
- **Babashka Book**: https://book.babashka.org/

## Installation

```clojure
;; deps.edn
{:deps {org.babashka/cli {:mvn/version "0.8.60"}}}

;; bb.edn (built-in since babashka 0.9.160)
{:deps {org.babashka/cli {:mvn/version "0.8.60"}}}
```
