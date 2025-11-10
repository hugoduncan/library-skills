# clj-kondo Quick Reference

## Command Line

```bash
# Basic linting
clj-kondo --lint <path>
clj-kondo --lint src test

# Output formats
clj-kondo --lint src --config '{:output {:format :json}}'
clj-kondo --lint src --config '{:output {:format :edn}}'

# Cache dependencies
clj-kondo --lint "$(clojure -Spath)" --dependencies --parallel --copy-configs

# Version
clj-kondo --version
```

## Configuration File Location

```
.clj-kondo/config.edn          # Project config
~/.config/clj-kondo/config.edn  # Global config
```

## Basic Configuration

```clojure
{:linters {:unused-binding {:level :warning}
           :unused-namespace {:level :warning}
           :unresolved-symbol {:level :error}
           :invalid-arity {:level :error}
           :misplaced-docstring {:level :warning}}
 :output {:pattern "{{LEVEL}} {{filename}}:{{row}}:{{col}} {{message}}"
          :exclude-files ["generated/" "resources/public/js/"]}}
```

## Linter Levels

```clojure
:off      ; Disable
:info     ; Informational
:warning  ; Warning (default)
:error    ; Error (fails build)
```

## Common Linter Configurations

### Disable Specific Linters

```clojure
{:linters {:unused-binding {:level :off}
           :unused-private-var {:level :off}}}
```

### Exclude Namespaces

```clojure
{:linters {:unused-binding {:exclude-ns [myapp.test-helpers
                                         myapp.dev]}}}
```

### Consistent Aliases

```clojure
{:linters {:consistent-alias {:level :warning
                              :aliases {clojure.string str
                                        clojure.set set
                                        clojure.java.io io}}}}
```

### Refer :all

```clojure
{:linters {:refer-all {:level :warning
                       :exclude [clojure.test]}}}
```

### Macro as Function

```clojure
{:lint-as {myapp/my-macro clojure.core/let
           myapp/defroute compojure.core/defroutes}}
```

## Inline Suppressions

```clojure
;; Suppress for entire namespace
(ns myapp.core
  {:clj-kondo/config '{:linters {:unused-binding {:level :off}}}})

;; Suppress specific linters for form
#_{:clj-kondo/ignore [:unused-binding :unresolved-symbol]}
(let [x 1] (undefined-fn))

;; Suppress all linters for form
#_{:clj-kondo/ignore true}
(problematic-code)

;; Suppress with underscore prefix
(let [_unused-var 42] ;; No warning
  ...)
```

## Hook Types

### analyze-call Hook

Analyze function/macro calls:

```clojure
;; config.edn
{:hooks {:analyze-call {mylib/deprecated-fn hooks.my/warn-deprecated}}}

;; hooks/my.clj
(ns hooks.my
  (:require [clj-kondo.hooks-api :as api]))

(defn warn-deprecated [{:keys [node]}]
  {:findings [{:message "This function is deprecated"
               :type :deprecated-api
               :row (api/row node)
               :col (api/col node)
               :level :warning}]})
```

### macroexpand Hook

Transform macro for analysis:

```clojure
;; config.edn
{:hooks {:macroexpand {mylib/defentity hooks.my/expand-defentity}}}

;; hooks/my.clj
(defn expand-defentity [{:keys [node]}]
  (let [[_ name & body] (:children node)]
    {:node (api/list-node
            [(api/token-node 'def)
             name
             (api/map-node body)])}))
```

## Hook API - Node Functions

```clojure
;; Query
(api/tag node)           ; :list, :vector, :map, :token, etc.
(api/sexpr node)         ; Convert to s-expression
(:children node)         ; Get child nodes
(api/string node)        ; String representation

;; Position
(api/row node)
(api/col node)
(api/end-row node)
(api/end-col node)

;; Predicates
(api/token-node? node)
(api/keyword-node? node)
(api/string-node? node)
(api/list-node? node)
(api/vector-node? node)
(api/map-node? node)
```

## Hook API - Node Constructors

```clojure
;; Atoms
(api/token-node 'symbol)
(api/keyword-node :keyword)
(api/string-node "string")
(api/number-node 42)

;; Collections
(api/list-node [node1 node2 node3])
(api/vector-node [node1 node2])
(api/map-node [k1 v1 k2 v2])
(api/set-node [node1 node2])
```

## Hook Return Values

```clojure
;; Return findings
{:findings [{:message "Error message"
             :type :my-custom-type
             :row (api/row node)
             :col (api/col node)
             :level :warning}]}

;; Return transformed node
{:node new-node}

;; Return both
{:node new-node
 :findings [{:message "..." ...}]}
```

## Common Hook Patterns

### Deprecation Warning

```clojure
(defn deprecation [{:keys [node]}]
  {:findings [{:message "Use new-api instead of old-api"
               :type :deprecated
               :row (api/row node)
               :col (api/col node)
               :level :warning}]})
```

### Argument Count Validation

```clojure
(defn validate-arity [{:keys [node]}]
  (let [args (rest (:children node))]
    (when (< (count args) 2)
      {:findings [{:message "Expected at least 2 arguments"
                   :type :invalid-arity
                   :row (api/row node)
                   :col (api/col node)
                   :level :error}]})))
```

### Argument Type Validation

```clojure
(defn validate-type [{:keys [node]}]
  (let [first-arg (second (:children node))]
    (when-not (api/keyword-node? first-arg)
      {:findings [{:message "First argument must be a keyword"
                   :type :invalid-argument-type
                   :row (api/row node)
                   :col (api/col node)
                   :level :error}]})))
```

### Required Map Keys

```clojure
(defn validate-keys [{:keys [node]}]
  (let [map-node (second (:children node))]
    (when (api/map-node? map-node)
      (let [keys (->> (:children map-node)
                      (take-nth 2)
                      (map api/sexpr)
                      (set))
            required #{:host :port}
            missing (clojure.set/difference required keys)]
        (when (seq missing)
          {:findings [{:message (str "Missing keys: " missing)
                       :type :missing-keys
                       :row (api/row node)
                       :col (api/col node)
                       :level :error}]})))))
```

### Macro Expansion for DSL

```clojure
(defn expand-defroute [{:keys [node]}]
  (let [[_ method path handler] (:children node)]
    {:node (api/list-node
            [(api/token-node 'def)
             (api/token-node (gensym "route"))
             (api/map-node [method path handler])])}))
```

## IDE Integration

### VS Code (Calva)
Built-in support, works automatically.

### Emacs
```elisp
(use-package flycheck-clj-kondo :ensure t)
```

### Vim/Neovim (ALE)
```vim
let g:ale_linters = {'clojure': ['clj-kondo']}
```

### IntelliJ/Cursive
Native integration via Preferences → Editor → Inspections.

## CI/CD Patterns

### GitHub Actions

```yaml
- name: Install clj-kondo
  run: |
    curl -sLO https://raw.githubusercontent.com/clj-kondo/clj-kondo/master/script/install-clj-kondo
    chmod +x install-clj-kondo
    ./install-clj-kondo
- name: Run clj-kondo
  run: clj-kondo --lint src test
```

### GitLab CI

```yaml
lint:
  image: cljkondo/clj-kondo:latest
  script:
    - clj-kondo --lint src test
```

### Pre-commit Hook

```bash
#!/bin/bash
clj-kondo --lint src test
```

## Troubleshooting

### False Positive - Macro Generated Symbol

```clojure
{:lint-as {myapp/my-macro clojure.core/let}}
```

### Exclude Generated Files

```clojure
{:output {:exclude-files ["generated/" "target/" "node_modules/"]}}
```

### Hook Not Triggering

1. Check hook registration in config.edn
2. Verify namespace path matches
3. Test with minimal example
4. Run with `--debug` flag

### Performance Issues

```bash
# Cache dependencies once
clj-kondo --lint "$(clojure -Spath)" --dependencies --parallel

# Exclude large directories
{:output {:exclude-files ["resources/public/"]}}
```

## Tips

- Start with zero config - defaults are good
- Use `--copy-configs` to get library-specific rules
- Write hooks for domain-specific linting
- Test hooks with minimal examples
- Document why you suppress warnings
- Run in CI to catch issues early
- Cache dependency analysis for speed
