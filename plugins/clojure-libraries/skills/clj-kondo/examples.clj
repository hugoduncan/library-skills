#!/usr/bin/env bb

;; clj-kondo Hook Examples
;; =======================
;;
;; This file demonstrates common patterns for writing clj-kondo hooks.
;; These examples should be adapted and placed in .clj-kondo/hooks/ files
;; in your project.

(println "clj-kondo Hook Examples\n")
(println "========================\n")
(println "These are example hooks to copy into your .clj-kondo/hooks/ files.")
(println "The clj-kondo.hooks-api is only available in hook context, not standalone.\n")

;;; Example 1: Basic Deprecation Warning Hook

(println "Example 1: Deprecation Warning")
(println "-------------------------------\n")

(println "(ns hooks.deprecation")
(println "  (:require [clj-kondo.hooks-api :as api]))")
(println "")
(println "(defn warn-deprecated")
(println "  \"Warn about deprecated function usage\"")
(println "  [{:keys [node]}]")
(println "  {:findings [{:message \"This function is deprecated. Use new-api instead.\"")
(println "               :type :deprecated-function")
(println "               :row (api/row node)")
(println "               :col (api/col node)")
(println "               :level :warning}]})")
(println "")
(println "Config: {:hooks {:analyze-call {mylib/old-api hooks.deprecation/warn-deprecated}}}\n")

;;; Example 2: Argument Count Validation

(println "Example 2: Argument Count Validation")
(println "-------------------------------------\n")

(println "(defn validate-min-args")
(println "  \"Ensures minimum number of arguments\"")
(println "  [{:keys [node]}]")
(println "  (let [args (rest (:children node))]")
(println "    (when (< (count args) 2)")
(println "      {:findings [{:message \"Expected at least 2 arguments\"")
(println "                   :type :invalid-arity")
(println "                   :row (api/row node)")
(println "                   :col (api/col node)")
(println "                   :level :error}]})))")
(println "")
(println "Config: {:hooks {:analyze-call {mylib/query hooks.validation/validate-min-args}}}\n")

;;; Example 3: Argument Type Validation

(println "Example 3: Argument Type Validation")
(println "------------------------------------\n")

(println "(defn validate-keyword-arg")
(println "  \"Ensures first argument is a keyword\"")
(println "  [{:keys [node]}]")
(println "  (let [first-arg (second (:children node))]")
(println "    (when (and first-arg (not (api/keyword-node? first-arg)))")
(println "      {:findings [{:message \"First argument must be a keyword\"")
(println "                   :type :invalid-argument-type")
(println "                   :row (api/row first-arg)")
(println "                   :col (api/col first-arg)")
(println "                   :level :error}]})))")
(println "")
(println "Config: {:hooks {:analyze-call {mylib/query hooks.validation/validate-keyword-arg}}}\n")

;;; Example 4: Required Map Keys

(println "Example 4: Required Map Keys Validation")
(println "----------------------------------------\n")

(println "(defn validate-config-keys")
(println "  \"Ensures config map has required keys\"")
(println "  [{:keys [node]}]")
(println "  (let [config-map (second (:children node))]")
(println "    (when (api/map-node? config-map)")
(println "      (let [keys (->> (:children config-map)")
(println "                      (take-nth 2)")
(println "                      (map api/sexpr)")
(println "                      (set))")
(println "            required #{:host :port :timeout}")
(println "            missing (clojure.set/difference required keys)]")
(println "        (when (seq missing)")
(println "          {:findings [{:message (str \"Missing keys: \" (vec missing))")
(println "                       :type :missing-config-keys")
(println "                       :row (api/row config-map)")
(println "                       :col (api/col config-map)")
(println "                       :level :error}]})))))")
(println "")
(println "Config: {:hooks {:analyze-call {mylib/connect hooks.validation/validate-config-keys}}}\n")

;;; Example 5: Macro Expansion

(println "Example 5: Macro Expansion (DSL Support)")
(println "-----------------------------------------\n")

(println "(defn expand-defentity")
(println "  \"Expands (defentity Name {...}) to (def Name ...) for analysis\"")
(println "  [{:keys [node]}]")
(println "  (let [[_ name-node & body-nodes] (:children node)")
(println "        entity-map (api/map-node")
(println "                    (concat [(api/keyword-node :type)")
(println "                             (api/keyword-node :entity)]")
(println "                            body-nodes))")
(println "        expanded (api/list-node")
(println "                  [(api/token-node 'def)")
(println "                   name-node")
(println "                   entity-map])]")
(println "    {:node expanded}))")
(println "")
(println "Config: {:hooks {:macroexpand {myapp.dsl/defentity hooks.dsl/expand-defentity}}}\n")

;;; Example 6: Route DSL

(println "Example 6: Route DSL Expansion")
(println "-------------------------------\n")

(println "(defn expand-defroute")
(println "  \"Expands route definition for proper analysis\"")
(println "  [{:keys [node]}]")
(println "  (let [[_ method path handler] (:children node)")
(println "        route-name (api/token-node (gensym \"route\"))")
(println "        route-def (api/map-node")
(println "                   [(api/keyword-node :method) method")
(println "                    (api/keyword-node :path) path")
(println "                    (api/keyword-node :handler) handler])")
(println "        expanded (api/list-node")
(println "                  [(api/token-node 'def)")
(println "                   route-name")
(println "                   route-def])]")
(println "    {:node expanded}))")
(println "")
(println "Config: {:hooks {:macroexpand {myapp.routes/defroute hooks.dsl/expand-defroute}}}\n")

;;; Example 7: Convention Enforcement

(println "Example 7: Naming Convention Enforcement")
(println "-----------------------------------------\n")

(println "(defn enforce-bang-suffix")
(println "  \"Enforces ! suffix on side-effect functions\"")
(println "  [{:keys [node]}]")
(println "  (let [fn-name (second (:children node))]")
(println "    (when (and fn-name (api/token-node? fn-name))")
(println "      (let [name-str (str (api/sexpr fn-name))]")
(println "        (when (and (not (clojure.string/ends-with? name-str \"!\"))")
(println "                   (or (clojure.string/includes? name-str \"save\")")
(println "                       (clojure.string/includes? name-str \"delete\")))")
(println "          {:findings [{:message \"Side-effect functions should end with !\"")
(println "                       :type :naming-convention")
(println "                       :row (api/row fn-name)")
(println "                       :col (api/col fn-name)")
(println "                       :level :warning}]})))))")
(println "")
(println "Config: {:hooks {:analyze-call {clojure.core/defn hooks.conventions/enforce-bang-suffix}}}\n")

;;; Example 8: Complete Hook File

(println "\n===================")
(println "Complete Hook File")
(println "===================\n")

(println "File: .clj-kondo/hooks/my_project.clj\n")

(println "(ns hooks.my-project")
(println "  (:require [clj-kondo.hooks-api :as api]))")
(println "")
(println "(defn deprecation [{:keys [node]}]")
(println "  {:findings [{:message \"Deprecated: use new-api\"")
(println "               :type :deprecated")
(println "               :row (api/row node)")
(println "               :col (api/col node)")
(println "               :level :warning}]})")
(println "")
(println "(defn validate-args [{:keys [node]}]")
(println "  (when (< (count (rest (:children node))) 2)")
(println "    {:findings [{:message \"Requires at least 2 arguments\"")
(println "                 :type :invalid-arity")
(println "                 :row (api/row node)")
(println "                 :col (api/col node)")
(println "                 :level :error}]}))")
(println "")
(println "File: .clj-kondo/config.edn\n")

(println "{:hooks {:analyze-call {mylib/old-api hooks.my-project/deprecation")
(println "                        mylib/query hooks.my-project/validate-args}}}")

(println "\n==================")
(println "Testing Your Hooks")
(println "==================\n")

(println "1. Create test file with triggering code")
(println "2. Run: clj-kondo --lint test-file.clj")
(println "3. Verify warnings/errors appear")
(println "")
(println "Test file example:")
(println "")
(println "(ns test)")
(println "(require '[mylib :as lib])")
(println "(lib/old-api)  ; Should warn")
(println "(lib/query :x) ; Should error")

(println "\n====================")
(println "Hook Development Tips")
(println "====================\n")

(println "• Start simple - deprecation warnings first")
(println "• Use api/sexpr to convert nodes to data")
(println "• Test with minimal examples")
(println "• Provide clear, actionable messages")
(println "• Keep hooks fast")
(println "• Handle edge cases gracefully")
(println "• Document what hooks check\n")

(println "For complete documentation, see SKILL.md")
