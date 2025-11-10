# clj-kondo Skill

A comprehensive skill for using clj-kondo, the fast and accurate Clojure linter, including guidance on writing custom hooks for domain-specific linting rules.

## Contents

- **SKILL.md** - Complete documentation covering usage, configuration, built-in linters, and custom hooks
- **QUICK_REFERENCE.md** - Quick reference for common configurations and hook patterns
- **INDEX.md** - Navigation guide and learning path
- **examples.clj** - Runnable hook examples

## What is clj-kondo?

clj-kondo is a static analyzer and linter for Clojure that provides:

- Fast, native binary with instant startup
- Comprehensive built-in linting rules
- Custom hooks for domain-specific linting
- Zero configuration required (but highly customizable)
- IDE integration (VS Code, Emacs, IntelliJ, Vim)
- CI/CD ready
- Cross-platform support (Linux, macOS, Windows)

## Quick Start

### Installation

```bash
# macOS/Linux
brew install clj-kondo/brew/clj-kondo

# Manual installation
curl -sLO https://raw.githubusercontent.com/clj-kondo/clj-kondo/master/script/install-clj-kondo
chmod +x install-clj-kondo
./install-clj-kondo
```

### Basic Usage

```bash
# Lint a file
clj-kondo --lint src/myapp/core.clj

# Lint a directory
clj-kondo --lint src

# Lint with JSON output
clj-kondo --lint src --config '{:output {:format :json}}'
```

### Configuration

Create `.clj-kondo/config.edn`:

```clojure
{:linters {:unused-binding {:level :warning}
           :unused-namespace {:level :warning}
           :unresolved-symbol {:level :error}}
 :output {:pattern "{{LEVEL}} {{filename}}:{{row}}:{{col}} {{message}}"}}
```

### Creating Your First Hook

1. **Create hook file** at `.clj-kondo/hooks/my_hooks.clj`:

```clojure
(ns hooks.my-hooks
  (:require [clj-kondo.hooks-api :as api]))

(defn deprecation-warning [{:keys [node]}]
  {:findings [{:message "This function is deprecated"
               :type :deprecated-api
               :row (api/row node)
               :col (api/col node)
               :level :warning}]})
```

2. **Register hook** in `.clj-kondo/config.edn`:

```clojure
{:hooks {:analyze-call {mylib/old-fn hooks.my-hooks/deprecation-warning}}}
```

3. **Test it**:

```bash
clj-kondo --lint src
```

## What This Skill Covers

### Basic Usage
- Installation and setup
- Command-line usage
- Output formats
- Cache management

### Configuration
- Configuration file structure
- Linter levels and options
- Local and global configuration
- Inline suppressions
- Configuration merging

### Built-in Linters
- Namespace and require linters
- Binding and symbol linters
- Function and arity linters
- Collection and syntax linters
- Type checking linters

### Custom Hooks (Advanced)
- What hooks are and when to use them
- Hook architecture and API
- `:analyze-call` hooks for custom warnings
- `:macroexpand` hooks for DSL support
- Node API reference
- Practical hook examples:
  - Deprecation warnings
  - Argument validation
  - DSL expansion
  - Thread-safety checks
  - Required keys validation
- Testing hooks
- Distributing hooks with libraries

### Integration
- IDE setup (VS Code, Emacs, IntelliJ, Vim)
- CI/CD integration (GitHub Actions, GitLab CI)
- Pre-commit hooks

### Best Practices
- Team configuration
- Gradual adoption for legacy code
- Performance optimization
- Thoughtful suppression

## Key Features Covered

### Built-in Linting
- Unused bindings and namespaces
- Unresolved symbols
- Invalid function arities
- Duplicate map/set keys
- Type mismatches
- Syntax errors
- Code style issues

### Custom Hooks
- API deprecation warnings
- Domain-specific validations
- Custom DSL support
- Team convention enforcement
- Advanced static analysis

### Developer Experience
- Real-time IDE feedback
- Minimal configuration
- Fast performance
- Clear error messages
- Extensibility

## Common Use Cases

### 1. Basic Project Linting

```bash
# Initial setup
cd my-project
clj-kondo --lint "$(clojure -Spath)" --dependencies --parallel --copy-configs

# Regular linting
clj-kondo --lint src test
```

### 2. Deprecation Warnings

Create hooks to warn about deprecated APIs:

```clojure
;; .clj-kondo/hooks/deprecation.clj
(defn warn-old-api [{:keys [node]}]
  {:findings [{:message "Use new-api instead"
               :level :warning
               :row (api/row node)
               :col (api/col node)}]})
```

### 3. DSL Linting

Expand custom macros for better analysis:

```clojure
;; .clj-kondo/hooks/dsl.clj
(defn expand-defentity [{:keys [node]}]
  (let [[_ name & body] (:children node)]
    {:node (api/list-node
            [(api/token-node 'def) name (api/map-node body)])}))
```

### 4. Team Standards

Enforce consistent aliases:

```clojure
{:linters {:consistent-alias {:level :warning
                              :aliases {clojure.string str
                                        clojure.set set}}}}
```

## Learning Path

1. **Start with README.md** (this file) - Quick overview
2. **Install clj-kondo** - Get it running
3. **Read SKILL.md "Getting Started"** - Basic usage
4. **Try basic linting** - Run on your code
5. **Configure for your project** - Customize linters
6. **Study built-in linters** - Understand what's checked
7. **Learn hook basics** - Read "Custom Hooks" section
8. **Write your first hook** - Start with deprecation warning
9. **Explore advanced hooks** - Study examples
10. **Integrate with IDE/CI** - Set up automation

## Hook Examples Preview

### Simple Deprecation Hook

```clojure
(defn deprecation [{:keys [node]}]
  {:findings [{:message "Deprecated: use new-fn"
               :type :deprecated
               :row (api/row node)
               :col (api/col node)}]})
```

### Argument Validation Hook

```clojure
(defn validate-args [{:keys [node]}]
  (let [args (rest (:children node))]
    (when (< (count args) 2)
      {:findings [{:message "Requires at least 2 arguments"
                   :type :invalid-args
                   :row (api/row node)
                   :col (api/col node)}]})))
```

### Macro Expansion Hook

```clojure
(defn expand-defroute [{:keys [node]}]
  (let [[_ method path & handlers] (:children node)]
    {:node (api/list-node
            [(api/token-node 'def)
             (api/token-node (gensym "route"))
             (api/map-node [method path (api/vector-node handlers)])])}))
```

## Why Use This Skill?

- **Comprehensive**: Covers all clj-kondo features including advanced hooks
- **Practical**: Real-world examples and patterns
- **Well-structured**: Easy navigation from basics to advanced topics
- **Hook-focused**: Extensive coverage of custom hook development
- **Production-ready**: Best practices for teams and CI/CD

## Additional Resources

- [Official GitHub Repository](https://github.com/clj-kondo/clj-kondo)
- [Configuration Reference](https://github.com/clj-kondo/clj-kondo/blob/master/doc/config.md)
- [Hooks Documentation](https://github.com/clj-kondo/clj-kondo/blob/master/doc/hooks.md)
- [Linters Reference](https://github.com/clj-kondo/clj-kondo/blob/master/doc/linters.md)
- [Hook Examples Repository](https://github.com/clj-kondo/clj-kondo/tree/master/examples)

## License

This skill documentation is provided as educational material. The clj-kondo tool is distributed under the EPL License (same as Clojure).
