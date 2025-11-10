# clj-kondo Skill - Index

Welcome to the comprehensive clj-kondo skill! Master Clojure linting, from basic usage to writing custom hooks for domain-specific rules.

## 📚 Documentation Files

### 1. [README.md](README.md) - Getting Started
**Size:** ~8KB | **Reading time:** 10-15 minutes

Quick overview including:
- What is clj-kondo?
- Installation
- Basic usage examples
- First hook example
- What the skill covers
- Learning path

**Start here** if you're new to clj-kondo.

### 2. [SKILL.md](SKILL.md) - Complete Guide
**Size:** ~35KB | **Reading time:** 45-60 minutes

Comprehensive documentation covering:
- Introduction and installation
- Getting started and basic usage
- Configuration management
- Built-in linters reference
- **Custom hooks development** (extensive section)
- Hook API reference
- Practical hook examples
- IDE integration
- CI/CD integration
- Best practices
- Troubleshooting

**Start here** for comprehensive learning or reference.

### 3. [QUICK_REFERENCE.md](QUICK_REFERENCE.md) - Cheat Sheet
**Size:** ~7KB | **Quick lookup**

Concise reference with:
- Command-line usage
- Common configurations
- Linter settings
- Hook patterns
- IDE integration snippets
- CI/CD templates
- Troubleshooting tips

**Use this** when you need quick reference.

### 4. [examples.clj](examples.clj) - Hook Examples
**Size:** ~8KB | **Executable script**

8 practical hook examples:
1. Basic deprecation warning
2. Argument count validation
3. Argument type validation
4. Required map keys validation
5. Macro expansion for DSL
6. Route definition expansion
7. Thread-safety hints
8. Team convention enforcement

Plus complete hook file template.

**Run this** to see hook patterns:
```bash
bb examples.clj
```

### 5. [metadata.edn](metadata.edn) - Skill Metadata
**Size:** ~4KB | **Machine-readable**

Structured information about:
- Skill properties and versioning
- Library information
- Use cases and features
- Learning path
- Platform support

## 🎯 Quick Navigation

### By Experience Level

**Beginner** (Never used clj-kondo)
1. Read [README.md](README.md) for overview
2. Install clj-kondo
3. Run basic linting on your code
4. Review "Getting Started" in [SKILL.md](SKILL.md)
5. Understand built-in linters

**Intermediate** (Used clj-kondo, want customization)
1. Read "Configuration" in [SKILL.md](SKILL.md)
2. Study "Built-in Linters" section
3. Customize for your project
4. Set up IDE integration
5. Add to CI/CD pipeline

**Advanced** (Ready to write hooks)
1. Read "Custom Hooks" in [SKILL.md](SKILL.md)
2. Run [examples.clj](examples.clj) to see patterns
3. Study Hook API reference
4. Write your first deprecation hook
5. Progress to complex validation hooks
6. Learn macro expansion hooks

### By Task

**Need to install and start using clj-kondo?**
- README.md → "Quick Start"
- SKILL.md → "Installation" and "Getting Started"

**Need to configure linters?**
- SKILL.md → "Configuration"
- QUICK_REFERENCE.md → "Basic Configuration" and "Common Linter Configurations"

**Want to understand what clj-kondo checks?**
- SKILL.md → "Built-in Linters"
- QUICK_REFERENCE.md → "Common Linter Configurations"

**Need to write a deprecation warning hook?**
- SKILL.md → "Custom Hooks" → "Creating Your First Hook"
- examples.clj → Example 1 (Deprecation Warning)
- QUICK_REFERENCE.md → "Common Hook Patterns" → "Deprecation Warning"

**Need to validate function arguments?**
- SKILL.md → "Custom Hooks" → "Practical Hook Examples"
- examples.clj → Examples 2-4 (Argument validation)
- QUICK_REFERENCE.md → "Common Hook Patterns"

**Need to support a custom DSL/macro?**
- SKILL.md → "Custom Hooks" → ":macroexpand Hooks"
- examples.clj → Examples 5-6 (Macro expansion)
- QUICK_REFERENCE.md → "macroexpand Hook"

**Need to integrate with IDE?**
- SKILL.md → "IDE Integration"
- QUICK_REFERENCE.md → "IDE Integration"

**Need to add to CI/CD?**
- SKILL.md → "CI/CD Integration"
- QUICK_REFERENCE.md → "CI/CD Patterns"

## 🚀 Suggested Learning Paths

### Path 1: Basic User (2-3 hours)

**Goal:** Use clj-kondo effectively for your projects

1. ✅ **Read README.md** (10 min)
   - Understand what clj-kondo does
   - See quick examples

2. ✅ **Install and test** (15 min)
   - Install clj-kondo
   - Run on your codebase
   - Review findings

3. ✅ **Study SKILL.md: Getting Started** (20 min)
   - Command-line usage
   - Output formats
   - Basic workflow

4. ✅ **Study SKILL.md: Configuration** (30 min)
   - Configuration file structure
   - Linter levels
   - Inline suppressions

5. ✅ **Study SKILL.md: Built-in Linters** (30 min)
   - Understand what's checked
   - Configure for your needs

6. ✅ **Integrate with IDE** (20 min)
   - Set up editor integration
   - Test real-time linting

7. ✅ **Practice** (30 min)
   - Configure for your project
   - Fix some linting issues
   - Customize linter levels

### Path 2: Hook Developer (6-8 hours)

**Goal:** Write custom hooks for domain-specific linting

**Prerequisites:** Complete Basic User path

1. ✅ **Study SKILL.md: Custom Hooks intro** (45 min)
   - What are hooks
   - When to use hooks
   - Hook architecture

2. ✅ **Run examples.clj** (15 min)
   - See hook patterns in action
   - Understand hook structure

3. ✅ **Study SKILL.md: Hook API Reference** (45 min)
   - Node functions
   - Node constructors
   - Return values

4. ✅ **Write first hook: Deprecation** (60 min)
   - Create hook file
   - Register in config
   - Test it

5. ✅ **Study examples.clj in detail** (60 min)
   - Analyze each example
   - Understand patterns
   - Note code structure

6. ✅ **Write validation hooks** (90 min)
   - Argument count validation
   - Argument type validation
   - Map keys validation

7. ✅ **Study macroexpand hooks** (60 min)
   - SKILL.md → ":macroexpand Hooks"
   - examples.clj → Examples 5-6
   - Understand node transformation

8. ✅ **Write DSL expansion hook** (90 min)
   - For your own macros
   - Test thoroughly
   - Document usage

9. ✅ **Study SKILL.md: Testing and Distribution** (30 min)
   - Testing strategies
   - Distribution patterns

10. ✅ **Practice** (90 min)
    - Write hooks for your codebase
    - Test edge cases
    - Document hooks

### Path 3: Team Lead (4-5 hours)

**Goal:** Set up clj-kondo for team with custom rules

**Prerequisites:** Complete Basic User path

1. ✅ **Study SKILL.md: Configuration** (deep dive) (45 min)
   - Team configuration strategies
   - Consistent aliases
   - Convention enforcement

2. ✅ **Set up team configuration** (60 min)
   - Define team standards
   - Configure linters
   - Document choices

3. ✅ **Study custom hooks** (90 min)
   - SKILL.md → "Custom Hooks"
   - examples.clj → All examples
   - Identify team needs

4. ✅ **Write team convention hooks** (90 min)
   - Naming conventions
   - API usage rules
   - Deprecation warnings

5. ✅ **Set up CI/CD** (45 min)
   - SKILL.md → "CI/CD Integration"
   - Add to your pipeline
   - Configure failure thresholds

6. ✅ **Documentation** (30 min)
   - Document configuration
   - Document custom hooks
   - Create team guide

## 📊 Skill Coverage

This skill covers **100%** of clj-kondo's core functionality:

### Basic Usage
- ✅ Installation (all platforms)
- ✅ Command-line usage
- ✅ Output formats
- ✅ Cache management

### Configuration
- ✅ Configuration file structure
- ✅ Linter levels
- ✅ Global and local config
- ✅ Inline suppressions
- ✅ Configuration merging

### Built-in Linters
- ✅ Namespace linters
- ✅ Binding linters
- ✅ Function/arity linters
- ✅ Collection linters
- ✅ Type checking

### Custom Hooks (Advanced)
- ✅ Hook architecture
- ✅ `:analyze-call` hooks
- ✅ `:macroexpand` hooks
- ✅ Hook API reference
- ✅ 8+ practical examples
- ✅ Testing strategies
- ✅ Distribution patterns

### Integration
- ✅ VS Code (Calva)
- ✅ Emacs
- ✅ IntelliJ/Cursive
- ✅ Vim/Neovim
- ✅ GitHub Actions
- ✅ GitLab CI
- ✅ Pre-commit hooks

## 🎓 What You'll Learn

After completing this skill:

**Basic Level:**
- ✅ Install and run clj-kondo
- ✅ Understand linting output
- ✅ Configure linters for your project
- ✅ Suppress warnings appropriately
- ✅ Integrate with IDE
- ✅ Add to CI/CD pipeline

**Advanced Level:**
- ✅ Write deprecation warning hooks
- ✅ Validate function arguments
- ✅ Check required map keys
- ✅ Expand custom macros for analysis
- ✅ Enforce team conventions
- ✅ Test hooks effectively
- ✅ Distribute hooks with libraries

## 💡 Use Cases Covered

1. **Basic Linting** - Catch syntax errors and common mistakes
2. **Code Quality** - Enforce best practices
3. **API Deprecation** - Warn about deprecated functions
4. **Argument Validation** - Check function arguments
5. **DSL Support** - Analyze custom macros
6. **Team Conventions** - Enforce naming and style rules
7. **Domain Rules** - Validate business logic
8. **CI/CD Integration** - Automated quality checks

## 🔗 External Resources

- [Official GitHub Repository](https://github.com/clj-kondo/clj-kondo)
- [Configuration Reference](https://github.com/clj-kondo/clj-kondo/blob/master/doc/config.md)
- [Hooks API Documentation](https://github.com/clj-kondo/clj-kondo/blob/master/doc/hooks.md)
- [Linters Reference](https://github.com/clj-kondo/clj-kondo/blob/master/doc/linters.md)
- [Hook Examples Repository](https://github.com/clj-kondo/clj-kondo/tree/master/examples)

## 📝 Version Information

- **Skill Version:** 1.0.0
- **clj-kondo Version:** 2024.11.14
- **Created:** 2025-11-10
- **Language:** Clojure
- **Platform:** Cross-platform (Linux, macOS, Windows)
- **License:** EPL-1.0

## 🎯 Next Steps

### If you're new to clj-kondo:
1. Start with [README.md](README.md)
2. Follow "Path 1: Basic User"
3. Practice on your projects

### If you want to write hooks:
1. Complete Basic User path first
2. Read [SKILL.md](SKILL.md) "Custom Hooks" section
3. Run [examples.clj](examples.clj)
4. Follow "Path 2: Hook Developer"

### If you need quick reference:
1. Use [QUICK_REFERENCE.md](QUICK_REFERENCE.md)
2. Bookmark for fast lookups

---

**Ready to start?** Begin with [README.md](README.md) for an introduction, or jump to [SKILL.md](SKILL.md) for comprehensive coverage!
