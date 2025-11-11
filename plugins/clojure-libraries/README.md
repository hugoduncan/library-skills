# Clojure Libraries Plugin

Skills for various Clojure libraries including babashka.fs and clj-kondo.

## Installation

### From Marketplace

```bash
/plugin marketplace add hugoduncan/library-skills
/plugin install clojure-libraries@library-skills
```

### Local Development

```bash
/plugin marketplace add ./
/plugin install clojure-libraries@library-skills
```

## Available Skills

### babashka.fs

Comprehensive guide to using babashka.fs for cross-platform file system operations in Clojure and Babashka.

**Invoke with:**
```
/skill clojure-libraries:babashka.fs
```

**Use for:**
- File and directory operations
- Path manipulation
- File searching and globbing
- Cross-platform file system tasks
- Build scripts and automation

**Documentation:**
- [SKILL.md](skills/babashka.fs/SKILL.md) - Comprehensive guide
- [README.md](skills/babashka.fs/README.md) - Quick start
- [QUICK_REFERENCE.md](skills/babashka.fs/QUICK_REFERENCE.md) - Cheat sheet
- [examples.clj](skills/babashka.fs/examples.clj) - Runnable examples

### clj-kondo

Comprehensive guide to using clj-kondo for Clojure code linting, including configuration, built-in linters, and writing custom hooks.

**Invoke with:**
```
/skill clojure-libraries:clj-kondo
```

**Use for:**
- Clojure code linting and static analysis
- Writing custom lint hooks
- Configuring linters for your project
- CI/CD integration
- Code quality automation

**Documentation:**
- [SKILL.md](skills/clj-kondo/SKILL.md) - Comprehensive guide
- [README.md](skills/clj-kondo/README.md) - Quick start
- [QUICK_REFERENCE.md](skills/clj-kondo/QUICK_REFERENCE.md) - Cheat sheet
- [examples.clj](skills/clj-kondo/examples.clj) - Runnable examples

## Usage

After installation, skills are automatically available in Claude Code. Use the `/skill` command with the fully qualified name (plugin:skill) to invoke a skill.

Example:
```
/skill clojure-libraries:babashka.fs
```

This loads the skill and makes its knowledge available to Claude Code for your current session.

## Testing Examples

Each skill includes runnable examples. To test them:

```bash
cd plugins/clojure-libraries/skills/babashka.fs
bb examples.clj

cd plugins/clojure-libraries/skills/clj-kondo
bb examples.clj
```

## License

EPL-2.0
