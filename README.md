# Library Skills

A Claude Code plugin marketplace providing comprehensive documentation and examples for popular libraries. Get instant access to library guides, API references, and runnable examples directly within Claude Code.

## Project Overview

Library Skills is a curated collection of library documentation packaged as Claude Code skills. Instead of searching external docs or switching contexts, you can invoke a skill to load comprehensive library knowledge into your Claude Code session.

**Value Proposition:**
- **Instant Access**: Load library documentation without leaving Claude Code
- **Comprehensive Guides**: Detailed API references, examples, and best practices
- **Runnable Examples**: Test code snippets with included executable examples
- **Always Up-to-Date**: Skills maintained alongside library releases

**Available Plugins:**

- **clojure-libraries** - Clojure and Babashka library skills
  - `babashka.fs` - File system operations
  - `clj-kondo` - Code linting and static analysis
  - `babashka-cli` - Command-line argument parsing
  - `telemere` - Structured logging and telemetry
  - `selmer` - Django-inspired HTML templating

- **emacs-libraries** - Emacs Lisp library skills
  - `ert` - Emacs Lisp Regression Testing
  - `magit-section` - Collapsible hierarchical buffer UIs
  - `widget` - Interactive UI elements in buffers
  - `package-conventions` - Emacs package development standards

## Installation

### Prerequisites

- [Claude Code](https://code.claude.com) installed
- Git (for contributors)

### Add the Marketplace

From GitHub:
```bash
/plugin marketplace add hugoduncan/library-skills
```

For local development:
```bash
cd /path/to/library-skills
/plugin marketplace add ./
```

### Install Plugins

Install a specific plugin:
```bash
/plugin install clojure-libraries@library-skills
```

Or install all plugins:
```bash
/plugin install emacs-libraries@library-skills
/plugin install clojure-libraries@library-skills
```

## Available Plugins

### clojure-libraries

Skills for Clojure and Babashka libraries.

**Skills:**

#### babashka.fs
Cross-platform file system operations for Clojure and Babashka.

**Invoke with:**
```
Use the clojure-libraries:babashka.fs skill
```

**Use for:**
- File and directory operations
- Path manipulation
- File searching and globbing
- Cross-platform file system tasks
- Build scripts and automation

#### clj-kondo
Code linting and static analysis for Clojure.

**Invoke with:**
```
Use the clojure-libraries:clj-kondo skill
```

**Use for:**
- Clojure code linting and static analysis
- Writing custom lint hooks
- Configuring linters for your project
- CI/CD integration
- Code quality automation

#### babashka-cli
Command-line argument parsing for turning Clojure functions into CLIs.

**Invoke with:**
```
Use the clojure-libraries:babashka-cli skill
```

**Use for:**
- Building CLI tools in Clojure
- Parsing command-line arguments
- Creating subcommands
- Auto-generating help text

#### telemere
Structured logging and telemetry for Clojure/Script with tracing and performance monitoring.

**Invoke with:**
```
Use the clojure-libraries:telemere skill
```

**Use for:**
- Application logging and structured telemetry
- Distributed tracing and request flow tracking
- Performance monitoring and bottleneck identification
- Error tracking with structured context
- Observability and production monitoring
- Debugging with trace and spy

#### selmer
Django-inspired HTML templating system for Clojure.

**Invoke with:**
```
Use the clojure-libraries:selmer skill
```

**Use for:**
- Web application HTML generation
- Email template rendering
- Server-side rendering
- Report generation with templates
- Dynamic content with filters and tags
- Template-based text output

### emacs-libraries

Skills for Emacs Lisp libraries.

**Skills:**

#### ert
Emacs Lisp Regression Testing framework.

**Invoke with:**
```
Use the emacs-libraries:ert skill
```

**Use for:**
- Writing tests for Emacs Lisp code
- Test organization and suites
- Test assertions and expectations
- Running tests interactively or in batch mode

#### magit-section
Build collapsible, hierarchical buffer UIs in Emacs.

**Invoke with:**
```
Use the emacs-libraries:magit-section skill
```

**Use for:**
- Creating collapsible section-based UIs
- Building hierarchical buffer interfaces
- Interactive buffer navigation
- Custom status buffers

#### widget
Interactive UI elements in Emacs buffers.

**Invoke with:**
```
Use the emacs-libraries:widget skill
```

**Use for:**
- Creating interactive forms
- Building UI elements (buttons, fields, menus)
- Custom configuration interfaces
- User input dialogs

#### package-conventions
Emacs Lisp package development standards and conventions.

**Invoke with:**
```
Use the emacs-libraries:package-conventions skill
```

**Use for:**
- Creating new Emacs packages
- MELPA submission guidelines
- Package naming and structure conventions
- Documentation and metadata standards
- Code quality and testing

## Usage

After installing plugins, skills are automatically available in Claude Code. Invoke a skill by mentioning it to Claude using the fully qualified name format: `plugin:skill`.

### Basic Invocation

```
Use the clojure-libraries:babashka.fs skill
```

This loads the comprehensive babashka.fs documentation into your Claude Code session, giving you instant access to:
- Complete API reference
- Practical examples
- Best practices
- Common use cases

### Typical Workflow

1. **Start a task** that involves a library
2. **Invoke the skill** by mentioning it to Claude
3. **Ask questions** or request code using the library
4. **Test examples** from the skill's examples file

Example session:
```
Use the clojure-libraries:babashka.fs skill

How do I find all .clj files recursively?
Show me how to copy a directory tree
What's the best way to work with temporary directories?
```

### Benefits

**vs. Manual Documentation Lookup:**
- No context switching to external docs
- All examples immediately available
- API reference integrated with your session
- Consistent format across libraries

**vs. Generic Claude Knowledge:**
- Up-to-date library-specific information
- Runnable, tested examples
- Library best practices and patterns
- Comprehensive API coverage

## Contributing New Skills

We welcome contributions of new library skills! Here's the step-by-step process:

### 1. Clone the Repository

```bash
git clone https://github.com/hugoduncan/library-skills.git
cd library-skills
```

### 2. Use the /add-skill Command

Create a new skill using Claude Code's `/add-skill` command. Provide the library name and optional description:

```bash
/add-skill babashka.process for managing processes and shell commands
```

The command will:
- Research the library documentation
- Infer the correct plugin (clojure-libraries or emacs-libraries)
- Generate comprehensive skill documentation
- Create all necessary files

### 3. Review Generated Files

The `/add-skill` command creates:
- `SKILL.md` - Comprehensive library documentation
- `metadata.edn` - Structured skill metadata
- `examples.clj` (Clojure) or example code (Emacs) - Runnable examples

### 4. Test Examples

For Clojure library skills with `examples.clj`:
```bash
bb examples.clj
```

Ensure all examples run without errors.

### 5. Create a Pull Request

```bash
git checkout -b add-skill-<library-name>
git add plugins/*/skills/<library-name>/
git commit -m "feat: add <library-name> skill to <plugin-name> plugin"
git push origin add-skill-<library-name>
```

Then create a PR on GitHub.

### Detailed Documentation

For complete details on skill structure, file formats, and conventions, see [CLAUDE.md](CLAUDE.md).

## Testing & Verification

Contributors should test their skills before submitting:

### Test Runnable Examples

For Clojure skills:
```bash
bb plugins/clojure-libraries/skills/<library-name>/examples.clj
```

### Verify Skill Loads

In Claude Code, invoke the skill:
```
Use the clojure-libraries:<library-name> skill
```
or
```
Use the emacs-libraries:<library-name> skill
```

Verify the skill loads without errors and documentation is complete.

### Checklist

Before submitting a PR:
- [ ] Examples run successfully
- [ ] `metadata.edn` has all required fields
- [ ] SKILL.md is comprehensive and well-structured
- [ ] Code examples use proper syntax
- [ ] Skill loads in Claude Code without errors

## License

This project is licensed under the Eclipse Public License 2.0 - see the [LICENSE](LICENSE) file for details.
