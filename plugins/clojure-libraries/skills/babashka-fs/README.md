# Babashka.fs Skill

A comprehensive skill for using the `babashka.fs` file system utility library in Clojure and Babashka.

## Contents

- **SKILL.md** - Complete documentation and guide for using babashka.fs
- **examples.clj** - Runnable examples demonstrating key features

## What is babashka.fs?

`babashka.fs` is a cross-platform file system utility library for Clojure that provides:

- Intuitive file and directory operations
- Powerful file searching with glob patterns
- Path manipulation utilities
- File metadata access
- Archive operations (zip/unzip)
- Cross-platform compatibility
- Built-in to Babashka (no dependencies needed)

## Quick Start

```clojure
#!/usr/bin/env bb

(require '[babashka.fs :as fs])

;; Check if a file exists
(fs/exists? "README.md")

;; Find all Clojure files
(fs/glob "." "**/*.clj")

;; Copy a file
(fs/copy "source.txt" "dest.txt")

;; Create directories
(fs/create-dirs "path/to/new/dir")

;; Work with temporary directories
(fs/with-temp-dir [tmp {}]
  (spit (fs/path tmp "test.txt") "data")
  ;; tmp automatically deleted after
  )
```

## Using This Skill

### Reading the Documentation

The `SKILL.md` file contains:

- Complete API reference organized by category
- Detailed examples for each function
- Common patterns and best practices
- Real-world use cases and recipes
- Performance tips and error handling
- Platform-specific considerations

### Running the Examples

The `examples.clj` file is an executable Babashka script:

```bash
# Make executable
chmod +x examples.clj

# Run with babashka
bb examples.clj

# Or directly if executable
./examples.clj
```

The examples demonstrate:

1. Basic file operations
2. Directory listing and filtering
3. Creating directory structures
4. Copy and move operations
5. Path manipulation
6. File metadata
7. Finding executables in PATH
8. Glob pattern matching
9. Recursive directory walking
10. File filtering pipelines
11. XDG base directories
12. Temporary file management

## Key Features Covered

### File Operations
- Creating, copying, moving, deleting files
- Reading and writing content
- Working with temporary files

### Directory Operations
- Listing directory contents
- Creating directory hierarchies
- Recursive tree walking
- Directory streams for efficiency

### Searching and Filtering
- Glob patterns for finding files
- Regular expression matching
- Custom filters and predicates
- File metadata queries

### Path Manipulation
- Joining path components
- Getting file names, extensions, parents
- Converting between relative and absolute paths
- Cross-platform path handling

### Advanced Features
- Archive operations (zip/unzip)
- File permissions (POSIX)
- Timestamps and metadata
- XDG base directories
- Finding executables

## Common Use Cases

The skill includes complete recipes for:

- Build tool tasks
- File backup systems
- Log rotation
- File synchronization
- Finding duplicate files
- Cross-platform scripts
- Testing with temporary files

## Integration

### With Babashka

```clojure
;; In bb.edn
{:tasks
 {:requires ([babashka.fs :as fs])
  
  clean {:doc "Remove build artifacts"
         :task (fs/delete-tree "target")}
  
  build {:doc "Build project"
         :task (do
                 (fs/create-dirs "target")
                 (println "Building..."))}}}
```

### With Clojure Projects

```clojure
;; deps.edn
{:deps {babashka/fs {:mvn/version "0.5.27"}}}

;; In your namespace
(ns myproject.core
  (:require [babashka.fs :as fs]))
```

## Why Use This Skill?

- **Comprehensive**: Covers all major functionality with examples
- **Practical**: Real-world patterns and recipes included
- **Cross-platform**: Learn once, works everywhere
- **Modern**: Uses NIO.2 for good performance
- **Battle-tested**: babashka.fs is widely used in the Clojure community

## Learning Path

1. **Start with SKILL.md "Core Concepts"** - Understand Path objects and cross-platform support
2. **Try the examples** - Run `examples.clj` to see it in action
3. **Review "Common Use Cases"** - See practical recipes
4. **Explore "Advanced Patterns"** - Learn best practices
5. **Reference as needed** - Use Quick Reference for common functions

## Additional Resources

- [Official GitHub Repository](https://github.com/babashka/fs)
- [API Documentation](https://github.com/babashka/fs/blob/master/API.md)
- [Babashka Book](https://book.babashka.org/)
- [cljdoc Documentation](https://cljdoc.org/d/babashka/fs/)

## License

This skill documentation is provided as educational material. The babashka.fs library itself is distributed under the EPL License (same as Clojure).

## Contributing

This skill is part of the Agent-o-rama skills collection. The examples and documentation are designed to help Claude (and humans!) effectively use the babashka.fs library.

For issues with the library itself, please visit the [official repository](https://github.com/babashka/fs).
