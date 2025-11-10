# Babashka.fs Skill - Index

Welcome to the comprehensive babashka.fs skill! This skill provides everything you need to master file system operations in Clojure and Babashka.

## 📚 Documentation Files

### 1. [SKILL.md](SKILL.md) - Main Documentation
**Size:** ~23KB | **Reading time:** 30-45 minutes

The comprehensive guide covering:
- Overview and setup
- Core concepts (Path objects, cross-platform support)
- Path operations (creating, manipulating, components)
- File and directory checks
- Creating files and directories
- Reading and writing files
- Copying, moving, and deleting
- Listing and traversing directories
- Searching and filtering (glob and match)
- File metadata and attributes
- Archive operations (zip/unzip)
- System paths and utilities
- Advanced patterns and best practices
- Common use cases and recipes
- Error handling and edge cases
- Performance tips
- Testing and mocking
- Platform-specific considerations

**Start here** if you want a complete understanding of the library.

### 2. [README.md](README.md) - Getting Started
**Size:** ~5KB | **Reading time:** 5-10 minutes

Quick overview including:
- What is babashka.fs?
- Quick start examples
- How to use this skill
- Key features overview
- Common use cases
- Integration examples
- Learning path

**Start here** if you want a quick introduction.

### 3. [QUICK_REFERENCE.md](QUICK_REFERENCE.md) - Cheat Sheet
**Size:** ~7KB | **Quick lookup**

Concise reference with:
- Function signatures organized by category
- Common glob patterns
- Frequent usage patterns
- Tips and anti-patterns
- Error handling patterns

**Use this** when you need to quickly look up a function or pattern.

### 4. [examples.clj](examples.clj) - Runnable Examples
**Size:** ~6KB | **Executable script**

13 practical examples demonstrating:
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
13. Temp directory context

**Run this** to see the library in action:
```bash
bb examples.clj
```

### 5. [metadata.edn](metadata.edn) - Skill Metadata
**Size:** ~5KB | **Machine-readable**

Structured information about:
- Skill properties and versioning
- Library information
- Use cases and features
- Learning path
- Platform support
- API coverage

## 🎯 Quick Navigation

### By Experience Level

**Beginner**
1. Read [README.md](README.md) for overview
2. Run [examples.clj](examples.clj) to see it work
3. Browse [QUICK_REFERENCE.md](QUICK_REFERENCE.md) for common functions
4. Read "Core Concepts" in [SKILL.md](SKILL.md)

**Intermediate**
1. Review "Path Operations" in [SKILL.md](SKILL.md)
2. Study "Searching and Filtering" section
3. Learn "Advanced Patterns and Best Practices"
4. Try implementing the recipes

**Advanced**
1. Deep dive into "Common Use Cases and Recipes"
2. Study error handling and performance sections
3. Review platform-specific considerations
4. Implement your own patterns

### By Task

**Need to find files?**
- SKILL.md → "Searching and Filtering: Glob and Match"
- QUICK_REFERENCE.md → "Searching" and "Common Glob Patterns"
- examples.clj → Example 8 (Glob patterns)

**Need to copy/move files?**
- SKILL.md → "Copying, Moving, and Deleting"
- QUICK_REFERENCE.md → "Copying/Moving/Deleting"
- examples.clj → Example 4 (Copy and move)

**Need to work with paths?**
- SKILL.md → "Path Operations"
- QUICK_REFERENCE.md → "Path Operations"
- examples.clj → Example 5 (Path manipulation)

**Need temporary files?**
- SKILL.md → "Creating Files and Directories" + "Working with Temporary Files"
- QUICK_REFERENCE.md → "Temporary Files"
- examples.clj → Examples 12-13 (Temp files)

**Need to process directories?**
- SKILL.md → "Listing and Traversing Directories"
- examples.clj → Examples 9-10 (Walking and filtering)

## 🚀 Suggested Learning Path

### Day 1: Foundations (1-2 hours)
1. ✅ Read README.md overview
2. ✅ Run examples.clj and study output
3. ✅ Read "Core Concepts" in SKILL.md
4. ✅ Review "Path Operations" in SKILL.md
5. ✅ Bookmark QUICK_REFERENCE.md for lookups

### Day 2: Core Skills (2-3 hours)
1. ✅ Study "File and Directory Checks"
2. ✅ Learn "Creating Files and Directories"
3. ✅ Practice "Reading and Writing Files"
4. ✅ Master "Copying, Moving, and Deleting"
5. ✅ Write your own simple script

### Day 3: Advanced Features (2-3 hours)
1. ✅ Deep dive into "Searching and Filtering"
2. ✅ Learn glob patterns thoroughly
3. ✅ Study "File Metadata and Attributes"
4. ✅ Practice with real-world scenarios
5. ✅ Review "Advanced Patterns"

### Day 4: Production Skills (1-2 hours)
1. ✅ Study "Common Use Cases and Recipes"
2. ✅ Learn "Error Handling and Edge Cases"
3. ✅ Review "Performance Tips"
4. ✅ Understand "Platform-Specific Considerations"
5. ✅ Implement a complete project

## 📊 Skill Coverage

This skill covers **100%** of the babashka.fs public API including:

- ✅ 40+ file system functions
- ✅ Path creation and manipulation
- ✅ File operations (create, read, write, delete)
- ✅ Directory operations (list, walk, create)
- ✅ Pattern matching (glob, regex)
- ✅ Metadata access (size, times, permissions)
- ✅ Archive operations (zip, unzip)
- ✅ System paths (home, temp, PATH)
- ✅ XDG directories (Linux/Unix)
- ✅ Temporary file management
- ✅ Cross-platform support

## 🎓 What You'll Learn

After completing this skill, you'll be able to:

- ✅ Perform all common file system operations in Clojure
- ✅ Write cross-platform file manipulation code
- ✅ Use glob patterns effectively for finding files
- ✅ Handle file metadata and permissions
- ✅ Manage temporary files safely
- ✅ Build robust file processing scripts
- ✅ Implement file-based automation tasks
- ✅ Handle errors gracefully
- ✅ Optimize file operations for performance
- ✅ Follow best practices for production code

## 🔗 External Resources

- [Official GitHub Repository](https://github.com/babashka/fs)
- [API Documentation](https://github.com/babashka/fs/blob/master/API.md)
- [Babashka Book](https://book.babashka.org/)
- [cljdoc API Docs](https://cljdoc.org/d/babashka/fs/)

## 📝 Version Information

- **Skill Version:** 1.0.0
- **Library Version:** 0.5.27
- **Created:** 2025-11-09
- **Language:** Clojure
- **Platform:** Cross-platform (Linux, macOS, Windows)
- **License:** EPL-1.0

## 🎯 Next Steps

1. Choose your starting point based on experience level
2. Follow the suggested learning path
3. Run the examples to see code in action
4. Use QUICK_REFERENCE.md for fast lookups
5. Implement your own projects
6. Share your learnings!

---

**Ready to start?** Begin with [README.md](README.md) for a gentle introduction, or dive straight into [SKILL.md](SKILL.md) for comprehensive coverage!
