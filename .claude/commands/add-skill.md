---
description: Create a new comprehensive skill for a Clojure library
arguments: <library-name> [description and context]
examples:
  - /add-skill babashka.process for managing processes and shell commands
  - /add-skill honey.sql SQL query builder library
  - /add-skill next.jdbc JDBC wrapper with connection pooling
tags: [skill, library, clojure, documentation]
---

# Add Skill Command

Create a new skill in the library-skills plugin repository.

Use the claude-marketplace skill.

## Task

You are to create a comprehensive skill for a Clojure library. The skill
will be added to the `plugins/clojure-libraries/skills/` directory.

## Command Arguments

Parse $ARGUMENTS to extract:
- Library name (required) - the library to create a skill for
- Brief description (optional) - what the library does
- Additional context (optional) - specific focus areas or aspects

## Steps

1. **Research the Library**
   - Use WebSearch to find the library's official documentation
   - Identify the library's GitHub repository
   - Find the latest version number
   - Understand the library's purpose, main features, and API
   - Identify common use cases and patterns

2. **Create Directory Structure**
   - Create `plugins/clojure-libraries/skills/{library-name}/`

3. **Generate SKILL.md**
   Structure:
   - Overview and introduction
   - Core concepts
   - API reference organized by category
   - Common patterns and best practices
   - Use cases
   - Error handling
   - Performance considerations
   - Platform-specific notes (if applicable)

   Make it comprehensive and practical. Be terse, concise and precise.

4. **Report Completion**
    - List all files created
    - Show the skill's location

## Guidelines

- Follow all repository conventions from CLAUDE.md
- Make all examples practical and runnable
- Keep documentation clear and well-structured
- Research thoroughly before generating content
- Include error handling
- Add performance tips where relevant

## Notes

- All skills should be comprehensive enough to use the library without referring to external docs
- Focus on practical usage and common patterns
- Include both beginner and advanced content
