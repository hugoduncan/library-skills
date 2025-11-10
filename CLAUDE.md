# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Repository Overview

This is a Claude Code plugin marketplace repository for library skills. It contains skills (documentation and examples) for various libraries, packaged as Claude Code plugins.

## Repository Structure

```
library-skills/
├── .claude-plugin/         # Marketplace metadata
│   └── marketplace.json    # Marketplace configuration
├── plugins/                # Plugin directory
│   └── clojure-libraries/  # Plugin for Clojure libraries
│       ├── .claude-plugin/ # Plugin metadata
│       │   └── plugin.json
│       └── skills/         # Individual skills
│           └── babashka-fs/
│               ├── SKILL.md           # Main documentation
│               ├── README.md          # Quick start guide
│               ├── QUICK_REFERENCE.md # Cheat sheet
│               ├── INDEX.md           # Navigation guide
│               ├── examples.clj       # Runnable examples
│               ├── metadata.edn       # Skill metadata
│               └── SUMMARY.txt        # Condensed summary
```

## Skill File Structure

Each skill must include:
- `SKILL.md` - Main comprehensive documentation
- `README.md` - Quick start and overview
- `metadata.edn` - Structured skill metadata (EDN format)
- `examples.clj` (optional) - Executable Babashka script with examples

Optional files:
- `QUICK_REFERENCE.md` - Concise cheat sheet
- `INDEX.md` - Navigation and learning path guide
- `SUMMARY.txt` - Condensed summary for AI consumption

## Metadata Files

### marketplace.json

Located at `.claude-plugin/marketplace.json`, defines:
- Marketplace name and owner
- List of plugins with their source locations
- Plugin categories and tags

### plugin.json

Located at `plugins/{plugin-name}/.claude-plugin/plugin.json`, defines:
- Plugin name, description, version
- Author information
- Keywords and license

### metadata.edn

Located at `plugins/{plugin-name}/skills/{skill-name}/metadata.edn`, defines:
- Skill name, version, description
- Library information (name, version, URL, license)
- Tags, use cases, features
- File structure
- Learning path
- Platform support and API coverage

## Creating New Skills

1. Create directory structure under `plugins/{plugin-name}/skills/{skill-name}/`
2. Write `metadata.edn` with skill metadata
3. Create `SKILL.md` with comprehensive documentation
4. Add `README.md` for quick start
5. Include runnable `examples.clj` if applicable
6. Optionally add `QUICK_REFERENCE.md` and `INDEX.md`
7. Update plugin's `plugin.json` if adding to existing plugin

## Adding New Plugins

1. Create `plugins/{plugin-name}/` directory
2. Add `.claude-plugin/plugin.json` with plugin metadata
3. Create `skills/` subdirectory
4. Add skills following skill structure above
5. Register plugin in `.claude-plugin/marketplace.json`

## File Format Guidelines

### EDN Files (metadata.edn)
- Use proper EDN syntax with keywords
- Include all standard metadata fields
- Keep structured and machine-readable

### Markdown Files
- Use clear hierarchical headings
- Include code examples with proper syntax highlighting
- Structure for both human reading and AI consumption

### Examples Files (examples.clj)
- Make executable with shebang: `#!/usr/bin/env bb`
- Include descriptive comments
- Demonstrate practical usage patterns
- Keep examples focused and runnable

## Skill Documentation Guidelines

### SKILL.md Structure
1. Overview and introduction
2. Core concepts
3. API reference organized by category
4. Examples for each function
5. Common patterns and best practices
6. Use cases and recipes
7. Error handling
8. Performance considerations
9. Platform-specific notes

### README.md Structure
1. Brief description
2. Quick start examples
3. Key features overview
4. Using the skill
5. Learning path
6. External resources

## Testing Examples

Before committing changes to example files:
```bash
bb examples.clj
```

Ensure examples run without errors and demonstrate intended functionality.
