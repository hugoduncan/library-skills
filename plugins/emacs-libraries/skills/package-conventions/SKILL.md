---
name: package-conventions
description: Emacs Lisp package development standards and conventions
---

# Emacs Package Conventions

Comprehensive guide to Emacs Lisp package development standards, covering naming, structure, metadata, and distribution requirements.

## Overview

Emacs packages follow strict conventions to ensure compatibility, discoverability, and quality. These conventions cover file structure, naming, metadata, documentation, and distribution through package archives like MELPA and GNU ELPA.

## Package Types

### Simple Package

Single `.el` file with header metadata.

```elisp
;;; mypackage.el --- Brief description  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Your Name

;; Author: Your Name <you@example.com>
;; Version: 1.0.0
;; Package-Requires: ((emacs "25.1"))
;; Keywords: convenience, tools
;; URL: https://github.com/user/mypackage

;;; Commentary:

;; Longer description of what the package does.

;;; Code:

(defun mypackage-hello ()
  "Say hello."
  (interactive)
  (message "Hello from mypackage!"))

(provide 'mypackage)
;;; mypackage.el ends here
```

### Multi-File Package

Directory with `-pkg.el` descriptor file.

Structure:
```
mypackage/
├── mypackage.el
├── mypackage-utils.el
├── mypackage-pkg.el
└── README.md
```

`mypackage-pkg.el`:
```elisp
(define-package "mypackage" "1.0.0"
  "Brief description"
  '((emacs "25.1")
    (dash "2.19.1"))
  :keywords '("convenience" "tools")
  :url "https://github.com/user/mypackage")
```

## File Header Conventions

### Required Headers

**Simple package** (single `.el`):
- `;;; filename.el --- description`
- `;; Author:`
- `;; Version:` or `;; Package-Version:`
- `;;; Commentary:`
- `;;; Code:`
- `(provide 'feature-name)`
- `;;; filename.el ends here`

### Standard Headers

```elisp
;;; mypackage.el --- Brief one-line description  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Author Name

;; Author: Author Name <email@example.com>
;; Maintainer: Maintainer Name <email@example.com>
;; Version: 1.0.0
;; Package-Requires: ((emacs "25.1") (dash "2.19.1"))
;; Keywords: convenience tools
;; URL: https://github.com/user/mypackage
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Detailed description spanning multiple lines.
;; Explain what the package does, how to use it.

;;; Code:
```

### Lexical Binding

Always enable lexical binding:
```elisp
;;; mypackage.el --- Description  -*- lexical-binding: t; -*-
```

Required for modern Emacs development and MELPA acceptance.

## Naming Conventions

### Package Prefix

Choose short, unique prefix. All public symbols must use this prefix.

```elisp
;; Package: super-mode
;; Prefix: super-

(defun super-activate ()          ; ✓ Public function
  ...)

(defvar super-default-value nil)  ; ✓ Public variable

(defun super--internal-helper ()  ; ✓ Private function (double dash)
  ...)

(defvar super--state nil)         ; ✓ Private variable
```

### Symbol Naming Rules

**Public vs Private:**
- Public: `prefix-name`
- Private: `prefix--name` (double dash)

**Variable types:**
- Function variable: `prefix-hook-function`
- Hook: `prefix-mode-hook`
- Option: `prefix-enable-feature`
- Local variable: `prefix--internal-state`

**Special cases:**
- Commands can omit prefix if memorable: `list-frobs` in `frob` package
- Major modes: `prefix-mode`
- Minor modes: `prefix-minor-mode`

### Case Convention

Use lowercase with hyphens (lisp-case):
```elisp
(defun my-package-do-something ()  ; ✓
  ...)

(defun myPackageDoSomething ()     ; ✗ Wrong
  ...)
```

## Package Metadata

### Version Format

Semantic versioning: `MAJOR.MINOR.PATCH`

```elisp
;; Version: 1.2.3
```

For snapshot builds:
```elisp
;; Package-Version: 1.2.3-snapshot
;; Version: 1.2.3
```

### Dependencies

Specify minimum Emacs version and package dependencies:

```elisp
;; Package-Requires: ((emacs "26.1") (dash "2.19.1") (s "1.12.0"))
```

Each dependency: `(package-name "version")`

### Keywords

Use standard keywords from `finder-known-keywords`:

```elisp
;; Keywords: convenience tools matching
```

Common keywords:
- `convenience` - Convenience features
- `tools` - Programming tools
- `extensions` - Emacs extensions
- `languages` - Language support
- `comm` - Communication
- `files` - File handling
- `data` - Data structures

Check available: `M-x describe-variable RET finder-known-keywords`

## Code Organization

### Feature Provision

Always end with `provide`:

```elisp
(provide 'mypackage)
;;; mypackage.el ends here
```

Feature name must match file name (without `.el`).

### Loading Behavior

**Don't modify Emacs on load:**
```elisp
;; ✗ Bad - changes behavior on load
(global-set-key (kbd "C-c m") #'my-command)

;; ✓ Good - user explicitly enables
(defun my-mode-setup ()
  "Set up keybindings for my-mode."
  (local-set-key (kbd "C-c m") #'my-command))
```

### Autoload Cookies

Mark interactive commands for autoloading:

```elisp
;;;###autoload
(defun my-package-start ()
  "Start my-package."
  (interactive)
  ...)

;;;###autoload
(define-minor-mode my-mode
  "Toggle My Mode."
  ...)
```

### Group and Custom Variables

Define customization group:

```elisp
(defgroup my-package nil
  "Settings for my-package."
  :group 'applications
  :prefix "my-package-")

(defcustom my-package-option t
  "Description of option."
  :type 'boolean
  :group 'my-package)
```

## Documentation Standards

### Docstrings

**Functions:**
```elisp
(defun my-package-process (input &optional format)
  "Process INPUT according to FORMAT.

INPUT should be a string or buffer.
FORMAT, if non-nil, specifies output format (symbol).

Return processed result as string."
  ...)
```

First line: brief description ending with period.
Following lines: detailed explanation.
Document arguments in CAPS.
Document return value.

**Variables:**
```elisp
(defvar my-package-cache nil
  "Cache for processed results.
Each entry is (KEY . VALUE) where KEY is input and VALUE is result.")
```

**User options:**
```elisp
(defcustom my-package-auto-save t
  "Non-nil means automatically save results.
When enabled, results are saved to `my-package-save-file'."
  :type 'boolean
  :group 'my-package)
```

### Checkdoc Compliance

Verify documentation:
```elisp
M-x checkdoc RET
```

Requirements:
- First line ends with period
- First line fits in 80 columns
- Argument names in CAPS
- References to symbols quoted with `symbol'
- No spelling errors

## Code Quality

### Required Tools

**package-lint:**
```elisp
M-x package-lint-current-buffer
```

Checks:
- Header format
- Dependency declarations
- Symbol naming
- Autoload cookies

**flycheck-package:**
```elisp
(require 'flycheck-package)
(flycheck-package-setup)
```

Real-time package.el validation.

### Common Issues

**Missing lexical binding:**
```elisp
;;; package.el --- Description  -*- lexical-binding: t; -*-
```

**Wrong provide:**
```elisp
;; File: my-utils.el
(provide 'my-utils)  ; ✓ Matches filename

;; File: my-package.el
(provide 'my-pkg)    ; ✗ Doesn't match filename
```

**Namespace pollution:**
```elisp
;; ✗ Bad
(defun format-string (s)  ; Collides with other packages
  ...)

;; ✓ Good
(defun my-package-format-string (s)
  ...)
```

**Global state on load:**
```elisp
;; ✗ Bad
(setq some-global-var t)  ; Changes Emacs on load

;; ✓ Good
(defcustom my-package-feature-enabled nil
  "Enable my-package feature."
  :type 'boolean
  :set (lambda (sym val)
         (set-default sym val)
         (when val (my-package-activate))))
```

## MELPA Submission

### Repository Requirements

**Source control:**
- Git or Mercurial only
- Official repository (no forks)
- Contains LICENSE file

**Structure:**
```
mypackage/
├── mypackage.el
├── LICENSE
└── README.md
```

### Recipe Format

Create `recipes/mypackage` in MELPA repository:

```elisp
(mypackage :fetcher github
           :repo "user/mypackage"
           :files (:defaults "icons/*.png"))
```

**Fetchers:**
- `:fetcher github` - GitHub repository
- `:fetcher gitlab` - GitLab repository
- `:fetcher codeberg` - Codeberg repository

**Files:**
- `:defaults` - Standard `.el` files
- Custom patterns: `"subdir/*.el"`

### Quality Checklist

Before submission:
- [ ] Lexical binding enabled
- [ ] All public symbols prefixed
- [ ] Private symbols use `--` separator
- [ ] Docstrings complete and accurate
- [ ] `package-lint` passes
- [ ] `checkdoc` clean
- [ ] Dependencies declared in `Package-Requires`
- [ ] Autoloads on interactive commands
- [ ] `provide` matches filename
- [ ] LICENSE file present
- [ ] No loading side effects
- [ ] Keywords from standard list

### Testing Locally

Build recipe locally:
```bash
make recipes/mypackage
```

Install from file:
```elisp
M-x package-install-file RET /path/to/mypackage.el
```

Test in clean Emacs:
```bash
emacs -Q -l package -f package-initialize -f package-install-file mypackage.el
```

## Major Mode Conventions

### Mode Definition

```elisp
(defvar my-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'my-mode-command)
    map)
  "Keymap for `my-mode'.")

(define-derived-mode my-mode fundamental-mode "My"
  "Major mode for editing My files.

\\{my-mode-map}"
  (setq-local comment-start "#")
  (setq-local comment-end ""))
```

### Mode Hooks

Provide hook for customization:

```elisp
(defvar my-mode-hook nil
  "Hook run when entering `my-mode'.")

(define-derived-mode my-mode fundamental-mode "My"
  ...
  (run-hooks 'my-mode-hook))
```

### Auto-mode Association

Use autoload for file associations:

```elisp
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.my\\'" . my-mode))
```

## Minor Mode Conventions

### Global Minor Mode

```elisp
;;;###autoload
(define-minor-mode my-minor-mode
  "Toggle My Minor Mode.

When enabled, provides X functionality."
  :global t
  :lighter " My"
  :group 'my-package
  (if my-minor-mode
      (my-minor-mode--enable)
    (my-minor-mode--disable)))
```

### Buffer-local Minor Mode

```elisp
;;;###autoload
(define-minor-mode my-local-mode
  "Toggle My Local Mode in current buffer."
  :lighter " MyL"
  :keymap my-local-mode-map
  (if my-local-mode
      (add-hook 'post-command-hook #'my-local-mode--update nil t)
    (remove-hook 'post-command-hook #'my-local-mode--update t)))
```

## Library vs Package

### Library

Collection of functions for other packages to use:

```elisp
;;; mylib.el --- Utility functions  -*- lexical-binding: t; -*-

;; Author: Name
;; Version: 1.0.0

;;; Commentary:

;; Library of utility functions.  Not a standalone package.

;;; Code:

(defun mylib-helper (x)
  "Help with X."
  ...)

(provide 'mylib)
;;; mylib.el ends here
```

### Package

End-user feature with commands:

```elisp
;;; mypackage.el --- User feature  -*- lexical-binding: t; -*-

;; Package-Requires: ((emacs "25.1"))
;; Keywords: convenience

;;; Code:

;;;###autoload
(defun mypackage-start ()
  "Start mypackage."
  (interactive)
  ...)

(provide 'mypackage)
;;; mypackage.el ends here
```

## Common Patterns

### Configuration Option

```elisp
(defcustom my-package-backend 'default
  "Backend to use for processing.
Possible values:
  `default' - Use built-in backend
  `external' - Use external program
  `auto' - Detect automatically"
  :type '(choice (const :tag "Default" default)
                 (const :tag "External" external)
                 (const :tag "Auto-detect" auto))
  :group 'my-package)
```

### Hook Variable

```elisp
(defvar my-package-before-save-hook nil
  "Hook run before saving with my-package.
Functions receive no arguments.")

(defun my-package-save ()
  "Save current state."
  (run-hooks 'my-package-before-save-hook)
  ...)
```

### Function Variable

```elisp
(defcustom my-package-format-function #'my-package-default-format
  "Function to format output.
Called with one argument (the data to format).
Should return formatted string."
  :type 'function
  :group 'my-package)
```

### Feature Check

```elisp
(when (featurep 'some-package)
  ;; Integration with some-package
  ...)
```

## Error Handling

### User Errors

```elisp
(defun my-package-process (input)
  "Process INPUT."
  (unless input
    (user-error "No input provided"))
  (unless (stringp input)
    (user-error "Input must be string, got %s" (type-of input)))
  ...)
```

### Regular Errors

```elisp
(defun my-package--internal ()
  "Internal function."
  (unless (my-package--valid-state-p)
    (error "Invalid state: %s" my-package--state))
  ...)
```

### Condition Handling

```elisp
(defun my-package-try-operation ()
  "Attempt operation, return nil on failure."
  (condition-case err
      (my-package--do-operation)
    (file-error
     (message "File error: %s" (error-message-string err))
     nil)
    (error
     (message "Operation failed: %s" (error-message-string err))
     nil)))
```

## Performance Considerations

### Deferred Loading

Use autoload to defer loading:

```elisp
;;;###autoload
(defun my-package-start ()
  "Start my-package."
  (interactive)
  (require 'my-package-core)
  (my-package-core-start))
```

### Compilation

Byte-compile packages for performance:
```bash
emacs -batch -f batch-byte-compile mypackage.el
```

Check warnings:
```elisp
M-x byte-compile-file RET mypackage.el
```

### Lazy Evaluation

```elisp
(defvar my-package--cache nil
  "Cached data.")

(defun my-package-get-data ()
  "Get data, using cache if available."
  (or my-package--cache
      (setq my-package--cache (my-package--compute-data))))
```

## Testing Packages

### ERT Tests

```elisp
;;; mypackage-tests.el --- Tests  -*- lexical-binding: t; -*-

(require 'ert)
(require 'mypackage)

(ert-deftest mypackage-test-basic ()
  (should (equal (mypackage-process "input") "expected")))

(ert-deftest mypackage-test-error ()
  (should-error (mypackage-process nil) :type 'user-error))
```

Run tests:
```elisp
M-x ert RET t RET
```

### Buttercup Tests

```elisp
(require 'buttercup)
(require 'mypackage)

(describe "mypackage-process"
  (it "handles valid input"
    (expect (mypackage-process "input") :to-equal "expected"))

  (it "signals error for nil"
    (expect (mypackage-process nil) :to-throw 'user-error)))
```

## Distribution

### GNU ELPA

Requirements:
- GPL-compatible license
- Copyright assignment to FSF (for core packages)
- High quality standards

Submit to `emacs-devel@gnu.org`

### MELPA

Requirements:
- Git/Mercurial repository
- Standard package format
- Quality checks pass

Submit PR to https://github.com/melpa/melpa

### MELPA Stable

Requires Git tags for versions:
```bash
git tag -a v1.0.0 -m "Release 1.0.0"
git push origin v1.0.0
```

Recipe includes `:branch`:
```elisp
(mypackage :fetcher github
           :repo "user/mypackage"
           :branch "stable")
```

## License Conventions

### GPL Boilerplate

Include above Commentary section:

```elisp
;;; mypackage.el --- Description  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Author Name
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:
```

### LICENSE File

Include full license text in repository root.

Common choices:
- GPL-3.0-or-later
- GPL-2.0-or-later
- MIT (for non-GNU repositories)

## Multi-File Packages

### Package Descriptor

`mypackage-pkg.el`:
```elisp
(define-package "mypackage" "1.0.0"
  "Brief description of package"
  '((emacs "26.1")
    (dash "2.19.1"))
  :keywords '("convenience" "tools")
  :authors '(("Author Name" . "email@example.com"))
  :maintainer '("Maintainer" . "email@example.com")
  :url "https://github.com/user/mypackage")
```

### File Organization

```
mypackage/
├── mypackage.el           ; Main entry point with autoloads
├── mypackage-core.el      ; Core functionality
├── mypackage-ui.el        ; UI components
├── mypackage-utils.el     ; Utilities
├── mypackage-pkg.el       ; Package descriptor
└── mypackage-tests.el     ; Tests (not packaged)
```

### Feature Naming

Each file provides named feature:

```elisp
;; mypackage.el
(provide 'mypackage)

;; mypackage-core.el
(provide 'mypackage-core)

;; mypackage-ui.el
(provide 'mypackage-ui)
```

Main file requires subfeatures:

```elisp
;;; mypackage.el --- Main file

(require 'mypackage-core)
(require 'mypackage-ui)

(provide 'mypackage)
```

## Migration and Compatibility

### Obsolete Functions

```elisp
(defun mypackage-new-name ()
  "New function name."
  ...)

(define-obsolete-function-alias
  'mypackage-old-name
  'mypackage-new-name
  "1.5.0"
  "Use `mypackage-new-name' instead.")
```

### Obsolete Variables

```elisp
(defvar mypackage-new-option t
  "New option.")

(define-obsolete-variable-alias
  'mypackage-old-option
  'mypackage-new-option
  "1.5.0"
  "Use `mypackage-new-option' instead.")
```

### Version Checking

```elisp
(when (version< emacs-version "26.1")
  (error "Mypackage requires Emacs 26.1 or later"))

;; Feature-based check preferred
(unless (fboundp 'some-function)
  (error "Mypackage requires some-function"))
```

## Summary

Emacs package conventions ensure quality, compatibility, and discoverability:

1. Use standard file header format with required metadata
2. Enable lexical binding in all files
3. Prefix all public symbols with package name
4. Use double-dash for private symbols
5. Write comprehensive docstrings
6. Mark interactive commands with autoload cookies
7. Provide customization group and options
8. Don't modify Emacs behavior on load
9. Include LICENSE file
10. Test with package-lint and checkdoc
11. Follow MELPA guidelines for distribution
12. Use semantic versioning
13. Declare all dependencies
14. End files with provide statement

Following these conventions enables smooth integration with package.el, acceptance into package archives, and positive user experience.
