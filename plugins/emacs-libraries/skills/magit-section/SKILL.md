---
name: magit-section
description: A guide to using magit-section for building collapsible, hierarchical buffer UIs in Emacs.
---

# magit-section: Collapsible Section-Based UIs

magit-section is Emacs's premier library for creating interactive, hierarchical buffer interfaces with collapsible sections. Originally extracted from Magit, it provides the foundation for building information-dense UIs that users can navigate and explore efficiently.

## Overview

magit-section enables creation of buffers with tree-like, collapsible content organized into sections. Each section can contain nested child sections, custom keybindings, associated data, and responsive highlighting.

**Key Characteristics:**
- Hierarchical collapsible sections with visibility control
- Section-specific keymaps and actions
- Built-in navigation commands
- Visibility caching across buffer refreshes
- Mouse and keyboard interaction
- Integrated with Emacs region selection
- Requires Emacs 28.1+

**Version:** 4.2.0+ (January 2025)
**Repository:** https://github.com/magit/magit
**License:** GPL-3.0+

## Core Concepts

### Section Object

Sections are EIEIO objects with these slots:

- `type` - Symbol identifying section kind (e.g., `file`, `commit`, `hunk`)
- `value` - Associated data (filename, commit SHA, etc.)
- `start` - Buffer position where section begins (includes heading)
- `content` - Buffer position where body content starts
- `end` - Buffer position where section ends
- `hidden` - Visibility state (nil=visible, non-nil=hidden)
- `children` - List of child sections
- `parent` - Parent section reference
- `keymap` - Section-specific key bindings
- `washer` - Function for deferred content generation

### Buffer Structure

Every magit-section buffer requires a single root section that spans the entire buffer. Sections form a tree hierarchy with proper nesting.

### Visibility States

Sections can be:
- **Fully visible** - Heading and all content shown
- **Hidden** - Only heading visible, content collapsed
- **Heading-only** - Nested sections show only headings

## API Reference

### Creating Sections

#### magit-insert-section

Primary macro for section creation:

```elisp
(magit-insert-section (type value &optional hide)
  [HEADING-FORM]
  BODY...)
```

**Arguments:**
- `type` - Section type (symbol or `(eval FORM)`)
- `value` - Data to store in section's value slot
- `hide` - Initial visibility (nil=visible, t=hidden)

**Example:**

```elisp
(magit-insert-section (file "README.md")
  (magit-insert-heading "README.md")
  (insert "File contents here\n"))
```

**Advanced Usage:**

```elisp
(magit-insert-section (commit commit-sha nil)
  (magit-insert-heading
    (format "%s %s"
            (substring commit-sha 0 7)
            (magit-commit-message commit-sha)))
  ;; Insert commit details
  (magit-insert-section (diffstat commit-sha)
    (insert (magit-format-diffstat commit-sha))))
```

#### magit-insert-heading

Insert section heading with optional child count:

```elisp
(magit-insert-heading &rest ARGS)
```

**Example:**

```elisp
(magit-insert-heading
  (format "Changes (%d)" (length file-list)))
```

#### magit-insert-section-body

Defer section body evaluation until first expansion:

```elisp
(magit-insert-section-body
  ;; Expensive operations here
  (insert (expensive-computation)))
```

Use for performance when sections are initially hidden.

#### magit-cancel-section

Abort partial section creation:

```elisp
(when (null items)
  (magit-cancel-section))
```

Removes partial section from buffer and section tree.

### Navigation Commands

#### Movement

```elisp
(magit-section-forward)           ; Next sibling or parent's next
(magit-section-backward)          ; Previous sibling or parent
(magit-section-up)                ; Parent section
(magit-section-forward-sibling)   ; Next sibling
(magit-section-backward-sibling)  ; Previous sibling
```

**Keybindings:**
- `n` - Forward
- `p` - Backward
- `^` - Up to parent
- `M-n` - Forward sibling
- `M-p` - Backward sibling

**Example Navigation Hook:**

```elisp
(add-hook 'magit-section-movement-hook
          (lambda ()
            (when (eq (oref (magit-current-section) type) 'commit)
              (message "On commit: %s"
                       (oref (magit-current-section) value)))))
```

### Visibility Control

#### Basic Toggle

```elisp
(magit-section-toggle &optional SECTION)  ; Toggle current/specified
(magit-section-show SECTION)              ; Expand
(magit-section-hide SECTION)              ; Collapse
```

**Keybindings:**
- `TAB` - Toggle current section
- `C-c TAB` / `C-<tab>` - Cycle visibility states
- `<backtab>` - Cycle all top-level sections

#### Recursive Operations

```elisp
(magit-section-show-children SECTION &optional DEPTH)
(magit-section-hide-children SECTION)
(magit-section-show-headings SECTION)
```

**Example:**

```elisp
;; Expand section and first level of children
(magit-section-show-children (magit-current-section) 1)
```

#### Level-Based Visibility

```elisp
(magit-section-show-level-1)      ; Show only top level
(magit-section-show-level-2)      ; Show two levels
(magit-section-show-level-3)      ; Show three levels
(magit-section-show-level-4)      ; Show four levels
```

**Keybindings:**
- `1` / `2` / `3` / `4` - Show levels around current section
- `M-1` / `M-2` / `M-3` / `M-4` - Show levels for entire buffer

#### Cycling

```elisp
(magit-section-cycle &optional SECTION)        ; Current section
(magit-section-cycle-global)                   ; All sections
```

Cycles through: collapsed → expanded → children-collapsed → collapsed

### Querying Sections

#### Current Section

```elisp
(magit-current-section)           ; Section at point
(magit-section-at &optional POS)  ; Section at position
```

**Example:**

```elisp
(let ((section (magit-current-section)))
  (message "Type: %s, Value: %s"
           (oref section type)
           (oref section value)))
```

#### Section Properties

```elisp
(magit-section-ident SECTION)        ; Unique identifier
(magit-section-lineage SECTION)      ; List of ancestor sections
(magit-section-hidden SECTION)       ; Hidden or ancestor hidden?
(magit-section-content-p SECTION)    ; Has body content?
```

**Example:**

```elisp
(when (magit-section-content-p section)
  (magit-section-show section))
```

#### Section Matching

```elisp
(magit-section-match CONDITIONS &optional SECTION)
```

**Conditions:**
- Symbol matches type
- List `(TYPE VALUE)` matches type and value
- List of symbols matches any type in list
- `[TYPE...]` matches type hierarchy

**Example:**

```elisp
;; Check if section is a file
(when (magit-section-match 'file)
  (message "Current section is a file"))

;; Match specific file
(when (magit-section-match '(file "README.md"))
  (message "On README.md"))

;; Match hierarchy: hunk within file
(when (magit-section-match [file hunk])
  (message "In a hunk within a file section"))
```

#### Region Selection

```elisp
(magit-region-sections &optional CONDITION MULTIPLE)
```

Get sibling sections in active region.

**Example:**

```elisp
(let ((selected-files (magit-region-sections 'file t)))
  (dolist (section selected-files)
    (message "Selected: %s" (oref section value))))
```

### Section Inspection

```elisp
(magit-describe-section &optional SECTION INTERACTIVE)
(magit-describe-section-briefly &optional SECTION INTERACTIVE)
```

Display detailed section information for debugging.

**Keybinding:** `C-h .` (in magit-section buffers)

### Section-Specific Keymaps

Define per-type keybindings using the `:keymap` class slot:

```elisp
(defclass my-file-section (magit-section)
  ((keymap :initform 'my-file-section-map)))

(defvar my-file-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'my-open-file)
    (define-key map (kbd "d") 'my-delete-file)
    map))
```

**Example:**

```elisp
(magit-insert-section (my-file-section filename)
  (magit-insert-heading filename)
  (insert "Content..."))
;; RET and d keys work only on this section type
```

## Configuration

### Highlighting

```elisp
;; Highlight current section
(setq magit-section-highlight-current t)

;; Highlight region selection
(setq magit-section-highlight-selection t)
```

### Child Count Display

```elisp
;; Show number of children in headings
(setq magit-section-show-child-count t)
```

### Visibility Indicators

```elisp
;; Customize expansion/collapse indicators
(setq magit-section-visibility-indicators
      '((expanded . "▼")
        (collapsed . "▶")))
```

### Visibility Caching

```elisp
;; Cache visibility across refreshes
(setq magit-section-cache-visibility t)

;; Cache only specific section types
(setq magit-section-cache-visibility '(file commit))

;; Set initial visibility by type
(setq magit-section-initial-visibility-alist
      '((stash . hide)
        (untracked . hide)))
```

### Line Numbers

```elisp
;; Disable line numbers in section buffers (default)
(setq magit-section-disable-line-numbers t)
```

## Common Patterns

### Basic Section Buffer

```elisp
(defun my-section-buffer ()
  "Create a buffer with collapsible sections."
  (interactive)
  (let ((buf (get-buffer-create "*My Sections*")))
    (with-current-buffer buf
      (magit-section-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (magit-insert-section (root)
          (magit-insert-heading "My Data")
          (magit-insert-section (files)
            (magit-insert-heading "Files")
            (dolist (file (directory-files default-directory))
              (magit-insert-section (file file)
                (insert file "\n"))))
          (magit-insert-section (buffers)
            (magit-insert-heading "Buffers")
            (dolist (buf (buffer-list))
              (magit-insert-section (buffer buf)
                (insert (buffer-name buf) "\n")))))))
    (pop-to-buffer buf)))
```

### Refreshable Buffer

```elisp
(defvar-local my-refresh-function nil)

(defun my-section-refresh ()
  "Refresh current section buffer."
  (interactive)
  (when my-refresh-function
    (let ((inhibit-read-only t)
          (line (line-number-at-pos))
          (col (current-column)))
      (erase-buffer)
      (funcall my-refresh-function)
      (goto-char (point-min))
      (forward-line (1- line))
      (move-to-column col))))

(defun my-create-buffer ()
  (let ((buf (get-buffer-create "*My Data*")))
    (with-current-buffer buf
      (magit-section-mode)
      (setq my-refresh-function #'my-insert-content)
      (local-set-key (kbd "g") #'my-section-refresh)
      (my-section-refresh))
    buf))
```

### Washing External Command Output

```elisp
(defun my-insert-git-status ()
  "Insert git status output as sections."
  (magit-insert-section (status)
    (magit-insert-heading "Git Status")
    (let ((start (point)))
      (call-process "git" nil t nil "status" "--short")
      (save-restriction
        (narrow-to-region start (point))
        (goto-char start)
        (while (not (eobp))
          (let ((line-start (point))
                (file (buffer-substring (+ (point) 3)
                                        (line-end-position))))
            (magit-insert-section (file file)
              (forward-line 1))))))))
```

### Section Actions

```elisp
(defclass file-section (magit-section)
  ((keymap :initform 'file-section-map)))

(defvar file-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'my-visit-file)
    (define-key map (kbd "d") 'my-delete-file)
    (define-key map (kbd "r") 'my-rename-file)
    map))

(defun my-visit-file ()
  "Visit file in current section."
  (interactive)
  (when-let ((section (magit-current-section)))
    (when (eq (oref section type) 'file-section)
      (find-file (oref section value)))))

(defun my-delete-file ()
  "Delete file in current section."
  (interactive)
  (when-let ((section (magit-current-section)))
    (when (and (eq (oref section type) 'file-section)
               (yes-or-no-p (format "Delete %s? "
                                    (oref section value))))
      (delete-file (oref section value))
      (my-section-refresh))))
```

### Deferred Content Loading

```elisp
(magit-insert-section (expensive-data)
  (magit-insert-heading "Large Dataset")
  (magit-insert-section-body
    ;; Only computed when section first expanded
    (insert (format-large-dataset (compute-expensive-data)))))
```

### Visibility Hooks

```elisp
(add-hook 'magit-section-set-visibility-hook
          (lambda (section)
            ;; Keep commit details hidden by default
            (when (eq (oref section type) 'commit-details)
              'hide)))
```

### Context Menu Integration

```elisp
(defun my-section-context-menu (menu section)
  "Add items to context menu based on section."
  (when (eq (oref section type) 'file)
    (define-key menu [my-open]
      '("Open File" . my-visit-file))
    (define-key menu [my-delete]
      '("Delete File" . my-delete-file)))
  menu)

(add-hook 'magit-menu-alternative-section-hook
          #'my-section-context-menu)
```

## Use Cases

### File Browser

```elisp
(defun my-file-browser (dir)
  "Browse directory with collapsible sections."
  (interactive "DDirectory: ")
  (let ((buf (get-buffer-create (format "*Files: %s*" dir))))
    (with-current-buffer buf
      (magit-section-mode)
      (let ((inhibit-read-only t)
            (default-directory dir))
        (erase-buffer)
        (magit-insert-section (root)
          (magit-insert-heading (format "Directory: %s" dir))
          (my-insert-directory-tree "."))))
    (pop-to-buffer buf)))

(defun my-insert-directory-tree (path)
  "Recursively insert directory structure."
  (dolist (file (directory-files path))
    (unless (member file '("." ".."))
      (let ((full-path (expand-file-name file path)))
        (if (file-directory-p full-path)
            (magit-insert-section (directory full-path)
              (magit-insert-heading (concat file "/"))
              (my-insert-directory-tree full-path))
          (magit-insert-section (file full-path)
            (insert file "\n")))))))
```

### Log Viewer

```elisp
(defun my-log-viewer (log-file)
  "View log file with collapsible sections per entry."
  (interactive "fLog file: ")
  (let ((buf (get-buffer-create "*Log Viewer*")))
    (with-current-buffer buf
      (magit-section-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (magit-insert-section (root)
          (magit-insert-heading (format "Log: %s" log-file))
          (with-temp-buffer
            (insert-file-contents log-file)
            (goto-char (point-min))
            (my-parse-log-entries)))))
    (pop-to-buffer buf)))
```

### Process Monitor

```elisp
(defun my-process-monitor ()
  "Display running processes in sections."
  (interactive)
  (let ((buf (get-buffer-create "*Processes*")))
    (with-current-buffer buf
      (magit-section-mode)
      (setq my-refresh-function #'my-insert-processes)
      (local-set-key (kbd "g") #'my-section-refresh)
      (local-set-key (kbd "k") #'my-kill-process)
      (my-section-refresh))
    (pop-to-buffer buf)))

(defun my-insert-processes ()
  "Insert process list as sections."
  (magit-insert-section (root)
    (magit-insert-heading "Running Processes")
    (dolist (proc (process-list))
      (magit-insert-section (process proc)
        (magit-insert-heading
          (format "%s [%s]"
                  (process-name proc)
                  (process-status proc)))
        (insert (format "  Command: %s\n"
                        (mapconcat #'identity
                                   (process-command proc)
                                   " ")))))))
```

### Configuration Inspector

```elisp
(defun my-config-inspector ()
  "Inspect Emacs configuration in sections."
  (interactive)
  (let ((buf (get-buffer-create "*Config*")))
    (with-current-buffer buf
      (magit-section-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (magit-insert-section (root)
          (magit-insert-heading "Emacs Configuration")

          (magit-insert-section (variables)
            (magit-insert-heading "Custom Variables")
            (dolist (var (sort (my-get-custom-vars) #'string<))
              (magit-insert-section (variable var)
                (magit-insert-heading (symbol-name var))
                (insert (format "  %S\n" (symbol-value var))))))

          (magit-insert-section (features)
            (magit-insert-heading "Loaded Features")
            (dolist (feature features)
              (magit-insert-section (feature feature)
                (insert (format "%s\n" feature))))))))
    (pop-to-buffer buf)))
```

## Error Handling

### Validation

```elisp
;; Ensure root section exists
(defun my-ensure-root-section ()
  (unless magit-root-section
    (error "Buffer does not have a root section. Enable magit-section-mode.")))

;; Validate section type
(defun my-require-file-section ()
  (let ((section (magit-current-section)))
    (unless (eq (oref section type) 'file)
      (user-error "Not on a file section"))))
```

### Graceful Degradation

```elisp
(defun my-safe-section-value ()
  "Get section value safely."
  (when-let ((section (magit-current-section)))
    (ignore-errors
      (oref section value))))
```

## Performance Considerations

### Lazy Loading

Use `magit-insert-section-body` for expensive operations:

```elisp
(magit-insert-section (large-data)
  (magit-insert-heading "Large Dataset")
  (magit-insert-section-body
    ;; Only executed when expanded
    (my-compute-and-insert-large-data)))
```

### Visibility Caching

Cache section visibility to preserve state across refreshes:

```elisp
(setq magit-section-cache-visibility t)

;; Preserve visibility during refresh
(let ((magit-section-preserve-visibility t))
  (my-refresh-buffer))
```

### Efficient Updates

Minimize full buffer refreshes:

```elisp
;; Update single section instead of full refresh
(defun my-update-section (section)
  (save-excursion
    (goto-char (oref section start))
    (let ((inhibit-read-only t)
          (hidden (oref section hidden)))
      (delete-region (oref section start) (oref section end))
      (my-insert-section-content section)
      (when hidden
        (magit-section-hide section)))))
```

## Integration with Emacs

### magit-section-mode

Enable in buffers using sections:

```elisp
(define-derived-mode my-viewer-mode magit-section-mode "MyViewer"
  "Major mode for viewing data with sections."
  (setq-local my-refresh-function #'my-insert-data))
```

### Faces

Customize appearance:

```elisp
(custom-set-faces
 '(magit-section-heading ((t (:weight bold :foreground "blue"))))
 '(magit-section-highlight ((t (:background "gray95")))))
```

### Mouse Support

Mouse clicks on section margins toggle visibility automatically.

## Debugging

### Inspection Commands

```elisp
;; Describe current section
(call-interactively #'magit-describe-section)

;; Brief description
(magit-describe-section-briefly)
```

### Debug Helpers

```elisp
(defun my-debug-section ()
  "Debug current section structure."
  (interactive)
  (let ((section (magit-current-section)))
    (message "Type: %S, Value: %S, Hidden: %S, Children: %d"
             (oref section type)
             (oref section value)
             (oref section hidden)
             (length (oref section children)))))
```

## Migration from Other UIs

### From outline-mode

magit-section offers superior interactivity and data association:

```elisp
;; outline-mode
(outline-hide-subtree)

;; magit-section equivalent
(magit-section-hide (magit-current-section))
```

### From org-mode

Use magit-section for custom data not suited to org's document structure:

```elisp
;; org-mode cycling
(org-cycle)

;; magit-section cycling
(magit-section-cycle)
```

## Resources

- **Repository:** https://github.com/magit/magit
- **Tutorial:** https://github.com/magit/magit/wiki/Magit-Section-Tutorial
- **Magit Documentation:** https://magit.vc/manual/magit.html
- **ELPA:** Available on NonGNU ELPA

## See Also

- **Magit:** Git interface built on magit-section
- **taxy-magit-section:** Integrate taxy hierarchies with magit-section
- **outline-mode:** Built-in outline collapsing
- **org-mode:** Document structure with folding
