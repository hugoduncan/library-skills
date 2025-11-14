# magit-section

Library for creating collapsible, hierarchical buffer UIs with interactive sections - the foundation of Magit's interface.

## Quick Start

### Basic Section Buffer

```elisp
(require 'magit-section)

(defun my-simple-browser ()
  "Create a buffer with collapsible file sections."
  (interactive)
  (let ((buf (get-buffer-create "*File Browser*")))
    (with-current-buffer buf
      (magit-section-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (magit-insert-section (root)
          (magit-insert-heading "Files")
          (dolist (file (directory-files default-directory))
            (magit-insert-section (file file)
              (magit-insert-heading file)
              (insert (format "  Size: %d bytes\n"
                              (file-attribute-size
                               (file-attributes file)))))))))
    (pop-to-buffer buf)))
```

### Navigation

**Keybindings:**
- `n` / `p` - Next/previous section
- `^` - Parent section
- `TAB` - Toggle current section
- `M-n` / `M-p` - Next/previous sibling
- `1-4` - Show levels around current section

### Section Creation

```elisp
;; Basic section
(magit-insert-section (type-name value)
  (magit-insert-heading "Section Title")
  (insert "Content here\n"))

;; Nested sections
(magit-insert-section (parent "parent-value")
  (magit-insert-heading "Parent")
  (magit-insert-section (child "child-value")
    (magit-insert-heading "Child")
    (insert "Nested content\n")))

;; Initially hidden section
(magit-insert-section (data value t)  ; t = hidden
  (magit-insert-heading "Hidden by Default")
  (insert "This content is initially collapsed\n"))
```

## Key Features

- **Hierarchical structure:** Nest sections to any depth
- **Interactive navigation:** Built-in keybindings for movement
- **Visibility control:** Collapse/expand sections individually or globally
- **Section-specific keymaps:** Custom actions per section type
- **Data association:** Store arbitrary values in sections
- **Visibility caching:** Preserve collapse state across refreshes
- **Performance:** Deferred content loading for large datasets
- **Mouse support:** Click margins to toggle sections

## Common Patterns

### Refreshable Buffer

```elisp
(defvar-local my-refresh-fn nil)

(defun my-refresh ()
  "Refresh current buffer."
  (interactive)
  (when my-refresh-fn
    (let ((inhibit-read-only t))
      (erase-buffer)
      (funcall my-refresh-fn))))

(defun my-create-buffer ()
  (let ((buf (get-buffer-create "*My Data*")))
    (with-current-buffer buf
      (magit-section-mode)
      (setq my-refresh-fn #'my-insert-data)
      (local-set-key (kbd "g") #'my-refresh)
      (my-refresh))
    buf))
```

### Section-Specific Actions

```elisp
(defclass file-section (magit-section)
  ((keymap :initform 'file-section-map)))

(defvar file-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'my-open-file)
    (define-key map (kbd "d") #'my-delete-file)
    map))

(defun my-open-file ()
  "Open file in current section."
  (interactive)
  (when-let ((section (magit-current-section)))
    (when (eq (oref section type) 'file-section)
      (find-file (oref section value)))))

;; Use the custom section type
(magit-insert-section (file-section "/path/to/file")
  (magit-insert-heading "README.md")
  (insert "Content\n"))
```

### Querying Sections

```elisp
;; Get current section
(let ((section (magit-current-section)))
  (message "Type: %s, Value: %s"
           (oref section type)
           (oref section value)))

;; Check section type
(when (magit-section-match 'file)
  (message "On a file section"))

;; Match hierarchy (hunk within file)
(when (magit-section-match [file hunk])
  (message "On a hunk in a file"))

;; Get selected sections in region
(let ((files (magit-region-sections 'file t)))
  (message "Selected %d files" (length files)))
```

### Deferred Content Loading

```elisp
;; Only compute when section is first expanded
(magit-insert-section (expensive-data)
  (magit-insert-heading "Large Dataset")
  (magit-insert-section-body
    (insert (compute-expensive-data))))
```

## Configuration

```elisp
;; Highlight current section
(setq magit-section-highlight-current t)

;; Show child count in headings
(setq magit-section-show-child-count t)

;; Cache visibility across refreshes
(setq magit-section-cache-visibility t)

;; Custom visibility indicators
(setq magit-section-visibility-indicators
      '((expanded . "▼")
        (collapsed . "▶")))

;; Initial visibility by type
(setq magit-section-initial-visibility-alist
      '((details . hide)
        (metadata . hide)))
```

## Use Cases

- **File browsers** with hierarchical directory trees
- **Log viewers** with collapsible entries
- **Process monitors** with expandable details
- **Configuration inspectors** organizing settings
- **Documentation browsers** with section navigation
- **Data explorers** for structured content
- **Custom Git UIs** (like Magit itself)
- **API response viewers** with nested data

## Using the Skill

This skill provides:
- Complete API reference for all magit-section functions
- Comprehensive usage patterns and best practices
- Detailed examples for common scenarios
- Performance optimization techniques
- Integration patterns with Emacs modes

Consult `SKILL.md` for in-depth documentation.

## Learning Path

1. **Basics:** Create simple sections, basic navigation
2. **Structure:** Nested hierarchies, headings, content
3. **Interaction:** Section keymaps, actions, queries
4. **Visibility:** Toggle, cycle, caching strategies
5. **Advanced:** Custom section types, washing buffers, performance
6. **Integration:** Major modes, refreshing, context menus

## External Resources

- [Magit Repository](https://github.com/magit/magit)
- [Magit-Section Tutorial](https://github.com/magit/magit/wiki/Magit-Section-Tutorial)
- [Magit Documentation](https://magit.vc/manual/magit.html)
- Package: Available on NonGNU ELPA

## Requirements

- Emacs 28.1+
- Dependencies: compat 30.1+, cond-let 0.1+, llama 1.0+, seq 2.24+

## Installation

```elisp
;; Via package.el
(package-install 'magit-section)

;; In your init file
(require 'magit-section)
```
