# Emacs Widget Library

Build interactive forms and UI elements in Emacs buffers.

## Quick Start

```elisp
(require 'widget)
(eval-when-compile
  (require 'wid-edit))

(defun my-simple-form ()
  (interactive)
  (switch-to-buffer "*My Form*")
  (kill-all-local-variables)
  (erase-buffer)
  (remove-overlays)

  ;; Create widgets
  (widget-insert "Contact Form\n\n")

  (widget-create 'editable-field
                 :format "Name: %v\n"
                 :size 30)

  (widget-insert "\n")

  (widget-create 'editable-field
                 :format "Email: %v\n"
                 :size 40)

  (widget-insert "\n\n")

  (widget-create 'push-button
                 :notify (lambda (&rest ignore)
                           (message "Form submitted!"))
                 "Submit")

  ;; Enable interaction
  (use-local-map widget-keymap)
  (widget-setup))
```

## Key Widget Types

**Text Input:**
- `editable-field` - Single-line text
- `text` - Multi-line text area

**Buttons:**
- `push-button` - Clickable action button
- `link` - Hyperlink with action

**Selection:**
- `checkbox` - Boolean toggle
- `radio-button-choice` - Single choice from options
- `menu-choice` - Dropdown selection
- `checklist` - Multiple selections

**Lists:**
- `editable-list` - Dynamic list with add/remove

## Essential Functions

```elisp
;; Create widget
(widget-create TYPE &rest ARGS)

;; Enable widgets (required after creation)
(widget-setup)

;; Get/set values
(widget-value WIDGET)
(widget-value-set WIDGET VALUE)

;; Navigate
(widget-forward)   ; TAB
(widget-backward)  ; S-TAB

;; Properties
(widget-get WIDGET PROPERTY)
(widget-put WIDGET PROPERTY VALUE)
```

## Common Pattern

```elisp
(defun my-widget-interface ()
  (interactive)
  (let (field1 field2)
    ;; Setup buffer
    (switch-to-buffer "*Interface*")
    (kill-all-local-variables)
    (erase-buffer)
    (remove-overlays)

    ;; Build UI
    (widget-insert "Title\n\n")
    (setq field1 (widget-create 'editable-field "default"))
    (widget-insert "\n")
    (setq field2 (widget-create 'checkbox t))

    ;; Submit button
    (widget-create 'push-button
                   :notify (lambda (&rest _)
                             (message "Values: %S %S"
                                      (widget-value field1)
                                      (widget-value field2)))
                   "Submit")

    ;; Activate
    (use-local-map widget-keymap)
    (widget-setup)))
```

## Using the Skill

Invoke this skill when building:
- Custom configuration interfaces
- Interactive forms
- Settings editors
- Data entry dialogs

The skill provides comprehensive coverage of widget types, API functions, patterns, and real-world examples.

## Learning Path

1. **Basics** - Simple forms with editable-field and push-button
2. **Selection** - checkbox, radio-button-choice, menu-choice
3. **Callbacks** - :notify handlers and value retrieval
4. **Validation** - Input validation and error handling
5. **Advanced** - Custom widgets and complex interfaces

## Resources

- Emacs Info: `C-h i m Widget RET`
- Source: `wid-edit.el` in Emacs distribution
- Built-in demo: `widget-example` (if available)
