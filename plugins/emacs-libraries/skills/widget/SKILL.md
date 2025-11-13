---
name: widget
description: A guide to using the Emacs Widget Library for creating interactive UI elements in buffers.
---

# Emacs Widget Library

Build interactive forms and UI elements in Emacs buffers using the built-in widget library (wid-edit.el).

## Overview

The Emacs Widget Library provides a framework for creating interactive UI elements within text buffers. Widgets enable building forms, configuration interfaces, and custom UI components without requiring external dependencies.

**Core libraries:**
- `widget.el` - Top-level widget interface
- `wid-edit.el` - Widget implementation and editing

**Primary use cases:**
- Custom configuration interfaces
- Interactive forms and dialogs
- Settings and preference editors
- Data entry and validation

## Setup

```elisp
(require 'widget)
(eval-when-compile
  (require 'wid-edit))
```

## Core Concepts

### Widget Lifecycle

1. **Create** - `widget-create` returns a widget object
2. **Setup** - `widget-setup` enables interaction after creation
3. **Interact** - User edits/activates widgets
4. **Query** - Retrieve values with `widget-value`
5. **Delete** - Clean up with `widget-delete`

### Widget Buffer Setup

```elisp
(defun my-widget-example ()
  (interactive)
  (switch-to-buffer "*Widget Example*")
  (kill-all-local-variables)
  (erase-buffer)
  (remove-overlays)

  ;; Create widgets
  (widget-insert "Example Form\n\n")
  (widget-create 'editable-field
                 :format "Name: %v"
                 :size 30
                 "Enter name")
  (widget-insert "\n\n")
  (widget-create 'push-button
                 :notify (lambda (&rest ignore)
                           (message "Submitted!"))
                 "Submit")
  (widget-insert "\n")

  ;; Enable widgets
  (use-local-map widget-keymap)
  (widget-setup))
```

### Widget Properties

Widgets are configured via keyword arguments:
- `:value` - Initial/current value
- `:format` - Display format string
- `:tag` - Label text
- `:notify` - Callback function on change
- `:help-echo` - Tooltip text

## Widget Types

### Text Input

#### editable-field

Basic text input field.

```elisp
;; Simple field
(widget-create 'editable-field
               :format "Label: %v\n"
               "default text")

;; Sized field
(widget-create 'editable-field
               :size 20
               :format "Username: %v\n")

;; Password field
(widget-create 'editable-field
               :secret ?*
               :format "Password: %v\n")

;; With change notification
(widget-create 'editable-field
               :notify (lambda (widget &rest ignore)
                         (message "Value: %s" (widget-value widget)))
               :format "Email: %v\n")
```

#### text

Multi-line text area.

```elisp
(widget-create 'text
               :format "Comments:\n%v"
               :value "Line 1\nLine 2\nLine 3")
```

### Buttons

#### push-button

Clickable button that triggers action.

```elisp
;; Basic button
(widget-create 'push-button
               :notify (lambda (&rest ignore)
                         (message "Clicked!"))
               "Click Me")

;; Styled button
(widget-create 'push-button
               :button-face 'custom-button
               :format "%[%t%]\n"
               :tag "Submit Form"
               :notify (lambda (&rest ignore)
                         (my-submit-form)))
```

#### link

Hyperlink that executes action.

```elisp
;; Function link
(widget-create 'link
               :button-face 'info-xref
               :help-echo "View documentation"
               :notify (lambda (&rest ignore)
                         (describe-function 'widget-create))
               "Documentation")

;; URL link
(widget-create 'url-link
               :format "%[%t%]"
               :tag "Emacs Manual"
               "https://www.gnu.org/software/emacs/manual/")
```

### Selection

#### checkbox

Boolean toggle (checked/unchecked).

```elisp
(widget-create 'checkbox
               :format "%[%v%] Enable feature\n"
               :notify (lambda (widget &rest ignore)
                         (message "Feature %s"
                                  (if (widget-value widget)
                                      "enabled"
                                    "disabled")))
               t)  ; Initial state
```

#### toggle

Text-based on/off switch.

```elisp
(widget-create 'toggle
               :on "Enabled"
               :off "Disabled"
               :notify (lambda (widget &rest ignore)
                         (my-update-setting (widget-value widget)))
               nil)  ; Initially off
```

#### radio-button-choice

Single selection from multiple options.

```elisp
(widget-create 'radio-button-choice
               :value "medium"
               :notify (lambda (widget &rest ignore)
                         (message "Selected: %s" (widget-value widget)))
               '(item "small")
               '(item "medium")
               '(item "large"))
```

#### menu-choice

Dropdown menu selection.

```elisp
(widget-create 'menu-choice
               :tag "Output Format"
               :value 'json
               '(const :tag "JSON" json)
               '(const :tag "XML" xml)
               '(const :tag "Plain Text" text)
               '(editable-field :menu-tag "Custom" "custom-format"))
```

#### checklist

Multiple selections (subset of options).

```elisp
(widget-create 'checklist
               :notify (lambda (widget &rest ignore)
                         (message "Selected: %S" (widget-value widget)))
               '(const :tag "Option A" option-a)
               '(const :tag "Option B" option-b)
               '(const :tag "Option C" option-c))
```

### Lists

#### editable-list

Dynamic list with add/remove buttons.

```elisp
(widget-create 'editable-list
               :entry-format "%i %d %v"
               :value '("item1" "item2")
               '(editable-field :value ""))
```

**Format specifiers:**
- `%i` - Insert button (INS)
- `%d` - Delete button (DEL)
- `%v` - The value widget

## Widget API

### Creation and Setup

#### widget-create

Create widget and return widget object.

```elisp
(widget-create TYPE [KEYWORD ARGUMENT]...)

;; Example
(setq my-widget
      (widget-create 'editable-field
                     :size 25
                     :value "initial"))
```

#### widget-setup

Enable widgets after creation. Must be called before user interaction.

```elisp
(widget-setup)
```

Required after:
- Initial widget creation
- Calling `widget-value-set`
- Modifying widget structure

#### widget-insert

Insert text at point (not a widget).

```elisp
(widget-insert "Header Text\n\n")
```

### Value Access

#### widget-value

Get current widget value.

```elisp
(widget-value WIDGET)

;; Example
(let ((name (widget-value name-widget))
      (enabled (widget-value checkbox-widget)))
  (message "Name: %s, Enabled: %s" name enabled))
```

#### widget-value-set

Set widget value programmatically.

```elisp
(widget-value-set WIDGET VALUE)
(widget-setup)  ; Required after value change

;; Example
(widget-value-set email-widget "user@example.com")
(widget-setup)
```

### Property Access

#### widget-get

Retrieve widget property.

```elisp
(widget-get WIDGET PROPERTY)

;; Example
(widget-get my-widget :tag)
(widget-get my-widget :size)
```

#### widget-put

Set widget property.

```elisp
(widget-put WIDGET PROPERTY VALUE)

;; Example
(widget-put my-widget :help-echo "Enter your email address")
```

#### widget-apply

Call widget method with arguments.

```elisp
(widget-apply WIDGET PROPERTY &rest ARGS)

;; Example
(widget-apply my-widget :notify)
```

### Navigation

#### widget-forward

Move to next widget (bound to TAB).

```elisp
(widget-forward &optional COUNT)
```

#### widget-backward

Move to previous widget (bound to S-TAB/M-TAB).

```elisp
(widget-backward &optional COUNT)
```

### Cleanup

#### widget-delete

Delete widget and clean up.

```elisp
(widget-delete WIDGET)
```

## Common Patterns

### Form with Validation

```elisp
(defun my-registration-form ()
  (interactive)
  (let (username-widget email-widget)
    (switch-to-buffer "*Registration*")
    (kill-all-local-variables)
    (erase-buffer)
    (remove-overlays)

    (widget-insert "User Registration\n\n")

    (setq username-widget
          (widget-create 'editable-field
                         :format "Username: %v\n"
                         :size 25))

    (widget-insert "\n")

    (setq email-widget
          (widget-create 'editable-field
                         :format "Email: %v\n"
                         :size 40))

    (widget-insert "\n\n")

    (widget-create 'push-button
                   :notify (lambda (&rest ignore)
                             (let ((user (widget-value username-widget))
                                   (email (widget-value email-widget)))
                               (if (and (> (length user) 0)
                                        (string-match-p "@" email))
                                   (message "Registered: %s <%s>" user email)
                                 (message "Invalid input"))))
                   "Register")

    (use-local-map widget-keymap)
    (widget-setup)))
```

### Settings Interface

```elisp
(defun my-settings ()
  (interactive)
  (let (theme-widget auto-save-widget)
    (switch-to-buffer "*Settings*")
    (kill-all-local-variables)
    (erase-buffer)
    (remove-overlays)

    (widget-insert "Application Settings\n\n")

    ;; Theme selection
    (widget-insert "Theme:\n")
    (setq theme-widget
          (widget-create 'radio-button-choice
                         :value my-current-theme
                         '(const :tag "Light" light)
                         '(const :tag "Dark" dark)
                         '(const :tag "Auto" auto)))

    (widget-insert "\n")

    ;; Auto-save toggle
    (setq auto-save-widget
          (widget-create 'checkbox
                         :format "%[%v%] Enable auto-save\n"
                         my-auto-save-enabled))

    (widget-insert "\n")

    ;; Save button
    (widget-create 'push-button
                   :notify (lambda (&rest ignore)
                             (setq my-current-theme
                                   (widget-value theme-widget))
                             (setq my-auto-save-enabled
                                   (widget-value auto-save-widget))
                             (message "Settings saved"))
                   "Save Settings")

    (use-local-map widget-keymap)
    (widget-setup)))
```

### Dynamic Form

```elisp
(defun my-dynamic-list ()
  (interactive)
  (let (items-widget)
    (switch-to-buffer "*Dynamic List*")
    (kill-all-local-variables)
    (erase-buffer)
    (remove-overlays)

    (widget-insert "Todo List\n\n")

    (setq items-widget
          (widget-create 'editable-list
                         :format "%v%i\n"
                         :entry-format "%i %d %v\n"
                         :value '("Buy groceries" "Write code")
                         '(editable-field :size 40)))

    (widget-insert "\n")

    (widget-create 'push-button
                   :notify (lambda (&rest ignore)
                             (let ((items (widget-value items-widget)))
                               (message "Todo items: %S" items)))
                   "Show Items")

    (use-local-map widget-keymap)
    (widget-setup)))
```

### Collecting Multiple Values

```elisp
(defun my-collect-values (widgets)
  "Collect values from multiple widgets into alist."
  (mapcar (lambda (pair)
            (cons (car pair)
                  (widget-value (cdr pair))))
          widgets))

(defun my-form-with-collection ()
  (interactive)
  (let (name-w email-w age-w widgets)
    (switch-to-buffer "*Form*")
    (kill-all-local-variables)
    (erase-buffer)
    (remove-overlays)

    (widget-insert "User Information\n\n")

    (setq name-w (widget-create 'editable-field
                                :format "Name: %v\n"))
    (setq email-w (widget-create 'editable-field
                                 :format "Email: %v\n"))
    (setq age-w (widget-create 'editable-field
                               :format "Age: %v\n"))

    (setq widgets `((name . ,name-w)
                    (email . ,email-w)
                    (age . ,age-w)))

    (widget-insert "\n")

    (widget-create 'push-button
                   :notify (lambda (&rest ignore)
                             (let ((data (my-collect-values widgets)))
                               (message "Data: %S" data)))
                   "Submit")

    (use-local-map widget-keymap)
    (widget-setup)))
```

## Format Strings

Widget `:format` property controls display using escape sequences:

- `%v` - Widget value
- `%t` - Tag
- `%d` - Documentation string
- `%h` - Help-echo
- `%[` - Button prefix
- `%]` - Button suffix
- `%%` - Literal %

```elisp
;; Custom formats
(widget-create 'push-button
               :format "Click %[here%] to continue\n"
               :notify #'my-callback
               "Continue")

(widget-create 'editable-field
               :format "%t: %v (%h)\n"
               :tag "Email"
               :help-echo "user@domain.com")
```

## Notifications and Callbacks

The `:notify` property specifies callback on widget change/activation.

**Callback signature:**
```elisp
(lambda (widget &optional changed-widget &rest event)
  ;; widget - the widget with :notify property
  ;; changed-widget - widget that actually changed (for composite widgets)
  ;; event - interaction event
  ...)
```

**Examples:**

```elisp
;; Simple notification
(widget-create 'checkbox
               :notify (lambda (w &rest _)
                         (message "Checked: %s" (widget-value w)))
               nil)

;; Access parent widget
(widget-create 'radio-button-choice
               :notify (lambda (parent child &rest _)
                         (message "Parent value: %s" (widget-value parent))
                         (message "Child value: %s" (widget-value child)))
               '(item "A")
               '(item "B"))

;; Update other widgets
(let (field1 field2)
  (setq field1
        (widget-create 'editable-field
                       :notify (lambda (w &rest _)
                                 (widget-value-set field2
                                                   (upcase (widget-value w)))
                                 (widget-setup))))
  (setq field2 (widget-create 'editable-field)))
```

## Keybindings

`widget-keymap` provides default bindings:

- `TAB` - `widget-forward` - Next widget
- `S-TAB` / `M-TAB` - `widget-backward` - Previous widget
- `RET` - `widget-button-press` - Activate button
- `C-k` - `widget-kill-line` - Kill to end of field
- `M-TAB` - `widget-complete` - Complete field (if supported)

**Custom keymap:**

```elisp
(defvar my-widget-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map widget-keymap)
    (define-key map (kbd "C-c C-c") 'my-submit)
    (define-key map (kbd "C-c C-k") 'my-cancel)
    map))

(use-local-map my-widget-mode-map)
```

## Error Handling

### Validation

```elisp
(defun my-validate-email (widget)
  (let ((value (widget-value widget)))
    (unless (string-match-p "^[^@]+@[^@]+\\.[^@]+$" value)
      (error "Invalid email format"))))

(widget-create 'editable-field
               :format "Email: %v\n"
               :notify (lambda (w &rest _)
                         (condition-case err
                             (my-validate-email w)
                           (error (message "Error: %s" (error-message-string err))))))
```

### Safe Value Retrieval

```elisp
(defun my-safe-widget-value (widget &optional default)
  "Get widget value with fallback."
  (condition-case nil
      (widget-value widget)
    (error default)))
```

## Buffer Cleanup

```elisp
(defun my-widget-cleanup ()
  "Clean up widget buffer."
  (interactive)
  (when (eq major-mode 'my-widget-mode)
    (remove-overlays)
    (kill-buffer)))

;; With kill-buffer-hook
(defvar my-widget-mode-hook nil)

(add-hook 'my-widget-mode-hook
          (lambda ()
            (add-hook 'kill-buffer-hook
                      (lambda ()
                        (remove-overlays))
                      nil t)))
```

## Defining Custom Widgets

```elisp
(define-widget 'my-email-field 'editable-field
  "Email input field with validation."
  :format "Email: %v\n"
  :size 40
  :valid-regexp "^[^@]+@[^@]+\\.[^@]+$"
  :notify (lambda (widget &rest ignore)
            (let ((value (widget-value widget))
                  (regexp (widget-get widget :valid-regexp)))
              (if (string-match-p regexp value)
                  (message "Valid email")
                (message "Invalid email format")))))

;; Usage
(widget-create 'my-email-field)
```

**Inheritance:**

```elisp
(define-widget 'my-readonly-field 'editable-field
  "Read-only text field."
  :keymap (let ((map (copy-keymap widget-field-keymap)))
            (suppress-keymap map)
            map))
```

## Performance Considerations

**Minimize redraws:**
```elisp
;; Bad: Multiple setups
(widget-value-set widget1 val1)
(widget-setup)
(widget-value-set widget2 val2)
(widget-setup)

;; Good: Batch updates
(widget-value-set widget1 val1)
(widget-value-set widget2 val2)
(widget-setup)
```

**Use buffer-local variables:**
```elisp
(defvar-local my-widget-data nil
  "Buffer-local widget storage.")
```

**Avoid redundant notifications:**
```elisp
(defvar my-updating nil)

(widget-create 'editable-field
               :notify (lambda (w &rest _)
                         (unless my-updating
                           (setq my-updating t)
                           (my-expensive-update)
                           (setq my-updating nil))))
```

## Integration with Major Modes

```elisp
(define-derived-mode my-widget-mode special-mode "MyWidget"
  "Major mode for widget-based interface."
  :group 'my-widget
  (setq truncate-lines t)
  (setq buffer-read-only nil))

(defun my-widget-interface ()
  (interactive)
  (switch-to-buffer "*My Interface*")
  (my-widget-mode)
  (erase-buffer)

  ;; Build interface
  (widget-insert "My Application\n\n")
  ;; ... create widgets ...

  (use-local-map widget-keymap)
  (widget-setup)
  (goto-char (point-min))
  (widget-forward 1))
```

## Debugging

```elisp
;; Inspect widget properties
(pp-eval-expression '(widget-get my-widget :value))

;; Check widget type
(widget-type my-widget)

;; View all properties
(let ((widget my-widget))
  (while widget
    (prin1 (car widget))
    (terpri)
    (setq widget (cdr widget))))

;; Trace notifications
(widget-create 'checkbox
               :notify (lambda (&rest args)
                         (message "Notify called with: %S" args)
                         (backtrace))
               t)
```

## Use Cases

### Configuration Editor

Customize package using widgets instead of Custom interface:

```elisp
(defun my-config-editor ()
  (interactive)
  (let (theme-w size-w)
    (switch-to-buffer "*Config*")
    (kill-all-local-variables)
    (erase-buffer)

    (widget-insert "Configuration\n\n")

    (setq theme-w
          (widget-create 'menu-choice
                         :tag "Theme"
                         :value my-theme
                         '(const light)
                         '(const dark)))

    (setq size-w
          (widget-create 'editable-field
                         :format "\nFont size: %v\n"
                         :value (number-to-string my-font-size)))

    (widget-insert "\n")
    (widget-create 'push-button
                   :notify (lambda (&rest _)
                             (setq my-theme (widget-value theme-w))
                             (setq my-font-size
                                   (string-to-number (widget-value size-w)))
                             (my-apply-config))
                   "Apply")

    (use-local-map widget-keymap)
    (widget-setup)))
```

### Search/Filter Interface

```elisp
(defun my-search-interface ()
  (interactive)
  (let (query-w type-w)
    (switch-to-buffer "*Search*")
    (kill-all-local-variables)
    (erase-buffer)

    (setq query-w
          (widget-create 'editable-field
                         :format "Query: %v\n"
                         :size 50))

    (setq type-w
          (widget-create 'checklist
                         :format "\nTypes:\n%v\n"
                         '(const :tag "Functions" function)
                         '(const :tag "Variables" variable)
                         '(const :tag "Faces" face)))

    (widget-create 'push-button
                   :notify (lambda (&rest _)
                             (my-perform-search
                              (widget-value query-w)
                              (widget-value type-w)))
                   "Search")

    (use-local-map widget-keymap)
    (widget-setup)))
```

### Wizard/Multi-step Form

```elisp
(defvar my-wizard-step 1)
(defvar my-wizard-data nil)

(defun my-wizard-next ()
  (setq my-wizard-data
        (plist-put my-wizard-data
                   (intern (format "step%d" my-wizard-step))
                   (my-collect-current-step)))
  (setq my-wizard-step (1+ my-wizard-step))
  (my-wizard-show))

(defun my-wizard-show ()
  (erase-buffer)
  (cond
   ((= my-wizard-step 1)
    (my-wizard-step-1))
   ((= my-wizard-step 2)
    (my-wizard-step-2))
   (t
    (my-wizard-finish)))
  (widget-setup))
```

## Common Pitfalls

**Forgetting widget-setup:**
```elisp
;; Wrong
(widget-create 'editable-field)
;; User can't interact yet!

;; Correct
(widget-create 'editable-field)
(widget-setup)
```

**Not calling widget-setup after value-set:**
```elisp
;; Wrong
(widget-value-set widget "new value")
;; Widget not updated!

;; Correct
(widget-value-set widget "new value")
(widget-setup)
```

**Incorrect buffer setup:**
```elisp
;; Missing keymap
(erase-buffer)
(widget-create 'editable-field)
(widget-setup)
;; TAB won't navigate!

;; Correct
(erase-buffer)
(widget-create 'editable-field)
(use-local-map widget-keymap)
(widget-setup)
```

**Not cleaning overlays:**
```elisp
;; Memory leak
(erase-buffer)
(widget-create ...)

;; Proper cleanup
(erase-buffer)
(remove-overlays)
(widget-create ...)
```

## Resources

- Info manual: `C-h i m Widget RET`
- Source: `wid-edit.el`, `widget.el` in Emacs distribution
- Demo: `M-x widget-example` (if available)
- Tutorial: Official Emacs Widget Library manual
