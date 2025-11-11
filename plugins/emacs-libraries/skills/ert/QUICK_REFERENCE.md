# ERT Quick Reference

## Test Definition

```elisp
(ert-deftest NAME ()
  [DOCSTRING]
  [:tags (TAG...)]
  BODY...)
```

**Example:**
```elisp
(ert-deftest my-pkg-test-feature ()
  "Test feature implementation."
  :tags '(unit quick)
  (should (my-pkg-feature-works)))
```

## Assertions

| Macro | Purpose | Example |
|-------|---------|---------|
| `(should FORM)` | Assert true | `(should (= 4 (+ 2 2)))` |
| `(should-not FORM)` | Assert false | `(should-not (zerop 5))` |
| `(should-error FORM [:type TYPE])` | Assert error | `(should-error (/ 1 0) :type 'arith-error)` |

## Running Tests

### Interactive

```elisp
M-x ert RET SELECTOR RET
```

**Common selectors:**
- `t` - All tests
- `"^my-pkg-"` - Tests matching regex
- `:tag quick` - Tests with tag
- `:failed` - Failed tests from last run
- `(not :tag slow)` - Exclude slow tests

### Batch Mode

```bash
# Run all tests
emacs -batch -l ert -l tests.el -f ert-run-tests-batch-and-exit

# Run specific tests
emacs -batch -l ert -l tests.el \
  --eval '(ert-run-tests-batch-and-exit "^test-feature-")'

# Quiet mode
emacs -batch -l ert -l tests.el \
  --eval '(let ((ert-quiet t)) (ert-run-tests-batch-and-exit))'
```

## Interactive Debugging Keys

In `*ert*` results buffer:

| Key | Command | Description |
|-----|---------|-------------|
| `.` | Jump to definition | Open test source code |
| `d` | Debug test | Re-run with debugger |
| `b` | Show backtrace | Display failure backtrace |
| `r` | Re-run test | Re-run test at point |
| `R` | Re-run all | Re-run all tests |
| `l` | Show assertions | Display executed `should` forms |
| `m` | Show messages | Display test messages |
| `TAB` | Expand/collapse | Toggle test details |

## Test Environment Patterns

### Temporary Buffer

```elisp
(with-temp-buffer
  (insert "content")
  (my-function)
  (should (string= (buffer-string) "expected")))
```

### Cleanup

```elisp
(let ((resource (create-resource)))
  (unwind-protect
      (should (use-resource resource))
    (cleanup-resource resource)))
```

### Local Variables

```elisp
(let ((my-var 'test-value))
  (should (eq 'test-value (get-my-var))))
```

## Mocking

### With cl-letf

```elisp
(cl-letf (((symbol-function 'external-fn)
           (lambda () "mocked")))
  (should (string= "mocked" (calls-external-fn))))
```

### With flet (older style)

```elisp
(require 'cl)
(flet ((external-fn () "mocked"))
  (should (string= "mocked" (calls-external-fn))))
```

## Skip Tests

```elisp
;; Skip if condition false
(skip-unless (featurep 'some-feature))

;; Skip if condition true
(skip-when (eq system-type 'windows-nt))
```

## Test Organization

### Naming Convention

```elisp
;; Format: package-test-feature
(ert-deftest my-package-test-parsing () ...)
(ert-deftest my-package-test-validation () ...)
```

### Tags

```elisp
(ert-deftest test-quick ()
  :tags '(quick unit)
  ...)

(ert-deftest test-slow ()
  :tags '(slow integration)
  ...)

;; Run: M-x ert RET :tag quick RET
;; Run: M-x ert RET (not :tag slow) RET
```

## Fixture Pattern

```elisp
(defun with-test-fixture (body)
  "Execute BODY within test fixture."
  (let ((setup (do-setup)))
    (unwind-protect
        (funcall body setup)
      (do-teardown setup))))

(ert-deftest my-test ()
  (with-test-fixture
   (lambda (fixture)
     (should (test-with fixture)))))
```

## Common Test Patterns

### Testing Errors

```elisp
;; Any error
(should-error (/ 1 0))

;; Specific error type
(should-error (error "msg") :type 'error)
(should-error (/ 1 0) :type 'arith-error)
```

### Testing Buffer State

```elisp
(with-temp-buffer
  (my-mode)
  (should (eq major-mode 'my-mode))
  (should (local-variable-p 'my-mode-var)))
```

### Testing Interactive Commands

```elisp
(with-temp-buffer
  (insert "text")
  (goto-char (point-min))
  (call-interactively 'my-command)
  (should (= (point) 5)))
```

### Testing Messages

```elisp
(let ((messages))
  (cl-letf (((symbol-function 'message)
             (lambda (fmt &rest args)
               (push (apply #'format fmt args) messages))))
    (my-function)
    (should (member "Expected message" messages))))
```

## Test File Template

```elisp
;;; my-package-test.el --- Tests for my-package -*- lexical-binding: t -*-

;;; Commentary:
;; Test suite for my-package.

;;; Code:

(require 'ert)
(require 'my-package)

(ert-deftest my-package-test-basic ()
  "Test basic functionality."
  (should (my-package-function)))

(provide 'my-package-test)
;;; my-package-test.el ends here
```

## Makefile Integration

```makefile
.PHONY: test
test:
	emacs -batch -l ert \
	  -l my-package.el \
	  -l test/my-package-test.el \
	  -f ert-run-tests-batch-and-exit
```

## CI/CD (GitHub Actions)

```yaml
- name: Run tests
  run: |
    emacs -batch -l ert \
      -l my-package.el \
      -l test/my-package-test.el \
      -f ert-run-tests-batch-and-exit
```

## Key Best Practices

1. **Name descriptively:** `package-test-feature` format
2. **Use tags:** Organize by speed/type
3. **Isolate environment:** `let`, temp buffers
4. **Always cleanup:** `unwind-protect`
5. **Mock I/O:** Avoid filesystem/network
6. **Test errors:** Use `should-error`
7. **One focus per test:** Clear failures
8. **Add docstrings:** Document test purpose

## Resources

- **Manual:** `C-h i m ert RET`
- **Online:** https://www.gnu.org/software/emacs/manual/html_mono/ert.html
- **Source:** `lisp/emacs-lisp/ert.el`
