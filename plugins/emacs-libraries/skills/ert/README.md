# ERT (Emacs Lisp Regression Testing)

Built-in testing framework for Emacs Lisp, providing comprehensive test definition, execution, and debugging capabilities.

## Quick Start

### Basic Test

```elisp
(require 'ert)

(ert-deftest test-addition ()
  "Test that addition works correctly."
  (should (= 4 (+ 2 2)))
  (should (= 0 (+ -5 5))))
```

### Run Tests

**Interactively:**
```elisp
M-x ert RET t RET
```

**From Command Line:**
```bash
emacs -batch -l ert -l my-tests.el -f ert-run-tests-batch-and-exit
```

### Assertions

```elisp
;; Assert true
(should (= 2 (+ 1 1)))

;; Assert false
(should-not (= 3 (+ 1 1)))

;; Assert error
(should-error (/ 1 0) :type 'arith-error)
```

## Key Features

- **Built-in:** No installation required (Emacs 24+)
- **Interactive debugging:** Backtrace inspection, test reruns
- **Flexible selectors:** Run specific tests by name, tag, or status
- **Batch mode:** CI/CD integration with exit codes
- **Mocking support:** Easy mocking via dynamic binding
- **Rich assertions:** Detailed failure reporting with subexpression values

## Common Patterns

### Temporary Buffers

```elisp
(ert-deftest test-buffer-operation ()
  (with-temp-buffer
    (insert "test content")
    (my-function)
    (should (string= (buffer-string) "expected"))))
```

### Cleanup with unwind-protect

```elisp
(ert-deftest test-with-cleanup ()
  (let ((temp-file (make-temp-file "test-")))
    (unwind-protect
        (progn
          (write-region "data" nil temp-file)
          (should (file-exists-p temp-file)))
      (delete-file temp-file))))
```

### Mocking Functions

```elisp
(ert-deftest test-with-mock ()
  (cl-letf (((symbol-function 'external-call)
             (lambda () "mocked result")))
    (should (string= "mocked result" (my-function)))))
```

### Test Organization with Tags

```elisp
(ert-deftest test-quick-operation ()
  :tags '(quick unit)
  (should (fast-function)))

;; Run only quick tests
M-x ert RET :tag quick RET
```

## Interactive Debugging Commands

When viewing test results (after `M-x ert`):

- `.` - Jump to test definition
- `d` - Re-run test with debugger
- `b` - Show backtrace
- `r` - Re-run test
- `R` - Re-run all tests
- `l` - Show executed assertions
- `m` - Show messages

## Best Practices

1. **Name tests descriptively:** `package-test-feature` format
2. **One assertion focus per test:** Makes failures clear
3. **Isolate test environment:** Use `let`, temporary buffers
4. **Always cleanup:** Use `unwind-protect` for resources
5. **Tag tests:** Organize by speed and type
6. **Test error cases:** Use `should-error` for edge cases
7. **Mock external dependencies:** Avoid filesystem/network I/O

## Using the Skill

This skill provides:
- Complete API reference for all ERT functions
- Comprehensive best practices and patterns
- Detailed examples for common testing scenarios
- Debugging techniques and workflow integration
- CI/CD integration examples

Consult `SKILL.md` for in-depth documentation.

## Learning Path

1. **Basics:** `ert-deftest`, `should`, running tests interactively
2. **Assertions:** `should-not`, `should-error` with error types
3. **Environment:** `with-temp-buffer`, `let`, `unwind-protect`
4. **Organization:** Tags, test naming, selectors
5. **Advanced:** Mocking, fixtures, custom assertions
6. **Integration:** Batch mode, CI/CD, Makefiles

## External Resources

- [Official ERT Manual](https://www.gnu.org/software/emacs/manual/html_mono/ert.html)
- [ERT in Emacs Info](info:ert) or `C-h i m ert RET`
- [ERT Reference Card](https://github.com/fniessen/refcard-ERT)
