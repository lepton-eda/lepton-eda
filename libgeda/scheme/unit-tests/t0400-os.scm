; Test Scheme procedures for accessing host operating system
; information.

(use-modules (unit-test)
             (geda os)
             (srfi srfi-1))

(begin-test 'platform
  (assert-true (every symbol? (platform))))

(begin-test 'separators
  (assert-true (char? separator-char))
  (assert-true (string? separator))
  (assert-equal 1 (string-length separator))

  (assert-true (char? path-separator-char))
  (assert-true (string? path-separator))
  (assert-equal 1 (string-length path-separator))

  (assert-true (separator-char? separator-char)))

(begin-test 'directories
  (assert-true (every string? (sys-data-dirs)))
  (assert-true (every string? (sys-config-dirs)))
  (assert-true (string? (user-data-dir)))
  (assert-true (string? (user-config-dir))))
