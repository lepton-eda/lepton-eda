; Test Scheme procedures for accessing host operating system
; information.

(use-modules (unit-test)
             (lepton os)
             ((geda os) #:renamer (symbol-prefix-proc 'geda:))
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

;;; The same tests for the deprecated (geda os) module
;;; functions.

(begin-test 'geda:platform
  (assert-true (every symbol? (geda:platform))))

(begin-test 'geda:separators
  (assert-true (char? geda:separator-char))
  (assert-true (string? geda:separator))
  (assert-equal 1 (string-length geda:separator))

  (assert-true (char? geda:path-separator-char))
  (assert-true (string? geda:path-separator))
  (assert-equal 1 (string-length geda:path-separator))

  (assert-true (separator-char? geda:separator-char)))

(begin-test 'geda:directories
  (assert-true (every string? (geda:sys-data-dirs)))
  (assert-true (every string? (geda:sys-config-dirs)))
  (assert-true (string? (geda:user-data-dir)))
  (assert-true (string? (geda:user-config-dir))))
