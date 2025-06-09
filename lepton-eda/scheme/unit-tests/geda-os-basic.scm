;;; Test Scheme procedures for accessing host operating system
;;; information.

(use-modules ((geda os) #:renamer (symbol-prefix-proc 'geda:))
             (srfi srfi-1))

(test-begin "geda:platform" 1)

(test-assert (every symbol? (geda:platform)))

(test-end "geda:platform")


(test-begin "geda:separators" 7)

(test-assert (char? geda:separator-char))
(test-assert (string? geda:separator))
(test-equal 1 (string-length geda:separator))

(test-assert (char? geda:path-separator-char))
(test-assert (string? geda:path-separator))
(test-equal 1 (string-length geda:path-separator))

(test-assert (geda:separator-char? geda:separator-char))

(test-end "geda:separators")


(test-begin "geda:directories" 4)

(test-assert (every string? (geda:sys-data-dirs)))
(test-assert (every string? (geda:sys-config-dirs)))
(test-assert (string? (geda:user-data-dir)))
(test-assert (string? (geda:user-config-dir)))

(test-end "geda:directories")
