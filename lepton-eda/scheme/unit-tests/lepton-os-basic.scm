;;; Test Scheme procedures for accessing host operating system
;;; information.

(use-modules (lepton os)
             (srfi srfi-1))

(test-begin "platform" 1)

(test-assert (every symbol? (platform)))

(test-end "platform")


(test-begin "separators" 7)

(test-assert (char? separator-char))
(test-assert (string? separator))
(test-equal 1 (string-length separator))

(test-assert (char? path-separator-char))
(test-assert (string? path-separator))
(test-equal 1 (string-length path-separator))

(test-assert (separator-char? separator-char))

(test-end "separators")


(test-begin "directories" 4)

(test-assert (every string? (sys-data-dirs)))
(test-assert (every string? (sys-config-dirs)))
(test-assert (string? (user-data-dir)))
(test-assert (string? (user-cache-dir)))
(test-assert (string? (user-config-dir)))

(test-end "directories")
