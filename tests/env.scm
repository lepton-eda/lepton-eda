;;; Common definitions for integration tests.

(define (build-filename . ls)
  (string-join ls file-name-separator-string))

(define *abs-top-builddir* (getenv "abs_top_builddir"))
(define *abs-top-srcdir* (getenv "abs_top_srcdir"))

(define *liblepton*
        (build-filename *abs-top-builddir*
                        "liblepton"
                        "src"
                        "liblepton"))

;;; Test if netlister exists :-)
(define *netlister*
  (build-filename *abs-top-builddir*
                  "utils"
                  "netlist"
                  "lepton-netlist"))

;;; For FreeBSD, where non-existing variables are not set by
;;; setenv() without putenv().
(putenv (string-append "LIBLEPTON" "=" *liblepton*))

(define %src-scheme-dir%
  (build-filename *abs-top-srcdir* "liblepton" "scheme"))

(define %build-scheme-dir%
  (build-filename *abs-top-builddir* "liblepton" "scheme"))

;;; Now set up %load-path to include local scheme directory.
(putenv (string-append "GUILE_LOAD_PATH" "="
                       (string-append %src-scheme-dir% ":" %build-scheme-dir%)))

(define (build-test-file-name name)
  (build-filename *abs-top-srcdir* "utils" "netlist" "tests" name))

(define *backend-directory*
  (build-filename *abs-top-srcdir* "utils" "netlist" "scheme" "backend"))

(define *symbol-directory*
  (build-filename *abs-top-srcdir* "utils" "netlist" "tests" "symcache"))

(define *component-library-command*
  (string-append "(component-library \"" *symbol-directory* "\")"))
