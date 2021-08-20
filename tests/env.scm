;;; Common definitions for integration tests.

(use-modules (srfi srfi-64))

(define-syntax test-run-success
  (lambda (x)
    (syntax-case x ()
      ((_ <prog> <arg> ...)
       #'(test-eq EXIT_SUCCESS
           (status:exit-val (system* <prog> <arg> ...)))))))

(define-syntax test-run-failure
  (lambda (x)
    (syntax-case x ()
      ((_ <prog> <arg> ...)
       #'(test-eq EXIT_FAILURE
           (status:exit-val (system* <prog> <arg> ...)))))))

(define-syntax test-grep-stdout
  (lambda (x)
    (syntax-case x ()
      ((_ <str> <command> <option> ...)
       #'(let ((command (string-join (list <command>
                                           <option> ...
                                           "|"
                                           "grep"
                                           (format #f "~S" <str>)))))
           (format (current-error-port) "Test command: ~A\n" command)
           (test-eq EXIT_SUCCESS
             (status:exit-val (system command))))))))

(define-syntax test-grep-stderr
  (lambda (x)
    (syntax-case x ()
      ((_ <str> <command> <option> ...)
       #'(let ((command (string-join (list <command>
                                           <option> ...
                                           "2>&1"
                                           ">/dev/null"
                                           "|"
                                           "grep"
                                           (format #f "~S" <str>)))))
           (format (current-error-port) "Test command: ~A\n" command)
           (test-eq EXIT_SUCCESS
             (status:exit-val (system command))))))))

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
