;;; Common definitions for integration tests.

(use-modules (ice-9 textual-ports)
             (srfi srfi-64))

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

;;; Get the exit status of COMMAND, its stdout and stderr output,
;;; and return the three values.
(define (command-values . command)
  (let* ((stdout-pipe (pipe))
         (stdout-pipe-inport (car stdout-pipe))
         (stdout-pipe-outport (cdr stdout-pipe))
         (stderr-pipe (pipe))
         (stderr-pipe-inport (car stderr-pipe))
         (stderr-pipe-outport (cdr stderr-pipe)))
    (format (current-error-port)
            "Test: ~A\n" (string-join command))
    (let ((exit-status
           (status:exit-val
            (with-output-to-port stdout-pipe-outport
              (lambda ()
                (with-error-to-port stderr-pipe-outport
                  (lambda () (apply system* command))))))))
      (close-port stdout-pipe-outport)
      (close-port stderr-pipe-outport)
      (let ((out-string (get-string-all stdout-pipe-inport))
            (err-string (get-string-all stderr-pipe-inport)))
        ;; I don't want to close input ports since the pipes
        ;; should be garbage collected after use.
        (format (current-error-port)
                "Status: ~A\nStdout:\n~A\n\nStderr:\n~A\n\n"
                exit-status
                out-string
                err-string)
        (values exit-status out-string err-string)))))


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

;;; Adjust LANG to avoid issues with localised output.  putenv is
;;; necessary for FreeBSD if the environment variable was not yet
;;; defined.
(putenv "LANG=C")

(define lepton-cli
  (build-filename *abs-top-builddir*
                  "utils"
                  "cli"
                  "scheme"
                  "lepton-cli"))

(define lepton-export
  (build-filename *abs-top-builddir*
                  "utils"
                  "cli"
                  "src"
                  "lepton-export"))

(putenv (string-append "LEPTON_EXPORT" "=" lepton-export))
