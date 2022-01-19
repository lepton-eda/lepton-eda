;;; Autotools compatible SRFI-64 Scheme unit-test framework
;;; Copyright (C) 2016 gEDA Contributors
;;; Copyright (C) 2018-2022 Lepton EDA Contributors
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.


;;; Refer to the following links for more information:
;;;   https://www.gnu.org/software/automake/manual/html_node/Tests.html
;;;   http://srfi.schemers.org/srfi-64/srfi-64.html
;;;
;;;
;;; This script is launched by autotools like this:
;;; (test.scm --test-name file.scm
;;;           --log-file file.log
;;;           --trs-file file.trs
;;;           --color-tests no
;;;           --enable-hard-errors yes
;;;           --expect-failure no
;;;           -- -L . unit-tests/file.scm)
;;;
;;;
;;; autotools expect the output .trs file contain the info as
;;; follows:
;;;
;;; :test-result: value
;;;   May be used several times, say, for several procedures. Its
;;;   value must be one of: "PASS", "XFAIL", "SKIP", "FAIL",
;;;   "XPASS", "ERROR" and optionally some text after it.
;;;
;;; :recheck: value
;;;   Must be present once or not present at all. If its value is
;;;   "no", 'make recheck' won't run the corresponding test. Since
;;;   I don't know if this would be useful, let's set it to "yes"
;;;   (we could just omit it as well).
;;;
;;; :copy-in-global-log: value
;;;   Must be present once. I believe the best value for it is
;;;   "no", so we won't double log info for each test.
;;;
;;; :test-global-result: value
;;;   Must be present once if several tests are done in one
;;;   script. It should output some summary.


;;; Set location of liblepton library.
(setenv "LIBLEPTON"
        (string-join '(".." "src" "liblepton")
                     file-name-separator-string))

(use-modules (srfi srfi-64)
             (srfi srfi-26)
             (ice-9 getopt-long)
             (ice-9 pretty-print)
             (lepton ffi)
             (lepton toplevel))

;;; In order to facilitate debugging, you can increase the pile of
;;; info reported on errors by setting the COLUMNS environment
;;; variable to a bigger value. Example:
;; (setenv "COLUMNS" "1000")

(setenv "LEPTON_INHIBIT_RC_FILES" "yes")
;;; The load path should not be changed after loading the
;;; liblepton library.  This is tested in the unit test
;;; "test-netlist-load-path.scm".
(setenv "INITIAL_GUILE_LOAD_PATH"
        (with-output-to-string (lambda () (write %load-path))))

;;; Initialize liblepton library.
(liblepton_init)
;;; Skip initialisation of RC paths here.  It's what the
;;; environment variable LEPTON_INHIBIT_RC_FILES is actually for.
;;;   (unless (getenv "LEPTON_INHIBIT_RC_FILES")
;;;     (register-data-dirs))
(edascm_init)

(define with-toplevel (@@ (lepton core toplevel) %with-toplevel))

;;; Syntax and procedure that check exception type and probably
;;; have no analogs in SRFI-64.
(define (%assert-thrown key thunk)
  (catch key
    (lambda ()
      (thunk)
      (throw 'test-failed-exception
             (simple-format #f "  assert-thrown: expected exception: ~S"
                            key)))
    (lambda (key . args) #t)))

(define-syntax test-assert-thrown
  (syntax-rules ()
    ((_ key . test-forms)
     (test-assert (%assert-thrown key (lambda () . test-forms))))))

(define (report s port)
  (display s port)
  (display s (current-error-port)))

(define (report-runner-properties runner port)
  (let ((detailed-info
         (apply format #f "
Detailed information
====================
File ~ALine ~A

Failed form:
~A

Expected value:
~A
Actual value:
~A

Expected error:
~A
Actual error:
~A
"
                (map (lambda (prop)
                       (with-output-to-string
                         (lambda () (pretty-print
                                (test-result-ref runner prop)))))
                     '(source-file
                       source-line
                       source-form
                       expected-value
                       actual-value
                       expected-error
                       actual-error)))))
    (report detailed-info port)))

(define (custom-test-runner name log-port trs-port colored?
                            enable-hard-errors? expect-failure?)
  (let ((runner (test-runner-null))
        (group-name #f)
        (group-count 0))
    (test-runner-on-group-begin! runner
      (lambda (runner suite-name count)
        (set! group-name suite-name)
        (set! group-count 0)))
    (test-runner-on-test-begin! runner
      (lambda (runner)
        (set! group-count (1+ group-count))))
    (test-runner-on-test-end! runner
      (lambda (runner)
        (let* ((result (test-result-kind runner))
               (result-string (case result
                                ((pass xpass fail xfail skip)
                                 (string-upcase (symbol->string result)))
                                (else "ERROR"))))
          (format trs-port ":test-result: ~A\n" result-string)
          (report (format #f "~A: ~A (~A)\n"
                          result-string group-name group-count)
                  log-port)
          (when (eq? result 'fail)
            (report-runner-properties runner log-port)))))
    (test-runner-on-final! runner
      (lambda (runner) (report "\n" log-port)))
        runner))

(define (main args)
  (define (yes-no s)
    (and (string? s) (not (string=? "no" s))))

  (let* ((option-spec '((test-name (value #t))
                        (log-file (value #t))
                        (trs-file (value #t))
                        (color-tests (value #t))
                        (enable-hard-errors (value #t))
                        (expect-failure (value #t))))
         (options (getopt-long args option-spec))
         (name (option-ref options 'test-name #f))
         (log (option-ref options 'log-file #f))
         (trs (option-ref options 'trs-file #f))
         (string-colored (option-ref options 'color-tests #f))
         (string-enable-hard-errors (option-ref options 'enable-hard-errors #f))
         (string-expect-failure (option-ref options 'expect-failure #f))
         (colored? (and=> string-colored yes-no))
         (enable-hard-errors? (and=> string-enable-hard-errors yes-no))
         (expect-failure? (and=> string-expect-failure yes-no)))
    (if (and (string? log) (string? trs))
        (let* ((log-port (open-output-file log))
               (trs-port (open-output-file trs))
               (runner (custom-test-runner name
                                           log-port
                                           trs-port
                                           colored?
                                           enable-hard-errors?
                                           expect-failure?)))
          (format trs-port ":copy-in-global-log: no\n:recheck: yes\n")
          (test-runner-factory (lambda () runner))
          (let ((captured-stack #f))
            (catch #t
              (lambda ()
                (report (format #f
                                "\nLoading: ~S\n"
                                name)
                        log-port)
                (load-from-path name))
              (lambda (key . args)
                (report (format #f
                                "ERROR:\n Unexpected exception on loading file ~S:\n~A\n"
                                name
                                (with-output-to-string
                                  (lambda ()
                                    (display-backtrace
                                     captured-stack
                                     (current-output-port)))))
                        log-port))
              (lambda (key . args)
                ;; Capture the stack here:
                (set! captured-stack (make-stack #t))))
            (if captured-stack

                (primitive-exit 1)))

          (format trs-port ":test-global-result: ~A\n"
                  (if (> (test-runner-fail-count runner) 0)
                      "FAIL" "PASS"))
          (test-runner-reset runner)
          (close-port log-port)
          (close-port trs-port))
        (display "Use 'make check' to run tests.\n"))))

;;; Wrapper for the main() function allowing using of liblepton
;;; variables, procedures, and modules.
(define (main/with-toplevel args)
  (with-toplevel (%make-toplevel)
   (lambda () (main args))))
