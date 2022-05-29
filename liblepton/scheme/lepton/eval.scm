;;; Lepton EDA library - Scheme API
;;; Copyright (C) 2020-2022 Lepton EDA Contributors
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

;;; Protected evaluation of Scheme expressions.

(define-module (lepton eval)
  #:use-module (ice-9 match)

  #:use-module (lepton gettext)
  #:use-module (lepton log)

  #:export (eval-protected
            eval-string-protected))


(define (process-error-stack stack key args)
  (match args
    ((subr message message-args rest)
     ;; Capture long error message (including possible backtrace).
     (let ((long-message
            (with-output-to-string
              (lambda ()
                (when (stack? stack)
                  (let ((port (current-output-port)))
                    (display (G_ "\nBacktrace:\n") port)
                    (display-backtrace stack port)
                    (display "\n" port)
                    (display-error #f port subr message message-args rest)))))))

       ;; Send long message to log.
       (log! 'message "~A\n" long-message)
       (format #t (G_ "ERROR: ~A\nPlease see log file for more information.\n")
               (apply format #f message message-args))))
    (_ #f)))


(define* (eval-protected exp #:optional env)
  "Safely evaluates EXP in the environment ENV.  If ENV is not
given, the function uses '(current-module)'. The function calls
for eval(), catches up any errors or exceptions, if any, and logs
them.  If an error occurs, returns #f, otherwise returns the
results of evaluation."
  (let ((captured-stack #f))
    (catch #t
      ;; Expression to evaluate.
      (lambda () (eval exp (or env (current-module))))
      (lambda (key . args)
        (process-error-stack captured-stack key args))
      (lambda (key . args)
        ;; Capture the stack here:
        (set! captured-stack (make-stack #t))))))


(define* (eval-string-protected s)
  "Safely evaluates string S.  The function catches up any errors
or exceptions, if any, and logs them.  If an error occurs, returns
#f, otherwise returns the results of evaluation."
  (let ((captured-stack #f))
    (catch #t
      ;; Expression to evaluate.
      (lambda () (eval-string s))
      (lambda (key . args)
        (process-error-stack captured-stack key args))
      (lambda (key . args)
        ;; Capture the stack here:
        (set! captured-stack (make-stack #t))))))
