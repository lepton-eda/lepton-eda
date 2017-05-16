(define-module (symbol check log)
  #:use-module (symbol gettext)
  #:use-module (geda log)

  #:export (set-check-log-destination!
            current-check-log-destination
            check-log!))

(define %check-log-destination 'log)

(define (set-check-log-destination! value)
  "Sets check-log! destination to VALUE which can be 'log or
'stdout. Returns the new destination. Signals an error is wrong
destination is specified."
  (if (or (eq? value 'log) (eq? value 'stdout))
      (begin (set! %check-log-destination value) value)
      (error (_ "Wrong check log destination!"))))

(define (current-check-log-destination)
  "Returns current destination for the check-log! procedure."
  %check-log-destination)

(define (check-log! level message . format-args)
  "Logs MESSAGE formatted using FORMAT-ARGS with severity
LEVEL. The output goes to the destination defined using
set-check-log-destination!."
  (define (stdout-log! level message . format-args)
    (apply format #t message format-args)
    (newline))

  (let ((log-func (case %check-log-destination
                    ((stdout) stdout-log!)
                    ((log) log!)
                    (else stdout-log!))))
    (apply log-func level message format-args)))
