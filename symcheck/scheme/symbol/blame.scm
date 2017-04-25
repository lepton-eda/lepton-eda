(define-module (symbol blame)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module (symbol gettext)
  #:use-module (symcheck option)
  #:use-module (geda log)

  #:export (object-blames
            blame-object
            report-blame-statistics))

;;; Object property for storing blaming info.
(define object-blames (make-object-property))

(define (blame-object object severity message)
  "Adds blaming MESSAGE with given SEVERITY for schematic
OBJECT. SEVERITY may be one of 'info, 'warning, or 'error."
  (let ((blames (object-blames object)))
    (set! (object-blames object)
          `((,severity . ,message) . ,(if blames blames '())))))

(define verbose (symcheck-option-ref-length 'verbose))
(define quiet (symcheck-option-ref 'quiet))

(define %check-log-destination 'stdout)

;;; Logs MESSAGE formatted using FORMAT-ARGS and log LEVEL. The
;;; output goes to the destination defined in variable
;;; %check-log-destination which currently may be 'stdout or 'log.
(define (check-log! level message . format-args)
  (define (stdout-log! level message . format-args)
    (unless quiet
      (apply format #t message format-args)
      (newline)))

  (let ((log-func (case %check-log-destination
                    ((stdout) stdout-log!)
                    ((log) log!)
                    (else stdout-log!))))
    (apply log-func level message format-args)))

(define (report-blame object)
  "Reports errors (blames) collected for OBJECT."
  (define (report blame)
    (match blame
      (('info . msg)
       (when (> verbose 2)
         (check-log! 'message (format #f (_ "Info: ~A") msg)))
       '(1 0 0 0))
      (('warning . msg)
       (when (> verbose 1)
         (check-log! 'message (format #f (_ "Warning: ~A") msg)))
       '(0 1 0 0))
      (('error . msg)
       (when (> verbose 0)
         (check-log! 'message (format #f (_ "ERROR: ~A") msg)))
       '(0 0 1 0))
      (_
       (check-log! 'message (format #f (_ "Unrecognized info: ~A\n") blame))
       '(0 0 0 1))))

  (map report (or (object-blames object) '())))

(define (report-blames objects)
  "Reports object errors (blames) and returns their count as a
list of the form:
  (info-count warning-count error-count unrecognized-count)"
  (match (append-map report-blame objects)
    (((info warning error unrecognized) ...)
     `(,(apply + info)
       ,(apply + warning)
       ,(apply + error)
       ,(apply + unrecognized)))))

(define (report-blame-statistics objects)
  "Reports blame statistics for given list of OBJECTS."
  (define (report-statistics info-count
                             warning-count
                             error-count
                             unrecognized-count)

    (unless (zero? warning-count)
      (check-log! 'message (N_ "~A warning found"
                               "~A warnings found"
                               warning-count)
                  warning-count)
      (when (< verbose 2)
        (check-log! 'message (_ "(use -vv to view details)"))))

    (if (zero? error-count)
        (check-log! 'message (_ "No errors found"))
        (begin
          (check-log! 'message (N_ "~A ERROR found"
                                   "~A ERRORS found"
                                   error-count)
                      error-count)
          (when (< verbose 1)
            (check-log! 'message (_ "(use -v to view details)")))))

    ;; return code
    (if (zero? error-count)
        (if (zero? warning-count) 0 1)
        2))

  (apply report-statistics (report-blames objects)))
