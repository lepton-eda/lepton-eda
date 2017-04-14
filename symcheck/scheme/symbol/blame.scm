(define-module (symbol blame)
  ;; Get symcheck options
  #:use-module (symbol core check)

  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module (symbol gettext)
  #:use-module (geda log)

  #:export (object-blames
            blame-object
            report-blames))

;;; Object property for storing blaming info.
(define object-blames (make-object-property))

(define (blame-object object severity message)
  "Adds blaming MESSAGE with given SEVERITY for schematic
OBJECT. SEVERITY may be one of 'info, 'warning, or 'error."
  (let ((blames (object-blames object)))
    (set! (object-blames object)
          `((,severity . ,message) . ,(if blames blames '())))))

(define verbose (%check-get-verbose-mode))

(define (report-blame object)
  "Reports errors (blames) collected for OBJECT."
  (define (report blame)
    (match blame
      (('info . msg)
       (when (> verbose 2)
         (log! 'message (format #f (_ "Info: ~A") msg)))
       '(1 0 0 0))
      (('warning . msg)
       (when (> verbose 1)
         (log! 'message (format #f (_ "Warning: ~A") msg)))
       '(0 1 0 0))
      (('error . msg)
       (when (> verbose 0)
         (log! 'message (format #f (_ "ERROR: ~A") msg)))
       '(0 0 1 0))
      (_
       (log! 'message (format #f (_ "Unrecognized info: ~A\n") blame))
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
