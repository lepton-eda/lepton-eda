(define-module (symbol check forbidden)
  #:use-module (geda object)
  #:use-module (symbol gettext)
  #:use-module (symbol blame)

  #:export (check-forbidden))

;;; Checks if current object is forbidden in symbols.
(define (forbidden? object)
  (or (bus? object)
      (net? object)
      (component? object)))

(define (check-forbidden object)
  "Checks if OBJECT is forbidden in symbols."
  (when (forbidden? object)
    (blame-object object
                  'error
                  (format #f
                          (_ "Object forbidden inside symbols: ~A")
                          (object-type object)))))
