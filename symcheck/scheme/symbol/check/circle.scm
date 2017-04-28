(define-module (symbol check circle)
  #:use-module (geda object)
  #:use-module (symbol gettext)
  #:use-module (symbol blame)

  #:export (check-circle-radius
            check-circle))

(define (check-circle-radius object)
  "Checks circle OBJECT's radius."
  (when (= 0 (circle-radius object))
    (blame-object object
                  'error
                  (format #f
                          (_ "Zero radius circle at ~A")
                          (circle-center object)))))

(define (check-circle object)
  "Checks circle OBJECT:
  * Checks that it has non-zero radius."
  (check-circle-radius object))
