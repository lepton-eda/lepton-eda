(define-module (symbol check connection)
  #:use-module (geda object)
  #:use-module (symbol gettext)
  #:use-module (symbol blame)

  #:export (check-connections))

(define (check-connections object)
  "Checks for forbidden OBJECT connections inside symbol."
  (unless (null? (object-connections object))
    (blame-object object
                  'error
                  (format #f
                          (_ "Object with forbidden connections: ~A")
                          (object-type object)))))
