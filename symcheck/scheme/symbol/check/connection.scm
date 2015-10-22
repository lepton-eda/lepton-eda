(define-module (symbol check connection)
  #:use-module (geda object)
  #:use-module (symbol gettext)
  #:use-module (symbol blame)

  #:export (check-connections
            check-net/bus))

(define (check-connections object)
  "Checks for OBJECT connection inside symbol."
  (unless (null? (object-connections object))
    (blame-object object
                  'error
                  (format #f (_ "Found a connection inside a symbol")))))

(define (check-net/bus object)
  "Checks if OBJECT is net or bus."
  (when (or (bus? object)
            (net? object))
    (blame-object object
                  'error
                  (format #f
                          (_ "Found a ~A inside a symbol")
                          (object-type object)))))
