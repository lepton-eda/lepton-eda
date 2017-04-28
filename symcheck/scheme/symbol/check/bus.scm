(define-module (symbol check bus)
  #:use-module (symbol check forbidden)
  #:use-module (symbol check connection)
  #:use-module (symbol check line)

  #:export (check-bus))

(define (check-bus object)
  "Checks bus object:
  * Checks if it is forbidden in symbols.
  * Checks that it has no forbidden connections.
  * Checks that it has non-zero size."
  (check-forbidden object)
  (check-connections object)
  (check-line-size object))
