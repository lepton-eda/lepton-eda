(define-module (symbol check net)
  #:use-module (symbol check forbidden)
  #:use-module (symbol check connection)
  #:use-module (symbol check line)

  #:export (check-net))

(define (check-net object)
  "Checks net OBJECT.
  * Checks if it is forbidden in symbols.
  * Checks if it has forbidden connections.
  * Checks that it has non-zero size."
  (check-forbidden object)
  (check-connections object)
  (check-line-size object))
