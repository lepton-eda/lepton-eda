(define-module (symbol check component)
  #:use-module (symbol check forbidden)
  #:use-module (symbol check connection)

  #:export (check-component))

(define (check-component object)
  "Checks component OBJECT.
  * Checks if it is forbidden in symbols.
  * Checks if it has forbidden connections."
  (check-forbidden object)
  (check-connections object))
