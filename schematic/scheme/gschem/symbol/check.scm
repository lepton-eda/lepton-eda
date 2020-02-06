;;; Deprecated module.

(define-module (gschem symbol check)
  #:use-module (geda deprecated)
  #:use-module (schematic symbol check)

  #:re-export (check-symbol
               object-blaming-info))

(deprecated-module-log-warning! "(schematic symbol check)")
