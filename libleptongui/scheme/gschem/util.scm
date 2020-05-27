;;; Deprecated module.

(define-module (gschem util)
  #:use-module (geda deprecated)
  #:use-module (schematic util)

  #:re-export (show-uri
               show-file))

(deprecated-module-log-warning! "(schematic util)")
