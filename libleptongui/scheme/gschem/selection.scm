;;; Deprecated module.

(define-module (gschem selection)
  #:use-module (geda deprecated)
  #:use-module (schematic selection)

  #:re-export (page-selection
               select-object!
               deselect-object!
               object-selected?))

(deprecated-module-log-warning! "(schematic selection)")
