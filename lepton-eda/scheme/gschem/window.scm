;;; Deprecated module.

(define-module (gschem window)
  #:use-module (geda deprecated)
  #:use-module (schematic window)

  #:re-export (active-page
               set-active-page!
               pointer-position
               snap-point))

(deprecated-module-log-warning! "(schematic window)")
