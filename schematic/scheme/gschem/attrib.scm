;;; Deprecated module.

(define-module (gschem attrib)
  #:use-module (geda deprecated)
  #:use-module (schematic attrib)

  #:re-export (add-attrib!))

(deprecated-module-log-warning! "(schematic attrib)")
