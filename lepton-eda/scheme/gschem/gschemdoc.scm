;;; Deprecated module.

(define-module (gschem gschemdoc)
  #:use-module (geda deprecated)
  #:use-module (schematic doc)

  #:re-export (sys-doc-dir
               user-doc-dir
               show-wiki
               show-component-documentation))

(deprecated-module-log-warning! "(schematic doc)")
