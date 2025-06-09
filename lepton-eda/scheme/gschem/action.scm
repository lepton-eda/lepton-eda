;;; Deprecated module.

(define-module (gschem action)
  #:use-module (geda deprecated)
  #:use-module (schematic action)

  #:re-export (eval-action!
               eval-action-at-point!
               action-position
               action?
               make-action
               set-action-property!
               action-property
               &repeat-last-action))

(deprecated-module-log-warning! "(schematic action)")
