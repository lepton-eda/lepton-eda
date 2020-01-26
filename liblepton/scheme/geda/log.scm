;;; Deprecated module.

(define-module (geda log)

  #:use-module (geda deprecated)
  #:use-module (lepton log)

  #:re-export (init-log
               log!))

(deprecated-module-log-warning! "(lepton log)")
