;;; Deprecated module.

(define-module (geda repl)
  #:use-module (lepton page)

  #:re-export (lepton-repl
               lepton-repl-save-history
               lepton-repl-welcome
               lepton-repl-readline-warning))

(deprecated-module-log-warning! "(lepton repl)")
