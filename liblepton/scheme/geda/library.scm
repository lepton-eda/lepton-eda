;;; Deprecated module.

(define-module (geda library)
  #:use-module (geda deprecated)
  #:use-module (lepton library)

  #:re-export (%default-source-library
               source-library
               source-library-search
               reset-source-library
               source-library-contents
               set-source-library-contents!
               get-source-library-file))

(deprecated-module-log-warning! "(lepton library)")
