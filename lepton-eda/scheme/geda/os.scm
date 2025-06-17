;;; Deprecated module.

(define-module (geda os)
  #:use-module (geda deprecated)
  #:use-module (lepton os)

  #:re-export (platform
               platform?
               separator-char
               separator
               path-separator-char
               path-separator
               separator-char?
               sys-data-dirs
               sys-config-dirs
               user-data-dir
               user-config-dir
               user-cache-dir
               expand-env-variables))

(deprecated-module-log-warning! "(lepton os)")
