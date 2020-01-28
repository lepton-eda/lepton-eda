;;; Deprecated module.

(define-module (geda config)

  #:use-module (geda deprecated)
  #:use-module (lepton config)

  #:re-export (config?
               default-config-context
               system-config-context
               user-config-context
               path-config-context
               cache-config-context
               config-filename
               config-load!
               config-loaded?
               config-save!
               config-changed?
               config-parent
               set-config-parent!
               config-trusted?
               set-config-trusted!
               config-trusted-context
               config-groups
               config-has-group?
               config-keys
               config-has-key?
               config-inherited?
               config-source
               config-string
               config-boolean
               config-int
               config-real
               config-string-list
               config-boolean-list
               config-int-list
               config-real-list
               set-config!
               add-config-event!
               remove-config-event!
               config-remove-key!
               config-remove-group!
               config-set-legacy-mode!))

(deprecated-module-log-warning! "(lepton config)")
