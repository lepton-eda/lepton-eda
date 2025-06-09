;;; Deprecated module.

(define-module (gschem hook)
  #:use-module (geda deprecated)
  #:use-module (schematic hook)

  #:re-export (add-objects-hook
               copy-objects-hook
               remove-objects-hook
               move-objects-hook
               mirror-objects-hook
               rotate-objects-hook
               paste-objects-hook
               attach-attribs-hook
               detach-attribs-hook
               select-objects-hook
               deselect-objects-hook
               new-page-hook
               open-page-hook
               action-property-hook
               bind-keys-hook
               switch-action-mode-hook))

(deprecated-module-log-warning! "(schematic hook)")
