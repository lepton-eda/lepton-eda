;;; Deprecated module.

(define-module (gschem keymap)
  #:use-module (geda deprecated)
  #:use-module (schematic keymap)
  #:re-export (;; Key combinations
               key?
               key->string
               key->display-string
               string->key
               ;; Key sequences
               keys?
               keys->string
               string->keys
               keys->display-string
               ;; Keymaps
               keymap?
               make-keymap
               keymap-lookup-key
               keymap-bind-key!
               keymap-lookup-binding
               keymap-for-each
               ;; Recursive keymaps
               lookup-keys
               bind-keys!
               lookup-binding))

(deprecated-module-log-warning! "(schematic keymap)")
