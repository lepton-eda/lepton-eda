;;; Deprecated module.

(define-module (gschem builtins)
  #:use-module (geda deprecated)
  #:use-module (schematic builtins)

  #:re-export (;; Special actions
               &cancel
               ;; File menu actions
               &file-new
               &file-open
               &file-save
               &file-save-as
               &file-save-all
               &file-print
               &file-image
               &file-script
               &file-new-window
               &file-close-window
               &file-quit
               &file-repl
               ;; General editing actions
               &edit-undo
               &edit-redo
               &edit-select
               &edit-select-all
               &edit-deselect
               &edit-delete
               &edit-move
               &edit-copy
               &edit-mcopy
               &edit-rotate-90
               &edit-mirror
               &edit-edit
               &edit-text
               &edit-slot
               &edit-object-properties
               &edit-translate
               &edit-lock
               &edit-unlock
               &edit-invoke-macro
               &edit-embed
               &edit-unembed
               &edit-update
               &edit-show-hidden
               ;; Clipboard actions
               &clipboard-cut
               &clipboard-copy
               &clipboard-paste
               ;; View control actions
               &view-sidebar
               &view-status
               &view-find-text-state
               &view-redraw
               &view-pan
               &view-pan-left
               &view-pan-right
               &view-pan-up
               &view-pan-down
               &view-zoom-box
               &view-zoom-extents
               &view-zoom-in
               &view-zoom-out
               &view-zoom-full
               &view-dark-colors
               &view-light-colors
               &view-bw-colors
               &view-color-edit
               ;; Page-related actions
               &page-revert
               &page-manager
               &page-prev
               &page-next
               &page-close
               &page-next-tab
               &page-prev-tab
               ;; Actions related to adding things
               &add-component
               &add-attribute
               &add-net
               &add-bus
               &add-text
               &add-line
               &add-path
               &add-box
               &add-circle
               &add-arc
               &add-pin
               &add-picture
               ;; Hierarchy actions
               &hierarchy-down-schematic
               &hierarchy-down-symbol
               &hierarchy-up
               ;; Attribute actions
               &attributes-attach
               &attributes-detach
               &attributes-show-value
               &attributes-show-name
               &attributes-show-both
               &attributes-visibility-toggle
               &edit-find-text
               &edit-hide-text
               &edit-show-text
               &edit-autonumber
               ;; Configuration actions
               &help-hotkeys
               &options-grid
               &options-snap
               &options-snap-size
               &options-scale-up-snap-size
               &options-scale-down-snap-size
               &options-action-feedback
               &options-rubberband
               &options-magneticnet
               &options-show-log-window
               &options-show-coord-window
               &options-select-font
               &options-draw-grips
               ;; Documentation-related actions
               &hierarchy-documentation
               &help-manual
               &help-wiki
               &help-about
               ;; Backward compatibility:
               &edit-color
               &edit-linetype
               &edit-filltype
               &edit-pin-type))

(deprecated-module-log-warning! "(schematic builtins)")
