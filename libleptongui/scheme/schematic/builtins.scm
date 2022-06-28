;; Lepton EDA Schematic Capture
;; Scheme API
;; Copyright (C) 2013 Peter Brett <peter@peter-b.co.uk>
;; Copyright (C) 2013-2015 gEDA Contributors
;; Copyright (C) 2017-2022 Lepton EDA Contributors
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.

(define-module (schematic builtins)
  #:use-module (srfi srfi-1)
  #:use-module (system foreign)

  #:use-module (lepton attrib)
  #:use-module (lepton ffi)
  #:use-module (lepton log)
  #:use-module (lepton object)
  #:use-module (lepton page)
  #:use-module (lepton repl)

  #:use-module (schematic action)
  #:use-module (schematic core gettext)
  #:use-module (schematic ffi)
  #:use-module (schematic doc)
  #:use-module (schematic gui keymap)
  #:use-module (schematic hook)
  #:use-module (schematic menu)
  #:use-module (schematic repl)
  #:use-module (schematic selection)
  #:use-module (schematic undo)
  #:use-module (schematic window foreign)
  #:use-module (schematic window))


(define-syntax define-action-public
  (syntax-rules ()
    ((_ (name . args) . forms)
     (begin
       (define-action (name . args) . forms)
       (export name)))))

;;; Check if current window is not NULL and run a foreign
;;; C-CALLBACK.  If it is NULL, inform the user and return
;;; #f. ACTION-NAME is a string representing the name of action
;;; the callback is called in.
(define-syntax run-callback
  (syntax-rules ()
    ((_ c-callback action-name)
     (let ((*window (and=> (current-window) window->pointer)))
       (if *window
           (c-callback %null-pointer *window)
           (begin
             (log! 'critical "~S: Current window is unavailable." action-name)
             #f))))))

(define-syntax *current-window
  (syntax-rules ()
    ((_)
     (let ((*window (and=> (current-window) window->pointer)))
       (or *window
           (error "Current window is unavailable."))))))

;; -------------------------------------------------------------------
;;;; Special actions

(define-action-public (&cancel #:label (G_ "Cancel"))
  (run-callback i_callback_cancel "&cancel"))

;; -------------------------------------------------------------------
;;;; File menu actions

(define-action-public (&file-new #:label (G_ "New File") #:icon "gtk-new")
  (run-callback i_callback_file_new "&file-new"))

(define-action-public (&file-open #:label (G_ "Open File") #:icon "gtk-open")
  (run-callback i_callback_file_open "&file-open"))

(define-action-public (&file-save #:label (G_ "Save") #:icon "gtk-save")
  (run-callback i_callback_file_save "&file-save"))

(define-action-public (&file-save-as #:label (G_ "Save As") #:icon "gtk-save-as")
  (define *window (*current-window))
  (x_fileselect_save *window
                     (schematic_window_get_active_page *window)
                     %null-pointer))

(define-action-public (&file-save-all #:label (G_ "Save All") #:icon "gtk-save")
  (run-callback i_callback_file_save_all "&file-save-all"))

(define-action-public (&file-print #:label (G_ "Print") #:icon "gtk-print")
  (x_print (*current-window)))

(define-action-public (&file-image #:label (G_ "Export Image"))
  (x_image_setup (*current-window)))

(define-action-public (&file-script #:label (G_ "Run Script") #:icon "gtk-execute")
  (run-callback i_callback_file_script "&file-script"))

(define (make-schematic-window app toplevel)
  (define new-window
    (x_window_setup (x_window_new (parse-gschemrc toplevel))))

  (x_window_open_page
   (x_window_create_main app
                         new-window
                         (make-main-menu new-window)
                         *process-key-event)
   %null-pointer))

(define-action-public (&file-new-window #:label (G_ "New Window") #:icon "window-new")
  (make-schematic-window (lepton_schematic_app)
                         (lepton_toplevel_new)))

(define-action-public (&file-close-window #:label (G_ "Close Window") #:icon "gtk-close")
  (log! 'message (G_ "Closing Window"))
  (x_window_close (*current-window)))

(define-action-public (&file-quit #:label (G_ "Quit") #:icon "gtk-quit")
  (lepton-repl-save-history)
  (x_window_close_all (*current-window)))

(define-action-public (&file-repl #:label (G_ "Terminal REPL") #:icon "gtk-execute")
  (start-repl-in-background-terminal))

;; -------------------------------------------------------------------
;;;; General editing actions

(define-action-public (&edit-undo #:label (G_ "Undo") #:icon "gtk-undo")
  (run-callback i_callback_edit_undo "&edit-undo"))

(define-action-public (&edit-redo #:label (G_ "Redo") #:icon "gtk-redo")
  (run-callback i_callback_edit_redo "&edit-redo"))

(define-action-public (&edit-select #:label (G_ "Select Mode") #:icon "select")
  (run-callback i_callback_edit_select "&edit-select"))

(define-action-public (&edit-select-all #:label (G_ "Select All") #:icon "gtk-select-all")
  (run-callback i_callback_edit_select_all "&edit-select-all"))

(define-action-public (&edit-deselect #:label (G_ "Deselect"))
  (run-callback i_callback_edit_deselect "&edit-deselect"))

(define-action-public (&edit-delete #:label (G_ "Delete") #:icon "gtk-delete")
  (run-callback i_callback_edit_delete "&edit-delete"))

(define-action-public (&edit-move #:label (G_ "Move Mode"))
  (run-callback i_callback_edit_move "&edit-move"))

(define-action-public (&edit-copy #:label (G_ "Copy Mode") #:icon "clone")
  (run-callback i_callback_edit_copy "&edit-copy"))

(define-action-public (&edit-mcopy #:label (G_ "Multiple Copy Mode") #:icon "multi-clone")
  (run-callback i_callback_edit_mcopy "&edit-mcopy"))

(define-action-public (&edit-rotate-90 #:label (G_ "Rotate Mode") #:icon "object-rotate-left")
  (run-callback i_callback_edit_rotate_90 "&edit-rotate-90"))

(define-action-public (&edit-mirror #:label (G_ "Mirror Mode") #:icon "object-flip-horizontal")
  (run-callback i_callback_edit_mirror "&edit-mirror"))

(define-action-public (&edit-edit #:label (G_ "Edit..."))
  (run-callback i_callback_edit_edit "&edit-edit"))

(define-action-public (&edit-text #:label (G_ "Edit Text") #:icon "gtk-edit")
  (text_edit_dialog (*current-window)))

(define-action-public (&edit-slot #:label (G_ "Choose Slot"))
  (run-callback i_callback_edit_slot "&edit-slot"))

;;; Show "object properties" widget.
(define-action-public (&edit-object-properties #:label (G_ "Edit Object Properties") #:icon "gtk-properties")
  (x_widgets_show_object_properties (*current-window)))

(define-action-public (&edit-translate #:label (G_ "Translate Symbol"))
  (run-callback i_callback_edit_translate "&edit-translate"))

(define-action-public (&edit-lock #:label (G_ "Lock"))
  (run-callback i_callback_edit_lock "&edit-lock"))

(define-action-public (&edit-unlock #:label (G_ "Unlock"))
  (run-callback i_callback_edit_unlock "&edit-unlock"))

(define-action-public (&edit-invoke-macro #:label (G_ "Invoke Macro"))
  (run-callback i_callback_edit_invoke_macro "&edit-invoke-macro"))

(define-action-public (&edit-embed #:label (G_ "Embed Component/Picture"))
  (run-callback i_callback_edit_embed "&edit-embed"))

(define-action-public (&edit-unembed #:label (G_ "Unembed Component/Picture"))
  (run-callback i_callback_edit_unembed "&edit-unembed"))

(define-action-public (&edit-update #:label (G_ "Update Component") #:icon "gtk-refresh")
  (run-callback i_callback_edit_update "&edit-update"))

(define-action-public (&edit-show-hidden #:label (G_ "Show/Hide Invisible Text"))
  (run-callback i_callback_edit_show_hidden "&edit-show-hidden"))

;; -------------------------------------------------------------------
;;;; Clipboard actions

(define-action-public (&clipboard-cut #:label (G_ "Cut") #:icon "gtk-cut")
  (run-callback i_callback_clipboard_cut "&clipboard-cut"))

(define-action-public (&clipboard-copy #:label (G_ "Copy") #:icon "gtk-copy")
  (run-callback i_callback_clipboard_copy "&clipboard-copy"))

(define-action-public (&clipboard-paste #:label (G_ "Paste") #:icon "gtk-paste")
  (run-callback i_callback_clipboard_paste "&clipboard-paste"))

;; -------------------------------------------------------------------
;;;; View control actions

(define-action-public (&view-sidebar #:label (G_ "Sidebar"))
  (run-callback i_callback_view_sidebar "&view-sidebar"))

(define-action-public (&view-status #:label (G_ "Status"))
  (run-callback i_callback_view_status "&view-status"))

;;; Show the find text state window.
(define-action-public (&view-find-text-state #:label (G_ "Find Text State"))
  (x_widgets_show_find_text_state (*current-window)))

;;; Redraw canvas.
(define-action-public (&view-redraw #:label (G_ "Redraw") #:icon "gtk-refresh")
  (gschem_page_view_invalidate_all
   (gschem_toplevel_get_current_page_view (*current-window))))

(define-action-public (&view-pan #:label (G_ "Pan"))
  (run-callback i_callback_view_pan "&view-pan"))

(define-action-public (&view-pan-left #:label (G_ "Pan Left"))
  (run-callback i_callback_view_pan_left "&view-pan-left"))

(define-action-public (&view-pan-right #:label (G_ "Pan Right"))
  (run-callback i_callback_view_pan_right "&view-pan-right"))

(define-action-public (&view-pan-up #:label (G_ "Pan Up"))
  (run-callback i_callback_view_pan_up "&view-pan-up"))

(define-action-public (&view-pan-down #:label (G_ "Pan Down"))
  (run-callback i_callback_view_pan_down "&view-pan-down"))

(define-action-public (&view-zoom-box #:label (G_ "Zoom Box"))
  (run-callback i_callback_view_zoom_box "&view-zoom-box"))

(define-action-public (&view-zoom-extents #:label (G_ "Zoom Extents") #:icon "gtk-zoom-fit")
  (run-callback i_callback_view_zoom_extents "&view-zoom-extents"))

(define-action-public (&view-zoom-in #:label (G_ "Zoom In") #:icon "gtk-zoom-in")
  (run-callback i_callback_view_zoom_in "&view-zoom-in"))

(define-action-public (&view-zoom-out #:label (G_ "Zoom Out") #:icon "gtk-zoom-out")
  (run-callback i_callback_view_zoom_out "&view-zoom-out"))

(define-action-public (&view-zoom-full #:label (G_ "Zoom Full"))
  (run-callback i_callback_view_zoom_full "&view-zoom-full"))

(define-action-public (&view-dark-colors #:label (G_ "Dark Color Scheme"))
  (run-callback i_callback_view_dark_colors "&view-dark-colors"))

(define-action-public (&view-light-colors #:label (G_ "Light Color Scheme"))
  (run-callback i_callback_view_light_colors "&view-light-colors"))

(define-action-public (&view-bw-colors #:label (G_ "Monochrome Color Scheme"))
  (run-callback i_callback_view_bw_colors "&view-bw-colors"))

(define-action-public (&view-color-edit #:label (G_ "Show Color Scheme Editor"))
  (run-callback i_callback_view_color_edit "&view-color-edit"))

;; -------------------------------------------------------------------
;;;; Page-related actions

(define-action-public (&page-revert #:label (G_ "Revert Changes") #:icon "gtk-revert-to-saved")
  (run-callback i_callback_page_revert "&page-revert"))

(define-action-public (&page-manager #:label (G_ "Page Manager"))
  (run-callback i_callback_page_manager "&page-manager"))

(define-action-public (&page-prev #:label (G_ "Previous Page") #:icon "gtk-go-back")
  (run-callback i_callback_page_prev "&page-prev"))

(define-action-public (&page-next #:label (G_ "Next Page") #:icon "gtk-go-forward")
  (run-callback i_callback_page_next "&page-next"))

(define-action-public (&page-close #:label (G_ "Close Page") #:icon "gtk-close")
  (run-callback i_callback_page_close "&page-close"))

(define-action-public (&page-next-tab #:label (G_ "Next Tab") #:icon "gtk-go-forward")
  (run-callback i_callback_page_next_tab "&page-next-tab"))

(define-action-public (&page-prev-tab #:label (G_ "Previous Tab") #:icon "gtk-go-back")
  (run-callback i_callback_page_prev_tab "&page-prev-tab"))

(define-action-public (&page-print #:label (G_ "Print Page") #:icon "gtk-print")
  (run-callback i_callback_page_print "&page-print"))

;; -------------------------------------------------------------------
;;;; Actions related to adding things

(define-action-public (&add-component #:label (G_ "Add Component") #:icon "insert-symbol")
  (run-callback i_callback_add_component "&add-component"))

(define-action-public (&add-attribute #:label (G_ "Add Attribute") #:icon "insert-attribute")
  (run-callback i_callback_add_attribute "&add-attribute"))

(define-action-public (&add-net #:label (G_ "Add Net") #:icon "insert-net")
  (run-callback i_callback_add_net "&add-net"))

(define-action-public (&add-bus #:label (G_ "Add Bus") #:icon "insert-bus")
  (run-callback i_callback_add_bus "&add-bus"))

(define-action-public (&add-text #:label (G_ "Add Text") #:icon "insert-text")
  (run-callback i_callback_add_text "&add-text"))

(define-action-public (&add-line #:label (G_ "Add Line") #:icon "insert-line")
  (run-callback i_callback_add_line "&add-line"))

(define-action-public (&add-path #:label (G_ "Add Path") #:icon "insert-path")
  (run-callback i_callback_add_path "&add-path"))

(define-action-public (&add-box #:label (G_ "Add Box") #:icon "insert-box")
  (run-callback i_callback_add_box "&add-box"))

(define-action-public (&add-circle #:label (G_ "Add Circle") #:icon "insert-circle")
  (run-callback i_callback_add_circle "&add-circle"))

(define-action-public (&add-arc #:label (G_ "Add Arc") #:icon "insert-arc")
  (run-callback i_callback_add_arc "&add-arc"))

(define-action-public (&add-pin #:label (G_ "Add Pin") #:icon "insert-pin")
  (run-callback i_callback_add_pin "&add-pin"))

(define-action-public (&add-picture #:label (G_ "Add Picture") #:icon "insert-image")
  (run-callback i_callback_add_picture "&add-picture"))

;; -------------------------------------------------------------------
;;;; Hierarchy actions

(define-action-public (&hierarchy-down-schematic #:label (G_ "Down Schematic") #:icon "gtk-go-down")
  (run-callback i_callback_hierarchy_down_schematic "&hierarchy-down-schematic"))

(define-action-public (&hierarchy-down-symbol #:label (G_ "Down Symbol") #:icon "gtk-goto-bottom")
  (run-callback i_callback_hierarchy_down_symbol "&hierarchy-down-symbol"))

(define-action-public (&hierarchy-up #:label (G_ "Up Hierarchy") #:icon "gtk-go-up")
  (run-callback i_callback_hierarchy_up "&hierarchy-up"))

;; -------------------------------------------------------------------
;;;; Attribute actions

( define-action-public
  ( &attributes-attach
    #:label (G_ "Attach Attributes")
    #:icon  "attribute-attach"
  )

  ( let*
    (
    ( page ( active-page ) )
    ( sel  ( if page (page-selection page) '() ) )
    )

    ( define ( can-attach-to? obj )
      ; return:
      ( and
        ( object? obj )
        ( not (text? obj) )
      )
    )

    ( define ( attachable-attr? obj )
      ; return:
      ( and
        ( attribute?    obj )           ; if it's attribute
        ( text-visible? obj )           ; if it's visible
        ( not (attrib-attachment obj) ) ; and does not already attached
      )
    )

    ( define ( attach-attr obj attr )
      ( attach-attribs! obj attr )
      ( log! 'message (G_ "Attribute attached: [~a]") (text-string attr) )
      ( deselect-object! attr )
    )


    ( let*
      (
      ( obj   (find   can-attach-to?   sel) )
      ( attrs (filter attachable-attr? sel) )
      ( attrs-not-empty ( not (null? attrs) ) )
      )

      ( when ( and obj attrs-not-empty )

        ( for-each
        ( lambda( attr )
          ( attach-attr obj attr )
        )
        attrs
        )

        ( deselect-object! obj )

        ( set-page-dirty! page )
        ( run-hook attach-attribs-hook attrs )
        ( undo-save-state )

      ) ; when

      ; return:
      attrs

    ) ; let

  ) ; let

) ; &attributes-attach action



( define-action-public
  (
    &attributes-detach
    #:label (G_ "Detach Attributes")
    #:icon "attribute-detach"
  )

  ( let*
    (
    ( page ( active-page ) )
    ( sel  ( if page (page-selection page) '() ) )
    )

    ( define ( detachable-attr? obj ) ; predicate
      ; return:
      ( and
        ( attribute?        obj ) ; if it's attribute
        ( text-visible?     obj ) ; if it's visible
        ( attrib-attachment obj ) ; and attached to some object
      )
    )

    ( define ( detach-attr attr )
      ( detach-attribs! (attrib-attachment attr) attr )
      ( log! 'message (G_ "Attribute detached: [~a]") (text-string attr) )
      ( deselect-object! attr )
    )


    ( let
      (
      ( attrs (filter detachable-attr? sel) )
      )

      ( unless (null? attrs)
        ( for-each detach-attr attrs )
        ( set-page-dirty! page )
        ( run-hook detach-attribs-hook attrs )
        ( undo-save-state )
      )

      ; return:
      attrs
    )

  ) ; let*

) ; &attributes-detach action



(define-action-public (&attributes-show-value #:label (G_ "Show Attribute Value") #:icon "attribute-show-value")
  (run-callback i_callback_attributes_show_value "&attributes-show-value"))

(define-action-public (&attributes-show-name #:label (G_ "Show Attribute Name") #:icon "attribute-show-name")
  (run-callback i_callback_attributes_show_name "&attributes-show-name"))

(define-action-public (&attributes-show-both #:label (G_ "Show Name & Value") #:icon "attribute-show-both")
  (run-callback i_callback_attributes_show_both "&attributes-show-both"))

(define-action-public (&attributes-visibility-toggle #:label (G_ "Toggle Text Visibility"))
  (run-callback i_callback_attributes_visibility_toggle "&attributes-visibility-toggle"))

(define-action-public (&edit-find-text #:label (G_ "Find Specific Text") #:icon "gtk-find")
  (run-callback i_callback_edit_find "&edit-find-text"))

(define-action-public (&edit-hide-text #:label (G_ "Hide Specific Text"))
  (run-callback i_callback_edit_hide_text "&edit-hide-text"))

(define-action-public (&edit-show-text #:label (G_ "Show Specific Text"))
  (run-callback i_callback_edit_show_text "&edit-show-text"))

(define-action-public (&edit-autonumber #:label (G_ "Autonumber Text"))
  (run-callback i_callback_edit_autonumber_text "&edit-autonumber"))

;; -------------------------------------------------------------------
;;;; Configuration actions

(define-action-public (&help-hotkeys #:label (G_ "Show Hotkeys") #:icon "preferences-desktop-keyboard-shortcuts")
  (run-callback i_callback_help_hotkeys "&help-hotkeys"))

(define-action-public (&options-grid #:label (G_ "Switch Grid Style"))
  (run-callback i_callback_options_grid "&options-grid"))

(define-action-public (&options-snap #:label (G_ "Switch Snap Mode"))
  (run-callback i_callback_options_snap "&options-snap"))

(define-action-public (&options-snap-size #:label (G_ "Set Grid Spacing"))
  (run-callback i_callback_options_snap_size "&options-snap-size"))

(define-action-public (&options-scale-up-snap-size #:label (G_ "Increase Grid Spacing"))
  (run-callback i_callback_options_scale_up_snap_size "&options-scale-up-snap-size"))

(define-action-public (&options-scale-down-snap-size #:label (G_ "Decrease Grid Spacing"))
  (run-callback i_callback_options_scale_down_snap_size "&options-scale-down-snap-size"))

(define-action-public (&options-action-feedback #:label (G_ "Toggle Outline Drawing"))
  (run-callback i_callback_options_afeedback "&options-action-feedback"))

(define-action-public (&options-rubberband #:label (G_ "Toggle Net Rubber Band"))
  (run-callback i_callback_options_rubberband "&options-rubberband"))

(define-action-public (&options-magneticnet #:label (G_ "Toggle Magnetic Nets"))
  (run-callback i_callback_options_magneticnet "&options-magneticnet"))

(define-action-public (&options-show-log-window #:label (G_ "Show Log Window"))
  (run-callback i_callback_options_show_log_window "&options-show-log-window"))

(define-action-public (&options-show-coord-window #:label (G_ "Show Coordinate Window"))
  (coord_dialog (*current-window) 0 0))

(define-action-public (&options-select-font #:label (G_ "Select Schematic Font"))
  (x_widgets_show_font_select (*current-window)))

(define-action-public (&options-draw-grips #:label (G_ "Toggle Grips"))
  (run-callback i_callback_options_draw_grips "&options-draw-grips"))

;; -------------------------------------------------------------------
;;;; Documentation-related actions

(define-action-public
    (&hierarchy-documentation #:label (G_ "Component Documentation")
                              #:icon "symbol-datasheet"
                              #:tooltip (G_ "View documentation for selected component"))

  "If a component is selected, search for and display corresponding
documentation in a browser or PDF viewer. If no documentation can be
found, shows a dialog with an error message."

     (let ((component
            (any (lambda (obj) (and (component? obj) obj))
                 (page-selection (active-page)))))
       (and component (show-component-documentation component))))


(define-action-public
    (&help-manual #:label (G_ "Lepton EDA Manual") #:icon "help-browser"
     #:tooltip (G_ "View the main page of the Lepton EDA Reference Manual in a browser."))
  (show-manual))


(define-action-public
    (&help-wiki #:label (G_ "Lepton EDA wiki") #:icon "web-browser"
     #:tooltip (G_ "View the front page of the Lepton EDA wiki in a browser."))
  (show-wiki))


(define-action-public (&help-about #:label (G_ "About lepton-schematic") #:icon "gtk-about")
  (run-callback i_callback_help_about "&help-about"))


; Backward compatibility:
;
(define &edit-color    &edit-object-properties) (export &edit-color)
(define &edit-linetype &edit-object-properties) (export &edit-linetype)
(define &edit-filltype &edit-object-properties) (export &edit-filltype)
(define &edit-pin-type &edit-object-properties) (export &edit-pin-type)

;; Local Variables:
;; eval: (put 'define-action-public 'scheme-indent-function 1)
;; End:
