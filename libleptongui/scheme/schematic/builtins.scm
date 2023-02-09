;; Lepton EDA Schematic Capture
;; Scheme API
;; Copyright (C) 2013 Peter Brett <peter@peter-b.co.uk>
;; Copyright (C) 2013-2015 gEDA Contributors
;; Copyright (C) 2017-2023 Lepton EDA Contributors
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
  #:use-module (ice-9 match)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)
  #:use-module (system foreign)

  #:use-module (lepton attrib)
  #:use-module (lepton ffi boolean)
  #:use-module (lepton ffi glib)
  #:use-module (lepton ffi)
  #:use-module (lepton gerror)
  #:use-module (lepton log)
  #:use-module (lepton object foreign)
  #:use-module (lepton object)
  #:use-module (lepton page foreign)
  #:use-module (lepton page)
  #:use-module (lepton rc)
  #:use-module (lepton repl)

  #:use-module (schematic action copy)
  #:use-module (schematic action)
  #:use-module (schematic action-mode)
  #:use-module (schematic buffer)
  #:use-module (schematic callback)
  #:use-module (schematic gettext)
  #:use-module (schematic ffi)
  #:use-module (schematic dialog)
  #:use-module (schematic dialog file-select)
  #:use-module (schematic dialog slot-edit)
  #:use-module (schematic doc)
  #:use-module (schematic gui keymap)
  #:use-module (schematic hook)
  #:use-module (schematic menu)
  #:use-module (schematic repl)
  #:use-module (schematic selection)
  #:use-module (schematic undo)
  #:use-module (schematic window global)
  #:use-module (schematic window foreign)
  #:use-module (schematic window list)
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


;; -------------------------------------------------------------------
;;;; Special actions

(define-action-public (&cancel #:label (G_ "Cancel"))
  (run-callback i_callback_cancel "&cancel"))

;; -------------------------------------------------------------------
;;;; File menu actions
(define-action-public (&file-new #:label (G_ "New File") #:icon "gtk-new")
  (callback-file-new %null-pointer (*current-window)))


(define-action-public (&file-open #:label (G_ "Open File") #:icon "gtk-open")
  (file-select-dialog (current-window)))

(define-action-public (&file-save #:label (G_ "Save") #:icon "gtk-save")
  (run-callback i_callback_file_save "&file-save"))

(define-action-public (&file-save-as #:label (G_ "Save As") #:icon "gtk-save-as")
  (define *window (*current-window))
  (x_fileselect_save *window
                     (schematic_window_get_active_page *window)
                     %null-pointer))

;;; Save all opened pages.
(define-action-public (&file-save-all #:label (G_ "Save All") #:icon "gtk-save")
  (define *window (*current-window))

  (define (save-page! page)
    (let ((*page (page->pointer page)))
      (if (true? (x_window_untitled_page *page))
          ;; For untitled pages, open "Save as..." dialog.
          (let ((bv (make-bytevector (sizeof int) 0)))
            ;; Skip the result if the File save dialog has been cancelled.
            (or (not (true? (x_fileselect_save *window *page (bytevector->pointer bv))))
                ;; Otherwise, get the result of the save operation.
                (true? (bytevector-sint-ref bv 0 (native-endianness) (sizeof int)))))

          ;; Simply save any other page.
          (true? (x_window_save_page *window
                                     *page
                                     (string->pointer (page-filename page)))))

      ;; Update tab view.
      (when (true? (x_tabs_enabled))
        (x_tabs_hdr_update *window *page))))

  (define (save-all-pages)
    (map save-page! (active-pages)))

  (define all-saved?
    (every identity (save-all-pages)))

  ;; Update statusbar once.
  (i_set_state_msg *window
                   (symbol->action-mode 'select-mode)
                   (string->pointer (if all-saved?
                                        (G_ "Saved All")
                                        (G_ "Failed to Save All"))))

  ;; Update Page select widget.
  (page_select_widget_update *window)
  ;; Update sensitivity of menu items.
  (i_update_menus *window))


(define-action-public (&file-print #:label (G_ "Print") #:icon "gtk-print")
  (x_print (*current-window)))

(define-action-public (&file-image #:label (G_ "Export Image"))
  (x_image_setup (*current-window)))


(define-action-public (&file-script #:label (G_ "Run Script") #:icon "gtk-execute")
  (define *window (*current-window))
  (define *filename (schematic_execute_script (*current-window)))

  (unless (null-pointer? *filename)
    (log! 'message (G_ "Executing Guile script: ~S") (pointer->string *filename))
    (g_read_file (gschem_toplevel_get_toplevel *window)
                 *filename
                 %null-pointer)
    (g_free *filename)))


(define-action-public (&file-new-window #:label (G_ "New Window") #:icon "window-new")
  (window-open-page!
   (make-schematic-window (lepton_schematic_app)
                          (lepton_toplevel_new))
   #f))

(define-action-public (&file-close-window #:label (G_ "Close Window") #:icon "gtk-close")
  (log! 'message (G_ "Closing Window"))
  (close-window! (pointer->window (*current-window))))


(define-action-public (&file-quit #:label (G_ "Quit") #:icon "gtk-quit")
  (lepton-repl-save-history)
  (for-each close-window! (schematic-windows)))


(define-action-public (&file-repl #:label (G_ "Terminal REPL") #:icon "gtk-execute")
  (start-repl-in-background-terminal))

;; -------------------------------------------------------------------
;;;; General editing actions

(define-action-public (&edit-undo #:label (G_ "Undo") #:icon "gtk-undo")
  (undo!))

(define-action-public (&edit-redo #:label (G_ "Redo") #:icon "gtk-redo")
  (redo!))

(define-action-public (&edit-select #:label (G_ "Select Mode") #:icon "select")
  (callback-edit-select (*current-window)))

;;; Select all objects on the active page.
;;; The action clears any existing selection, then selects
;;; everything visible and unlocked on the current page, and any
;;; attached attributes whether visible or invisible.
(define-action-public (&edit-select-all #:label (G_ "Select All")
                                        #:icon "gtk-select-all")
   (define (select-visible-and-selectable! object)
     ;; Skip invisible text objects and locked objects.
     (unless (or (and (text? object)
                      (not (text-visible? object))
                      (not show-hidden-text?))
                 (not (object-selectable? object)))
       ;; Add object to selection.
       (select-object! object)
       ;; Add any attributes of object to selection as well.
       (for-each select-object! (object-attribs object))))

  (define *window (*current-window))
  (define show-hidden-text?
    (true? (gschem_toplevel_get_show_hidden_text *window)))

  (o_redraw_cleanstates *window)

  (o_select_unselect_all *window)

  (for-each select-visible-and-selectable!
            (page-contents (active-page)))

  ;; Run hooks for all items selected.
  (let ((new-selection (page-selection (active-page))))
    (unless (null? new-selection)
      (run-hook select-objects-hook new-selection)))

  (set-action-mode! 'select-mode)
  (i_action_stop *window)
  (i_update_menus *window))


;;; Deselect all objects on the active page.
(define-action-public (&edit-deselect #:label (G_ "Deselect"))
  (define *window (*current-window))

  (o_redraw_cleanstates *window)

  (o_select_unselect_all *window)

  (set-action-mode! 'select-mode)
  (i_action_stop *window)
  (i_update_menus *window))


(define-action-public (&edit-delete #:label (G_ "Delete") #:icon "gtk-delete")
  (define *window (*current-window))

  (unless (null? (page-selection (active-page)))
    (o_redraw_cleanstates *window)
    (o_delete_selected *window)
    ;; If you delete the objects you must go into select mode
    ;; after the delete.
    (i_action_stop *window)
    (set-action-mode! 'select-mode)
    (i_update_menus *window)))


(define-action-public (&edit-move #:label (G_ "Move Mode"))
  (define *window (*current-window))

  (if (null? (page-selection (active-page)))
      (i_set_state_msg *window
                       (symbol->action-mode 'select-mode)
                       (string->pointer (G_ "Select objs first")))
      (let ((position (action-position)))
        (o_redraw_cleanstates *window)
        (and position
             (match (snap-point position)
               ((x . y) (o_move_start *window x y))
               (_ #f)))
        (set-action-mode! 'move-mode))))


(define-action-public (&edit-copy #:label (G_ "Copy Mode") #:icon "clone")
  (define *window (*current-window))

  (if (null? (page-selection (active-page)))
      (i_set_state_msg *window
                       (symbol->action-mode 'select-mode)
                       (string->pointer (G_ "Select objs first")))
      (let ((position (action-position)))
        (o_redraw_cleanstates *window)
        (and position
             (match (snap-point position)
               ((x . y) (start-copy *window x y))
               (_ #f)))
        (set-action-mode! 'copy-mode))))


(define-action-public (&edit-mcopy #:label (G_ "Multiple Copy Mode") #:icon "multi-clone")
  (define *window (*current-window))

  (if (null? (page-selection (active-page)))
      (i_set_state_msg *window
                       (symbol->action-mode 'select-mode)
                       (string->pointer (G_ "Select objs first")))
      (let ((position (action-position)))
        (o_redraw_cleanstates *window)
        (and position
             (match (snap-point position)
               ((x . y) (start-copy *window x y))
               (_ #f)))
        (set-action-mode! 'multiple-copy-mode))))


;;; Rotate all objects in the place list or in the selection list,
;;; if the former is empty, by 90 degrees.
(define-action-public (&edit-rotate-90 #:label (G_ "Rotate Mode") #:icon "object-rotate-left")
  (define *window (*current-window))
  (define *page (and=> (active-page) page->pointer))

  (when *page
    (if (and (in-action?)
             (not (null-pointer? (schematic_window_get_place_list *window))))
        ;; If there are objects to place, rotate them.
        (o_place_rotate *window)
        ;; Otherwise, rotate the current selection, if any.
        (let ((position (action-position)))
          (if position
              ;; Mouse pointer is over the canvas: rotate selection.
              (let ((*objects (lepton_list_get_glist
                               (schematic_window_get_selection_list *window))))
                (o_redraw_cleanstates *window)
                (unless (null-pointer? *objects)
                  (match (snap-point position)
                    ((x . y) (o_rotate_world_update *window x y 90 *objects))
                    (_ #f))))
              ;; Mouse pointer is out of the canvas: just set the
              ;; rotation mode.
              (set-action-mode! 'rotate-mode))))))


;;; Mirror objects in the place list or in the selection list if
;;; the former is empty.
(define-action-public (&edit-mirror #:label (G_ "Mirror Mode") #:icon "object-flip-horizontal")
  (define *window (*current-window))
  (define *page (and=> (active-page) page->pointer))

  (when *page
    (if (and (in-action?)
             (not (null-pointer? (schematic_window_get_place_list *window))))
        ;; If there are objects to place, mirror them.
        (o_place_mirror *window)
        ;; Otherwise, mirror the current selection, if any.
        (let ((position (action-position)))
          (if position
              ;; Mouse pointer is over the canvas: mirror selection.
              (let ((*objects (lepton_list_get_glist
                               (schematic_window_get_selection_list *window))))
                (o_redraw_cleanstates *window)
                (unless (null-pointer? *objects)
                  (match (snap-point position)
                    ((x . y) (o_mirror_world_update *window x y *objects))
                    (_ #f))))
              ;; Mouse pointer is out of the canvas: just set the
              ;; mirror mode.
              (set-action-mode! 'mirror-mode))))))


(define-action-public (&edit-edit #:label (G_ "Edit..."))
  (define *window (*current-window))
  (define *selection (schematic_window_get_selection_list *window))
  (o_edit *window (lepton_list_get_glist *selection)))


(define-action-public (&edit-text #:label (G_ "Edit Text") #:icon "gtk-edit")
  (text_edit_dialog (*current-window)))

(define-action-public (&edit-slot #:label (G_ "Choose Slot"))
  (match (filter component? (page-selection (active-page)))
    ((a b . c) (schematic-message-dialog (G_ "Please select only one component!")))
    ((component) (slot-edit-dialog (*current-window) component))
    (_ (schematic-message-dialog (G_ "Please first select a component!")))))

;;; Show "object properties" widget.
(define-action-public (&edit-object-properties #:label (G_ "Edit Object Properties") #:icon "gtk-properties")
  (x_widgets_show_object_properties (*current-window)))


(define-action-public (&edit-translate #:label (G_ "Translate Symbol"))
  (define *window (*current-window))
  (define *options (schematic_window_get_options *window))
  (define snap-mode
    (string->symbol
     (pointer->string
      (schematic_snap_mode_to_string
       (gschem_options_get_snap_mode *options)))))

  (when (eq? snap-mode 'off)
    (log! 'message (G_ "WARNING: Do not translate with snap off!"))
    (log! 'message (G_ "WARNING: Turning snap on and continuing with translate."))
    (gschem_options_set_snap_mode *options
                                  (schematic_snap_mode_from_string (string->pointer "grid")))
    ;; Update status on screen.
    (i_show_state *window %null-pointer))

  (when (not (= 100 (gschem_options_get_snap_size *options)))
    (log! 'message (G_ "WARNING: Snap grid size is not equal to 100!"))
    (log! 'message (G_ "WARNING: If you are translating a symbol to the origin,
the snap grid size should be set to 100")))

  (schematic_window_show_translate_widget *window))


;;; Lock all objects in selection list.
;;; The function locks the entire selected list. It does lock
;;; components, but does NOT change the color of primitives of the
;;; components.
(define-action-public (&edit-lock #:label (G_ "Lock"))
  (define *window (*current-window))
  (define (lock-object! object)
    (set-object-selectable! object #f))
  (define (lock-object-with-attribs! object)
    (lock-object! object)
    (for-each lock-object! (object-attribs object)))

  ;; Lock selected objects with their attributes.
  (for-each lock-object-with-attribs! (page-selection (active-page)))

  ;; Apart from setting the current page as changed, the function
  ;; updates the Page manager.
  (schematic_window_active_page_changed *window)

  (unless (true? (schematic_window_get_shift_key_pressed *window))
    (o_select_unselect_all *window))

  (undo-save-state)
  (i_update_menus *window)

  ;; Refresh page view to properly restore attributes' colors.
  (gschem_page_view_invalidate_all
   (gschem_toplevel_get_current_page_view *window)))


;;; Unlock all objects in selection list.
;;; The function unlocks currenly selected objects.  Locked
;;; objects can be selected with a bounding box.
(define-action-public (&edit-unlock #:label (G_ "Unlock"))
  (define *window (*current-window))
  (define (unlock-object! object)
    (set-object-selectable! object #t))
  (define (unlock-object-with-attribs! object)
    (unlock-object! object)
    (for-each unlock-object! (object-attribs object)))

  ;; Unlock selected objects with their attributes.
  (for-each unlock-object-with-attribs! (page-selection (active-page)))

  ;; Apart from setting the current page as changed, the function
  ;; updates the Page manager.
  (schematic_window_active_page_changed *window)

  (undo-save-state)

  ;; Refresh page view to properly restore attributes' colors.
  (gschem_page_view_invalidate_all
   (gschem_toplevel_get_current_page_view *window)))


(define-action-public (&edit-invoke-macro #:label (G_ "Invoke Macro"))
  (macro_widget_show (schematic_window_get_macro_widget (*current-window))))


;;; Embed all objects in the current selection list.
(define-action-public (&edit-embed #:label (G_ "Embed Component/Picture"))
  (define *window (*current-window))

  (define (embed! object)
    (set-object-embedded! object #t))

  (let ((selection (page-selection (active-page))))
    ;; Is anything selected?
    (if (null? selection)
        ;; Nothing selected, go back to select state.
        (begin
          (o_redraw_cleanstates *window)
          (i_action_stop *window)
          (set-action-mode! 'select-mode))

        ;; Embed all selected components and pictures.
        (begin
          (for-each embed! selection)
          (undo-save-state)
          (page_select_widget_update *window)))))


;;; Unembed all objects in the current selection list.
(define-action-public (&edit-unembed #:label (G_ "Unembed Component/Picture"))
  (define *window (*current-window))

  (define (unembed! object)
    (set-object-embedded! object #f))

  (let ((selection (page-selection (active-page))))
    ;; Is anything selected?
    (if (null? selection)
        ;; Nothing selected, go back to select state.
        (begin
          (o_redraw_cleanstates *window)
          (i_action_stop *window)
          (set-action-mode! 'select-mode))

        ;; Unembed all selected components and pictures.
        (begin
          (for-each unembed! selection)
          (undo-save-state)
          (page_select_widget_update *window)))))


;;; Update a component.
(define-action-public (&edit-update #:label (G_ "Update Component")
                                    #:icon "gtk-refresh")
  ;; Update component to the latest version of the symbol
  ;; available in the symbol library, while preserving any
  ;; attributes set in the current schematic. On success, the
  ;; component is deleted on its page and replaced with a new
  ;; one. On failure, the current component is left unchanged.
  (define (update-component *window current-component)
    (let ((basename (component-basename current-component))
          (page (object-page current-component))
          (position (component-position current-component))
          (angle (component-angle current-component))
          (mirror? (component-mirror? current-component))
          (locked? (component-locked? current-component))
          (embedded? (object-embedded? current-component))
          (attribs (object-attribs current-component)))

      ;; Force symbol data to be reloaded from source
      (s_clib_symbol_invalidate_data
       (s_clib_get_symbol_by_name (string->pointer basename)))

      ;; Create new object.
      (let ((library-component (make-component/library basename
                                                       position
                                                       angle
                                                       mirror?
                                                       locked?)))
        (if library-component
            (let ((new-component
                   (set-component-with-transform! library-component
                                                  position
                                                  angle
                                                  mirror?
                                                  locked?)))
              ;; Embed new object if the old one is embedded.
              (set-object-embedded! new-component embedded?)

              ;; Unselect the old object.
              (deselect-object! current-component)

              ;; Detach attributes from old component.
              (apply detach-attribs! current-component attribs)

              ;; Replace the old component with the new one.
              (page-remove! page current-component)
              (page-append! page new-component)

              ;; Promote new attributes.
              (let ((new-attribs (promote-attribs! new-component))
                    (current-attrib-names (map attrib-name attribs)))
                ;; Remove any attributes from the new attribute
                ;; list that are already attached to the old
                ;; component.  The attributes are matched by name.
                (for-each
                 (lambda (attrib)
                   (let ((name (attrib-name attrib)))
                     (when (member (attrib-name attrib) current-attrib-names)
                       (detach-attribs! new-component attrib)
                       (page-remove! (object-page attrib) attrib))))
                 new-attribs)

                ;; Attach old attributes to the new component.
                (apply attach-attribs! new-component attribs)

                ;; Update pinnumbers for current slot
                (s_slot_update_object (object->pointer new-component))

                ;; Select the new component.
                (select-object! new-component)

                ;; Mark the page as modified.
                (gschem_toplevel_page_content_changed *window (page->pointer page))

                ;; Register changes for undo.
                (undo-save-state)))

            ;; If component not found, warn the user.
            (log! 'message
                  (G_ "Could not find symbol ~S in library. Update failed.")
                  basename)))))

  (define *window (*current-window))

  (define selected-components (filter component? (page-selection (active-page))))

  ;; Update only selected components.
  (if (null? selected-components)
      (begin
        ;; Nothing selected, go back to select state.
        (o_redraw_cleanstates *window)
        (i_action_stop *window)
        (set-action-mode! 'select-mode))
      (for-each
       (lambda (component) (update-component *window component))
       selected-components)))


(define-action-public (&edit-show-hidden #:label (G_ "Show/Hide Invisible Text"))
  (define *window (*current-window))
  (define *page (and=> (active-page) page->pointer))
  (when *page
    (o_edit_show_hidden *window
                        (lepton_page_objects *page))))


;; -------------------------------------------------------------------
;;;; Clipboard actions

(define CLIPBOARD_BUFFER 0)

(define (buffer-paste-start *window buffer-number)
  ;; Choose a default position to start pasting. This is required to
  ;; make pasting when the cursor is outside the screen or pasting via
  ;; menu work as expected.
  (let ((position (action-position)))
    (and position
         (match (snap-point position)
           ((x . y)
            (o_redraw_cleanstates *window)
            ;; Cancel current place or draw action if it is being
            ;; done.
            (when (in-action? (pointer->window *window))
              (i_callback_cancel %null-pointer *window))
            (let ((empty? (paste-buffer (pointer->window *window)
                                        (cons x y)
                                        buffer-number)))
              (when empty?
                (i_set_state_msg *window
                                 (symbol->action-mode 'select-mode)
                                 (string->pointer (G_ "Empty clipboard"))))))
           (_ #f)))))

;;; Cut the current selection to the clipboard, via buffer 0.
(define-action-public (&clipboard-cut #:label (G_ "Cut") #:icon "gtk-cut")
  (selection->buffer (current-window) CLIPBOARD_BUFFER 'cut))

;;; Copies the current selection to the clipboard, via buffer 0.
(define-action-public (&clipboard-copy #:label (G_ "Copy") #:icon "gtk-copy")
  (selection->buffer (current-window) CLIPBOARD_BUFFER))

;;; Start pasting the current clipboard contents, via buffer 0.
(define-action-public (&clipboard-paste #:label (G_ "Paste") #:icon "gtk-paste")
  (buffer-paste-start (*current-window) CLIPBOARD_BUFFER))

;; -------------------------------------------------------------------
;;;; View control actions

;;; Toggle the visibility of the sidebar.
(define-action-public (&view-sidebar #:label (G_ "Sidebar"))
  (x_widgets_toggle_widget_visibility
   (schematic_window_get_right_notebook (*current-window))))


;;; Toggle the visibility of the status window.
(define-action-public (&view-status #:label (G_ "Status"))
  (x_widgets_toggle_widget_visibility
   (schematic_window_get_bottom_notebook (*current-window))))


;;; Show the find text state window.
(define-action-public (&view-find-text-state #:label (G_ "Find Text State"))
  (x_widgets_show_find_text_state (*current-window)))

;;; Redraw canvas.
(define-action-public (&view-redraw #:label (G_ "Redraw") #:icon "gtk-refresh")
  (gschem_page_view_invalidate_all
   (gschem_toplevel_get_current_page_view (*current-window))))


(define-action-public (&view-pan #:label (G_ "Pan"))
  (define *window (*current-window))

  (match (action-position)
    ((x . y)
     (gschem_page_view_pan (gschem_toplevel_get_current_page_view *window) x y)
     (when (true? (schematic_window_get_undo_panzoom *window))
       (o_undo_savestate_viewport *window)))
    (_
     (o_redraw_cleanstates *window)
     (i_action_stop *window)
     (set-action-mode! 'pan-mode))))


;;; Viewport moving.  The distance can be set with the
;;; [schematic.gui]::keyboardpan-gain configuration setting.

;;; Moves the viewport to the left.
(define-action-public (&view-pan-left #:label (G_ "Pan Left"))
  (define *window (*current-window))
  (gschem_page_view_pan_mouse (gschem_toplevel_get_current_page_view *window)
                              (schematic_window_get_keyboardpan_gain *window)
                              0))


;;; Moves the viewport to the right.
(define-action-public (&view-pan-right #:label (G_ "Pan Right"))
  (define *window (*current-window))
  (gschem_page_view_pan_mouse (gschem_toplevel_get_current_page_view *window)
                              (- (schematic_window_get_keyboardpan_gain *window))
                              0))


;;; Moves the viewport up.
(define-action-public (&view-pan-up #:label (G_ "Pan Up"))
  (define *window (*current-window))
  (gschem_page_view_pan_mouse (gschem_toplevel_get_current_page_view *window)
                              0
                              (schematic_window_get_keyboardpan_gain *window)))


;;; Moves the viewport down.
(define-action-public (&view-pan-down #:label (G_ "Pan Down"))
  (define *window (*current-window))
  (gschem_page_view_pan_mouse (gschem_toplevel_get_current_page_view *window)
                              0
                              (- (schematic_window_get_keyboardpan_gain *window))))


;;; Definitions from "gschem_defines.h".
(define DONTCARE 0)
(define MENU 1)
(define HOTKEY 2)
(define ZOOM_OUT 0)
(define ZOOM_IN 1)
(define ZOOM_FULL 2)


(define-action-public (&view-zoom-box #:label (G_ "Zoom Box"))
  (define *window (*current-window))

  (o_redraw_cleanstates *window)

  (set-action-mode! 'zoom-box-mode)

  (match (action-position)
    ((x . y) (a_zoom_box_start *window x y))
    (_ #f)))


(define-action-public (&view-zoom-extents #:label (G_ "Zoom Extents") #:icon "gtk-zoom-fit")
  (define *window (*current-window))

  (gschem_page_view_zoom_extents (gschem_toplevel_get_current_page_view *window)
                                 %null-pointer)

  (if (true? (schematic_window_get_undo_panzoom *window))
      (o_undo_savestate_viewport *window)))


(define-action-public (&view-zoom-in #:label (G_ "Zoom In") #:icon "gtk-zoom-in")
  (define *window (*current-window))

  (a_zoom *window
          (gschem_toplevel_get_current_page_view *window)
          ZOOM_IN
          (match (action-position)
            ((x . y) HOTKEY)
            (_ MENU)))

  (when (true? (schematic_window_get_undo_panzoom *window))
    (o_undo_savestate_viewport *window)))


(define-action-public (&view-zoom-out #:label (G_ "Zoom Out") #:icon "gtk-zoom-out")
  (define *window (*current-window))

  (a_zoom *window
          (gschem_toplevel_get_current_page_view *window)
          ZOOM_OUT
          (match (action-position)
            ((x . y) HOTKEY)
            (_ MENU)))

  (when (true? (schematic_window_get_undo_panzoom *window))
    (o_undo_savestate_viewport *window)))


(define-action-public (&view-zoom-full #:label (G_ "Zoom Full"))
  (define *window (*current-window))
  (define *page-view (gschem_toplevel_get_current_page_view *window))

  (a_zoom *window *page-view ZOOM_FULL DONTCARE)

  (when (true? (schematic_window_get_undo_panzoom *window))
    (o_undo_savestate_viewport *window)))


(define (load-color-scheme *window color-scheme)
  (load-rc-from-sys-config-dirs color-scheme)

  (x_colorcb_update_colors)
  (color_edit_widget_update *window)

  (gschem_page_view_invalidate_all
   (gschem_toplevel_get_current_page_view *window)))


;;; Load the Dark color scheme.
(define-action-public (&view-dark-colors #:label (G_ "Dark Color Scheme"))
  (load-color-scheme (*current-window) "gschem-colormap-darkbg"))

(define-action-public (&view-light-colors #:label (G_ "Light Color Scheme"))
  (load-color-scheme (*current-window) "gschem-colormap-lightbg"))

(define-action-public (&view-bw-colors #:label (G_ "Monochrome Color Scheme"))
  (load-color-scheme (*current-window) "gschem-colormap-bw"))


;;; Show color scheme editor widget.
(define-action-public (&view-color-edit #:label (G_ "Show Color Scheme Editor"))
  (x_widgets_show_color_edit (*current-window)))


;; -------------------------------------------------------------------
;;;; Page-related actions

(define-action-public (&page-revert #:label (G_ "Revert Changes") #:icon "gtk-revert-to-saved")
  (define *window (*current-window))
  (define (untitled-page? page)
    (true? (x_window_untitled_page (page->pointer page))))

  (define filename (page-filename (active-page)))

  (when (and (not (untitled-page? (active-page)))
             (true? (schematic_page_revert_dialog *window (string->pointer filename))))

    (let ((*page_current (schematic_window_get_active_page *window))
          ;; If there's only one opened page, create a dummy page
          ;; (to be closed afterwards) to prevent
          ;; window-close-page!() from creating a stray blank
          ;; page.
          (*dummy-page (if (= 1 (length (active-pages)))
                           (page->pointer (window-open-page! (current-window) #f))
                           %null-pointer)))
      (unless (null-pointer? *dummy-page)
        (*window-set-current-page! *window *dummy-page)
        (*window-set-current-page! *window *page_current))

      (let ((page-control (lepton_page_get_page_control *page_current))
            (up (lepton_page_get_up *page_current)))

        ;; Delete the page then re-open the file as a new page.
        (window-close-page! (current-window) (active-page))

        ;; Force symbols to be re-loaded from disk.
        (s_clib_refresh)

        (let ((*page (page->pointer (window-open-page! (current-window) filename))))

          ;; Raise an error if the page cannot be reloaded for
          ;; some reason.
          (when (null-pointer? *page)
            (error "Could not open file ~S" filename))

          ;; Make sure we maintain the hierarchy info.
          (lepton_page_set_page_control *page page-control)
          (lepton_page_set_up *page up)

          (*window-set-current-page! *window *page)

          ;; Close dummy page, if it exists.
          (unless (null-pointer? *dummy-page)
            (*window-set-current-page! *window *dummy-page)
            (window-close-page! (current-window)
                                (pointer->page *dummy-page))
            (*window-set-current-page! *window *page))

          (when (true? (x_tabs_enabled))
            ;; Page hierarchy info was changed after the page is
            ;; opened; update tab's header (e.g. show/hide
            ;; "hierarchy up" button).
            (x_tabs_hdr_update *window *page)))))))


(define-action-public (&page-manager #:label (G_ "Page Manager"))
  (define *window (*current-window))
  (x_widgets_show_page_select *window)
  (page_select_widget_update *window))


;;; The function searches the next sibling of current page in the
;;; the given page list.
(define* (hierarchy-goto-nearest-page *window #:key (previous? #f))
  (define current-page (active-page))
  (define enforce-hierarchy? (true? (schematic_window_get_enforce_hierarchy *window)))
  (define (find-page ls)
    (and (not (null? ls))
         (let ((next-page (car ls)))
           (if enforce-hierarchy?
               (if (= (lepton_page_get_page_control (page->pointer current-page))
                      (lepton_page_get_page_control (page->pointer next-page)))
                   next-page
                   (find-page (cdr ls)))
               next-page))))
  (define next-pages
    (cdr (member current-page ((if previous? reverse identity) (active-pages)))))

  (let ((found-page (find-page next-pages)))
    (when found-page
      (*window-set-current-page! *window (page->pointer found-page)))))

;;; Search for a page preceding a given page in hierarchy.
(define-action-public (&page-prev #:label (G_ "Previous Page") #:icon "gtk-go-back")
  (hierarchy-goto-nearest-page (*current-window) #:previous? #t))

;;; Search for a page following a given page in hierarchy.
(define-action-public (&page-next #:label (G_ "Next Page") #:icon "gtk-go-forward")
  (hierarchy-goto-nearest-page (*current-window) #:previous? #f))


(define-action-public (&page-close #:label (G_ "Close Page") #:icon "gtk-close")
  (callback-page-close %null-pointer (*current-window)))

(define-action-public (&page-next-tab #:label (G_ "Next Tab") #:icon "gtk-go-forward")
  (x_tabs_next (*current-window)))

(define-action-public (&page-prev-tab #:label (G_ "Previous Tab") #:icon "gtk-go-back")
  (x_tabs_prev (*current-window)))

;; -------------------------------------------------------------------
;;;; Actions related to adding things

(define-action-public (&add-component #:label (G_ "Add Component") #:icon "insert-symbol")
  (callback-add-component %null-pointer (*current-window)))


(define-action-public (&add-attribute #:label (G_ "Add Attribute") #:icon "insert-attribute")
  ;; Definitions from "gschem_defines.h".
  (define FROM_MENU 0)
  (define FROM_HOTKEY 1)

  (define *window (*current-window))

  (attrib_edit_dialog *window
                      %null-pointer
                      (match (action-position)
                        ((x . y) FROM_HOTKEY)
                        (_ FROM_MENU)))

  (set-action-mode! 'select-mode))


(define-action-public (&add-net #:label (G_ "Add Net") #:icon "insert-net")
  (callback-add-net (*current-window)))

(define-action-public (&add-bus #:label (G_ "Add Bus") #:icon "insert-bus")
  (callback-add-bus (*current-window)))

(define-action-public (&add-text #:label (G_ "Add Text") #:icon "insert-text")
  (callback-add-text %null-pointer (*current-window)))


(define-action-public (&add-line #:label (G_ "Add Line") #:icon "insert-line")
  (define *window (*current-window))
  (o_redraw_cleanstates *window)
  (o_invalidate_rubber *window)

  (set-action-mode! 'line-mode)

  (let ((position (action-position)))
    (and position
         (match (snap-point position)
           ((x . y) (o_line_start *window x y))
           (_ #f)))))


(define-action-public (&add-path #:label (G_ "Add Path") #:icon "insert-path")
  (define *window (*current-window))

  (o_redraw_cleanstates *window)
  (o_invalidate_rubber *window)

  (set-action-mode! 'path-mode)

  ;; Don't start path here since setting of its first point and
  ;; control point requires the left button click and release.
  )


(define-action-public (&add-box #:label (G_ "Add Box") #:icon "insert-box")
  (define *window (*current-window))

  (o_redraw_cleanstates *window)
  (o_invalidate_rubber *window)

  (set-action-mode! 'box-mode)

  (let ((position (action-position)))
    (and position
         (match (snap-point position)
           ((x . y) (o_box_start *window x y))
           (_ #f)))))


(define-action-public (&add-circle #:label (G_ "Add Circle") #:icon "insert-circle")
  (define *window (*current-window))

  (o_redraw_cleanstates *window)
  (o_invalidate_rubber *window)

  (set-action-mode! 'circle-mode)

  (let ((position (action-position)))
    (and position
         (match (snap-point position)
           ((x . y) (o_circle_start *window x y))
           (_ #f)))))


(define-action-public (&add-arc #:label (G_ "Add Arc") #:icon "insert-arc")
  (define *window (*current-window))

  (o_redraw_cleanstates *window)
  (o_invalidate_rubber *window)

  (set-action-mode! 'arc-mode)

  (let ((position (action-position)))
    (and position
         (match (snap-point position)
           ((x . y) (o_arc_start *window x y))
           (_ #f)))))


(define-action-public (&add-pin #:label (G_ "Add Pin") #:icon "insert-pin")
  (define *window (*current-window))

  (o_redraw_cleanstates *window)
  (o_invalidate_rubber *window)

  (set-action-mode! 'pin-mode)

  (let ((position (action-position)))
    (and position
         (match (snap-point position)
           ((x . y) (o_pin_start *window x y))
           (_ #f)))))


(define-action-public (&add-picture #:label (G_ "Add Picture") #:icon "insert-image")
  (define *window (*current-window))

  (o_redraw_cleanstates *window)
  (o_invalidate_rubber *window)

  (set-action-mode! 'select-mode)

  (picture_selection_dialog *window))


;; -------------------------------------------------------------------
;;;; Hierarchy actions

(define (failed-to-descend-error filename message)
  (log! 'message (G_ "Failed to descend into ~S: ~A") filename message)
  (format #f (string-append
              (format #f
                      (G_ "Failed to descend hierarchy into ~S: ~A\n\n")
                      filename
                      message)
              (G_ "The lepton-schematic log may contain more information."))))

(define (source-attrib? attrib)
  (string= (attrib-name attrib) "source"))

(define (split-attrib-value object)
  (string-split (attrib-value object) #\,))

(define (zoom-child-page *window *parent *child)
  (define *toplevel (gschem_toplevel_get_toplevel *window))
  (define *page-view (gschem_toplevel_get_current_page_view *window))
  (lepton_toplevel_goto_page *toplevel *child)
  (gschem_toplevel_page_changed *window)
  (gschem_page_view_zoom_extents *page-view %null-pointer)
  (undo-save-state)
  (lepton_toplevel_goto_page *toplevel *parent)
  (gschem_toplevel_page_changed *window))

(define (hierarchy-down-error-dialog filename *error)
  (let ((secondary-message
         (failed-to-descend-error filename
                                  (if (or (null-pointer? *error)
                                          (null-pointer? (dereference-pointer *error)))
                                      (G_ "Unknown error.")
                                      (gerror-message (dereference-pointer *error))))))

    (schematic-error-dialog (G_ "Failed to descend hierarchy.")
                            secondary-message
                            %null-pointer)

    (g_clear_error *error)))

(define (hierarchy-down-filename filename *window *parent page-control)
  (define use-tabs? (true? (x_tabs_enabled)))

  (log! 'message (G_ "Searching for source ~S") filename)
  (let* ((*error (bytevector->pointer (make-bytevector (sizeof '*) 0)))
         (*child (s_hierarchy_down_schematic_single *window
                                                    (string->pointer filename)
                                                    *parent
                                                    page-control
                                                    *error)))
    (if (null-pointer? *child)
        ;; Launch the error dialog.
        (begin
          (hierarchy-down-error-dialog filename *error)
          #f)
        ;; Open the child page.
        (begin
          ;; Notify window that another page became active.
          (gschem_toplevel_page_changed *window)
          (if use-tabs?
              ;; Tabbed GUI is used.  Create a tab for every
              ;; subpage loaded.  Zoom will be set in
              ;; set-tab-page!().
              (*window-set-current-page! *window *child)
              ;; s_hierarchy_down_schematic_single() does not zoom
              ;; the loaded page, so zoom it here.
              (zoom-child-page *window *parent *child))
          *child))))

(define (hierarchy-filenames->pages filenames *window *parent)
  (let loop ((page-control 0)
             (filenames filenames)
             (children '()))
    (if (null? filenames)
        (reverse children)
        (let ((*child (hierarchy-down-filename (car filenames)
                                               *window
                                               *parent
                                               page-control)))
          (loop (if *child
                    (lepton_page_get_page_control *child)
                    page-control)
                (cdr filenames)
                (if *child
                    (cons *child children)
                    children))))))

(define-action-public (&hierarchy-down-schematic #:label (G_ "Down Schematic")
                                                 #:icon "gtk-go-down")
  (define *window (*current-window))

  (match (filter component? (page-selection (active-page)))
    ((a b . c) (schematic-message-dialog (G_ "Please select only one component!")))
    ((component)
     ;; Get component's "source=" attributes.  First look into
     ;; attached attribs and return the list if it is not empty.  If
     ;; the list of attached "source=" attribs is empty, look into
     ;; the list of inherited attribs.  The resulting list is
     ;; transformed into the list of files by splitting up the
     ;; attributes of the form "source=filename1,filename2,..." by
     ;; comma and appending the resulting lists.
     (let ((pages
            (hierarchy-filenames->pages
             (append-map split-attrib-value
                         (let ((attached-attribs (filter source-attrib?
                                                         (object-attribs component))))
                           (if (null? attached-attribs)
                               (filter source-attrib? (inherited-attribs component))
                               attached-attribs)))
             *window
             (schematic_window_get_active_page *window))))
       (unless (null? pages)
         ;; If the list of resulting pages is not empty, make the
         ;; first page active.
         (*window-set-current-page! *window (car pages)))))
    (_ (schematic-message-dialog (G_ "Please first select a component!")))))


(define (hierarchy-down-symbol *window *toplevel *symbol *parent)
  (define filename
    (let ((*clib-filename (s_clib_symbol_get_filename *symbol)))
      (if (null-pointer? *clib-filename)
          (begin
            (log! 'message (G_ "Symbol is not a real file. Symbol cannot be loaded."))
            #f)
          (let ((clib-filename (pointer->string *clib-filename)))
            (g_free *clib-filename)
            clib-filename))))

  (and filename
       (let* ((*filename (string->pointer (canonicalize-path filename)))
              (*page (lepton_toplevel_search_page *toplevel *filename)))
         (if (null-pointer? *page)
             ;; Create a new page.
             (let ((*page (lepton_page_new *toplevel *filename)))
               (schematic_file_open *window
                                    *page
                                    (lepton_page_get_filename *page)
                                    %null-pointer)

               (schematic_hierarchy_increment_page_control_counter)
               (lepton_page_set_page_control *page
                                             (schematic_hierarchy_get_page_control_counter))
               *page)
             ;; Return existing page.
             *page))))


(define-action-public (&hierarchy-down-symbol #:label (G_ "Down Symbol") #:icon "gtk-goto-bottom")
  (define *window (*current-window))

  (match (filter component? (page-selection (active-page)))
    ((a b . c) (schematic-message-dialog (G_ "Please select only one component!")))
    ((component)
     ;; Get pointer to the first selected component.
     (let ((name (component-basename component)))
       (log! 'message (G_ "Searching for symbol: ~S") name)
       (let ((*sym (s_clib_get_symbol_by_name (string->pointer name))))
         (unless (null-pointer? *sym)
           (let* ((*toplevel (gschem_toplevel_get_toplevel *window))
                  (*parent (schematic_window_get_active_page *window))
                  (*page (hierarchy-down-symbol *window *toplevel *sym *parent)))
             (when *page

               ;; Change link to parent page since we can come here
               ;; from any parent and must come back to the same page.
               (lepton_page_set_up *page (lepton_page_get_pid *parent))
               (lepton_toplevel_goto_page *toplevel *page)

               (gschem_toplevel_page_changed *window)

               ;; Get active page once again, it should now be the symbol
               ;; page.
               (*window-set-current-page! *window
                                          (schematic_window_get_active_page *window))

               ;; s_hierarchy_down_symbol() will not zoom the loaded page.
               ;; Tabbed GUI: zoom is set in set-tab-page!().
               (unless (true? (x_tabs_enabled))
                 (gschem_page_view_zoom_extents
                  (gschem_toplevel_get_current_page_view *window)
                  %null-pointer))

               (undo-save-state)))))))
    (_ (schematic-message-dialog (G_ "Please first select a component!")))))


;;; Go to the upper hierarchy level page, that is, return to the
;;; page which is parent for the current page in the hierarchy of
;;; schematics.
(define-action-public (&hierarchy-up #:label (G_ "Up Hierarchy") #:icon "gtk-go-up")
  (define *window (*current-window))
  (define *page (and=> (active-page) page->pointer))

  (when *page
    (let ((*upper-page (s_hierarchy_find_up_page *page)))
      (if (null-pointer? *upper-page)
          (log! 'message (G_ "Cannot find any schematics above the current one!"))
          (let ((changed? (true? (lepton_page_get_changed *page))))
            (when (or (not changed?)
                      ;; If the page has changed, ask the user to
                      ;; really close it.
                      (true? (x_dialog_close_changed_page *window *page)))
              (window-close-page! (current-window) (active-page))
              (*window-set-current-page! *window *upper-page)))))))


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


(define (set-selected-attribs-show-mode! mode *window)
  (define (set-show-mode! object)
    (set-text-attribute-mode! object mode))
  (let ((attribs (filter attribute? (page-selection (active-page)))))
    (unless (null? attribs)
      (for-each set-show-mode! attribs)
      ;; If attribute mode has changed, the page must get changed,
      ;; too.  Update the window only in such a case.
      (when (page-dirty? (active-page))
        (schematic_window_active_page_changed *window))
      (undo-save-state))))


(define-action-public (&attributes-show-value #:label (G_ "Show Attribute Value") #:icon "attribute-show-value")
  (unless (in-action?)
    (set-selected-attribs-show-mode! 'value (*current-window))))


(define-action-public (&attributes-show-name #:label (G_ "Show Attribute Name") #:icon "attribute-show-name")
  (unless (in-action?)
    (set-selected-attribs-show-mode! 'name (*current-window))))


(define-action-public (&attributes-show-both #:label (G_ "Show Name & Value") #:icon "attribute-show-both")
  (unless (in-action?)
    (set-selected-attribs-show-mode! 'both (*current-window))))


(define-action-public (&attributes-visibility-toggle #:label (G_ "Toggle Text Visibility"))
  (define (toggle-visibility! object)
    (set-text-visibility! object (not (text-visible? object))))

  (unless (in-action?)
    (let ((attribs (filter attribute? (page-selection (active-page)))))
      (unless (null? attribs)
        (for-each toggle-visibility! attribs)
        ;; Here, the attribute visibility changes anyways, so it's
        ;; safe to mark the page as changed and update the window.
        (schematic_window_active_page_changed (*current-window))
        (undo-save-state)))))


(define-action-public (&edit-find-text #:label (G_ "Find Specific Text") #:icon "gtk-find")
  (unless (in-action?)
    (find_text_dialog (*current-window))))


(define-action-public (&edit-hide-text #:label (G_ "Hide Specific Text"))
  (unless (in-action?)
    (hide_text_dialog (*current-window))))


(define-action-public (&edit-show-text #:label (G_ "Show Specific Text"))
  (unless (in-action?)
    (show_text_dialog (*current-window))))


(define-action-public (&edit-autonumber #:label (G_ "Autonumber Text"))
  (unless (in-action?)
    (autonumber_text_dialog (*current-window))))


;; -------------------------------------------------------------------
;;;; Configuration actions

(define-action-public (&help-hotkeys #:label (G_ "Show Hotkeys") #:icon "preferences-desktop-keyboard-shortcuts")
  (x_dialog_hotkeys (*current-window)))


;;; Cycle grid mode.
(define-action-public (&options-grid #:label (G_ "Switch Grid Style"))
  (gschem_options_cycle_grid_mode (schematic_window_get_options (*current-window))))


(define-action-public (&options-snap #:label (G_ "Switch Snap Mode"))
  (define *window (*current-window))
  (define *options (schematic_window_get_options *window))

  (gschem_options_cycle_snap_mode *options)

  (let ((snap-mode (pointer->string
                    (schematic_snap_mode_to_string
                     (gschem_options_get_snap_mode *options)))))
    ;; FIXME: the user should be always aware of snap mode change,
    ;; no matter what is used to switch the mode, a hotkey or a
    ;; mouse click on the status bar.  May be show the messages
    ;; below in the status bar?
    (match snap-mode
      ("off" (log! 'message (G_ "Snap OFF (CAUTION!)")))
      ("grid" (log! 'message (G_ "Snap ON")))
      ("resnap" (log! 'message (G_ "Snap back to the grid (CAUTION!)")))
      (_ (error "Invalid snap_mode: ~S" snap-mode))))

  (i_show_state *window %null-pointer)
  (i_update_grid_info *window))


;;; Shows the options widget.
(define-action-public (&options-snap-size #:label (G_ "Set Grid Spacing"))
  (x_widgets_show_options (*current-window)))


;;; Multiplies by two the snap grid size.
(define-action-public (&options-scale-up-snap-size #:label (G_ "Increase Grid Spacing"))
  (define *options (schematic_window_get_options (*current-window)))
  (gschem_options_set_snap_size *options
                                (* (gschem_options_get_snap_size *options) 2)))


;;; Divides by two the snap grid size (if it's and even number).
(define-action-public (&options-scale-down-snap-size #:label (G_ "Decrease Grid Spacing"))
  (define *options (schematic_window_get_options (*current-window)))
  (define snap-size (gschem_options_get_snap_size *options))
  (when (even? snap-size)
    (gschem_options_set_snap_size *options (/ snap-size 2))))


;;; Toggles visibility of currently selected objects between
;;; outline mode (fully visible) and bounding box mode
;;; (displaying bounding box rectangle).
(define-action-public (&options-action-feedback #:label (G_ "Toggle Outline Drawing"))
  (define *window (*current-window))

  ;; Definitions from "gschem_defines.h".
  (define OUTLINE 0)
  (define BOUNDINGBOX 1)

  (if (= (schematic_window_get_actionfeedback_mode *window) BOUNDINGBOX)
      (begin
        (schematic_window_set_actionfeedback_mode *window OUTLINE)
        (log! 'message (G_ "Action feedback mode set to OUTLINE")))

      (begin
        (schematic_window_set_actionfeedback_mode *window BOUNDINGBOX)
        (log! 'message (G_ "Action feedback mode set to BOUNDINGBOX"))))

  (when (and (in-action?)
             (not (null-pointer? (schematic_window_get_place_list *window))))
    (o_place_invalidate_rubber *window FALSE)))


(define-action-public (&options-rubberband #:label (G_ "Toggle Net Rubber Band"))
  (gschem_options_cycle_net_rubber_band_mode
   (schematic_window_get_options (*current-window))))

(define-action-public (&options-magneticnet #:label (G_ "Toggle Magnetic Nets"))
  (define *window (*current-window))
  (gschem_options_cycle_magnetic_net_mode (schematic_window_get_options *window))
  (i_show_state *window %null-pointer))


(define-action-public (&options-show-log-window #:label (G_ "Show Log Window"))
  (x_widgets_show_log (*current-window)))

(define-action-public (&options-show-coord-window #:label (G_ "Show Coordinate Window"))
  (coord_dialog (*current-window) 0 0))

(define-action-public (&options-select-font #:label (G_ "Select Schematic Font"))
  (x_widgets_show_font_select (*current-window)))


(define-action-public (&options-draw-grips #:label (G_ "Toggle Grips"))
  (define *window (*current-window))
  (define draw-grips (true? (schematic_window_get_draw_grips *window)))

  (schematic_window_set_draw_grips *window (if draw-grips FALSE TRUE))
  (gschem_page_view_invalidate_all
   (gschem_toplevel_get_current_page_view *window)))


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
  (about_dialog (*current-window)))


; Backward compatibility:
;
(define &edit-color    &edit-object-properties) (export &edit-color)
(define &edit-linetype &edit-object-properties) (export &edit-linetype)
(define &edit-filltype &edit-object-properties) (export &edit-filltype)
(define &edit-pin-type &edit-object-properties) (export &edit-pin-type)

;; Local Variables:
;; eval: (put 'define-action-public 'scheme-indent-function 1)
;; End:
