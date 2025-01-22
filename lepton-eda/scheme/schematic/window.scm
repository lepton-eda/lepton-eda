;; Lepton EDA Schematic Capture
;; Scheme API
;; Copyright (C) 2010-2011 Peter Brett <peter@peter-b.co.uk>
;; Copyright (C) 2017-2026 Lepton EDA Contributors
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
;;

(define-module (schematic window)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)
  #:use-module (system foreign)

  #:use-module (lepton attrib)
  #:use-module (lepton config)
  #:use-module (lepton eval)
  #:use-module (lepton ffi boolean)
  #:use-module (lepton ffi check-args)
  #:use-module (lepton ffi glib)
  #:use-module (lepton ffi gobject)
  #:use-module (lepton ffi)
  #:use-module (lepton gerror)
  #:use-module (lepton gettext)
  #:use-module (lepton log)
  #:use-module (lepton m4)
  #:use-module (lepton object foreign)
  #:use-module (lepton object)
  #:use-module (lepton page foreign)
  #:use-module (lepton page)
  #:use-module (lepton toplevel foreign)
  #:use-module (lepton toplevel)

  #:use-module (schematic action copy)
  #:use-module (schematic action edit)
  #:use-module (schematic action rotate)
  #:use-module (schematic action-mode)
  #:use-module (schematic buffer)
  #:use-module (schematic callback cancel)
  #:use-module (schematic callback)
  #:use-module (schematic canvas foreign)
  #:use-module (schematic canvas)
  #:use-module (schematic dialog close-page)
  #:use-module (schematic dialog close-window)
  #:use-module (schematic event)
  #:use-module (schematic ffi)
  #:use-module (schematic ffi gtk)
  #:use-module (schematic gtk helper)
  #:use-module (schematic gui keymap)
  #:use-module (schematic gui stroke)
  #:use-module (schematic hook)
  #:use-module (schematic menu)
  #:use-module (schematic mouse-pointer)
  #:use-module (schematic rc)
  #:use-module (schematic tabs)
  #:use-module (schematic toolbar)
  #:use-module (schematic undo)
  #:use-module (schematic viewport foreign)
  #:use-module (schematic window foreign)
  #:use-module (schematic window global)
  #:use-module (schematic window list)
  #:use-module (schematic window page)

  #:re-export (%lepton-window
               current-window
               with-window
               pointer-position)

  #:export (close-window!
            make-schematic-window
            active-page
            set-active-page!
            show-macro-widget
            snap-point
            window-canvas
            window-close-page!
            window-open-page!
            window-set-current-page!
            *window-set-current-page!
            zoom-box-start)

  ;; Overrides the close-page! procedure in the (lepton page)
  ;; module.
  #:replace (close-page!))


(define (process-key-event *page_view *event *window)
  (with-window *window
    (eval-press-key-event *event *page_view *window)))

(define *process-key-event
  (procedure->pointer int process-key-event '(* * *)))


(define (destroy-window-widgets! *window)
  (define (destroy-widget! *widget)
    (unless (null-pointer? *widget) (gtk_widget_destroy *widget)))

  (x_widgets_destroy_dialogs *window)

  (for-each destroy-widget!
            (list (schematic_window_get_compselect_widget *window)
                  (schematic_window_get_newtext_dialog *window)
                  (schematic_window_get_arc_edit_widget *window)
                  (schematic_window_get_multiattrib_widget *window)
                  (schematic_window_get_attrib_edit_widget *window)
                  (schematic_window_get_hotkey_widget *window)
                  (schematic_window_get_coord_widget *window)
                  (schematic_window_get_slot_edit_widget *window))))


(define (close-window! window)
  "Closes WINDOW."
  (define *window (window->pointer window))
  (define last-window? (= (length (schematic-windows)) 1))
  ;; Remember current active page.  In cases when the user saves
  ;; some untitled pages but leaves some other ones unsaved, the
  ;; window won't be closed and we have to restore the active
  ;; page.
  (define active-page
    (pointer->page (schematic_window_get_active_page *window)))

  (define (close!)
    ;; Close the window if the user didn't cancel the close.
    (x_clipboard_finish *window)

    ;; The window will be destroyed, prohibit possible but
    ;; superfluous actions on object invalidation.
    (schematic_window_set_dont_invalidate *window TRUE)

    ;; Destroy all widgets.
    (destroy-window-widgets! *window)

    ;; Check if the window is the last one and do jobs that have
    ;; to be done before freeing its memory.
    (when last-window?
      ;; Save window geometry.
      (schematic_window_save_geometry *window)
      ;; Close the log file.
      (s_log_close)
      ;; Free the buffers.
      (free-buffers))

    ;; Destroy main widget of the window.
    (gtk_widget_destroy (schematic_window_get_main_window *window))
    (remove-window! (pointer->window *window))
    (schematic_window_free *window)

    ;; Just closed last window, so quit.
    (when (zero? (length (schematic-windows)))
      ;; Clean up all memory objects allocated during the
      ;; lepton-schematic runtime.

      ;; Save cache config on exit.
      (config-save! (cache-config-context))
      (s_clib_free)
      (s_attrib_free)
      (x_stroke_free)
      (undo-cleanup-backup-files!)

      ;; Check whether the main loop is running.
      (if (zero? (gtk_main_level))
          (primitive-exit 0)
          (gtk_main_quit))))

  ;; If we're closing whilst inside an action, re-wind the page
  ;; contents back to their state before we started.
  (when (in-action? window)
    (callback-cancel *window))

  (case (close-window-dialog window)
    ;; Close window.
    ((close) (close!))
    ;; Restore active page.
    ((restore) (window-set-toplevel-page! window active-page))
    ;; Dialog has been cancelled. Do nothing.
    (else #f)))


(define (callback-close-schematic-window *widget *event *window)
  (if (null-pointer? *window)
      (error "NULL window.")
      (close-window! (pointer->window *window)))
  ;; Stop further propagation of the "delete-event" signal for
  ;; window:
  ;;   - if the user has cancelled closing, the window should
  ;;     obviously not be destroyed
  ;;   - otherwise the window has already been destroyed, nothing
  ;;     more to do
  TRUE)


;;; When invoked via signal "delete-event", the function closes
;;; the current window and, if this is the last window, quits
;;; lepton-schematic.  The signal is emitted when you click the
;;; close button on the window.
(define *callback-close-schematic-window
  (procedure->pointer int callback-close-schematic-window '(* * *)))


;;; Opens a file selected in recent-chooser.
(define (callback-recent-chooser-item-activated *chooser *window)
  (define window (pointer->window *window))
  (define *filename
    (schematic_menu_recent_chooser_get_filename *chooser *window))
  (define filename (pointer->string *filename))

  ;; Free the returned C string.
  (g_free *filename)

  (window-set-current-page! window
                            (window-open-page! window filename)))


;;; C callback for the above function.
(define *callback-recent-chooser-item-activated
  (procedure->pointer void
                      callback-recent-chooser-item-activated
                      '(* *)))


;;; GtkNotebook "page-reordered" signal handler.
(define (callback-page-reordered *notebook *widget-tab new-index *window)

 (when (null-pointer? *window)
   (error "NULL window."))

 (let ((*pages (schematic_window_get_pages *window)))
   (when (null-pointer? *pages)
     (error "NULL pages."))

   (let* ((*info-list (schematic_window_get_tab_info_list *window))
          (*tab-info (get-tab-info *widget-tab *info-list)))
     (when (null-pointer? *tab-info)
       (error "NULL tabinfo."))

     (lepton_list_move_item *pages (schematic_tab_info_get_page *tab-info) new-index)

     (gtk_widget_grab_focus (schematic_tab_info_get_canvas *tab-info))))

 (page_select_widget_update *window))

(define *callback-page-reordered
  (procedure->pointer void
                      callback-page-reordered
                      (list '* '* int '*)))


(define (process-pending-events)
  (let loop ((pending-event? (true? (gtk_events_pending))))
    (when pending-event?
      (gtk_main_iteration)
      (loop (true? (gtk_events_pending))))))


(define (grab-focus *tab-info)
  (schematic_canvas_grab_focus
   (schematic_tab_info_get_canvas *tab-info)))


;;; Defines from schematic_defines.h.
(define MOUSEBTN_DO_STROKE 0)
(define MOUSEBTN_DO_REPEAT 1)
(define MOUSEBTN_DO_ACTION 2)
(define MOUSEBTN_DO_POPUP  4)
(define MOUSEBTN_DO_PAN    5)


(define (window-save-modifiers *window *event)
  (define alt-mask (schematic_event_alt_mask))
  (define control-mask (schematic_event_control_mask))
  (define shift-mask (schematic_event_shift_mask))

  (define (state-contains? state mask)
    (if (logtest state mask) 1 0))

  (define state (event-state *event))

  (schematic_window_set_shift_key_pressed *window
                                          (state-contains? state shift-mask))
  (schematic_window_set_control_key_pressed *window
                                            (state-contains? state control-mask))
  (schematic_window_set_alt_key_pressed *window
                                        (state-contains? state alt-mask)))


(define (zoom-box window)
  "Zoom WINDOW using the info saved in its page view geometry."
  (define *window (check-window window 1))
  (define canvas (window-canvas window))
  (define *canvas (canvas->pointer canvas))
  (define *viewport (viewport->pointer (canvas-viewport canvas)))
  (define x1 (schematic_window_get_first_wx *window))
  (define x2 (schematic_window_get_second_wx *window))
  (define y1 (schematic_window_get_first_wy *window))
  (define y2 (schematic_window_get_second_wy *window))

  ;; Test if there is really a box.
  (if (or (= x1 x2) (= y1 y2))
      (log! 'message (G_ "Zoom too small!  Cannot zoom further."))
      ;; Calculate new zoom factors.
      (let* ((zx (/ (abs (- (schematic_viewport_get_left *viewport)
                            (schematic_viewport_get_right *viewport)))
                    (abs (- x1 x2))))
             (zy (/ (abs (- (schematic_viewport_get_top *viewport)
                            (schematic_viewport_get_bottom *viewport)))
                    (abs (- y1 y2))))
             ;; Choose the smaller one.
             (relative-zoom-factor (if (< zx zy) zx zy))

             ;; Calculate the center of the zoom box.
             (world-pan-center-x (round (/ (+ x1 x2) 2)))
             (world-pan-center-y (round (/ (+ y1 y2) 2))))

        ;; Update the canvas with new values.
        (schematic_canvas_pan_general *canvas
                                      world-pan-center-x
                                      world-pan-center-y
                                      relative-zoom-factor))))


(define (zoom-box-start window x y)
  "Start zooming in WINDOW at coords X and Y."
  (define *window (check-window window 1))

  (i_action_start *window)
  (schematic_window_set_first_wx *window x)
  (schematic_window_set_second_wx *window x)
  (schematic_window_set_first_wy *window y)
  (schematic_window_set_second_wy *window y))


(define (zoom-box-invalidate window)
  "Schedule redrawing of the canvas region in WINDOW during box
zooming."
  (define *window (check-window window 1))

  (schematic_canvas_invalidate_world_rect
   (schematic_window_get_current_canvas *window)
   (schematic_window_get_first_wx *window)
   (schematic_window_get_first_wy *window)
   (schematic_window_get_second_wx *window)
   (schematic_window_get_second_wy *window)))


(define (zoom-box-motion window x y)
  "Process motion events in *WINDOW when box zooming is in action."
  (define *window (check-window window 1))

  (unless (in-action? window)
    (error "zoom-box-motion(): The window is not in action!"))

  (when (true? (schematic_window_get_rubber_visible *window))
    (zoom-box-invalidate window))

  (schematic_window_set_second_wx *window x)
  (schematic_window_set_second_wy *window y)

  (zoom-box-invalidate window)
  (schematic_window_set_rubber_visible *window 1))


(define (zoom-box-end window x y)
  "End zooming in WINDOW at coords X and Y."
  (define *window (check-window window 1))
  (check-integer x 2)
  (check-integer y 3)

  (unless (in-action? window)
    (error "zoom-box-end(): The window is not in action!"))
  (zoom-box-invalidate window)
  (schematic_window_set_rubber_visible *window 0)
  (zoom-box window)
  (undo-save-viewport window)
  (i_action_stop *window)
  (set-action-mode! 'select-mode #:window window))


(define-syntax-rule (process-canvas-event *canvas *event *window process-func)
  (if (or (null-pointer? *window)
          (null-pointer? *canvas))
      (error "NULL page view or window.")
      (let ((*page (schematic_canvas_get_page *canvas)))
        (if (null-pointer? *page)
            ;; If there is no page, terminate event.
            TRUE
            ;; Some underlying functions have to know what window
            ;; they operate on.  Set the current window explicitly
            ;; as it is not defined for C GLib callbacks.
            (with-window *window
              (process-func *canvas *event *window))))))


(define (callback-button-released *canvas *event *window)
  (define window (pointer->window *window))
  (define current-action-mode (action-mode window))
  (define window-coords (event-coords *event))
  (define unsnapped-x-bv (make-bytevector (sizeof int) 0))
  (define unsnapped-y-bv (make-bytevector (sizeof int) 0))
  (define (process-event *canvas *event *window)
    (let ((button-number (schematic_event_get_button *event))
          (window-x (car window-coords))
          (window-y (cdr window-coords)))
      (window-save-modifiers *window *event)
      (schematic_canvas_SCREENtoWORLD *canvas
                                      (inexact->exact (round window-x))
                                      (inexact->exact (round window-y))
                                      (bytevector->pointer unsnapped-x-bv)
                                      (bytevector->pointer unsnapped-y-bv))
      (let* ((unsnapped-x (bytevector-sint-ref unsnapped-x-bv 0 (native-endianness) (sizeof int)))
             (unsnapped-y (bytevector-sint-ref unsnapped-y-bv 0 (native-endianness) (sizeof int)))
             (x (snap_grid *window unsnapped-x))
             (y (snap_grid *window unsnapped-y)))

        ;; Evaluate state transitions.
        (match button-number
          (1
           (when (in-action? window)
             (if (not (null-pointer? (schematic_window_get_place_list *window)))
                 (match current-action-mode
                   ('copy-mode (finish-copy *window))
                   ('multiple-copy-mode (finish-copy *window 'keep-on))
                   ('move-mode (o_move_end *window))
                   (_ FALSE))

                 (match current-action-mode
                   ('grips-mode (o_grips_end *window))
                   ('path-mode (o_path_end *window x y))
                   ('box-select-mode (o_select_box_end *window unsnapped-x unsnapped-y))
                   ('select-mode (o_select_end *window unsnapped-x unsnapped-y))
                   ('zoom-box-mode (zoom-box-end window unsnapped-x unsnapped-y))
                   (_ FALSE)))))

          (2
           (when (in-action? window)
             (when (or (eq? current-action-mode 'component-mode)
                       (eq? current-action-mode 'text-mode)
                       (eq? current-action-mode 'move-mode)
                       (eq? current-action-mode 'copy-mode)
                       (eq? current-action-mode 'multiple-copy-mode)
                       (eq? current-action-mode 'paste-mode))
               (if (eq? current-action-mode 'move-mode)
                   (o_move_invalidate_rubber *window FALSE)
                   (o_place_invalidate_rubber *window FALSE))
               (schematic_window_set_rubber_visible *window 0)

               (o_place_rotate *window)

               (when (eq? current-action-mode 'component-mode)
                 (o_component_place_changed_run_hook *window))

               (if (eq? current-action-mode 'move-mode)
                   (o_move_invalidate_rubber *window TRUE)
                   (o_place_invalidate_rubber *window TRUE))

               (schematic_window_set_rubber_visible *window 1)))
           (unless (and (in-action? window)
                        (or (eq? current-action-mode 'component-mode)
                            (eq? current-action-mode 'text-mode)
                            (eq? current-action-mode 'move-mode)
                            (eq? current-action-mode 'copy-mode)
                            (eq? current-action-mode 'multiple-copy-mode)
                            (eq? current-action-mode 'paste-mode)))
             (let ((middle-button (schematic_window_get_middle_button *window)))
               (cond
                ((= middle-button MOUSEBTN_DO_ACTION)
                 (when (and (in-action? window)
                            (not (null-pointer? (schematic_window_get_place_list *window))))
                   (match current-action-mode
                     ('copy-mode (finish-copy *window))
                     ('move-mode (o_move_end *window))
                     (_ FALSE))))

                ((= middle-button MOUSEBTN_DO_STROKE)
                 (schematic_event_set_doing_stroke FALSE)
                 (let ((*str (x_stroke_translate_and_execute *window)))
                   ;; When libbstroke is not defined, e.g. for
                   ;; GTK3 port, the above function returns #f.
                   (when (and *str
                              (not (null-pointer? *str)))
                     (let ((str (pointer->string *str)))
                       (g_free *str)
                       (with-window *window
                         (eval-stroke str)))))
                 FALSE)

                ((= middle-button MOUSEBTN_DO_PAN)
                 (when (true? (schematic_canvas_pan_end *canvas))
                   (undo-save-viewport)))
                (else FALSE)))))

          (3
           ;; Just for ending a mouse pan.
           (when (true? (schematic_canvas_pan_end *canvas))
             (undo-save-viewport)))

          (_ FALSE))

        FALSE)))

  (process-canvas-event *canvas *event *window process-event))

(define *callback-button-released
  (procedure->pointer int callback-button-released '(* * *)))


(define (callback-button-pressed *canvas *event *window)
  (define window (pointer->window *window))
  (define current-action-mode (action-mode window))
  (define window-coords (event-coords *event))
  (define unsnapped-x-bv (make-bytevector (sizeof int) 0))
  (define unsnapped-y-bv (make-bytevector (sizeof int) 0))

  (define (continue-placement?)
    (if (config-boolean (path-config-context (getcwd))
                        "schematic.gui"
                        "continue-component-place")
        TRUE
        FALSE))

  (define (process-event *canvas *event *window)
    (schematic_canvas_grab_focus *canvas)
    (let ((button-number (schematic_event_get_button *event))
          (window-x (car window-coords))
          (window-y (cdr window-coords)))
      (schematic_canvas_SCREENtoWORLD *canvas
                                      (inexact->exact (round window-x))
                                      (inexact->exact (round window-y))
                                      (bytevector->pointer unsnapped-x-bv)
                                      (bytevector->pointer unsnapped-y-bv))
      (let* ((unsnapped-x (bytevector-sint-ref unsnapped-x-bv 0 (native-endianness) (sizeof int)))
             (unsnapped-y (bytevector-sint-ref unsnapped-y-bv 0 (native-endianness) (sizeof int)))
             (x (snap_grid *window unsnapped-x))
             (y (snap_grid *window unsnapped-y))
             (*selection (schematic_window_get_selection_list *window)))
        (if (and (true? (schematic_event_is_double_button_press *event))
                 (eq? current-action-mode 'select-mode))
            ;; Process double-click event.
            (begin
              ;; GDK_BUTTON_EVENT is emitted before GDK_2BUTTON_EVENT, which
              ;; leads to setting of the inside_action flag.  If edit-objects()
              ;; brings up a modal window (e.g., the edit attribute dialog),
              ;; it intercepts the release button event and thus doesn't
              ;; allow resetting of the inside_action flag so we do it
              ;; manually here before processing the double-click event.
              (i_action_stop *window)
              (edit-objects window
                            (glist->list (lepton_list_get_glist *selection)
                                         pointer->object))
              FALSE)
            ;; Process simple one click event.
            (begin
              (window-save-modifiers *window *event)
              ;; Evaluate state transitions.
              (match button-number
                ;; First mouse button.
                (1
                 (if (in-action? window)
                     ;; End action.
                     (if (not (null-pointer? (schematic_window_get_place_list *window)))
                         (match current-action-mode
                           ('component-mode
                            (o_place_end *window
                                         x
                                         y
                                         (continue-placement?)
                                         (string->pointer "add-objects-hook")))
                           ('text-mode
                            (o_place_end *window x y FALSE (string->pointer "add-objects-hook")))
                           ('paste-mode
                            (o_place_end *window x y FALSE (string->pointer "paste-objects-hook")))
                           (_ FALSE))

                         (match current-action-mode
                           ('arc-mode (o_arc_end1 *window x y))
                           ('box-mode (o_box_end *window x y))
                           ('bus-mode (o_bus_end *window x y))
                           ('circle-mode (o_circle_end *window x y))
                           ('line-mode (o_line_end *window x y))
                           ('net-mode (o_net_end *window x y))
                           ('path-mode (o_path_continue *window x y))
                           ('picture-mode (o_picture_end *window x y))
                           ('pin-mode (o_pin_end *window x y))
                           (_ FALSE)))

                     ;; Start action
                     (match current-action-mode
                       ('arc-mode (o_arc_start *window x y))
                       ('box-mode (o_box_start *window x y))
                       ('bus-mode (o_bus_start *window x y))
                       ('circle-mode (o_circle_start *window x y))
                       ('line-mode (o_line_start *window x y))
                       ('net-mode (o_net_start *window x y))
                       ('path-mode (o_path_start *window x y))
                       ('picture-mode (o_picture_start *window x y))
                       ('pin-mode (o_pin_start *window x y))
                       ('zoom-box-mode (zoom-box-start window unsnapped-x unsnapped-y))
                       ('select-mode (o_select_start *window x y))
                       ((or 'copy-mode 'multiple-copy-mode) (start-copy *window x y))
                       ('move-mode (o_move_start *window x y))
                       (_ FALSE)))

                 (match current-action-mode
                   ('rotate-mode
                    (rotate-objects x y 90 (lepton_list_get_glist *selection)
                                    #:window window))
                   ('mirror-mode
                    (o_mirror_world_update *window x y (lepton_list_get_glist *selection)))
                   ('pan-mode
                    (schematic_canvas_pan *canvas x y)
                    (set-action-mode! 'select-mode #:window window))
                   (_ FALSE))
                 ;; Finish event processing.
                 FALSE)

                ;; Second mouse button.
                (2
                 (if (in-action? window)
                     (unless (or (eq? current-action-mode 'component-mode)
                                 (eq? current-action-mode 'text-mode)
                                 (eq? current-action-mode 'move-mode)
                                 (eq? current-action-mode 'copy-mode)
                                 (eq? current-action-mode 'multiple-copy-mode)
                                 (eq? current-action-mode 'paste-mode))
                       (callback-cancel *window))

                     (let ((middle-button (schematic_window_get_middle_button *window)))
                       (cond
                        ((= middle-button MOUSEBTN_DO_ACTION)
                         ;; Don't want to search an object under
                         ;; mouse cursor if Shift key is pressed.
                         (unless (true? (schematic_window_get_shift_key_pressed *window))
                           (o_find_object *window unsnapped-x unsnapped-y TRUE))

                         ;; Make sure the selection list is not empty.
                         (if (not (true? (o_select_selected *window)))
                             (begin
                               ;; This means the above find did not
                               ;; find anything.
                               (i_action_stop *window)
                               (set-action-mode! 'select-mode #:window window))

                             ;; Determine here if copy or move
                             ;; should be started.
                             (if (true? (schematic_window_get_alt_key_pressed *window))
                                 ;; Set copy mode and start copying.
                                 (begin
                                   (set-action-mode! 'copy-mode #:window window)
                                   (start-copy *window x y))
                                 ;; Start moving objects.
                                 (o_move_start *window x y))))

                        ((= middle-button MOUSEBTN_DO_REPEAT)
                         ((@ (schematic action) &repeat-last-action)))

                        ((= middle-button MOUSEBTN_DO_STROKE)
                         (schematic_event_set_doing_stroke TRUE))

                        ((= middle-button MOUSEBTN_DO_PAN)
                         (schematic_canvas_pan_start *canvas
                                                     (inexact->exact (round window-x))
                                                     (inexact->exact (round window-y))))

                        ((= middle-button MOUSEBTN_DO_POPUP)
                         (i_update_menus *window)
                         (do_popup *window *event))

                        (else FALSE))))
                 ;; Finish event processing.
                 FALSE)

                ;; Third mouse button.
                (3
                 (if (not (in-action? window))
                     (if (eq? (schematic_window_get_third_button *window)
                              MOUSEBTN_DO_POPUP)
                         (begin
                           ;; (third-button "popup")
                           ;; Update menus before popup.
                           (i_update_menus *window)
                           (do_popup *window *event))
                         ;; (third-button "mousepan")
                         (schematic_canvas_pan_start *canvas
                                                     (inexact->exact (round window-x))
                                                     (inexact->exact (round window-y))))
                     (if (and (eq? (schematic_window_get_third_button *window)
                                   MOUSEBTN_DO_PAN)
                              (not (true? (schematic_window_get_third_button_cancel *window))))
                         (schematic_canvas_pan_start *canvas
                                                     (inexact->exact (round window-x))
                                                     (inexact->exact (round window-y)))
                         (begin
                           ;; This is the default cancel.
                           ;; Reset all draw and place actions.
                           (match current-action-mode
                             ('arc-mode (o_arc_invalidate_rubber *window))
                             ('box-mode (o_box_invalidate_rubber *window))
                             ('bus-mode (o_bus_invalidate_rubber *window))
                             ('circle-mode (o_circle_invalidate_rubber *window))
                             ('line-mode (o_line_invalidate_rubber *window))
                             ('net-mode (o_net_invalidate_rubber *window))
                             ('path-mode (o_path_invalidate_rubber *window))
                             ('picture-mode (o_picture_invalidate_rubber *window))
                             ('pin-mode (o_pin_invalidate_rubber *window))
                             (_ 'do-nothing))
                           (when (true? (schematic_window_get_third_button_cancel *window))
                             (callback-cancel *window)))))
                 ;; Finish event processing.
                 FALSE)

                (_ FALSE)))))))

  (process-canvas-event *canvas *event *window process-event))

(define *callback-button-pressed
  (procedure->pointer int callback-button-pressed '(* * *)))


(define (callback-motion *canvas *event *window)
  (define window (pointer->window *window))
  (define current-action-mode (action-mode window))
  (define window-coords (event-coords *event))
  ;; Define from arc_object.h.
  (define ARC_RADIUS 1)

  (define (process-event *canvas *event *window)
    (let ((window-x (car window-coords))
          (window-y (cdr window-coords)))
      (window-save-modifiers *window *event)

      (if (true? (schematic_event_get_doing_stroke))
          (let ((int-x (inexact->exact (round window-x)))
                (int-y (inexact->exact (round window-y))))
            (x_stroke_record *window int-x int-y)
            FALSE)
          (if (true? (schematic_event_skip_motion_event *event))
              FALSE

              (let ((unsnapped-x-bv (make-bytevector (sizeof int) 0))
                    (unsnapped-y-bv (make-bytevector (sizeof int) 0)))
                (schematic_canvas_SCREENtoWORLD *canvas
                                                (inexact->exact (round window-x))
                                                (inexact->exact (round window-y))
                                                (bytevector->pointer unsnapped-x-bv)
                                                (bytevector->pointer unsnapped-y-bv))
                (let* ((unsnapped-x (bytevector-sint-ref unsnapped-x-bv 0 (native-endianness) (sizeof int)))
                       (unsnapped-y (bytevector-sint-ref unsnapped-y-bv 0 (native-endianness) (sizeof int)))
                       (x (snap_grid *window unsnapped-x))
                       (y (snap_grid *window unsnapped-y)))
                  (unless (null-pointer? (schematic_window_get_coord_widget *window))
                    (coord_display_update *window
                                          (inexact->exact (round window-x))
                                          (inexact->exact (round window-y))))
                  (schematic_canvas_pan_motion *canvas
                                               (schematic_window_get_mousepan_gain *window)
                                               (inexact->exact (round window-x))
                                               (inexact->exact (round window-y)))
                  ;; Evaluate state transitions.
                  (begin
                    (if (in-action? window)
                        (if (not (null-pointer? (schematic_window_get_place_list *window)))
                            (match current-action-mode
                              ((or 'copy-mode
                                   'multiple-copy-mode
                                   'component-mode
                                   'paste-mode
                                   'text-mode)
                               (o_place_motion *window x y))
                              ('move-mode (o_move_motion *window x y))
                              (_ FALSE))

                            (match current-action-mode
                              ('arc-mode (o_arc_motion *window x y ARC_RADIUS))
                              ('box-mode (o_box_motion *window x y))
                              ('bus-mode (o_bus_motion *window x y))
                              ('circle-mode (o_circle_motion *window x y))
                              ('line-mode (o_line_motion *window x y))
                              ('net-mode (o_net_motion *window x y))
                              ('path-mode (o_path_motion *window x y))
                              ('picture-mode (o_picture_motion *window x y))
                              ('pin-mode (o_pin_motion *window x y))
                              ('grips-mode (o_grips_motion *window x y))
                              ('box-select-mode (o_select_box_motion *window unsnapped-x unsnapped-y))
                              ('zoom-box-mode (zoom-box-motion window unsnapped-x unsnapped-y))
                              ('select-mode (o_select_motion *window x y))
                              (_ FALSE)))

                        ;; Not inside action.
                        (match current-action-mode
                          ('net-mode (o_net_start_magnetic *window x y))
                          (_ FALSE)))

                    FALSE)))))))

  (process-canvas-event *canvas *event *window process-event))

(define *callback-motion
  (procedure->pointer int callback-motion '(* * *)))


(define (callback-enter *widget *event *window)
  ;; This will most likely optimized out by the compiler.
  ;; However the syntax check might be useful if we add anything
  ;; useful here.
  (define window (pointer->window *window))
  ;; Do nothing for now.
  FALSE)


(define *callback-enter
  (procedure->pointer int callback-enter '(* * *)))


;;; Set up GTK+ callback handlers for the *MAIN-WINDOW widget of
;;; *WINDOW.
(define (setup-main-window-draw-events *window *main-window)
  (g_signal_connect *main-window
                    (string->pointer "enter-notify-event")
                    *callback-enter
                    *window))


(define (setup-canvas-draw-events *window *canvas)
  (define signal-callback-list
    (list
     `(,(if %m4-use-gtk3 "draw" "expose-event") . ,*redraw-canvas)
     `("button-press-event" . ,*callback-button-pressed)
     `("button-release-event" . ,*callback-button-released)
     `("motion-notify-event" . ,*callback-motion)
     `("configure-event" . ,*x_event_configure)
     `("key-press-event" . ,*process-key-event)
     `("key-release-event" . ,*process-key-event)
     `("scroll-event" . ,*scroll-canvas)
     `("update-grid-info" . ,*i_update_grid_info_callback)
     `("notify::page" . ,*schematic_window_notify_page_callback)))

  (define (connect-signal element)
    (g_signal_connect *canvas
                      (string->pointer (car element))
                      (cdr element)
                      *window))

  (x_window_setup_draw_events_drawing_area *window *canvas)

  (for-each connect-signal signal-callback-list))


;;; After calling this function it may be necessary to wait to let
;;; page view creation to complete.  See process-pending-events()
;;; above.
(define (tab-add-page! *window *page)
  "Creates a new page view for *PAGE in *WINDOW and adds it to the
tab notebook.  Returns a C TabInfo structure."
  (define *wtab (gtk_scrolled_window_new %null-pointer %null-pointer))

  (x_window_setup_scrolling *window *wtab)

  (let ((*canvas (schematic_canvas_new_with_page *page)))
    (add-tab-canvas! *wtab *canvas)
    (setup-canvas-draw-events *window *canvas)
    (schematic_window_set_current_canvas *window *canvas)
    (let ((page-index (append-tab! *window *wtab)))

      (gtk_notebook_set_tab_reorderable (schematic_window_get_tab_notebook *window)
                                        *wtab
                                        TRUE)
      ;; Return TabInfo.
      (add-tab-info! *window *wtab *canvas *page page-index))))


;;; Creates and returns a new untitled page in *WINDOW.
(define (window-make-untitled-page *window)
  (define quiet-mode? (true? (get_quiet_mode)))

  (when (null-pointer? *window)
    (error "NULL window."))

  (let ((*toplevel (schematic_window_get_toplevel *window)))
    (when (null-pointer? *toplevel)
      (error "NULL toplevel."))

    ;; New page file name.
    (let* ((*filename (untitled_filename *window))
           ;; Create a new page.
           (*page (lepton_page_new *toplevel *filename)))

      ;; Switch to the new page.
      (window-set-toplevel-page! (pointer->window *window)
                                 (pointer->page *page))
      (unless quiet-mode?
        (log! 'message (G_ "New file ~S") (pointer->string *filename)))

      (g_free *filename)

      ;; Run new page hook.
      (with-window *window
        (run-hook new-page-hook (pointer->page *page)))

      ;; Save current state of the page.
      (o_undo_savestate *window *page FALSE)

      *page)))


;;; Opens a new page for *FILENAME in *WINDOW.  If there is no
;;; page for *FILENAME in toplevel's list of pages, it creates a
;;; new page, loads the file in it and returns a pointer to the
;;; new page. Otherwise it returns a pointer to the existing page.
;;; If the filename passed is %null-pointer, this function creates
;;; an empty, untitled page.  The name of the untitled page is
;;; built from configuration data ("default-filename" in the
;;; "schematic" config group) and an incrementing counter for
;;; uniqueness.  The opened page becomes the current page.
(define (window-open-file! *window *filename)
  (define *toplevel (schematic_window_get_toplevel *window))
  (define quiet-mode? (true? (get_quiet_mode)))
  (define (load-schematic-message)
    (unless quiet-mode?
      (log! 'message
            (G_ "Loading schematic ~S")
            (pointer->string *filename))))

  (define (get-gerror-message *error)
    (let ((*err (dereference-pointer *error)))
      (unless (null-pointer? *err)
        (gerror-message *err))))

  (define (gerror-error-message *error)
    (let ((msg (get-gerror-message *error)))
      (g_clear_error *error)
      msg))

  (define (*make-new-page)
    (let ((*new-page (lepton_page_new *toplevel *filename)))
      ;; Switch to the new page.  NOTE: the call sets
      ;; the current active page of toplevel.
      (window-set-toplevel-page! (pointer->window *window)
                                 (pointer->page *new-page))
      (load-schematic-message)
      (let ((*error (bytevector->pointer (make-bytevector (sizeof '*) 0))))
        ;; Try to load *FILENAME.
        (if (false? (schematic_file_open *window *new-page *filename *error))
            (let ((error-message (gerror-error-message *error)))
              (log! 'warning "~A" error-message)
              (open_page_error_dialog *window
                                      *filename
                                      (string->pointer error-message))
              ;; Loading failed.  Delete the page and open a new
              ;; blank one.
              (lepton_page_delete *toplevel *new-page)
              ;; Open and return a new blank page.
              (window-make-untitled-page *window))

            (begin
              ;; Run hook.
              (run-hook open-page-hook (pointer->page *new-page))
              ;; Add page file name to the recent file list.
              (recent_manager_add *window (lepton_page_get_filename *new-page))
              ;; Save current state of the page.
              (o_undo_savestate *window *new-page FALSE)
              ;; Return new page.
              *new-page)))))

  (when (null-pointer? *toplevel)
    (error "NULL toplevel."))

  (if (null-pointer? *filename)
      ;; New blank page requested.
      (window-make-untitled-page *window)
      ;; Try to open page for filename.
      (let ((*page (lepton_toplevel_search_page *toplevel *filename)))
        (if (null-pointer? *page)
            ;; There is no open page for filename, create a new page.
            (*make-new-page)
            ;; Return existing page if it is already loaded.
            *page))))


(define (set-window-current-page! *window *page)
  "Change the current page in *WINDOW to *PAGE, draw it, and update
the user interface.  *PAGE has to be in the list of pages of the
window."
  (define *canvas (schematic_window_get_current_canvas *window))

  (when (null-pointer? *window)
    (error "NULL window."))
  (when (null-pointer? *page)
    (error "NULL page."))
  (when (null-pointer? *canvas)
    (error "NULL canvas."))

  (o_redraw_cleanstates *window)
  (schematic_canvas_set_page *canvas *page)
  (i_update_menus *window)
  (page_select_widget_update *window)
  (schematic_multiattrib_widget_update *window))


(define (open-tab! *window *filename)
  "Creates a new page, page view and tab for *FILENAME in *WINDOW.
If *FILENAME is %null-pointer, the page will be blank.  If there
is a page with the given *FILENAME, switches to its tab.  Returns
the new or found page."
  (define (setup-tab-header *tab-info)
    (set-tab-header! (schematic_window_get_tab_notebook *window)
                     *tab-info))

  (define (open-new-page *tab-info)
    (let ((*page (window-open-file! *window *filename)))
      (schematic_tab_info_set_page *tab-info *page)
      (set-window-current-page! *window *page)

      (setup-tab-header *tab-info)
      (grab-focus *tab-info)
      ;; Finish page view creation by processing pending events.
      (process-pending-events)

      *page))

  (define (set-notebook-current *tab-info)
    (let ((*notebook (schematic_window_get_tab_notebook *window)))
      (gtk_notebook_set_current_page
       *notebook
       (gtk_notebook_page_num *notebook
                              (schematic_tab_info_get_tab_widget *tab-info)))))

  (define (open-tab-page)
    ;; Find TabInfo for a page view that is set as current
    ;; for toplevel (w_current->drawing_area):
    (let ((*current-tab-info (x_tabs_info_cur *window)))

      (when (null-pointer? *current-tab-info)
        (error "No current TabInfo found."))

      (if (null-pointer? (schematic_tab_info_get_page *current-tab-info))
          ;; The current tab info may not have an associated page
          ;; in the following cases:
          ;;   - when a first blank page is created upon startup
          ;;   - when a first cmd-line supplied page is opened
          ;;     upon startup
          ;;   - when a new page is created after the last page is
          ;;     closed
          (open-new-page *current-tab-info)

          (let ((*page (if (null-pointer? *filename)
                           %null-pointer
                           (lepton_toplevel_search_page (schematic_window_get_toplevel *window)
                                                        *filename))))
            ;; *FILENAME may be NULL when the user triggers one of
            ;; the actions:
            ;;   - File -> Open page
            ;;   - File -> New page
            (if (null-pointer? *page)
                (begin
                  ;; First we have to cancel all current actions.
                  ;; This prevents assertion triggering in
                  ;; o_place_invalidate_rubber() if File->New is
                  ;; invoked while component selection mode is
                  ;; active.
                  (callback-cancel *window)

                  ;; Create a new tab with a new page.
                  (open-new-page (tab-add-page! *window %null-pointer)))

                ;; If the page exists, switch to its existing
                ;; page view.
                (let ((*existing-tab-info
                       (x_tabs_info_find_by_page
                        (schematic_window_get_tab_info_list *window)
                        *page)))

                  (when (null-pointer? *existing-tab-info)
                    (error "NULL TabInfo for existing page."))

                  (set-notebook-current *existing-tab-info)
                  (grab-focus *existing-tab-info)

                  ;; Return the existing page.
                  *page))))))

  (when (null-pointer? *window)
    (error "NULL window pointer in open-tab!()"))

  (let ((*page (open-tab-page)))
    (if (null-pointer? *page)
        (error "open-tab!: Could not open a page for ~S" (pointer->string *filename))
        *page)))


(define (set-tab-page! *window *page)
  "Sets page of the current tab in *WINDOW to *PAGE.  If there's a
tab that contains *PAGE, it will be activated, otherwise a new tab
for *PAGE page will be created and set active."
  ;; Find a page in the list of loaded pages.
  (define (find-page *page)
    (define *toplevel (schematic_window_get_toplevel *window))
    (define page-ls
      (glist->list (lepton_list_get_glist
                    (lepton_toplevel_get_pages *toplevel))
                   pointer->page))

    (any (lambda (p) (eq? (pointer->page *page) p)) page-ls))

  (when (null-pointer? *window)
    (error "NULL window pointer in set-tab-page!()"))

  (let ((*tab-info (x_tabs_info_find_by_page (schematic_window_get_tab_info_list *window)
                                             *page))
        (page-index -1))

    ;; There is page view for *PAGE: switch to it.
    (if (not (null-pointer? *tab-info))

        (let ((page-index (gtk_notebook_page_num (schematic_window_get_tab_notebook *window)
                                                 (schematic_tab_info_get_tab_widget *tab-info))))
          (if (>= page-index 0)
              (begin
                (gtk_notebook_set_current_page (schematic_window_get_tab_notebook *window)
                                               page-index)
                (grab-focus *tab-info))
              (log! 'warning "set-tab-page!: Negative page index.")))

        ;; There is no page view for *PAGE, create a new one.
        (when (and (null-pointer? *tab-info)
                   (find-page *page))
          (let ((*tab-info (tab-add-page! *window *page)))

            (set-tab-header! (schematic_window_get_tab_notebook *window) *tab-info)
            (gtk_notebook_set_current_page (schematic_window_get_tab_notebook *window)
                                           page-index)
            (grab-focus *tab-info)

            ;; Finish page view creation by processing pending events.
            (process-pending-events)

            ;; Zoom new page view created for existing page.
            (schematic_canvas_zoom_extents (schematic_window_get_current_canvas *window)
                                           %null-pointer))))))


(define (new-active-page *toplevel *page)
  (define *pages (lepton_toplevel_get_pages *toplevel))
  ;; As it will delete current page select new current page first
  ;; look up in page hierarchy.
  (define id (lepton_page_get_up *page))
  (define *new-active-page
    (lepton_toplevel_search_page_by_id *pages id))

  (define (prev-or-next all-items item)
    (let ((rev-ls (cdr (memq item (reverse all-items)))))
      (if (null? rev-ls)
          (let ((ls (cdr (memq item all-items))))
            (and (not (null? ls))
                 (car ls)))
          (car rev-ls))))

  (if (null-pointer? *new-active-page)
      ;; When no upper page in hierarchy found, find its previous
      ;; or next page.
      (let* ((page-ls (glist->list (lepton_list_get_glist *pages)
                                   pointer->page))
             (page (pointer->page *page))
             (new-active-page (prev-or-next page-ls page)))
        (if new-active-page
            (page->pointer new-active-page)
            ;; Need to add a new untitled page.
            %null-pointer))
      ;; Found a page upper in hierarchy.
      *new-active-page))


;;; Closes *PAGE in *WINDOW.  The current page in *WINDOW is
;;; changed to the next valid page.  If necessary, a new untitled
;;; page is created (unless tabbed GUI is enabled: return NULL in
;;; that case).
(define (close-window-page! *window *page)
  (define *toplevel (schematic_window_get_toplevel *window))
  (define tabs-enabled? (true? (x_tabs_enabled)))

  (when (null-pointer? *toplevel)
    (error "NULL toplevel."))
  (when (null-pointer? *page)
    (error "NULL page."))

  ;; If we're closing whilst inside an action, re-wind the page
  ;; contents back to their state before we started.
  (when (in-action? (pointer->window *window))
    (callback-cancel *window))

  ;; *new-current-page will be the new current page at the end of
  ;; the function.
  (let ((*new-current-page
         (if (equal? *page (lepton_toplevel_get_page_current *toplevel))
             (new-active-page *toplevel *page)
             %null-pointer)))

    (log! 'message
          (if (true? (lepton_page_get_changed *page))
              (G_ "Discarding page ~S")
              (G_ "Closing ~S"))
          (pointer->string (lepton_page_get_filename *page)))

    ;; Remove the page from toplevel list of pages.
    (lepton_page_delete *toplevel *page)
    (schematic_window_page_changed *window)

    ;; Switch to a different page if we just removed the current.
    (if (null-pointer? (lepton_toplevel_get_page_current *toplevel))
        (if tabs-enabled?
            ;; If tabs are enabled, return the page as is, even if
            ;; it is NULL.
            *new-current-page
            ;; Tabs are disabled.
            (let ((*really-new-current-page
                   (if (null-pointer? *new-current-page)
                       ;; Page wasn't found, create a new page.
                       (window-make-untitled-page *window)
                       ;; Use found page.
                       *new-current-page)))
              ;; Change to the new current page and update display.
              (set-window-current-page! *window *really-new-current-page)
              ;; Return the page.
              *really-new-current-page))
        ;; Return the page if it is not NULL.
        *new-current-page)))


;;; Closes the tab of *WINDOW which contains *PAGE.  When the last
;;; tab is closed, a new tab with blank page will be opened.
(define (close-tab! *window *page)
  (when (null-pointer? *window)
    (error "NULL window in ~S()" 'close-tab!))

  (let ((*current-tab-info
         (x_tabs_info_find_by_page (schematic_window_get_tab_info_list *window)
                                   *page)))

    (when (null-pointer? *current-tab-info)
      (error "NULL TabInfo in ~S()" 'close-tab!))

    (let ((*notebook (schematic_window_get_tab_notebook *window)))

      (when (< (gtk_notebook_get_n_pages *notebook) 1)
        (error "Wrong number of tabs in ~S()" 'close-tab!))

      (let* ((*current-page (schematic_tab_info_get_page *current-tab-info))
             ;; Page to be set as current after the current page is closed.
             (*new-current-page (close-window-page! *window *current-page)))

        (close-page-tab! *window *current-page)

        (delete-tab-info! *window *current-tab-info)

        (if (null-pointer? *new-current-page)
            (begin
              (tab-add-page! *window %null-pointer)
              ;;  tab-add-page!() just invoked, but no need to process
              ;;  pending events here: it will be done in x_tabs_page_open()
              (open-tab! *window %null-pointer))
            (set-tab-page! *window *new-current-page))))))


(define (callback-tab-button-close *button *tab-info)
  (if (null-pointer? *tab-info)
      (error "NULL TabInfo pointer.")
      (let ((*window (schematic_tab_info_get_window *tab-info))
            (*page (schematic_tab_info_get_page *tab-info)))
        (set-tab-page! *window *page)

        (when (close-page-dialog (pointer->window *window)
                                 (pointer->page *page))
          (close-tab! *window *page)))))

(define *callback-tab-button-close
  (procedure->pointer void callback-tab-button-close '(* *)))


(define (callback-tab-button-save *button *tab-info)
  (when (null-pointer? *tab-info)
    (error "NULL TabInfo pointer."))

  (let ((*window (schematic_tab_info_get_window *tab-info))
        (*page (schematic_tab_info_get_page *tab-info)))

    (set-tab-page! *window *page)
    (window-save-active-page! (pointer->window *window))))


(define *callback-tab-button-save
  (procedure->pointer void callback-tab-button-save '(* *)))


;;; Go to the upper hierarchy level page.
(define (hierarchy-up *window)
  (define *page (schematic_window_get_active_page *window))

  (unless (null-pointer? *page)

    (let ((*parent (s_hierarchy_find_up_page *page)))
      (if (null-pointer? *parent)
          (log! 'message (G_ "Cannot find any schematics above the current one!"))

          (when (close-page-dialog (pointer->window *window)
                                   (pointer->page *page))
            (close-tab! *window *page)
            (set-tab-page! *window *parent))))))


(define (callback-tab-button-up *button *tab-info)
  (if (null-pointer? *tab-info)
      (error "NULL TabInfo pointer.")
      (let ((*window (schematic_tab_info_get_window *tab-info))
            (*page (schematic_tab_info_get_page *tab-info)))
        (set-tab-page! *window *page)
        (hierarchy-up *window))))

(define *callback-tab-button-up
  (procedure->pointer void callback-tab-button-up '(* *)))


;;; GtkNotebook "switch-page" signal handler.
(define (callback-tabs-switch-page *notebook *wtab id *window)
  (define *current-page (schematic_window_get_active_page *window))
  (define *current-canvas (schematic_window_get_current_canvas *window) )

  (unless (and (null-pointer? *current-page)
               (null-pointer? *current-canvas))

    (let* ((*info-list (schematic_window_get_tab_info_list *window))
           (*tab-info (get-tab-info *wtab *info-list)))

      (unless (null-pointer? *tab-info)

        (log! 'debug "callback-tabs-switch-page()")

        ;; Before changing toplevel's current page and page view,
        ;; cancel all actions that may be in progress on previous
        ;; page.
        (callback-cancel *window)

        (schematic_window_set_current_canvas *window (schematic_tab_info_get_canvas *tab-info))
        (let ((*page (schematic_tab_info_get_page *tab-info)))
          (window-set-toplevel-page! (pointer->window *window)
                                     (pointer->page *page))

          (set-window-current-page! *window *page))))))

(define *callback-tabs-switch-page
  (procedure->pointer void callback-tabs-switch-page (list '* '* int '*)))


(define (*window-set-current-page! *window *page)
  "Sets current page of *WINDOW to *PAGE."
  (if (true? (x_tabs_enabled))
      (set-tab-page! *window *page)
      (set-window-current-page! *window *page)))


;;; Return the subpages of PAGE in *WINDOW.  If any subpages are
;;; not loaded, this function will load them.
(define (page-subpages *window page)
  (define *page (page->pointer page))
  (define objects (page-contents page))

  (define (split-attrib-value object)
    (string-split (attrib-value object) #\,))

  (define (source-attrib? attrib)
    (string= "source" (attrib-name attrib)))

  (define (attached-source-attribs object)
    (filter source-attrib? (object-attribs object)))

  (define (inherited-source-attribs object)
    (filter source-attrib? (inherited-attribs object)))

  (define (source-attribs object)
    (let ((attached-attribs (attached-source-attribs object)))
      (if (null? attached-attribs)
          (inherited-source-attribs object)
          attached-attribs)))

  (define (get-subpages *page page-ls object)
    (let loop ((filenames (append-map split-attrib-value
                                      (source-attribs object)))
               (page-ls page-ls))
      (if (null? filenames)
          page-ls
          (let ((*subpage (s_hierarchy_load_subpage *window
                                                    *page
                                                    (string->pointer (car filenames))
                                                    %null-pointer)))
            (loop (cdr filenames)
                  (if (null-pointer? *subpage)
                      page-ls
                      (cons (pointer->page *subpage) page-ls)))))))

  (reverse
   (let loop ((objects (filter component? objects))
              (page-ls '()))
     (if (null? objects)
         page-ls
         (let ((new-page-ls (get-subpages *page
                                          page-ls
                                          (car objects))))
           (loop (cdr objects) new-page-ls))))))


;;; The function obtains a list of pages for an operation.  It
;;; processes the list of *PAGES in *WINDOW, descending the
;;; hierarchy of pages if DESCEND? is not #f, adding all subpages,
;;; and removing duplicate pages.  The function returns a new list
;;; of all found pages.
(define (find-text-get-all-pages *window *pages descend?)
  (define page-ls (glist->list *pages pointer->page))

  (let loop ((input-ls page-ls)
             (output-ls '()))
    (if (null? input-ls)
        ;; Return the output list.
        (reverse output-ls)

        (let* ((page (car input-ls))
               (new-input-ls (cdr input-ls))
               (page-visited? (memq page output-ls)))
          (loop (if page-visited?
                    new-input-ls
                    (if (true? descend?)
                        (append new-input-ls
                                (page-subpages *window page))
                        new-input-ls))
                (if page-visited?
                    output-ls
                    (cons page output-ls)))))))


(define (search-text *window *toplevel)
  (define show-hidden-text?
    (schematic_window_get_show_hidden_text *window))
  (define *find-text-widget
    (schematic_window_get_find_text_widget *window))
  (define *find-text-state-widget
    (schematic_window_get_find_text_state_widget *window))
  (define *pages
    (lepton_list_get_glist
     (lepton_toplevel_get_pages *toplevel)))
  (define find-type
    (schematic_find_text_widget_get_find_type *find-text-widget))
  (define *text-string
    (schematic_find_text_widget_get_find_text_string *find-text-widget))
  (define descend?
    (schematic_find_text_widget_get_descend *find-text-widget))
  (define all-pages
    (find-text-get-all-pages *window *pages descend?))
  (define *all-pages
    (let loop ((pages all-pages)
               (*pages-gslist %null-pointer))
      (if (null? pages)
          *pages-gslist
          (loop (cdr pages)
                (g_slist_prepend *pages-gslist (page->pointer (car pages)))))))
  (define count
    (schematic_find_text_state_find *window
                                    *find-text-state-widget
                                    *all-pages
                                    find-type
                                    *text-string
                                    descend?
                                    show-hidden-text?))
  (g_slist_free *all-pages)

  (if (> count 0)
      (begin
        (x_widgets_show_find_text_state *window)
        TRUE)
      FALSE))


(define (find-text *widget response *window)
  (when (null-pointer? *window)
    (error "NULL window."))

  (let ((*toplevel (schematic_window_get_toplevel *window)))
    (when (null-pointer? *toplevel)
      (error "NULL toplevel."))

    (let ((close?
           (case (gtk-response->symbol response)
             ((ok) (true? (search-text *window *toplevel)))
             ((cancel delete-event) #t)
             (else (log! 'warning "find-text(): strange-signal ~A" response)
                   #f))))
      (when close?
        (let ((*drawing-area (schematic_window_get_drawing_area *window)))
          (gtk_widget_grab_focus *drawing-area)
          (gtk_widget_hide *widget))))))

(define *callback-find-text
  (procedure->pointer void find-text (list '* int '*)))


(define (select-object *state *object *window)
  (define *canvas (schematic_window_get_current_canvas *window))
  (when (null-pointer? *canvas)
    (error "NULL canvas."))

  (when (null-pointer? *object)
    (error "NULL object."))

  (let* ((*page (schematic_canvas_get_page *canvas))
         (*object-page (lepton_object_get_page *object))
         (same-page? (equal? *page *object-page)))
    (when (null-pointer? *page)
      (error "NULL page."))

    (when (null-pointer? *object-page)
      (error "NULL object page."))

    (begin
      (unless same-page?
        ;; Open object's page.
        (set-window-current-page! *window *object-page))
      (begin
        (unless same-page?
          ;; Open object's page.
          (set-window-current-page! *window *object-page))
        ;; In tabbed GUI the current canvas may be
        ;; different.
        (let ((*current-canvas (if same-page?
                                   *canvas
                                   (schematic_window_get_current_canvas *window))))
          (schematic_canvas_zoom_object *current-canvas *object))))))

(define *callback-select-object
  (procedure->pointer void select-object '(* * *)))


(define (callback-page-manager-selection-changed *selection *widget)
  (define *page
    (pagesel_callback_selection_changed *selection *widget))

  (*window-set-current-page!
   (schematic_page_select_widget_get_window *widget) *page))


(define *callback-page-manager-selection-changed
  (procedure->pointer void callback-page-manager-selection-changed '(* *)))


;;; Evaluate macro defined in the C string *MACRO-TEXT.  Output
;;; the macro and the result of its evaluation to log.
(define (eval-macro-string! *window *macro-text)
  (with-window
   *window
   (let ((result
          (eval-string-protected (pointer->string *macro-text))))
     (unless (eq? result 'error)
       (log! 'message (format #f "~A" result))))))


;;; Hide the Macro widget.
(define (hide-macro-widget *widget)
  (when (null-pointer? *widget)
    (error "NULL widget."))
  (let ((*window (schematic_macro_widget_get_window *widget)))
    (when (null-pointer? *window)
      (error "NULL window."))
    (let ((*drawing_area (schematic_window_get_drawing_area *window)))
      (gtk_widget_hide *widget)
      (gtk_widget_grab_focus *drawing_area))))


;;; Show the Macro widget.
(define (show-macro-widget *widget)
  (when (null-pointer? *widget)
    (error "NULL widget."))
  (let ((*entry (schematic_macro_widget_get_entry *widget)))
    (when (null-pointer? *entry)
      (error "NULL entry widget."))
    (gtk_widget_show *widget)
    (gtk_widget_grab_focus *entry)))


;;; Eval the Guile code passed to *MACRO-WIDGET in the *TEXT
;;; argument.
(define (exec-macro! *macro-widget *text)
  (when (null-pointer? *macro-widget)
    (error "NULL widget."))
  (let ((*store (schematic_macro_widget_get_store *macro-widget))
        (*window (schematic_macro_widget_get_window *macro-widget)))
    (when (null-pointer? *store)
      (error "NULL list store."))
    (when (null-pointer? *window)
      (error "NULL window."))

    (unless (or (null-pointer? *text)
                (zero? (string-length (pointer->string *text))))
      ;; Save the history and hide the Macro widget BEFORE
      ;; evaluating the macro code provided by the user, since
      ;; that code may terminate the program.
      (schematic_macro_widget_add_history *store *text)
      (schematic_macro_widget_truncate_history *store)
      (schematic_macro_widget_save_history *store)
      ;; Hide the widget and go to the canvas.
      (hide-macro-widget *macro-widget)
      ;; Evaluate the provided macro string.
      (eval-macro-string! *window *text))))


;;; Callback for when the user presses Enter in the entry widget.
(define (activate-macro-widget-entry *entry *widget)
  (when (null-pointer? *widget)
    (error "NULL widget."))
  ;; gtk_entry_get_text_length() returns guint16.
  (if (zero? (gtk_entry_get_text_length *entry))
      (hide-macro-widget *widget)
      (let ((*text (gtk_entry_get_text *entry)))
        (exec-macro! *widget *text))))

(define *callback-activate-macro-widget-entry
  (procedure->pointer void activate-macro-widget-entry '(* *)))


;;; Callback for when the user clicks the Cancel button.
(define (click-macro-widget-cancel-button *button *widget)
  (when (null-pointer? *widget)
    (error "NULL widget."))
  (hide-macro-widget *widget))

(define *callback-click-macro-widget-cancel-button
  (procedure->pointer void click-macro-widget-cancel-button '(* *)))


;;; Callback for when the user clicks the Evaluate button.
(define (click-macro-widget-evaluate-button *button *widget)
  (when (null-pointer? *widget)
    (error "NULL widget."))
  (let* ((*entry (schematic_macro_widget_get_entry *widget))
         (*text (gtk_entry_get_text *entry)))
    (exec-macro! *widget *text)))

(define *callback-click-macro-widget-evaluate-button
  (procedure->pointer void click-macro-widget-evaluate-button '(* *)))


;;; GtkEntry's "text" property change notification signal handler.
(define (notify-macro-widget-entry-text *entry *param-spec *widget)
  (when (null-pointer? *widget)
    (error "NULL widget."))
  (let ((*evaluate-button
         (schematic_macro_widget_get_evaluate_button *widget))
        ;; gtk_entry_get_text_length() returns guint16.
        (len (gtk_entry_get_text_length *entry)))
    ;; Update the sensitivity of the evaluate button.
    (gtk_widget_set_sensitive *evaluate-button
                              (if (zero? len) FALSE TRUE))))

(define *callback-notify-macro-widget-entry-text
  (procedure->pointer void notify-macro-widget-entry-text '(* * *)))


(define (make-macro-widget *window *work-box)
  "Create the Macro widget for *WINDOW and pack it in *WORK-BOX."
  (define *widget (schematic_macro_widget_new *window))

  (schematic_window_set_macro_widget *window *widget)
  (gtk_box_pack_start *work-box *widget FALSE FALSE 0)

  (let ((*entry (schematic_macro_widget_get_entry *widget))
        (*cancel-button (schematic_macro_widget_get_cancel_button *widget))
        (*evaluate-button (schematic_macro_widget_get_evaluate_button *widget)))
    (g_signal_connect *entry
                      (string->pointer "activate")
                      *callback-activate-macro-widget-entry
                      *widget)
    (g_signal_connect *cancel-button
                      (string->pointer "clicked")
                      *callback-click-macro-widget-cancel-button
                      *widget)
    (g_signal_connect *evaluate-button
                      (string->pointer "clicked")
                      *callback-click-macro-widget-evaluate-button
                      *widget)
    (g_signal_connect *entry
                      (string->pointer "notify::text")
                      *callback-notify-macro-widget-entry-text
                      *widget)))


;;; Creates and returns a scrolled canvas widget in the working area
;;; *WORK-BOX of *WINDOW This function is used when tabs are disabled.
(define (make-canvas *window *work-box)
  (when (null-pointer? *window)
    (error "NULL window."))
  (when (null-pointer? *work-box)
    (error "NULL work box."))

  ;; scrolled window (parent of page view):
  (let ((*scrolled
         (gtk_scrolled_window_new %null-pointer %null-pointer)))
    (gtk_container_add *work-box *scrolled)

    ;; Create page view.
    (x_window_create_drawing *scrolled *window)
    (x_window_setup_scrolling *window *scrolled)

    (schematic_window_get_current_canvas *window)))


;;; Shows widgets in *MAIN-WINDOW of *WINDOW, sets visibility of
;;; right and bottom notebooks, and focuses to the drawing area.
(define (show-main-window *window *main-window)
  (when (null-pointer? *window)
    (error "NULL window."))
  (when (null-pointer? *main-window)
    (error "NULL main window."))

  ;; Show all widgets.
  (gtk_widget_show_all *main-window)

  (unless (true? (x_widgets_use_docks))
    (let ((*bottom-notebook
           (schematic_window_get_bottom_notebook *window))
          (*right-notebook
           (schematic_window_get_right_notebook *window)))
      (gtk_widget_set_visible *right-notebook FALSE)
      (gtk_widget_set_visible *bottom-notebook FALSE)))

  (let ((*drawing-area (schematic_window_get_drawing_area *window)))
    ;; Focus canvas.
    (gtk_widget_grab_focus *drawing-area)))


;;; Creates and initializes a new lepton-schematic window based on
;;; data stored in the context *TOPLEVEL.
(define (make-window *toplevel)
  (define *window (schematic_window_new))

  (schematic_window_set_toplevel *window *toplevel)

  (lepton_object_add_change_notify *toplevel
                                   *o_invalidate
                                   *o_invalidate
                                   *window)
  ;; Initialize tabbed GUI.
  (x_tabs_init)

  *window)


(define (translate-response *widget response-id *window)
  (x_window_translate_response *widget response-id *window))

(define *callback-translate-response
  (procedure->pointer void translate-response (list '* int '*)))


(define (make-schematic-window *app *toplevel)
  "Creates a new lepton-schematic window.  APP is a pointer to the
GtkApplication structure of the program (when compiled with
--with-gtk3).  TOPLEVEL is a foreign LeptonToplevel structure."
  (define (setup-window *window)
    (let ((*toplevel (schematic_window_get_toplevel *window)))
      ;; Immediately setup user params.
      (i_vars_set *window)

      ;; Initialize the autosave callback.
      (lepton_toplevel_init_autosave *toplevel)

      ;; Initialize the clipboard callback.
      (x_clipboard_init *window)

      ;; Add to the list of windows.
      (add-window! (pointer->window *window))

      ;; Return the window.
      *window))

  (define *window
    (setup-window (make-window (parse-gschemrc *toplevel))))

  (let ((*main-window (schematic_window_create_app_window *app)))
    (g_signal_connect *main-window
                      (string->pointer "delete-event")
                      *callback-close-schematic-window
                      *window)

    (let ((*main-box (schematic_window_create_main_box *main-window))
          (*menubar (make-main-menu *window *callback-recent-chooser-item-activated))
          (*work-box (schematic_window_create_work_box)))
      (schematic_window_create_menubar *window *main-box *menubar)

      ;; Make toolbar.
      (let ((create-toolbar?
             (config-boolean (path-config-context (getcwd))
                             "schematic.gui"
                             "toolbars")))
        (when create-toolbar?
          (schematic_window_set_toolbar *window
                                        (make-toolbar *window *main-box))))
      ;; Make main popup menu.
      (schematic_window_create_main_popup_menu *window)

      (if (true? (x_tabs_enabled))
          (let ((*notebook (make-tabs-notebook *window *work-box)))
            (schematic_tabs_set_callback (string->pointer "page-close")
                                         *callback-tab-button-close)
            (schematic_tabs_set_callback (string->pointer "file-save")
                                         *callback-tab-button-save)
            (schematic_tabs_set_callback (string->pointer "hierarchy-up")
                                         *callback-tab-button-up)
            (g_signal_connect *notebook
                              (string->pointer "switch-page")
                              *callback-tabs-switch-page
                              *window)
            (g_signal_connect *notebook
                              (string->pointer "page-reordered")
                              *callback-page-reordered
                              *window)
            (tab-add-page! *window %null-pointer))

          (let ((*canvas (make-canvas *window *work-box)))
            ;; Setup callbacks for page view draw events.
            (setup-canvas-draw-events *window *canvas)))


      ;; Setup callbacks for main window draw events.
      (setup-main-window-draw-events *window *main-window)

      ;; Setup hidden infowidgets.
      (let ((*find-text-widget (schematic_find_text_widget_new)))
        (schematic_window_set_find_text_widget *window *find-text-widget)
        (schematic_window_pack_widget *work-box *find-text-widget)
        (g_signal_connect *find-text-widget
                          (string->pointer "response")
                          *callback-find-text
                          *window)
        (let ((*entry (schematic_find_text_widget_get_entry *find-text-widget))
              (*combo (schematic_find_text_widget_get_combo *find-text-widget))
              (*cancel-button (schematic_find_text_widget_get_cancel_button *find-text-widget))
              (*find-button (schematic_find_text_widget_get_find_button *find-text-widget)))
          (g_signal_connect *entry
                            (string->pointer "activate")
                            *schematic_find_text_widget_activate_entry
                            *find-text-widget)
          (g_signal_connect *entry
                            (string->pointer "notify::text")
                            *schematic_find_text_widget_notify_entry_text
                            *find-text-widget)
          (g_signal_connect *combo
                            (string->pointer "changed")
                            *schematic_find_text_widget_changed_type
                            *find-text-widget)
          (g_signal_connect *cancel-button
                            (string->pointer "clicked")
                            *schematic_find_text_widget_click_cancel
                            *find-text-widget)
          (g_signal_connect *find-button
                            (string->pointer "clicked")
                            *schematic_find_text_widget_click_find
                            *find-text-widget)))
      (schematic_window_create_hide_text_widget *window *work-box)
      (schematic_window_create_show_text_widget *window *work-box)
      (make-macro-widget *window *work-box)
      (let ((*translate-widget
             (schematic_window_create_translate_widget *window *work-box)))
        (g_signal_connect *translate-widget
                          (string->pointer "response")
                          *callback-translate-response
                          *window))
      ;; Setup various widgets.
      (x_widgets_init)
      (schematic_window_set_object_properties_widget *window
                                                     (schematic_object_properties_widget_new *window))
      (schematic_window_set_text_properties_widget *window
                                                   (schematic_text_properties_widget_new *window))
      (schematic_window_set_options_widget *window
                                           (schematic_options_widget_new *window))
      (schematic_window_set_log_widget *window
                                       (schematic_log_widget_new))
      (let ((*find-text-state-widget (schematic_find_text_state_new)))
        (schematic_window_set_find_text_state_widget *window *find-text-state-widget)
        (g_signal_connect *find-text-state-widget
                          (string->pointer "select-object")
                          *callback-select-object
                          *window)
        ;; Attach signal to detect user selection.
        (let ((*selection
               (schematic_find_text_state_get_selection *find-text-state-widget)))
          (g_signal_connect *selection
                            (string->pointer "changed")
                            *schematic_find_text_state_select
                            *find-text-state-widget)))
      (schematic_window_set_color_edit_widget *window
                                              (color_edit_widget_new *window))
      (schematic_window_set_font_select_widget *window
                                               (font_select_widget_new *window))
      (schematic_page_select_widget_set_callback (string->pointer "file-new")
                                                 *callback-file-new)
      (schematic_page_select_widget_set_callback (string->pointer "file-open")
                                                 *callback-file-open)
      (schematic_page_select_widget_set_callback (string->pointer "file-save")
                                                 *callback-file-save)
      (schematic_page_select_widget_set_callback (string->pointer "page-close")
                                                 *callback-page-close)
      (schematic_page_select_widget_set_callback (string->pointer "selection-changed")
                                                 *callback-page-manager-selection-changed)
      (schematic_window_set_page_select_widget *window
                                               (page_select_widget_new *window))

      ;; Setup layout of notebooks.
      (schematic_window_create_notebooks *window *main-box *work-box)

      ;; Setup statusbar.
      (schematic_window_create_statusbar *window *main-box)

      (schematic_window_restore_geometry *window *main-window)

      (show-main-window *window *main-window)
      ;; Returns *window.
      (schematic_window_set_main_window *window *main-window)))

  (pointer->window *window))


(define (active-page)
  "Returns the page which is active in the current
lepton-schematic window.  If there is no active page, returns #f."
  (let ((*page (lepton_toplevel_get_page_current
                (toplevel->pointer (current-toplevel)))))
    (and (not (null-pointer? *page))
         (pointer->page *page))))


(define (set-active-page! page)
  "Sets the page which is active in the current lepton-schematic
window to PAGE.  Returns PAGE."
  (define *window
    (or (and=> (current-window) window->pointer)
        (error "~S: Current window is unavailable." 'set-active-page!)))

  (define *page (check-page page 1))

  (*window-set-current-page! *window *page)
  page)


(define (window-close-page! window page)
  "Closes PAGE of WINDOW."
  (define *window (check-window window 1))
  (define *page (check-page page 2))
  (define tabs-enabled? (true? (x_tabs_enabled)))
  (if tabs-enabled?
      (close-tab! *window *page)
      (close-window-page! *window *page)))


(define (close-page! page)
  "Closes PAGE."
  (define *page (check-page page 1))
  (define *window
    (or (and=> (current-window) window->pointer)
        (error "~S: Current window is unavailable." 'close-page!)))

  ;; Currently active page.
  (define *active_page
    (schematic_window_get_active_page *window))

  (if (eq? page (active-page))
      (window-close-page! (current-window) (active-page))
      ;; If the page is not active, make it active and close, then
      ;; switch back to the previously active page.
      (begin
        (*window-set-current-page! *window *page)
        (window-close-page! (current-window)
                             (pointer->page (schematic_window_get_active_page *window)))
        (*window-set-current-page! *window *active_page)))

  ;; Return value is unspecified.
  (if #f #f))


(define (window-open-page! window filename)
  "Loads file FILENAME and opens a new page for it.  If FILENAME is
#f, opens a new untitled page.  Returns the new page."
  (define *window (check-window window 1))
  (define *filename
    (if filename
        (and (check-string filename 1)
             (string->pointer filename))
        %null-pointer))

  (define *page
    (if (true? (x_tabs_enabled))
        (open-tab! *window *filename)
        (window-open-file! *window *filename)))

  (unless (or (null-pointer? *filename)
              (null-pointer? *page))
    ;; Check for symbol version changes, display an error dialog
    ;; box, if necessary.
    (major_changed_dialog *window))

  (pointer->page *page))


(define (window-set-current-page! window page)
  "Sets current page of WINDOW to PAGE."
  (define *window (check-window window 1))
  (define *page (check-page page 2))
  (*window-set-current-page! *window *page))


(define (window-canvas window)
  "Return the <canvas> object of WINDOW."
  (define *window (check-window window 1))

  (pointer->canvas (schematic_window_get_current_canvas *window)))


(define (snap-point point)
  "Snaps POINT in the form (X . Y) to the snap grid, returning the
snapped point position as a pair in the same form.  This always
snaps the given point to the grid, disregarding the current user
snap settings."
  (define *window
    (or (and=> (current-window) window->pointer)
        (error "~S: Current window is unavailable." 'snap-point)))

  ;; Mimic snap_grid() when snapping is not off.
  (define (snap-grid coord)
    (lepton_coord_snap coord
                       (schematic_options_get_snap_size
                        (schematic_window_get_options *window))))

  (check-coord point 1)

  (cons (snap-grid (car point)) (snap-grid (cdr point))))
