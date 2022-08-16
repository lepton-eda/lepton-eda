;; Lepton EDA Schematic Capture
;; Scheme API
;; Copyright (C) 2010-2011 Peter Brett <peter@peter-b.co.uk>
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
;;

(define-module (schematic window)
  #:use-module (ice-9 format)
  #:use-module (rnrs bytevectors)
  #:use-module (system foreign)

  #:use-module (lepton config)
  #:use-module (lepton ffi boolean)
  #:use-module (lepton ffi check-args)
  #:use-module (lepton ffi glib)
  #:use-module (lepton ffi)
  #:use-module (lepton gettext)
  #:use-module (lepton log)
  #:use-module (lepton page foreign)
  #:use-module (lepton page)
  #:use-module (lepton toplevel foreign)
  #:use-module (lepton toplevel)

  #:use-module (schematic callback)
  #:use-module (schematic ffi)
  #:use-module (schematic ffi gtk)
  #:use-module (schematic gui keymap)
  #:use-module (schematic menu)
  #:use-module (schematic toolbar)
  #:use-module (schematic window foreign)
  #:use-module (schematic window global)
  #:use-module (schematic window list)

  #:re-export (%lepton-window
               current-window
               with-window)

  #:export (close-window!
            make-schematic-window
            active-page
            set-active-page!
            pointer-position
            snap-point
            window-close-page!
            window-open-page!
            window-set-current-page!
            *window-set-current-page!)

  ;; Overrides the close-page! procedure in the (lepton page)
  ;; module.
  #:replace (close-page!))


(define (process-key-event *page_view *event *window)
  (with-window *window
    (eval-press-key-event *event *page_view *window)))

(define *process-key-event
  (procedure->pointer int process-key-event '(* * *)))


(define (close-window! window)
  "Closes WINDOW."
  (define *window (window->pointer window))

  (let ((last-window? (= (length (schematic-windows)) 1)))
    ;; If we're closing whilst inside an action, re-wind the page
    ;; contents back to their state before we started.
    (when (true? (schematic_window_get_inside_action *window))
      (i_callback_cancel %null-pointer *window))

    ;; Last chance to save possible unsaved pages.
    (when (true? (x_dialog_close_window *window))
      ;; Close the window if the user didn't cancel the close.
      (x_clipboard_finish *window)
      (let ((*cswindow (schematic_window_get_compselect_widget *window))
            (*tiwindow (schematic_window_get_text_input_widget *window))
            (*aawindow (schematic_window_get_arc_edit_widget *window))
            (*aewindow (schematic_window_get_attrib_edit_widget *window))
            (*hkwindow (schematic_window_get_hotkey_widget *window))
            (*cowindow (schematic_window_get_coord_widget *window))
            (*sewindow (schematic_window_get_slot_edit_widget *window)))

        (schematic_window_set_dont_invalidate *window TRUE)

        (x_widgets_destroy_dialogs *window)

        ;; Close all the dialog boxes.
        (unless (null-pointer? *cswindow) (gtk_widget_destroy *cswindow))
        (unless (null-pointer? *tiwindow) (gtk_widget_destroy *tiwindow))
        (unless (null-pointer? *aawindow) (gtk_widget_destroy *aawindow))
        (x_multiattrib_close *window)
        (unless (null-pointer? *aewindow) (gtk_widget_destroy *aewindow))
        (unless (null-pointer? *hkwindow) (gtk_widget_destroy *hkwindow))
        (unless (null-pointer? *cowindow) (gtk_widget_destroy *cowindow))
        (unless (null-pointer? *sewindow) (gtk_widget_destroy *sewindow))


        ;; Check if the window is the last one and do jobs that have
        ;; to be done before freeing its memory.
        (when last-window?
          ;; Save window geometry.
          (schematic_window_save_geometry *window)
          ;; Close the log file.
          (s_log_close)
          ;; free the buffers.
          (o_buffer_free *window)))

      ;; Destroy main widget of the window.
      (gtk_widget_destroy (schematic_window_get_main_window *window))
      (remove-window! (pointer->window *window))
      (gschem_toplevel_free *window)

      ;; Just closed last window, so quit.
      (when (zero? (length (schematic-windows)))
        ;; Clean up all memory objects allocated during the
        ;; lepton-schematic runtime.

        ;; Save cache config on exit.
        (config-save! (cache-config-context))
        (s_clib_free)
        (s_attrib_free)
        (x_stroke_free)
        (o_undo_cleanup)

        ;; Check whether the main loop is running.
        (if (zero? (gtk_main_level))
            (primitive-exit 0)
            (gtk_main_quit))))))


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


(define (callback-page-reordered *notebook *widget-tab new-index *window)
  (x_tabs_page_on_reordered *notebook *widget-tab new-index *window)
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
  (schematic_page_view_grab_focus
   (schematic_tab_info_get_page_view *tab-info)))


;;; After calling this function it may be necessary to wait to let
;;; page view creation to complete.  See process-pending-events()
;;; above.
(define (tab-add-page! *window *page)
  "Creates a new page view for *PAGE in *WINDOW and adds it to the
tab notebook.  Returns a C TabInfo structure."
  (define *wtab (gtk_scrolled_window_new %null-pointer %null-pointer))

  (let ((*page-view (x_tabs_pview_create *window *page *wtab)))
    (x_tabs_tl_pview_cur_set *window *page-view)
    (let ((page-index (x_tabs_nbook_page_add *window *page *page-view *wtab)))

      (gtk_notebook_set_tab_reorderable (schematic_window_get_tab_notebook *window)
                                        *wtab
                                        TRUE)
      ;; Return TabInfo.
      (x_tabs_info_add *window page-index *page *page-view *wtab))))


(define (open-tab! *window *filename)
  "Creates a new page, page view and tab for *FILENAME in *WINDOW.
If *FILENAME is %null-pointer, the page will be blank.  If there
is a page with the given *FILENAME, switches to its tab.  Returns
the new or found page."
  (define (setup-tab-header *tab-info)
    (x_tabs_hdr_set (schematic_window_get_tab_notebook *window)
                    *tab-info))

  (define (open-new-page *tab-info)
    (let ((*page (x_window_open_page *window *filename)))
      (schematic_tab_info_set_page *tab-info *page)
      (x_window_set_current_page *window *page)

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
                           (lepton_toplevel_search_page (gschem_toplevel_get_toplevel *window)
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
                  (x_tabs_cancel_all *window)

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
                   (true? (x_tabs_tl_page_find *window *page)))
          (let ((*tab-info (tab-add-page! *window *page)))

            (x_tabs_hdr_set (schematic_window_get_tab_notebook *window) *tab-info)
            (gtk_notebook_set_current_page (schematic_window_get_tab_notebook *window)
                                           page-index)
            (grab-focus *tab-info)

            ;; Finish page view creation by processing pending events.
            (process-pending-events)

            ;; Zoom new page view created for existing page.
            (gschem_page_view_zoom_extents (x_tabs_tl_pview_cur *window)
                                           %null-pointer))))))


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
             (*new-current-page (x_window_close_page *window *current-page)))

        (x_tabs_nbook_page_close *window *current-page)

        (x_tabs_info_rm *window *current-tab-info)

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

        (unless (and (true? (lepton_page_get_changed *page))
                     (not (true? (x_dialog_close_changed_page *window *page))))
          (close-tab! *window *page)))))

(define *callback-tab-button-close
  (procedure->pointer void callback-tab-button-close '(* *)))


(define (callback-tab-button-save *button *tab-info)
  (when (null-pointer? *tab-info)
    (error "NULL TabInfo pointer."))

  (let ((*window (schematic_tab_info_get_window *tab-info))
        (*page (schematic_tab_info_get_page *tab-info)))

    (set-tab-page! *window *page)
    (i_callback_file_save %null-pointer *window)))


(define *callback-tab-button-save
  (procedure->pointer void callback-tab-button-save '(* *)))


;;; Go to the upper hierarchy level page.
(define (hierarchy-up *window)
  (define *page (schematic_window_get_active_page *window))

  (unless (null-pointer? *page)

    (let ((*parent (s_hierarchy_find_up_page *page)))
      (if (null-pointer? *parent)
          (log! 'message (G_ "Cannot find any schematics above the current one!"))

          (unless (and (true? (lepton_page_get_changed *page))
                       (not (true? (x_dialog_close_changed_page *window *page))))
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


(define (*window-set-current-page! *window *page)
  "Sets current page of *WINDOW to *PAGE."
  (if (true? (x_tabs_enabled))
      (set-tab-page! *window *page)
      (x_window_set_current_page *window *page)))


(define (callback-page-manager-selection-changed *selection *widget)
  (define *page
    (pagesel_callback_selection_changed *selection *widget))

  (*window-set-current-page!
   (schematic_page_select_widget_get_window *widget) *page))


(define *callback-page-manager-selection-changed
  (procedure->pointer void callback-page-manager-selection-changed '(* *)))


(define (make-schematic-window *app *toplevel)
  "Creates a new lepton-schematic window.  APP is a pointer to the
GtkApplication structure of the program (when compiled with
--with-gtk3).  TOPLEVEL is a foreign LeptonToplevel structure."
  (define (setup-window *window)
    (let ((*toplevel (gschem_toplevel_get_toplevel *window)))
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
    (setup-window (x_window_new (parse-gschemrc *toplevel))))

  (let ((*main-window (schematic_window_create_app_window *app)))
    (schematic_signal_connect *main-window
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

      ;; Set up key event processing function.
      (schematic_window_set_key_event_callback *process-key-event)

      (if (true? (x_tabs_enabled))
          (let ((*notebook (x_tabs_nbook_create *window *work-box)))
            (schematic_tabs_set_callback (string->pointer "page-close")
                                         *callback-tab-button-close)
            (schematic_tabs_set_callback (string->pointer "file-save")
                                         *callback-tab-button-save)
            (schematic_tabs_set_callback (string->pointer "hierarchy-up")
                                         *callback-tab-button-up)
            (schematic_signal_connect *notebook
                                      (string->pointer "switch-page")
                                      *x_tabs_page_on_sel
                                      *window)
            (schematic_signal_connect *notebook
                                      (string->pointer "page-reordered")
                                      *callback-page-reordered
                                      *window)
            (tab-add-page! *window %null-pointer))

          (let ((*page-view (schematic_window_create_page_view *window *work-box)))
            ;; Setup callbacks for page view draw events.
            (x_window_setup_draw_events_drawing_area *window *page-view)))


      ;; Setup callbacks for main window draw events.
      (x_window_setup_draw_events_main_wnd *window *main-window)

      ;; Setup hidden infowidgets.
      (schematic_window_create_find_text_widget *window *work-box)
      (schematic_window_create_hide_text_widget *window *work-box)
      (schematic_window_create_show_text_widget *window *work-box)
      (schematic_window_create_macro_widget *window *work-box)
      (schematic_window_create_translate_widget *window *work-box)

      ;; Setup various widgets.
      (x_widgets_init)
      (schematic_window_set_object_properties_widget *window
                                                     (gschem_object_properties_widget_new *window))
      (schematic_window_set_text_properties_widget *window
                                                   (gschem_text_properties_widget_new *window))
      (schematic_window_set_options_widget *window
                                           (gschem_options_widget_new *window))
      (schematic_window_set_log_widget *window
                                       (gschem_log_widget_new))
      (schematic_window_set_find_text_state_widget *window (gschem_find_text_state_new))
      (schematic_signal_connect (schematic_window_get_find_text_state_widget *window)
                                (string->pointer "select-object")
                                *x_window_select_object
                                *window)
      (schematic_window_set_color_edit_widget *window
                                              (color_edit_widget_new *window))
      (schematic_window_set_font_select_widget *window
                                               (font_select_widget_new *window))
      (schematic_page_select_widget_set_callback (string->pointer "file-new")
                                                 *callback-file-new)
      (schematic_page_select_widget_set_callback (string->pointer "file-open")
                                                 *callback-file-open)
      (schematic_page_select_widget_set_callback (string->pointer "file-save")
                                                 *i_callback_file_save)
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

      (schematic_window_show_all *window *main-window)
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
      (x_window_close_page *window *page)))


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
        (x_window_open_page *window *filename)))

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


(define (pointer-position)
  "Returns the current mouse pointer position, expressed in world
coordinates in the form (X . Y).  If the pointer is outside the
schematic drawing area, returns #f."
  (define *window
    (or (and=> (current-window) window->pointer)
        (error "~S: Current window is unavailable." 'pointer-position)))

  (define x (make-bytevector (sizeof int)))
  (define y (make-bytevector (sizeof int)))

  (let ((result (true? (x_event_get_pointer_position
                        *window
                        FALSE
                        (bytevector->pointer x)
                        (bytevector->pointer y)))))
    (and result
         (cons (bytevector-sint-ref x 0 (native-endianness) (sizeof int))
               (bytevector-sint-ref y 0 (native-endianness) (sizeof int))))))


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
                       (gschem_options_get_snap_size
                        (schematic_window_get_options *window))))

  (check-coord point 1)

  (cons (snap-grid (car point)) (snap-grid (cdr point))))
