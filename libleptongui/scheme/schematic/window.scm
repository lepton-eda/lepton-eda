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

  #:use-module (lepton ffi)
  #:use-module (lepton gettext)
  #:use-module (lepton log)
  #:use-module (lepton page foreign)
  #:use-module (lepton page)
  #:use-module (lepton toplevel foreign)
  #:use-module (lepton toplevel)

  #:use-module (schematic ffi)
  #:use-module (schematic gui keymap)
  #:use-module (schematic menu)
  #:use-module (schematic window foreign)

  #:export (%lepton-window
            current-window
            with-window
            make-schematic-window
            active-page
            set-active-page!
            pointer-position
            snap-point)

  ;; Overrides the close-page! procedure in the (lepton page)
  ;; module.
  #:replace (close-page!))


;;; This is a fluid that is initialized with pointer to a new
;;; lepton-schematic window when it is created.  Any Scheme
;;; callback procedure called inside the window may use the value
;;; of the fluid to reference its window, thus avoiding the need
;;; of any additional arguments.  In any window, the fluid points
;;; exactly to it.
(define %lepton-window (make-fluid))


;;; Execute forms in the dynamic context of WINDOW and its
;;; toplevel.  We have to dynwind LeptonToplevel here as well
;;; since there are functions that depend on it and should know
;;; what its current value is.
(define-syntax-rule (with-window window form form* ...)
  (with-fluids ((%lepton-window window)
                (%lepton-toplevel
                 (gschem_toplevel_get_toplevel window)))
    form form* ...))


(define (current-window)
  "Returns the <window> instance associated with the current
dynamic context."
  (and=> (fluid-ref %lepton-window) pointer->window))


(define (process-key-event *page_view *event *window)
  (with-window *window
    (eval-press-key-event *event *page_view *window)))

(define *process-key-event
  (procedure->pointer int process-key-event '(* * *)))

(define (make-pointer-to-pointer pointer)
  (define bv (make-bytevector (sizeof '*)))
  (bytevector-uint-set! bv
                        0
                        (pointer-address pointer)
                        (native-endianness)
                        (sizeof '*))
  (bytevector->pointer bv))

(define (make-schematic-window *app *toplevel)
  "Creates a new lepton-schematic window.  APP is a pointer to the
GtkApplication structure of the program (when compiled with
--with-gtk3).  TOPLEVEL is a foreign LeptonToplevel structure."
  (define *window
    (x_window_setup (x_window_new (parse-gschemrc *toplevel))))

  (let ((*main-window (schematic_window_create_app_window *app)))
    (schematic_signal_connect *main-window
                              (string->pointer "delete-event")
                              (procedure->pointer int i_callback_close_wm '(* * *))
                              *window)

    (let ((*main-box (schematic_window_create_main_box *main-window))
          (*menubar (make-main-menu *window))
          (*work-box (schematic_window_create_work_box)))
      (schematic_window_create_menubar *window *main-box *menubar)
      (let ((*toolbar (schematic_window_create_toolbar *window *main-box)))
        (schematic_window_create_toolbar_button *window
                                                *toolbar
                                                (string->pointer "document-new")
                                                (string->pointer (G_ "New"))
                                                (string->pointer (G_ "New file"))
                                                (procedure->pointer void i_callback_file_new '(* *))
                                                0)
        (schematic_window_create_toolbar_button *window
                                                *toolbar
                                                (string->pointer "document-open")
                                                (string->pointer (G_ "Open"))
                                                (string->pointer (G_ "Open file"))
                                                (procedure->pointer void i_callback_file_open '(* *))
                                                1)
        (schematic_window_create_toolbar_button *window *toolbar
                                                (string->pointer "document-save")
                                                (string->pointer (G_ "Save"))
                                                (string->pointer (G_ "Save file"))
                                                (procedure->pointer void i_callback_file_save '(* *))
                                                2)
        (schematic_window_create_toolbar_separator *toolbar 3)
        (schematic_window_create_toolbar_button *window
                                                *toolbar
                                                (string->pointer "edit-undo")
                                                (string->pointer (G_ "Undo"))
                                                (string->pointer (G_ "Undo last operation"))
                                                (procedure->pointer void i_callback_edit_undo '(* *))
                                                4)
        (schematic_window_create_toolbar_button *window
                                                *toolbar
                                                (string->pointer "edit-redo")
                                                (string->pointer (G_ "Redo"))
                                                (string->pointer (G_ "Redo last undo"))
                                                (procedure->pointer void i_callback_edit_redo '(* *))
                                                5)
        (schematic_window_create_toolbar_separator *toolbar 6)
        (schematic_window_create_toolbar_button *window
                                                *toolbar
                                                (string->pointer "insert-symbol")
                                                (string->pointer (G_ "Component"))
                                                (string->pointer (G_ "Add component...
Select library and component from list, move the mouse into main window, click to place.
Right mouse button to cancel"))
                                                (procedure->pointer void i_callback_add_component '(* *))
                                                7)

        (let* ((*radio-button
                (schematic_window_create_toolbar_radio_button (bytevector->pointer (make-bytevector (sizeof '*) 0))
                                                              *window
                                                              *toolbar
                                                              (string->pointer "insert-net")
                                                              (string->pointer (G_ "Nets"))
                                                              (string->pointer (G_ "Add nets mode
Right mouse button to cancel"))
                                                              (procedure->pointer void i_callback_toolbar_add_net '(* *))
                                                              8))
               (*radio-group (schematic_window_get_toolbar_radio_button_group *radio-button)))
          (schematic_window_set_toolbar_net *window *radio-button)

          (let* ((*radio-button
                  (schematic_window_create_toolbar_radio_button (make-pointer-to-pointer *radio-group)
                                                                *window
                                                                *toolbar
                                                                (string->pointer "insert-bus")
                                                                (string->pointer (G_ "Bus"))
                                                                (string->pointer (G_ "Add buses mode
Right mouse button to cancel"))
                                                                (procedure->pointer void i_callback_toolbar_add_bus '(* *))
                                                                9))
                 (*radio-group (schematic_window_get_toolbar_radio_button_group *radio-button)))
            (schematic_window_set_toolbar_bus *window *radio-button)

            (schematic_window_init_toolbar *window *toolbar *radio-group))))
      ;; Make main popup menu.
      (schematic_window_create_main_popup_menu *window)

      ;; Set up key event processing function.
      (schematic_window_set_key_event_callback *process-key-event)

      (if (true? (x_tabs_enabled))
          (x_tabs_create *window *work-box)
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
      (x_widgets_create *window)

      ;; Setup layout of notebooks.
      (schematic_window_create_notebooks *window *main-box *work-box)

      ;; Setup statusbar.
      (schematic_window_create_statusbar *window *main-box)

      (schematic_window_restore_geometry *window *main-window)

      (schematic_window_show_all *window *main-window)
      ;; Returns *window.
      (schematic_window_set_main_window *window *main-window))))


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

  (x_window_set_current_page *window *page)
  page)


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
      (x_window_close_page *window *page)
      ;; If the page is not active, make it active and close, then
      ;; switch back to the previously active page.
      (begin
        (x_window_set_current_page *window *page)
        (x_window_close_page *window
                             (schematic_window_get_active_page *window))
        (x_window_set_current_page *window *active_page)))

  ;; Return value is unspecified.
  (if #f #f))


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
