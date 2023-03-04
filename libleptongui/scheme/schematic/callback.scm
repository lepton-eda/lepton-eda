;;; Lepton EDA Schematic Capture
;;; Scheme API
;;; Copyright (C) 2022-2023 Lepton EDA Contributors
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.


(define-module (schematic callback)
  #:use-module (ice-9 match)
  #:use-module (system foreign)

  #:use-module (lepton config)
  #:use-module (lepton ffi boolean)
  #:use-module (lepton ffi)
  #:use-module (lepton gettext)
  #:use-module (lepton log)
  #:use-module (lepton m4)
  #:use-module (lepton page foreign)

  #:use-module (schematic action)
  #:use-module (schematic action-mode)
  #:use-module (schematic dialog file-select)
  #:use-module (schematic ffi)
  #:use-module (schematic window foreign)
  #:use-module (schematic window)

  #:export (callback-add-bus
            callback-add-component
            callback-add-net
            callback-add-text
            callback-file-new
            *callback-file-new
            callback-file-open
            *callback-file-open
            callback-page-close
            *callback-page-close
            callback-edit-select
            callback-toolbar-add-bus
            callback-toolbar-add-net
            callback-toolbar-edit-select))


(define (callback-file-new *widget *window)
  ;; Create a new page.
  (let ((*page (page->pointer (window-open-page! (pointer->window *window) #f))))
    (if (null-pointer? *page)
        (error (G_ "Could not create a new page."))
        (begin
          (*window-set-current-page! *window *page)
          (log! 'message
                (G_ "New page created: ~S")
                (pointer->string (lepton_page_get_filename *page)))))))

(define *callback-file-new
  (procedure->pointer void callback-file-new '(* *)))


(define (callback-file-open *widget *window)
  (file-select-dialog (pointer->window *window)))

(define *callback-file-open
  (procedure->pointer void callback-file-open '(* *)))


(define (callback-page-close *widget *window)
  (let ((*page (schematic_window_get_active_page *window)))
    (unless (or (null-pointer? *page)
                (and (true? (lepton_page_get_changed *page))
                     (not (true? (x_dialog_close_changed_page *window *page)))))
      (window-close-page! (pointer->window *window)
                          (pointer->page *page)))))

(define *callback-page-close
  (procedure->pointer void callback-page-close '(* *)))


(define (callback-add-bus *window)
  (o_redraw_cleanstates *window)
  (o_invalidate_rubber *window)
  (set-action-mode! 'bus-mode #:window (pointer->window *window))
  (let ((position (action-position)))
    (and position
         (match (snap-point position)
           ((x . y)
            (o_bus_start *window x y))
           (_ #f)))))


(define (callback-add-net *window)
  (o_redraw_cleanstates *window)
  (set-action-mode! 'net-mode #:window (pointer->window *window))
  (let ((position (action-position)))
    (and position
         (match (snap-point position)
           ((x . y)
            (o_net_reset *window)
            (o_net_start *window x y))
           (_ #f)))))


(define (callback-edit-select *window)
  (o_redraw_cleanstates *window)
  (set-action-mode! 'select-mode #:window (pointer->window *window))
  (i_action_stop *window))


(define (callback-toolbar-add-bus *widget *window)
  (when (true? (schematic_toolbar_toggle_tool_button_get_active *widget))
    (callback-add-bus *window)))


(define (callback-toolbar-add-net *widget *window)
  (when (true? (schematic_toolbar_toggle_tool_button_get_active *widget))
    (callback-add-net *window)))


(define (callback-toolbar-edit-select *widget *window)
  (when (true? (schematic_toolbar_toggle_tool_button_get_active *widget))
    (unless (true? (o_invalidate_rubber *window))
      (i_callback_cancel *widget *window))
    (callback-edit-select *window)))


(define (callback-add-component *widget *window)
  (define window (pointer->window *window))
  (define signal-callback-list
    (list
     (if %m4-use-gtk3
         `("draw" . ,*x_event_draw)
         `("expose-event" . ,*x_event_expose))
     `("realize" . ,*preview_callback_realize)
     `("button-press-event" . ,*preview_callback_button_press)
     `("configure-event" . ,*x_event_configure)
     `("scroll-event" . ,*preview_event_scroll)))

  (o_redraw_cleanstates *window)

  (set-action-mode! 'component-mode #:window window)
  (when (null-pointer? (schematic_window_get_compselect *window))
    (let ((*compselect-widget (schematic_compselect_new *window)))
      (schematic_signal_connect *compselect-widget
                                (string->pointer "response")
                                *x_compselect_callback_response
                                *window)
      (let ((*preview (schematic_compselect_get_preview *compselect-widget)))
        (for-each
         (lambda (element)
           (schematic_signal_connect *preview
                                     (string->pointer (car element))
                                     (cdr element)
                                     (schematic_preview_get_preview_w_current *preview)))
         signal-callback-list))
      (schematic_window_set_compselect *window *compselect-widget)))
  (x_compselect_open (schematic_window_get_compselect *window))

  (set-action-mode! 'select-mode #:window window))


(define (callback-add-text *widget *window)
  (o_redraw_cleanstates *window)
  (o_invalidate_rubber *window)

  (i_action_stop *window)
  (set-action-mode! 'select-mode #:window (pointer->window *window))

  (text_input_dialog *window))
