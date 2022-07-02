;;; Lepton EDA Schematic Capture
;;; Scheme API
;;; Copyright (C) 2022 Lepton EDA Contributors
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
  #:use-module (system foreign)

  #:use-module (lepton config)
  #:use-module (lepton ffi boolean)
  #:use-module (lepton ffi)
  #:use-module (lepton gettext)
  #:use-module (lepton log)

  #:use-module (schematic ffi)
  #:use-module (schematic window foreign)

  #:export (callback-file-new
            *callback-file-new
            callback-file-open
            *callback-file-open
            callback-page-close
            *callback-page-close
            callback-toolbar-edit-select))


(define (callback-file-new *widget *window)
  ;; Create a new page.
  (let ((*page (x_window_open_page *window %null-pointer)))
    (if (null-pointer? *page)
        (error (G_ "Could not create a new page."))
        (begin
          (x_window_set_current_page *window *page)
          (log! 'message
                (G_ "New page created: ~S")
                (pointer->string (lepton_page_get_filename *page)))))))

(define *callback-file-new
  (procedure->pointer void callback-file-new '(* *)))


(define (callback-file-open *widget *window)
  (x_fileselect_open *window))

(define *callback-file-open
  (procedure->pointer void callback-file-open '(* *)))


(define (callback-page-close *widget *window)
  (let ((*page (schematic_window_get_active_page *window)))
    (unless (or (null-pointer? *page)
                (and (true? (lepton_page_get_changed *page))
                     (not (true? (x_dialog_close_changed_page *window *page)))))
      (x_window_close_page *window *page))))

(define *callback-page-close
  (procedure->pointer void callback-page-close '(* *)))


(define (callback-toolbar-edit-select *widget *window)
  (when (true? (schematic_toolbar_toggle_tool_button_get_active *widget))
    (unless (true? (o_invalidate_rubber *window))
      (i_callback_cancel *widget *window))
    (i_callback_edit_select *widget *window)))
