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

  #:use-module (lepton ffi)
  #:use-module (lepton gettext)
  #:use-module (lepton log)

  #:use-module (schematic ffi)
  #:use-module (schematic window foreign)

  #:export (callback-file-new
            *callback-file-new))


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
