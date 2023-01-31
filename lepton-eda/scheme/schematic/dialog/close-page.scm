;;; Lepton EDA Schematic Capture
;;; Scheme API
;;; Copyright (C) 2023-2026 Lepton EDA Contributors
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


(define-module (schematic dialog close-page)
  #:use-module (lepton ffi boolean)
  #:use-module (lepton page foreign)

  #:use-module (schematic ffi)
  #:use-module (schematic window foreign)
  #:use-module (schematic window page)

  #:export (close-page-dialog))

(define (close-page-dialog window page)
  (define *window (check-window window 1))
  (define *page (check-page page 2))
  (define active-page
    (pointer->page (schematic_window_get_active_page *window)))
  (define result
    (true? (x_dialog_close_changed_page *window *page)))

  ;; Switch back to the page we were on if it wasn't the one being
  ;; closed.
  (when (and active-page
             (not (eq? active-page page)))
    (window-set-toplevel-page! window active-page))

  result)
