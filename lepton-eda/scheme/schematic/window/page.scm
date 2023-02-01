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

(define-module (schematic window page)
  #:use-module (system foreign)

  #:use-module (lepton ffi boolean)
  #:use-module (lepton ffi)
  #:use-module (lepton page foreign)

  #:use-module (schematic ffi)
  #:use-module (schematic window foreign)

  #:export (window-set-toplevel-page!
            window-save-active-page!))

(define (window-set-toplevel-page! window page)
  (define *window (check-window window 1))
  (define *page (check-page page 2))
  (define *toplevel (schematic_window_get_toplevel *window))

  (lepton_toplevel_goto_page *toplevel *page)
  (schematic_window_page_changed *window))


(define (window-save-active-page! window)
  (define *window (check-window window 1))
  (define *page (schematic_window_get_active_page *window))

  (unless (null-pointer? *page)
    (if (true? (x_window_untitled_page *page))
        ;; Open "Save as..." dialog.
        (x_fileselect_save *window *page %null-pointer)
        ;; Save page.
        (x_window_save_page *window *page (lepton_page_get_filename *page)))))
