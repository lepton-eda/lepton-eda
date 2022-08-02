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


(define-module (schematic dialog file-select)
  #:use-module (srfi srfi-1)
  #:use-module (system foreign)

  #:use-module (lepton ffi glib)

  #:use-module (schematic ffi)

  #:export (file-select-open))


(define (file-select-open *window)
  "Opens file selection dialog in *WINDOW.  Loads selected files as
pages.  The current page of the window is set to the page of the
last loaded page."
  (define filenames
    (gslist->list (x_fileselect_open *window) pointer->string 'free))

  ;; Open each file.
  (define *pages
    (map
     (lambda (filename)
       (x_window_open_page *window (string->pointer filename)))
     filenames))

  ;; Switch to the last page opened.
  (unless (null? *pages)
    (x_window_set_current_page *window (last *pages))))
