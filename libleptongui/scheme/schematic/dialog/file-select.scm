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
  #:use-module (schematic window)
  #:use-module (schematic window foreign)

  #:export (file-select-dialog))


(define (file-select-dialog window)
  "Opens file selection dialog in WINDOW.  Loads selected files as
pages.  The current page of the window is set to the page of the
last loaded page."
  (define *window (check-window window 1))

  (define filenames
    (gslist->list (x_fileselect_open *window) pointer->string 'free))

  ;; Open each file.
  (define pages
    (map (lambda (filename) (window-open-page! window filename))
         filenames))

  ;; Switch to the last page opened.
  (unless (null? pages)
    (window-set-current-page! window (last pages))))
