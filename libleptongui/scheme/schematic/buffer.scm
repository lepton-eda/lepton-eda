;;; Lepton EDA Schematic Capture
;;; Scheme API
;;; Copyright (C) 2023 Lepton EDA Contributors
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


(define-module (schematic buffer)
  #:use-module (lepton ffi glib)
  #:use-module (lepton object foreign)

  #:use-module (schematic ffi)
  #:use-module (schematic hook)
  #:use-module (schematic window foreign)
  #:use-module (schematic window global)

  #:export (window-selection->buffer
            window-selection->buffer!))


;;; Defined in globals.h.
(define CLIPBOARD_BUFFER 0)
(define MAX_BUFFERS 5)


(define (check-buffer-number num)
  (when (or (< num 0)
            (> num MAX_BUFFERS))
    (error "Wrong buffer number.")))


(define (window-selection->buffer window buffer-number)
  "Copy the current WINDOW selection into a buffer with given
BUFFER-NUMBER."
  (define *window (check-window window 1))

  (check-buffer-number buffer-number)

  (schematic_buffer_from_selection *window buffer-number)

  (with-window *window
   (run-hook copy-objects-hook
             (glist->list (schematic_buffer_get_objects buffer-number)
                          pointer->object)))

  (when (= buffer-number CLIPBOARD_BUFFER)
    (x_clipboard_set *window
                     (schematic_buffer_get_objects buffer-number))))


(define (window-selection->buffer! window buffer-number)
  "Cut the current WINDOW selection into a buffer with given
BUFFER-NUMBER."
  (define *window (check-window window 1))

  (check-buffer-number buffer-number)

  (schematic_buffer_from_selection *window buffer-number)

  (o_delete_selected *window)

  (when (= buffer-number CLIPBOARD_BUFFER)
    (x_clipboard_set *window
                     (schematic_buffer_get_objects buffer-number))))
