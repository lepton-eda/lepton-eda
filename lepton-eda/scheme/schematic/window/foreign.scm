;;; Lepton EDA library - Scheme API
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


(define-module (schematic window foreign)
  #:use-module (ice-9 format)
  #:use-module (system foreign)

  #:use-module (lepton ffi check-args)
  #:use-module (lepton ffi)

  #:export (is-window?
            check-window
            window->pointer
            pointer->window))


;;; Define a wrapped pointer type.
(define-wrapped-pointer-type <window>
  is-window?
  wrap-window
  unwrap-window
  ;; Printer.
  (lambda (window port)
    (format port "#<window-0x~x>"
            (pointer-address (unwrap-window window)))))


;;; Helper transformers between the <window> type and C window
;;; pointers.
(define (window->pointer window)
  "Transforms WINDOW which should be an instance of the <window>
type into a foreign C pointer.  If WINDOW has another type, raises
a 'wrong-type-arg error."
  (if (is-window? window)
      (unwrap-window window)
      (error-wrong-type-arg 1 '<window> window)))


(define (pointer->window pointer)
  "Transforms POINTER to a <window> type instance. Raises a
'wrong-type-arg error if POINTER is not a foreign C pointer.
Raises a 'misc-error error if the pointer is a NULL pointer."
  (if (pointer? pointer)
      (if (null-pointer? pointer)
          (error "Cannot convert NULL pointer to <window>.")
          (wrap-window pointer))
      (error-wrong-type-arg 1 'pointer pointer)))


;;; Syntax rules to check <window> instances.  The same as for
;;; <object> in the module (lepton object foreign).
(define-syntax check-window
  (syntax-rules ()
    ((_ window pos)
     (let ((pointer (and (is-window? window)
                         (unwrap-window window))))
       (if (or (not pointer)
               (null-pointer? pointer))
           (error-wrong-type-arg pos '<window> window)
           pointer)))))
