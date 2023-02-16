;;; Lepton EDA Schematic Capture
;;; Scheme API
;;; Copyright (C) 2023 Lepton EDA Contributors
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not write to the Free Software
;;; Foundation Inc. 51 Franklin Street Fifth Floor Boston MA 02110-1301 USA.


(define-module (schematic action edit)
  #:use-module (lepton attrib)
  #:use-module (lepton object foreign)
  #:use-module (lepton object)

  #:use-module (schematic ffi)
  #:use-module (schematic window foreign)

  #:export (edit-objects))

;; Definitions from "gschem_defines.h".
(define FROM_MENU 0)
(define FROM_HOTKEY 1)

(define (edit-objects window objects)
  "Start editing action in WINDOW for OBJECTS.  The type of action
depends of the type of the first object in the object list."
  (define *window (check-window window 1))
  (define (line-number s)
    (length (string-split s #\newline)))

  ;; Return #f if object list is empty.
  (and (not (null? objects))
       ;; For now deal with only the first item.
       (let* ((object (car objects))
              (*object (object->pointer object)))
         (case (object-type object)
           ;; Also add the ability to multi attrib edit: nets, busses,
           ;; pins.
           ((complex net pin bus) (x_multiattrib_open *window))
           ((picture) (picture_change_filename_dialog *window))
           ((arc) (arc_angle_dialog *window *object))
           ((text)
            (let ((str (text-string object)))
              (if (and (attribute? object)
                       ;; Attribute editor only accepts one line values for
                       ;; attributes.
                       (= (line-number str) 1))
                  (attrib_edit_dialog *window *object FROM_MENU)
                  (text_edit_dialog *window))))
           (else #f)))))
