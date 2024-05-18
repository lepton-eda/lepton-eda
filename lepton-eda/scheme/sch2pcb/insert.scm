;;; Lepton EDA Schematic to PCB conversion
;;; Scheme API
;;; Copyright (C) 2023-2025 Lepton EDA Contributors
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

(define-module (sch2pcb insert)
  #:use-module (system foreign)

  #:use-module (lepton ffi boolean)
  #:use-module (lepton ffi sch2pcb)

  #:export (insert-file-element))

(define (insert-file-element *output-file element-filename *element)
  "Insert the contents of the file ELEMENT-FILENAME into *OUTPUT-FILE
replacing its fields 'footprint', 'refdes', and 'value' with the
corresponding fields of *ELEMENT."
  ;; Check that *OUTPUT-FILE is not NULL.  Otherwise the next call
  ;; will crash.
  (when (null-pointer? *output-file)
    (error "insert-file-element(): NULL output file"))
  (let* ((*element-filename (string->pointer element-filename))
         (*input-file (sch2pcb_open_file_to_read *element-filename)))
    (if (null-pointer? *input-file)
        (begin
          (format (current-error-port)
                  "insert-file-element(): can't open ~A\n"
                  element-filename)
          #f)
        (true? (sch2pcb_insert_element *input-file
                                       *output-file
                                       *element-filename
                                       (pcb_element_get_description *element)
                                       (pcb_element_get_refdes *element)
                                       (pcb_element_get_value *element))))))
