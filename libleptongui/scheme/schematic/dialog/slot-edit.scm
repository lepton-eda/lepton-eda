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


(define-module (schematic dialog slot-edit)
  #:use-module (srfi srfi-1)
  #:use-module (system foreign)

  #:use-module (lepton attrib)
  #:use-module (lepton ffi)

  #:use-module (schematic ffi)
  #:use-module ((schematic selection) #:select (page-selection))
  #:use-module (schematic window)
  #:use-module ((lepton object foreign) #:select (object->pointer))

  #:export (slot-edit-dialog))


(define (slot-edit-dialog-response *widget response *window)
  (let ((accepted? (true? (slot_edit_dialog_response response))))
    (when accepted?
      (let ((*string (slot_edit_dialog_get_text *widget)))
        (unless (null-pointer? *string)
          (with-window
           *window
           ;; The only one component must be selected.
           (let ((component (car (page-selection (active-page))))
                 (slot-string (string-append "slot=" (pointer->string *string))))
             (o_slot_end *window
                         (object->pointer component)
                         (string->pointer slot-string)))))))
    (slot_edit_dialog_quit *window)))


(define *slot-edit-dialog-response
  (procedure->pointer void slot-edit-dialog-response (list '* int '*)))


;;; Launch a dialog for changing slot of selected COMPONENT in *WINDOW.
(define (slot-edit-dialog *window component)
  (define (attrib-by-name attribs name)
    (let loop ((ls attribs))
      (and (not (null? ls))
           (if (string= name (attrib-name (car ls)))
               (car ls)
               (loop (cdr ls))))))

  (define (component-attrib-by-name component name)
    (or (attrib-by-name (object-attribs component) name)
        (attrib-by-name (inherited-attribs component) name)))

  (define (component-attrib-value-by-name component name)
    (and=> (component-attrib-by-name component name) attrib-value))

  ;; We ignore possible lack of the "numslot" attribute though
  ;; cannot do this for "slot".  If the latter is missing, it
  ;; will be added with the value "1" for now.
  (let ((numslots-value (or (component-attrib-value-by-name component "numslots") ""))
        (slot-value (or (component-attrib-value-by-name component "slot") "1")))
    ;; Run the dialog.
    (slot_edit_dialog *window
                      (string->pointer numslots-value)
                      (string->pointer slot-value)
                      *slot-edit-dialog-response)))
