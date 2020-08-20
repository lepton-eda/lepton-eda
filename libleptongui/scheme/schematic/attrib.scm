;; Lepton EDA Schematic Capture
;; Scheme API
;; Copyright (C) 2011 Peter Brett
;; Copyright (C) 2017-2020 Lepton EDA Contributors
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
;;

(define-module (schematic attrib)
  #:use-module (system foreign)

  #:use-module (schematic core attrib)

  #:export (attribute-name))

;; add-attrib! target name value visible attribute-mode
;;
;; Create a new attribute, either attached to a target object in the
;; current page, or floating in the current page if target is #f.  The
;; name and value for the attribute must be strings, and if visible is
;; #f, the attribute will be invisible.  The attribute-mode controls
;; which parts of the attribute will be visible, and must be one of
;; the following symbols:
;;
;;   name
;;   value
;;   both
;;
;; See also active-page in the (schematic window) module.
(define-public add-attrib! %add-attrib!)


(define libleptongui (dynamic-link "libleptongui"))

(define s_attrib_uniq
  (let ((proc (delay
                (pointer->procedure
                 int
                 (dynamic-func "s_attrib_uniq" libleptongui)
                 (list '*)))))
    (force proc)))

(define s_attrib_add_entry
  (let ((proc (delay
                (pointer->procedure
                 int
                 (dynamic-func "s_attrib_add_entry" libleptongui)
                 (list '*)))))
    (force proc)))

(define (attribute-name name)
  "Adds attribute NAME to the list of attributes shown in the
\"Add attribute\" and \"Edit attribute\" dialogs.  Returns #t if
the attribute was added successfully, otherwise returns #f."
  (when (not (string? name))
    (throw 'wrong-type-arg "attribute-name: The argument must be a string."))

  (when (string-null? name)
    (throw 'wrong-type-arg "attribute-name: The argument cannot be an empty string."))

  (let ((pname (string->pointer name)))
    (and (not (zero? (s_attrib_uniq pname)))
         (not (zero? (s_attrib_add_entry pname))))))
