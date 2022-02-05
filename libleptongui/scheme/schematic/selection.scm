;; Lepton EDA Schematic Capture
;; Scheme API
;; Copyright (C) 2010 Peter Brett <peter@peter-b.co.uk>
;; Copyright (C) 2020-2022 Lepton EDA Contributors
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

(define-module (schematic selection)
  #:use-module (system foreign)

  #:use-module (lepton ffi)
  #:use-module (lepton page foreign)
  #:use-module (lepton object foreign)
  #:use-module (lepton object type)

  ;; Import C procedures
  #:use-module (schematic core selection)

  #:export (object-selected?
            page-selection
            select-object!
            deselect-object!))

(define (page-selection page)
  "Return a list of selected objects on PAGE."
  (define *page (geda-page->pointer* page 1))

  (glist->list (lepton_list_get_glist (lepton_page_get_selection_list *page))
               pointer->geda-object))


(define (select-object! object)
  "Adds OBJECT to its associated page's selection.  If OBJECT is
not included directly in a page (i.e. inclusion in a component is
not permitted), raises the 'object-state Scheme error.  If OBJECT
is already selected, does nothing. Returns OBJECT."
  (define *object (geda-object->pointer* object 1))

  (let ((*page (lepton_object_get_page *object)))
    (when (or (null-pointer? *page)
              (not (null-pointer? (lepton_object_get_parent *object))))
      (scm-error 'object-state
                 select-object!
                 "Object ~A is not directly included in a page."
                 (list object)
                 '()))
    (unless (true? (lepton_object_get_selected *object))
      (o_selection_add (lepton_page_get_selection_list *page)
                       *object))
    object))


(define (deselect-object! object)
  "Removes OBJECT from its associated page's selection.  If OBJECT
is not included directly in a page (i.e. not via inclusion in a
component), raises the 'object-state Scheme error.  If OBJECT is
not selected,does nothing.  Returns OBJECT."
  (define *object (geda-object->pointer* object 1))

  (let ((*page (lepton_object_get_page *object)))
    (when (or (null-pointer? *page)
              (not (null-pointer? (lepton_object_get_parent *object))))
      (scm-error 'object-state
                 select-object!
                 "Object ~A is not directly included in a page."
                 (list object)
                 '()))
    (when (true? (lepton_object_get_selected *object))
      (o_selection_remove (lepton_page_get_selection_list *page)
                          *object))
    object))


(define (object-selected? object)
  "Returns #t if OBJECT is selected.  Otherwise, returns #f.  If
OBJECT is not included directly in a page (i.e. not via inclusion
in a component), raises the 'object-state Scheme error."
  (define *object (geda-object->pointer* object 1))

  (let ((*page (lepton_object_get_page *object)))
    (when (or (null-pointer? *page)
              (not (null-pointer? (lepton_object_get_parent *object))))
      (scm-error 'object-state
                 'object-selected?
                 "Object ~A is not directly included in a page."
                 (list object)
                 '()))
    (true? (lepton_object_get_selected *object))))
