;;; Lepton EDA library - Scheme API
;;; Copyright (C) 2021 Lepton EDA Contributors
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


(define-module (lepton page foreign)
  #:use-module (system foreign)

  #:use-module (lepton ffi)

  #:export (geda-page-pointer?
            geda-page->pointer
            pointer->geda-page
            glist->page-list))

;;; Helper transformers between #<geda-page> smobs and C page
;;; pointers.
(define (geda-page->pointer smob)
  (or (false-if-exception (edascm_to_page (scm->pointer smob)))
      ;; Return NULL if the SMOB is not the #<geda-page> smob.
      %null-pointer))

(define (pointer->geda-page pointer)
  ;; Return #f if the pointer is wrong.
  (false-if-exception (pointer->scm (edascm_from_page pointer))))

(define (geda-page-pointer? pointer)
  (true? (edascm_is_page pointer)))

(define (glist->page-list gls)
  "Converts a GList of foreign page pointers GLS into a Scheme
list.  Returns the Scheme list of pages."
  (let loop ((gls gls)
             (ls '()))
    (if (null-pointer? gls)
        (reverse ls)
        (loop (glist-next gls)
              (cons (pointer->geda-page (glist-data gls)) ls)))))
