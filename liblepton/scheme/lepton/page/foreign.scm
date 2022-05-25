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

  #:export (page?
            check-page
            page->pointer
            pointer->page
            glist->page-list))


(define-wrapped-pointer-type <page>
  page?
  pointer->page
  page->pointer
  (lambda (page port)
    (format port "#<page-0x~x>"
            (pointer-address (page->pointer page)))))


(define (glist->page-list gls)
  "Converts a GList of foreign page pointers GLS into a Scheme
list.  Returns the Scheme list of pages."
  (let loop ((gls gls)
             (ls '()))
    (if (null-pointer? gls)
        (reverse ls)
        (loop (glist-next gls)
              (cons (pointer->page (glist-data gls)) ls)))))

(define-syntax check-page
  (syntax-rules ()
    ((_ page pos)
     (let ((pointer (and (page? page)
                         (page->pointer page))))
       (if (or (not pointer)
               (null-pointer? pointer))
           (let ((proc-name (frame-procedure-name (stack-ref (make-stack #t) 1))))
             (scm-error 'wrong-type-arg
                        proc-name
                        "Wrong type argument in position ~A: ~A"
                        (list pos page)
                        #f))
           pointer)))))
