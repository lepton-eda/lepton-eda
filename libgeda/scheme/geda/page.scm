;; gEDA - GPL Electronic Design Automation
;; libgeda - gEDA's library - Scheme API
;; Copyright (C) 2010 Peter Brett
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
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111 USA
;;

(define-module (geda page)

  ; Import C procedures
  #:use-module (geda core smob)
  #:use-module (geda core page)

  #:use-module (ice-9 optargs))

(define-public object-page %object-page)

(define-public page? %page?)
(define-public active-pages %active-pages)
(define-public make-page %new-page)
(define-public close-page! %close-page!)
(define-public page-filename %page-filename)
(define-public set-page-filename! %set-page-filename!)
(define-public page-contents %page-contents)
(define-public page-dirty? %page-dirty?)

;; page-append! P obj ...
;;
;; Adds obj (and any additional objects) to the contents of the page
;; P. Returns P.
(define-public (page-append! P . objects)
  (for-each (lambda (x) (%page-append! P x)) objects)
  P)

;; page-remove! P obj ...
;;
;; Removes obj (and any additional objects) from the contents of the
;; page P. Returns P.
(define-public (page-remove! P . objects)
  (for-each (lambda (x) (%page-remove! P x)) objects)
  P)

;; set-page-dirty! [state]
;;
;; Set whether page is flagged as changed according to the optional
;; flag state.  If state is omitted, the page is marked as changed.
(define*-public (set-page-dirty! page #:optional (state #t))
  (%set-page-dirty! page state))
