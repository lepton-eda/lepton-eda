;; gEDA - GPL Electronic Design Automation
;; gschem - gEDA Schematic Capture - Scheme API
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

;; This file contains deprecated Scheme API features, which should not
;; be used in new code.

(define-module (gschem deprecated)

  #:use-module (geda page)
  #:use-module (geda object)
  #:use-module (geda attrib)
  #:use-module (gschem window))

(define-public (set-attribute-value! attrib value)
  (let ((params (text-info attrib))
        (name-value (attrib-parse attrib))
       (list-set! params 3 (simple-format "~A=~A" (car name-value) value))
       (apply set-text! attrib params))))

(define-public (get-objects-in-page page)
  (reverse! (page-contents page)))

(define-public get-current-page active-page)

(define-public (get-object-pins object)
  (if (component? object)
      (reverse! (filter! pin? (component-contents object)))
      '()))

(define-public (get-pin-ends pin)
  (let ((params (line-info pin)))
    (cons (list-ref params 0) (list-ref params 1))))
