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

;; This file contains deprecated Scheme API features, which should not
;; be used in new code.

(define-module (geda deprecated)

 #:use-module (geda page)
 #:use-module (geda object)
 #:use-module (geda attrib)

 ;; Import C procedures
 #:use-module (geda core deprecated)
 #:re-export (OBJ_LINE OBJ_PATH OBJ_BOX OBJ_PICTURE OBJ_CIRCLE OBJ_NET
              OBJ_BUS OBJ_COMPLEX OBJ_TEXT OBJ_PIN OBJ_ARC)
)

(define-public get-line-width %get-line-width)

(define-public get-attribute-name-value parse-attrib)

(define-public get-attribute-angle text-angle)

(define-public (get-object-attributes object)
  (reverse! (object-attribs object)))

(define-public (get-attrib-value-by-attrib-name object name)
  (reverse!
   (filter!
    string?
    (map (lambda (x)
           (let ((name-value (parse-attrib x)))
             (if (string=? name (car name-value))
                 (cdr name-value)
                 #f)))
         (object-attribs object)))))


(define-public (get-object-type object)
  (case (object-type object)
    ((arc) OBJ_ARC)
    ((box) OBJ_BOX)
    ((bus) OBJ_BUS)
    ((circle) OBJ_CIRCLE)
    ((complex) OBJ_COMPLEX)
    ((line) OBJ_LINE)
    ((net) OBJ_NET)
    ((path) OBJ_PATH)
    ((picture) OBJ_PICTURE)
    ((pin) OBJ_PIN)
    ((text) OBJ_TEXT)
    (else (error "Unknown object type ~A" (object-type object)))))

(define-public get-page-filename page-filename)

(define-public set-page-filename set-page-filename!)

(define-public (get-attribute-bounds object)
  ;; object-bounds returns ((left . top) . (right . bottom)).
  ;; Put in form ((left . right) . (top . bottom))
  (let* ((bounds (object-bounds object))
         (top (cdr (car bounds)))
         (right (car (cdr bounds))))
    (set-cdr! (car bounds) right)
    (set-car! (cdr bounds) top)
    bounds))

(define-public (calcule-new-attrib-bounds attrib align angle x y)
  (define align-table
    '(("Lower Left" . lower-left)
      ("Middle Left" . middle-left)
      ("Upper Left" . upper-left)
      ("Lower Middle" . lower-center)
      ("Middle Middle" . middle-center)
      ("Upper Middle" . upper-center)
      ("Lower Right" . lower-right)
      ("Middle Right" . middle-right)
      ("Upper Right" . upper-right)))

  (let* ((t (copy-object attrib))
         (params (text-info t))
         (location (car params)))

    (apply set-text! t
           (cons (if (= x -1) (car location) x) ; location
                 (if (= y -1) (cdr location) y))
           (or (assoc-ref align-table align) (text-align t)) ; alignment
           (if (= angle -1) (list-ref params 2) angle) ; angle
           (list-tail params 3)) ; other params

    ;; object-bounds returns ((left . top) . (right . bottom)).
    ;; Put in form ((left . right) . (bottom . top))
    (let* ((bounds (object-bounds t))
           (top (cdr (car bounds)))
           (bottom (cdr (cdr bounds)))
           (right (car (cdr bounds))))
      (set-cdr! (car bounds) right)
      (set-car! (cdr bounds) bottom)
      (set-cdr! (cdr bounds) top)
      bounds)))
