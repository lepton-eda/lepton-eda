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

(define-module (geda attrib)

  ; Import C procedures
  #:use-module (geda core attrib)

  #:use-module (geda object))

(define-public parse-attrib %parse-attrib)
(define-public object-attribs %object-attribs)
(define-public attrib-attachment %attrib-attachment)
(define-public attach-attrib! %attach-attrib!)
(define-public detach-attrib! %detach-attrib!)
(define-public promotable-attribs %promotable-attribs)

;; attribute? a
;;
;; Returns #t if a is an text object in attribute format.
(define-public (attribute? a)
  (and (text? a) (parse-attrib a)))

;; attrib-name a
;;
;; Returns the attribute name of a, or #f if a is not in attribute
;; format.
(define-public (attrib-name a)
  (let ((v (parse-attrib a)))
    (if v (car v) v)))

;; attrib-value a
;;
;; Returns the attribute value of a, or #f if a is not in attribute
;; format.
(define-public (attrib-value a)
  (let ((v (parse-attrib a)))
    (if v (cdr v) v)))

;; inherited-attribs object
;;
;; Returns the inherited attributes of object, if object is a
;; component.  The inherited attributes are the unattached top-level
;; attributes in the component.  If object is not a component, returns
;; the empty list.
(define-public (inherited-attribs object)
  (if (component? object)
      (filter! (lambda (x) (and (attribute? x) (not (attrib-attachment x))))
               (component-contents object))
      '()))
