;; gEDA - GPL Electronic Design Automation
;; libgeda - gEDA's library - Scheme API
;; Copyright (C) 2010-2011 Peter Brett <peter@peter-b.co.uk>
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

  #:use-module (geda object)
  #:use-module (geda page))

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
  (false-if-exception (and (parse-attrib a) #t)))

;; attrib-name a
;;
;; Returns the attribute name of a, or raises an attribute-format
;; error if a is not in attribute format.
(define-public (attrib-name a)
  (let ((v (parse-attrib a)))
    (if v (car v) v)))

;; attrib-value a
;;
;; Returns the attribute value of a, or raises an attribute-format
;; error if a is not in attribute format.
(define-public (attrib-value a)
  (let ((v (parse-attrib a)))
    (if v (cdr v) v)))

;; set-attrib-value! a val
;;
;; Updates the attribute a with the new value val.
(define-public (set-attrib-value! a val)
  (let ((name (attrib-name a)))
    (set-text-string! a (string-join (list name val) "="))))

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

;; promote-attribs! object
;;
;; Promotes any promotable attributes from an object into its current
;; page, if object is a component, keeping the original attributes as
;; invisible attributes inside the component.  Returns a list of the
;; objects that were added to the page.  If object is not a component,
;; returns the empty list.  If object is not in a page, throws an
;; object-state error.
;;
;; See also promotable-attribs.
(define-public (promote-attribs! object)
  (let ((p (or (object-page object)
               (scm-error 'object-state #f
                          "Object ~A is not part of a page" (list object) #f))))
    (if (component? object)
        (map (lambda (x)
               (let ((y (copy-object x)))
                 ;; Make original object invisible
                 (set-text-visibility! x #f)
                 ;; Append copy of the object to page
                 (page-append! p y)
                 ;; Attach it to object
                 (attach-attrib! object y)
                 ;; Return copy
                 y))
               (promotable-attribs object))
        #f)))

;; attrib-inherited? attrib
;;
;; Returns #t if attrib is a toplevel un-attached attribute inside a
;; component.
(define-public (attrib-inherited? attrib)
  (not (or (attrib-attachment attrib)
           (not (object-component attrib)))))
