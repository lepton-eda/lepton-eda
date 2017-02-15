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

  #:use-module (geda core gettext)

  #:use-module (geda object)
  #:use-module (geda page))

(define-public parse-attrib %parse-attrib)
(define-public attrib-name %attrib-name)
(define-public object-attribs %object-attribs)
(define-public attrib-attachment %attrib-attachment)
(define-public promotable-attribs %promotable-attribs)

(define-public (attribute? a)
  (false-if-exception (and (attrib-name a) #t)))

(define-public (attrib-value a)
  (let ((v (parse-attrib a)))
    (if v (cdr v) v)))

(define-public (set-attrib-value! a val)
  (let ((name (attrib-name a)))
    (set-text-string! a (string-join (list name val) "="))))

(define-public (inherited-attribs object)
  (if (component? object)
      (filter! (lambda (x) (and (attribute? x) (not (attrib-attachment x))))
               (component-contents object))
      '()))

(define-public (promote-attribs! object)
  (let ((p (or (object-page object)
               (scm-error 'object-state #f
                          (_ "Object ~A is not part of a page")
                          (list object) #f))))
    (if (component? object)
        (map (lambda (x)
               (let ((y (copy-object x)))
                 ;; Make original object invisible
                 (set-text-visibility! x #f)
                 ;; Append copy of the object to page
                 (page-append! p y)
                 ;; Attach it to object
                 (attach-attribs! object y)
                 ;; Return copy
                 y))
               (promotable-attribs object))
        '())))

(define-public (attrib-inherited? attrib)
  (not (or (attrib-attachment attrib)
           (not (object-component attrib)))))

(define-public (attach-attribs! obj . attribs)
  (for-each (lambda (x) (%attach-attrib! obj x)) attribs)
  obj)

(define-public (detach-attribs! obj . attribs)
  (for-each (lambda (x) (%detach-attrib! obj x)) attribs)
  obj)
