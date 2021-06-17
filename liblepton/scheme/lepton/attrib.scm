;; gEDA - GPL Electronic Design Automation
;; libgeda - gEDA's library - Scheme API
;; Copyright (C) 2010-2011 Peter Brett <peter@peter-b.co.uk>
;; Copyright (C) 2000-2011 gEDA Contributors
;; Copyright (C) 2017-2021 Lepton EDA Contributors
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

(define-module (lepton attrib)
  #:use-module (rnrs bytevectors)
  #:use-module (system foreign)

  ; Import C procedures
  #:use-module (lepton core attrib)
  #:use-module (lepton core gettext)

  #:use-module (lepton ffi)
  #:use-module (lepton object)
  #:use-module (lepton object type)
  #:use-module (lepton page)

  #:export (parse-attrib))

(define (parse-attrib object)
  "Parses attribute text OBJECT into name and value strings.  If
successful, returns a pair in the form (name . value).  Otherwise,
raises an 'attribute-format error."
  (define pointer (geda-object->pointer* object 1 text? 'text))

  (let* ((name-ptr-ptr (bytevector->pointer (make-bytevector (sizeof '*))))
         (value-ptr-ptr (bytevector->pointer (make-bytevector (sizeof '*)))))
    (if (true? (o_attrib_get_name_value pointer name-ptr-ptr value-ptr-ptr))
        (let* ((name-ptr (dereference-pointer name-ptr-ptr))
               (value-ptr (dereference-pointer value-ptr-ptr))
               (name (pointer->string name-ptr))
               (value (pointer->string value-ptr)))
          (g_free name-ptr)
          (g_free value-ptr)
          ;; Return the pair of name and value.
          (cons name value))
        (scm-error 'attribute-format
                   'parse-attrib
                   "~A is not a valid attribute: invalid string '~A'."
                   (list object (text-string object))
                   '()))))

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
                          (G_ "Object ~A is not part of a page")
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
