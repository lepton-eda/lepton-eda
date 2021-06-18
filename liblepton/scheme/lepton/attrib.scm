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
  #:use-module (srfi srfi-11)
  #:use-module (system foreign)

  ; Import C procedures
  #:use-module (lepton core attrib)
  #:use-module (lepton core gettext)

  #:use-module (lepton ffi)
  #:use-module (lepton object)
  #:use-module (lepton object foreign)
  #:use-module (lepton object type)
  #:use-module (lepton page)

  #:export (attribute?
            attrib-name
            attrib-value
            parse-attrib))

(define-syntax geda-object->attrib-pointers
  (syntax-rules ()
    ((_ <object> <pos>)
     ;; Check if object is text and error with 'wrong-type-arg if
     ;; not.
     (let ((pointer (geda-object->pointer* <object> <pos> text? 'text)))
       ;; Now check if it is attribute and, if yes, return name
       ;; and value.  Otherwise raise an error with the key
       ;; 'attribute-format.
       (let ((name-pointer (lepton_text_object_get_name pointer))
             (value-pointer (lepton_text_object_get_value pointer)))
         (if (null-pointer? name-pointer)
             (scm-error 'attribute-format
                        'attrib-name
                        "~A is not a valid attribute: invalid string '~A'."
                        (list <object> (text-string <object>))
                        '())
             (values name-pointer value-pointer)))))))

(define (attribute? object)
  "Returns #t if OBJECT is an attribute text object, otherwise
returns #f."
  (false-if-exception (and (geda-object->attrib-pointers object 1) #t)))

(define (attrib-name object)
  "Obtain the name of attribute text OBJECT.  If successful,
returns the name as a string.  Otherwise, raises an
'attribute-format error.  If OBJECT is not a text object, raises
'wrong-type-arg error."
  (let-values (((name-pointer value-pointer)
                (geda-object->attrib-pointers object 1)))
    (pointer->string name-pointer)))

(define (parse-attrib object)
  "Parses attribute text OBJECT into name and value strings.  If
successful, returns a pair in the form (name . value).  Otherwise,
raises an 'attribute-format error."
  (let-values (((name-pointer value-pointer)
                (geda-object->attrib-pointers object 1)))
    (cons (pointer->string name-pointer)
          (if (null-pointer? value-pointer)
              ""
              (pointer->string value-pointer)))))

(define-public object-attribs %object-attribs)
(define-public attrib-attachment %attrib-attachment)
(define-public promotable-attribs %promotable-attribs)

(define (attrib-value object)
  "Obtain the value of attribute text OBJECT.  If successful,
returns the value as a string.  Otherwise, raises an
'attribute-format error.  If OBJECT is not a text object, raises
'wrong-type-arg error."
  (let-values (((name-pointer value-pointer)
                (geda-object->attrib-pointers object 1)))
    (if (null-pointer? value-pointer)
        ""
        (pointer->string value-pointer))))

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
