;; gEDA - GPL Electronic Design Automation
;; libgeda - gEDA's library - Scheme API
;; Copyright (C) 2010-2011 Peter Brett <peter@peter-b.co.uk>
;; Copyright (C) 2000-2011 gEDA Contributors
;; Copyright (C) 2017-2022 Lepton EDA Contributors
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

  #:use-module (lepton color-map)
  #:use-module (lepton ffi)
  #:use-module (lepton gettext)
  #:use-module (lepton object)
  #:use-module (lepton object foreign)
  #:use-module (lepton object type)
  #:use-module (lepton page)

  #:export (attrib-attachment
            attach-attribs!
            detach-attribs!
            attrib-name
            attrib-value
            set-attrib-value!
            object-attribs
            attrib-inherited?
            inherited-attribs
            parse-attrib
            promotable-attribs
            promote-attribs!)

  #:re-export (attribute?))

(define-syntax geda-object->attrib-pointers
  (syntax-rules ()
    ((_ <object> <pos>)
     ;; Check if object is text and error with 'wrong-type-arg if
     ;; not.
     (let ((pointer (check-object <object> <pos> text? 'text)))
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

(define (object-attribs object)
  "Returns the attribute list of OBJECT."
  (define pointer (check-object object 1))

  (glist->object-list (lepton_object_get_attribs pointer)))


(define (attrib-attachment object)
  "Returns the object that attribute OBJECT is attached to.  If
OBJECT is not attached as an attribute, returns #f."
  (define pointer (check-object object 1 attribute? 'attribute))

  (let ((attachment (lepton_object_get_attached_to pointer)))
    (and (not (null-pointer? attachment))
         (pointer->object attachment))))


(define (promotable-attribs object)
  "Returns the promotable attributes of component OBJECT,
according to the current configuration."
  (define pointer (check-object object 1 component? 'component))

  (glist->object-list
   (lepton_component_object_get_promotable pointer FALSE)))


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

(define (set-attrib-value! object val)
  "Sets the value part of attribute OBJECT to VAL."
  (let ((name (attrib-name object)))
    (set-text-string! object (string-join (list name val) "="))))

(define (inherited-attribs object)
  "Returns the inherited attributes of OBJECT, if object is a
component. If OBJECT is not a component, returns the empty list."
  (if (component? object)
      (filter! (lambda (x) (and (attribute? x) (not (attrib-attachment x))))
               (component-contents object))
      '()))

(define (promote-attribs! object)
  "Promotes all promotable attributes from component OBJECT into
the page that contains the component.  If component is not in a
page, an 'object-state error is raised.  All promotable attributes
are copied, and made invisible. The copies are added to the page,
and attached as attributes of component.  The list of promoted
attributes is returned.  If OBJECT is not in fact a component
object, the function does nothing and returns the empty list."
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

(define (attrib-inherited? object)
  "Returns #t if OBJECT is an inherited attribute.  Otherwise
returns #f."
  (not (or (attrib-attachment object)
           (not (object-component object)))))


;;; Attaches ATTRIB to OBJECT.  The following conditions must be
;;; satisfied:
;;; - Neither OBJECT nor ATTRIB may be already attached as an
;;;   attribute.
;;; - Both OBJECT and ATTRIB must be part of the same page
;;;   and/or component object. (They can't be "loose" objects).
;;; - ATTRIB must be a text object.
;;; If attrib is already attached to object, does nothing
;;; successfully.
(define (%attach-attrib! object attrib)
  (define object-pointer (check-object object 1))
  (define attrib-pointer
    (check-object attrib 2 attribute? 'attribute))

  ;; Check that attachment doesn't already exist.
  (if (equal? (lepton_object_get_attached_to attrib-pointer)
              object-pointer)
      object

      (let ((parent (lepton_object_get_parent object-pointer)))

        ;; Check that both are in the same page and/or component
        ;; object.
        (when (or (not (equal? parent
                               (lepton_object_get_parent attrib-pointer)))
                  (not (equal? (lepton_object_get_page object-pointer)
                               (lepton_object_get_page attrib-pointer)))
                  (and (null-pointer? parent)
                       (null-pointer? (lepton_object_get_page object-pointer))))
          (scm-error 'object-state
                     'attach-attribs!
                     "Objects ~A and ~A are not part of the same page and/or component object"
                     (list object attrib)
                     '()))

        ;; Check that neither is already an attached attribute.
        (unless (null-pointer? (lepton_object_get_attached_to object-pointer))
          (scm-error 'object-state
                     'attach-attribs!
                     "Object ~A is already attached as an attribute"
                     (list object)
                     '()))
        (unless (null-pointer? (lepton_object_get_attached_to attrib-pointer))
          (scm-error 'object-state
                     'attach-attribs!
                     "Object ~A is already attached as an attribute"
                     (list attrib)
                     '()))

        ;; Carry out the attachment.
        (lepton_object_emit_pre_change_notify attrib-pointer)
        (o_attrib_attach attrib-pointer object-pointer TRUE)
        (lepton_object_emit_change_notify attrib-pointer)

        (lepton_object_page_set_changed object-pointer)

        object)))

(define (attach-attribs! obj . attribs)
  "Attaches ATTRIBS to OBJECT.  The following conditions must be
satisfied, or an 'object-state error will be raised:
- All the ATTRIBS must be text objects.
- Neither OBJECT nor any of ATTRIBS may be already attached as an
  attribute.
- Both OBJECT and all ATTRIBS must be part of the same page
  and/or component object.
Any of the ATTRIBS that are already attached to OBJECT are
ignored. Returns OBJECT."
  (for-each (lambda (x) (%attach-attrib! obj x)) attribs)
  obj)


;;; Detaches ATTRIB from OBJECT.  If ATTRIB is not attached as an
;;; attribute, does nothing silently.  If ATTRIB is attached as an
;;; attribute of an object other than OBJECT, raises an
;;; 'object-state error.  Returns OBJECT.
(define (%detach-attrib! object attrib)
  (define object-pointer (check-object object 1))
  (define attrib-pointer
    (check-object attrib 2 attribute? 'attribute))

  ;; If attrib isn't attached do nothing.
  (if (null-pointer? (lepton_object_get_attached_to attrib-pointer))
      object

      (begin
        ;; Check that attrib isn't attached elsewhere.
        (unless (equal? (lepton_object_get_attached_to attrib-pointer)
                        object-pointer)
          (scm-error 'object-state
                     'detach-attrib!
                     "Object ~A is attribute of wrong object"
                     (list attrib)
                     '()))

        (let ((attribs (lepton_object_get_attribs object-pointer)))
          ;; Detach object.
          (lepton_object_emit_pre_change_notify attrib-pointer)

          (lepton_object_set_attribs object-pointer
                                     (g_list_remove attribs attrib-pointer))
          (lepton_object_set_attached_to attrib-pointer %null-pointer)

          (lepton_object_set_color attrib-pointer
                                   (color-map-name-to-index 'detached-attribute))
          (lepton_object_emit_change_notify attrib-pointer)

          (lepton_object_page_set_changed object-pointer)

          object))))

(define (detach-attribs! obj . attribs)
  "Detach ATTRIBS from OBJECT.  Any of the ATTRIBS that are not
attached as attributes are ignored.  If any of the ATTRIBS are
attached to objects other than OBJECT, an 'object-state error is
raised.  Returns OBJECT."
  (for-each (lambda (x) (%detach-attrib! obj x)) attribs)
  obj)
