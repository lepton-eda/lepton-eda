;;; Lepton EDA library - Scheme API
;;; Copyright (C) 2010-2011 Peter Brett <peter@peter-b.co.uk>
;;; Copyright (C) 2012-2016 gEDA Contributors
;;; Copyright (C) 2017-2021 Lepton EDA Contributors
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


(define-module (lepton object type)
  #:use-module (system foreign)

  #:use-module (lepton ffi)
  #:use-module (lepton object foreign)

  #:export (geda-object->pointer*

            arc?
            attribute?
            box?
            bus?
            circle?
            component?
            line?
            net?
            path?
            picture?
            pin?
            net-pin?
            bus-pin?
            text?

            object?
            object-type
            object-type?))

;;; This syntax rule is intended for use in toplevel 'define' or
;;; 'let' forms in the functions where the check for wrong type of
;;; OBJECT is necessary.  The rule checks the object and, if it is
;;; not #<geda-object>, throws an error with the 'wrong-type-arg
;;; key reporting the function name and position POS of the
;;; OBJECT argument.  In short, the usage is as follows:
;;;   (define (myfunc object)
;;;     (define pointer (geda-object->pointer* object 1))
;;;     (function-body))
(define-syntax geda-object->pointer*
  (syntax-rules ()
    ((_ object pos)
     (let ((pointer (geda-object->pointer object)))
       (if (null-pointer? pointer)
           (let ((proc-name
                  (frame-procedure-name (stack-ref (make-stack #t) 1))))
             (scm-error 'wrong-type-arg
                        proc-name
                        "Wrong type argument in position ~A: ~A"
                        (list pos object)
                        #f))
           pointer)))
    ((_ object pos object-check-func type)
     (let ((pointer (geda-object->pointer object))
           (proc-name (frame-procedure-name (stack-ref (make-stack #t) 1))))
       (if (null-pointer? pointer)
           (scm-error 'wrong-type-arg
                      proc-name
                      "Wrong type argument in position ~A: ~A"
                      (list pos object)
                      #f)
           (if (object-check-func object)
               pointer
               (scm-error 'wrong-type-arg
                          proc-name
                          "Wrong type argument in position ~A (expecting ~A object): ~A"
                          (list pos type object)
                          #f)))))))


(define (object? object)
  "Returns #t if OBJECT is a #<geda-object> instance, otherwise
returns #f."
  (geda-object-pointer? (scm->pointer object)))


(define (arc? object)
  "Returns #t if OBJECT is a arc object, otherwise returns #f."
  (true? (lepton_object_is_arc (geda-object->pointer object))))

(define (box? object)
  "Returns #t if OBJECT is a box object, otherwise returns #f."
  (true? (lepton_object_is_box (geda-object->pointer object))))

(define (bus? object)
  "Returns #t if OBJECT is a bus object, otherwise returns #f."
  (true? (lepton_object_is_bus (geda-object->pointer object))))

(define (circle? object)
  "Returns #t if OBJECT is a circle object, otherwise returns #f."
  (true? (lepton_object_is_circle (geda-object->pointer object))))

(define (component? object)
  "Returns #t if OBJECT is a component object, otherwise returns #f."
  (true? (lepton_object_is_component (geda-object->pointer object))))

(define (line? object)
  "Returns #t if OBJECT is a line object, otherwise returns #f."
  (true? (lepton_object_is_line (geda-object->pointer object))))

(define-public (net? object)
  "Returns #t if OBJECT is a net object, otherwise returns #f."
  (true? (lepton_object_is_net (geda-object->pointer object))))

(define (path? object)
  "Returns #t if OBJECT is a path object, otherwise returns #f."
  (true? (lepton_object_is_path (geda-object->pointer object))))

(define (picture? object)
  "Returns #t if OBJECT is a picture object, otherwise returns #f."
  (true? (lepton_object_is_picture (geda-object->pointer object))))

(define (pin? object)
  "Returns #t if OBJECT is a pin object, otherwise returns #f."
  (true? (lepton_object_is_pin (geda-object->pointer object))))

(define (net-pin? object)
  "Returns #t if OBJECT is a net pin object, otherwise returns
#f."
  (and (pin? object)
       (true? (lepton_pin_object_is_net_pin
               (geda-object->pointer object)))))

(define (bus-pin? object)
  "Returns #t if OBJECT is a bus pin object, otherwise returns
#f."
  (and (pin? object)
       (true? (lepton_pin_object_is_bus_pin
               (geda-object->pointer object)))))


(define (text? object)
  "Returns #t if OBJECT is a text object, otherwise returns #f."
  (true? (lepton_object_is_text (geda-object->pointer object))))

(define (attribute? object)
  "Returns #t if OBJECT is an attribute text object, otherwise
returns #f."
  (true? (lepton_object_is_attrib (geda-object->pointer object))))

(define (object-type object)
  "Returns a Scheme symbol representing the type of OBJECT.  The
type may be one of the symbols: 'arc, 'box, 'bus, 'circle,
'complex, 'line, 'net, 'path, 'picture, 'pin, or 'text."
  (define pointer (geda-object->pointer* object 1))

  (cond
   ((arc? object) 'arc)
   ((box? object) 'box)
   ((bus? object) 'bus)
   ((circle? object) 'circle)
   ((component? object) 'complex)
   ((line? object) 'line)
   ((net? object) 'net)
   ((path? object) 'path)
   ((picture? object) 'picture)
   ((pin? object) 'pin)
   ((text? object) 'text)
   (else (error "Object ~A has bad type '~A'"
                object
                (integer->char (lepton_object_get_type pointer))))))

(define (object-type? object type)
  "Returns #t if OBJECT is a Lepton primitive object and its type
is TYPE which should be one of the symbols: 'arc, 'box, 'bus,
'circle, 'complex, 'line, 'net, 'path, 'picture, 'pin, or 'text.
Otherwise returns #f."
  (and (object? object)
       (eq? (object-type object) type)))
