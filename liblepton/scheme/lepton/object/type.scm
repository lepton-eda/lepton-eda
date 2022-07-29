;;; Lepton EDA library - Scheme API
;;; Copyright (C) 2010-2011 Peter Brett <peter@peter-b.co.uk>
;;; Copyright (C) 2012-2016 gEDA Contributors
;;; Copyright (C) 2017-2022 Lepton EDA Contributors
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

  #:use-module (lepton ffi boolean)
  #:use-module (lepton ffi)
  #:use-module (lepton object foreign)

  #:export (arc?
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


(define (object? object)
  "Returns #t if OBJECT is an <object> instance, otherwise returns
#f."
  (is-object? object))


(define (arc? object)
  "Returns #t if OBJECT is a arc object, otherwise returns #f."
  (and (is-object? object)
       (true? (lepton_object_is_arc (object->pointer object)))))

(define (box? object)
  "Returns #t if OBJECT is a box object, otherwise returns #f."
  (and (is-object? object)
       (true? (lepton_object_is_box (object->pointer object)))))

(define (bus? object)
  "Returns #t if OBJECT is a bus object, otherwise returns #f."
  (and (is-object? object)
       (true? (lepton_object_is_bus (object->pointer object)))))

(define (circle? object)
  "Returns #t if OBJECT is a circle object, otherwise returns #f."
  (and (is-object? object)
       (true? (lepton_object_is_circle (object->pointer object)))))

(define (component? object)
  "Returns #t if OBJECT is a component object, otherwise returns #f."
  (and (is-object? object)
       (true? (lepton_object_is_component (object->pointer object)))))

(define (line? object)
  "Returns #t if OBJECT is a line object, otherwise returns #f."
  (and (is-object? object)
       (true? (lepton_object_is_line (object->pointer object)))))

(define-public (net? object)
  "Returns #t if OBJECT is a net object, otherwise returns #f."
  (and (is-object? object)
       (true? (lepton_object_is_net (object->pointer object)))))

(define (path? object)
  "Returns #t if OBJECT is a path object, otherwise returns #f."
  (and (is-object? object)
       (true? (lepton_object_is_path (object->pointer object)))))

(define (picture? object)
  "Returns #t if OBJECT is a picture object, otherwise returns #f."
  (and (is-object? object)
       (true? (lepton_object_is_picture (object->pointer object)))))

(define (pin? object)
  "Returns #t if OBJECT is a pin object, otherwise returns #f."
  (and (is-object? object)
       (true? (lepton_object_is_pin (object->pointer object)))))

(define (net-pin? object)
  "Returns #t if OBJECT is a net pin object, otherwise returns
#f."
  (and (pin? object)
       (true? (lepton_pin_object_is_net_pin
               (object->pointer object)))))

(define (bus-pin? object)
  "Returns #t if OBJECT is a bus pin object, otherwise returns
#f."
  (and (pin? object)
       (true? (lepton_pin_object_is_bus_pin
               (object->pointer object)))))


(define (text? object)
  "Returns #t if OBJECT is a text object, otherwise returns #f."
  (and (is-object? object)
       (true? (lepton_object_is_text (object->pointer object)))))

(define (attribute? object)
  "Returns #t if OBJECT is an attribute text object, otherwise
returns #f."
  (and (is-object? object)
       (true? (lepton_object_is_attrib (object->pointer object)))))

(define (object-type object)
  "Returns a Scheme symbol representing the type of OBJECT.  The
type may be one of the symbols: 'arc, 'box, 'bus, 'circle,
'complex, 'line, 'net, 'path, 'picture, 'pin, or 'text."
  (define pointer (check-object object 1))

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
