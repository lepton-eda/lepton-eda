;;; Lepton EDA netlister
;;; Copyright (C) 2016-2017 gEDA Contributors
;;; Copyright (C) 2017-2020 Lepton EDA Contributors
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

(define-module (netlist package-pin)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (lepton attrib)
  #:use-module (lepton object)

  #:export-syntax (make-package-pin package-pin?
                   package-pin-id set-package-pin-id!
                   package-pin-object set-package-pin-object!
                   package-pin-number set-package-pin-number!
                   package-pin-name set-package-pin-name!
                   package-pin-label set-package-pin-label!
                   package-pin-attribs set-package-pin-attribs!
                   package-pin-net-map set-package-pin-net-map!
                   package-pin-parent set-package-pin-parent!
                   package-pin-connection set-package-pin-connection!
                   package-pin-named-connection set-package-pin-named-connection!
                   package-pin-port-connection set-package-pin-port-connection!)

  #:export (object->package-pin
            set-package-pin-printer!
            set-package-pin-parent-component!))

(define-record-type <package-pin>
  (make-package-pin id object number name label attribs net-map parent connection named-connection port-connection)
  package-pin?
  ;; This field is used just for the record representation in
  ;; set-record-type-printer! below.
  (id package-pin-id set-package-pin-id!)
  ;; The underlying primitive pin object.
  (object package-pin-object set-package-pin-object!)
  ;; Corresponds to pin's "pinnumber" attribute.
  (number package-pin-number set-package-pin-number!)
  ;; Corresponds to net name of the net the pin is connected to.
  (name package-pin-name set-package-pin-name!)
  ;; Corresponds to pin's "pinlabel" attribute.
  (label package-pin-label set-package-pin-label!)
  ;; The alist representing attributes of the underlying object.
  (attribs package-pin-attribs set-package-pin-attribs!)
  ;; net= attribute mapping for the pin.
  (net-map package-pin-net-map set-package-pin-net-map!)
  ;; Parent component of the pin.
  (parent package-pin-parent set-package-pin-parent!)
  ;; <schematic-connection> the pin is connected to.
  (connection package-pin-connection set-package-pin-connection!)
  ;; Common net name <schematic-connection> the pin is connected to.
  (named-connection package-pin-named-connection set-package-pin-named-connection!)
  ;; Hierarchical (via port) <schematic-connection> the pin is connected to.
  (port-connection package-pin-port-connection set-package-pin-port-connection!))

;;; Sets default printer for <package-pin>
(set-record-type-printer!
 <package-pin>
 (lambda (record port) (format port "#<geda-package-pin ~A>" (package-pin-id record))))

(define (set-package-pin-printer! format-string . args)
  "Adjust pretty-printing of <package-pin> records.
FORMAT-STRING must be in the form required by the procedure
`format'. The following ARGS may be used:
  'id
  'object
  'number
  'name
  'label
  'attribs
  'net-map
  'parent
  'connection
  'named-connection
  'port-connection
Any other unrecognized argument will lead to yielding '?' in the
corresponding place.
Example usage:
  (set-package-pin-printer! \"<package-pin-~A (~A)>\" 'id 'number)"
  (set-record-type-printer!
   <package-pin>
   (lambda (record port)
     (apply format port format-string
            (map
             (lambda (arg)
               (match arg
                 ('id (package-pin-id record))
                 ('object (package-pin-object record))
                 ('number (package-pin-number record))
                 ('name (package-pin-name record))
                 ('label (package-pin-label record))
                 ('attribs (package-pin-attribs record))
                 ('net-map (package-pin-net-map record))
                 ('parent (package-pin-parent record))
                 ('connection (package-pin-connection record))
                 ('named-connection (package-pin-named-connection record))
                 ('port-connection (package-pin-port-connection record))
                 (_ #\?)))
             args)))))

(define (set-package-pin-parent-component! pin component)
  (delay (set-package-pin-parent! pin component)))


(define (object->package-pin pin-object)
  "Transform a primitive PIN-OBJECT into <package-pin>."
  (define (add-attrib attrib)
    (cons (string->symbol (attrib-name attrib))
          (attrib-value attrib)))

  (define attribs
    (map add-attrib (object-attribs pin-object)))

  (and (net-pin? pin-object)
       (make-package-pin (object-id pin-object)
                         ;; Primitive pin object.
                         pin-object
                         ;; Number.
                         (assq-ref attribs 'pinnumber)
                         ;; Add name later.
                         #f
                         ;; Label.
                         (assq-ref attribs 'pinlabel)
                         ;; Attributes.
                         attribs
                         ;; No net-map yet.
                         #f
                         ;; Set parent component later.
                         #f
                         ;; No connection yet.
                         #f
                         ;; No netname connection yet.
                         #f
                         ;; No port connection yet.
                         #f)))
