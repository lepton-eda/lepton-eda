;;; Lepton EDA netlister
;;; Copyright (C) 2016-2017 gEDA Contributors
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

(define-module (netlist schematic-component)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (lepton attrib)
  #:use-module (lepton object)
  #:use-module (netlist config)
  #:use-module (netlist net)
  #:use-module (netlist package-pin)
  #:use-module (symbol check net-attrib)

  #:export-syntax (make-schematic-component schematic-component?
                   schematic-component-id set-schematic-component-id!
                   schematic-component-refdes set-schematic-component-refdes!
                   schematic-component-parent set-schematic-component-parent!
                   schematic-component-sources set-schematic-component-sources!
                   schematic-component-object set-schematic-component-object!
                   schematic-component-iattribs set-schematic-component-iattribs!
                   schematic-component-attribs set-schematic-component-attribs!
                   schematic-component-net-maps set-schematic-component-net-maps!
                   schematic-component-pins set-schematic-component-pins!
                   schematic-component-port set-schematic-component-port!
                   schematic-component-subschematic set-schematic-component-subschematic!)

  #:export (component->schematic-component
            schematic-component-attributes
            schematic-component-attribute
            schematic-component-graphical?
            schematic-component-nc?
            schematic-component-simple-refdes
            schematic-component-subcircuit?
            set-schematic-component-printer!))

(define-record-type <schematic-component>
  (make-schematic-component id refdes parent sources object iattribs attribs net-maps pins port subschematic)
  schematic-component?
  (id schematic-component-id set-schematic-component-id!)
  (refdes schematic-component-refdes set-schematic-component-refdes!)
  (parent schematic-component-parent set-schematic-component-parent!)
  (sources schematic-component-sources set-schematic-component-sources!)
  (object schematic-component-object set-schematic-component-object!)
  (iattribs schematic-component-iattribs set-schematic-component-iattribs!)
  (attribs schematic-component-attribs set-schematic-component-attribs!)
  (net-maps schematic-component-net-maps set-schematic-component-net-maps!)
  (pins schematic-component-pins set-schematic-component-pins!)
  (port schematic-component-port set-schematic-component-port!)
  (subschematic schematic-component-subschematic set-schematic-component-subschematic!))

;;; Sets default printer for <schematic-component>
(set-record-type-printer!
 <schematic-component>
 (lambda (record port) (format port "#<geda-schematic-component ~A>" (schematic-component-id record))))

(define (set-schematic-component-printer! format-string . args)
  "Adjust pretty-printing of <schematic-component> records.
FORMAT-STRING must be in the form required by the procedure
`format'. The following ARGS may be used:
  'id
  'refdes
  'parent
  'sources
  'object
  'iattribs
  'attribs
  'net-maps
  'pins
  'port
  'subschematic
Any other unrecognized argument will lead to yielding '?' in the
corresponding place.
Example usage:
  (set-schematic-component-printer! \"<schematic-component-~A (~A)>\" 'id 'refdes)"
  (set-record-type-printer!
   <schematic-component>
   (lambda (record port)
     (apply format port format-string
            (map
             (lambda (arg)
               (match arg
                 ('id (schematic-component-id record))
                 ('refdes (schematic-component-refdes record))
                 ('parent (schematic-component-parent record))
                 ('sources (schematic-component-sources record))
                 ('object (schematic-component-object record))
                 ('iattribs (schematic-component-iattribs record))
                 ('attribs (schematic-component-attribs record))
                 ('net-maps (schematic-component-net-maps record))
                 ('pins (schematic-component-pins record))
                 ('port (schematic-component-port record))
                 ('subschematic (schematic-component-subschematic record))
                 (_ #\?)))
             args)))))


(define (schematic-component-attributes component name)
  "Returns the list of attached attributes called NAME for
COMPONENT. NAME must be a Scheme symbol (not string). If no
attached attributes found, returns the list of inherited
attributes with the same name. If neither attached nor inherited
attributes have been found, returns #f."
  (or (assq-ref (schematic-component-attribs component) name)
      (assq-ref (schematic-component-iattribs component) name)))


(define (schematic-component-attribute component name)
  "Returns first attached attribute of COMPONENT called NAME. NAME
must be a Scheme symbol (not string). If no attached attribute
found, returns first inherited attribute with NAME. If neither
attached nor inherited attribute found, returns #f."
  (and=> (schematic-component-attributes component name) car))


(define (schematic-component-attribute-string=? component name value)
  "Returns #t if COMPONENT has attribute NAME equal to VALUE,
otherwise returns #f. NAME must be a symbol, while VALUE should be
a string."
  (and=> (schematic-component-attribute component name)
         (lambda (x) (string=? x value))))


(define (schematic-component-graphical? component)
  "Returns #t if COMPONENT is graphical, that is, it has attribute
\"graphical=1\", otherwise returns #f."
  (schematic-component-attribute-string=? component 'graphical "1"))


(define (schematic-component-nc? component)
  "Returns #t if COMPONENT is 'no-connect' component, that is, it
has attribute \"symbol=nc\". Otherwise returns #f."
  (or (schematic-component-attribute-string=? component 'symbol "nc")
      ;; Obsolete "no-connect" component definition.
      (and (schematic-component-graphical? component)
           (schematic-component-attribute-string=? component 'device "DRC_Directive")
           (schematic-component-attribute-string=? component 'value "NoConnection"))))


(define (schematic-component-subcircuit? component)
  "Returns #t if COMPONENT is composite, that is, it has
underlying subcircuit defined by its \"source=\"
attributes. Otherwise returns #f."
  (not (not (schematic-component-sources component))))


(define (set-schematic-component-pins/parent! component pins)
  "Sets COMPONENT field [pins] to PINS and, for each pin in PINS,
sets the component to be its parent component."
  (define (set-pin-parent! pin)
    (force (set-package-pin-parent-component! pin component)))

  (for-each set-pin-parent! pins)
  (set-schematic-component-pins! component pins))


(define (get-sources graphical? inherited-attribs attached-attribs)
  (define (non-null* ls)
    (and (not (null? ls)) ls))

  ;; Given a list of strings, some of which may contain commas,
  ;; splits comma separated strings and returns the new combined
  ;; list
  (define (comma-separated->list ls)
    (append-map (lambda (s) (string-split s #\,)) ls))

  (and (not graphical?)
       (netlist-config-ref 'traverse-hierarchy)
       (let ((sources
              (or (non-null* (assq-ref attached-attribs 'source))
                  (non-null* (assq-ref inherited-attribs 'source)))))
         (and=> sources comma-separated->list))))


;;; Searches for pinnumers in NET-MAPS and, if found, updates
;;; corresponding pins in PIN-LIST, otherwise creates new pins and
;;; adds them to the list.
(define (net-maps->package-pins net-maps pin-list)
  (define (pinnumber->pin pinnumber pin-list)
    (and (not (null? pin-list))
         (let ((package-pinnumber (package-pin-number (car pin-list))))
           ;; FIXME: a pin may have no "pinnumber=", and we have
           ;; to deal with such cases. A test and drc check is
           ;; needed.
           (if (and package-pinnumber
                    (string=? package-pinnumber pinnumber))
               (car pin-list)
               (pinnumber->pin pinnumber (cdr pin-list))))))

  (define (make-or-update-net-map-pin net-map)
    (let ((pin (pinnumber->pin (net-map-pinnumber net-map)
                               pin-list)))
      (if pin
          ;; If pin exists, just assign net-map for it.
          (begin
            (set-package-pin-net-map! pin net-map)
            ;; Return #f to filter out existing pins.
            #f)
          ;; Otherwise, make a new virtual pin.
          (make-package-pin #f
                            #f
                            (net-map-pinnumber net-map)
                            #f
                            #f
                            '()
                            net-map
                            #f
                            #f
                            #f
                            #f))))

  ;; Create virtual 'net-map' pins.
  (filter-map make-or-update-net-map-pin net-maps))


(define (component->schematic-component object)
  ;; Makes attribute list of OBJECT using getter GET-ATTRIBS.
  (define (make-attrib-list get-attribs object)
    (define (add-attrib ls attrib)
      (let* ((name (string->symbol (attrib-name attrib)))
             (prev-value (assq-ref ls name))
             (new-value (attrib-value attrib)))
        (if prev-value
            (assq-set! ls name (cons new-value prev-value))
            (acons name (list new-value) ls))))

    (let loop ((in (get-attribs object))
               (out '()))
      (if (null? in)
          out
          (loop (cdr in)
                (add-attrib out (car in))))))

  (let* ((id (object-id object))
         (inherited-attribs (make-attrib-list inherited-attribs object))
         (attached-attribs (make-attrib-list object-attribs object))
         (net-maps (check-net-maps object))
         (component (make-schematic-component id
                                              #f ; get refdes later
                                              #f ; get parent subschematic later
                                              #f ; get sources later
                                              object
                                              inherited-attribs
                                              attached-attribs
                                              net-maps
                                              ;; get pins later
                                              '()
                                              ;; not a port initially
                                              #f
                                              ;; no subschematic
                                              #f))
         (graphical (or (schematic-component-graphical? component)
                        (schematic-component-nc? component)))
         (sources (get-sources graphical
                               inherited-attribs
                               attached-attribs))
         (real-pins (filter-map object->package-pin
                                (component-contents object)))
         (net-map-pins (net-maps->package-pins net-maps real-pins))
         (pins (append real-pins net-map-pins)))
    (set-schematic-component-sources! component sources)
    (set-schematic-component-pins/parent! component pins)
    component))


(define (schematic-component-simple-refdes component)
  "Returns simple, non-hierarchical refdes of COMPONENT."
  (let ((refdes (schematic-component-refdes component)))
    (match refdes
      ((? list? refdes) (car refdes))
      (refdes refdes))))
