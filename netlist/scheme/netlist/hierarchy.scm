;;; Lepton EDA netlister
;;; Copyright (C) 2016-2017 gEDA Contributors
;;; Copyright (C) 2017-2019 Lepton EDA Contributors
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
;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
;;; MA 02111-1301 USA.

(define-module (netlist hierarchy)
  ;; Import C procedures and variables.
  #:use-module (netlist core gettext)

  #:use-module (ice-9 match)
  #:use-module (ice-9 receive)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)

  #:use-module (geda log)
  #:use-module (geda object)
  #:use-module (netlist attrib refdes)
  #:use-module (netlist config)
  #:use-module (netlist core gettext)
  #:use-module (netlist mode)
  #:use-module (netlist net)
  #:use-module (netlist package-pin)
  #:use-module (netlist schematic-component)
  #:use-module (netlist schematic-connection)
  #:use-module (netlist schematic-port)
  #:use-module (netlist subschematic)
  #:use-module (symbol check net-attrib)

  #:export (hierarchy-post-process))



;;; Lookups for pinnumber and parent component's refdes for PIN.
;;; If they're somehow wrong, warns the users and sets new
;;; appropriate values.  Returns the pair (refdes . pinnumber),
;;; fixed if needed.
(define (pin-refdes-pinnumber-pair pin)
  (let ((refdes (attrib-value-by-name (object-component pin)
                                      "refdes"))
        (pinnumber (attrib-value-by-name pin "pinnumber")))
    (match `(,refdes . ,pinnumber)
      ;; Wrong case, neither refdes nor pinnumber found.
      ((#f . #f)
       (log! 'critical (_ "Missing attributes refdes= and pinnumber="))
       '("U?" . "?"))
      ;; Missing pin number while refdes exists.
      ((refdes . #f)
       (log! 'critical (_ "Missing pinnumber= for refdes=~A)") refdes)
       `(,refdes . "?"))
      ;; Otherwise, anything is OK, return it as is.  Even if
      ;; refdes=#f and pinnumber is non-#f, it is an acceptable case
      ;; for using with the "net=" attribute. Return it as is.
      (x x))))


;;; Checks if OBJECT is a pin that should be treated as one
;;; defining a name of the net connected to it via the "net="
;;; attribute of its parent component object.  Such components
;;; (e.g. "gnd-1.sym") have to have no refdes, and their "net="
;;; components should correspond to pinnumbers of their existing
;;; pins.
(define (net-attrib-pin? object)
  (and (net-pin? object)
       (let ((refdes (attrib-value-by-name (object-component object)
                                           "refdes"))
             (pinnumber (attrib-value-by-name object "pinnumber")))
         (and pinnumber (not refdes)))))


(define (update-component-pins schematic-component)
  (define (update-package-pin-name pin)
    (set-package-pin-name! pin
                           (schematic-connection-override-name (package-pin-port-connection pin))))

  (for-each update-package-pin-name
            (schematic-component-pins schematic-component)))

(define %unnamed-net-counter 0)
(define (increment-unnamed-net-counter)
  (set! %unnamed-net-counter (1+ %unnamed-net-counter))
  %unnamed-net-counter)


(define (create-unnamed-netname tag)
  (define (hierarchical-default-name s)
    (create-net-name (string-append (gnetlist-config-ref 'default-net-name) s)
                     tag
                     ;; The below means just #f.
                     (not 'power-rail)))
  ((if (eq? (netlist-mode) 'spice) identity hierarchical-default-name)
   (number->string (increment-unnamed-net-counter))))


(define %unnamed-pin-counter 0)
(define (increment-unnamed-pin-counter)
  (set! %unnamed-pin-counter (1+ %unnamed-pin-counter))
  %unnamed-pin-counter)


(define (create-unconnected-netname)
  (string-append "unconnected_pin-"
                 (number->string (increment-unnamed-pin-counter))))


(define (make-special-netname objects hierarchy-tag)
  (if (and (= (length objects) 1)
           (net-pin? (car objects)))
      (create-unconnected-netname)
      (create-unnamed-netname hierarchy-tag)))


(define (nets-netname connection)
  (define (unnamed? name)
    (eq? 'unnamed (car name)))

  (define (net? name)
    (eq? 'net (car name)))

  (let ((name (make-hierarchical-connection-name connection)))
    (set-schematic-connection-override-name!
     connection
     (if (unnamed? name)
         (make-special-netname (schematic-connection-objects connection)
                               (cdr name))
         (create-net-name (cadr name) (cddr name) (net? name))))))


(define (compat-refdes schematic-component)
  (set-schematic-component-refdes! schematic-component
                                   (hierarchical-refdes->string
                                    (schematic-component-refdes schematic-component)))
  schematic-component)


(define (hierarchy-post-process components connections)
  (define subcircuit-components
    (filter schematic-component-subcircuit? components))

  (define (component-subcircuit-ports component)
    (schematic-component-ports component))

  (define (disable-component-refdes component)
    (set-schematic-component-refdes! component #f))

  (define (fix-composite-component component)
    ;; Disable refdeses of all inner port components.
    (for-each disable-component-refdes
              (cons component
                    (map schematic-port-inner-component
                         (component-subcircuit-ports component)))))

  (define (net-map-pin? pin)
    (package-pin-net-map pin))

  (define (set-net-map-pin-name! pin)
    (and (net-map-pin? pin)
         (set-package-pin-name!
          pin
          (create-net-name (net-map-netname (package-pin-net-map pin))
                           (subschematic-name
                            (schematic-component-parent (package-pin-parent pin)))
                           'power-rail))))

  (for-each nets-netname connections)

  (for-each set-net-map-pin-name!
            (append-map schematic-component-pins components))

  (for-each update-component-pins components)

  (for-each fix-composite-component subcircuit-components)

  (map compat-refdes components))
