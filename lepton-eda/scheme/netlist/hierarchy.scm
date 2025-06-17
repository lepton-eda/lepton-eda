;;; Lepton EDA netlister
;;; Copyright (C) 2016-2017 gEDA Contributors
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

(define-module (netlist hierarchy)
  #:use-module (srfi srfi-1)

  #:use-module (lepton gettext)
  #:use-module (lepton object)
  #:use-module (netlist attrib refdes)
  #:use-module (netlist config)
  #:use-module (netlist mode)
  #:use-module (netlist net)
  #:use-module (netlist package-pin)
  #:use-module (netlist schematic-component)
  #:use-module (netlist schematic-connection)
  #:use-module (netlist schematic-port)
  #:use-module (netlist subschematic)
  #:use-module (symbol check net-attrib)

  #:export (hierarchy-post-process))


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
    (create-net-name (string-append (netlist-config-ref 'default-net-name) s)
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

  (define (disable-component-refdes component)
    (set-schematic-component-refdes! component #f))

  (define (fix-composite-component component)
    ;; Disable refdeses of all inner port components.
    (for-each disable-component-refdes
              (cons component
                    (map schematic-port-inner-component
                         (schematic-component-ports component)))))

  (for-each nets-netname connections)

  (for-each update-component-pins components)

  (for-each fix-composite-component subcircuit-components)

  (map compat-refdes components))
