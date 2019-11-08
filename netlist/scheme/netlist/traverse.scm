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

(define-module (netlist traverse)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (geda object)
  #:use-module (netlist net)
  #:use-module (netlist package-pin)
  #:use-module (netlist pin-net)
  #:use-module (netlist schematic-component)
  #:use-module (netlist subschematic)
  #:use-module (symbol check net-attrib)

  #:export (set-package-pin-nets-properties!))


;;; Tracks which objects have been visited so far, and how many
;;; times.
(define %visits '())
;;; Increment the current visit count for a particular OBJECT.
(define (visit! object)
  (set! %visits (cons object %visits)))
;;; Retrieve the current visit count for a particular OBJECT.
(define (visited? object)
  (member object %visits))
;;; Reset all visit counts. Simply clears the %visits completely.
(define (clear-visits!)
  (set! %visits '()))


(define (traverse-net pin-object)
  (define (traverse-net-object connection-objects starting object)
    (visit! object)
    (let ((nets (cons object connection-objects)))
      (if (or (net? object)
              starting)
          (let loop ((connections (object-connections object))
                     (nets nets))
            (if (null? connections)
                nets
                (loop (cdr connections)
                      (let ((conn (car connections)))
                        (if (visited? conn)
                            nets
                            (traverse-net-object nets #f conn))))))
          nets)))

  (clear-visits!)

  (if (null? (object-connections pin-object))
      ;; If there is no connections, we have an only pin. There is
      ;; no point to do something in this case.
      '()
      (reverse (traverse-net-object '() #t pin-object))))


(define (make-new-pin-net object)
  (make-pin-net
   ;; id
   (object-id object)
   ;; object
   object
   ;; name
   #f))


(define (nets-netnames nets)
  (filter-map
   (lambda (x) (let ((object (pin-net-object x)))
            (and (net? object)
                 (attrib-value-by-name object "netname"))))
   nets))


(define (set-real-package-pin-nets-properties! pin)
  (let* ((tag
          (subschematic-name (schematic-component-parent
                              (package-pin-parent pin))))
         (pin-object (package-pin-object pin))
         (nets (map make-new-pin-net (traverse-net pin-object)))
         (net-objects (filter (lambda (x) (net? (pin-net-object x))) nets))
         (pin-objects (filter (lambda (x) (pin? (pin-net-object x))) nets)))
    (set-package-pin-nets! pin nets)
    (set-package-pin-netname! pin (nets-netnames nets))
    (for-each (cut assign-net-netname! <> tag) net-objects)
    (for-each (cut assign-pin-properties! <> tag) pin-objects)
    pin))


(define (set-net-map-package-pin-nets-properties! pin)
  (let* ((parent-component (package-pin-parent pin))
         (tag (subschematic-name (schematic-component-parent parent-component)))
         (netname (create-net-name (net-map-netname (package-pin-net-map pin))
                                   tag
                                   'power-rail))
         (nets (list (make-pin-net (package-pin-id pin)
                                   (package-pin-object pin)
                                   netname))))
    (set-package-pin-nets! pin nets)))


(define (set-package-pin-nets-properties! component)
  (define (real-pin? pin)
    (package-pin-object pin))

  (define (set-nets-properties! pin)
    (if (real-pin? pin)
        (set-real-package-pin-nets-properties! pin)
        (set-net-map-package-pin-nets-properties! pin)))

  (for-each set-nets-properties! (schematic-component-pins component)))
