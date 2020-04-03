;;; Lepton EDA netlister
;;; Copyright (C) 2007-2010 John P. Doty
;;; Copyright (C) 2007-2017 gEDA Contributors
;;; Copyright (C) 2017-2018 Lepton EDA Contributors
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

;; Netlister for symbolic circuit analysis using Mathematica.
;; See the Mathematica notebook gEDA.nb (obtainable at www.noqsi.com)
;; for usage.

(use-modules (srfi srfi-1)
             (netlist schematic)
             (netlist schematic toplevel))

(define (netname-connections->pin-voltages netname)
  (define package car)
  (define pinnumber cdr)
  (define (connection->voltage-equation connection)
    (format #f "v[\"~A\",\"~A\"]=v[\"~A\"];\n"
            (package connection)
            (pinnumber connection)
            netname))
  (string-join (map connection->voltage-equation
                    (get-all-connections netname))
               ""))

(define (netnames->pin-voltages netnames)
  (string-join (map netname-connections->pin-voltages netnames) ""))

(define (netname->node-currents netname)
  (define package car)
  (define pinnumber cdr)
  (define (connection->node-current-string connection)
    (format #f "i[\"~A\",\"~A\"]"
            (package connection)
            (pinnumber connection)))
  (string-join (map connection->node-current-string
                    (get-all-connections netname))
               "+"))


(define (netnames->current-string netnames)
  (define (netname->current-string netname)
    (and (not (string=? netname "GND"))
         (format #f "~A==0"
                 (netname->node-currents netname))))
  (string-join (filter-map netname->current-string netnames) ",\n"))



(define (model->string refdes)
  (define (get-device-model-string model refdes)
    (format #f "~A[\"~A\"]" model refdes))

  (define (get-device-value-string device value refdes)
    (format #f "~A[value->~A][\"~A\"]"
            (string-downcase device) value refdes))

  (let ((device (gnetlist:get-package-attribute refdes "device"))
        (value (gnetlist:get-package-attribute refdes "value"))
        (model (gnetlist:get-package-attribute refdes "model")))

    (if (unknown? model)
        (get-device-value-string device
                                 (if (unknown? value)
                                     (string-downcase refdes)
                                     value)
                                 refdes)
        (get-device-model-string model refdes))))

(define (models->string refdeses)
  (string-join (map model->string refdeses) ",\n"))

(define (netnames->voltage-string netnames)
  (define (netname->voltage-string netname)
    (and (not (string=? netname "GND"))
         (format #f "v[\"~A\"]" netname)))

  (string-join (filter-map netname->voltage-string netnames) ",\n" 'suffix))


(define (netnames->connection-currents netnames)
  (define package car)
  (define pinnumber cdr)
  (define (connection->current-string connection)
    (format #f "i[\"~A\",\"~A\"]" (package connection) (pinnumber connection)))

  (string-join (map connection->current-string
                    (sort (append-map get-all-connections netnames)
                          pair<?))
               ",\n"))


(define (mathematica output-filename)
  (let ((nets (schematic-nets (toplevel-schematic)))
        (packages (schematic-package-names (toplevel-schematic))))
    (display (netnames->pin-voltages nets))
    (display "nodeEquations={\n")
    (display (netnames->current-string nets))
    (display "};\n")
    (display "modelEquations={\n")
    (display (models->string packages))
    (display "};\n")
    (display "variables={\n")
    (display (netnames->voltage-string nets))
    (display (netnames->connection-currents nets))
    (display "};\n")))
