;;; Lepton EDA netlister
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
;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.

(define-module (netlist verbose)
  #:use-module (ice-9 format)
  #:use-module (netlist option)
  #:use-module (netlist schematic-component)
  #:use-module (netlist schematic-connection)
  #:use-module (netlist subschematic)
  #:use-module (netlist package-pin)

  #:export (verbose-print-netlist))

(define (verbose-print-netlist netlist)
  (define (print-pin-connection-info pin)
    (let ((refdes (schematic-component-refdes (package-pin-parent pin)))
          (pinnumber (package-pin-number pin)))
      (if (and refdes pinnumber)
          (format #f "\t\t~A ~A [~A]\n"
                  refdes
                  pinnumber
                  (package-pin-id pin))
          "")))

  (define (print-pin-info pin)
    (format #f "\tpin~A (~A) ~A\n~A\n"
            (or (package-pin-number pin) "?")
            (or (package-pin-label pin) "")
            (or (package-pin-name pin) "Null net name")
            (string-join
             (map print-pin-connection-info
                  (schematic-connection-pins (package-pin-connection pin)))
             "")))

  (define (print-pin-list pin-list)
    (map print-pin-info pin-list))

  (define (print-schematic-component-info package)
    (format #f "component ~S\n~
                Hierarchy tag: ~S\n~
                ~A\n"
            (or (schematic-component-refdes package) "SPECIAL")
            (or (subschematic-name (schematic-component-parent package)) "")
            (print-pin-list (schematic-component-pins package))))

   (format #t "\nInternal netlist representation:\n\n~
               ~A\n"
           (string-join (map print-schematic-component-info netlist) "")))
