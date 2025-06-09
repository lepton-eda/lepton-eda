;;; Lepton EDA netlister
;;; Copyright (C) 2019 Lepton EDA Contributors
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

(define-module (netlist schematic-port)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (netlist package-pin)
  #:export-syntax (make-schematic-port schematic-port?
                   schematic-port-id set-schematic-port-id!
                   schematic-port-inner-pin set-schematic-port-inner-pin!
                   schematic-port-outer-pin set-schematic-port-outer-pin!)

  #:export (schematic-port-inner-component
            schematic-port-outer-component))

(define-record-type <schematic-port>
  (make-schematic-port inner-pin outer-pin)
  schematic-port?
  (inner-pin schematic-port-inner-pin set-schematic-port-inner-pin!)
  (outer-pin schematic-port-outer-pin set-schematic-port-outer-pin!))


(define (schematic-port-inner-component port)
  "Returns the parent component of PORT's inner pin."
  (and=> (schematic-port-inner-pin port) package-pin-parent))


(define (schematic-port-outer-component port)
  "Returns the parent component of PORT's outer pin."
  (and=> (schematic-port-outer-pin port) package-pin-parent))
