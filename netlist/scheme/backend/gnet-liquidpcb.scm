;;; Lepton EDA netlister
;;; Copyright (C) 2008-2010 Ales Hvezda
;;; Copyright (C) 2012-2017 gEDA Contributors
;;; Copyright (C) 2018 Lepton EDA Contributors
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

;; --------------------------------------------------------------------------
;;
;; liquid pcb gnetlist backend
;;

(use-modules (netlist schematic)
             (netlist schematic toplevel))

;;
;; Write the individual net connections
;;
(define (connections->string connections)
  (define package car)
  (define pinnumber cdr)
  (define (connection->string connection)
    (format #f "\t\t\t<netnode component=\"~A\" pin=~A />\n"
            (package connection)
            (pinnumber connection)))
  (string-join (map connection->string connections) ""))


;;
;; Write netname : uref pin, uref pin, ...
;;
(define (nets->liquidpcb-netlist netnames)
  (define (net->string netname)
    (format #f "\t\t<net name=\"~A\">\n~A\t\t</net>\n"
            netname
            (connections->string (get-all-connections netname))))
  (map net->string netnames))


;;
;; Highest level function
;;
(define (liquidpcb output-filename)
  (display "<LiquidPCB>\n")
  (display "\t<netlist name=\"Main netlist\">\n")
  (for-each display
            (nets->liquidpcb-netlist (schematic-nets (toplevel-schematic))))
  (display "\t</netlist>\n")
  (display "</LiquidPCB>\n"))

;;
;; liquid PCB netlist backend ends
;;
;; --------------------------------------------------------------------------
