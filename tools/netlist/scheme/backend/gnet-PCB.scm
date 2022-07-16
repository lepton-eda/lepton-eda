;;; Lepton EDA netlister
;;; Copyright (C) 1998-2010 Ales Hvezda
;;; Copyright (C) 1998-2017 gEDA Contributors
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

;;  PCB format

(use-modules (netlist schematic)
             (netlist schematic toplevel))

(define (connection->string connection)
  (define refdes car)
  (define pin-number cdr)
  (string-append (refdes connection) "-" (pin-number connection)))

(define (net->string netname)
  (let ((connections (get-all-connections netname)))
    (string-append
     netname
     "\t"
     (gnetlist:wrap (string-join (map connection->string connections))
                    200
                    " \\")
     "\n")))

(define (nets->PCB-netlist nets)
  (map net->string nets))


(define (PCB output-filename)
  (for-each display
            (nets->PCB-netlist (schematic-nets (toplevel-schematic)))))
