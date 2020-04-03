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

;; --------------------------------------------------------------------------
;;
;; Bartels Format
;; Layout board;
;; PARTS
;;   part : footprint;
;; CONNECT
;;   /net1/ uref.pin=uref.pin=uref.pin=...uref.pin;
;;   /net2/ PRIORITY(1..100) MINDIST(mm) ROUTWIDTH(mm) uref.pin(width_mm)=...;
;; END.
;;

(use-modules (netlist schematic)
             (netlist schematic toplevel))
;;
;; Top level component writing
;;
(define (bae:components ls)
  (define (output-package package)
    (format #t "    ~A : ~A;\n"
            package
            (gnetlist:get-package-attribute package "footprint")))
  (for-each output-package ls))

;;
;; Display the individual net connections
;;
(define (connections->string connections)
  (define package car)
  (define pinnumber cdr)
  (define (connection->string connection)
    (format #f "~A.~A" (package connection) (pinnumber connection)))
  (string-join (map connection->string connections) "="))

;;
;; Write netname : uref pin, uref pin, ...
;;
(define (bae:write-net netname)
  (format #t "    /'~A'/ ~A;\n" netname
          (connections->string (get-all-connections netname))))

;;
;; Write the net part of the gEDA format
;;
(define (bae:nets nets)
  (for-each bae:write-net nets))

;;; Highest level function
;;; Write my special testing netlist format
;;;
(define (bae output-filename)
  (display "LAYOUT board;\n")
  (display "PARTS\n")
  (bae:components (schematic-package-names (toplevel-schematic)))
  (display "CONNECT\n")
  (bae:nets (schematic-nets (toplevel-schematic)))
  (display "END.\n"))
