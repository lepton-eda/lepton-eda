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

(use-modules (ice-9 optargs)
             (netlist schematic)
             (netlist schematic toplevel))

;; --------------------------------------------------------------------------
;;
;; protelII netlist format specific functions go here
;;
;; PROTEL NETLIST 2.0
;; [   -- element for list of components
;; DESIGNATOR
;;   REFDES attribute.
;; FOOTPRINT
;;   FOOTPRINT attrbute.
;; PARTTYPE
;;   Either:
;;     If VALUE attribute exists, output VALUE attribute.
;;     Otherwise, output DEVICE attrbute.
;;     (This covers the case of ICs, which usually carry their part no (e.g. uA741) in the DEVICE attribute.)
;; DESCRIPTION
;;   DEVICE attribute
;; Part Field 1
;; *
;; Part Field 2
;; *
;; Part Field 3
;; *
;; Part Field 4
;; *
;; Part Field 5
;; *
;; Part Field 6
;; *
;; Part Field 7
;; *
;; Part Field 8
;; *
;; Part Field 9
;; *
;; Part Field 10
;; *
;; Part Field 11
;; *
;; Part Field 12
;; *
;; Part Field 13
;; *
;; Part Field 14
;; *
;; Part Field 15
;; *
;; Part Field 16
;; *
;; LIBRARYFIELD1
;; empty line
;; LIBRARYFIELD2
;; empty line
;; LIBRARYFIELD3
;; empty line
;; LIBRARYFIELD4
;; empty line
;; LIBRARYFIELD5
;; empty line
;; LIBRARYFIELD6
;; empty line
;; LIBRARYFIELD7
;; empty line
;; LIBRARYFIELD8
;; empty line
;; ]
;; [
;; ... other components ...
;; ]
;; (  -- element for list of nets
;; NETNAME
;; PART-PIN# VALUE-PINNAME PINTYPE  -- use PASSIVE for PINTYPE
;; ...more connections...
;; )
;; (
;; ...more nets...
;; )
;; { -- element for net option list
;; NETNAME
;; OPTION
;; OPTIONVALUE
;; TRACK
;; 24
;; VIA
;; 40
;; NET TOPOLOGY
;; SHORTEST
;; ROUTING PRIORITY
;; MEDIUM
;; LAYER
;; UNDEFINED
;; }
;; {
;; ...more net options...
;; }
;;

;;
;; Top level header
;;
(define (protelII:write-top-header)
  (display "PROTEL NETLIST 2.0\r\n"))

;;
;; Top level component writing
;;
(define (protelII:components packages)
  (for-each
   (lambda (package)
     (let* ((footprint (gnetlist:get-package-attribute package "footprint"))
            (device (gnetlist:get-package-attribute package "device"))
            (value (gnetlist:get-package-attribute package "value")))
       (format #t "[\r
DESIGNATOR\r
~A\r
FOOTPRINT\r
~A\r
PARTTYPE\r
~A\r
DESCRIPTION\r
~A\r
Part Field 1\r
*\r
Part Field 2\r
*\r
Part Field 3\r
*\r
Part Field 4\r
*\r
Part Field 5\r
*\r
Part Field 6\r
*\r
Part Field 7\r
*\r
Part Field 8\r
*\r
Part Field 9\r
*\r
Part Field 10\r
*\r
Part Field 11\r
*\r
Part Field 12\r
*\r
Part Field 13\r
*\r
Part Field 14\r
*\r
Part Field 15\r
*\r
Part Field 16\r
*\r
LIBRARYFIELD1\r
\r
LIBRARYFIELD2\r
\r
LIBRARYFIELD3\r
\r
LIBRARYFIELD4\r
\r
LIBRARYFIELD5\r
\r
LIBRARYFIELD6\r
\r
LIBRARYFIELD7\r
\r
LIBRARYFIELD8\r
\r
]\r
"
               package
               footprint
               (if (unknown? value) device value)
               device)))
   packages))

;;
;; Display the individual net connections
;;
(define (connections->string connections)
  (define package car)
  (define pinnumber cdr)
  (define (connection->string connection)
    (let* ((pack (package connection))
           (pin (pinnumber connection))
           (device (gnetlist:get-package-attribute pack "device")))
      (format #f
              "~A-~A ~A-~A PASSIVE"
              pack
              pin
              device
              pin)))

  (string-join (map connection->string connections) "\r\n" 'suffix))

;;
;; Display all nets
;;
(define (protelII:display-name-nets nets)
   (protelII:display-connections nets)
   (display " \r\n"))

;;
;; Write netname : uref pin, uref pin, ...
;;
(define (protelII:write-net netnames)
  (for-each
   (lambda (netname)
     (format #t
             "(\r\n~A\r\n~A)\r\n"
             netname
             (connections->string (get-all-connections netname))))
   netnames))

;;
;; Write the net part of the gEDA format
;;
(define (protelII:nets nets)
  (protelII:write-net nets))

;;; Highest level function
;;; Write my special testing netlist format
;;;
(define (protelII output-filename)
  (protelII:write-top-header)
  (protelII:components (schematic-package-names (toplevel-schematic)))
  (protelII:nets (schematic-nets (toplevel-schematic))))

;;
;; gEDA's native test netlist format specific functions ends
;;
;; --------------------------------------------------------------------------
