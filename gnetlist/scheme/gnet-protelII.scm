;;; gEDA - GPL Electronic Design Automation
;;; gnetlist - gEDA Netlist
;;; Copyright (C) 1998-2010 Ales Hvezda
;;; Copyright (C) 1998-2010 gEDA Contributors (see ChangeLog for details)
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

(use-modules (ice-9 optargs))

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

;; We redefine the newline function, because this file format requires
;; Windows-style "\r\n" line endings rather than Unix-style "\n"
;; endings.
(define* (newline #:optional)
  (display "\r\n" (or (current-output-port))))

;;
;; Top level header
;;
(define (protelII:write-top-header)
  (display "PROTEL NETLIST 2.0")
  (newline))

;;
;; Top level component writing
;;
(define protelII:components
   (lambda (ls)
      (if (not (null? ls))
         (let ((package (car ls)))
            (begin
               (display "[")
               (newline)
               (display "DESIGNATOR")
               (newline)
               (display package)
               (newline)
               (display "FOOTPRINT")
               (newline)
               (display (gnetlist:get-package-attribute package  "footprint"))
               (newline)
               (display "PARTTYPE")
               (newline)
               (let ((value (get-value package)))          ;; This change by SDB on 10.12.2003.
                     (if (string-ci=? value "unknown")
                         (display (get-device package))
                         (display value)
                         )
               )
               (newline)
               (display "DESCRIPTION")
               (newline)
               (display (get-device package))
               (newline)
               (display "Part Field 1")
               (newline)
               (display "*")
               (newline)
               (display "Part Field 2")
               (newline)
               (display "*")
               (newline)
               (display "Part Field 3")
               (newline)
               (display "*")
               (newline)
               (display "Part Field 4")
               (newline)
               (display "*")
               (newline)
               (display "Part Field 5")
               (newline)
               (display "*")
               (newline)
               (display "Part Field 6")
               (newline)
               (display "*")
               (newline)
               (display "Part Field 7")
               (newline)
               (display "*")
               (newline)
               (display "Part Field 8")
               (newline)
               (display "*")
               (newline)
               (display "Part Field 9")
               (newline)
               (display "*")
               (newline)
               (display "Part Field 10")
               (newline)
               (display "*")
               (newline)
               (display "Part Field 11")
               (newline)
               (display "*")
               (newline)
               (display "Part Field 12")
               (newline)
               (display "*")
               (newline)
               (display "Part Field 13")
               (newline)
               (display "*")
               (newline)
               (display "Part Field 14")
               (newline)
               (display "*")
               (newline)
               (display "Part Field 15")
               (newline)
               (display "*")
               (newline)
               (display "Part Field 16")
               (newline)
               (display "*")
               (newline)
               (display "LIBRARYFIELD1")
               (newline)
               (display "")
               (newline)
               (display "LIBRARYFIELD2")
               (newline)
               (display "")
               (newline)
               (display "LIBRARYFIELD3")
               (newline)
               (display "")
               (newline)
               (display "LIBRARYFIELD4")
               (newline)
               (display "")
               (newline)
               (display "LIBRARYFIELD5")
               (newline)
               (display "")
               (newline)
               (display "LIBRARYFIELD6")
               (newline)
               (display "")
               (newline)
               (display "LIBRARYFIELD7")
               (newline)
               (display "")
               (newline)
               (display "LIBRARYFIELD8")
               (newline)
               (display "")
               (newline)
               (display "]")
               (newline)
               (protelII:components (cdr ls)))))))

;;
;; Display the individual net connections
;;
(define protelII:display-connections
   (lambda (nets)
      (if (not (null? nets))
         (begin
            (let ((package (car (car nets))))
               (display package)
               (write-char #\-)
               (display (car (cdr (car nets))))
               (display " ")
               (display (get-device package))
               (display "-")
               (display (car (cdr (car nets))))
               (display " PASSIVE"))
            (if (not (null? (cdr nets)))
               (begin
                  (newline)))
            (protelII:display-connections (cdr nets))))))

;;
;; Display all nets
;;
(define protelII:display-name-nets
   (lambda (nets)
      (begin
         (protelII:display-connections nets)
         (write-char #\space)
         (newline))))

;;
;; Write netname : uref pin, uref pin, ...
;;
(define protelII:write-net
   (lambda (netnames)
      (if (not (null? netnames))
         (let ((netname (car netnames)))
            (begin
               (display "(")
               (newline)
               (display netname)
               (newline)
               (protelII:display-name-nets (gnetlist:get-all-connections netname))
               (display ")")
               (newline)
               (protelII:write-net (cdr netnames)))))))

;;
;; Write the net part of the gEDA format
;;
(define protelII:nets
   (lambda ()
      (let ((all-uniq-nets (gnetlist:get-all-unique-nets "dummy")))
         (protelII:write-net all-uniq-nets))))

;;; Highest level function
;;; Write my special testing netlist format
;;;
(define (protelII output-filename)
  (set-current-output-port (gnetlist:output-port output-filename))
  (begin
    (protelII:write-top-header)
    (protelII:components packages)
    (protelII:nets))
  (close-output-port (current-output-port)))

;;
;; gEDA's native test netlist format specific functions ends
;;
;; --------------------------------------------------------------------------
