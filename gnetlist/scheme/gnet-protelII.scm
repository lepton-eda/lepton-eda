;;; gEDA - GNU Electronic Design Automation
;;; gnetlist - GNU Netlist
;;; Copyright (C) 1998 Ales V. Hvezda
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
;;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;; --------------------------------------------------------------------------
;;
;; protelII netlist format specific functions go here 
;;
;; PROTEL NETLIST 2.0
;; [   -- element for list of components
;; DESIGNATOR
;; attrib uref
;; FOOTPRINT
;; attrib footprint
;; PARTTYPE
;; attrib value
;; DESCRIPTION
;; empty line
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
(define protelII:write-top-header
   (lambda (p)
      (display "PROTEL NETLIST 2.0" p) 
      (newline p)))
      
;;
;; header for components section
;;
(define protelII:start-components
   (lambda (p)
      (display "" p)))
;; no header for components   

;;
;; footer for components section
;;
(define protelII:end-components
   (lambda (p)
      (display "" p)))

;;
;; header for renamed section
;;
(define protelII:start-renamed-nets
   (lambda (p)
      (display "" p)))

;;
;; footer for renamed section
;;
(define protelII:end-renamed-nets
   (lambda (p)
      (display "" p)))

;;
;; header for nets section
;;
(define protelII:start-nets
   (lambda (p)
      (display "" p)))

;;
;; footer for net section
;;
(define protelII:end-nets
   (lambda (p)
      (display "" p)))
	
;;
;; Top level component writing 
;;
(define protelII:components
   (lambda (port ls)
      (if (not (null? ls))
         (let ((package (car ls)))
            (begin
	       (display "[" port)
	       (newline port)
	       (display "DESIGNATOR" port)
	       (newline port)
               (display package port)
	       (newline port)
	       (display "FOOTPRINT" port)
	       (newline port)
	       (display (gnetlist:get-package-attribute package  "physical") port)
	       (newline port)
	       (display "PARTTYPE" port)
	       (newline port)
               (display (get-value package) port)
	       (newline port)
	       (display "DESCRIPTION" port)
	       (newline port)
               (display (get-device package) port)
	       (newline port)
	       (display "Part Field 1" port)
	       (newline port)
               (display "*" port)
	       (newline port)
	       (display "Part Field 2" port)
	       (newline port)
               (display "*" port)
	       (newline port)
	       (display "Part Field 3" port)
	       (newline port)
               (display "*" port)
	       (newline port)
	       (display "Part Field 4" port)
	       (newline port)
               (display "*" port)
	       (newline port)
	       (display "Part Field 5" port)
	       (newline port)
               (display "*" port)
	       (newline port)
	       (display "Part Field 6" port)
	       (newline port)
               (display "*" port)
	       (newline port)
	       (display "Part Field 7" port)
	       (newline port)
               (display "*" port)
	       (newline port)
	       (display "Part Field 8" port)
	       (newline port)
               (display "*" port)
	       (newline port)
	       (display "Part Field 9" port)
	       (newline port)
               (display "*" port)
	       (newline port)
	       (display "Part Field 10" port)
	       (newline port)
               (display "*" port)
	       (newline port)
	       (display "Part Field 11" port)
	       (newline port)
               (display "*" port)
	       (newline port)
	       (display "Part Field 12" port)
	       (newline port)
               (display "*" port)
	       (newline port)
	       (display "Part Field 13" port)
	       (newline port)
               (display "*" port)
	       (newline port)
	       (display "Part Field 14" port)
	       (newline port)
               (display "*" port)
	       (newline port)
	       (display "Part Field 15" port)
	       (newline port)
               (display "*" port)
	       (newline port)
	       (display "Part Field 16" port)
	       (newline port)
               (display "*" port)
	       (newline port)
	       (display "LIBRARYFIELD1" port)
	       (newline port)
               (display "" port)
	       (newline port)
	       (display "LIBRARYFIELD2" port)
	       (newline port)
               (display "" port)
	       (newline port)
	       (display "LIBRARYFIELD3" port)
	       (newline port)
               (display "" port)
	       (newline port)
	       (display "LIBRARYFIELD4" port)
	       (newline port)
               (display "" port)
	       (newline port)
	       (display "LIBRARYFIELD5" port)
	       (newline port)
               (display "" port)
	       (newline port)
	       (display "LIBRARYFIELD6" port)
	       (newline port)
               (display "" port)
	       (newline port)
	       (display "LIBRARYFIELD7" port)
	       (newline port)
               (display "" port)
	       (newline port)
	       (display "LIBRARYFIELD8" port)
	       (newline port)
               (display "" port)
	       (newline port)
               (display "]" port)
	       (newline port)
               (protelII:components port (cdr ls)))))))

;;
;; renamed nets writing 
;;
(define protelII:renamed-nets
   (lambda (port ls)
      (if (not (null? ls))
         (let ((renamed-pair (car ls)))
            (begin
;;;	       (display renamed-pair) (newline)
;;;            (display (car renamed-pair) port)
;;;            (display " -> " port)
;;;            (display (car (cdr renamed-pair)) port)
;;;            (newline port)
               (display "" port)
               (protelII:renamed-nets port (cdr ls)))))))

;;
;; Display the individual net connections
;;
(define protelII:display-connections
   (lambda (nets port)
      (if (not (null? nets))
  	 (begin
	    (let ((package (car (car nets))))
	       (display package port)
	       (write-char #\- port) 
	       (display (car (cdr (car nets))) port)
	       (display " " port)
	       (display (get-device package) port)
	       (display "-" port)
	       (display (car (cdr (car nets))) port)
	       (display " PASSIVE" port))
	    (if (not (null? (cdr nets)))
	       (begin
	          (newline port))) 
	    (protelII:display-connections (cdr nets) port)))))

;;
;; Display all nets 
;;
(define protelII:display-name-nets
   (lambda (port nets)
      (begin
         (protelII:display-connections nets port)
         (write-char #\space port) 
         (newline port))))

;;
;; Write netname : uref pin, uref pin, ...
;;
(define protelII:write-net
   (lambda (port netnames)
      (if (not (null? netnames))
         (let ((netname (car netnames)))
	    (begin
	       (display "(" port)
	       (newline port)
	       (display netname port)
	       (newline port)
               (protelII:display-name-nets port (gnetlist:get-all-connections netname))
	       (display ")" port)
	       (newline port)
	       (protelII:write-net port (cdr netnames))))))) 

;;
;; Write the net part of the gEDA format
;;
(define protelII:nets
   (lambda (port)
      (let ((all-uniq-nets (gnetlist:get-all-unique-nets "dummy")))
         (protelII:write-net port all-uniq-nets))))

;;; Highest level function
;;; Write my special testing netlist format
;;;
(define protelII 
   (lambda (output-filename)
      (let ((port (open-output-file output-filename)))
         (begin
;;;         (gnetlist:set-netlist-mode "gEDA") No longer needed
            (protelII:write-top-header port)
            (protelII:start-components port)
            (protelII:components port packages)
            (protelII:end-components port)
            (protelII:start-renamed-nets port)
            (protelII:renamed-nets port (gnetlist:get-renamed-nets "dummy"))
            (protelII:end-renamed-nets port)
            (protelII:start-nets port)
            (protelII:nets port)
            (protelII:end-nets port))
         (close-output-port port))))

;;
;; gEDA's native test netlist format specific functions ends 
;;
;; --------------------------------------------------------------------------
