;;; gEDA - GNU Electronic Design Automation
;;; gnetlist - GNU Netlist
;;; Copyright (C) 1998-2000 Ales V. Hvezda
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
;; TANGO netlist backend written by Nuno Sucena starts here
;;

;;
;; Given a uref, returns the device attribute value (for tango-netlist)
;;
(define tango:get-device
   (lambda (package)
      (gnetlist:get-package-attribute package "device")))

;;
;; Given a uref, returns the pattern attribute value (PATTERN if not defined)
;;
(define tango:get-pattern
   (lambda (package)
      (define pattern (gnetlist:get-package-attribute package "pattern"))
      (if (string=? "unknown" pattern)
         "PATTERN"
         pattern)))
; how do i return "PATTERN" if not defined? humm... need to read some
; guile stuff... i did, and see the result :)

;;
;; Given a uref, returns the value attribute (empty if not defined)
;;
(define tango:get-value
   (lambda (package)
      (define value (gnetlist:get-package-attribute package "value"))
      (if (string=? "unknown" value)
         ""
	 value)))
 
;;
;; Top level header
;;
(define tango:write-top-header
   (lambda (p)
      (display "START header" p) 
      (newline p)
      (newline p)
      (display "TANGO netlist for gnetlist" p)
      (newline p)
      (display "TANGO gnetlist backend written by Nuno Sucena" port)
      (newline p)
      (display "END header" p)
      (newline p)
      (newline p)))

;;
;; Top level component writing 
;;
(define tango:components
   (lambda (port ls)
      (if (not (null? ls))
         (let ((package (car ls)))
	    (begin
	       (display "[" port)
	       (newline port)
	       (display package port)
	       (newline port)
	       (display (tango:get-pattern package) port)
	       (newline port)
	       (display (tango:get-device package) port)
	       (newline port)
	       (display (tango:get-value package) port)
	       (newline port)
	       (newline port)
	       (display "]" port)
	       (newline port)
	       (tango:components port (cdr ls)))))))

;;
;; Display the individual net connections
;;
(define tango:display-connections
   (lambda (nets port)
      (if (not (null? nets))
	 (begin
	   (display (car (car nets)) port)
	   (display "-" port) 
	   (display (car (cdr (car nets))) port)
	   (if (not (null? (cdr nets)))
		(begin
		  (newline port)))
	          (tango:display-connections (cdr nets) port)))))


;;
;; Properly format the name of the net and the actual net connections
;;
(define tango:display-name-nets
   (lambda (port nets)
      (begin
         (tango:display-connections nets port))))

;;
;; Write out a net associated with a particular package and pin
;;
(define tango:write-net
   (lambda (port netnames)
      (if (not (null? netnames))
         (let ((netname (car netnames)))
	    (begin
	       (display "(" port)
               (newline port)
               (display netname port)
               (newline port)

	       (tango:display-name-nets port (gnetlist:get-all-connections netname))
	       (newline port)
	       (display ")" port)
	       (newline port)
	       (tango:write-net port (cdr netnames))))))) 


;;
;; Top level function to write out nets associated with a particular component
;;
(define tango:nets
   (lambda (port)
      (let ((all-uniq-nets (gnetlist:get-all-unique-nets "dummy")))
         (tango:write-net port all-uniq-nets))))

;;; Highest level function
;;; Write tango netlist format
;;;
(define tango
   (lambda (output-filename)
      (let ((port (open-output-file output-filename)))
         (begin
;;;	    (gnetlist:set-netlist-mode "TANGO") No longer needed
	    (tango:components port packages)
	    (tango:nets port))
	 (close-output-port port))))

;;
;; TANGO netlist backend written by Nuno Sucena ends here
;;
;; --------------------------------------------------------------------------

