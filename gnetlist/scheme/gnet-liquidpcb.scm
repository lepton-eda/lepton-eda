;;; gEDA - GPL Electronic Design Automation
;;; gnetlist - gEDA Netlist
;;; Copyright (C) 2008 Ales Hvezda
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
;; liquid pcb gnetlist backend
;;

;;
;; Top level header
;;
(define liquidpcb:write-top-header
   (lambda (p)
      (display "<LiquidPCB>" p) 
      (newline p)))

;;
;; Bottom footer
;;
(define liquidpcb:write-bottom-footer
   (lambda (p)
      (display "</LiquidPCB>" p) 
      (newline p)))

;; 
;; Header for netlist section
;;
(define liquidpcb:start-netlist
   (lambda (p)
      (display "	<netlist name=\"Main netlist\">" p)
      (newline p)))

;;
;; footer for netlist section
;;
(define liquidpcb:end-netlist
   (lambda (p)
      (display "	</netlist>" p)
      (newline p)))
	
;;
;; Write the individual net connections
;;
(define liquidpcb:write-connections
   (lambda (nets port)
      (if (not (null? nets))
	 (begin
	    (display "			<netnode component=\"" port)
	    (display (car (car nets)) port)
	    (display "\" pin=" port)
	    (display (car (cdr (car nets))) port)
	    (display " />" port)
	    (newline port)
	    (liquidpcb:write-connections (cdr nets) port)))))


;;
;; Write netname : uref pin, uref pin, ...
;;
(define liquidpcb:write-net
   (lambda (port netnames)
      (if (not (null? netnames))
         (let ((netname (car netnames)))
	    (begin
 	       (display "		<net name=\"" port)
	       (display netname port)
	       (display "\">" port)
	       (newline port)
               (liquidpcb:write-connections (gnetlist:get-all-connections netname) port)
 	       (display "		</net>" port)
	       (newline port)
	       (liquidpcb:write-net port (cdr netnames))))))) 

;;
;; Write the netlist section of the liquidPCB format
;;
(define liquidpcb:write-netlist
   (lambda (port)
      (let ((all-uniq-nets (gnetlist:get-all-unique-nets "dummy")))
         (liquidpcb:write-net port all-uniq-nets))))

;;
;; Highest level function
;;
(define liquidpcb 
   (lambda (output-filename)
      (let ((port (open-output-file output-filename)))
         (begin
            (liquidpcb:write-top-header port)
            (liquidpcb:start-netlist port)
            (liquidpcb:write-netlist port)
            (liquidpcb:end-netlist port))
            (liquidpcb:write-bottom-footer port)
         (close-output-port port))))

;;
;; liquid PCB netlist backend ends
;;
;; --------------------------------------------------------------------------

