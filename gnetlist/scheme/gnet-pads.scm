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

;; PADS netlist format

(define pads:components
   (lambda (port packages)
      (if (not (null? packages))
         (begin
            (let ((pattern (gnetlist:get-package-attribute (car packages) 
                                                           "pattern"))
                  (package (car packages)))
               (if (not (string=? pattern "unknown"))
                  (display pattern port))
               (display package port)
	       (write-char #\tab port) 
               (display (gnetlist:get-package-attribute package "footprint") port)
               (newline port))
            (pads:components port (cdr packages))))))

(define pads:display-connections
   (lambda (port nets)
      (if (not (null? nets))
	 (begin
	    (write-char #\space port) 
	    (display (car (car nets)) port)
	    (write-char #\. port) 
	    (display (car (cdr (car nets))) port)
	    (if (null? (cdr nets))
	       (newline port)
               (begin
	          (pads:display-connections port (cdr nets))
		))))))

(define pads:write-net
   (lambda (port netnames)
      (if (not (null? netnames))
         (let ((netname (car netnames)))
	    (display "*SIGNAL* " port)
	    (display netname port)
	    (newline port)
            (pads:display-connections port (gnetlist:get-all-connections netname))
	    (pads:write-net port (cdr netnames)))))) 

(define pads 
   (lambda (filename)
      (let ((port (open-output-file filename)))
         (display "!PADS-POWERPCB-V3.0-MILS!\n" port)
         (display "\n*PART*\n" port)
         (pads:components port packages)
         (display "\n*NET*\n" port)
         (pads:write-net port (gnetlist:get-all-unique-nets "dummy"))
         (display "\n*END*\n" port)
         (close-output-port port))))

