;;; gEDA - GPL Electronic Design Automation
;;; gnetlist - gEDA Netlist
;;; Backend for propagating pin names from gschem to footprints in pcb
;;; Copyright (C) 2005 Dan McMahill
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

(use-modules (srfi srfi-13) (srfi srfi-14))

;; write out the pins for a particular component
(define pcbpins:component_pins
  (lambda (port package pins)
    (if (and (not (null? package)) (not (null? pins)))
	(begin
	  (let (
		(pin (car pins))
		(label #f)
		(pinnum #f)
		)
	    (display "ChangePinName(" port)
	    (display package port)
	    (display ", " port)

	    (set! pinnum (gnetlist:get-attribute-by-pinnumber package pin "pinnumber"))

	    (display pinnum port)
	    (display ", " port)

	    (set! label (gnetlist:get-attribute-by-pinnumber package pin "pinlabel"))
	    (if (string=? label "unknown") 
		(set! label pinnum)
		)
	    (display label port)
	    (display ")\n" port)
	    )
	  (pcbpins:component_pins port package (cdr pins))
	  )
	)
    )
  )

	    
;; write out the components
(define pcbpins:components
   (lambda (port packages symcnt)
      (if (not (null? packages))
         (begin
	   (let ((package (car packages)))

	     ;;
	     (display "\n# Start of element " port)
	     (display package port)
	     (newline port)

	     ;; write the pins
	     (pcbpins:component_pins port package (gnetlist:get-pins package))
	     )
	   (pcbpins:components port (cdr packages) (+ symcnt 1))
	   )
	 )
      ) 
   )

;; The top level netlister for pcbpins
(define pcbpins
  (lambda (filename)
    (newline)
    (display "---------------------------------\n")
    (display "gEDA/gnetlist pcbpins Backend\n")
    (display "This backend is EXPERIMENTAL\n")
    (display "Use at your own risk!\n")
    (display "---------------------------------\n\n")
    
    (let ((port (open-output-file filename)))
      
      ;; write the header
      (display "# Pin name action command file\n" port)
      
      ;; write the components
      (pcbpins:components port packages 1)
      
      ;; close netlist
      (close-output-port port)
      )
    )
  )

;; Custom get-uref function to stip lowercase suffixes
(define (get-uref object)
  (let ((real_uref (gnetlist:get-uref object)))
    (if real_uref
      (string-trim-right real_uref char-set:lower-case)
      #f
    )
  )
)
