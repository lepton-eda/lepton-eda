;;; $Id$
;;;
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

;;
;; Verilog netlist backend written by Mike Jarabek starts here
;;
;; --------------------------------------------------------------------------

;; some useful regexes for working with net-names
;;
(use-modules (ice-9 regex))

(define id-regexp "[a-zA-Z_][a-zA-Z0-9_$]*")
(define numeric  "[0-9]+")
;; match on a verilog identifier like:  netname[x:y]
(define bit-range-reg (make-regexp
		       (string-append "^(" id-regexp ")[[:space:]]*" 
				      "\\["
				      "[[:space:]]*(" numeric ")[[:space:]]*"
				      ":"
				      "[[:space:]]*(" numeric ")[[:space:]]*"
				      "\\]")))

;; match on a verilog identifier like:  netname[x]
(define single-bit-reg (make-regexp 
			(string-append "^(" id-regexp ")[[:space:]]*"
				       "\\["
				       "[[:space:]]*(" numeric ")[[:space:]]*"
				       "\\]" )))

;; match on a verilog identifier like:  netname
(define simple-id-reg (make-regexp 
		       ( string-append "^(" id-regexp ")$" )))


;; return the top level block name for the module
(define verilog:get-module-name
  ( gnetlist:get-toplevel-attribute "module_name" ))

;; return a list of nets whose pins have the desired attribute name/value
;; pair
(define verilog:get-matching-nets
  (lambda (attribute value)
    (map car (verilog:filter attribute value packages))))

;; This function takes an attribute name, desired value, and a list of
;; packages.  For each of the packages, it looks up that attribute, and
;; if it matches, that package name is added to the list, and the function
;; recurses on the remaining packages.  If the attribute does not match, 
;; the function just recuses on the remaing packages. Thanks to Mohina Lal
;; for this trick.
;;

(define verilog:filter 
  (lambda (attribute value package-list)
    (cond ((null? package-list) '())
	  ((string=? (gnetlist:get-package-attribute (car package-list) 
						      attribute) value)
	   (cons 
	    (map (lambda (pin)
		   (car (gnetlist:get-nets (car package-list) pin)))
		 (pins (car package-list)))
	    (verilog:filter attribute value (cdr package-list))))
	  (else (verilog:filter attribute value (cdr package-list)))))
)


;;
;; Output the guts of the module ports here
;;
;; Scan through the list of components, and pins from each one, finding the
;; pins that have PINTYPE == CHIPIN, CHIPOUT, CHIPTRI (for inout)
;; build three lists one each for the inputs, outputs and inouts
;; return the a list of three lists that contain the pins in the order 
;; we want.
(define verilog:get-port-list
  (lambda ()
    ;; construct list
    (list (verilog:get-matching-nets "device" "IPAD")
	  (verilog:get-matching-nets "device" "OPAD")
	  (verilog:get-matching-nets "device" "IOPAD"))))

;;
;; output the meat of the module port section
;;
;; each line in the declaration is formatted like this:
;;
;;       PORTNAME , <newline>
;;
(define verilog:write-module-declaration
  (lambda (module-name port-list p)
    (begin
      (display "module " p)
      (verilog:display-escaped-identifier module-name p)
      (display " (" p)
      (newline p)
      (let ((the-pins ( append (car port-list)     ; build up list of pins
			       (cadr  port-list)
			       (caddr port-list))))
	(begin
	  ;(display the-pins)
	  ; do pins, but take care of last comma
	  (if (not (null? the-pins))
	      (begin
		(display "       " p) 
		(display (verilog:netname (car the-pins)) p)
		(if (not (null? (cdr the-pins)))
		    (for-each (lambda (pin)   ; loop over outputs
				(begin
				  (display " ," p)
				  (newline p)
				  (display "       " p)
				  (display (verilog:netname pin) p)))
			      (cdr the-pins) ))))
	(newline p)
	(display "      );" p)
	(newline p))))))
;;
;; output the module direction section
;;
(define verilog:write-port-directions
  (lambda (port-list p)
    (let ((in    (car   port-list))    ; extract list of pins 
	  (out   (cadr  port-list))
	  (inout (caddr port-list)))
      (begin
	(display "/* Port directions begin here */" p)
	(newline p)
	(for-each (lambda (pin)
		    (begin
		      (display "input " p)
		      (verilog:display-wire (verilog:get-net 
					     (verilog:netname pin)) p)
		      (display " ;" p)
		      (newline p))) in)       ; do each input

	(for-each (lambda (pin)
		    (begin
		      (display "output " p)
		      (verilog:display-wire (verilog:get-net 
					     (verilog:netname pin)) p)
		      (display " ;" p)
		      (newline p))) out)      ; do each output

	(for-each (lambda (pin)
		    (begin
		      (display "inout " p)
		      (verilog:display-wire (verilog:get-net 
					     (verilog:netname pin)) p)
		      (display " ;" p)
		      (newline p))) inout)    ; do each inout
		      
	(newline p)))))
;;
;; Top level header
;;

(define verilog:write-top-header
	(lambda (p)
	  (let ((port-list (verilog:get-port-list)))
	    (begin
	      (display "/* structural Verilog generated by gnetlist */\n" p)
	      (display "/* WARNING: This is a generated file, edits */\n" p)
	      (display "/*        made here will be lost next time  */\n" p)
	      (display "/*        you run gnetlist!                 */\n" p)
	      (display "/* Id ..........$Id$ */\n" p)
	      (display "/* Source.......$Source$ */\n" p)
	      (display "/* Revision.....$Revision$ */\n" p)
	      (display "/* Author.......$Author$ */\n" p)
	      (newline p)
	      (verilog:write-module-declaration verilog:get-module-name
						port-list p)
	      (newline p)
	      (verilog:write-port-directions port-list p)
	      (newline p)))))

;;
;; Footer for file
;;
(define verilog:write-bottom-footer
  (lambda (p)
    (display "endmodule" p)
    (newline p)
    )
)

;;
;; Take a netname and parse it into a structure that describes the net:
;;
;;    (   netname            ; name of the wire
;;      ( N1                 ; first limit
;;        N2                 ; second limit
;;        Increasing_order   ; #t if N2>N1
;;        sure               ; #t if we are sure about the order
;;      ))
(define verilog:net-parse
  (lambda (netname)
    (let 
	((bit-range (regexp-exec bit-range-reg netname))
	 (single-bit (regexp-exec single-bit-reg netname))
	 (simple-id (regexp-exec simple-id-reg netname)))

      ;; check over each expression type, and build the appropriate
      ;; result
      ;(display netname) (display ": ")
      (cond
       ;; is it a bit range?
       (bit-range
	;(display "bit-range" )
	(list (match:substring bit-range 1) 
	      (list (string->number (match:substring bit-range 2))
		    (string->number (match:substring bit-range 3))
		    (> (string->number (match:substring bit-range 3))
		       (string->number (match:substring bit-range 2))) 
		    '#t netname)))

       ;; just a single bit?
       (single-bit 
	;(display "single-bit")
	(list (match:substring single-bit 1) 
	      (list (string->number (match:substring single-bit 2))
		    (string->number (match:substring single-bit 2))
		    '#f '#f netname)))

       ;; or a net without anything
       (simple-id   
	;(display "bare-net")
	(list (match:substring simple-id 1) (list 0 0 #f #f netname)))

       (else       
	(display 
	 (string-append "Warning: `" netname 
			"' is not likely a valid Verilog identifier"))
	(newline)
	(list netname (list 0 0 #f #f netname)))
       ))))

;;
;; Return #t if the passed name is something that might pass as a
;; verilog identifier.
;;
(define verilog:identifier?
  (lambda (netname)
    (let 
	((bit-range (regexp-exec bit-range-reg netname))
	 (single-bit (regexp-exec single-bit-reg netname))
	 (simple-id (regexp-exec simple-id-reg netname)))

      ;; check over each expression type, return
      ;; result
      ;(display netname) (display ": ")
      (cond
       (bit-range  `#t )
       (single-bit `#t )
       (simple-id  `#t )
       (else       `#f )
       ))))

;;
;; Display a verilog identifier that is escaped if needed 
;;
(define verilog:display-escaped-identifier
  (lambda (netname port)
    (if (verilog:identifier? netname)
	(display netname port) ; just display the identifier
	(display (string-append "\\" netname " ") port)))) ; need to escape
    

;;
;; return just the netname part of a verilog identifier
;;
(define verilog:netname
  (lambda (netname)
    (car (verilog:net-parse netname))))
      
;;  Update the given bit range with data passed.  Take care
;;  of ordering issues.
;;  
;;   n1     : new first range
;;   n2     : new second range
;;   old-n1 : first range to be updated
;;   old-n2 : second range to be updated
;;   increasing : original order was increasing
(define verilog:update-range
  (lambda (n1 n2 old-n1 old-n2 increasing)
    (let ((rn1 (if increasing
		   (min n1 old-n1)     ; originally increasing
		   (max n1 old-n1)))   ; originally decreasing

	  (rn2 (if increasing
		   (max n2 old-n2)     ; originally increasing
		   (min n2 old-n2))))
;      (display (string-append "increasing:" 
;			      (if increasing "increasing" "decreasing")
;			      " rn1:" (number->string rn1) 
;			      " rn2:" (number->string rn2)
;			      " n1:" (number->string n1)
;			      " n2:" (number->string n2)
;			      " old-n1:" (number->string old-n1)
;			      " old-n2:" (number->string old-n2))) (newline)
      (list rn1 rn2)
	
      )))


;; return a record that has been updated with the given
;; parameters
(define verilog:update-record
  (lambda (n1
	   n2 
	   list-n1 
	   list-n2
	   increasing
	   sure
	   real)
    (list
     (append (verilog:update-range 
	      n1 n2 list-n1 list-n2
	      increasing)
	     (list increasing
		   sure
		   real)))))

;;
;;  Work over the list of `unique' nets in the design,
;;  extracting names, and bit ranges, if appropriate.
;;  return a list of net description objects
;;

(define the-nets '())

(define verilog:get-nets 
  (begin 
    (for-each 
     (lambda (netname)
       ; parse the netname, and see if it is already on the list
       (let* ((parsed (verilog:net-parse netname))
	      (listed (assoc (car parsed) the-nets)))
	 (if listed
	     (begin ; it is, do some checks, and update the record
	       ;; extract fields from list
	       (let* ((list-name       (car listed))
		      (list-n1         (car (cadr listed)))
		      (list-n2         (cadr (cadr listed)))
		      (list-increasing (caddr (cadr listed)))
		      (list-sure       (cadddr (cadr listed)))
		      (list-real       (cadddr (cdr (cadr listed))))
		      
		      (name            (car parsed))
		      (n1              (car (cadr parsed)))
		      (n2              (cadr (cadr parsed)))
		      (increasing      (caddr (cadr parsed)))
		      (sure            (cadddr (cadr parsed)))
		      (real            (cadddr (cdr (cadr parsed))))

		      (consistant      (or (and list-increasing increasing)
					   (and (not list-increasing) 
						(not increasing))))
		      
		     )

		 (cond
		  ((and list-sure consistant)
		   (begin
		     (set-cdr! listed
			       (verilog:update-record n1 n2
						      list-n1 list-n2
						      increasing
						      #t
						      real)
			       )))
		   ((and list-sure (not sure) (zero? n1) (zero? n2))
		    '() ;; this is a net without any expression, leave it
		    )
		  ((and list-sure (not consistant))
		   (begin      ;; order is inconsistent
		     (display 
		      (string-append "Warning: Net `" real "' has a " 
				     "bit order that conflicts with "
				     "the original definition of `"
				     list-real "', ignoring `"
				     real "'"
				     ))
		     (newline))) 
		   ((and (not list-sure) sure consistant)
		    (begin
		      (set-cdr! listed
				(verilog:update-record n1 n2
						       list-n1 list-n2
						       increasing
						       #t
						       real))))
		    
		   ((and (not list-sure) sure (not consistant))
		    (begin
		      (set-cdr! listed
				(verilog:update-record n1 n2
						       list-n2 list-n1
						       increasing
						       #t
						       real))))
		   ((and (not list-sure) (not sure))
		    (begin
		      (set-cdr! listed
				(verilog:update-record n1 n2
						       list-n1 list-n2
						       increasing
						       #f
						       real))))
		   (else
		    (begin
		      (display "This should never happen!")
		      (newline)))
		   )
	     )
	 )
       (begin ; it is not, just add it to the end
	 (set! the-nets 
	       (append the-nets 
		       (list parsed))))
       ))
     )
     
    all-unique-nets)
    the-nets))

;; Retrieve the requested net record from the database.

(define verilog:get-net
  (lambda (wire)
    (begin
      (assoc wire verilog:get-nets)
      )))

;;
;;  Display wires from the design
;;
;;  Display a net in a legal verilog format, based on the object passed
(define verilog:display-wire
  (lambda (wire p)
    ;; figure out if we need a bit range
    (let ((name            (car wire))
	  (n1              (car (cadr wire)))
	  (n2              (cadr (cadr wire)))
	  )
      
      (if (not (and (zero? n1) (zero? n2)))
	  (begin     ;; yes, print it
	    (display "[ " p)
	    (display n1 p)
	    (display " : " p)
	    (display n2 p)
	    (display " ] " p)
	  )
	)
    ;; print the wire name
      (verilog:display-escaped-identifier name p))))

;;
;;  Loop over the list of nets in the design, writing one by one
;;
(define verilog:write-wires
  (lambda (p)
    (display "/* Wires from the design */" p)
    (newline p)
    (for-each (lambda (wire)          ; print a wire statement for each
		(display "wire " p)   ; net in the design
		(verilog:display-wire wire p)
		(display " ;" p)
		(newline p))
	      verilog:get-nets ) 
    (newline p)))

;;
;;  Output any continuous assignment statements generated
;; by placing `high' and `low' components on the board 
;;
(define verilog:write-continuous-assigns
  (lambda (p)
    (display "/* continuous assignments */" p) (newline p)
    (for-each (lambda (wire)             ; do high values
		(begin
		  (display "assign " p) 	
		  ;; XXX fixme, multiple bit widths!
		  (verilog:display-escaped-identifier wire p) 
		  (display " = 1'b1;" p) 
		  (newline p)))
	      (verilog:get-matching-nets "device" "HIGH"))

    (for-each (lambda (wire)
		(begin
		  (display "assign " p) 
		  ;; XXX fixme, multiple bit widths!
		  (verilog:display-escaped-identifier wire p)
		  (display " = 1'b0;" p)
		  (newline p)))
	      (verilog:get-matching-nets "device" "LOW"))
    (newline p))
)



;;
;; Top level component writing 
;;
;; Output a compoment instatantiation for each of the
;; components on the board
;; 
;; use the format:
;;
;;  device-attribute refdes (
;;        .pinname ( net_name ),
;;        ...
;;    );
;;
(define verilog:components
  (lambda (packages port)
    (begin
      (display "/* Package instantiations */" port)
      (newline port)
      (for-each (lambda (package)         ; loop on packages
		  (begin
		    (let ((device (get-device package)))
 		      (if (not (memv (string->symbol device) ; ignore specials
 				     (map string->symbol
 					  (list "IOPAD" "IPAD" "OPAD"
 						"HIGH" "LOW"))))
 			  (begin
 			    (verilog:display-escaped-identifier 
			     (get-device package) port)
 			    (display " " port)
 			    (verilog:display-escaped-identifier package port)
 			    (display " ( " port)
			    ; if this module wants positional pins, 
			    ; then output that format, otherwise
			    ; output normal named declaration
			    (verilog:display-connections 
			     package 
			     (string=? (gnetlist:get-package-attribute
					package "VERILOG_PORTS" )
				       "POSITIONAL")
			     port)
 			    (display "    );" port)
	 		    (newline port)
 			    (newline port))))))
 		packages)))
)

;;
;; output a module connection for the package given to us with named ports
;;
(define verilog:display-connections
   (lambda (package positional port)
     (begin
       (let ( (pin-list (gnetlist:get-pins-nets package)) 
	      (comma_pending #f) )
 	(if (not (null? pin-list))
 	    (begin
	      (newline port)
 	      (for-each (lambda (pin)
 			  (if (not (string=? "unconnected_pin" (cdr pin)))
			      (begin
				;; handle commas after the first pin
				(if comma_pending 
				    (begin
				      (display "," port)
				      (newline port))
				    (set! comma_pending #t))
				(verilog:display-pin pin positional port))))
 			pin-list)
 	      (newline port))))))
)

;;
;; Display the individual net connections
;;  in this format if positional is true:
;;
;;    /* PINNAME */ NETNAME
;;
;;  otherwise emit:
;; 
;;      .PINNAME ( NETNAME )
;;
(define verilog:display-pin
    (lambda (pin positional port)
      (begin
	(if positional
	    (begin    ; output a positional port instanace
	      (display "  /* " port)
	      (display (car pin) port)  ; add in name for debugging
	      (display " */ " port )
	      (display (cdr pin) port))
	    (begin    ; else output a named port instance 
	      (display "    ." port)
	      ; Display the escaped version of the identifier
	      (verilog:display-escaped-identifier (verilog:netname (car pin))
						  port)
	      (display " ( " port)
	      (verilog:display-escaped-identifier (cdr pin) port)
	      (display " )" port))))))
    
	 

;;; Highest level function
;;; Write Structural verilog representation of the schematic
;;;
(define verilog 
  (lambda (output-filename)
    (let ((port (open-output-file output-filename)))
      (begin
	(verilog:write-top-header port)
	(verilog:write-wires port)
	(verilog:write-continuous-assigns port)
	(verilog:components packages port)
	(verilog:write-bottom-footer port)
	)
      (close-output-port port)
      )
    )
) 
;;
;; Verilog netlist backend written by Mike Jarabek ends here
;;
;; --------------------------------------------------------------------------

