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

;;
;; I need to eliminate the C'ish style of Scheme that's in this file.
;;


;; Support functions

;; get all packages for a particular schematic page 
;; eventually placeholder will be either the hierarchical level or something 
;; of the sort
(define packages 
  (gnetlist:get-packages "placeholder")
)

;; return a list of all the nets in the design
(define all-nets
  (gnetlist:get-all-nets "placeholder"))

;;
;; Given a uref, returns the device attribute value (unknown if not defined)
;;
(define get-device
	(lambda (package)
		(gnetlist:get-package-attribute package "device")
	)
)

;; return all pins for a particular package 
(define pins
	(lambda (package)
		(gnetlist:get-pins package)
	)
)

;; not very useful, but amusing 
(define all-pins
	(map gnetlist:get-pins packages)
)

;; this is really crude, but I'm tired... :)
(define display-nl
	(lambda (list)
		(display list) 
		(newline)
	)
)


;; ah.. wonder what use this is...
(define display-pin
	(lambda (pin-list)
		(for-each display-nl pin-list)
	)
)


;; ha. I'm playing with scheme here.. don't mind me
(define display-all-pins
	( lambda ()
		(for-each display-pin all-pins)
	)
)


;; another misc function
(define print-packages
	( lambda (plist)
		(for-each display-nl plist)
	)
)

;; --------------------------------------------------------------------------
;;
;; gEDA's native test netlist format specific functions go here 
;;

;;
;; Top level header
;;
(define geda:write-top-header
	(lambda (p)
		(display "START header" p) 
		(newline p)
		(newline p)
		(display "gEDA's netlist format" p)
		(newline p)
		(display "Created specifically for testing of gnetlist" p)
		(newline p)
		(newline p)
		(display "END header" p)
		(newline p)
		(newline p)
	)
)

;;
;; header for components section
;;
(define geda:start-components
	(lambda (p)
		(display "START components" p)
		(newline p)
		(newline p)
	)
)

;;
;; footer for components section
;;
(define geda:end-components
	(lambda (p)
		(newline p)
		(display "END components" p)
		(newline p)
		(newline p)
	)
)

;;
;; header for nets section
;;
(define geda:start-nets
	(lambda (p)
		(display "START nets" p)
		(newline p)
		(newline p)
	)
)

;;
;; footer for net section
;;
(define geda:end-nets
	(lambda (p)
		(newline p)
		(display "END nets" p)
		(newline p)
		(newline p)
	)
)
	
;;
;; Top level component writing 
;;
(define geda:components
	(lambda (port ls)
		(if (not (null? ls))
			(let ((package (car ls)))
				(begin
				   (display package port)
				   (write-char #\space port)
				   (display "device=" port)
				   (display (get-device package) port)
				   (newline port)
				   (geda:components port (cdr ls))
				)
			)
		)
	)
)

;;
;; Display the individual net connections
;;
(define geda:display-connections
   (lambda (nets port)
      (if (not (null? nets))
	 (begin
	   (display (car (car nets)) port)
	   (write-char #\space port) 
	   (display (car (cdr (car nets))) port)
;	   (newline port)
	   (if (not (null? (cdr nets)))
		(begin
	   	  (write-char #\, port) 
	          (write-char #\space port) 
		)
	   )
	   (geda:display-connections (cdr nets) port)
	 )
      )
   )
)

;;
;; Properly format the name of the net and the actual net connections
;;
(define geda:display-name-nets
   (lambda (port net)
      (let ((netname (car net)) (nets (cdr net)))
         (if (and 
		(not (string=? "duplicate" netname))
		(not (string=? "unconnected pin" netname)))
	    (begin
	       (display netname port)
	       (display " : " port)
; (newline port)
	       (geda:display-connections nets port)
	       (write-char #\space port) 
	       (newline port)
	    )
         )
      )
   )
)

;;
;; Write out a net associated with a particular package and pin
;;
(define geda:write-net
   (lambda (port package pins)
      (if (not (null? pins))
         (let ((pin (car pins)))
	    (begin
	       (geda:display-name-nets port (gnetlist:get-nets package pin))
	       (geda:write-net port package (cdr pins))
	    )
         )
      )
   )
) 


;;
;; Top level function to write out nets associated with a particular component
;;
(define geda:nets
   (lambda (port ls)
      (if (not (null? ls))
         (let ((package (car ls)))
	    (begin
	       (let ((package-pins (gnetlist:get-pins package)))

;; temporary 
;;		  (display package port) 
;;		  (write-char #\space port) 
;;		  (display package-pins port)
;;		  (newline port)

                  (geda:write-net port package package-pins)
               )
	       (geda:nets port (cdr ls))
	    )
         )
      )
   )
)


;;; Highest level function
;;; Write my special testing netlist format
;;;
(define geda 
	(lambda (output-filename)
		(let ((port (open-output-file output-filename)))
			(begin
				(display "WARNING: gnetlist still has some serious bugs -- bogus netlists are possible!") (newline)
				(gnetlist:set-netlist-mode "gEDA")
				(geda:write-top-header port)
				(geda:start-components port)
				(geda:components port packages)
				(geda:end-components port)
				(geda:start-nets port)
				(geda:nets port packages)
				(geda:end-nets port)
			)
		(close-output-port port)
		)
	)
)

;;
;; gEDA's native test netlist format specific functions ends 
;;
;; --------------------------------------------------------------------------





;; --------------------------------------------------------------------------
;;
;; SPICE netlist backend written by S. Gieltjes starts here
;;

;;
;; Given a uref, returns the device associated nets(s) ordered by 
;; their pin#, what when not defined?
;; problem is slotted components e.g. ../examples/singlenet_1.sch
;;

;; Ales' changed implemenation 
;; Commented out since it has some problems
;; (define spice:write-net-name-of-node 
;; 	(lambda (uref pins port)
;; 		(if (not (null? pins))
;;        		(let ((pin (car pins)))
;; 	    			(begin
;; 					(display pin) (newline)
;; 					(display (car (gnetlist:get-nets uref pin)) port)
;;					(write-char #\space port)
;;					(spice:write-net-name-of-node uref (cdr pins) port)
;;				)
;;			)
;;		)
;;	)
;;)

;; S. Gieltjes' original implementation
;;
;; Given a uref, returns the device associated nets(s) ordered by their pin#, 
;; what when not defined?
;;      problem is slotted components e.g. ../examples/singlenet_1.sch
(define spice:write-net-name-of-node 
        (lambda (uref number-of-pin port)
                (if (> number-of-pin 0)
                        (begin          ;; generate a pin-name e.g. pin1, pin2, pin3 ...
                                (spice:write-net-name-of-node uref (- number-of-pin 1) port)
                                (let    ((pin-name (string-append "pin" (number->string number-of-pin))))


					;; debug code
                                        ;;(newline) (display (gnetlist:get-nets uref (gnetlist:get-package-attribute uref pin-name)))
					;;(newline) (display uref) (write-char #\space) (display pin-name) (newline)



                                        (display (car (gnetlist:get-nets uref (gnetlist:get-package-attribute uref pin-name))) port)

                                        (write-char #\space port)
                                )
                        )
                )
        )
)


;;
;; Given a uref, returns the device attribute value (unknown if not defined)
;;
(define spice:component-value
        (lambda (package)
                (gnetlist:get-package-attribute package "value")
        )
)

;;
;; write the uref, to the pin# connected net and component value
;;
(define spice:component-writing
        (lambda (port ls)
                (if (not (null? ls))
                        (let ((package (car ls)))
                                (begin
                                        (display package port)  
                                        (write-char #\space port)
                                                ;; write net names, slotted components not implemented

					;; Ales' changed implementation
                                        ;;(write-net-name-of-node package (gnetlist:get-pins package) port)
			
					(spice:write-net-name-of-node package (length (gnetlist:get-pins package)) port)

                                                ;; write component value, if components have a label "value=#"
                                        (display (spice:component-value package) port)
                                        (newline port)
                                        (spice:component-writing port (cdr ls))
                                )
                        )
                )
        )
)

;; 
;; Spice netlist generation warning
;;
(define spice:header-warning
        (lambda (port)
                (display "* Spice netlist for gnetlist" port)  
		(newline port)
                (display "* Spice backend written by S. Gieltjes" port)
		(newline port)
        )
)

;; Spice netlist generatie
;; output : uref, net-name for each pin, value
;;
(define spice
        (lambda (output-filename)
                (let ((port (open-output-file output-filename)))
                        (begin
				(display "WARNING: gnetlist still has some serious bugs -- bogus netlists are possible!") (newline)
				(gnetlist:set-netlist-mode "SPICE")
                                (spice:header-warning port)
                                (spice:component-writing port packages)
                        )
                (close-output-port port)
                )
        )
)

;;
;; SPICE netlist backend written by S. Gieltjes ends here
;;
;; --------------------------------------------------------------------------


;; --------------------------------------------------------------------------
;;
;; TANGO netlist backend written by Nuno Sucena starts here
;;

;;
;; Given a uref, returns the device attribute value (for tango-netlist)
;;
(define tango:get-device
	(lambda (package)
		(gnetlist:get-package-attribute package "device")
	)
)

;;
;; Given a uref, returns the pattern attribute value (PATTERN if not defined)
;;
(define tango:get-pattern
	(lambda (package)
   	     (define pattern (gnetlist:get-package-attribute package "pattern"))
	     (if (string=? "unknown" pattern)
		    "PATTERN"
		    pattern)
; how do i return "PATTERN" if not defined? humm... need to read some
; guile stuff... i did, and see the result :)
	)
)

;;
;; Given a uref, returns the value attribute (empty if not defined)
;;
(define tango:get-value
        (lambda (package)
	        (define value (gnetlist:get-package-attribute package "value"))
		(if (string=? "unknown" value)
		    ""
		    value)
	)
)
 
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
		(newline p)
	)
)

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
					(tango:components port (cdr ls))
				)
			)
		)
	)
)

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
;	   (newline port)
	   (if (not (null? (cdr nets)))
		(begin
		  (newline port)
		)
	   )
	   (tango:display-connections (cdr nets) port)
	 )
      )
   )
)


;;
;; Properly format the name of the net and the actual net connections
;;
(define tango:display-name-nets
   (lambda (port net)
      (let ((netname (car net)) (nets (cdr net)))
         (if (and 
		(not (string=? "duplicate" netname))
		(not (string=? "unconnected pin" netname)))
	    (begin
	       (display "(" port)
	       (newline port)
	       (display netname port)
	       (newline port)
	       (tango:display-connections nets port)
	       (newline port)
	       (display ")" port)
	       (newline port)
	    )
         )
      )
   )
)

;;
;; Write out a net associated with a particular package and pin
;;
(define tango:write-net
   (lambda (port package pins)
      (if (not (null? pins))
         (let ((pin (car pins)))
	    (begin
	       (tango:display-name-nets port (gnetlist:get-nets package pin))
	       (tango:write-net port package (cdr pins))
	    )
         )
      )
   )
) 


;;
;; Top level function to write out nets associated with a particular component
;;
(define tango:nets
   (lambda (port ls)
      (if (not (null? ls))
         (let ((package (car ls)))
	    (begin
	       (let ((package-pins (gnetlist:get-pins package)))

;; temporary 
;;		  (display package port) 
;;		  (write-char #\space port) 
;;		  (display package-pins port)
;;		  (newline port)

                  (tango:write-net port package package-pins)
               )
	       (tango:nets port (cdr ls))
	    )
         )
      )
   )
)


;;; Highest level function
;;; Write tango netlist format
;;;
(define tango
	(lambda (output-filename)
		(let ((port (open-output-file output-filename)))
			(begin
				(display "WARNING: gnetlist still has some serious bugs -- bogus netlists are possible!") (newline)
				(gnetlist:set-netlist-mode "TANGO")
;				(tango:write-top-header port)
;				(geda:start-components port)
				(tango:components port packages)
;				(geda:end-components port)
;				(geda:start-nets port)
				(tango:nets port packages)
;				(geda:end-nets port)
			)
		(close-output-port port)
		)
	)
)

;;
;; TANGO netlist backend written by Nuno Sucena ends here
;;
;; --------------------------------------------------------------------------

;;
;; Verilog netlist backend written by Mike Jarabek starts here
;;
;; --------------------------------------------------------------------------

;; return a list of nets whose pins have the desired attribute name/value
;; pair
(define verilog:get-matching-nets
  (lambda (attribute value)
    (map (lambda (package)      ; loop over packages
	   (let ((attr (gnetlist:get-package-attribute package attribute)))
	     (if (string=? attr value)
		 (map (lambda (pin)   ; loop over pins on the packages
			  (car (gnetlist:get-nets package pin)))
		      (pins package))
		() )))          ; return empty list, if key does not match
	     packages)))


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
      (display module-name p)
      (display " (" p)
      (newline p)
      (for-each (lambda (group)               ; loop over groups
		  (begin
		    (for-each (lambda (pin)   ; loop over pins
				(if (not (null? pin))
				    (begin
				      (display "       " p)
				      (display (car pin) p)
				      (display " ," p)
				      (newline p))))
			      group)))
		port-list)
      (display "      );" p)
      (newline p))))
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
		    (if (not (null? pin))
			(begin
			  (display "input " p)
			  (display (car pin) p)
			  (display " ;" p)
			  (newline p)))) in)       ; do each input

	(for-each (lambda (pin)
		    (if (not (null? pin))
			(begin
			  (display "output " p)
			  (display (car pin) p)
			  (display " ;" p)
			  (newline p)))) out)      ; do each output

	(for-each (lambda (pin)
		    (if (not (null? pin))
			(begin
			  (display "inout " p)
			  (display (car pin) p)
			  (display " ;" p)
			  (newline p)))) inout)    ; do each inout
		      
	(newline p)))))
;;
;; Top level header
;;

(define verilog:write-top-header
	(lambda (p)
	  (let ((port-list (verilog:get-port-list)))
	    (begin
	      (display "/* structural Verilog generated by gnetlist */" p)
	      (newline p)
	      (verilog:write-module-declaration "top" port-list p)
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
;;  Display wires from the design
;;
(define verilog:write-wires
  (lambda (p)
    (display "/* Wires begin here */" p)
    (newline p)
    (newline p)
    (for-each (lambda (wire)          ; print a wire statement for each
		(display "wire " p)   ; net in the design
		(display wire p)
		(display " ;" p)
		(newline p)) all-nets)
    (newline p)))

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
    (for-each (lambda (package)         ; loop on packages
		(begin
		  (let ((device (get-device package)))
		    (if (not (string=? device "IOPAD"))       ; ignore pads
			(if (not (string=? device "IPAD"))
			    (if (not (string=? device "OPAD"))
				(begin
				  (display (get-device package) port)
				  (display " " port)
				  (display package port)
				  (display " (" port)
				  (newline port)
				  (verilog:display-connections 
				   (gnetlist:get-pins package) package port)
					;(display "/* put in net connections here */" port)
				  (display " );" port)
				  (newline port)
				  (newline port))))))))
			packages)))



;; output a module connection for all of the pins given to us
;;
(define verilog:display-connections
  (lambda (pins package port)
    (for-each (lambda (pin)                  ; loop over pins
		(verilog:format-connection 
		 pin (car (gnetlist:get-nets package pin)) port))
	      pins )))

;;
;; Display the individual net connections
;;  in this format:
;;
;;      .PINNAME ( NETNAME ),
;;
(define verilog:format-connection
   (lambda (pinname netname port)
     (if (not (string=? "unconnected_pin" netname))
	 (begin
	   (display "     ." port)
	   (display pinname port)
	   (display " ( " port)
	   (display netname port)
	   (display " )," port)
	   (newline port)))))



;;; Highest level function
;;; Write Structural verilog representation of the schematic
;;;
(define verilog 
  (lambda (output-filename)
    (let ((port (open-output-file output-filename)))
      (begin
	(gnetlist:set-netlist-mode "SPICE")  ;; don't want 'duplicate' nets
	(verilog:write-top-header port)
	(verilog:write-wires port)
	(verilog:components packages port)
	;				(verilog:end-components port)
	;				(verilog:start-nets port)
	;				(verilog:nets port packages)
	;				(verilog:end-nets port)
	(verilog:write-bottom-footer port)
	)
      (close-output-port port)
      )
    )
  )


