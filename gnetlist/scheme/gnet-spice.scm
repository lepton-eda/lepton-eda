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
(define spice:write-one-component
	(lambda (package port)
		(display package port)
		(write-char #\space port)
			;; write net names, slotted components not implemented
			
			;; Ales' changed implementation
			;;(write-net-name-of-node package (gnetlist:get-pins package) port)
		(spice:write-net-name-of-node package (length (gnetlist:get-pins package)) port)
			;; write component value, if components have a label "value=#"
		(display (spice:component-value package) port)
		(newline port)
	)
)

;;
;; write a current controlled voltage source and implement the necessary 
;;   current measuring voltage source
(define spice:write-ccvs
	(lambda (package port)					  
		( begin
			(display "* begin ccvs implementation ( h<name> ... )" port) 
			(newline port)
;;				implement the controlled current source ... the user should create the uref label begining with a h
			(display (string-append package " ") port)
			(display (car (gnetlist:get-nets package (gnetlist:get-package-attribute package "pin1"))) port)
			(write-char #\space port)
			(display (car (gnetlist:get-nets package (gnetlist:get-package-attribute package "pin2"))) port)
			(display (string-append " v-measure-" package  " " (spice:component-value package) ) port)
			(newline port)
;;				implement the current measuring voltage source
			(display (string-append "v-measure-" package " ") port)
			(display (car (gnetlist:get-nets package (gnetlist:get-package-attribute package "pin3"))) port)
			(write-char #\space port)
			(display (car (gnetlist:get-nets package (gnetlist:get-package-attribute package "pin4"))) port)
			(display " dc 0" port)
			(newline port)
;; 				now it is possible to leave the output voltage source unconnected
;;				i.e. spice won't complain about unconnected nodes
			(display (string-append "i-out-" package " ") port)
			(display (car (gnetlist:get-nets package (gnetlist:get-package-attribute package "pin1"))) port)
			(write-char #\space port)
			(display (car (gnetlist:get-nets package (gnetlist:get-package-attribute package "pin2"))) port)
			(display " dc 0" port)
			(newline port)
			(display "* end ccvs implementation" port)
			(newline port)
		)
	)
)

;;
;; write a current controlled current source and implement the necessary 
;;   current measuring voltage source
(define spice:write-cccs
	(lambda (package port)					  
		( begin
			(display "* begin cccs implementation ( f<name> ... )" port) 
			(newline port)
;;				implement the controlled current source ... the user should create the uref label begining with a f
			(display (string-append package " ") port)
			(display (car (gnetlist:get-nets package (gnetlist:get-package-attribute package "pin1"))) port)
			(write-char #\space port)
			(display (car (gnetlist:get-nets package (gnetlist:get-package-attribute package "pin2"))) port)
			(display (string-append " v-measure-" package " " (spice:component-value package) ) port)
			(newline port)
;;				implement the current measuring voltage source
			(display (string-append "v-measure-" package " ") port)
			(display (car (gnetlist:get-nets package (gnetlist:get-package-attribute package "pin3"))) port)
			(write-char #\space port)
			(display (car (gnetlist:get-nets package (gnetlist:get-package-attribute package "pin4"))) port)
			(display " dc 0" port)
			(newline port)
			(display "* end cccs implementation" port)
			(newline port)
		)
	)
)

;;
;; write a voltage controlled voltage source and implement the necessary 
;;   voltage measuring current source
(define spice:write-vcvs
	(lambda (package port)					  
		( begin
			(display "* begin vcvs implementation ( e<name> ... )" port) 
			(newline port)
;;				implement the controlled voltage source ... the user should create the uref label begining with an e
			(display (string-append package " ") port)
			(display (car (gnetlist:get-nets package (gnetlist:get-package-attribute package "pin1"))) port)
			(write-char #\space port)
			(display (car (gnetlist:get-nets package (gnetlist:get-package-attribute package "pin2"))) port)
			(write-char #\space port)
			(display (car (gnetlist:get-nets package (gnetlist:get-package-attribute package "pin3"))) port)
			(write-char #\space port)
			(display (car (gnetlist:get-nets package (gnetlist:get-package-attribute package "pin4"))) port)
			(display (string-append  " " (spice:component-value package) ) port)
			(newline port)
;;				implement the voltage measuring current source
;; 				imagine yourself copying the voltage of a voltage source with an internal
;; 				impedance, spice starts complaining about unconnected nets if this current
;; 				source is not here.
			(display (string-append "i-measure-" package " ") port)
			(display (car (gnetlist:get-nets package (gnetlist:get-package-attribute package "pin3"))) port)
			(write-char #\space port)
			(display (car (gnetlist:get-nets package (gnetlist:get-package-attribute package "pin4"))) port)
			(display " dc 0" port)
			(newline port)
;; 				now it is possible to leave the output voltage source unconnected
;;				i.e. spice won't complain about unconnected nodes
			(display (string-append "i-out-" package " ") port)
			(display (car (gnetlist:get-nets package (gnetlist:get-package-attribute package "pin1"))) port)
			(write-char #\space port)
			(display (car (gnetlist:get-nets package (gnetlist:get-package-attribute package "pin2"))) port)
			(display " dc 0" port)
			(newline port)
			(display "* end vcvs implementation" port)
			(newline port)
		)
	)
)

;;
;; write a voltage controlled current source and implement the necessary 
;;   voltage measuring current source
(define spice:write-vccs
	(lambda (package port)					  
		( begin
			(display "* begin vccs implementation ( g<name> ... )" port) 
			(newline port)
;;				implement the controlled voltage source ... the user should create the uref label begining with a g
			(display (string-append package " ") port)
			(display (car (gnetlist:get-nets package (gnetlist:get-package-attribute package "pin1"))) port)
			(write-char #\space port)
			(display (car (gnetlist:get-nets package (gnetlist:get-package-attribute package "pin2"))) port)
			(write-char #\space port)
			(display (car (gnetlist:get-nets package (gnetlist:get-package-attribute package "pin3"))) port)
			(write-char #\space port)
			(display (car (gnetlist:get-nets package (gnetlist:get-package-attribute package "pin4"))) port)
			(display (string-append  " " (spice:component-value package) ) port)
			(newline port)
;;				implement the voltage measuring current source
;; 				imagine yourself copying the voltage of a voltage source with an internal
;; 				impedance, spice starts complaining about unconnected nets if this current
;; 				source is not here.
			(display (string-append "i-measure-" package " ") port)
			(display (car (gnetlist:get-nets package (gnetlist:get-package-attribute package "pin3"))) port)
			(write-char #\space port)
			(display (car (gnetlist:get-nets package (gnetlist:get-package-attribute package "pin4"))) port)
			(display " dc 0" port)
			(newline port)
			(display "* end vccs implementation" port)
			(newline port)
		)
	)
)

;;
;; Create a nullor, make sure it consists of a voltage controlled source
;;
(define spice:write-nullor
	(lambda (package port)					  
		( begin
			(display "* begin nullor implementation ( e<name> ... )" port) 
			(newline port)
;;				implement the controlled voltage source ... the user should create the uref label begining with an e
			(display (string-append "E" package " ") port)
			(display (car (gnetlist:get-nets package (gnetlist:get-package-attribute package "pin1"))) port)
			(write-char #\space port)
			(display (car (gnetlist:get-nets package (gnetlist:get-package-attribute package "pin2"))) port)
			(write-char #\space port)
			(display (car (gnetlist:get-nets package (gnetlist:get-package-attribute package "pin3"))) port)
			(write-char #\space port)
			(display (car (gnetlist:get-nets package (gnetlist:get-package-attribute package "pin4"))) port)
			(display (string-append  " " (spice:component-value package) ) port)
			(newline port)
;;				implement the voltage measuring current source
;; 				imagine yourself copying the voltage of a voltage source with an internal
;; 				impedance, spice starts complaining about unconnected nets if this current
;; 				source is not here.
			(display (string-append "i-measure-" package " ") port)
			(display (car (gnetlist:get-nets package (gnetlist:get-package-attribute package "pin3"))) port)
			(write-char #\space port)
			(display (car (gnetlist:get-nets package (gnetlist:get-package-attribute package "pin4"))) port)
			(display " dc 0" port)
			(newline port)
;; 				now it is possible to leave the output voltage source unconnected
;;				i.e. spice won't complain about unconnected nodes
			(display (string-append "i-out-" package " ") port)
			(display (car (gnetlist:get-nets package (gnetlist:get-package-attribute package "pin1"))) port)
			(write-char #\space port)
			(display (car (gnetlist:get-nets package (gnetlist:get-package-attribute package "pin2"))) port)
			(display " dc 0" port)
			(newline port)
			(display "* end nullor implementation" port)
			(newline port)
		)
	)
)

;;
;; write the uref, to the pin# connected net and component value
;; check if the component is a special spice component
;;
(define spice:component-writing
        (lambda (port ls)
                (if (not (null? ls))
                        (let ((package (car ls)))

;;	search for the spice specific device labels and write the necessary extra components
			    (begin
				(cond 	( (string=? (get-device package) "SPICE-ccvs") 
						(spice:write-ccvs package port))
        				( (string=? (get-device package) "SPICE-cccs") 
						(spice:write-cccs package port))
        				( (string=? (get-device package) "SPICE-vcvs") 
						(spice:write-vcvs package port))
        				( (string=? (get-device package) "SPICE-vccs") 
						(spice:write-vccs package port))
        				( (string=? (get-device package) "SPICE-nullor") 
						(spice:write-nullor package port))
;;	the components without the spice label are treated here
      					( else (spice:write-one-component package port))
				)
				(spice:component-writing port (cdr ls))			     
;; 	end of search
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
                (display "* Spice backend written by Bas Gieltjes" port)
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
				(gnetlist:set-netlist-mode "SPICE")
                                (spice:header-warning port)
                                (spice:component-writing port packages)
                        )
                (close-output-port port)
                )
        )
)


;; SPICE netlist backend written by S. Gieltjes ends here
;;
;; --------------------------------------------------------------------------

