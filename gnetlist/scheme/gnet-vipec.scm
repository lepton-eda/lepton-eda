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


;; Given a uref, returns the device associated nets(s) ordered by 
;; their pin#, what when not defined?
;; problem is slotted components e.g. ../examples/singlenet_1.sch
;;

;;
;; Given a uref, returns the device associated nets(s) ordered by their pin#, 
;; what when not defined?
;;      problem is slotted components e.g. ../examples/singlenet_1.sch
(define vipec:write-net-name-of-node 
   (lambda (uref number-of-pin port)
      (if (> number-of-pin 0)
         (begin          ;; generate a pin-name e.g. pin1, pin2, pin3 ...
            (vipec:write-net-name-of-node uref (- number-of-pin 1) port)
            (let ((pin-name (string-append "pin" (number->string number-of-pin))))
                                        ;;(newline) (display (gnetlist:get-nets uref (gnetlist:get-package-attribute uref pin-name)))
					;;(newline) (display uref) (write-char #\space) (display pin-name) (newline)
               (display (car (gnetlist:get-nets uref (gnetlist:get-package-attribute uref pin-name))) port)
               (write-char #\space port))))))


;;
;; Given a uref, returns the device attribute value (unknown if not defined)
;;
(define vipec:component-value
   (lambda (package)
      (gnetlist:get-package-attribute package "value")))

;;
;; write the uref, to the pin# connected net and component value
;;
(define vipec:write-one-component
   (lambda (package port)
      (display package port)
      (write-char #\space port)
			;; Ales' changed implementation
			;;(write-net-name-of-node package (gnetlist:get-pins package) port)
      (vipec:write-net-name-of-node package (length (gnetlist:get-pins package)) port)
			;; write component value, if components have a label "value=#"
      (display (vipec:component-value package) port)
      (newline port)))

;;
;; write a current controlled voltage source and implement the necessary 
;;   current measuring voltage source
(define vipec:write-ccvs
	(lambda (package port)					  
		( begin
			(display "* begin ccvs implementation ( h<name> ... )" port) 
			(newline port)
;;				implement the controlled current source ... the user should create the uref label begining with a h
			(display (string-append package " ") port)
			(display (car (gnetlist:get-nets package (gnetlist:get-package-attribute package "pin1"))) port)
			(write-char #\space port)
			(display (car (gnetlist:get-nets package (gnetlist:get-package-attribute package "pin2"))) port)
			(display (string-append " v-measure-" package  " " (vipec:component-value package) ) port)
			(newline port)
;;				implement the current measuring voltage source
			(display (string-append "v-measure-" package " ") port)
			(display (car (gnetlist:get-nets package (gnetlist:get-package-attribute package "pin3"))) port)
			(write-char #\space port)
			(display (car (gnetlist:get-nets package (gnetlist:get-package-attribute package "pin4"))) port)
			(display " dc 0" port)
			(newline port)
;; 				now it is possible to leave the output voltage source unconnected
;;				i.e. vipec won't complain about unconnected nodes
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
;; Create a nullor, make sure it consists of a voltage controlled source
;;
(define vipec:write-nullor
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
			(display (string-append  " " (vipec:component-value package) ) port)
			(newline port)
;;				implement the voltage measuring current source
;; 				imagine yourself copying the voltage of a voltage source with an internal
;; 				impedance, vipec starts complaining about unconnected nets if this current
;; 				source is not here.
			(display (string-append "i-measure-" package " ") port)
			(display (car (gnetlist:get-nets package (gnetlist:get-package-attribute package "pin3"))) port)
			(write-char #\space port)
			(display (car (gnetlist:get-nets package (gnetlist:get-package-attribute package "pin4"))) port)
			(display " dc 0" port)
			(newline port)
;; 				now it is possible to leave the output voltage source unconnected
;;				i.e. vipec won't complain about unconnected nodes
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
;; check if the component is a special vipec component
;;
(define vipec:component-writing
   (lambda (port ls)
      (if (not (null? ls))
         (let ((package (car ls)))
            (cond 	
               ((string=? (get-device package) "RESISTOR") 
                  (vipec:write-component-2pins package port "R"))
               ((string=? (get-device package) "CAPACITOR") 
                  (vipec:write-component-2pins package port "C"))
               ((string=? (get-device package) "INDUCTOR") 
                  (vipec:write-inductor package port))
               ((string=? (get-device package) "TLIN") 
                  (vipec:write-tlin package port))
               ((string=? (get-device package) "TLIN4") 
                  (vipec:write-tlin4 package port))
               ((string=? (get-device package) "CLIN") 
                  (vipec:write-clin package port))
               ((string=? (get-device package) "SPARAMBLOCK") 
                  (vipec:write-block package port))
               ( else (vipec:write-one-component package port)))
            (vipec:component-writing port (cdr ls))))))

(define vipec:header
   (lambda (port)
      (display "* ViPEC RF Netlister\n" port)  
      (display "* Written by Matthew Ettus\n" port)
      (display "* Based on code by Bas Gieltjes\n" port)))

(define vipec
   (lambda (output-filename)
      (let ((port (open-output-file output-filename)))
         (vipec:header port)
         (vipec:component-writing port packages)
         (close-output-port port))))

