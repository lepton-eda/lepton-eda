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


;; Given a uref, returns the device associated nets(s) ordered by their pin#, 
;; what when not defined?
;;      problem is slotted components e.g. ../examples/singlenet_1.sch

(define vipec:write-net-name-of-node 
   (lambda (uref number-of-pin netnumbers port)
      (if (> number-of-pin 0)
         (begin          ;; generate a pin-name e.g. pin1, pin2, pin3 ...
            (vipec:write-net-name-of-node uref (- number-of-pin 1) netnumbers port)
            (let ((pin-name (string-append "pin" (number->string number-of-pin))))
               (display (get-net-number (car (gnetlist:get-nets uref (gnetlist:get-package-attribute uref pin-name))) netnumbers) port)
               (write-char #\space port))))))


;; write the uref, to the pin# connected net and component value

(define vipec:write-one-component
   (lambda (package port netnumbers)
      (display package port)
      (write-char #\space port)
      (vipec:write-net-name-of-node package (length (gnetlist:get-pins package)) netnumbers port)
      (display (get-value package) port)
      (newline port)))

;;(write-net-name-of-node package (gnetlist:get-pins package) port)


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
			(display (string-append " v-measure-" package  " " (get-value package) ) port)
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
			(newline port))))

(define vipec:write-resistor
   (lambda (package port netnumbers)
      (begin
         (display "RES\t" port)
         (vipec:write-net-name-of-node package (length (gnetlist:get-pins package)) netnumbers port)
         (display "\t" port)
         (display "R=" port)
         (display (get-value package) port)
         (newline port))))

(define vipec:write-capacitor
   (lambda (package port netnumbers)
      (begin
         (display "CAP\t" port)
         (vipec:write-net-name-of-node package (length (gnetlist:get-pins package)) netnumbers port)
         (display "\t" port)
         (display "C=" port)
         (display (get-value package) port)
         (newline port))))

(define vipec:write-inductor
   (lambda (package port netnumbers)
      (begin
         (display "IND\t" port)
         (vipec:write-net-name-of-node package (length (gnetlist:get-pins package)) netnumbers port)
         (display "\t" port)
         (display "L=" port)
         (display (get-value package) port)
         (display "  Q=" port)
         (display (gnetlist:get-package-attribute package "Q") port)
         (newline port))))

(define vipec:component-writing
   (lambda (port ls netnumbers)
      (if (not (null? ls))
         (let ((package (car ls)))
            (cond 	
               ((string=? (get-device package) "RESISTOR") 
                  (vipec:write-resistor package port netnumbers))
               ((string=? (get-device package) "CAPACITOR") 
                  (vipec:write-capacitor package port netnumbers))
               ((string=? (get-device package) "INDUCTOR") 
                  (vipec:write-inductor package port netnumbers))
               ((string=? (get-device package) "TLIN") 
                  (vipec:write-tlin package port netnumbers))
               ((string=? (get-device package) "TLIN4") 
                  (vipec:write-tlin4 package port netnumbers))
               ((string=? (get-device package) "CLIN") 
                  (vipec:write-clin package port netnumbers))
               ((string=? (get-device package) "SPARAMBLOCK") 
                  (vipec:write-block package port netnumbers))
               ( else (vipec:write-one-component package port netnumbers)))
            (vipec:component-writing port (cdr ls) netnumbers)))))

(define vipec:header
   (lambda (port)
      (display "* ViPEC RF Netlister\n" port)  
      (display "* Written by Matthew Ettus\n" port)
      (display "* Based on code by Bas Gieltjes\n" port)))

(define vipec
   (lambda (output-filename)
      (let ((port (open-output-file output-filename))
            (netnumbers (number-nets all-unique-nets 1)))
         (vipec:header port)
         (vipec:component-writing port packages netnumbers)
         (close-output-port port))))

