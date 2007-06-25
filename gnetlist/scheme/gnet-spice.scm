;;; gEDA - GPL Electronic Design Automation
;;; gnetlist - gEDA Netlist
;;; Copyright (C) 1998-2007 Ales Hvezda
;;; Copyright (C) 1998-2007 gEDA Contributors (see ChangeLog for details)
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
;;   (lambda (uref pins port)
;;     (if (not (null? pins))
;;            (let ((pin (car pins)))
;;             (begin
;;           (display pin) (newline)
;;           (display (car (gnetlist:get-nets uref pin)) port)
;;          (write-char #\space port)
;;          (spice:write-net-name-of-node uref (cdr pins) port)
;;        )
;;      )
;;    )
;;  )
;;)


;;
;; gnet-spice replacement of gnetlist:get-nets, a net labeled "GND" becomes 0 
;;
(define spice:get-net
  (lambda (uref pin-name )
(let ((net-name (gnetlist:get-nets uref pin-name)))
    (cond ((string=? (car net-name) "GND") (cons "0" #t))
          (else                            (cons (car net-name) #t))))))


;;
;; write netnames connected to pin-a and pin-b
;;   (currently used by the controlled sources (e,g,f and h)
;;
(define spice:write-two-pin-names
  (lambda (package pin-a pin-b port)
    (display (string-append 
      (car (spice:get-net package (gnetlist:get-attribute-by-pinseq package pin-a "pinnumber"))) " ") port)
    (display (string-append 
      (car (spice:get-net package (gnetlist:get-attribute-by-pinseq package pin-b "pinnumber"))) " ") port)))
    


;;
;; write a current controlled voltage source and implement the necessary 
;;   current measuring voltage source
(define spice:write-ccvs
  (lambda (package port)            
    ( begin
      (display "* begin ccvs expansion, h<name>\n" port) 
          ;; implement the controlled current source
          ;; the user should create the uref label begining with a h
      (display (string-append package " ") port)
      (spice:write-two-pin-names package "1" "2" port)
      (display (string-append "Vsense_" package  " " (spice:component-value package) "\n" ) port)
          ;; implement the current measuring voltage source
      (display (string-append "Vsense_" package " ") port)
      (spice:write-two-pin-names package "3" "4" port)
      (display "dc 0\n" port)
          ;; now it is possible to leave the output voltage source unconnected
          ;; i.e. spice won't complain about unconnected nodes
      (display (string-append "Iout_" package " ") port)
      (spice:write-two-pin-names package "1" "2" port)
      (display "dc 0\n" port)
      (display "* end ccvs expansion\n" port))))


;;
;; write a current controlled current source and implement the necessary 
;;   current measuring voltage source
(define spice:write-cccs
  (lambda (package port)            
    ( begin
      (display "* begin cccs expansion, f<name>\n" port) 
          ;; implement the controlled current source
          ;; the user should create the uref label begining with a f
      (display (string-append package " ") port)
      (spice:write-two-pin-names package "1" "2" port)
      (display (string-append "Vsense_" package " " (gnetlist:get-package-attribute package "value") "\n" ) port)
          ;; implement the current measuring voltage source
      (display (string-append "Vsense_" package " ") port)
      (spice:write-two-pin-names package "3" "4" port)
      (display "dc 0\n" port)
      (display "* end cccs expansion\n" port))))



;;
;; write a voltage controlled current source and implement the necessary 
;;   voltage measuring current source
(define spice:write-vccs
  (lambda (package port)            
    ( begin
      (display "* begin vccs expansion, g<name>\n" port) 
          ;; implement the controlled current source
          ;; the user should create a uref label beginning with a g
      (display (string-append package " ") port)
      (spice:write-net-name-of-component package (length (gnetlist:get-pins package)) port)
       (display  (string-append (spice:component-value package) "\n")  port)
          ;; implement the voltage measuring current source
          ;; imagine yourself copying the voltage of a voltage source with an internal
          ;; impedance, spice starts complaining about unconnected nets if this current
          ;; source is not here.
      (display (string-append "Imeasure_" package " ") port)
      (spice:write-two-pin-names package "3" "4" port)
      (display "dc 0\n" port)
      (display "* end vccs expansion\n" port))))


;;
;; write a voltage controlled voltage source and implement the necessary 
;;   voltage measuring current source
(define spice:write-vcvs
  (lambda (package port)            
    ( begin
      (display "* begin vcvs expansion, e<name>\n" port) 
          ;; implement the controlled voltage source
          ;; the user should create a uref label beginning with an e
      (display (string-append package " ") port)
      (spice:write-net-name-of-component package (length (gnetlist:get-pins package)) port)
      (display (string-append (gnetlist:get-package-attribute package "value") "\n" ) port)
          ;; implement the voltage measuring current source
          ;; imagine yourself copying the voltage of a voltage source with an internal
          ;; impedance, spice starts complaining about unconnected nets if this current
          ;; source is not here.
      (display (string-append "Isense_" package " ") port)
      (spice:write-two-pin-names package "3" "4" port)
      (display "dc 0\n" port)
          ;; with an output current source it is possible to leave the output voltage source 
          ;; unconnected i.e. spice won't complain about unconnected nodes
      (display (string-append "Iout_" package " ") port)
      (spice:write-two-pin-names package "1" "2" port)
      (display "dc 0\n" port)
      (display "* end vcvs expansion\n" port))))


;;
;; Create a nullor, make sure it consists of a voltage controlled source
;;
(define spice:write-nullor
  (lambda (package port)            
    ( begin
      (display "* begin nullor expansion, e<name>\n" port) 
          ;; implement the controlled voltage source 
      (display (string-append "E-" package " ") port)
      (spice:write-net-name-of-component package (length (gnetlist:get-pins package)) port)
      (display (string-append (gnetlist:get-package-attribute package "value") "\n" ) port)
          ;; implement the voltage measuring current source
          ;; imagine yourself copying the voltage of a voltage source with an internal
          ;; impedance, spice starts complaining about unconnected nets if this current
          ;; source is not here.
      (display (string-append "Imeasure_" package " ") port)
      (spice:write-two-pin-names package "3" "4" port)
      (display "dc 0\n" port)
          ;; with an output current source it is possible to leave the output voltage source 
          ;; unconnected i.e. spice won't complain about unconnected nodes
      (display (string-append "Iout_" package " ") port)
      (spice:write-two-pin-names package "1" "2" port)
      (display "dc 0\n" port)
      (display "* end of nullor expansion\n" port))))



;;
;; write all listed and available attributes in the form of <variable>=<value>
;;
(define spice:write-list-of-attributes
  (lambda (package attrib-list port)
    (if (not (null? attrib-list))
      (begin      
            ; Is it possible to make no differentiation between upper and lower case?
            ; That relieves you of mixed case forms e.g. As, AS, as..., they are the 
            ; same attributes, spice3f5 is case insensitive.  And other spice versions?
        (if (not (string=? (gnetlist:get-package-attribute package (car attrib-list)) "unknown"))
          (display (string-append  " " (car attrib-list) "=" 
                               (gnetlist:get-package-attribute package (car attrib-list))) port))
        (spice:write-list-of-attributes package (cdr attrib-list) port)))))


;;
;;  write mos transistor
;;
(define spice:write-mos-transistor
  (lambda (package port)
    (spice:write-one-component package port) 
            ;; create list of attributes which can be attached to a mosfet
    (let ((attrib-list (list "l" "w" "as" "ad" "pd" "ps" "nrd" "nrs" "temp" "ic")))
      (spice:write-list-of-attributes package attrib-list port))
            ;; write the off attribute separately
    (let ((off-value (gnetlist:get-package-attribute package "off")))
      (cond ((string=? off-value "#t") (display " off" port))
            ((string=? off-value "1" ) (display " off" port))))
    (newline port)))


;;
;; Given a uref, returns the device associated nets(s) ordered by their pin#, 
;; what when not defined?
;;      problem is slotted components e.g. ../examples/singlenet_1.sch
;;
(define spice:write-net-name-of-component
  (lambda (uref number-of-pin port)
    (if (> number-of-pin 0)
      (begin                                   
            ;; first find pin1 and then start writing the connected net name
        (spice:write-net-name-of-component uref (- number-of-pin 1) port)
            ;; generate a pin-name e.g. pin1, pin2, pin3 ...
        (let ((pin-name (number->string number-of-pin)))  
          (display (car (spice:get-net uref (gnetlist:get-attribute-by-pinseq uref pin-name "pinnumber"))) port)
          (write-char #\space port))))))


;;
;; Given a uref, returns the device attribute value as string
;;
(define spice:component-value
  (lambda (package) 
    (let ((value (gnetlist:get-package-attribute package "value"))) 
      (if (not (string=? value "unknown"))
        value
        "<No valid value attribute found>"))))


;;
;; Include a file
;;
(define spice:write-include
  (lambda (package port)
    (display (string-append package " " (spice:component-value package) "\n") port)))


;;
;; write the uref, the net name connected to pin# and the component value. No extra attributes.
;;
(define spice:write-one-component
  (lambda (package port)
    (display (string-append package " ") port) 
        ;; write net names, slotted components not implemented
    (spice:write-net-name-of-component package (length (gnetlist:get-pins package)) port)
        ;; write component value, if components have a label "value=#"
        ;; what if a component has no value label, currently unknown is written  
    (display (spice:component-value package) port)))


;;
;; write the uref, to the pin# connected net and component value and optional extra attributes
;; check if the component is a special spice component
;;
(define spice:write-netlist
  (lambda (port ls)
     (if (not (null? ls))
      (let ((package (car ls)))                           ;; search for specific device labels
        (cond   
          ( (string=? (get-device package) "SPICE-ccvs")
              (spice:write-ccvs package port))
          ( (string=? (get-device package) "SPICE-cccs") 
              (spice:write-cccs package port))
          ( (string=? (get-device package) "SPICE-vcvs") 
              (spice:write-vcvs package port))
          ( (string=? (get-device package) "SPICE-vccs") 
              (spice:write-vccs package port))
          ( (string=? (get-device package) "SPICE-nullor") 
              (spice:write-nullor package port))
          ( (string=? (get-device package) "PMOS_TRANSISTOR")
              (spice:write-mos-transistor package port))
          ( (string=? (get-device package) "NMOS_TRANSISTOR")
              (spice:write-mos-transistor package port))
          ( (string=? (get-device package) "include")
              (spice:write-include package port))
          ( else (spice:write-one-component package port)
               (newline port)))
        (spice:write-netlist port (cdr ls)) ))))  


;; 
;; Spice netlist header
;;
(define spice:write-top-header
  (lambda (port)
    (display "* Spice netlister for gnetlist\n" port)))


;;
;; Write the .END line
;;
(define spice:write-bottom-footer
  (lambda (port)
    (display ".END" port)
    (newline port)))

;;
;; Spice netlist generation
;;
(define spice
  (lambda (output-filename)
    (let ((port (open-output-file output-filename)))
;; No longer needed
;;      (gnetlist:set-netlist-mode "SPICE")   
      (spice:write-top-header port)
      (spice:write-netlist port packages)
      (spice:write-bottom-footer port)
      (close-output-port port))))


;; SPICE netlist backend written by S. Gieltjes ends here
;;
;; --------------------------------------------------------------------------
