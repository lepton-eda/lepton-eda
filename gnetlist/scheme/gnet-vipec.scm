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

(define vipec:analysis-templates
   (list
      (cons
         (cons "VIPEC" " ")
         (list
            (list "value" "R=" #t)))
))

(define vipec:component-templates
   (list
      (cons 
         (cons "RESISTOR" "RES")
         (list 
            (list "value" "R=" #t "use value attrib for resistance")))
      (cons
         (cons "INDUCTOR" "IND")
         (list
            (list "value" "L=" #t "use value attrib for inductance")
            (list "Q" "Q=" #f)))
      (cons
         (cons "CAPACITOR" "CAP")
         (list
            (list "value" "C=" #t "use value attrib for capacitance")))
      (cons
         (cons "TLIN" "TLIN")
         (list
            (list "Z" "Z=" #t 50)
            (list "length" "E=" #t "length attrib for length")
            (list "F" "F=" #t "F attrib for frequency")))
      (cons
         (cons "CLIN" "CLIN")
         (list
            (list "ZE" "ZE=" #t)
            (list "ZO" "ZO=" #t)
            (list "E" "E=" #t)
            (list "F" "F=" #t)))
      (cons
         (cons "SPARAMBLOCK" "BLOCK")
         (list
            (list "filename" "" #t "filename attrib for sparams")))            
))

(define vipec:get-template
   (lambda (templates device)
      (if (not (null? templates))
         (if (string=? device (car (car (car templates))))
            (car templates)
            (vipec:get-template (cdr templates) device))
         (begin
            (display "Template not found   ")
            (display device)
            (newline)
            (cons (cons device "error") '())))))

(define vipec:write-net-name-of-node 
   (lambda (uref number-of-pin netnumbers port)
      (if (> number-of-pin 0)
         (begin          
            (vipec:write-net-name-of-node uref (- number-of-pin 1) netnumbers port)
            (let ((pin-name (number->string number-of-pin)))
               (display (get-net-number (car (gnetlist:get-nets uref (gnetlist:get-pin-attribute-seq uref pin-name "pinnumber"))) netnumbers) port)
               (write-char #\space port))))))

(define vipec:write-attribs
   (lambda (package attribs port term)
      (if (not (null? attribs))
         (let ((attrib (car attribs))
               (value (gnetlist:get-package-attribute package (car(car attribs)))))
            (if (not (string=? value "unknown"))
               (begin
                  (display (cadr attrib) port)
                  (display value port)
                  (display term port))
               (if (and (caddr attrib)(not (null? (cdddr attrib))))
                  (begin
                     (display (cadr attrib) port)
                     (display (cadddr attrib) port)
                     (display term port))))
         (vipec:write-attribs package (cdr attribs) port term)))))

(define vipec:write-gen-component
   (lambda (package port netnumbers)
      (let ((template (vipec:get-template vipec:component-templates (get-device package))))
         (display "\t" port)
         (display (cdr (car template)) port)
         (display "\t" port)
         (vipec:write-net-name-of-node package
            (length (gnetlist:get-pins package)) netnumbers port)
         (vipec:write-attribs package (cdr template) port "\t")
         (display (string-append "\t% " package) port)
         (newline port))))

(define vipec:component-writing
   (lambda (port ls netnumbers)
      (if (not (null? ls))
         (let ((package (car ls))
               (device (get-device (car ls))))
            (cond
               ((string=? device "VIPEC") #t)
               ((string=? device "SMITH") #t)
               ((string=? device "GRID") #t)
               (else (vipec:write-gen-component package port netnumbers)))
            (vipec:component-writing port (cdr ls) netnumbers)))))

(define vipec:misc-components
   (lambda (netnumbers port)
;;      (display "\tRES\t0 " port)
;;      (display (get-net-number "GND" netnumbers) port)
;;      (display " R=0.00001\t% Assign ground net\n" port)
      (display "\tDEF2P\t" port)
      (display (get-net-number "PORT1" netnumbers) port)
      (display "  " port)
      (display (get-net-number "PORT2" netnumbers) port)
      (display "\n\tTERM\t50 50\n" port)))

(define vipec:header
   (lambda (port)
      (display "% ViPEC RF Netlister\n" port)  
      (display "% Written by Matthew Ettus\n" port)
      (display "% Based on code by Bas Gieltjes\n" port)))

(define vipec:analysis-block
   (lambda (packages port)
      (if (not (null? packages))
         (begin
            (if (string=? (get-device (car packages)) "VIPEC")
               (let ((template (vipec:get-template vipec:analysis-templates "VIPEC")))
                  (vipec:write-attribs (car packages) (cdr template) port "\n")
                  (newline port)))
            (vipec:analysis-block (cdr packages) port)))))

(define vipec
   (lambda (output-filename)
      (let ((port (open-output-file output-filename))
            (netnumbers (number-nets all-unique-nets 1)))
         (vipec:header port)
         (display "CKT\n" port)
         (vipec:component-writing port packages netnumbers)
         (vipec:misc-components netnumbers port)
         (newline port)
         (vipec:analysis-block packages port)
         (close-output-port port))))

