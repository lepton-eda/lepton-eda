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

(define vipec:templates
   (list
      (cons 
         (cons "RESISTOR" "RES")
         (list 
            (list "value" "R=" #t)))
      (cons
         (cons "INDUCTOR" "IND")
         (list
            (list "value" "L=" #t)
            (list "Q" "Q=" #f)))
      (cons
         (cons "CAPACITOR" "CAP")
         (list
            (list "value" "C=" #t)))
      (cons
         (cons "TLIN" "TLIN")
         (list
            (list "Z" "Z=" #t 50)
            (list "E" "E=" #t)
            (list "F" "F=" #t)))
      (cons
         (cons "CLIN" "CLIN")
         (list
            (list "ZE" "ZE=" #t)
            (list "ZO" "ZO=" #t)
            (list "E" "E=" #t)
            (list "F" "F=" #t)))
            
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
            (let ((pin-name (string-append "pin" (number->string number-of-pin))))
               (display (get-net-number (car (gnetlist:get-nets uref (gnetlist:get-package-attribute uref pin-name))) netnumbers) port)
               (write-char #\space port))))))

(define vipec:write-attribs
   (lambda (package attribs port)
      (if (not (null? attribs))
         (let ((attrib (car attribs))
               (value (gnetlist:get-package-attribute package (car(car attribs)))))
            (if (not (string=? value "unknown"))
               (begin
                  (display (car (cdr attrib)) port)
                  (display value port)
                  (display "\t" port))
               (if (and (caddr attrib)(not (null? (cdddr attrib))))
                  (begin
                     (display (cadr attrib) port)
                     (display (cadddr attrib) port)
                     (display "\t" port))))))))

(define vipec:write-gen-component
   (lambda (package port netnumbers)
      (let ((template (vipec:get-template vipec:templates (get-device package))))
         (display "\t" port)
         (display (cdr (car template)) port)
         (display "\t" port)
         (vipec:write-net-name-of-node package (length (gnetlist:get-pins package)) netnumbers port)
         (vipec:write-attribs package (cdr template) port)
         (display package port)
         (newline port))))

(define vipec:write-component
   (lambda (package port netnumbers)
      (display package port)
      (write-char #\space port)
      (vipec:write-net-name-of-node package (length (gnetlist:get-pins package)) netnumbers port)
      (display (get-value package) port)
      (display "\t" port)
      (display package port)
      (newline port)))

(define vipec:write-resistor
   (lambda (package port netnumbers)
      (display "   RES\t" port)
      (vipec:write-net-name-of-node package (length (gnetlist:get-pins package)) netnumbers port)
      (display "\t" port)
      (display "R=" port)
      (display (get-value package) port)
      (display "\t" port)
      (display package port)
      (newline port)))

(define vipec:write-capacitor
   (lambda (package port netnumbers)
      (display "   CAP\t" port)
      (vipec:write-net-name-of-node package (length (gnetlist:get-pins package)) netnumbers port)
      (display "\t" port)
      (display "C=" port)
      (display (get-value package) port)
      (display "\t" port)
      (display package port)
      (newline port)))

(define vipec:write-inductor
   (lambda (package port netnumbers)
      (display "   IND\t" port)
      (vipec:write-net-name-of-node package (length (gnetlist:get-pins package)) netnumbers port)
      (display "\t" port)
      (display "L=" port)
      (display (get-value package) port)
      (display "\t" port)
      (let ((Q (gnetlist:get-package-attribute package "Q")))
         (if (not (string=? Q "unknown"))
            (begin
               (display "Q=" port)
               (display Q port)
               (display "\t" port))))
      (display package port)
      (newline port)))

(define vipec:write-tlin
   (lambda (package port netnumbers)
      (display "   TLIN\t" port)
      (vipec:write-net-name-of-node package (length (gnetlist:get-pins package)) netnumbers port)
      (display "\t" port)
      (let ((Z (gnetlist:get-package-attribute package "Z"))
            (E (gnetlist:get-package-attribute package "len"))
            (F (gnetlist:get-package-attribute package "freq")))
         (if (not (string=? Z "unknown"))
            (begin
               (display "Z=" port)
               (display Z port)
               (display "\t" port)))
         (if (not (string=? E "unknown"))
            (begin
               (display "E=" port)
               (display E port)
               (display "\t" port)))
         (if (not (string=? F "unknown"))
            (begin
               (display "F=" port)
               (display F port)
               (display "\t" port))))
      (display package port)
      (newline port)))

(define vipec:component-writing
   (lambda (port ls netnumbers)
      (if (not (null? ls))
         (let ((package (car ls)))
            (cond 	
               ((string=? (get-device package) "SPARAMBLOCK") 
                  (vipec:write-block package port netnumbers))
               ( else (vipec:write-gen-component package port netnumbers)))
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
         (display "CKT\n" port)
         (vipec:component-writing port packages netnumbers)
         (close-output-port port))))

