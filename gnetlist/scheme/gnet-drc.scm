;;; gEDA - GPL Electronic Design Automation
;;; gnetlist - gEDA Netlist
;;; Copyright (C) 1998-2008 Ales Hvezda
;;; Copyright (C) 1998-2008 gEDA Contributors (see ChangeLog for details)
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
;; DRC backend written by Matt Ettus starts here
;;

;; DRC rules format:  (list (part rules) (net rules) (pin rules))
;; Part rules:  List of predicates of one variable, uref
;; Net rules:  List of predicates of one variable, net name
;; Pin Rules:  List of predicates of 2 variables, uref and pin number

(define drc:parseconfig
  (lambda (port)
    (let ((read-from-file (read port)))
      (if (not (eof-object? read-from-file))
          (cons (symbol->string read-from-file) (drc:parseconfig port))
          '()))))

(define drc:attriblist
  (drc:parseconfig 
    (open-input-file "attribs")))

(define drc
  (lambda (output-filename)
    (let ((port (open-output-file output-filename)))
      (drc:device-rules drc:attriblist packages port)
      (drc:net-rules (gnetlist:get-all-unique-nets "dummy") port)
      (drc:pin-rules packages port)
      (close-output-port port))))


(define drc:net-rules
  (lambda(nets port)
    (cond 
      ((null? nets) #t)
      ((null? (gnetlist:get-all-connections (car nets)))
          (begin
            (display "Net " port)
            (display (car nets) port)
            (display " has no connected pins\n" port)
            (drc:net-rules (cdr nets) port)
            #f))
      ((null? (cdr (gnetlist:get-all-connections (car nets))))
          (begin
            (display "Net " port)
            (display (car nets) port)
            (display " has only 1 connected pin\n" port)
            (drc:net-rules (cdr nets) port)
            #f))
      (#t (drc:net-rules (cdr nets) port)))))

(define drc:pin-rules
  (lambda(packages port)
    #t))

(define drc:device-rules
  (lambda (attriblist packages port)
    (if (not (null? packages))
      (begin
        (drc:has-attributes? attriblist (car packages) port)
        (drc:device-rules attriblist (cdr packages) port)))))

(define drc:has-attributes?
  (lambda (attriblist uref port)
    (if (not (null? attriblist)) 
      (begin
        (if (string=? "unknown" (gnetlist:get-package-attribute uref (car attriblist)))
          (begin
            (display uref port)
            (display " Does not have attribute: " port)
            (display (car attriblist) port)
            (newline port)))
        (drc:has-attributes? (cdr attriblist) uref port)))))


;;
;; DRC backend written by Matt Ettus ends here
;;
;; --------------------------------------------------------------------------

