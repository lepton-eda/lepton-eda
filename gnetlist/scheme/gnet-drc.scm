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
          (cons read-from-file (drc:parseconfig port))
          '()))))

(define drc:attriblist
  (drc:parseconfig 
    (open-input-file "attribs")))

(define drc
  (lambda (output-filename)
    (let ((port (open-output-file output-filename)))
      (drc:device-rules drc:attriblist packages port)
      (drc:net-rules packages port)
      (drc:pin-rules packages port))))


(define drc:net-rules
  (lambda(packages port)
    #t))

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

