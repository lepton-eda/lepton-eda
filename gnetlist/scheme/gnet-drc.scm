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

(define drc
  (lambda (output-filename)
	(newline)
   ))

;;(define drc
  ;;(lambda (output-filename)
    ;;(let ((port (open-output-file output-filename))
	  ;;(attriblist (drc:parseconfig (open-input-file "attribs"))))
      ;;(drc:printlist (cons 'package attriblist) port)
      ;;(drc:components port packages attriblist))))

(define drc:has-attribute
  (lambda (attribute)
    (lambda (uref)
      (gnetlist:get-package-attribute uref attribute))))

(define drc:attriblist
  (list "uref" "device" "footprint" "value"))

(define drc:device-preds
  (for-each drc:has-attribute drc:attriblist))


(define drc:printlist
  (lambda (ls port)
    (if (null? ls)
	(newline port)
	(begin
	  (display (car ls) port)
	  (write-char #\tab port)
	  (drc:printlist (cdr ls) port)))))

(define drc:parseconfig
  (lambda (port)
    (let ((read-from-file (read port)))
      (if (not (eof-object? read-from-file))
          (cons read-from-file (drc:parseconfig port))
          '()))))

(define drc:components
  (lambda (port ls attriblist)
    (if (not (null? ls))
	(let ((package (car ls)))
          (if (not (string=? "1" (gnetlist:get-package-attribute package "nodrc")))
	    (begin
              (display package port)
	      (write-char #\tab port)
              (drc:printlist (drc:find-attribs package attriblist) port)))
	  (drc:components port (cdr ls) attriblist)))))

(define drc:find-attribs
  (lambda (package attriblist)
    (if (null? attriblist)
	'()
	(cons (gnetlist:get-package-attribute package (car attriblist))
	      (drc:find-attribs package (cdr attriblist))))))

;;
;; DRC backend written by Matt Ettus ends here
;;
;; --------------------------------------------------------------------------

