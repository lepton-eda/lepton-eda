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


;; --------------------------------------------------------------------------
;;
;; Bill of Material backend written by Matt Ettus starts here
;;

;;; Bill Of Materials Generator
;;; You must have a file called attribs in the pwd
;;; The file should be a text list of attributes you want listed,
;;; One per line.  No comments are allowed in the file.
;;; Questions? Contact matt@ettus.com
;;; This software is released under the terms of the GNU GPL

(define bom
  (lambda (output-filename)
    (let ((port (open-output-file output-filename))
	  (attriblist (bom:parseconfig (open-input-file "attribs"))))
      (bom:printlist (cons 'package attriblist) port)
      (bom:components port packages attriblist))))

(define bom:printlist
  (lambda (ls port)
    (if (null? ls)
	(newline port)
	(begin
	  (display (car ls) port)
	  (write-char #\tab port)
	  (bom:printlist (cdr ls) port)))))

;;  Change courtesy of Stefan.  Gets red of strip1
(define bom:parseconfig
  (lambda (port)
    (let ((read-from-file (read port)))
      (if (not (eof-object? read-from-file))
          (cons read-from-file (bom:parseconfig port))
          '()))))

(define bom:components
  (lambda (port ls attriblist)
    (if (not (null? ls))
	(let ((package (car ls)))
          (if (not (string=? "1" (gnetlist:get-package-attribute package "nobom")))
	    (begin
              (display package port)
	      (write-char #\tab port)
              (bom:printlist (bom:find-attribs package attriblist) port)))
	  (bom:components port (cdr ls) attriblist)))))

(define bom:find-attribs
  (lambda (package attriblist)
    (if (null? attriblist)
	'()
	(cons (gnetlist:get-package-attribute package (car attriblist))
	      (bom:find-attribs package (cdr attriblist))))))

;;
;; Bill of Material backend written by Matt Ettus ends here
;;
;; --------------------------------------------------------------------------

