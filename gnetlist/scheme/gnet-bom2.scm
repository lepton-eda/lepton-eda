;;; gEDA - GNU Electronic Design Automation
;;; gnetlist - GNU Netlist
;;; Copyright (C) 1998-2001 Ales V. Hvezda
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

(define bom2
  (lambda (output-filename)
    (let ((port (open-output-file output-filename))
	  (attriblist (bom2:parseconfig (open-input-file "attribs"))))
      (bom2:printlist (cons 'package attriblist) port #\:)
      (newline port)
      (bom2:printbom port (bom2:components packages attriblist))
      (close-output-port port))))

(define bom2:printbom
  (lambda (port bomlist)
    (if (not (null? bomlist))
      (if (not (null? (caar bomlist)))
        (begin
          (display (caaar bomlist) port)
          (if (not (null? (cdaar bomlist)))
            (write-char #\, port))
          (bom2:printbom port (cons (cons (cdaar bomlist)(cdar bomlist))(cdr bomlist))))
        (begin
          (display #\: port)
          (bom2:printlist (cdar bomlist) port #\:)
          (newline port)
          (bom2:printbom port (cdr bomlist)))))))

(define bom2:printlist
  (lambda (ls port delimiter)
    (if (null? ls)
        #f
	(begin
	  (display (car ls) port)
          (if (not (null? (cdr ls)))
	    (write-char delimiter port))
	  (bom2:printlist (cdr ls) port delimiter)))))

; Parses attrib file. Returns a list of read attributes.
(define bom2:parseconfig
  (lambda (port)
    (let ((read-from-file (read-delimited " \n\t" port)))
      (cond ((eof-object? read-from-file)
	     '())
	    ((= 0 (string-length read-from-file))
	     (bom2:parseconfig port))
	    (else
	     (cons read-from-file (bom2:parseconfig port)))))))

(define bom2:match-list?
  (lambda (l1 l2)
    (cond
      ((and (null? l1)(null? l2))#t)
      ((null? l1) #f)
      ((null? l2) #f)
      ((not (string=? (car l1)(car l2)))#f)
      (#t (bom2:match-list? (cdr l1)(cdr l2))))))

(define bom2:match?
  (lambda (uref attriblist bomlist)
    (if (null? bomlist)
      (list (cons (list uref) attriblist))
      (if (bom2:match-list? attriblist (cdar bomlist))
;;        (cons (cons (cons uref (caar bomlist)) (cdar bomlist))(cdr bomlist))
        (cons (cons (merge (list uref) (caar bomlist) string<? ) (cdar bomlist))(cdr bomlist))
        (cons (car bomlist)(bom2:match? uref attriblist (cdr bomlist)))))))

(define bom2:components
  (lambda (ls attriblist)
    (if (null? ls)
      '()
      (let ((package (car ls))
            (bomlist (bom2:components (cdr ls) attriblist))
            (attribs (bom2:find-attribs (car ls) attriblist)))
        (if (not (string=? "unknown" (gnetlist:get-package-attribute package "nobom")))
          bomlist
          (bom2:match? package attribs bomlist))))))

(define bom2:find-attribs
  (lambda (package attriblist)
    (if (null? attriblist)
	'()
	(cons (gnetlist:get-package-attribute package (car attriblist))
	      (bom2:find-attribs package (cdr attriblist))))))

;;
;; Bill of Material backend written by Matt Ettus ends here
;;
;; --------------------------------------------------------------------------

