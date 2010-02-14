;;; gEDA - GPL Electronic Design Automation
;;; gnetlist - gEDA Netlist
;;; Copyright (C) 1998-2010 Ales Hvezda
;;; Copyright (C) 1998-2010 gEDA Contributors (see ChangeLog for details)
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
;; gEDA's native test netlist format specific functions go here 
;;

;;
;; Top level header
;;
(define geda:write-top-header
   (lambda (p)
      (display "START header" p) 
      (newline p)
      (newline p)
      (display "gEDA's netlist format" p)
      (newline p)
      (display "Created specifically for testing of gnetlist" p)
      (newline p)
      (newline p)
      (display "END header" p)
      (newline p)
      (newline p)))

;;
;; header for components section
;;
(define geda:start-components
   (lambda (p)
      (display "START components" p)
      (newline p)
      (newline p)))

;;
;; footer for components section
;;
(define geda:end-components
   (lambda (p)
      (newline p)
      (display "END components" p)
      (newline p)
      (newline p)))

;;
;; header for renamed section
;;
(define geda:start-renamed-nets
   (lambda (p)
      (display "START renamed-nets" p)
      (newline p)
      (newline p)))

;;
;; footer for renamed section
;;
(define geda:end-renamed-nets
   (lambda (p)
      (newline p)
      (display "END renamed-nets" p)
      (newline p)
      (newline p)))

;;
;; header for nets section
;;
(define geda:start-nets
   (lambda (p)
      (display "START nets" p)
      (newline p)
      (newline p)))

;;
;; footer for net section
;;
(define geda:end-nets
   (lambda (p)
      (newline p)
      (display "END nets" p)
      (newline p)
      (newline p)))
	
;;
;; Top level component writing 
;;
(define geda:components
   (lambda (port ls)
      (if (not (null? ls))
         (let ((package (car ls)))
            (begin
               (display package port)
               (write-char #\space port)
               (display "device=" port)
               (display (get-device package) port)
               (newline port)
               (geda:components port (cdr ls)))))))

;;
;; renamed nets writing 
;;
(define geda:renamed-nets
   (lambda (port ls)
      (if (not (null? ls))
         (let ((renamed-pair (car ls)))
            (begin
;;;	       (display renamed-pair) (newline)
               (display (car renamed-pair) port)
	       (display " -> " port)
               (display (car (cdr renamed-pair)) port)
               (newline port)
               (geda:renamed-nets port (cdr ls)))))))

;;
;; Display the individual net connections
;;
(define geda:display-connections
   (lambda (nets port)
      (if (not (null? nets))
	 (begin
	    (display (car (car nets)) port)
	    (write-char #\space port) 
	    (display (car (cdr (car nets))) port)
	    (if (not (null? (cdr nets)))
               (begin
	   	  (write-char #\, port) 
	          (write-char #\space port)))
	       (geda:display-connections (cdr nets) port)))))

;;
;; Display all nets 
;;
(define geda:display-name-nets
   (lambda (port nets)
      (begin
         (geda:display-connections nets port)
         (write-char #\space port) 
         (newline port))))

;;
;; Write netname : uref pin, uref pin, ...
;;
(define geda:write-net
   (lambda (port netnames)
      (if (not (null? netnames))
         (let ((netname (car netnames)))
	    (begin
	       (display netname port)
	       (display " : " port)
               (geda:display-name-nets port (gnetlist:get-all-connections netname))
	       (geda:write-net port (cdr netnames))))))) 

;;
;; Write the net part of the gEDA format
;;
(define geda:nets
   (lambda (port)
      (let ((all-uniq-nets (gnetlist:get-all-unique-nets "dummy")))
         (geda:write-net port all-uniq-nets))))

;;; Highest level function
;;; Write my special testing netlist format
;;;
(define geda 
   (lambda (output-filename)
      (let ((port (open-output-file output-filename)))
         (begin
;;;         (gnetlist:set-netlist-mode "gEDA") No longer needed
            (geda:write-top-header port)
            (geda:start-components port)
            (geda:components port packages)
            (geda:end-components port)
            (geda:start-renamed-nets port)
            (geda:renamed-nets port (gnetlist:get-renamed-nets "dummy"))
            (geda:end-renamed-nets port)
            (geda:start-nets port)
            (geda:nets port)
            (geda:end-nets port))
         (close-output-port port))))

;;
;; gEDA's native test netlist format specific functions ends 
;;
;; --------------------------------------------------------------------------

