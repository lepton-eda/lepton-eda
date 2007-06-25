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
;; Bartels Format
;; Layout board;
;; PARTS
;;   part : footprint;
;; CONNECT
;;   /net1/ uref.pin=uref.pin=uref.pin=...uref.pin;
;;   /net2/ PRIORITY(1..100) MINDIST(mm) ROUTWIDTH(mm) uref.pin(width_mm)=...;
;; END.
;;

;;
;; Top level header
;;
(define bae:write-top-header
   (lambda (p)
      (display "LAYOUT board;" p) 
      (newline p)))
      
;;
;; header for components section
;;
(define bae:start-components
   (lambda (p)
      (display "PARTS" p)
      (newline p)))
;; no header for components   

;;
;; footer for components section
;;
(define bae:end-components
   (lambda (p)
      (display "" p)))

;;
;; header for renamed section
;;
(define bae:start-renamed-nets
   (lambda (p)
      (display "" p)))

;;
;; footer for renamed section
;;
(define bae:end-renamed-nets
   (lambda (p)
      (display "" p)))

;;
;; header for nets section
;;
(define bae:start-nets
   (lambda (p)
      (display "CONNECT" p)
      (newline p)))

;;
;; footer for net section
;;
(define bae:end-nets
   (lambda (p)
      (display "END." p)
      (newline p)))
	
;;
;; Top level component writing 
;;
(define bae:components
   (lambda (port ls)
      (if (not (null? ls))
         (let ((package (car ls)))
            (begin
	       (display "    " port)
               (display package port)
	       (display " : " port)
	       (display (gnetlist:get-package-attribute package  "footprint") port)
	       (display ";" port)
	       (newline port)
               (bae:components port (cdr ls)))))))

;;
;; renamed nets writing 
;;
(define bae:renamed-nets
   (lambda (port ls)
      (if (not (null? ls))
         (let ((renamed-pair (car ls)))
            (begin
;;;	       (display renamed-pair) (newline)
;;;            (display (car renamed-pair) port)
;;;            (display " -> " port)
;;;            (display (car (cdr renamed-pair)) port)
;;;            (newline port)
               (display "" port)
               (bae:renamed-nets port (cdr ls)))))))

;;
;; Display the individual net connections
;;
(define bae:display-connections
   (lambda (nets port)
      (if (not (null? nets))
  	 (begin
	    (let ((package (car (car nets))))
	       (display package port)
	       (write-char #\. port) 
	       (display (car (cdr (car nets))) port))
	    (if (not (null? (cdr nets)))
	       (begin
	          (display #\= port)))
	    (bae:display-connections (cdr nets) port)))))

;;
;; Display all nets 
;;
(define bae:display-name-nets
   (lambda (port nets)
      (begin
         (bae:display-connections nets port)
         (write-char #\; port)))) 

;;
;; Write netname : uref pin, uref pin, ...
;;
(define bae:write-net
   (lambda (port netnames)
      (if (not (null? netnames))
         (let ((netname (car netnames)))
	    (begin
	       (display "    " port)
	       (display "/'" port)
	       (display netname port)
	       (display "'/ " port)
               (bae:display-name-nets port (gnetlist:get-all-connections netname))
	       (newline port)
	       (bae:write-net port (cdr netnames))))))) 

;;
;; Write the net part of the gEDA format
;;
(define bae:nets
   (lambda (port)
      (let ((all-uniq-nets (gnetlist:get-all-unique-nets "dummy")))
         (bae:write-net port all-uniq-nets))))

;;; Highest level function
;;; Write my special testing netlist format
;;;
(define bae 
   (lambda (output-filename)
      (let ((port (open-output-file output-filename)))
         (begin
;;;         (gnetlist:set-netlist-mode "gEDA") No longer needed
            (bae:write-top-header port)
            (bae:start-components port)
            (bae:components port packages)
            (bae:end-components port)
            (bae:start-renamed-nets port)
            (bae:renamed-nets port (gnetlist:get-renamed-nets "dummy"))
            (bae:end-renamed-nets port)
            (bae:start-nets port)
            (bae:nets port)
            (bae:end-nets port))
         (close-output-port port))))

;;
;; gEDA's native test netlist format specific functions ends 
;;
;; --------------------------------------------------------------------------
