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
;; Netlister for GOSSIP system simulation system, based on GUILE
;;  For more info see http://gossip.sourceforge.net 
;;


(define gossip:write-top-header
   (lambda (p)
      (display ";; Gossip Netlist Created by gNetlist" p) 
      (newline p)
      (newline p)
      (display ";; Created By Matt Ettus <matt@ettus.com>" p)
      (newline p)
      (display ";; Libraries:" p)
      (newline p)
      (newline p)))

(define gossip:get-libraries
  (lambda (p components done)
    (if (not (null? components))
      (let ((lib (gnetlist:get-package-attribute (car components) "library")))
        (if (string=? "unknown" lib)
          (begin
            (display "Component ")
            (display (car components))
            (display " does not have a library attribute\n")))
        (if (contains? done lib)
          (gossip:get-libraries p (cdr components) done)
          (begin
            (display "(use-library " p)
            (display lib p)
            (display " *)" p)
            (newline p)
            (gossip:get-libraries p (cdr components) (cons lib done))))))))

(define gossip:components
   (lambda (port ls)
      (if (not (null? ls))
         (let ((package (car ls)))
            (display package port)
            (write-char #\space port)
            (display "device=" port)
            (display (get-device package) port)
            (newline port)
            (gossip:components port (cdr ls))))))

(define gossip:display-connections
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
	       (gossip:display-connections (cdr nets) port)))))

(define gossip:display-name-nets
   (lambda (port nets)
      (begin
         (gossip:display-connections nets port)
         (write-char #\space port) 
         (newline port))))

(define gossip:write-net
   (lambda (port netnames)
      (if (not (null? netnames))
         (let ((netname (car netnames)))
	    (begin
	       (display netname port)
	       (display " : " port)
               (gossip:display-name-nets port (gnetlist:get-all-connections netname))
	       (gossip:write-net port (cdr netnames))))))) 

(define gossip:nets
   (lambda (port)
      (let ((all-uniq-nets (gnetlist:get-all-unique-nets "dummy")))
         (gossip:write-net port all-uniq-nets))))

(define gossip:write-block-header
   (lambda (port)
      (let ((blockname (gnetlist:get-toplevel-attribute "blockname")))
         (display "(define-block (" port)
         (display blockname port)
         (display " (" port))))
;;  Need to add code to find all input and output ports

(define gossip 
   (lambda (output-filename)
      (let ((port (open-output-file output-filename)))
         (begin
            (gossip:write-top-header port)
            (gossip:get-libraries port packages '())
            (gossip:write-block-header port)
            (gossip:nets port)
            (gossip:components port packages))
         (close-output-port port))))

