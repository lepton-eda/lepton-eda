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

(define gossip:list-pins
   (lambda (allnets uref pin port)
      (let ((pinname (gnetlist:get-pin-attribute2 uref (number->string pin) "label")))
         (if (string=? "unknown" pinname)
            (display ")\n" port)
            (begin
               (display " :" port)
               (display pinname port)
               (write-char #\space port)
               (display (gossip:find-net uref pin allnets) port)
               (gossip:list-pins allnets uref (+ 1 pin) port))))))
      
;(define gossip:reverse-netlist
;   (lambda (allnets)
;      (if (null? allnets)
;         '()
;         (let ((connections (gnetlist:get-all-connections (car allnets))))
;            (cons (gossip:connectionlist connections)
;                  (gossip:reverse-netlist (cdr allnets))))))
      
(define gossip:find-net
   (lambda (uref pin allnets)
      (cond
         ((null? allnets) "Not Connected" )
         ((gossip:finder uref pin (gnetlist:get-all-connections (car allnets)))(car allnets))
         (#t (gossip:find-net uref pin (cdr allnets))))))

(define gossip:finder
   (lambda (uref pin list)
      (cond
         ((null? list)#f)
         ((and (string=? uref (caar list)) (string=? (number->string pin) (cadar list))) #t)
         (#t (gossip:finder uref pin (cdr list))))))

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

(define gossip:blocks
   (lambda (port ls allnets)
      (if (not (null? ls))
         (let ((package (car ls)))
            (display "   (" port)
            (display package port)
            (gossip:list-pins allnets package 1 port)
            (gossip:blocks port (cdr ls) allnets)))))

(define gossip:signals
   (lambda (port)
      (display "(signals " port)
      (display (gnetlist:get-all-unique-nets "dummy") port)
      (display ")\n" port)))

(define gossip:write-block-header
   (lambda (port)
      (let ((blockname (gnetlist:get-toplevel-attribute "blockname")))
         (display "(define-block (" port)
         (display blockname port)
         (display " (" port)
         (newline port))))

(define gossip 
   (lambda (output-filename)
      (let ((port (open-output-file output-filename)))
         (begin
            (gossip:write-top-header port)
            (gossip:get-libraries port packages '())
            (gossip:write-block-header port)
            (gossip:signals port)
            (gossip:blocks port packages (gnetlist:get-all-unique-nets "dummy")))
         (close-output-port port))))


