;;; gEDA - GPL Electronic Design Automation
;;; gnetlist back end for Osmond PCB Design
;;; Copyright (C) 2007 John P. Doty
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

;;
;; Parts list 
;;
(define osmond:parts
   (lambda (port ls)
      (if (not (null? ls))
         (let ((package (car ls)))
            (begin
	       (display "Part " port)
               (display (gnetlist:get-package-attribute package "footprint") port)
	       (display " { Name " port)
	       (display package port )
	       (display " }" port)
               (newline port)
               (osmond:parts port (cdr ls)))))))

;;
;; List all connections to a net
;;
(define (osmond:list-connections nets)
  (let ((k ""))
    (for-each (lambda (in-string)
                (set! k (string-append k in-string)))
              (map (lambda (net)
                     (string-append " " (car net) "-" (car (cdr net))))
                   nets))
   k))

;
; Write out each signal
;
(define osmond:write-signal
   (lambda (port signals)
      (if (not (null? signals))
         (let ((signal (car signals)))
	    (begin
	       (display "Signal " port )
	       (write-char #\" port)
	       (display signal port)
	       (write-char #\" port)
               (newline port)
	       (display "  {" port)
               (display (osmond:list-connections 
	          (gnetlist:get-all-connections signal)) port)
	       (display " }" port)
	       (newline port)
	       (osmond:write-signal port (cdr signals)))))))
	              
;;
;; Write out all the signals
;;	       
(define osmond:signal
   (lambda (port)
      (let ((all-uniq-nets (gnetlist:get-all-unique-nets "dummy")))
         (osmond:write-signal port all-uniq-nets))))


(define osmond 
   (lambda (output-filename)
      (let ((port (open-output-file output-filename)))
         (begin
	    (osmond:parts port packages)
	    (osmond:signal port)
         )
         (close-output-port port))))
