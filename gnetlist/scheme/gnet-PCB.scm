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

;;  PCB format

(define (PCB:display-connections nets)
  (let ((k ""))
    (for-each (lambda (in-string)
                (set! k (string-append k in-string)))
              (map (lambda (net)
                     (string-append (car net) "-" (car (cdr net)) " "))
                   nets))
    (string-append k "\n")))


; This function is replaced with the above one. Due to non existent
; verification, this function is left commented out.
; /spe, 2002-01-08
;(define (PCB:display-connections nets)
;  (if (not (null? nets))
;      (string-append
;       (car (car nets)) "-" (car (cdr (car nets))) " " 
;       (PCB:display-connections (cdr nets)))
;      "\n"))


(define (PCB:write-net netnames port)
  (if (not (null? netnames))
      (let ((netname (car netnames)))
	(display netname port)
	(display "\t" port)
	(display (gnetlist:wrap (PCB:display-connections (gnetlist:get-all-connections netname)) 200 " \\") port)
	(PCB:write-net (cdr netnames) port))))


(define (PCB output-filename)
  (let ((port (open-output-file output-filename)))
    (PCB:write-net (gnetlist:get-all-unique-nets "dummy") port)
    (close-output-port port)))

