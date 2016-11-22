;;; gEDA - GPL Electronic Design Automation
;;; gnetlist - gEDA Netlist
;;; Copyright (C) 2008-2010 Ales Hvezda
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
;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
;;; MA 02111-1301 USA

;; --------------------------------------------------------------------------
;;
;; liquid pcb gnetlist backend
;;

;;
;; Write the individual net connections
;;
(define (liquidpcb:write-connections nets)
  (if (not (null? nets))
      (begin
        (display "\t\t\t<netnode component=\"")
        (display (car (car nets)))
        (display "\" pin=")
        (display (car (cdr (car nets))))
        (display " />")
        (newline)
        (liquidpcb:write-connections (cdr nets)))))


;;
;; Write netname : uref pin, uref pin, ...
;;
(define (liquidpcb:write-net netnames)
  (if (not (null? netnames))
      (let ((netname (car netnames)))
        (begin
          (display "\t\t<net name=\"")
          (display netname)
          (display "\">")
          (newline)
          (liquidpcb:write-connections (gnetlist:get-all-connections netname))
          (display "\t\t</net>")
          (newline)
          (liquidpcb:write-net (cdr netnames))))))

;;
;; Write the netlist section of the liquidPCB format
;;
(define (liquidpcb:write-netlist)
  (liquidpcb:write-net (gnetlist:get-all-unique-nets "dummy")))

;;
;; Highest level function
;;
(define (liquidpcb output-filename)
  (with-output-to-port (gnetlist:output-port output-filename)
    (lambda ()
      (display "<LiquidPCB>\n")
      (display "\t<netlist name=\"Main netlist\">\n")
      (liquidpcb:write-netlist)
      (display "\t</netlist>\n")
      (display "</LiquidPCB>\n"))))

;;
;; liquid PCB netlist backend ends
;;
;; --------------------------------------------------------------------------
