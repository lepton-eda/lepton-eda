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
;; Top level header
;;
(define liquidpcb:write-top-header
   (lambda ()
      (display "<LiquidPCB>")
      (newline)))

;;
;; Bottom footer
;;
(define liquidpcb:write-bottom-footer
   (lambda ()
      (display "</LiquidPCB>")
      (newline)))

;;
;; Header for netlist section
;;
(define liquidpcb:start-netlist
   (lambda ()
      (display "\t<netlist name=\"Main netlist\">")
      (newline)))

;;
;; footer for netlist section
;;
(define liquidpcb:end-netlist
   (lambda ()
      (display "\t</netlist>")
      (newline)))

;;
;; Write the individual net connections
;;
(define liquidpcb:write-connections
   (lambda (nets)
      (if (not (null? nets))
         (begin
            (display "\t\t\t<netnode component=\"")
            (display (car (car nets)))
            (display "\" pin=")
            (display (car (cdr (car nets))))
            (display " />")
            (newline)
            (liquidpcb:write-connections (cdr nets))))))


;;
;; Write netname : uref pin, uref pin, ...
;;
(define liquidpcb:write-net
   (lambda (netnames)
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
               (liquidpcb:write-net (cdr netnames)))))))

;;
;; Write the netlist section of the liquidPCB format
;;
(define liquidpcb:write-netlist
   (lambda ()
      (let ((all-uniq-nets (gnetlist:get-all-unique-nets "dummy")))
         (liquidpcb:write-net all-uniq-nets))))

;;
;; Highest level function
;;
(define (liquidpcb output-filename)
  (set-current-output-port (gnetlist:output-port output-filename))
  (begin
     (liquidpcb:write-top-header)
     (liquidpcb:start-netlist)
     (liquidpcb:write-netlist)
     (liquidpcb:end-netlist))
     (liquidpcb:write-bottom-footer)
  (close-output-port (current-output-port)))

;;
;; liquid PCB netlist backend ends
;;
;; --------------------------------------------------------------------------
