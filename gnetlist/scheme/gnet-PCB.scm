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

;;  PCB format

(define PCB:display-connections
   (lambda (nets port)
      (if (not (null? nets))
         (begin
            (display (car (car nets)) port)
            (display "-" port)
            (display (car (cdr (car nets))) port)
            (write-char #\space port)
            (PCB:display-connections (cdr nets) port))
         (newline port))))


(define PCB:write-net
   (lambda (port netnames)
      (if (not (null? netnames))
         (let ((netname (car netnames)))
            (display netname port)
            (display "\t" port)
            (PCB:display-connections (gnetlist:get-all-connections netname) port)
            (PCB:write-net port (cdr netnames))))))

(define PCB
   (lambda (output-filename)
      (let ((port (open-output-file output-filename)))
         (gnetlist:set-netlist-mode "gEDA")
         (PCB:write-net port (gnetlist:get-all-unique-nets "dummy"))
         (close-output-port port))))

