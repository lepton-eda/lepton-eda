;;; gEDA - GPL Electronic Design Automation
;;; gnetlist - gEDA Netlist
;;; Copyright (C) 1998-2008 Ales Hvezda
;;; Copyright (C) 1998-2008 gEDA Contributors (see ChangeLog for details)
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

;; RACAL-REDAC / Cadstar netlist format by Wojciech Kazubski 2003

;;
;; Display the individual net connections
;;
(define redac:display-connections
   (lambda (nets port k)
      (if (not (null? nets))
	 (let ((item (string-append (car (car nets)) " " (car (cdr (car nets))))))
	    (display item port)
	    (if (not (null? (cdr nets)))
	       (begin
	       (if (> k 0)
	          (begin
	    	    (display " " port)
		    (redac:display-connections (cdr nets) port (- k 1)))
	          (begin
	            (display (string-append "\r\n"  item " ") port)
		    (redac:display-connections (cdr nets) port (+ k 6))))))))))


(define redac:write-net
   (lambda (port netnames)
      (if (not (null? netnames))
         (let ((netname (car netnames)))
	    (display ".REM " port)
	    (display netname port)
	    (display "\r\n" port)
            (redac:display-connections 
		       (gnetlist:get-all-connections netname) port 7)
	    (display "\r\n" port)
	    (redac:write-net port (cdr netnames))
	    ))))

(define redac 
   (lambda (filename)
      (let ((port (if (string=? "-" filename)
		      (current-output-port)
		      (open-output-file filename))))
         (display ".PCB\r\n" port)
         (display ".REM CREATED BY gEDA GNETLIST\r\n" port)
         (display ".CON\r\n" port)
         (display ".COD 2\r\n\r\n" port)
         (redac:write-net port (gnetlist:get-all-unique-nets "dummy"))
         (display ".EOD\r\n" port)
         (close-output-port port))))


