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

;; MAXASCII netlist format

(define maxascii:components
   (lambda (port packages)
      (if (not (null? packages))
         (begin
            (let ((pattern (gnetlist:get-package-attribute (car packages) 
                                                           "pattern"))
                  (package (car packages)))
;               (if (not (string=? pattern "unknown"))
;                  (display pattern port))
               (display "*COMP " port)
               (display package port)
	       (write-char #\tab port) 
               (display "\"" port)
               (display (gnetlist:get-package-attribute package "footprint") port)
               (display "\"" port)
               (newline port))
            (maxascii:components port (cdr packages))))))

(define (maxascii:display-connections nets)
  (if (not (null? nets))
      (string-append " " (car (car nets)) ".\"" (car (cdr (car nets))) "\""
       (maxascii:display-connections (cdr nets)))
      "\n"))


;;
;; Wrap a string into lines no longer than wrap-length
;; (from Stefan Petersen)
(define (maxascii:wrap string-to-wrap wrap-length netname)
  (if (> wrap-length (string-length string-to-wrap))
      string-to-wrap ; Last snippet of string
      (let ((pos (string-rindex string-to-wrap #\space 0 wrap-length)))
	(cond ((not pos)
	       (display "Couldn't wrap string  at requested position\n")
	       " Wrap error!")
	      (else
	       (string-append 
		(substring string-to-wrap 0 pos) 
		" \n*NET \"" netname "\" " 
		(maxascii:wrap (substring string-to-wrap (+ pos 1)) wrap-length netname)))))))



(define maxascii:write-net
   (lambda (port netnames)
      (if (not (null? netnames))
         (let ((netname (car netnames)))
	    (display "*NET " port)
            (display "\"" port)
	    (display netname port)
            (display "\"" port)
	    (newline port)
	    (display "*NET " port)
            (display "\"" port)
	    (display netname port)
            (display "\"" port)
            (display (maxascii:wrap 
		      (maxascii:display-connections 
		       (gnetlist:get-all-connections netname)) 
		      490 netname) 
		     port)
;;            (display (maxascii:display-connections 
;;		       (gnetlist:get-all-connections netname)) 
;;		     port)
	    (maxascii:write-net port (cdr netnames))))))

(define maxascii 
   (lambda (filename)
      (let ((port (open-output-file filename)))
         (display "*OrCAD\n*START\n" port)

         (maxascii:components port packages)


         (maxascii:write-net port (gnetlist:get-all-unique-nets "dummy"))
         (display "\n*END\n" port)
         (close-output-port port))))

