;;; $Id$
;;;
;;; gEDA - GNU Electronic Design Automation
;;; gnetlist - GNU Netlist
;;; Copyright (C) 2004 Braddock Gaskill (braddock@braddock.com, 
;;;                                      adapted PCB code to Eagle)
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

;; EAGLE netlist format

;; This procedure takes a net name as determined by gnetlist and
;; modifies it to be a valid eagle net name.
;;
(define eagle:map-net-names
  (lambda (net-name)
    (let ((net-alias net-name)
          )
      ;; Convert to all upper case because Eagle seems
      ;; to do that internally anyway and we'd rather do
      ;; it here to catch shorts created by not preserving
      ;; case.  Plus we can eliminate lots of ECO changes
      ;; that will show up during backannotation.
      (string-upcase net-alias)
      )
    )
  )

(define eagle:components
   (lambda (port packages)
      (if (not (null? packages))
         (begin
            (let ((pattern (gnetlist:get-package-attribute (car packages) 
                                                           "pattern"))
	    ;; The above pattern should stay as "pattern" and not "footprint"
                  (package (car packages))
		  (lib (gnetlist:get-package-attribute (car packages) "lib"))
		  (value (gnetlist:get-package-attribute (car packages) "value"))
		  (device (gnetlist:get-package-attribute (car packages) "device"))
		  )
               (if (not (string=? pattern "unknown"))
                  (display pattern port))
	       (display "ADD '" port)
               (display package port)
	       (display "' " port)
;;	       (display "' TQFP144@atmel (0 0)" port)
;;;	       (write-char #\tab port) 
               (display (gnetlist:get-package-attribute package "footprint") port)
	       (display "@" port)
	       (if (not (string=? lib "unknown"))   
		   (display lib port)
		   (display "smd-ipc" port))
	       (display " (1 1);" port)
	       (newline port)
               (if (not (string=? value "unknown"))
		   (begin
		     (display "VALUE '" port)
		     (display package port)
		     (display "' '" port)
		     (display value port)
		     (display "';" port)
		     (newline port)
		     )
		   (if (not (string=? device "unknown"))
		       (begin
			 (display "VALUE '" port)
			 (display package port)
			 (display "' '" port)
			 (display device port)
			 (display "';" port)
			 (newline port)
			 )
		   ))
	       )
            (eagle:components port (cdr packages))))))

(define (eagle:display-connections nets)
  (let ((k ""))
    (for-each (lambda (in-string)
                (set! k (string-append k in-string)))
              (map (lambda (net)
                     (string-append "   '" (car net) "' '" (car (cdr net)) "'\r\n"))
                   nets))
    (string-append k ";\n")))


; This function is replaced with the above one. Due to non existent
; verification, this function is left commented out.
; /spe, 2002-01-08
;(define (eagle:display-connections nets)
;  (if (not (null? nets))
;      (string-append " " (car (car nets)) "." (car (cdr (car nets)))
;       (eagle:display-connections (cdr nets)))
;      "\n"))



(define eagle:write-net
   (lambda (port netnames)
      (if (not (null? netnames))
         (let ((netname (car netnames)))
	    (display "SIGNAL '" port)
	    (display (gnetlist:alias-net netname) port)
	    (display "'" port)
	    (newline port)
;            (display (gnetlist:wrap 
;		      (eagle:display-connections 
;		       (gnetlist:get-all-connections netname)) 
;		      78
;		      "") 
;		     port)
            (display (eagle:display-connections 
		       (gnetlist:get-all-connections netname))
		     port)
	    (eagle:write-net port (cdr netnames))))))

(define eagle 
   (lambda (filename)
      (let ((port (open-output-file filename)))
	;; initialize the net-name aliasing
	(gnetlist:build-net-aliases eagle:map-net-names all-unique-nets)
	
	;; print out the header
;;;	(display "!EAGLE-POWERPCB-V3.0-MILS!\n" port)
;;;	(display "\n*PART*\n" port)
;;;	(display "/* CADSoft Eagle Scripted Netlist Format */\n" port)
	(display "   ;\n" port)
	
	;; print out the parts
	(eagle:components port packages)
	
	;; print out the net information
;;;	(display "\n*NET*\n" port)
	(eagle:write-net port (gnetlist:get-all-unique-nets "dummy"))
	
	;; print out the footer
;;;	(display "\n*END*\n" port)
	(close-output-port port))))


