;;; $Id$
;;;
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

;; PADS netlist format

;; This procedure takes a net name as determined by gnetlist and
;; modifies it to be a valid pads net name.
;;
(define pads:map-net-names
  (lambda (net-name)
    (let ((net-alias net-name)
          )
      ;; Convert to all upper case because Pads seems
      ;; to do that internally anyway and we'd rather do
      ;; it here to catch shorts created by not preserving
      ;; case.  Plus we can eliminate lots of ECO changes
      ;; that will show up during backannotation.
      (string-upcase net-alias)
      )
    )
  )

;; This procedure takes a refdes as determined by gnetlist and
;; modifies it to be a valid pads refdes.
;;
(define pads:map-refdes
  (lambda (refdes)
    (let ((refdes-alias refdes)
          )
      ;; Convert to all upper case because Pads seems
      ;; to do that internally anyway and we'd rather do
      ;; it here to catch name clashes created by not preserving
      ;; case.
      (string-upcase refdes-alias)
      )
    )
  )

(define pads:components
   (lambda (port packages)
      (if (not (null? packages))
         (begin
            (let ((pattern (gnetlist:get-package-attribute (car packages) 
                                                           "pattern"))
	    ;; The above pattern should stay as "pattern" and not "footprint"
                  (package (car packages)))
               (if (not (string=? pattern "unknown"))
                  (display pattern port))

	       ;; print out the refdes with aliasing
               (display (gnetlist:alias-refdes package) port)

	       (write-char #\tab port) 
               (display (gnetlist:get-package-attribute package "footprint") port)
               (newline port))
            (pads:components port (cdr packages))))))

(define (pads:display-connections nets)
  (let ((k ""))
    (for-each (lambda (in-string)
                (set! k (string-append k in-string)))
              (map (lambda (net)
                     (string-append " " (gnetlist:alias-refdes (car net)) "." (car (cdr net))))
                   nets))
    (string-append k "\n")))


; This function is replaced with the above one. Due to non existent
; verification, this function is left commented out.
; /spe, 2002-01-08
;(define (pads:display-connections nets)
;  (if (not (null? nets))
;      (string-append " " (car (car nets)) "." (car (cdr (car nets)))
;       (pads:display-connections (cdr nets)))
;      "\n"))



(define pads:write-net
   (lambda (port netnames)
      (if (not (null? netnames))
         (let ((netname (car netnames)))
	    (display "*SIGNAL* " port)
	    (display (gnetlist:alias-net netname) port)
	    (newline port)
            (display (gnetlist:wrap 
		      (pads:display-connections 
		       (gnetlist:get-all-connections netname)) 
		      78
		      "") 
		     port)
	    (pads:write-net port (cdr netnames))))))

(define pads 
   (lambda (filename)
      (let ((port (open-output-file filename)))
	;; initialize the net-name aliasing
	(gnetlist:build-net-aliases pads:map-net-names all-unique-nets)
	
	;; initialize the refdes aliasing
	(gnetlist:build-refdes-aliases pads:map-refdes packages)

	;; print out the header
	(display "!PADS-POWERPCB-V3.0-MILS!\n" port)
	(display "\n*PART*\n" port)
	
	;; print out the parts
	(pads:components port packages)
	
	;; print out the net information
	(display "\n*NET*\n" port)
	(pads:write-net port (gnetlist:get-all-unique-nets "dummy"))
	
	;; print out the footer
	(display "\n*END*\n" port)
	(close-output-port port))))

