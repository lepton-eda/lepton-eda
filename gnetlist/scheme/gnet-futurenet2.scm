;;; gEDA - GPL Electronic Design Automation
;;; gnetlist - gEDA Netlist
;;; FutureNet2 backend
;;; Copyright (C) 2003, 2005 Dan McMahill
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


;;; Notes about futurenet2 (.NV2) format pinlists
;;;  
;;;  - Case does not seem to be preserved so to avoid issues,
;;;    simply output the netnames in all caps.
;;;
;;;  - Netname length is 8 characters max.  +,-, and _ are allowed
;;;
;;;  - How are DATA,3 and DATA,4 used?  In one example, DATA,4 is 
;;;    not used.  In the other DATA,3 and DATA,4 are identical and
;;;    appear to be set to the value (10.0k for example) of the part.
;;;
;;;    From Ferenc Marton (martonf at datapress dot hu):
;;;
;;;    These get combined in Ranger2 to produce the device
;;;    description with a "," separating the two entries.
;;;    DATA,3 is the "device name", max of 5 characters
;;;    DATA,4 is the "device value", max of 13 characters.
;;;    We could put the footprint into DATA,4
;;;
;;;  - In the "PIN" and "SIG" lines, what are the various fields really
;;;    doing?
;;;
;;;    It seems that for a PIN line, the format is:
;;;
;;;    PIN,,<netname>,1-1,5,<net attribute number>,<pinnumber>
;;;
;;;    What are the "1-1" and the "5"?  On the <net attribute
;;;    number> I've seen "23", "25", and "100".  Maybe these
;;;    indicate signal vs power or some sort of routing preference?
;;;
;;;    For a SIG line, the format seems to be:
;;;
;;;    SIG,<netname>,1-1,5,<netname>
;;;
;;;    What exactly are "1-1" and "5"?  I think maybe the "1-1" part
;;;    has something to do with sheet number in a multipage schematic.
;;;


;; This procedure takes a net name as determined by gnetlist and
;; modifies it to be a valid FutureNet2 net name.
;;
(define futurenet2:map-net-names
  (lambda (net-name)
    (let ((rx (make-regexp "^unnamed_net"))
	  (net-alias net-name)
          )
      ;; XXX we should use a dynamic regexp based on the current value
      ;; for the unnamed net base string.

      ;; Remove "unnamed_net" and replace with "NET"
      (if (regexp-exec rx net-name) 
	    (set! net-alias (string-append "NET" (substring net-name 11)))
	    )
      
      ;; Truncate to 8 characters
      (if (> (string-length net-alias) 8)
	  (set! net-alias (substring net-alias 0 8))
	  )
      ;; Convert to all upper case
      (string-upcase net-alias)
      )
    )
  )

;; This procedure takes a refdes as determined by gnetlist and
;; modifies it to be a valid FutureNet2 refdes.
;;
(define futurenet2:map-refdes
  (lambda (refdes)
    (let ((refdes-alias refdes)
          )

      ;; XXX do we need to truncate to 8 characters
      ;; like with net names?
;;      (if (> (string-length refdes-alias) 8)
;;	  (set! refdes-alias (substring refdes-alias 0 8))
;;	  )

      ;; Convert to all upper case
      (string-upcase refdes-alias)
      )
    )
  )

;; write out the pins for a particular component
(define futurenet2:component_pins
  (lambda (port package pins)
    (if (and (not (null? package)) (not (null? pins)))
	(begin
	  (let (
		(pin (car pins)))
	    ;;  PIN,,NetName,1-1,5,20/23,pinnum
	    (display "PIN,," port)

	    (display 
	     (gnetlist:alias-net (car (gnetlist:get-nets package pin)))
	     port)
	    
	    ;; XXX I've seen 20, 23, and 100 in the position where the
	    ;; "23" is here.  Seems to be a property like signal vs
	    ;; power net.  Not sure how to support that.
	    (display ",1-1,5,23," port)

	    (display 
	     (gnetlist:get-attribute-by-pinnumber package pin "pinnumber")
	     port)
	    (newline port)
	    )
	  (futurenet2:component_pins port package (cdr pins))
	  )
	)
    )
  )

	    
;; write out the components
(define futurenet2:components
   (lambda (port packages symcnt)
      (if (not (null? packages))
         (begin
            (let ((pattern (gnetlist:get-package-attribute (car packages) 
                                                           "pattern"))
	    ;; The above pattern should stay as "pattern" and not "footprint"
                  (package (car packages)))
	      (display "(SYM," port)
	      (display symcnt port)

	      ;; write the reference designator
	      (display "\nDATA,2," port)
	      (display (gnetlist:alias-refdes package) port)
	      
	      ;; If there is a "value" attribute, output that.
	      ;; Otherwise output the "device" attribute (the symbol name).
	      (display "\nDATA,3," port)
	      (let 
		  ((val (gnetlist:get-package-attribute package
							"value")) )
		(if (string=? val "unknown") 
		    (set! val (gnetlist:get-package-attribute package "device") )
		    )
		(display  val port)
		)
	      (display "\n" port)

	      ;; write the footprint
	      (display "DATA,4," port)
	      (display (gnetlist:get-package-attribute package
						       "footprint")
		       port)
	      (display "\n" port)
	      
	      ;; write the pins
	      (futurenet2:component_pins port package
					 (gnetlist:get-pins package))

	      ;; close the part
	      (display ")\n" port)
	      
	      )
            (futurenet2:components port (cdr packages) (+ symcnt 1))
	    )
	 )
      )
   )

;; write out the nets
(define futurenet2:write-net
   (lambda (port netnames)
      (if (not (null? netnames))
         (let (
	       (netname (car netnames))
	       (alias (gnetlist:alias-net (car netnames)))
	       )
	   (display "SIG," port)
	   (display alias port)
	   (display ",1-1,5," port)
	   (display alias port)
	   (newline port)
	   (futurenet2:write-net port (cdr netnames))
	   )
	 )
      )
   )

;; The top level netlister for futurenet2
(define futurenet2 
   (lambda (filename)
     (newline)
     (display "---------------------------------\n")
     (display "gEDA/gnetlist FutureNet2 Backend\n")
     (display "This backend is EXPERIMENTAL\n")
     (display "Use at your own risk!\n")
     (display "\n")
     (display "You may need to run the output netlist\n")
     (display "through unix2dos before importing to\n")
     (display "Ranger2 or other windows based layout tools\n")
     (display "---------------------------------\n\n")

      (let ((port (open-output-file filename))
	    (all-nets (gnetlist:get-all-unique-nets "dummy"))
	    )
	
	;; initialize the net-name aliasing
        (gnetlist:build-net-aliases futurenet2:map-net-names all-unique-nets)

	;; initialize the refdes aliasing
        (gnetlist:build-refdes-aliases futurenet2:map-refdes packages)

	;; write the header
	(display "PINLIST,2\n" port)
	(display "(DRAWING,GEDA.PIN,1-1\n" port)
	
	;; write the components
	(futurenet2:components port packages 1)
	(display ")\n" port)
	
	;; write the nets
	(futurenet2:write-net port all-nets)
	
	;; terminating ")"
	(display ")\n" port)
	
	;; close netlist
	(close-output-port port)
	)
      )
   )

