;;; gEDA - GPL Electronic Design Automation
;;; gnetlist - gEDA Netlist
;;; Backend for cascade (http://rfcascade.sourceforge.net)
;;; Copyright (C) 2003-2010 Dan McMahill
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


;; Locate and print out the global defaults if the element exists
(define cascade:write-defaults-top
  (lambda (port pkgs)
    (if (not (null? pkgs))
	(let ( (pkg (car pkgs)) )
	  (if (string=? (get-device pkg) "cascade-defaults-top")
	      (begin 
		(display "# Initial global defaults\n" port)
		(display "defaults " port)
		(map (lambda (attrib)
		       (let ((val (gnetlist:get-package-attribute pkg attrib)))
			 (if (not (string=? val "unknown"))
			     (display (string-append attrib "=" val " ") port)
			     )
			 )
		       )
		     (list "rin" "RIN" "rout" "ROUT" "rho" "RHO")
		     )
		(newline port)
		(newline port)
		)
	      (cascade:write-defaults-top port (cdr pkgs))
	      )
	  )
	)
    )
  )

;; Locate and print out the "source" line and return the refdes of
;; the first element in the cascade
(define cascade:write-source
  (lambda (port pkgs)
    (if (not (null? pkgs))
	(let ( (package (car pkgs) )
	       (sourcenet #f)
	       )
	  (if (string=? (get-device package) "cascade-source")
	      (begin
		(set! sourcenet (gnetlist:get-nets package "1"))
		(display "source " port)
		(map (lambda (attrib)
		       (let ((val (gnetlist:get-package-attribute package attrib)))
			 (if (not (string=? val "unknown"))
			     (display (string-append attrib "=" val " ") port)
			     )
			 )
		       )
		     (list "c" "C" "cn0" "CN0" "cn" "CN" "bw" "BW")
		     )
		(newline port)

		(if (string=? (caadr sourcenet) package)
		    (caaddr sourcenet)
		    (caadr sourcenet)
		    )
		)
	      (cascade:write-source port (cdr pkgs) )
	      )
	  )
	;; the list of packages is now empty
	'()
	)
    )
  )

;; recursively follow the cascade and print out each element as its
;; found 
(define cascade:follow-cascade
  (lambda (port pkg)
    (if (not (null? pkg)) 
	(begin
	  (let ( (outnet (gnetlist:get-nets pkg "2"))
		 )

	    ;; Is this a "defaults" element or a normal element?
	    ;; If its a defaults element, then print "defaults"
	    ;; instead of the reference designator because thats
	    ;; a keyword for cascade.
	    (if (string=? (get-device pkg) "cascade-defaults")
		(display "defaults " port)
		(display (string-append pkg " ") port)
		)

	    ;; spit out all the relevant attributes for element or
	    ;; defaults lines
	    (map (lambda (attrib)
		   (let ((val (gnetlist:get-package-attribute pkg attrib)))
		     (if (not (string=? val "unknown"))
			 (display (string-append attrib "=" val " ") port)
			 )
		     )
		   )
		 (list "g" "G" "gp" "GP" "gv" "GV" "nf" "NF" "iip3"
		       "IIP3" "r" "R" "rin" "RIN" "rout" "ROUT"
		       "rho" "RHO")
		 )
	    (newline port)

	    ;;(display "cascade:follow-cascade  -- outnet = ")
	    ;;(display outnet)
	    ;;(newline)
	    (if (>= (length outnet) 3)
		(if (string=? (caadr outnet) pkg)
		    (cascade:follow-cascade port(caaddr outnet))
		    (cascade:follow-cascade port (caadr outnet))
		    )
		)
	    )
	  )
	)
    )
  )

;; The top level netlister for cascade
(define cascade 
   (lambda (filename)
     (newline)
     (display "---------------------------------\n")
     (display "gEDA/gnetlist Cascade Backend\n")

     (display "---------------------------------\n\n")

     (display (string-append "Writing to output file \"" filename
			     "\"... ") )
      (let ((port (open-output-file filename))
	    (first_block #f)
	    )

	;; write the header
	(display "# Cascade (http://rfcascade.sourceforge.net)\n"
		 port)
	(display "# Created with gEDA/gnetlist\n\n" port)

	;; Write out an initial "defaults" line if it exists
	(cascade:write-defaults-top port packages)

	;; Write out the "source" line and keep track of what its
	;; connected to.  If we couldn't find the source, then
	;; exit out.
	(display "# Source definition\n" port)
	(set! first_block (cascade:write-source port packages))
	(if (null? first_block)
	    (error "You must include a source element in your schematic!")
	    )

	;; write the components
	(display "\n# Cascaded system\n" port)
	(cascade:follow-cascade port first_block)

	;; write the footer
	(newline port)
	(display "# End of netlist created by gEDA/gnetlist\n\n" port)
	
	;; close netlist
	(close-output-port port)

	)
      
      (display "done\n")
      )
   )

