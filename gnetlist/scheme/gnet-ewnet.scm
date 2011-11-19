;;; gEDA - GPL Electronic Design Automation
;;; gnetlist - gEDA Netlist
;;; Ultiboard (ewnet) backend
;;; Copyright (C) 2011 Dan McMahill
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


;;; Notes about Ultiboard (.ewnet) netlists.  "ew" 
;;; stands for Electronic Workbench.
;;;  

;;; FIXME -- the biggest problem with this backend is that I
;;; had no documentation at all on the file format.  I just
;;; found a .ewnet file online somewhere, read it, and guessed
;;; as to the right way to create these.  Full documentation would
;;; help considerably!

;; Function:  ewnet:map-net-names
;;
;; This procedure takes a net name as determined by gnetlist and
;; modifies it to be a valid ewnet net name.
;;
;; FIXME -- need to determine what restrictions there may be on 
;; ewnet net names.  For example:
;;   - allowed characters
;;   - case sensitive or not?
;;   - max length
;;
;; See the futurenet2 backend for examples of some of what may go
;; into a net name aliasing function.  For now this function just returns
;; the input string and acts as a placeholder in case we find
;; that net name mapping is required.
(define ewnet:map-net-names
  (lambda (net-name)
    (let ((net-alias net-name)
          )
      net-alias
      )
    )
  )

;; Function:  ewnet:map-refdes
;;
;; This procedure takes a refdes as determined by gnetlist and
;; modifies it to be a valid ewnet refdes.
;;
;; FIXME -- need to determine what restrictions there may be on 
;; ewnet instance names (refdes).  For example:
;;   - allowed characters
;;   - case sensitive or not?
;;   - max length
;;
;; See the futurenet2 backend for examples of some of what may go
;; into a instance name aliasing function.  For now this function 
;; just returns the input string and acts as a placeholder in
;; case we find that instance name mapping is required.
(define ewnet:map-refdes
  (lambda (refdes)
    (let ((refdes-alias refdes)
          )
      
      refdes-alias
      )
    )
  )

;; write out the pins for a particular component
(define ewnet:component_pins
  (lambda (port package pins)
    (if (and (not (null? package)) (not (null? pins)))
	(begin
	  (let (
		(pin (car pins)))

	    ;; pin number
	    (display "\t\t(pin \"" port)
	    (display 
	     (gnetlist:get-attribute-by-pinnumber package pin "pinnumber")
	     port)
	    (display "\"\n" port)

	    ;; net
	    (display "\t\t\t(net \"" port)
	    (display 
	     (gnetlist:alias-net (car (gnetlist:get-nets package pin)))
	     port)
	    (display "\")\n" port)

	    ;; pin type.  I have seen "PWR", "GND", "IN", "OUT", "BIDIR"
	    (display "\t\t\t(pintype \"" port)
	    ;; FIXME -- need to translate between geda and the allowed
	    ;; ewnet types here
	    (display "BIDIR" port)
	    (display "\")\n" port)
	    
	    (display "\t\t\t(gategroup \"\")\n" port)
	    (display "\t\t\t(pingroup \"\")\n" port)

	    ;; label (pin name)
	    (display "\t\t\t(label \"" port)
	    (display pin port)
	    (display "\")\n" port)

	    ;; gate
	    (display "\t\t\t(gate \"\")\n" port)
	    

	    (display "\t\t)\n" port)
	    )
	  (ewnet:component_pins port package (cdr pins))
	  )
	)
    )
  )

	    
;; write out the components
(define ewnet:components
   (lambda (port packages)
      (if (not (null? packages))
         (begin
            (let (
		  (device (gnetlist:get-package-attribute (car packages) 
                                                           "device"))
		  (pattern (gnetlist:get-package-attribute (car packages) 
                                                           "pattern"))
		  (value (gnetlist:get-package-attribute (car packages) 
                                                           "value"))
	    ;; The above pattern should stay as "pattern" and not "footprint"
                  (package (car packages)))

	      ;; start the instance
	      (display "\t(instance \"" port)

	      ;; write the footprint
	      (display (gnetlist:get-package-attribute package
						       "footprint")
		       port)
	      (display "\" \"" port)

	      ;; write the reference designator
	      (display (gnetlist:alias-refdes package) port)
	      (display "\"\n" port)
	      

	      ;; device 
	      (display "\t\t(device \"" port)
	      (display device port)
	      (display "\")\n" port)

	      ;; If there is a "value" attribute, output that.
	      ;; Otherwise output the "device" attribute (the symbol name).
	      (if (string=? value "unknown") 
		  (set! value device )
		  )

	      ;; value 
	      (display "\t\t(value \"" port)
	      (display value port)
	      (display "\")\n" port)
	      
	      (display "\t\t(gateswap \"0\")\n" port)
	      (display "\t\t(pinswap \"0\")\n" port)
	      (display "\t\t(component_space \"0.00000000e+000\")\n" port)
	      (display "\t\t(component_group \"\")\n" port)
	      (display "\t\t(settings_locked \"0\")\n" port)
	      (display "\t\t(comp_variants \"Default1;\")\n" port)
	      (display "\t\t(comp_variant_independent \"0\")\n" port)

	      ;; write the pins
	      (ewnet:component_pins port package
					 (gnetlist:get-pins package))

	      ;; close the part
	      (display "\t)\n" port)
	      
	      )
            (ewnet:components port (cdr packages) )
	    )
	 )
      )
   )

;; write out the nets
(define ewnet:write-net
   (lambda (port netnames)
      (if (not (null? netnames))
         (let (
	       (netname (car netnames))
	       (alias (gnetlist:alias-net (car netnames)))
	       )
	   (display "\t( net \"" port)
	   (display alias port)
	   (display "\"\n" port)

	   (display "\t\t(trackwidth \"-1.00000000e+000\")\n" port)
	   (display "\t\t(trackwidth_max \"-1.00000000e+000\")\n" port)
	   (display "\t\t(trackwidth_min \"-1.00000000e+000\")\n" port)
	   (display "\t\t(tracklength_max \"-1.00000000e+000\")\n" port)
	   (display "\t\t(tracklength_min \"-1.00000000e+000\")\n" port)
	   (display "\t\t(clearance_to_trace \"-1.00000000e+000\")\n" port)
	   (display "\t\t(clearance_to_pad \"-1.00000000e+000\")\n" port)
	   (display "\t\t(clearance_to_via \"-1.00000000e+000\")\n" port)
	   (display "\t\t(clearance_to_copper \"-1.00000000e+000\")\n" port)
	   (display "\t\t(routing_layer \"\")\n" port)
	   (display "\t\t(settings_locked \"0\")\n" port)
	   (display "\t\t(net_group \"\")\n" port)
	   (display "\t)\n" port)

	   (ewnet:write-net port (cdr netnames))
	   )
	 )
      )
   )

;; write out the header
(define ewnet:write-header
  (lambda (port)

     (display "(ToolInfo\n" port)
     (display "\t(netlist \"ULTIboard\" 7 0 0)\n" port)
     (display "\t(tool \"Multisim\" 7 0 0)\n" port)
     (display "\t(timestamp \"15:19:8\" \"9-6-2006\")\n" port)
     (display "\t(version 3 0 0)\n" port)
     (display "\t(gateswap 2)\n" port)
     (display "\t(pinswap 1)\n" port)
     (display "\t(crossprobe {D6EE9C01-C93E-4246-9BC1-214A35C4954C})\n" port)
     (display "\t(units Mil)\n" port)
     (display ")\n" port)
     )
)


;; write out the layer information
;; FIXME -- can this just be left out?  If not, how shall we define
;; stackup in a way where gnetlist can find it?
(define ewnet:layers
  (lambda (port)
    
    (display "\t(layer \"Copper Bottom\"\n" port)
    (display "\t\t(routable \"1\")\n" port)
    (display "\t\t(type \"Signal\")\n" port)
    (display "\t)\n" port)
    (display "\t(layer \"Copper Top\"\n" port)
    (display "\t\t(routable \"1\")\n" port)
    (display "\t\t(type \"Signal\")\n" port)
    (display "\t)\n" port)
    )
)

;; The top level netlister for ewnet
(define ewnet 
  (lambda (filename)
    (newline)
    (display "---------------------------------\n")
    (display "gEDA/gnetlist ewnet Backend\n")
    (display "This backend is EXPERIMENTAL\n")
    (display "Use at your own risk!\n")
    (display "\n")
    (display "You may need to run the output netlist\n")
    (display "through unix2dos before importing to\n")
    (display "windows based layout tools\n")
    (display "---------------------------------\n\n")
    
    (let ((port (open-output-file filename))
	  (all-nets (gnetlist:get-all-unique-nets "dummy"))
	  )
      
      ;; initialize the net-name aliasing
      (gnetlist:build-net-aliases ewnet:map-net-names all-unique-nets)
      
      ;; initialize the refdes aliasing
      (gnetlist:build-refdes-aliases ewnet:map-refdes packages)
      
      ;; write the header
      (ewnet:write-header port)
      
      ;; write the nets
      (display "(nets\n" port)
      (ewnet:write-net port all-nets)
      (display ")\n" port)
      
      ;; write the components
      (display "(components\n" port)
      (ewnet:components port packages)
      (display ")\n" port)
      
      ;; write the board layers
      (display "(layers\n" port)
      (ewnet:layers port)
      (display ")\n" port)
      
      ;; close netlist
      (close-output-port port)
      )
    )
  )



