;; gEDA - GNU Electronic Design Automation
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
;;
;; --------------------------------------------------------------------------
;;
;; SPICE netlist backend written by S. Gieltjes starts here
;;
;; further modified by W. Kazubski to use scaling parameters for devices 
;; other than MOSFETS
;;
;;----------------------------------------------------------------------
;;
;; Started with gnet-spice1.scm by W. Kazubski.  Radically 
;; hacked by SDB to support advanced spice netlist generation.
;; Project started 3.5.2003 -- SDB.    
;;
;; Details and documentation at http://www.brorson.com/gEDA/SPICE/
;;
;;  Change log:
;;  3.5.2003 -- Started hacking.  SDB.
;;  3.17.2003 -- 2nd version.  Hacked to allow for .SUBCKT files to model ics.
;;               Changed write-ic.  Added get-file-type.  Added 
;;               write-subcircuit.  SDB.
;;  3.31.2003 -- 3rd version.  Hacked to enable creating .SUBCKT schematics for
;;               hierarchical circuit modeling.
;;  8.29.2003 -- 4th version.  Include patches from Ken Healy to sort netlist, 
;;               code by SDB to use gnetlist command line args in Scheme fcns,
;;               as well as from Theo Deckers to fix strange problem with '.SUBCKT 
;;               quoting.
;;

;;-----------------------------------------------------------
;; gnet-spice replacement of gnetlist:get-nets, a net labeled "GND" becomes 0
;;-----------------------------------------------------------
(define spice-sdb:get-net
  (lambda (refdes pin-name)

;;  --- debug stuff ---
;;    (display (string-append "in get-net, pin-name = " pin-name "\n"))
;;  -------------------

    (let ((net-name (gnetlist:get-nets refdes pin-name)))
      (cond ((string=? (car net-name) "GND") (cons "0" #t))
	    (else                            (cons (car net-name) #t)))
    )
  )
)

;;---------------------------------------------------------------------
;; write netnames connected to pin-a and pin-b
;;   (currently used by the controlled sources (e, g, f and h)
;;---------------------------------------------------------------------
(define spice-sdb:write-two-pin-names
  (lambda (package pin-a pin-b port)
    (display (string-append 
      (car (spice-sdb:get-net package (gnetlist:get-attribute-by-pinseq package pin-a "pinnumber"))) " ") port)
    (display (string-append 
      (car (spice-sdb:get-net package (gnetlist:get-attribute-by-pinseq package pin-b "pinnumber"))) " ") port)))


;;----------------------------------------------------------------
;; write a current controlled voltage source and implement the necessary
;;   current measuring voltage source
;;----------------------------------------------------------------
(define spice-sdb:write-ccvs
  (lambda (package port)
    ( begin
      (display "* begin ccvs expansion, h<name>\n" port)
          ;; implement the controlled current source
          ;; the user should create the refdes label begining with a h
      (display (string-append package " ") port)
      (spice-sdb:write-two-pin-names package "1" "2" port)
      (display (string-append "v-sense-" package  " " (spice-sdb:component-value package) "\n" ) port)
          ;; implement the current measuring voltage source
      (display (string-append "v-sense-" package " ") port)
      (spice-sdb:write-two-pin-names package "3" "4" port)
      (display "dc 0\n" port)
          ;; now it is possible to leave the output voltage source unconnected
          ;; i.e. spice won't complain about unconnected nodes
      (display (string-append "i-out-" package " ") port)
      (spice-sdb:write-two-pin-names package "1" "2" port)
      (display "dc 0\n" port)
      (display "* end ccvs expansion\n" port))))


;;-----------------------------------------------------------------------
;; write a current controlled current source and implement the necessary
;;   current measuring voltage source
;;-----------------------------------------------------------------------
(define spice-sdb:write-cccs
  (lambda (package port)
    ( begin
      (display "* begin cccs expansion, f<name>\n" port)
          ;; implement the controlled current source
          ;; the user should create the refdes label begining with a f
      (display (string-append package " ") port)
      (spice-sdb:write-two-pin-names package "1" "2" port)
      (display (string-append "v-sense-" package " " (gnetlist:get-package-attribute package "value") "\n" ) port)
          ;; implement the current measuring voltage source
      (display (string-append "v-sense-" package " ") port)
      (spice-sdb:write-two-pin-names package "3" "4" port)
      (display "dc 0\n" port)
      (display "* end cccs expansion\n" port))))


;;-------------------------------------------------------------------------
;; write a voltage controlled current source and implement the necessary
;;   voltage measuring current source
;;-------------------------------------------------------------------------
(define spice-sdb:write-vccs
  (lambda (package port)
    ( begin
      (display "* begin vccs expansion, g<name>\n" port)
          ;; implement the controlled current source
          ;; the user should create a refdes label beginning with a g
      (display (string-append package " ") port)
      (spice-sdb:write-net-name-of-component package (length (gnetlist:get-pins package)) port)
       (display  (string-append (spice-sdb:component-value package) "\n")  port)
          ;; implement the voltage measuring current source
          ;; imagine yourself copying the voltage of a voltage source with an internal
          ;; impedance, spice starts complaining about unconnected nets if this current
          ;; source is not here.
      (display (string-append "i-measure-" package " ") port)
      (spice-sdb:write-two-pin-names package "3" "4" port)
      (display "dc 0\n" port)
      (display "* end vccs expansion\n" port))))


;;------------------------------------------------------------------------
;; write a voltage controlled voltage source and implement the necessary
;;   voltage measuring current source
;;------------------------------------------------------------------------
(define spice-sdb:write-vcvs
  (lambda (package port)
    ( begin
      (display "* begin vcvs expansion, e<name>\n" port)
          ;; implement the controlled voltage source
          ;; the user should create a refdes label beginning with an e
      (display (string-append package " ") port)
      (spice-sdb:write-net-name-of-component package (length (gnetlist:get-pins package)) port)
      (display (string-append (gnetlist:get-package-attribute package "value") "\n" ) port)
          ;; implement the voltage measuring current source
          ;; imagine yourself copying the voltage of a voltage source with an internal
          ;; impedance, spice starts complaining about unconnected nets if this current
          ;; source is not here.
      (display (string-append "i-sense-" package " ") port)
      (spice-sdb:write-two-pin-names package "3" "4" port)
      (display "dc 0\n" port)
          ;; with an output current source it is possible to leave the output voltage source
          ;; unconnected i.e. spice won't complain about unconnected nodes
      (display (string-append "i-out-" package " ") port)
      (spice-sdb:write-two-pin-names package "1" "2" port)
      (display "dc 0\n" port)
      (display "* end vcvs expansion\n" port))))


;;--------------------------------------------------------------------------
;; Create a nullor, make sure it consists of a voltage controlled source
;;--------------------------------------------------------------------------
(define spice-sdb:write-nullor
  (lambda (package port)
    ( begin
      (display "* begin nullor expansion, e<name>\n" port)
          ;; implement the controlled voltage source
      (display (string-append "E-" package " ") port)
      (spice-sdb:write-net-name-of-component package (length (gnetlist:get-pins package)) port)
      (display (string-append (gnetlist:get-package-attribute package "value") "\n" ) port)
          ;; implement the voltage measuring current source
          ;; imagine yourself copying the voltage of a voltage source with an internal
          ;; impedance, spice starts complaining about unconnected nets if this current
          ;; source is not here.
      (display (string-append "i-measure-" package " ") port)
      (spice-sdb:write-two-pin-names package "3" "4" port)
      (display "dc 0\n" port)
          ;; with an output current source it is possible to leave the output voltage source
          ;; unconnected i.e. spice won't complain about unconnected nodes
      (display (string-append "i-out-" package " ") port)
      (spice-sdb:write-two-pin-names package "1" "2" port)
      (display "dc 0\n" port)
      (display "* end of nullor expansion\n" port))))

;;----------------------------------------------------------
;; Given a filename, open the file, get the contents, and dump them
;; into the spice file.
;; Calling form is "(insert-text-file input-file output-file)"
;; The function opens input-file, but assumes that output-file is
;; already open. 
;;
;; This function is usually used to include spice models contained in 
;; files into the netlist.  Note that it doesn't
;; check the correctness of the spice code in the file.
;;----------------------------------------------------------
(define insert-text-file
  (lambda (model-filename port)
    (let ((model-file (open-input-file model-filename)) )
      (let while ((model-line (read-line model-file)))
	  (if (not (eof-object? model-line))
		   (begin
		     (display (string-append model-line "\n") port)
		     ;; (display (string-append "-- model-line = " model-line "\n")) ;; debug statement
		     (while (read-line model-file))
		   )  ;; end of inner begin
	  ) ;; end of if
	)  ;; end of inner let
        (close-port model-file)
     ) ;; end of outer let
  )
)

;;----------------------------------------------------------
;; Figure out if this schematic is a .SUBCKT lower level.
;; This is determined if there is a spice-subcircuit-LL  
;; device instantiated somewhere on the schematic.
;; If it is a .SUBCKT, return ".SUBCKT model-name"
;;----------------------------------------------------------
(define spice-sdb:get-schematic-type
  (lambda (ls)     
     (if (not (null? ls))
      (let* ((package (car ls))             ;; assign package
	     (device (get-device package))  ;; assign device.    
	    )                               ;; end of let* assignments
	(begin 
	  ;; (display (string-append "in get-schematic-type, device = " device "\n"))
	  (if (string=? device "spice-subcircuit-LL")  ;; look for subcircuit label
	      (string-append ".SUBCKT " (gnetlist:get-package-attribute package "model-name"))
	      (spice-sdb:get-schematic-type (cdr ls))  ;; otherwise just iterate to next package.
	  )
	)
      )    ; end of let*
      "normal schematic"   ; return "normal schematic" if no spice-subcircuit-LL is found
    )    ; end of if 
))


;;----------------------------------------------------------
;; Extract the modelname from the .SUBCKT modelname line.
;; Just grab the chars from char 8 to the end of the string.
;;---------------------------------------------------------
(define spice-sdb:get-subcircuit-modelname
  (lambda (schematic-type)
    (substring schematic-type 8 (string-length schematic-type))
  )
)


;;-----------------------------------------------------------
;;  This iterates through the schematic and compiles a list of
;;  all spice-IO pins found.  This is used when writing out
;;  a .SUBCKT lower level netlist.
;;-----------------------------------------------------------
(define spice-sdb:get-spice-IO-pins
  (lambda (ls spice-io-package-list)
    (if (null? ls)

	spice-io-package-list        ;; end iteration & return list if ls is empty.

	(let* ((package (car ls))    ;; otherwise process package. . .     
	       (device (get-device package))  
	      )
	   (if (string=? device "spice-IO")  ;; look for subcircuit label

	       ;; we have found a spice-IO pin.
	       (spice-sdb:get-spice-IO-pins (cdr ls) (cons package spice-io-package-list))

	       ;; no spice-IO pin found.  Iterate . . . .
	       (spice-sdb:get-spice-IO-pins (cdr ls) spice-io-package-list)

	   ) ;; end of if string=?

	)  ;; end of let*
   ) ;; end if null
 )
)

;;----------------------------------------------------------------
;;  This takes the list of io-pin-packages and sorts it in order of 
;;  refdes.
;;----------------------------------------------------------------
(define spice-sdb:sort-spice-IO-pins 
  (lambda (package-list)
    (sort package-list string<?)
  )
)


;;----------------------------------------------------------------
;;  Given a list of spice-IO packages (refdeses), this function returns the list
;;  of nets attached to the IOs.
;;----------------------------------------------------------------
(define spice-sdb:get-IO-nets
  (lambda (package-list net-list)
    (if (null? package-list)

	net-list        ;; end iteration & return net-list if ls is empty.

	(let* ((package (car package-list))                  ;; otherwise process package. . .     
	       (net (car (gnetlist:get-nets package "1")))   ;; get the net attached to pin 1
	      )
	 ;; now iterate
	  (spice-sdb:get-IO-nets (cdr package-list) (cons net net-list))
	)
    ) ;; end of if
  )
)


;;----------------------------------------------------------
;;  This takes a list and turns it into a string.
;;  The difference between this and list->string is that
;;  this fun can handle lists made up of multi-char strings.
;;----------------------------------------------------------
(define list-2-string
  (lambda (ls st)
    (if (null? ls)

	st        ;; end iteration & return string if list is empty.

	(let* ((element (car ls))  ;; otherwise turn next element of list into string. . .     
	      )
	 ;; now iterate
	  (list-2-string (cdr ls) (string-append element " " st))
	)

    ) ;; end of if
  )
)


;;----------------------------------------------------------
;; Given a filename, open the file, get the first line,
;; and see if it is a .MODEL or .SUBCKT file.  
;; Returns either ".MODEL" or ".SUBCKT" or "OTHER"
;; Calling form is "(spice-sdb:get-file-type input-file)"
;; The function opens input-file, and closes it when it is done.
;;----------------------------------------------------------
(define spice-sdb:get-file-type
  (lambda (model-filename)
    (let ((model-file (open-input-file model-filename)) )
      (let while ((file-line (read-line model-file)) )
	
	;; debug display . . .
	;; (display (string-append "In get-file-type, file-line = " file-line "\n"))
	
	(cond
	 ((eof-object? file-line)
	  (begin
	    ;; (display "In get-file-type, found eof-object before .MODEL or .SUBCKT\n")
	    ('OTHER)
	  )
	 )
	     
	 ;;((string-null? file-line)
	 ;; (begin
	 ;;   (display "In get-file-type, first-char = <null> \n")
	 ;;   (while (read-line model-file))
	 ;; )
	 ;;)
	   
	 ((string=? (string (string-ref file-line 0)) "*")
	  (begin
	    ;; (display "In get-file-type, first-char = *\n")
	    (while (read-line model-file))
	  )
	 )
	   
	 ((string=? (string (string-ref file-line 0)) ".")
	  (begin
	    ;; (display "In get-file-type, first-char = .\n")
	    (cond
	     ((or 
	       (string=? (substring file-line 0 7) ".SUBCKT")
	       (string=? (substring file-line 0 7) ".subckt")
	      )
	      ;; (display (string-append "Found ., type = " (substring file-line 0 7) "\n"))
	      ".SUBCKT"
	     )

	     ((or 
	       (string=? (substring file-line 0 6) ".MODEL")
	       (string=? (substring file-line 0 6) ".model")
	      )
	      ;; (display (string-append "Found ., type = " (substring file-line 0 6) "\n"))
	      ".MODEL"
	     )
	   
	    ) ; inner cond
	  ) ; inner begin
	 )

	 (else
	  (begin
	    ;; (display "In get-file-type, first-char = <other>\n")
	    (while (read-line model-file))
	    )
	  )

	) ; outer cond

       ) ;; end of inner lets
      ) ;; end of outer let
  )
) ;; end define


;;-------------------------------------------------------------------
;; write all listed and available attributes in the form of <variable>=<value>
;;-------------------------------------------------------------------
(define spice-sdb:write-list-of-attributes
  (lambda (package attrib-list port)
    (if (not (null? attrib-list))
      (begin
            ; Is it possible to make no differentiation between upper and lower case?
            ; That relieves you of mixed case forms e.g. As, AS, as..., they are the
            ; same attributes, spice3f5 is case insensitive.  And other spice versions?
        (if (not (string=? (gnetlist:get-package-attribute package (car attrib-list)) "unknown"))
          (display (string-append  " " (car attrib-list) "="
                               (gnetlist:get-package-attribute package (car attrib-list))) port))
        (spice-sdb:write-list-of-attributes package (cdr attrib-list) port)))))


;;---------------------------------------------------------------
;;  write prefix if first char of refdes is improper,
;;  eg. if MOSFET is named T1 then becomes MT1 in SPICE
;;---------------------------------------------------------------
(define spice-sdb:write-prefix
    (lambda (package prefix port)
	(if (not (string=? (substring package 0 1) prefix))
	    (display prefix port))))



;;----------------------------------------------------------------
;;
;; Write-transistor-diode: writes out component followed by 
;; model or model file associated
;; with the component.
;;  This function does the following:
;;   1.  Assumes that the refdes has had the correct refdes prefix written out 
;;       by the calling function (if necessary). 
;;   2.  Writes out the refdes and nets 
;;   3.  Looks for "model-name" attribute. Writes it out if it exists.
;;   4.  If there is no "model-name" attribute, it writes out the "value"
;;       attribute.  If there is no "value" attribute, it writes out "unknown"
;;       and returns, causing the spice simulator to puke when the netlist 
;;       is run.  This is important 
;;       'cause the spice simulator needs to have some indication of what
;;       model to look for.
;;   5.  Outputs a new line
;;   6.  Looks for a the "model" attribute.  If it exists, it it writes out
;;       a .MODEL line like this:  .MODEL model-name type (model)
;;   7.  If there is no "model" attribute, it looks for a "file"
;;       attribute.  If it exists, it opens up the corresponding file
;;       and dumps the contents into the netlist.  (Note that the 
;;       model file must hold all the correct code and attributes 
;;       to work with the netlist.)
;;      
;;----------------------------------------------------------------
(define spice-sdb:write-transistor-diode
  (lambda (package type port)
    
            ;; Here we write out the refdes and nets.  
    (spice-sdb:write-component-no-value package port)

             ;; First do local assignments
    (let ((model-name (gnetlist:get-package-attribute package "model-name"))
	  (model (gnetlist:get-package-attribute package "model"))
	  (value (gnetlist:get-package-attribute package "value"))
	  (model-file (gnetlist:get-package-attribute package "file"))
	 )   ;; end of local assignments

;; -------- Debug stuff ------------
;;      (display (string-append "### in write-trans-diode, package = " package "\n"))
;;      (display (string-append "### in write-trans-diode, model-name = " model-name "\n"))
;;      (display (string-append "### in write-trans-diode, model = " model "\n"))
;;      (display (string-append "### in write-trans-diode, value = " value "\n"))
;;      (display (string-append "### in write-trans-diode, model-file = " model-file "\n"))
;; ---------------------------------

             ;; next look for "model-name" attribute.  Write it out if it exists.
             ;; otherwise look for "device" attribute.  
        (if (not (string=? model-name "unknown"))
	    (display (string-append model-name " " ) port)  ;; display model-name if known 
	    (display (string-append value " ") port))       ;; otherwise display device

	   (newline port)

	     ;; Now write out any model which is pointed to by the part.
	(cond
	     ;; one line model and model name exist
	 ( (not (or (string=? model "unknown") (string=? model-name "unknown")))
	   (display (string-append "found model and model-name for " package "\n"))  ;;  ******* Debug stuff
	   (display (string-append ".MODEL " model-name " " type " \(" model "\)\n") port) )

             ;; one line model and component value exist
	 ( (not (or (string=? model "unknown") (string=? value "unknown")))
	   (display (string-append "found model and value for " package "\n"))  ;;  ******* Debug stuff
	   (display (string-append ".MODEL " model-name " " type " \(" value "\)\n") port) )

             ;; model file and model name exist
	 ( (not (or (string=? model-file "unknown") (string=? model-name "unknown")))
	   (display (string-append "found file and model-name for " package "\n"))  ;;  ******* Debug stuff
	   (insert-text-file model-file port) 
	 )

             ;; model file and component value exist
	 ( (not (or (string=? model-file "unknown") (string=? value "unknown")))
	   (display (string-append "found file and value for " package "\n"))  ;;  ******* Debug stuff
	   (insert-text-file model-file port) 
	 )

	 )  ;; close of cond
	)
    )
)


;;----------------------------------------------------------------
;;  write diode
;;  This writes out a valid diode refdes & then calls
;;  the function which writes the rest of the line.
;;----------------------------------------------------------------
(define spice-sdb:write-diode
  (lambda (package port)
    (display (string-append "Found diode.  Refdes = " package "\n"))
    (spice-sdb:write-prefix package "D" port)  ;; this appends a "D" to the refdes 
                                            ;; if required.
    (spice-sdb:write-transistor-diode package "D" port)))  


;;----------------------------------------------------------------
;;  write ic
;;  This writes out a valid ic refdes & then calls
;;  the function which writes the rest of the line.
;;  The algorithm is as follows:
;;  1. Get attributes attached to ic.  Then, depending upon which attributes
;;     are attached, do the following:
;;     -- One line model and (model-name or value) exist: 
;;        write out the refdes, nets, and
;;        model-name.  Then on next line output .MODEL model-name model.
;;     -- file and (model-name or value) exist:
;;        Open up model file, and see if it is a .MODEL or a .SUBCKT.
;;        -- If it's a .MODEL, write out refdes, nets, and model-name.  On
;;           next lines, output model file.
;;        -- If it's a .SUBCKT, write out an 'X' in front of the refdes.  Then
;;           write out refdes, nets, and model-name.  On next lines, output
;;           .SUBCKT
;;     -- no model or file attributes:  Just output refdes, nets, and (model-name or value).
;;
;;----------------------------------------------------------------
(define spice-sdb:write-ic
  (lambda (package port)
    (display (string-append "Found ic.  Refdes = " package "\n"))

             ;; First do local assignments
    (let ((model-name (gnetlist:get-package-attribute package "model-name"))
	  (model (gnetlist:get-package-attribute package "model"))
	  (value (gnetlist:get-package-attribute package "value"))
	  (model-file (gnetlist:get-package-attribute package "file"))
	 )   ;; end of local assignments

;; -------- Debug stuff ------------
;;      (display (string-append "### in write-ic, package = " package "\n"))
;;      (display (string-append "### in write-ic, model-name =  model-name "\n"))
;;      (display (string-append "### in write-ic, model = " model "\n"))
;;      (display (string-append "### in write-ic, value = " value "\n"))
;;      (display (string-append "### in write-ic, model-file = " model-file "\n"))
;; ---------------------------------
    
      (cond
       ;; ----- one line model and (model-name or value) exist -----
       ( (not (string=? model "unknown"))
	 (begin
	  (spice-sdb:write-component-no-value package port)
	  (if (not (string=? model-name "unknown"))
	      (begin
	       (display (string-append "found one-line model and model-name for " package "\n"))
	       (display (string-append model-name " " ) port)) 
	      (begin
	       (display (string-append "found one-line model and value for " package "\n"))
	       (display (string-append value " ") port)))
	  (newline port)
	  (display (string-append ".MODEL " model-name " " type " \(" model "\)\n") port)
	 )
       )

       ;; -----  model-file and (model-name or value) exist -----
       ( (not (string=? model-file "unknown"))
	 (begin
	  ;; first check to see if model-file starts with .MODEL or with .SUBCKT
	  (let ((file-type (spice-sdb:get-file-type model-file)) )

	    (cond 
	 ;; ---- file holds a model ----
	     ((string=? file-type ".MODEL") 
	      (begin
		(spice-sdb:write-component-no-value package port)
		(if (not (string=? model-name "unknown"))
		    (begin
		     (display (string-append "Found .MODEL with model-file and model-name for " package "\n"))
		     (display (string-append model-name " " ) port))
		    (begin
		     (display (string-append "Found .MODEL with model-file and value for " package "\n"))
		     (display (string-append value " ") port)))
		(newline port)
		(insert-text-file model-file port) 
	      )
	     ) ; end of string=? .model

	 ;; ---- file holds a subcircuit ----
	     ((string=? file-type ".SUBCKT") 
	      (begin
		(spice-sdb:write-prefix package "X" port)  ;; this appends an "X" to the refdes
		(spice-sdb:write-component-no-value package port)
		(if (not (string=? model-name "unknown"))
		    (begin
		     (display (string-append "Found .SUBCKT with model-file and model-name for " package "\n"))
		     ;; (display (string-append "model-name = " model-name " " ))
		     (display (string-append model-name " " ) port))
		    (begin
		     (display (string-append "Found .SUBCKT with model-file and value for " package "\n"))
		     (display (string-append value " ") port)))
		(newline port)
		(insert-text-file model-file port) 
	      )
	     ) ; end of string=? .subckt
	   )  ;; close of inner cond


	 ) ;; end of let
	) ;; end of begin
       )

       ;; ----- else just write out what little we know -----
       ( else
	 (begin
	  (spice-sdb:write-component-no-value package port)
	  (if (not (string=? model-name "unknown"))
	      (begin
	       (display (string-append "outputting only model-name for " package "\n"))
	       (display (string-append model-name " " ) port)) 
	      (begin
	       (display (string-append "outputting only value for " package "\n"))
	       (display (string-append value " ") port)))
	  (newline port)
	 )
       )


      ) ;; end of cond
  ) ;; end of let
 )
)



;;----------------------------------------------------------------
;;  write-subcircuit
;;  This writes out a valid subcircuit refdes & then calls
;;  the function which writes the rest of the line.
;;  The algorithm is as follows:
;;  1. Get attributes attached to subcircuit.  Then, depending upon which attributes
;;     are attached, do the following:
;;     -- file and (model-name or value) exist: 
;;        Output refdes, nets, and value. Then open up file & write it out.
;;     -- no file attribute:  Just output refdes, nets, and (model-name or value).
;;----------------------------------------------------------------
(define spice-sdb:write-subcircuit
  (lambda (package port)
    (display (string-append "Found .subcircuit.  Refdes = " package "\n"))

             ;; First do local assignments
    (let ((model-name (gnetlist:get-package-attribute package "model-name"))
	  (model (gnetlist:get-package-attribute package "model"))
	  (value (gnetlist:get-package-attribute package "value"))
	  (model-file (gnetlist:get-package-attribute package "file"))
	 )   ;; end of local assignments

;; -------- Debug stuff ------------
;;      (display (string-append "### in write-subcircuit, package = " package "\n"))
;;      (display (string-append "### in write-subcircuit, model-name =  model-name "\n"))
;;      (display (string-append "### in write-subcircuit, model = " model "\n"))
;;      (display (string-append "### in write-subcircuit, value = " value "\n"))
;;      (display (string-append "### in write-subcircuit, model-file = " model-file "\n"))
;; ---------------------------------
    
      (cond

       ;; -----  model-file and (model-name or value) exist -----
       ( (not (string=? model-file "unknown"))
	 (begin
	  ;; first check to see if model-file starts with .MODEL or with .SUBCKT
	  (let ((file-type (spice-sdb:get-file-type model-file)) )

	    (cond 
	 ;; ---- file holds a model ----
	     ((string=? file-type ".MODEL") 
	      (begin
		(spice-sdb:write-prefix package "U" port)  ;; this appends a "U" to the refdes
		(spice-sdb:write-component-no-value package port)
		(if (not (string=? model-name "unknown"))
		    (begin
		     (display (string-append "Found .MODEL with model-file and model-name for " package "\n"))
		     (display (string-append model-name " " ) port))
		    (begin
		     (display (string-append "Found .MODEL with model-file and value for " package "\n"))
		     (display (string-append value " ") port)))
		(newline port)
		(insert-text-file model-file port) 
	      )
	     ) ; end of string=? .model

	 ;; ---- file holds a subcircuit ----
	     ((string=? file-type ".SUBCKT") 
	      (begin
		(spice-sdb:write-component-no-value package port)
		(if (not (string=? model-name "unknown"))
		    (begin
		     (display (string-append "Found .SUBCKT with model-file and model-name for " package "\n"))
		     ;; (display (string-append "model-name = " model-name " " ))
		     (display (string-append model-name " " ) port))
		    (begin
		     (display (string-append "Found .SUBCKT with model-file and value for " package "\n"))
		     (display (string-append value " ") port)))
		(newline port)
		(insert-text-file model-file port) 
	      )
	     ) ; end of string=? .subckt

	   )  ;; close of inner cond

	 ) ;; end of let
	) ;; end of begin
       )

       ;; ----- else just write out what little we know -----
       ( else
	 (begin
	  (spice-sdb:write-component-no-value package port)
	  (if (not (string=? model-name "unknown"))
	      (begin
	       (display (string-append "outputting only model-name for " package "\n"))
	       (display (string-append model-name " " ) port)) 
	      (begin
	       (display (string-append "outputting only value for " package "\n"))
	       (display (string-append value " ") port)))
	  (newline port)
	 )
       )


      ) ;; end of cond
  ) ;; end of let
 )
)

;;-----------------------------------------------------------
;;  write npn bipolar transistor
;;  This writes out a valid transistor refdes & then calls
;;  the function which writes the rest of the line.
;;-----------------------------------------------------------
(define spice-sdb:write-npn-bipolar-transistor
  (lambda (package port)

    (display (string-append "Found npn bipolar transistor.  Refdes = " package "\n"))
    (spice-sdb:write-prefix package "Q" port)
    (spice-sdb:write-transistor-diode package "NPN" port))
)


;;-----------------------------------------------------------
;;  write pnp bipolar transistor
;;-----------------------------------------------------------
(define spice-sdb:write-pnp-bipolar-transistor
  (lambda (package port)
    (display (string-append "Found pnp bipolar transistor.  Refdes = " package "\n"))
    (spice-sdb:write-prefix package "Q" port)
    (spice-sdb:write-transistor-diode package "PNP" port)))


;;-----------------------------------------------------------
;;  write n-channel jfet transistor
;;-----------------------------------------------------------
(define spice-sdb:write-jfet-transistor
  (lambda (package port)
    (spice-sdb:write-prefix package "J" port)
    (spice-sdb:write-transistor-diode package "NJF" port))
)

;;-----------------------------------------------------------
;;  write p-channel jfet transistor
;;-----------------------------------------------------------
(define spice-sdb:write-jfet-transistor
  (lambda (package port)
    (spice-sdb:write-prefix package "J" port)
    (spice-sdb:write-transistor-diode package "PJF" port))
)


;;------------------------------------------------------
;;  write mos transistor
;;------------------------------------------------------
;; *********  Fix this!  *****************
(define spice-sdb:write-mos-transistor
  (lambda (package port)
    (spice-sdb:write-prefix package "M" port)
    (spice-sdb:write-component-no-value package port)
            ;; write component model, if components have a label "model=#"
	    ;; what if a component has no model label, currently unknown is written
    (display (spice-sdb:component-model package) port)
            ;; create list of attributes which can be attached to a mosfet
    (let ((attrib-list (list "l" "w" "as" "ad" "pd" "ps" "nrd" "nrs" "temp" "ic")))
      (spice-sdb:write-list-of-attributes package attrib-list port))
            ;; write the off attribute separately
    (let ((off-value (gnetlist:get-package-attribute package "off")))
      (cond ((string=? off-value "#t") (display " off" port))
            ((string=? off-value "1" ) (display " off" port))))
    (newline port)))



;;------------------------------------------------------------
;;  write mesfet transistor
;;------------------------------------------------------------
;; ************  Fix this!!!!!!!!!!  **************
(define spice-sdb:write-mesfet-transistor
  (lambda (package port)
    (spice-sdb:write-prefix package "Z" port)
    (spice-sdb:write-transistor-diode package port)))  ;; XXXXXX Fix this!!!


;;--------------------------------------------------------------------
;;  write resistor
;;--------------------------------------------------------------------
(define spice-sdb:write-resistor
  (lambda (package port)

    (display (string-append "Found resistor.  Refdes = " package "\n"))

            ;; first write out refdes and attached nets
    (spice-sdb:write-component-no-value package port) 

;;            ;; next write our model name if it exists
;;    ---- Note that this stuff is applicable to PSpice, but not to our spice. ----
;;    (let* ((model-name (gnetlist:get-package-attribute package "model-name")))
;;        (if (not (string=? model-name "unknown")) 
;;		(display (string-append model-name " " ) port))
;;    )

	    ;; next write out mandatory resistor value
    (let ((value (gnetlist:get-package-attribute package "value")))
	(display value port))

	    ;; next create list of attributes which can be attached to a resistor
    (let ((attrib-list (list "l" "w" "temp")))
	    ;; write the attributes (if any) separately
      (spice-sdb:write-list-of-attributes package attrib-list port)
      (display " " port))  ;; add additional space. . . . 

           ;; finally output a new line
    (newline port)
    )
)


;;----------------------------------------------------------------------------
;;  write capacitor
;;----------------------------------------------------------------------------
(define spice-sdb:write-capacitor
  (lambda (package port)

    (display (string-append "Found capacitor.  Refdes = " package "\n"))

            ;; first write out refdes and attached nets
    (spice-sdb:write-component-no-value package port)

;;            ;; next write capacitor model name, if any.
;;   ----- Note that this stuff is applicable to PSpice, but not our spice. -----
;;   (let ((model-name (gnetlist:get-package-attribute package "model-name")))
;;        (if (not (string=? model-name "unknown"))
;;		(display (string-append model-name " " ) port))
;;   )

            ;; next write capacitor value, if any.  Note that if the 
	    ;; component value is not assigned, then it will write "unknown"
    (let ((value (gnetlist:get-package-attribute package "value")))
		(display value port)
    )


            ;; create list of attributes which can be attached to a capacitor
    (let ((attrib-list (list "l" "w" "ic")))
      (spice-sdb:write-list-of-attributes package attrib-list port)
            ;; write the off attribute separately
		(display " " port))  ;; add additional space. . . . 

    (newline port)
  )
)


;;----------------------------------------------------------------------------
;;  write inductor
;;----------------------------------------------------------------------------
(define spice-sdb:write-inductor
  (lambda (package port)

    (display (string-append "Found inductor.  Refdes = " package "\n"))

            ;; first write out refdes and attached nets
    (spice-sdb:write-component-no-value package port)

;;            ;; next write inductor model name, if any.
;;    (let ((model-name (gnetlist:get-package-attribute package "model-name")))
;;        (if (not (string=? model "unknown"))
;;		(display (string-append model-name " " ) port)
;;    )

            ;; next write inductor value, if any.  Note that if the 
	    ;; component value is not assigned, then it will write "unknown"
    (let ((value (gnetlist:get-package-attribute package "value")))
		(display value port)
    )


            ;; create list of attributes which can be attached to a inductor
    (let ((attrib-list (list "l" "w" "ic")))
      (spice-sdb:write-list-of-attributes package attrib-list port)
            ;; write the off attribute separately
		(display " " port))  ;; add additional space. . . . 

    (newline port)
  )
)


;;-------------------------------------------------------------------------
;;  write independent voltage source
;;  The behavior of the voltage source is held in the "value" attribute
;;-------------------------------------------------------------------------
(define spice-sdb:write-independent-voltage-source
  (lambda (package port)

    (display (string-append "Found independent voltage source.  Refdes = " package "\n"))

            ;; first write out refdes and attached nets
    (spice-sdb:write-component-no-value package port)

            ;; next write voltage value, if any.  Note that if the 
	    ;; voltage value is not assigned, then it will write "unknown"
    (let ((value (gnetlist:get-package-attribute package "value")))
		(display value port)
    )

    (newline port))
)

;;-------------------------------------------------------------------------
;;  write independent current source
;;  The behavior of the current source is held in the "value" attribute
;;-------------------------------------------------------------------------
(define spice-sdb:write-independent-current-source
  (lambda (package port)

    (display (string-append "Found independent current source.  Refdes = " package "\n"))

            ;; first write out refdes and attached nets
    (spice-sdb:write-component-no-value package port)

            ;; next write current value, if any.  Note that if the 
	    ;; current value is not assigned, then it will write "unknown"
    (let ((value (gnetlist:get-package-attribute package "value")))
		(display value port)
    )

    (newline port))
)


;;--------------------------------------------------------------------
;; Given a refdes, returns the device associated nets(s) ordered by their pin#,
;; what when not defined?
;;      problem is slotted components e.g. ../examples/singlenet_1.sch
;;--------------------------------------------------------------------
(define spice-sdb:write-net-name-of-component
  (lambda (refdes number-of-pin port)
    (if (> number-of-pin 0)
      (begin
            ;; first find pin1 and then start writing the connected net name
        (spice-sdb:write-net-name-of-component refdes (- number-of-pin 1) port)
            ;; generate a pin-name e.g. pin1, pin2, pin3 ...
        (let* ((pin-name (number->string number-of-pin))
	       (pinnumber (gnetlist:get-attribute-by-pinseq refdes pin-name "pinnumber"))
	       )  

;; -------  Debug stuff  --------
;;	  (display "In write-net-name-of-component. . . . \n")
;;	  (display (string-append "     pin-name = " pin-name "\n"))
;;	  (display (string-append "     pinnumber = " pinnumber "\n"))
;; ------------------------------ 

	  (display (car (spice-sdb:get-net refdes pinnumber) ) port)
          (write-char #\space port))
      )
    )
  )
)



;;-----------------------------------------------------------
;; Given a refdes, returns the device attribute "value" as string
;; Used when "value" is a mandatory attribute.
;; Returns "<no valid attribute . . .>" if not available.
;;-----------------------------------------------------------
(define spice-sdb:component-value
  (lambda (package)
    (let ((value (gnetlist:get-package-attribute package "value")))
;;      (display (string-append "in get-package-attribute, value = " value "\n"))
      (if (not (string=? value "unknown"))
        value
        "<No valid value attribute found>"))))


;;------------------------------------------------------------
;; Given a refdes, returns the device attribute "value" as string
;; Used when "value" is an optional attribute.
;; Returns "unknown" if not available.
;;------------------------------------------------------------
(define spice-sdb:component-optional-value
  (lambda (package)
    (let ((value (gnetlist:get-package-attribute package "value")))
      (if (not (string=? value "unknown"))
        (string-append value " ")
        ""))))


;;------------------------------------------------------------
;; Given a refdes, returns the "area" attribute as string
;;------------------------------------------------------------
(define spice-sdb:component-area
  (lambda (package)
    (let ((area (gnetlist:get-package-attribute package "area")))
      (if (not (string=? area "unknown"))
        (string-append " " area)
        ""))))


;;-----------------------------------------------------------
;; Given a refdes, returns the device attribute "model" as string
;;-----------------------------------------------------------
(define spice-sdb:component-model
  (lambda (package)
    (let ((model (gnetlist:get-package-attribute package "model")))
      (if (not (string=? model "unknown"))
        model spice-sdb:component-value))))


;;----------------------------------------------------------
;; Include SPICE statements from a SPICE directive block.
;; This includes an arbitrary piece of SPICE code.  
;;----------------------------------------------------------
(define spice-sdb:write-directive
  (lambda (package port)
             ;; Collect variables used in creating spice code
	(let ((value (gnetlist:get-package-attribute package "value"))
	      (file (gnetlist:get-package-attribute package "file"))
	     )   ;; end of local assignments

;;  ------------ Debug stuff -----------
	  (display (string-append "Found SPICE directive box.  Refdes = " package "\n"))
;;	  (display (string-append "in write-directive, value = " value "\n"))
;;	  (display (string-append "in write-directive, file = " file "\n"))
;;  ------------------------------------

	  (cond
	      ;; First look to see if there is a value.
	   ((not (string=? value "unknown"))
	     (display (string-append value "\n") port))
              ;; since there is no value, look for file. 
	   ((not (string=? file "unknown"))
	     (insert-text-file file port) )
	  ) ;; close of cond
	) ;; close of let
    ) ;; close of lambda
) ;; close of define


;;----------------------------------------------------------
;; Include a file using an .INCLUDE directive
;;----------------------------------------------------------
(define spice-sdb:write-include
  (lambda (package port)
    (let ((file-name (gnetlist:get-package-attribute package "file")) )
      (display (string-append "Found .INCLUDE box.  Refdes = " package "\n"))
      (display (string-append ".INCLUDE " file-name "\n") port)
    )
  )
)

;;----------------------------------------------------------
;; Include an option using an .OPTIONS directive
;;----------------------------------------------------------
(define spice-sdb:write-options
  (lambda (package port)
      (display (string-append "Found .OPTIONS box.  Refdes = " package "\n"))
    (display (string-append ".OPTIONS " (spice-sdb:component-value package) "\n") port)))


;;----------------------------------------------------------
;; Include a spice model (instantiated as a model box on the schematic)
;;  Two types of model can be included:
;;  1.  An embedded model, which is a one- or multi-line string held in the attribute "model".
;;      In this case, the following attributes are mandatory:
;;      --  model (i.e. list of parameter=value strings)
;;      --  model-name
;;      --  type
;;      In this case, the function creates and formats the correct spice model line(s).
;;  2.  A model held in a file whose name is held in the attribute "file"
;;      In this case, the following attribute are mandatory:
;;      --  file (i.e. list of parameter=value strings)
;;      In this case, the function just opens the file and dumps the contents
;;      into the netlist.
;;----------------------------------------------------------
(define spice-sdb:write-model
  (lambda (package port)
             ;; Collect variables used in creating spice code
	(let ((model-name (gnetlist:get-package-attribute package "model-name"))
	      (model-file (gnetlist:get-package-attribute package "file"))
	      (model (gnetlist:get-package-attribute package "model"))
	      (type (gnetlist:get-package-attribute package "type"))
	     )   ;; end of local assignments

	  (display (string-append "Found .MODEL box.  Refdes = " package "\n"))
	  
	  ;; Now, depending upon what combination of model, model-file, and model-name
	  ;; exist (as described above) write out lines into spice netlist.
	  (cond
	     ;; one model and model name exist
	   ( (not (or (string=? model "unknown") (string=? model-name "unknown")))
	     (display (string-append "found model and model-name for " package "\n"))  ;;  ******* Debug stuff
	     (display (string-append ".MODEL " model-name " " type " \(" model "\)\n") port) )

             ;; model file exists
	   ( (not (or (string=? model-file "unknown") ))
	     (display (string-append "found model-file for " package "\n"))  ;;  ******* Debug stuff
	     (insert-text-file model-file port) 
	   )

	  )  ;; close of cond
	) ;; close of let
    ) ;; close of lambda
) ;; close of define




;;-------------------------------------------------------------------
;;  This writes out the default component (i.e. the "device" attribute
;;  was not recognized).  This function does the following:
;;
;;  1.  Gets the refdes (package).
;;  2.  Checks the refdes against a short list of possible values.
;;      Depending upon the refdes, it does the following thing:
;;      D? -- Invokes write-diode
;;      Q? -- Invokes write-transistor-diode. (The "type" attribute is <unknown> 
;;            in this case so that the spice simulator will barf if the user
;;            has been careless.)
;;      U? -- Invokes write-ic. This provides the opportunity for a component
;;            model to be instantiated.
;;      X? -- Invokes write-subcircuit.  This provides the opportunity for a component
;;            model to be instantiated. 
;;      V? -- Invokes write-independent-voltage-source
;;      I? -- Invokes write-independent-current-source
;;      Otherwise, it just outputs the refdes, the attached nets, and the 
;;      value of the "value" attribute.
;;  
;;-------------------------------------------------------------------
(define spice-sdb:write-default-component
  (lambda (package port)

    (let ((first-char (string (string-ref package 0)) ))  ;; extract first char of refdes.
      (cond
       ((string=? first-char "D") (spice-sdb:write-diode package port))
       ((string=? first-char "Q") (spice-sdb:write-transistor-diode package "<unknown>" port))
       ((string=? first-char "U") (spice-sdb:write-ic package port))
       ((string=? first-char "V") (spice-sdb:write-independent-voltage-source package port))
       ((string=? first-char "I") (spice-sdb:write-independent-current-source package port))
       ((string=? first-char "X") (spice-sdb:write-subcircuit package port))
       (else 
	(display (string-append "Found unknown component.  Refdes = " package "\n"))
	(display (string-append package " ") port)
        ;; write net names, slotted components not implemented
	(spice-sdb:write-net-name-of-component package (length (gnetlist:get-pins package)) port)
        ;; write component value, if components have a label "value=#"
        ;; what if a component has no value label, currently unknown is written
	(display (spice-sdb:component-value package) port)
	(newline port)
       )
      ) ;; end cond
     )  ;; end let
  )
)



;;-------------------------------------------------------------------
;; Write the refdes, the net name connected to pin# wihout the 
;; component value. No extra attributes.
;; Note that no component value or any other attributes are written.  
;; Don't append carrage
;; return either.
;;-------------------------------------------------------------------
(define spice-sdb:write-component-no-value
  (lambda (package port)
    (display (string-append package " ") port)  ;; write component refdes
        ;; write net names, slotted components not implemented
    (spice-sdb:write-net-name-of-component package (length (gnetlist:get-pins package)) port)))


;;----------------------------------------------------------------------
;; write-netlist is passed a list of refdess (ls).  It uses 
;; each refdes to get the corresponding
;; "device" attribute.  Depending upon the device, it then invokes one or another of the 
;; spice line output fcns to output a line of the spice netlist.
;; I have enlarged the number of devices it recognizes -- SDB.
;; write the refdes, to the pin# connected net and component 
;; value and optional extra attributes
;; check if the component is a special spice component.
;;----------------------------------------------------------------------
(define spice-sdb:write-netlist
  (lambda (port ls)     
     (if (not (null? ls))
      (let* ((package (car ls))             ;; assign package
	     (device (get-device package))  ;; assign device.    
	    )                               ;; end of let* assignments

;; Debug stuff -- outputs line describing device being processed.
;;	(display (string-append "--- checking package = " package "\n"))
;;	(display (string-append "    device = " device "\n"))
;; done with debug stuff

        (cond
          ( (string=? device "none"))                 ;; do nothing for graphical symbols.
          ( (string=? device "spice-subcircuit-LL"))  ;; do nothing for subcircuit declaration.
          ( (string=? device "spice-IO"))             ;; do nothing for SPICE IO pins.
          ( (string=? device "SPICE-ccvs")
              (spice-sdb:write-ccvs package port))
          ( (string=? device "SPICE-cccs")
              (spice-sdb:write-cccs package port))
          ( (string=? device "SPICE-vcvs")
              (spice-sdb:write-vcvs package port))
          ( (string=? device "SPICE-vccs")
              (spice-sdb:write-vccs package port))
          ( (string=? device "SPICE-nullor")
              (spice-sdb:write-nullor package port))
          ( (string=? device "DIODE")
              (spice-sdb:write-diode package port))
          ( (string=? device "PMOS_TRANSISTOR")
              (spice-sdb:write-mos-transistor package port))
          ( (string=? device "NMOS_TRANSISTOR")
              (spice-sdb:write-mos-transistor package port))
          ( (string=? device "PNP_TRANSISTOR")
              (spice-sdb:write-pnp-bipolar-transistor package port))
          ( (string=? device "NPN_TRANSISTOR")
              (spice-sdb:write-npn-bipolar-transistor package port))
          ( (string=? device "PFET_TRANSISTOR")
              (spice-sdb:write-jfet-transistor package port))
          ( (string=? device "NFET_TRANSISTOR")
              (spice-sdb:write-jfet-transistor package port))
          ( (string=? device "MESFET_TRANSISTOR")
              (spice-sdb:write-mesfet-transistor package port))
          ( (string=? device "RESISTOR")
              (spice-sdb:write-resistor package port))
          ( (string=? device "CAPACITOR")
              (spice-sdb:write-capacitor package port))
          ( (string=? device "POLARIZED_CAPACITOR")
              (spice-sdb:write-capacitor package port))       ;; change someday
          ( (string=? device "INDUCTOR")
              (spice-sdb:write-inductor package port))     
          ( (string=? device "VOLTAGE_SOURCE")
              (spice-sdb:write-independent-voltage-source package port)) ;; change someday
          ( (string=? device "CURRENT_SOURCE")
              (spice-sdb:write-independent-current-source package port)) ;; change someday
          ( (string=? device "model")
              (spice-sdb:write-model package port))
          ( (string=? device "options")
              (spice-sdb:write-options package port))
          ( (string=? device "directive")
              (spice-sdb:write-directive package port))
          ( (string=? device "include")
              (spice-sdb:write-include package port))
          ( else 
	      (spice-sdb:write-default-component package port))
        ) ;; end of cond
	(spice-sdb:write-netlist port (cdr ls))
	 ))))


;;--------------------------------------------------------------
;; Spice netlist header
;;--------------------------------------------------------------
(define spice-sdb:write-top-header
  (lambda (port)
    (display "*********************************************************\n" port)
    (display "* Spice file generated by gnetlist                      *\n" port)
    (display "* spice-sdb version 8.29.2003 by SDB --                 *\n" port)
    (display "* provides advanced spice netlisting capability.        *\n" port)
    (display "* Documentation at http://www.brorson.com/gEDA/SPICE/   *\n" port)
    (display "*********************************************************\n" port)
  )
)

;;--------------------------------------------------------------
;; .SUBCKT netlist header
;;--------------------------------------------------------------
(define spice-sdb:write-subcircuit-header
  (lambda (port)
    (display "*******************************\n" port)
    (display "* Begin .SUBCKT model         *\n" port)
    (display "* spice-sdb ver 8.29.2003     *\n" port)
    (display "*******************************\n" port)
  )
)


;;---------------------------------------------------------------
;; Write the .END line
;;---------------------------------------------------------------
(define spice-sdb:write-bottom-footer
  (lambda (salutation port)
    (display salutation port)
    (newline port)))

;;---------------------------------------------------------------
;; Spice netlist generation
;;   This is the entry point.
;;   Hacked on 3.31.2003 to enable writing out .SUBCKT models -- SDB.
;;   The algorithm is as follows:
;;   1.  
;;   2.  Figure out if there is a .SUBCKT block on the schematic,
;;       or if it is just a normal schematic.
;;       If a .SUBCKT:
;;       -- Write out subcircuit header (a comment identifying the netlister).
;;       -- find all spice-IO pins.  Get a list of the packages.
;;       -- put them in order (ordered by package refdes)
;;       -- get the list of nets attached to the spice-IO pins.
;;       -- write out .SUBCKT line
;;       If a normal schematic: 
;;       -- Write out top header (a comment identifying the netlister).
;;   3.  Write out netlist.
;;   4.  If a .SUBCKT:  write out .ENDS
;;       Otherwise: write out .END
;;   5.  Close up file and return.
;;---------------------------------------------------------------
(define spice-sdb
  (lambda (output-filename)
    ;; 
    ;; First find out if this is a .SUBCKT lower level, 
    ;; or if it is a regular schematic.
    ;;
    (let* ((port (open-output-file output-filename))
	   (schematic-type (spice-sdb:get-schematic-type packages))
	   (model-name (spice-sdb:get-subcircuit-modelname schematic-type)) 
	  )
      (display "Using SPICE backend by SDB -- Version of 3.31.2003\n")
      (display (string-append "schematic-type = " schematic-type "\n"))
      (display (string-append "model-name = " model-name "\n"))

      (if (not (string=? schematic-type "normal schematic"))
    ;; we have found a .SUBCKT type schematic.
	  (let* ((io-pin-packages (spice-sdb:get-spice-IO-pins packages (list) ))
                 (io-pin-packages-ordered (spice-sdb:sort-spice-IO-pins io-pin-packages))
	         (io-nets-list (reverse (spice-sdb:get-IO-nets io-pin-packages-ordered (list) )))
                )

;;-------------  Debug stuff  ----------------
;;	    (display "Found .SUBCKT. spice-IO pin refdeses are: \n")
;;	    (display io-pin-packages)
;;	    (display "\n After sorting, they are: \n")
;;	    (display io-pin-packages-ordered)
;;	    (display "\n attached net list is: \n")
;;	    (display io-nets-list)
;;	    (display "\n")
;;--------------------------------------------
	    
    ;; now write out .SUBCKT header and .SUBCKT line
	    (spice-sdb:write-subcircuit-header port)   
	    (display (string-append schematic-type " " (list-2-string io-nets-list "") "\n") port)
	  )
	  
	  ;; Otherwise it's a regular schematic.  Write out comments in file header.
	  (spice-sdb:write-top-header port)   

      ) ;; end of if.

    ;; now write out netlist
    ;; **** Modified by kh to sort list of packages so Spice directives, etc. (A?) are output last,
    ;; **** and in increasing order.
      (if (spice-sdb:sort-refdes? (gnetlist:get-calling-flags))
	  (spice-sdb:write-netlist port (sort packages spice-sdb:packsort))  ;; sort on refdes
	  (spice-sdb:write-netlist port packages)                            ;; don't sort.
      )

    ;; now write out .END(S) of netlist.
      (if (not (string=? schematic-type "normal schematic"))
          (begin 
	    (spice-sdb:write-bottom-footer (string-append ".ENDS " model-name) port)
	    (display "*******************************\n" port)
	  )
          (spice-sdb:write-bottom-footer ".END" port)
      )

    ;; now we're done.
      (close-output-port port))))

;;---------------------------------------------------------------
;; spice-sdb:packsort
;;   Sort procedure to order refdes's alphabetically but
;;   keep A? packages at the end of list so SPICE simulation
;;   directives operate correctly.
;;  This fcn written by Ken Healy to enable SPICE netlisting for 
;;  Gnucap, which wants A refdes cards (i.e. SPICE directives) 
;;  to appear last in the SPICE netlist.  Slightly modified
;;  and incorporated into main spice-sdb release by SDB on 9.1.2003.
;;  To output the netlist in sorted order, use the -s switch 
;;  when invoking gnetlist from the command line.  Example:
;;  gnetlist -s -g spice-sdb -o output.spice Schematic.sch
;;  The default behavior (i.e. if -s is not specified) is to do
;;  no sorting.
;;---------------------------------------------------------------
(define spice-sdb:packsort
  (lambda (x y)
    (let ((xdes (string-ref x 0)) 
          (ydes (string-ref y 0))
          (xnum (string-tail x 1)) 
          (ynum (string-tail y 1)) 
         )

      (if (char-ci=? xdes ydes)
          (if (string-ci<? xnum ynum) #t #f)
          (if (char-ci=? xdes #\A) #f
              (if (char-ci=? ydes #\A) #t 
                  (if (char-ci<? xdes ydes) #t #f)))))))
          

(define (string-tail string start)
  (substring string start (string-length string)))


;;---------------------------------------------------------------
;; spice-sdb:sort_refdes?
;;   Returns #t or #f depending upon if -s was discovered in 
;;   the calling flags given to gnetlist.   Used in conjunction with 
;;   spice-sdb:packsort.
;;   Calling form: (spice-sdb:sort-refdes? (gnetlist:get-calling-flags))
;;   9.1.2003 -- SDB.
;;---------------------------------------------------------------
(define spice-sdb:sort-refdes?
  (lambda (calling-flag-list)

    (if (null? calling-flag-list)
	  '#f                                             ;; return #f if null list -- sort_mode not found.
	  (let*	((calling-pair (car calling-flag-list))   ;; otherwise look for sort_mode in remainder of list.
		 (calling-flag (car calling-pair))
		 (flag-value (cadr calling-pair))  )

	    (if (string=? calling-flag "sort_mode")
		flag-value                                               ;; return flag-value if sort_mode found
		(spice-sdb:sort-refdes? (gnetlist:get-calling-flags))    ;; otherwise recurse until sort_mode is found
	    )  ;; end if  
	  )  ;; end of let*
     )  ;; end of if
))

