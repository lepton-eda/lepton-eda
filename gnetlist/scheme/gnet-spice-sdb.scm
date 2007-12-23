;;; gEDA - GPL Electronic Design Automation
;;; gnetlist - gEDA Netlist
;;; Copyright (C) 1998-2007 Ales Hvezda
;;; Copyright (C) 1998-2007 gEDA Contributors (see ChangeLog for details)
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
;;  9.9.2003  -- 5th version.  Rearranged code for more organization (I was beginning
;;               to get lost. . . .).  Incorporated changes to handle external SPICE
;;               files more intelligently.  Changed spew to be configurable by setting
;;               -v from the command line.  Placed new fcn debug-spew into gnetlist.scm.
;;               Added -I command line flag.
;;  10.14.2003 -- Bugfixes: Added empty-string? and hacked get-file-type to handle 
;;                case where a model file has an empty line before .SUBCKT or .MODEL.  
;;                Also modified write-net-names-on-component to gracefully handle
;;                case where not every pin has a pinseq attribute.  Now only outputs
;;                pins with valid pinseq attribute.
;;  12.25.2003 -- Bugfix:  Unswizzled emission of pins from user-defined .subckts.
;;                (Now correctly uses pinseq to define emission order of pins.)  Also
;;                added ability to emit attributes for semiconductors (e.g. area, off,
;;                ic, etc.)  Added in response to user requests.
;;  12.29.2003 -- Two small enhancements requested by Peter Kaiser.
;;  12.29.2003.a -- Minor bugfix.
;;  12.29.2003.b -- Second minor bugfix.
;;  12.29.2003.c -- Change res & cap to incorporate modelname & "area=" attrib.
;;  3.24.2004 -- Bugfixes made to JFET stuff during Feb.  Change released now.
;;  8.22.2004 -- Added command line as first line of file.
;;  8.29.2004 -- Changed sense source naming in controlled sources because the old convention
;;               was confusing ngspice.
;;  10.9.2004 -- Added patches for voltage controlled switches from Peter Kaiser.
;;  3.16.2005 -- Fixed CCCS bug (typo in Vsense) noticed by David Logan
;;  5.16.2005 -- Modified behavior of .INCLUDE directive.  Now by default it just 
;;               spits out the string instead of putting the contents of the file
;;               into the SPICE netlist.  You can force insertion of the file using
;;               the -e flag.
;;  6.12.2005 -- Changed order of writing out netlist and .model/.subckt cards to 
;;               facilitate use of numparam with ngspice.  Change supplied by 
;;               Dominique Michel.
;;  9.11.2005 -- Incorporated patch from Paul Bunyk to enable netlisting of
;;               Josephson junctions and "K" mutual inductances.  Also enabled 
;;               netlisting of "COIL" devices as inductors.
;;  12.27.2005 -- Fix bug discovered by John Doty: spice-IO pins with refdes greater
;;                than P9 were sorted incorrectly (as strings).  Now they are sorted
;;                as numbers.
;;  3.10.2006 -- Added "m" attribute to PMOS and NMOS per request of Peter Kaiser.
;;  4.11.2006 --  Changed the .END and .ENDS cards to lowercase. 
;;                This fixes bug 1442912. Carlos Nieves Onega.
;;  2.10.2007 -- Various bugfixes.  Also incorporated slotted part
;;               netlist patch from Jeff Mallatt.  SDB.
;;  4.28.2007 -- Fixed slotted part stuff so that it uses pinseq to emit pins.  SDB
;;               
;;**********************************************************************************
;;
;;  Organization of gnet-spice-sdb.scm file:
;;  --  Functions for program housekeeping, handling of calling flags, file manipulation.
;;  --  Functions for handling nets & devices and creating SPICE cards.
;;  --  High-level functions which control program flow.  Note that the program entry
;;      point lives at the very bottom of this file.
;;
;;  Unfortunately, no organization is present beneath this top level. . . .
;;
;;**********************************************************************************


;;**********************************************************************************
;;************  Program housekeeping, handling calling flags, etc.  ****************
;;**********************************************************************************

;; The following is needed to make guile 1.8.x happy.
(use-modules (ice-9 rdelim))

;;--------------------------------------------------------------------------------
;; spice-sdb:loop-through-files -- loops through the model-file list, and for each file
;;  name discovered in the list, it processes the file by invoking handle-spice-file.
;;--------------------------------------------------------------------------------
(define spice-sdb:loop-through-files
  (lambda (file-info-list port)
    (if (not (null? file-info-list))
	(let*  ((list-element (car file-info-list))
		(model-name (car list-element))
		(file-name (cadr list-element))
		(file-type (caddr list-element))
	       )
	  (spice-sdb:handle-spice-file file-name port)
	  (spice-sdb:loop-through-files (cdr file-info-list) port)
	)  ;; end of let*
)))


;;--------------------------------------------------------------------------------
;; spice-sdb:get-file-info-list-item  -- loops through the model-file list looking
;;  for triplet corresponding to model-name.  If found, it returns the corresponding
;;  list.  If not found, returns #f
;;--------------------------------------------------------------------------------
(define spice-sdb:get-file-info-list-item
  (lambda (model-name file-info-list)
    (if (null? file-info-list)
	'()                                           ;; return #f upon empty list.
	                                              ;; #f replaced with '() by peter
	(let*  ((list-element (car file-info-list))   ;; else process list-item
		(list-elt-model-name (car list-element))
		(list-elt-file-name (cadr list-element))
		(list-elt-file-type (caddr list-element))
	       )
	  (if (string=? list-elt-model-name model-name)
	      list-element                                                        ;; found model-name.  Reutrn list-element.
	      (spice-sdb:get-file-info-list-item model-name (cdr file-info-list)) ;; otherwise, recurse.
	  )
        )  ;; end of let*
)))



;;--------------------------------------------------------------------------
;; handle-spice-file:  This wraps insert-text-file.
;; Calling form: (handle-spice-file file-name)
;; It looks to see if the -I flag was set at the command line.  If so,
;; it just writes a .INCLUDE card with the file name.  If not,  it calls
;; insert-text-file to stick the file's contents into the SPICE netlist.
;;--------------------------------------------------------------------------
(define spice-sdb:handle-spice-file
  (lambda (file-name port)
    (debug-spew (string-append "Handling spice model file " file-name "\n"))
    (if (calling-flag? "include_mode" (gnetlist:get-calling-flags))
	(display (string-append ".INCLUDE " file-name "\n") port)       ;; -I found: just print out .INCLUDE card
	(spice-sdb:insert-text-file file-name port)                     ;; -I not found: invoke insert-text-file
    )  ;; end of if (calling-flag
))



;;--------------------------------------------------------------------------
;; Given a filename, open the file, get the contents, and dump them
;; into the spice file.
;; Calling form is "(insert-text-file input-file output-file)"
;; The function opens input-file, but assumes that output-file is
;; already open. 
;;
;; This function is usually used to include spice models contained in 
;; files into the netlist.  Note that it doesn't
;; check the correctness of the spice code in the file -- you're on your own!
;;---------------------------------------------------------------------------
(define spice-sdb:insert-text-file
  (lambda (model-filename port)
    (let ((model-file (open-input-file model-filename)) )
      (display (string-append "*vvvvvvvv  Included SPICE model from " model-filename " vvvvvvvv\n") port)
      (let while ((model-line (read-line model-file)))
	  (if (not (eof-object? model-line))
		   (begin
		     (display (string-append model-line "\n") port)
		     ;; (display (string-append "-- model-line = " model-line "\n")) ;; super debug statement
		     (while (read-line model-file))
		   )  ;; end of inner begin
	  ) ;; end of if
	)  ;; end of inner let
        (close-port model-file)
	(display (string-append "*^^^^^^^^  End of included SPICE model from " model-filename " ^^^^^^^^\n") port)
	(display (string-append "*\n") port)
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
;;  Repaired on 12.27.2005 to correctly sort pin numbers > 9.
;;----------------------------------------------------------------
(define spice-sdb:sort-spice-IO-pins 
  (lambda (package-list)
    ;;  Yes, this isn't good Scheme form.  Tough!  Writing this out
    ;;  in a functional programming form would be totally confusing!
    ;;  Note that this fcn requires that 
    ;;  each spice-IO pin have the same, single character prefix (i.e. 'P')
    (let* ((char-prefixes              (map car (map string->list package-list)))  ;; Pull off first char (prefix)
	   (prefixes                   (map string char-prefixes))                 ;; Make list of strings from prefixes
	   (split-numbers-list         (map cdr (map string->list package-list)))  ;; Pull off refdes numbers as list elements
	   (string-numbers-list        (map list->string split-numbers-list))      ;; Recombine split up (multidigit) number strings
	   (numbers-list               (map string->number string-numbers-list))   ;; Convert strings to numbers for sorting
	   (sorted-numbers-list        (sort numbers-list <))                      ;; Sort refdes numbers as numbers
	   (sorted-string-numbers-list (map number->string sorted-numbers-list)) ) ;; Create sorted list of refdes strings.

      ;; (debug-spew "Packages found = \n")
      ;; (debug-spew package-list)
      ;; (debug-spew "\nPrefixes found = \n")
      ;; (debug-spew prefixes)
      ;; (debug-spew "\nNumbers found -- split-numbers-list\n")
      ;; (debug-spew split-numbers-list)
      ;; (debug-spew "\nNumbers found -- numbers-list\n")
      ;; (debug-spew numbers-list)
      ;; (debug-spew "\nSorted-numbers-list\n")
      ;; (debug-spew sorted-numbers-list)
      ;; (debug-spew "\nSorted-string-numbers-list\n")
      ;; (debug-spew sorted-string-numbers-list)

      (map-in-order string-append  prefixes sorted-string-numbers-list)  ;; Laminate prefixes back onto refdes numbers & return.

    )
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
  (lambda (ls)
    (let while 
	((st (string)) 
	 (local-list ls)
	)
      (if (null? local-list)
	  st                                                    ;; end iteration & return string if list is empty.
	  (begin                                                ;; otherwise turn next element of list into string. . .     
	    (set! st (string-append (car local-list) " " st))   ;; stuff next element onto st
	    (while st (cdr local-list))                         ;; iterate with remainder of ls
	  )
      ) ;; end of if
    )
  )
)

;;----------------------------------------------------------
;;  This returns #t if the string is composed only of
;;  whitespace.  It works by turning the string into 
;;  a list, and then checking to see if it is the empty
;;  list.  If so, it returns #t.
;;----------------------------------------------------------
(define empty-string?
  (lambda (string)
    (null? (string->list string))
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

	(cond
	 ((eof-object? file-line)         ;;  Arrived at end of line without finding .MODEL or .SUBCKT.  Return "OTHER"
	    "OTHER")
	   
	 ((empty-string? file-line)
	    (while (read-line model-file)) )        ;; Found empty line.  Iterate before doing anything else.

	 ((string=? (string (string-ref file-line 0)) "*")
	    (while (read-line model-file)) )                       ;; Found *comment.  Iterate.
	   
	 ((string=? (string (string-ref file-line 0)) ".")
	  (begin
	    (debug-spew "In get-file-type, first-char = .\n")  ;; DEBUG stuff
	    (cond

	      ((string-ci=? (safe-string-head file-line 7) ".subckt")  ;; found .subckt as first line.
	       ".SUBCKT" )

	      ((string-ci=? (safe-string-head file-line 6) ".model")   ;; found .model as first line.
	       ".MODEL"  )
	     
	      (else "OTHER")   ;; first . spice card is neither .model nor .subckt

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
      (let ((different-prefix (not (string=? (substring package 0 1) prefix)) )
            (nomunge (calling-flag? "nomunge_mode" (gnetlist:get-calling-flags)) )
           )
	(debug-spew (string-append "Checking prefix.  Package prefix =" (substring package 0 1) "\n"))
	(debug-spew (string-append "                  correct prefix =" prefix "\n"))
	(debug-spew "   nomunge mode = ")
	(debug-spew nomunge)
	(debug-spew (string-append "\n  different-prefix="))
	(debug-spew different-prefix)
	(debug-spew "\n")
	(if (and different-prefix (not nomunge))
	    (display prefix port) )
      )
    )
)


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
;;  Note:  I should re-write this to use calling-flag? . . . .
(define spice-sdb:sort-refdes?
  (lambda (calling-flag-list)

    (if (null? calling-flag-list)
	  '#f                                             ;; return #f if null list -- sort_mode not found.
	  (let*	((calling-pair (car calling-flag-list))   ;; otherwise look for sort_mode in remainder of list.
		 (calling-flag (car calling-pair))
		 (flag-value (cadr calling-pair))  )

	    (if (string=? calling-flag "sort_mode")
		flag-value                                               ;; return flag-value if sort_mode found
		(spice-sdb:sort-refdes? (cdr calling-flag-list))    ;; otherwise recurse until sort_mode is found
	    )  ;; end if  
	  )  ;; end of let*
     )  ;; end of if
))
	    



;;**********************************************************************************
;;***************  Dealing with nets, devices, & SPICE cards.    *******************
;;**********************************************************************************

;;-----------------------------------------------------------
;; gnet-spice replacement of gnetlist:get-nets, a net labeled "GND" becomes 0
;;-----------------------------------------------------------
(define spice-sdb:get-net
  (lambda (refdes pin-name)
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
      (display (string-append "Vsense_" package  " " (spice-sdb:component-value package) "\n" ) port)
          ;; implement the current measuring voltage source
      (display (string-append "Vsense_" package " ") port)
      (spice-sdb:write-two-pin-names package "3" "4" port)
      (display "dc 0\n" port)
          ;; now it is possible to leave the output voltage source unconnected
          ;; i.e. spice won't complain about unconnected nodes
      (display (string-append "IOut_" package " ") port)
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
      (display (string-append "Vsense_" package " " (gnetlist:get-package-attribute package "value") "\n" ) port)
          ;; implement the current measuring voltage source
      (display (string-append "Vsense_" package " ") port)
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
      (spice-sdb:write-net-names-on-component package (length (gnetlist:get-pins package)) port)
       (display  (string-append (spice-sdb:component-value package) "\n")  port)
          ;; implement the voltage measuring current source
          ;; imagine yourself copying the voltage of a voltage source with an internal
          ;; impedance, spice starts complaining about unconnected nets if this current
          ;; source is not here.
      (display (string-append "IMeasure_" package " ") port)
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
      (spice-sdb:write-net-names-on-component package (length (gnetlist:get-pins package)) port)
      (display (string-append (gnetlist:get-package-attribute package "value") "\n" ) port)
          ;; implement the voltage measuring current source
          ;; imagine yourself copying the voltage of a voltage source with an internal
          ;; impedance, spice starts complaining about unconnected nets if this current
          ;; source is not here.
      (display (string-append "Isense_" package " ") port)
      (spice-sdb:write-two-pin-names package "3" "4" port)
      (display "dc 0\n" port)
          ;; with an output current source it is possible to leave the output voltage source
          ;; unconnected i.e. spice won't complain about unconnected nodes
      (display (string-append "IOut_" package " ") port)
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
      (spice-sdb:write-net-names-on-component package (length (gnetlist:get-pins package)) port)
      (display (string-append (gnetlist:get-package-attribute package "value") "\n" ) port)
          ;; implement the voltage measuring current source
          ;; imagine yourself copying the voltage of a voltage source with an internal
          ;; impedance, spice starts complaining about unconnected nets if this current
          ;; source is not here.
      (display (string-append "IMeasure_" package " ") port)
      (spice-sdb:write-two-pin-names package "3" "4" port)
      (display "dc 0\n" port)
          ;; with an output current source it is possible to leave the output voltage source
          ;; unconnected i.e. spice won't complain about unconnected nodes
      (display (string-append "IOut_" package " ") port)
      (spice-sdb:write-two-pin-names package "1" "2" port)
      (display "dc 0\n" port)
      (display "* end of nullor expansion\n" port))))


;;----------------------------------------------------------------
;;
;; Write-transistor-diode: writes out component followed by 
;; model or model file associated
;; with the component.
;;  This function does the following:
;;   1.  Writes out the correct refdes prefix (if specified and necessary).
;;   2.  Writes out the refdes and nets 
;;   3.  Looks for "model-name" attribute. Writes it out if it exists.
;;   4.  If there is no "model-name" attribute, it writes out the "value"
;;       attribute.  If there is no "value" attribute, it writes out "unknown"
;;       and returns, causing the spice simulator to puke when the netlist 
;;       is run.  This is important 
;;       'cause the spice simulator needs to have some indication of what
;;       model to look for.
;;   5.  Outputs optional attributes attached to device, if any.  Feature 
;;       added by SDB on 12.25.2003.
;;   6.  Outputs a new line
;;   *.  Loops back to "1." if more than one slot.
;;   7.  Looks for a the "model" attribute.  If it exists, it it writes out
;;       a .MODEL line like this:  .MODEL model-name type (model)
;;      
;;----------------------------------------------------------------
(define spice-sdb:write-transistor-diode
  (lambda (package prefix type attrib-list port)

    ;; First do local assignments
    (let ((model-name (gnetlist:get-package-attribute package "model-name"))
	  (model (gnetlist:get-package-attribute package "model"))
	  (value (gnetlist:get-package-attribute package "value"))
	  (area (gnetlist:get-package-attribute package "area"))
	  (off (gnetlist:get-package-attribute package "off"))
	  (model-file (gnetlist:get-package-attribute package "file"))
	 )   ;; end of local assignments

    ;; loop over slots
      (do ((slot 1 (1+ slot))) ((> slot (length (gnetlist:get-unique-slots package))))

     ;; Write out the refdes prefix, if specified and necessary.
	(if prefix
	  (spice-sdb:write-prefix package prefix port)
	)

     ;; Next we write out the refdes and nets.  
	(spice-sdb:write-component-slotted-no-value package slot port)

     ;; next look for "model-name" attribute.  Write it out if it exists.
     ;; otherwise look for "device" attribute.  
        (if (not (string=? model-name "unknown"))
	    (display (string-append model-name " " ) port)  ;; display model-name if known 
	    (display (string-append value " ") port))       ;; otherwise display device

    ;; Next write out attribtes if they exist
    ;; First attribute is area.  It is written as a simple string
	(if (not (string=? area "unknown"))
	    (display (string-append area " ") port))

    ;; Next attribute is off.    It is written as a simple string
	(if (not (string=? off "unknown"))
	    (display (string-append off " ") port))

    ;; Write out remaining attributes
	(spice-sdb:write-list-of-attributes package attrib-list port)

    ;; Now write out newline in preparation for writing out model.
	(newline port)

      ) ;; do

     ;; Now write out any model which is pointed to by the part.
	(cond

     ;; one line model and model name exist
	 ( (not (or (string=? model "unknown") (string=? model-name "unknown")))
	   (debug-spew (string-append "found model and model-name for " package "\n"))
	   (display (string-append ".MODEL " model-name " " type " (" model ")\n") port) )

     ;; one line model and component value exist
	 ( (not (or (string=? model "unknown") (string=? value "unknown")))
	   (debug-spew (string-append "found model and value for " package "\n"))
	   (display (string-append ".MODEL " model-name " " type " (" value ")\n") port) )

     ;; model file and model name exist
	 ( (not (or (string=? model-file "unknown") (string=? model-name "unknown")))
	   (debug-spew (string-append "found file and model-name for " package "\n"))
	   (debug-spew "I'll deal with the file later . . .\n")
	 )

     ;; model file and component value exist
	 ( (not (or (string=? model-file "unknown") (string=? value "unknown")))
	   (debug-spew (string-append "found file and value for " package "\n"))
	   (debug-spew "I'll deal with the file later . . .\n")
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
    (debug-spew (string-append "Found diode.  Refdes = " package "\n"))
    (let ((attrib-list (list "ic" "temp") ))  
      (spice-sdb:write-transistor-diode package "D" "D" attrib-list port))
  )
)


;;----------------------------------------------------------------
;;  spice-sdb:write-ic
;;  This writes out a valid ic line.
;;  The algorithm is as follows:
;;  1.  Figure out what type of model goes with this part from 
;;      file-info-list.  If it isn't listed, look for a MODEL attribute.
;;      If MODEL attribute is attached, write out SPICE card, and then
;;      write out .MODEL on next line.
;;      If no MODEL attribute is attached, just write out what litte 
;;      we know.  Then return
;;  2.  If the model-name is in the file-info-list, get the associated
;;      file-type.  Compare it against the component's refdes.  If model-type 
;;      == .SUBCKT and refdes doesn't begin with X, prepend an X to the refdes.
;; 3.   Print out the rest of the line.     
;;
;;----------------------------------------------------------------
(define spice-sdb:write-ic
  (lambda (package file-info-list port)

    (debug-spew (string-append "Found ic.  Refdes = " package "\n"))

    ;; First do local assignments
    (let ((model-name (gnetlist:get-package-attribute package "model-name"))
	  (model (gnetlist:get-package-attribute package "model"))
	  (value (gnetlist:get-package-attribute package "value"))
	  (model-file (gnetlist:get-package-attribute package "file"))
	  (list-item (list))
	 )   ;; end of local assignments

    ;; First, if model-name is empty, we use value attribute instead.
    ;; We do this by sticking the contents of "value" into "model-name".
      (if (string=? model-name "unknown")
	  (set! model-name value))
      
    ;; Now get item from file-info-list using model-name as key
      (set! list-item (spice-sdb:get-file-info-list-item model-name file-info-list))

    ;; check to see if list-item is null.
      (if (null? list-item)

    ;; list-item is null.  Evidently, we didn't discover any files holding this model.  
    ;; Instead we look for model attribute
	  (if (not (string=? model "unknown"))             
	    (begin                                     ;; model attribute exists -- write out card and model.
	      (debug-spew "Model info not found in model file list, but model attribute exists.  Write out spice card and .model line..\n") 
	      (do ((slot 1 (1+ slot))) ((> slot (length (gnetlist:get-unique-slots package))))  ;; loop over slots
		(spice-sdb:write-component-slotted-no-value package slot port)
		(display (string-append model-name "\n" ) port)
	      ) ;; do
	      (display (string-append ".MODEL " model-name " " type " (" model ")\n") port)
	    )
	    (begin                                     ;; no model attribute either.  Just write out card.
	      (debug-spew "Model info not found in model file list.  No model attribute either.  Just write what we know.\n")
	      (do ((slot 1 (1+ slot))) ((> slot (length (gnetlist:get-unique-slots package))))  ;; loop over slots
		(spice-sdb:write-component-slotted-no-value package slot port)
		(display (string-append model-name "\n" ) port)
	      ) ;; do
	    )
	  )   ;; end if (not (string=? . . . .

    ;; list-item is not null.  Therefore we process line depending upon contents of list-item
	  (let ((file-type (caddr list-item)) )
	   (cond 
	      ;; ---- file holds a model ----
	      ((string=? file-type ".MODEL") 
	       (begin
		(debug-spew (string-append "Found .MODEL with model-file and model-name for " package "\n")) 
		 (do ((slot 1 (1+ slot))) ((> slot (length (gnetlist:get-unique-slots package))))  ;; loop over slots
		   (spice-sdb:write-prefix package "U" port)  ;; this appends an "U" to the refdes since we have a .model
		   (spice-sdb:write-component-slotted-no-value package slot port)
		   (display (string-append model-name "\n" ) port)
		 ) ;; do
		(debug-spew "We'll handle the file contents later . . .\n")
	       ))

	      ;; ---- file holds a subcircuit ----
	      ((string=? file-type ".SUBCKT") 
	       (begin
		 (debug-spew (string-append "Found .SUBCKT with model-file and model-name for " package "\n")) 
		 (do ((slot 1 (1+ slot))) ((> slot (length (gnetlist:get-unique-slots package))))  ;; loop over slots
		   (spice-sdb:write-prefix package "X" port)  ;; this appends an "X" to the refdes since we have a .subckt
		   (spice-sdb:write-component-slotted-no-value package slot port)
		   (display (string-append model-name "\n" ) port)
		 ) ;; do
		 (debug-spew "We'll handle the file contents later . . .\n")
	       ))
	   )  ;; close of inner cond
	 )   ;; end of inner let
       )  ;; end of if (null? list-item

  ) ;; end of outer let
 )
)


;;----------------------------------------------------------------
;;  spice-sdb:write-subcircuit
;;  This writes out a valid subcircuit line.
;;  The algorithm is as follows:
;;  1.  Figure out what type of model goes with this part from 
;;      file-info-list.  If it isn't listed, look for a MODEL attribute.
;;      If MODEL attribute is attached, write out SPICE card, and then
;;      write out .MODEL on next line.
;;      If no MODEL attribute is attached, just write out what little 
;;      we know.  Then return.
;;  2.  If the model-name is in the file-info-list, get the associated
;;      file-type.  Compare it against the component's refdes.  If model-type 
;;      == .MODEL and refdes doesn't begin with U, prepend an U to the refdes.
;; 3.   Print out the rest of the line.     
;;
;;  Note:  This is basically a clone of write-ic.  I can probably just
;;         eliminate this fcn and call write-ic for all U or X refdeses.
;;         Maybe on the next revision?
;;----------------------------------------------------------------
(define spice-sdb:write-subcircuit
  (lambda (package file-info-list port)

    (debug-spew (string-append "Found subcircuit.  Refdes = " package "\n"))

    ;; First do local assignments
    (let ((model-name (gnetlist:get-package-attribute package "model-name"))
	  (model (gnetlist:get-package-attribute package "model"))
	  (value (gnetlist:get-package-attribute package "value"))
	  (model-file (gnetlist:get-package-attribute package "file"))
	  (list-item (list))
	 )   ;; end of local assignments

    ;; First, if model-name is empty, we use value attribute instead.
    ;; We do this by sticking the contents of "value" into "model-name".
      (if (string=? model-name "unknown")
	  (set! model-name value))
      
    ;; Now get item from file-info-list using model-name as key
      (set! list-item (spice-sdb:get-file-info-list-item model-name file-info-list))

    ;; check to see if list-item is null.
      (if (null? list-item)

    ;; list-item is null.  Evidently, we didn't discover any files holding this model.  
    ;; Instead we look for model attribute
	  (if (not (string=? model "unknown"))             
	    (begin                                     ;; model attribute exists -- write out card and model.
	      (debug-spew "Model info not found in model file list, but model attribute exists.  Write out spice card and .model line..\n") 
	      (do ((slot 1 (1+ slot))) ((> slot (length (gnetlist:get-unique-slots package))))  ;; loop over slots
		(spice-sdb:write-component-slotted-no-value package slot port)
		(display (string-append model-name "\n" ) port)
	      ) ;; do
	      (display (string-append ".MODEL " model-name " " type " (" model ")\n") port)
	    )
	    (begin                                     ;; no model attribute either.  Just write out card.
	      (debug-spew "Model info not found in model file list.  No model attribute either.  Just write what we know.\n")
	      (do ((slot 1 (1+ slot))) ((> slot (length (gnetlist:get-unique-slots package))))  ;; loop over slots
		(spice-sdb:write-component-slotted-no-value package slot port)
		(display (string-append model-name "\n" ) port)
	      ) ;; do
	    )
	  )   ;; end if (not (string=? . . . .

    ;; list-item is not null.  Therefore we process line depending upon contents of list-item
	  (let ((file-type (caddr list-item)) )
	   (cond 
	      ;; ---- file holds a model ----
	      ((string=? file-type ".MODEL") 
	       (begin
		(debug-spew (string-append "Found .MODEL with model-file and model-name for " package "\n")) 
		 (do ((slot 1 (1+ slot))) ((> slot (length (gnetlist:get-unique-slots package))))  ;; loop over slots
		   (spice-sdb:write-prefix package "U" port)  ;; this prepends an "U" to the refdes if needed
		   (spice-sdb:write-component-slotted-no-value package slot port)
		   (display (string-append model-name "\n" ) port)
		 ) ;; do
		(debug-spew "We'll handle the file contents later . . .\n")
	       ))

	      ;; ---- file holds a subcircuit ----
	      ((string=? file-type ".SUBCKT") 
	       (begin
		 (debug-spew (string-append "Found .SUBCKT with model-file and model-name for " package "\n")) 
		 (do ((slot 1 (1+ slot))) ((> slot (length (gnetlist:get-unique-slots package))))  ;; loop over slots
		   (spice-sdb:write-prefix package "X" port)  ;; this appends an "X" to the refdes if needed
		   (spice-sdb:write-component-slotted-no-value package slot port)
		   (display (string-append model-name "\n" ) port)
		 ) ;; do
		 (debug-spew "We'll handle the file contents later . . .\n")
	       ))
	   )  ;; close of inner cond
	 )   ;; end of inner let
       )  ;; end of if (null? list-item

  ) ;; end of outer let
 )
)




;;-----------------------------------------------------------
;;  write npn bipolar transistor
;;  This writes out a valid transistor refdes & then calls
;;  the function which writes the rest of the line.
;;-----------------------------------------------------------
(define spice-sdb:write-npn-bipolar-transistor
  (lambda (package port)
    (debug-spew (string-append "Found npn bipolar transistor.  Refdes = " package "\n"))
    (let ((attrib-list (list "ic" "temp") ))  
      (spice-sdb:write-transistor-diode package "Q" "NPN" attrib-list port))
  )
)


;;-----------------------------------------------------------
;;  write pnp bipolar transistor
;;-----------------------------------------------------------
(define spice-sdb:write-pnp-bipolar-transistor
  (lambda (package port)
    (debug-spew (string-append "Found pnp bipolar transistor.  Refdes = " package "\n"))
    (let ((attrib-list (list "ic" "temp") ))  
      (spice-sdb:write-transistor-diode package "Q" "PNP" attrib-list port))
  )
)


;;-----------------------------------------------------------
;;  write n-channel jfet transistor
;;-----------------------------------------------------------
(define spice-sdb:write-nfet-transistor
  (lambda (package port)
    (debug-spew (string-append "Found n-channel JFET.  Refdes = " package "\n"))
    (let ((attrib-list (list "ic" "temp") ))
      (spice-sdb:write-transistor-diode package "J" "NJF" attrib-list port))
  )
)

;;-----------------------------------------------------------
;;  write p-channel jfet transistor
;;-----------------------------------------------------------
(define spice-sdb:write-pfet-transistor
  (lambda (package port)
    (debug-spew (string-append "Found p-channel JFET.  Refdes = " package "\n"))
    (let ((attrib-list (list "ic" "temp") ))
      (spice-sdb:write-transistor-diode package "J" "PJF" attrib-list port))
  )
)


;;------------------------------------------------------
;;  write pmos transistor
;;------------------------------------------------------
(define spice-sdb:write-pmos-transistor
  (lambda (package port)
    (debug-spew (string-append "Found PMOS transistor.  Refdes = " package "\n"))
    (let ((attrib-list (list "l" "w" "as" "ad" "pd" "ps" "nrd" "nrs" "temp" "ic" "m")))
      (spice-sdb:write-transistor-diode package "M" "PMOS" attrib-list port))
  )
)

;;------------------------------------------------------
;;  write nmos transistor
;;------------------------------------------------------
(define spice-sdb:write-nmos-transistor
  (lambda (package port)
    (debug-spew (string-append "Found NMOS transistor.  Refdes = " package "\n"))
    (let ((attrib-list (list "l" "w" "as" "ad" "pd" "ps" "nrd" "nrs" "temp" "ic" "m")))
      (spice-sdb:write-transistor-diode package "M" "NMOS" attrib-list port))
  )
)


;;------------------------------------------------------------
;;  write mesfet transistor
;;------------------------------------------------------------
;; ************  Fix this!!!!!!!!!!  **************
(define spice-sdb:write-mesfet-transistor
  (lambda (package port)
    (spice-sdb:write-transistor-diode package "Z" "MESFET" (list) port)))  ;; XXXXXX Fix this!!!


;;-----------------------------------------------------------
;;  write voltage controled switch
;;-----------------------------------------------------------
(define spice-sdb:write-vc-switch
  (lambda (package port)
    (debug-spew (string-append "Found voltage controled switch.  Refdes = " package "\n"))
    (let ((attrib-list (list " " ) ))
      (spice-sdb:write-transistor-diode package "S" "SW" attrib-list port))
  )
)


;;--------------------------------------------------------------------
;;  write resistor
;;--------------------------------------------------------------------
(define spice-sdb:write-resistor
  (lambda (package port)

    (debug-spew (string-append "Found resistor.  Refdes = " package "\n"))

    ;; loop over slots
    (do ((slot 1 (1+ slot))) ((> slot (length (gnetlist:get-unique-slots package))))

    ;; first write out refdes and attached nets
    (spice-sdb:write-component-slotted-no-value package slot port) 

    ;; next write out mandatory resistor value if it exists.
    (let ((value (gnetlist:get-package-attribute package "value")))
        (if (not (string=? value "unknown")) 
		(display (string-append value " " ) port))
    )

    ;; next write our model name if it exists
    (let* ((model-name (gnetlist:get-package-attribute package "model-name")))
        (if (not (string=? model-name "unknown")) 
		(display (string-append model-name " " ) port))
    )

    ;; next create list of attributes which can be attached to a resistor.
    ;; I include non-standard "area" attrib here per popular demand.
    (let ((attrib-list (list "area" "l" "w" "temp")))
	    ;; write the attributes (if any) separately
      (spice-sdb:write-list-of-attributes package attrib-list port)
      (display " " port))  ;; add additional space. . . . 

    ;; finally output a new line
    (newline port)

    ) ;; do
  )
)


;;----------------------------------------------------------------------------
;;  write capacitor
;;----------------------------------------------------------------------------
(define spice-sdb:write-capacitor
  (lambda (package port)

    (debug-spew (string-append "Found capacitor.  Refdes = " package "\n"))

    ;; loop over slots
    (do ((slot 1 (1+ slot))) ((> slot (length (gnetlist:get-unique-slots package))))

    ;; first write out refdes and attached nets
    (spice-sdb:write-component-slotted-no-value package slot port) 

    ;; next write capacitor value, if any.  Note that if the 
    ;; component value is not assigned nothing will be written out.
    (let ((value (gnetlist:get-package-attribute package "value")))
        (if (not (string=? value "unknown"))
		(display (string-append value " " ) port))
    )

    ;; next write capacitor model name, if any.  This is applicable to 
    ;; semiconductor caps used in chip design.
    (let ((model-name (gnetlist:get-package-attribute package "model-name")))
        (if (not (string=? model-name "unknown"))
		(display (string-append model-name " " ) port))
    )

    ;; Next write out attribtes if they exist.  Use 
    ;; a list of attributes which can be attached to a capacitor.
    ;; I include non-standard "area" attrib here per request of Peter Kaiser.
    (let ((attrib-list (list "area" "l" "w" "ic")))
      (spice-sdb:write-list-of-attributes package attrib-list port)
            ;; write the off attribute separately
		(display " " port))  ;; add additional space. . . . 

    (newline port)

    ) ;; do
  )
)


;;----------------------------------------------------------------------------
;;  write inductor
;;----------------------------------------------------------------------------
(define spice-sdb:write-inductor
  (lambda (package port)

    (debug-spew (string-append "Found inductor.  Refdes = " package "\n"))

    ;; loop over slots
    (do ((slot 1 (1+ slot))) ((> slot (length (gnetlist:get-unique-slots package))))

    ;; first write out refdes and attached nets
    (spice-sdb:write-component-slotted-no-value package slot port) 

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

    ) ;; do
  )
)


;;-------------------------------------------------------------------------
;;  write independent voltage source
;;  The behavior of the voltage source is held in the "value" attribute
;;-------------------------------------------------------------------------
(define spice-sdb:write-independent-voltage-source
  (lambda (package port)
    (debug-spew (string-append "Found independent voltage source.  Refdes = " package "\n"))

    ;; loop over slots
    (do ((slot 1 (1+ slot))) ((> slot (length (gnetlist:get-unique-slots package))))

            ;; first write out refdes and attached nets
    (spice-sdb:write-component-slotted-no-value package slot port) 

            ;; next write voltage value, if any.  Note that if the 
	    ;; voltage value is not assigned, then it will write "unknown"
    (let ((value (gnetlist:get-package-attribute package "value")))
		(display value port)
    )

    (newline port)

    ) ;; do
  )
)

;;-------------------------------------------------------------------------
;;  write independent current source
;;  The behavior of the current source is held in the "value" attribute
;;-------------------------------------------------------------------------
(define spice-sdb:write-independent-current-source
  (lambda (package port)

	(debug-spew (string-append "Found independent current source.  Refdes = " package "\n")) 

    ;; loop over slots
    (do ((slot 1 (1+ slot))) ((> slot (length (gnetlist:get-unique-slots package))))

            ;; first write out refdes and attached nets
    (spice-sdb:write-component-slotted-no-value package slot port) 

            ;; next write current value, if any.  Note that if the 
	    ;; current value is not assigned, then it will write "unknown"
    (let ((value (gnetlist:get-package-attribute package "value")))
		(display value port)
    )

    (newline port)

    ) ;; do
  )
)

;;----------------------------------------------------------------------------
;;  write Josephson junction in wrspice format. Paul Bunyk, Sep 2, 2005
;;----------------------------------------------------------------------------
(define spice-sdb:write-josephson-junction
  (lambda (package port)

    (debug-spew (string-append "Found Josephson junction.  Refdes = " package "\n"))

    ;; loop over slots
    (do ((slot 1 (1+ slot))) ((> slot (length (gnetlist:get-unique-slots package))))

    ;; first write out refdes and attached nets
    (spice-sdb:write-component-slotted-no-value package slot port) 

    ;; next, add a dummy node for JJ phase. Unlike in Xic netlister, give it 
    ;; a reasonable name, not a number, e.g., refdes.
    (display (string-append package " ") port)

    ;; next write JJ model name, if any.  
    (let ((model-name (gnetlist:get-package-attribute package "model-name")))
        (if (not (string=? model-name "unknown"))
		(display (string-append model-name " " ) port))
    )

    ;; Next write out attribtes if they exist.  Use 
    ;; a list of attributes which can be attached to a junction.
    (let ((attrib-list (list "area")))
      (spice-sdb:write-list-of-attributes package attrib-list port)
            ;; write the off attribute separately
		(display " " port))  ;; add additional space. . . . 

    (newline port)

    ) ;; do
  )
)

;;----------------------------------------------------------------------------
;;  write mutual inductance(actually K). Paul Bunyk, Sep 2, 2005
;;----------------------------------------------------------------------------
(define spice-sdb:write-coupling-coefficient
  (lambda (package port)

    (debug-spew (string-append "Found mutual inductance.  Refdes = " package "\n"))

    ;; loop over slots
    (do ((slot 1 (1+ slot))) ((> slot (length (gnetlist:get-unique-slots package))))

    ;; first write out refdes and attached nets (none)
    (spice-sdb:write-component-slotted-no-value package slot port) 

    ;; next two inductor names and value
    (let ((inductors (gnetlist:get-package-attribute package "inductors"))
	  (value (gnetlist:get-package-attribute package "value")) )
        (if (not (string=? inductors "unknown"))
		(display (string-append inductors " " ) port))	
        (if (not (string=? value "unknown"))
		(display (string-append value " " ) port))
	
    )

    (newline port)

    ) ;; do
  )
)

;;--------------------------------------------------------------------
;; Given a refdes and number of pins, this writes out the nets
;; attached to the component's pins.  This is used to write out
;; non-slotted parts.  Call it with a component refdes and the number 
;; of pins left on this component to look at.
;;--------------------------------------------------------------------
(define spice-sdb:write-net-names-on-component
  (lambda (refdes number-of-pins port)
    (if (> number-of-pins 0)
      (begin
            ;; first find pin1 and then start writing the connected net name
        (spice-sdb:write-net-names-on-component refdes (- number-of-pins 1) port)
            ;; generate a pin-name e.g. pin1, pin2, pin3 ...
        (let* ((pin-name (number->string number-of-pins))
	       (pinnumber (gnetlist:get-attribute-by-pinseq refdes pin-name "pinnumber"))
	       (pinseq (gnetlist:get-attribute-by-pinseq refdes pin-name "pinseq"))
	       (netname (car (spice-sdb:get-net refdes pinnumber)) )
	       )

;; -------  Super debug stuff  --------
	  (debug-spew "  In write-net-names-on-component. . . . \n")
	  (debug-spew (string-append "     pin-name = " pin-name "\n"))
	  (debug-spew (string-append "     pinnumber = " pinnumber "\n"))
	  (debug-spew (string-append "     pinseq = " pinseq "\n"))
	  (debug-spew (string-append "     netname = " netname "\n"))
;; ------------------------------ 

	  (if (not (string=? netname "ERROR_INVALID_PIN"))
             (display (string-append netname " ") port)     ;; write out attached net if OK.
             (debug-spew (string-append "For " refdes ", found pin with no pinseq attribute.  Ignoring. . . .\n"))
          )
        )  ;; let*
      )    ;; begin
    )
  )
)

;;--------------------------------------------------------------------
;; Given a refdes and number of pins, this writes out the nets
;; attached to the component's pins.  This version is used to write out
;; slotted parts.  Call it with a component refdes and the number 
;; of pins left on this component to look at.
;; New fcn cloned from write-net-names-on-component on 4.28.2007 -- SDB.
;;--------------------------------------------------------------------
(define spice-sdb:write-net-names-on-component-slotted
  (lambda (refdes this-pin end-pin pins-per-slot port)
    (if (>= this-pin end-pin)
      (begin
	;; recurse to implement loop over pins
        (spice-sdb:write-net-names-on-component-slotted refdes (- this-pin 1) end-pin pins-per-slot port)
	;; This is hack to deal with slotted pins.
        (let* ((pin-name (number->string this-pin)) 
	       (pinnumber (gnetlist:get-attribute-by-pinseq refdes pin-name "pinnumber"))
	       (netname (car (spice-sdb:get-net refdes pinnumber)) )
	      )
;; -------  Super debug stuff  --------
	  (debug-spew "  In write-net-names-on-component-slotted. . . . \n")
	  (debug-spew (string-append "     this-pin = " (number->string this-pin) "\n"))
	  (debug-spew (string-append "     end-pin = " (number->string end-pin) "\n"))
	  (debug-spew (string-append "     pin-name = " pin-name "\n"))
	  (debug-spew (string-append "     pinnumber = " pinnumber "\n"))
	  (debug-spew (string-append "     netname = " netname "\n"))
;; ------------------------------ 
	  (if (not (string=? netname "ERROR_INVALID_PIN"))
             (display (string-append netname " ") port)     ;; write out attached net if OK.
             (debug-spew (string-append "For " refdes ", found pin with no pinseq attribute.  Ignoring. . . .\n"))
          )
        )  ;; let*
      )    ;; begin
    )
  )
)


;;-------------------------------------------------------------------
;; Write the refdes -dot- slot (if not only slot), and the net names
;; connected to pins in this slot.  No return, and no component value
;; is written, or extra attribs.  Those are handled later.
;; This fcn is called once for each slot in a component.
;;-------------------------------------------------------------------
(define spice-sdb:write-component-slotted-no-value
  (lambda (package slot port)
    (let ((numslots (gnetlist:get-package-attribute package "numslots"))
	  (slot-count (length (gnetlist:get-unique-slots package)))
	  (pin-count (length (gnetlist:get-pins package))) )
      (if (or (string=? numslots "unknown") (string=? numslots "0"))
	  (begin	                                ;; non-slotted part.
	    (display (string-append package " ") port)  ;; write component refdes
	    (spice-sdb:write-net-names-on-component package pin-count port)
	  )   ;; begin
	  (let* ((pins-per-slot (/ pin-count slot-count))       ;; slotted part
	         (end-pos (+ (* pins-per-slot (- slot 1)) 1) )  ;; start high
		 (beginning-pos  (* pins-per-slot slot))        ;; and count down.
                )
;; -------  Super debug stuff for writing out slotted components  --------
	    (debug-spew "In write-component-slotted-no-value. . . . \n")
	    (debug-spew (string-append "     pins per slot = " (number->string pins-per-slot) "\n"))
	    (debug-spew (string-append "     slot = " (number->string slot) "\n"))
	    (debug-spew (string-append "     beginning-pos = " (number->string beginning-pos) "\n"))
	    (debug-spew (string-append "     end-pos = " (number->string end-pos) "\n"))
;; ------------------------------ 
	    (format port "~a.~a " package slot)  ;; write component refdes -dot- slot
	    (spice-sdb:write-net-names-on-component-slotted package beginning-pos end-pos pins-per-slot port)
	  )  ;; let*
      )  ;; if
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
;;----------------------------------------------------------
(define spice-sdb:write-directive
  (lambda (package port)
             ;; Collect variables used in creating spice code
	(let ((value (gnetlist:get-package-attribute package "value"))
	      (file (gnetlist:get-package-attribute package "file"))
	     )   ;; end of local assignments

	  (debug-spew (string-append "Found SPICE directive box.  Refdes = " package "\n"))

	  (cond

	      ;; First look to see if there is a value.
	   ((not (string=? value "unknown"))
	    (begin
	      (display (string-append value "\n") port)
	      (debug-spew (string-append "Appending value = \"" value "\" to output file.\n"))
	    ))

              ;; since there is no value, look for file. 
	   ((not (string=? file "unknown"))
	    (begin
	      (spice-sdb:insert-text-file file port)   ;; Note that we don't wait until the end here.  Is that OK?
	      (debug-spew (string-append "Inserting contents of file = " file " into output file.\n"))
	    ))

	  ) ;; close of cond
	) ;; close of let
    ) ;; close of lambda
) ;; close of define


;;----------------------------------------------------------
;; Include a file using an .INCLUDE directive
;; Changed on 6.12.2005: to embedd the contents of the file,
;; you must call gnetlist with the -e flag set.
;;----------------------------------------------------------
(define spice-sdb:write-include
  (lambda (package port)
    (let ((value (gnetlist:get-package-attribute package "value"))
	  (file (gnetlist:get-package-attribute package "file"))
	  )   ;; end of local assignments

      (debug-spew (string-append "Found SPICE include box.  Refdes = " package "\n"))
      ;; (debug-spew (string-append "   value = " value "\n"))
      ;; (debug-spew (string-append "   file = " file "\n"))
      
      (cond
       ;; First look to see if value attribute is used
       ((not (string=? value "unknown"))
	(begin
	  ;; (debug-spew "This include directive uses a value attribute.\n")
	  (if (calling-flag? "embedd_mode" (gnetlist:get-calling-flags))
	      (begin
		(spice-sdb:insert-text-file value port)                 ;; -e found: invoke insert-text-file
		(debug-spew (string-append "embedding contents of " value " into netlist.\n")))
	      (begin
		(display (string-append ".INCLUDE " value "\n") port)   ;; -e not found: just print out .INCLUDE card
		(debug-spew "placing .include directive string into netlist.\n"))
	 )))

       ;; Now look to see if file is used
       ((not (string=? file "unknown"))
	(begin
	  ;; (debug-spew "This include directive uses a file attribute.\n")
	  (if (calling-flag? "embedd_mode" (gnetlist:get-calling-flags))
	      (begin
		(spice-sdb:insert-text-file file port)                 ;; -e found: invoke insert-text-file
		(debug-spew (string-append "embedding contents of " value " into netlist.\n")))
	      (begin
		(display (string-append ".INCLUDE " file "\n") port)   ;; -e not found: just print out .INCLUDE card
		(debug-spew "placing .include directive string into netlist.\n"))
	  )  ;; end of if (calling-flag
        )
       )
     ) ;; end of cond
)))

;;----------------------------------------------------------
;; Include an option using an .OPTIONS directive
;;----------------------------------------------------------
(define spice-sdb:write-options
  (lambda (package port)
    (debug-spew (string-append "Found .OPTIONS box.  Refdes = " package "\n")) 
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

	  (debug-spew (string-append "Found .MODEL box.  Refdes = " package "\n"))
	  
	  ;; Now, depending upon what combination of model, model-file, and model-name
	  ;; exist (as described above) write out lines into spice netlist.
	  (cond
	     ;; one model and model name exist
	   ( (not (or (string=? model "unknown") (string=? model-name "unknown")))
	     (debug-spew (string-append "found model and model-name for " package "\n"))
	     (display (string-append ".MODEL " model-name " " type " (" model ")\n") port) )

             ;; model file exists
	   ( (not (or (string=? model-file "unknown") ))
	     (debug-spew (string-append "found model-file for " package "\n"))
	     ;; (spice-sdb:insert-text-file model-file port)   ;; don't write it out -- it's handled after the second pass.
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
  (lambda (package file-info-list port)

    (let ((first-char (string (string-ref package 0)) ))  ;; extract first char of refdes.
      (cond
       ((string=? first-char "D") (spice-sdb:write-diode package port))
       ((string=? first-char "Q") (spice-sdb:write-transistor-diode package #f "<unknown>" (list) port))
       ((string=? first-char "M") (spice-sdb:write-transistor-diode package #f "<unknown>" (list) port))
       ((string=? first-char "U") (spice-sdb:write-ic package file-info-list port))
       ((string=? first-char "V") (spice-sdb:write-independent-voltage-source package port))
       ((string=? first-char "I") (spice-sdb:write-independent-current-source package port))
       ((string=? first-char "X") (spice-sdb:write-subcircuit package file-info-list port))
       (else 
	(display (string-append "Found unknown component.  Refdes = " package "\n"))
	(do ((slot 1 (1+ slot))) ((> slot (length (gnetlist:get-unique-slots package))))  ;; loop over slots
	  (spice-sdb:write-component-slotted-no-value package slot port)
	  ;; write component value, if components have a label "value=#"
	  ;; what if a component has no value label, currently unknown is written
	  (display (spice-sdb:component-value package) port)
	  (newline port)
	) ;; do
       )
      ) ;; end cond
     )  ;; end let
  )
)



;;**********************************************************************************
;;***************  High-level functions for program control  ***********************
;;**********************************************************************************

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
  (lambda (port file-info-list ls)     
     (if (not (null? ls))
      (let* ((package (car ls))             ;; assign package
	     (device (get-device package))  ;; assign device.    
	    )                               ;; end of let* assignments

;; Super debug stuff -- outputs line describing device being processed.
	(debug-spew (string-append "--- checking package = " package "\n"))
	(debug-spew (string-append "    device = " device "\n"))
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
              (spice-sdb:write-pmos-transistor package port))
          ( (string=? device "NMOS_TRANSISTOR")
              (spice-sdb:write-nmos-transistor package port))
          ( (string=? device "PNP_TRANSISTOR")
              (spice-sdb:write-pnp-bipolar-transistor package port))
          ( (string=? device "SPICE-PNP")
              (spice-sdb:write-pnp-bipolar-transistor package port))
          ( (string=? device "NPN_TRANSISTOR")
              (spice-sdb:write-npn-bipolar-transistor package port))
          ( (string=? device "SPICE-NPN")
              (spice-sdb:write-npn-bipolar-transistor package port))
          ( (string=? device "PFET_TRANSISTOR")
              (spice-sdb:write-pfet-transistor package port))
          ( (string=? device "NFET_TRANSISTOR")
              (spice-sdb:write-nfet-transistor package port))
          ( (string=? device "MESFET_TRANSISTOR")
	      (spice-sdb:write-mesfet-transistor package port))
	  ( (string=? device "SPICE-VC-switch")
	      (spice-sdb:write-vc-switch package port))
          ( (string=? device "RESISTOR")
	      (spice-sdb:write-resistor package port))
          ( (string=? device "CAPACITOR")
              (spice-sdb:write-capacitor package port))
          ( (string=? device "POLARIZED_CAPACITOR")
              (spice-sdb:write-capacitor package port))                  ;; change someday
          ( (string=? device "INDUCTOR")
              (spice-sdb:write-inductor package port))     
          ( (string=? device "COIL")           ;; Added to enable netlisting of coil-*.sym
              (spice-sdb:write-inductor package port))     
          ( (string=? device "VOLTAGE_SOURCE")
              (spice-sdb:write-independent-voltage-source package port)) ;; change someday
          ( (string=? device "CURRENT_SOURCE")
              (spice-sdb:write-independent-current-source package port)) ;; change someday
          ( (string=? device "JOSEPHSON_JUNCTION")
              (spice-sdb:write-josephson-junction package port)) 
          ( (string=? device "K")
              (spice-sdb:write-coupling-coefficient package port)) 
          ( (string=? device "model")
              (spice-sdb:write-model package port))
          ( (string=? device "options")
              (spice-sdb:write-options package port))
          ( (string=? device "directive")
              (spice-sdb:write-directive package port))
          ( (string=? device "include")
              (spice-sdb:write-include package port))
          ( else 
	      (spice-sdb:write-default-component package file-info-list port))
        ) ;; end of cond
	(spice-sdb:write-netlist port file-info-list (cdr ls))
	 ))))



;;----------------------------------------------------------------------
;; create-file-info-list: This takes as arugment the list of packages (refdesses).
;;   It runs through the package list, and for each gets the attributes.  If there is a
;;   "FILE" attribute, it gets the file info & uses it to build the
;;   file-info-list.  When done, it returns the file-info-list.
;;----------------------------------------------------------------------
(define spice-sdb:create-file-info-list
  (lambda (package-list file-info-list)     
     (if (null? package-list)
	file-info-list                          ;; end of packages processed.  Return file-info-list	 
	(let* ((package (car package-list))     ;; otherwise get next package (i.e. refdes)
	       (device (string))
	       (model (string))
	       (value (string))
	       (model-file (string))
	      )                                 ;; end of let* assignments

	  (set! device (get-device package) )  
	  (set! model (gnetlist:get-package-attribute package "model-name") )
	  (set! value (gnetlist:get-package-attribute package "value") )
	  (set! model-file (gnetlist:get-package-attribute package "file") )

	  ;; sometimes get-package-attribute returns "?" instead of "unknown".  WTF?  This should fix that . . .
	  (if (string-ci=? model-file "?")
	      (set! model-file "unknown"))

	  ;; Now run a series of checks to see if we should stick this file into the file-info-list
	  ;; Check to see if "file" attribute is non-empty
	  (if (not (string-ci=? model-file "unknown"))
	      (begin 
		(debug-spew 
		   (string-append "found file attribute for " package ".  File name = " model-file "\n"))  ;;  ******* Debug stuff
	      
	      ;; Now check to see if file is in file-info-list
		(if (not (spice-sdb:in-file-info-list? model-file file-info-list))

	      ;; File is new.  Open file, find out what type it is, and push info into file-info-list
		    (let ((file-type (spice-sdb:get-file-type model-file))
			 )
		      (debug-spew (string-append "File is new.  New file type is " file-type " \n"))      ;; DEBUG
		  
	      ;; Check to see if file-type is known.
		      (if (not (string=? file-type "OTHER"))
	      ;; file-type is OK.  Return file-info-list with new triplet attached.  
			  (begin
			    (debug-spew (string-append "Inserting " model-file " into list of known model files.\n"))
			    (set! file-info-list (append (list (list model model-file file-type)) file-info-list) )
			  )

              ;;  Otherwise, file type is not a model type.  Don't stick it in list.  Print debug spew if desired.
			  (debug-spew "File type is OTHER, and therefore will not be entered in known model file list.\n")
		       )   ;; end if (not (string=?

		     )  ;; end let ((file-type . . .

	      ;;  File is already in list.  Print debug spew if desired.
		    (debug-spew "File has already been seen and entered into known model file list.\n")
		)  ;; end if (spice-sdb:in-file-info-list . . .

	      )  ;; end begin . . .
	  )  ;; end if (not( string-ci=? model-file

	  ;; having done checking and processing of this package, iterate to the next one.
	  (spice-sdb:create-file-info-list (cdr package-list) file-info-list)

      )  ;; end let*
   )  ;; end if (null? package-list . . .
 ) 
)



;;  in-file-info-list? -- helper function.  Returns #t if file is already in file-info-list, otherwise #f
;;  assumes file-info-list of form: ((model1 file1 file-type1)  (model2 file2 file-type2) . . . .)
(define spice-sdb:in-file-info-list?
  (lambda (model-file file-info-list)
    (if (null? file-info-list)
	(begin
	  #f                                            ;; return #f if file-info-list itself is empty.
	)
	(let ((list-element (car file-info-list)) )   ;; otherwise process list-element
	  (if (null? list-element)
	      #f                                      ;;  item not found.  Return #f.  Note that we should never get here . . .
	      (let ((list-file-name (cadr list-element)) )
		(if (string=? list-file-name model-file)
		   #t                            ;; item found.  Return #t
		   (spice-sdb:in-file-info-list? model-file (cdr file-info-list))  ;; iterate . . .
	        )  ;; end if (string=?
	      )  ;; end of let . . .
	  )  ;; end if (null? list-element . . .
	)  ;; end let* ((list-element . . .
    )  ;; end if (null? file-info-list . .
))  ;; end define spice-sdb:in-file-info-list?




;;--------------------------------------------------------------
;; Write out spice netlist header
;;--------------------------------------------------------------
(define spice-sdb:write-top-header
  (lambda (port)
    (display "*********************************************************\n" port)
    (display "* Spice file generated by gnetlist                      *\n" port)
    (display "* spice-sdb version 4.28.2007 by SDB --                 *\n" port)
    (display "* provides advanced spice netlisting capability.        *\n" port)
    (display "* Documentation at http://www.brorson.com/gEDA/SPICE/   *\n" port)
    (display "*********************************************************\n" port)
  )
)

;;--------------------------------------------------------------
;; Write out .SUBCKT netlist header
;;--------------------------------------------------------------
(define spice-sdb:write-subcircuit-header
  (lambda (port)
    (display "*******************************\n" port)
    (display "* Begin .SUBCKT model         *\n" port)
    (display "* spice-sdb ver 4.28.2007     *\n" port)
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
;;   Hacked again in Sept 2003 to enable more intelligent embedding of external
;;       SPICE files into netlist -- SDB.
;;   The algorithm is as follows:
;;   1.  Figure out if there is a .SUBCKT block on the schematic,
;;       or if it is just a normal schematic.
;;       If a .SUBCKT:
;;       -- Write out subcircuit header (a comment identifying the netlister).
;;       -- find all spice-IO pins.  Get a list of the packages.
;;       -- put them in order (ordered by package refdes)
;;       -- get the list of nets attached to the spice-IO pins.
;;       -- write out .SUBCKT line
;;       If a normal schematic: 
;;       -- Write out top header (a comment identifying the netlister).
;;   2.  Loop through all components, looking for components with a "file"
;;       attribute.  Every time a "file" attribute is found do this:
;;       --  Open the file and find out what kind of file it is (.SUBCKT or .MODEL).
;;       --  Determine if the file has previously been processed.  If not: stick the 
;;           follwing info into the file-info list: (model-name file-name file-type). 
;;           Otherwise just continue.
;;   3.  Loop through all components again, and write out a SPICE card for each.  
;;   4.  Afterwards, for each item in the file-info list, open the file, and
;        write its contents into the netlist.
;;   5.  If the schematic-type is .SUBCKT:  write out .ENDS,  Otherwise: write out .END
;;   6.  Close up the SPICE netlist file and return.
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
	   (file-info-list (list))
	  )
      (display "Using SPICE backend by SDB -- Version of 4.28.2007\n")
      (display (string-append "schematic-type = " schematic-type "\n"))
      ;; (display (string-append "model-name = " model-name "\n"))

      (if (not (string=? schematic-type "normal schematic"))
      ;; we have found a .SUBCKT type schematic.
	  (let* ((io-pin-packages (spice-sdb:get-spice-IO-pins packages (list) ))
                 (io-pin-packages-ordered (spice-sdb:sort-spice-IO-pins io-pin-packages))
	         (io-nets-list (spice-sdb:get-IO-nets io-pin-packages-ordered (list) ))
                )
            (debug-spew "found .SUBCKT type schematic")
      ;; now write out .SUBCKT header and .SUBCKT line
	    (spice-sdb:write-subcircuit-header port)   
	    (let ((io-nets-string (list-2-string io-nets-list)) )
	      ;; (display (string-append "Found IO nets for subckt = " io-nets-string "\n"))   ;; DEBUG stuff . . .
	      ;; (write io-nets-list)
	      ;; (display "\n")
	      (display (string-append schematic-type " " (list-2-string io-nets-list) "\n") port)
	    )
	  )
	  
      ;; Otherwise it's a regular schematic.  Write out command line followed by comments in file header.
	  (begin
            (debug-spew "found normal type schematic")
	    (display (string-append "* " (gnetlist:get-command-line) "\n") port)
	    (spice-sdb:write-top-header port)   
	  )

      ) ;; end of if (not (string=? . . . .


;;
;; Now loop through all devices and process all "FILE" attributes.  Create
;; file-info-list.
;; Thanks to Carlos Nieves Onega for his e-mail to 
;; geda-dev which is the genesis of this section.
;;
      (debug-spew "\nMake first pass through design and create list of all model files referenced.\n")
      (set! file-info-list (spice-sdb:create-file-info-list packages file-info-list))
      (debug-spew "Done creating file-info-list.\n")

      ;;  extra debug spew -- comment out when done . . .
      ;; (display "file-info-list = \n")
      ;; (write file-info-list)
      ;; (display "\n")

      (debug-spew "\n")


;;
;;  Moved this loop before the next one to get numparam to work with ngspice,
;;  because numparam will at the subckt definition come before the main netlist.
;;  Change suggested by Dominique Michel; implemented in code on 6.12.2005.
;;
;;  Next loop through all items in file-info-list in the SPICE netlist.  
;;  For each model-name, open up the corresponding file, and call handle-spice-file 
;;  to stick the corresponding stuff into the output SPICE file.
;;
      (debug-spew "Now process the items in model file list -- stick appropriate references to models in output SPICE file.\n")
      (spice-sdb:loop-through-files file-info-list port)
      (debug-spew "Done processing items in model file list.\n")


;;
;; Now write out netlist as before.  But don't write file contents out.
;; **** Modified by kh to sort list of packages so Spice directives, etc. (A?) are output last,
;; **** and in increasing order.
;;
      (debug-spew "Make second pass through design and write out a SPICE card for each component found.\n")
      (display (string-append "*==============  Begin SPICE netlist of main design ============\n") port)
      (if (spice-sdb:sort-refdes? (gnetlist:get-calling-flags))
	  (spice-sdb:write-netlist port file-info-list (sort packages spice-sdb:packsort))  ;; sort on refdes
	  (spice-sdb:write-netlist port file-info-list packages)                            ;; don't sort.
      )
      (debug-spew "Done writing SPICE cards . . .\n\n")


;;
;;  Now write out .END(S) of netlist, depending upon whether this schematic is a
;;  "normal schematic" or a .SUBCKT.
;;
      (if (not (string=? schematic-type "normal schematic"))
          (begin 
	    (spice-sdb:write-bottom-footer (string-append ".ends " model-name) port)
	    (display "*******************************\n" port)
	  )
          (spice-sdb:write-bottom-footer ".end" port)
      )



;;
;;  Finally, close up and go home.
;;
      (close-output-port port)
      (debug-spew "\nOutput file is written.  We are done.\n")
   )    ;; (let* ((port . . . .
 )
)
