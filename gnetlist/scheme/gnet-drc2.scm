;;; gEDA - GPL Electronic Design Automation
;;; gnetlist - gEDA Netlist
;;; Copyright (C) 1998-2010 Ales Hvezda
;;; Copyright (C) 1998-2010 gEDA Contributors (see ChangeLog for details)
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

;; --------------------------------------------------------------------------
;;
;; DRC backend written by Carlos Nieves Onega starts here.
;;
;;  2010-12-11: Fix stack overflows with large designs.
;;  2010-10-02: Applied patch from Karl Hammar. Do drc-matrix lower triangular
;;                    and let get-drc-matrixelement swap row/column if row < column.
;;  2006-04-22: Display the pins when reporting a net with only one connection.
;;  2006-04-08: Added support for DRC directives (DontCheckPintypes and 
;;              NoConnection), so the DRC doesn't depend on the net name
;;              anymore.
;;              Changed the drc connection matrix. Now an unknown pin doesn't 
;;              generate an error, and it can drive a net.
;;              Added report for pins without the 'pintype' attribute.
;;  2006-04-05: Fixed parenthesis mismatch in function drc2:check-slots.
;;              Thanks to David Logan for reporting the bug.
;;  2006-03-02: Don't check pintypes of net "NoConnection". 
;;              Thanks to Holger Oehm for the bug report and providing 
;;              a patch. 
;;  2006-02-28: Added netname in the output message when checking pintype
;;              connections. Thanks to Holger Oehm for providing the patch. 
;;  2006-01-15: Changed error message to explain it a little bit.
;;  2006-01-07: Added missing 'passive' in the pintype-full-names list, and
;;              changed the pintype error/warning message to something more
;;              self-explaining.
;;  2005-02-11: Output to stdout if the output filename is "-".
;;  2005-02-08: Use a parameter instead of the quiet mode of gnetlist so 
;;              gnetlist doesn't return a non-zero value when there are only
;;              warnings. This parameter is 'ignore-warnings-in-return-value'.
;;  2005-02-06: Make gnetlist return a non-zero value when errors or warnings
;;              are found. If there is only warnings, the non-zero return value
;;              can be disabled using the "quiet mode" option of gnetlist.
;;  2005-02-06: Fixed bug when packages list is empty.
;;  2005-01-23: Added check for duplicated references.
;;  2003-10-24: Added numslots and slot attributes check.
;;  2003-06-17: Added configuration support and slots check.
;;  2003-06-05: Now checking for unconnected pins look into the DRC matrix if 
;;              it should issue an error, warning, or do nothing.
;;              If the drc-matrix is defined before the execution of the backend,
;;              then it's not overwritten. It allows backend configuration.
;;
;;  2003-06-04: Added check for unconnected pins and fix one small error (index limit error).
;;  2003-06-03: First release

;; Parameters
;; ----------
;; Parameters should be passed to the backed using -O option in gnetlist's
;; command line.
;;
;;   * ignore-warnings-in-return-value: By default, this backend makes gnetlist
;;        return a non-zero value when warnings or errors are found. This is 
;;        useful for Makefiles. Using this option, gnetlist will return a zero
;;        value if there are only DRC warnings.
;;
;; Output
;; ------
;; By default, the backend outputs to the filename specified in the command line, or to
;; stdout if the output filename is "-".
;; 
;; Configuration
;; -------------
;; 
;; Some test can be disabled defining some variables. Following is a list with a pair of check
;; and variable. If the variable is defined, then that check is not performed.
;;
;;       Check                                    Variable                       Value
;; -----------------------------------------------------------------------------------------------
;; Not numbered parts.                     dont-check-non-numbered-parts         whatever you want
;; Duplicated part references  (Note 1)    dont-check-duplicated-references      whatever you want
;; Nets with only one connection.          dont-check-one-connection-nets        whatever you want
;; Type of pins connected to each net.     dont-check-pintypes-of-nets           whatever you want
;; Net not driven.                         dont-check-not-driven-nets            whatever you want
;; Unconnected pins                        dont-check-unconnected-pins           whatever you want
;; Values of slot and numslots attribs.    dont-check-slots                      whatever you want
;; Slot is used more than one time.        dont-check-duplicated-slots           whatever you want
;; Reports unused slots                    dont-check-unused-slots               whatever you want
;;     Don't report anything               action-unused-slots                   #\c
;;     Report them as a warning            action-unused-slots                   #\w
;;     Report them as an error             action-unused-slots                   #\w
;;
;; Note 1: DRC checks are case sensitive by default. If you want them to be case insensitive, then you
;; only have to define the variable 'case_insensitive' to whatever value you want.
;;
;; Example:
;; (define dont-check-non-numbered-parts 1)
;; (define dont-check-duplicated-references 1)
;; (define dont-check-one-connection-nets 1)
;; (define dont-report-unknown-pintypes 1)
;; (define dont-check-pintypes-of-nets 1)
;; (define dont-check-not-driven-nets 1)
;; (define dont-check-unconnected-pins 1)
;; (define dont-check-duplicated-slots 1)
;; (define dont-check-unused-slots 1)
;; (define action-unused-slots #\w)
;; (define case_insensitive 1)
;;
;; The check for not driven nets only is performed when checking the type of the pins connected 
;; to each net.
;; There is a list which specifies which type of pin can drive a net. It's called pintype-can-drive.
;; It's a list, with 0 or 1 integer elements. The order is specified below and is very important, since
;; each position in the list matches one type of pin. This list can be specified before running this 
;; backend, otherwise, the backend will use the default values.
;;
;; Example:
;;   (define pintype-can-drive (list 0 0 1 1 1 1 1 1 1 0 1 0 ))
;;
;; There are two checks that are configurable by a DRC connection matrix: check for unconnected pins 
;; and check for the type of pins connected to each net.
;; Each element of the DRC matrix matches one connection between two pins (the "row" pin and the "column"
;; pin). The order is specified below and is very important, since each position in the list matches 
;; one type of pin.
;; The DRC matrix can be specified before running this backend. Otherwise, the backend will use the
;; default values.
;;
;; Example (default matrix):
;;
;;    (define drc-matrix (list
;;;  Order is important !
;;;             unknown in    out   io    oc    oe    pas   tp    tri   clk   pwr unconnected
;;;unknown
;;  '(            #\c )
;;;in
;;  '(            #\c   #\c)
;;;out
;;  '(            #\c   #\c   #\e )
;;;io
;;  '(            #\c   #\c   #\w   #\c)
;;;oc
;;  '(            #\c   #\c   #\e   #\w   #\e)
;;;oe
;;  '(            #\c   #\c   #\e   #\w   #\c   #\e)
;;;pas
;;  '(            #\c   #\c   #\c   #\c   #\c   #\c   #\c)
;;;tp
;;  '(            #\c   #\c   #\e   #\w   #\e   #\e   #\c   #\e)
;;;tri
;;  '(            #\c   #\c   #\e   #\c   #\c   #\c   #\c   #\e   #\c)
;;;clk
;;  '(            #\c   #\c   #\c   #\c   #\c   #\c   #\c   #\c   #\c   #\c)
;;;pwr
;;  '(            #\c   #\c   #\e   #\w   #\e   #\e   #\c   #\e   #\e   #\e   #\c)
;;;unconnected
;;  '(            #\e   #\e   #\e   #\e   #\e   #\e   #\e   #\e   #\e   #\e   #\e   #\e )))



;; -------------------------------------------------------------------------------
;; IMPORTANT: Don't modify anything below unless you know what you are doing.
;; -------------------------------------------------------------------------------

(use-modules (srfi srfi-1))
(use-modules (ice-9 syncase))

(define-syntax define-undefined
  (syntax-rules ()
    ((_ name expr)
     (define name (if (defined? (quote name)) name expr)))))

;;
;; Some internal definitions
;;


; Pintype definitions. Overwrite previous definitions, because the backend depends on them.
(define unknown  0)
(define in       1)
(define out      2)
(define io       3)
(define oc       4)
(define oe       5)
(define pas      6)
(define tp       7)
(define tri      8)
(define clk      9)
(define pwr     10)
(define undefined 11)
(define pintype-names (list "unknown" "in" "out" "io" "oc" "oe" "pas" "tp" "tri" "clk" "pwr" "unconnected"))
(define pintype-full-names (list "unknown" "input" "output" "input/output" "open collector" "open emitter" "passive" "totem-pole" "tristate" "clock" "power" "unconnected"))

; define if a specified pin can drive a net
(define-undefined pintype-can-drive
;         unk in out io oc oe pas tp tri clk pwr undef
  (list 1   0  1   1  1  1  1   1  1   0   1    0 ))

(define (pintype-can-drive-valid? lst)
  (define (int01? x)
    (and (integer? x)
         (or (= x 0)
             (= x 1))))
  (and (list? lst)
       (= (length lst) (length pintype-names))
       (every int01? lst)))

(or (pintype-can-drive-valid? pintype-can-drive)
    (begin
      (display "INTERNAL ERROR: List of pins which can drive a net bad specified. Using default value.")
      (newline)
      (set! pintype-can-drive 1)))

; DRC matrix
;
; #\e: error    #\w: warning   #\c: correct
(define-undefined drc-matrix
  (list
;  Order is important !
;             unknown in    out   io    oc    oe    pas   tp    tri   clk   pwr unconnected
;unknown
  '(            #\c )
;in
  '(            #\c   #\c   )
;out
  '(            #\c   #\c   #\e   )
;io
  '(            #\c   #\c   #\w   #\c   )
;oc
  '(            #\c   #\c   #\e   #\w   #\e   )
;oe
  '(            #\c   #\c   #\e   #\w   #\c   #\e   )
;pas
  '(            #\c   #\c   #\c   #\c   #\c   #\c   #\c   )
;tp
  '(            #\c   #\c   #\e   #\w   #\e   #\e   #\c   #\e   )
;tri
  '(            #\c   #\c   #\e   #\c   #\c   #\c   #\c   #\e   #\c   )
;clk
  '(            #\c   #\c   #\c   #\c   #\c   #\c   #\c   #\c   #\c   #\c   )
;pwr
  '(            #\c   #\c   #\e   #\w   #\e   #\e   #\c   #\e   #\e   #\e   #\c  )
;unconnected
  '(            #\e   #\e   #\e   #\e   #\e   #\e   #\e   #\e   #\e   #\e   #\e   #\e )
))

;; Number of errors and warnings found
(define errors_number 0)
(define warnings_number 0)

(define-undefined action-unused-slots #\w)

(if (or (not (char? action-unused-slots))
        (not (or (char=? action-unused-slots #\w)
                 (char=? action-unused-slots #\c)
                 (char=? action-unused-slots #\e))))
    (begin
      (display "INTERNAL ERROR: Action when unused slots are found has a wrong value. Using default.")
      (newline)
      (set! action-unused-slots #\w)))

;-----------------------------------------------------------------------
;   DRC matrix functions
;

; Get the position of a pintype in the list, by its pintype name ("io", "in",...)
(define drc2:position-of-pintype 
  (lambda (type)
    (- (length pintype-names) (length (member (string-downcase type) pintype-names)))))

; Get the full name of a specified position in the pintype list.
(define drc2:get-full-name-of-pintype-by-number
  (lambda (type)
    (list-ref pintype-full-names type)))

; Get the full name of a specified pintype short name. (i.e "io" -> "input/output")
(define drc2:get-full-name-of-pintype-by-name
  (lambda (type)
    (list-ref pintype-full-names (drc2:position-of-pintype (string-downcase type)))))

; Get value x y from matrix
(define drc2:get-drc-matrix-element
  (lambda (row column)
    (if (< row column)
	(list-ref (list-ref drc-matrix column) row)
	(list-ref (list-ref drc-matrix row) column))))
  
; Check if all elements of the DRC matrix are characters
(define drc2:drc-matrix-elements-are-correct?
  (lambda ()
    (let check-row ((row 0))
      (if (let check-column ((column 0)) 
	    (if (not (char? (drc2:get-drc-matrix-element row column)))
		#f
		(if (< column (- (length pintype-names) 1))
		    (check-column (+ column 1)) 		    
		    #t)
		)
	    )
	  (if (< row (- (length pintype-names) 1))
	      (check-row (+ row 1)) 
	      #t)	  
	 #f)
      )
      
))

;
; End of DRC matrix functions
;-----------------------------------------------------------------------

;-----------------------------------------------------------------------
; SYMBOLS checking functions
;

;;
;; Check for symbols not numbered.
;;
;; example of packages: (U100 U101 U102)
(define drc2:check-non-numbered-items
   (lambda (port packages)
      (if (not (null? packages))
         (let ((package (car packages)))
            (begin
	       (if (not (eq? (string-index package #\?) #f))
		   (begin (display "ERROR: Reference not numbered: " port)
			  (display package port)
			  (newline port)
			  (set! errors_number (+ errors_number 1))
			  )
		   )
	       (drc2:check-non-numbered-items port (cdr packages)))))))


;;
;; Check for duplicated slots
;;
;; Check if a slot of a package is used more than one time. Checks all packages in the design.
(define drc2:check-duplicated-slots
  (lambda (port)
    (define check-duplicated-slots-of-package
      (lambda (uref)
	(define check-slots-loop
	  (lambda (slots_list)
	    (if (> (length slots_list) 1)
		(begin
		  (if (member (car slots_list) (cdr slots_list))
		      (begin
			(display (string-append "ERROR: duplicated slot " 
						(number->string (car slots_list))
						" of uref "
						uref) port)
			(newline port)
			(set! errors_number (+ errors_number 1))))
		  (check-slots-loop (cdr slots_list))
		  ))))
	(check-slots-loop (gnetlist:get-slots uref))))
    (for-each check-duplicated-slots-of-package packages)
))



;;
;; Checks for slots not used.
;;
(define drc2:check-unused-slots
  (lambda (port)
    (define check-unused-slots-of-package
      (lambda (uref)

 	(define check-slots-loop
 	  (lambda (slot_number slots_list)
 	    (let ( (numslots (string->number (gnetlist:get-package-attribute uref "numslots"))) )
	      (if (not (member slot_number slots_list))
		  (begin
		    (if (not (char=? action-unused-slots #\c))
			(begin
			  (if (char=? action-unused-slots #\e)
			      (begin 
				(display (string-append "ERROR: Unused slot "
							(number->string slot_number)
							" of uref " uref) port)
				(set! errors_number (+ errors_number 1)))
			      (begin
				(display (string-append "WARNING: Unused slot "
							(number->string slot_number)
							" of uref " uref) port)
				(set! warnings_number (+ warnings_number 1))))
			  (newline port)))))
	      (if (< slot_number numslots)
		  (check-slots-loop (+ slot_number 1) slots_list)))))

	(if (integer? (string->number (gnetlist:get-package-attribute uref "numslots")))
	    (check-slots-loop 1 (gnetlist:get-unique-slots uref))
	    )
	))

    (for-each check-unused-slots-of-package packages)
    ))

;;
;; Check slot number is greater or equal than numslots for all packages
;;
(define drc2:check-slots
  (lambda (port)
    (define check-slots-of-package
      (lambda (uref)
	
	(let* ( (numslots_string (gnetlist:get-package-attribute uref "numslots"))
		(numslots (string->number numslots_string))
		(slot_string (let ((slots (gnetlist:get-all-package-attributes uref "slot")))
                               (if (or (null? slots) (not (car slots)))
                                   "unknown" (car slots))))
		(slot (string->number slot_string))
		)
	  (let ()
	    (define check-slots-loop
	      (lambda (slots_list)
		(if (not (null? slots_list))
		    (let ((this_slot (car slots_list)))
		      (if (integer? this_slot)
			  (if (not (and (<= this_slot numslots) (>= this_slot 1)))
			      ;; If slot is not between 1 and numslots, then report an error.
			      (begin
				(display (string-append "ERROR: Reference " uref 
							": Slot out of range (" 
							(number->string this_slot)
							").") port)
				(newline port)
				(set! errors_number (+ errors_number 1)))))
		      
		      (check-slots-loop (cdr slots_list))
		      ))))
	    
	    (if (string-ci=? slot_string "unknown")
		(begin
		  ;; If slot attribute is not defined.
		  (if (or (string-ci=? numslots_string "unknown") (= numslots 0))
		      (begin
			;; No slot neither numslots (or set to zero) attributes defined.
			;; This is correct.
			;;(display (string-append "No slotted reference: " uref))
			(display "")
			;;(newline)
			)
		      (begin
			;; Slot not defined, but numslots defined or different than 0.
			;; This is incorrect. Check if numslots is a number and
			;; report the situation to the user.
			(if (integer? numslots)
			    ;; If no slot attribute, but numslots is defined and not zero.
			    (begin
			      ;; If numslots is a number, then slot should be defined.
			      (display (string-append "ERROR: Multislotted reference " uref 
						      " has no slot attribute defined.") port)
			      (newline port)
			      (set! errors_number (+ errors_number 1)))
			    (begin
			      (display (string-append "ERROR: Reference " uref 
						      ": Incorrect value of numslots attribute ("
						      numslots_string ").") 
				       port)
			      (newline port)
			       (set! errors_number (+ errors_number 1))
			      )
			    )
			))
		  )
		(begin
		  ;; Slot attribute defined.
		  ;; If it's a number, then check slots. If it's not, then report an error.
		  (if (integer? slot)
		      (if (integer? numslots)
			  (check-slots-loop (gnetlist:get-unique-slots uref))
			  (begin
			    ;; Slot is defined and it's a number, but numslots it's not a number.
			    (display (string-append "ERROR: Reference " uref
						    ": Incorrect value of numslots attribute ("
						    numslots_string ").") port)
			    (newline port)
			    (set! errors_number (+ errors_number 1))))
		      (begin
			;; Slot attribute is not a number.
			(display (string-append "ERROR: Reference " uref 
						": Incorrect value of slot attribute ("
						slot_string ").") port)
			(newline port)
			(set! errors_number (+ errors_number 1))))
		  ))))))
    

    (for-each check-slots-of-package packages)
    ))

;; Count the ocurrences of a given reference in the given list.
(define (drc2:count-reference-in-list refdes lst)
  (define refdes=? (if (defined? 'case_insensitive) string-ci=? string=?))
  (fold
   (lambda (x count) (if (refdes=? refdes x) (1+ count) count))
   0 lst))

;; Check duplicated references of the given list
;;   If the number of ocurrences of a reference in the schematic doesn't match the number
;;   of unique slots used by that part, then that reference is used more than one time in
;;   the schematic.
(define drc2:check-duplicated-references 
  (lambda (port list)
    (if (null? list)
	0
	(let ( (refdes (car list)))
	       (if (> (drc2:count-reference-in-list refdes (gnetlist:get-non-unique-packages ""))
		      (length (gnetlist:get-unique-slots refdes)))
		   (begin
		     (display (string-append "ERROR: Duplicated reference " refdes ".") port)
		     (newline port)
		     (set! errors_number (+ errors_number 1))))
	       (drc2:check-duplicated-references port (cdr list))
	       ))
))


;
;  End of symbol checking functions
;-----------------------------------------------------------------------


;-----------------------------------------------------------------------
;  NETs checking functions
;

;;
;; Check for NoConnection nets with more than one pin connected.
;;
;; Example of all-nets: (net1 net2 net3 net4)
(define (drc2:check-connected-noconnects port all-nets)
  (for-each
    (lambda (netname)
      (let
        ((directives (gnetlist:graphical-objs-in-net-with-attrib-get-attrib
                    netname
                    "device=DRC_Directive"
                    "value")))
        ;Only check nets with a NoConnection directive
        (and
          (member "NoConnection" directives)
          ( >  (length (gnetlist:get-all-connections netname)) '1)
          (begin
            (display (string-append "ERROR: Net '"
                            netname "' has connections, but "
                            "has the NoConnection DRC directive: ") port)
            (drc2:display-pins-of-type port "all" (gnetlist:get-all-connections netname))
            (display "." port)
            (newline port)
            (set! errors_number (1+ errors_number))))))
    all-nets))

;;
;; Check for nets with less than two pins connected.
;;
;; Example of all-nets: (net1 net2 net3 net4)
(define drc2:check-single-nets
  (lambda (port all-nets)
      (if (not (null? all-nets))
	  (let* ((netname (car all-nets))
		 (directives (gnetlist:graphical-objs-in-net-with-attrib-get-attrib
			      netname
			      "device=DRC_Directive"
			      "value")))
	    (begin
	      ; If one of the directives is NoConnection, 
	      ; then it shouldn't be checked.
 	      (if (not (member "NoConnection" directives))
		  (begin
		    (if (eq? (length (gnetlist:get-all-connections netname)) '0)
			(begin (display (string-append "ERROR: Net '"
						       netname "' has no connections.") port)
			       (newline port)
			       (set! errors_number (+ errors_number 1))
			       )                      
			)
		    (if (eq? (length (gnetlist:get-all-connections netname)) '1)
			(begin (display (string-append "ERROR: Net '"
						       netname "' is connected to only one pin: ") port)
			       (drc2:display-pins-of-type port "all" (gnetlist:get-all-connections netname))
			       (display "." port)
			       (newline port)
			       (set! errors_number (+ errors_number 1))
			       )                      
			)
		    ))
	      (drc2:check-single-nets port (cdr all-nets)))))
  ))

;;
;; Return a list with the pintypes of the pins connected to a net.
;;
;; Example. net-conn: ((U100 1) (U101 1)). pintypes-list: ("in" "out" "in")
(define drc2:get-pintypes-of-net-connections
  (lambda (net-conn pintypes-list)
    (if (not (null? net-conn))
	(let* ( (element (car net-conn)) 
		(device (car element))
		(pin (car (cdr (car net-conn))))
		(pintype (gnetlist:get-attribute-by-pinnumber device pin "pintype"))
		)
	  (begin
	    (cons pintype 
		  (drc2:get-pintypes-of-net-connections (cdr net-conn)
							  pintypes-list)
		  )
	    ))
	(list)
	)
))

;;
;;  Count pintypes of a net.
;;
;; net: "in", "out", for example.
(define drc2:count-pintypes-of-net
  (lambda (net port)
    (define output-list (make-list (length pintype-names) 0))
    (define add-pintype
      (lambda (type)
	   (if (not (member (string-downcase type) pintype-names))
	       (begin
		 (display "INTERNAL ERROR: unknown pin type : " port)
		 (display type port)
		 (newline port))
	       (begin
		 (list-set! output-list (drc2:position-of-pintype type)
                                       (+ 1 (list-ref output-list (drc2:position-of-pintype type))))))
	   ))
    (for-each add-pintype net)
    output-list
))


;;
;; Display pins of a specified type connected to a net
;;
;; type: number of the position of the type in the vector, or 
;;       the string "all" to display all the pins.
;; connections: ((U100 1) (U101 1)), for example.
(define drc2:display-pins-of-type
  (lambda (port type connections)
    (if (not (null? connections))
	(begin
	  (let ((device (car (car connections)))
		(pin (car (cdr (car connections)))))
	    (if (or (and (string? type) (string-ci=? type "all"))
		    (string-ci=? (list-ref pintype-names type)
				 (gnetlist:get-attribute-by-pinnumber device pin "pintype"))
		    )
		(begin
		  (display device port)
		  (display ":" port)
		  (display pin port)
		  (display " " port)))
	    (drc2:display-pins-of-type port type (cdr connections))
	    ""
	    )))))

;;
;; Check connection between two pintypes
;;
;; type1,type2: number of the position of the type in the vector.
;; connections: ((U100 1) (U101 1)), for example.
(define drc2:check-connection-of-two-pintypes
  (lambda (port type1 type2 connections netname)
    (let* (( drc-matrix-value (drc2:get-drc-matrix-element type1 type2)))
      (cond
       ((eqv? drc-matrix-value #\c) 1)
       (else (if (and (not (eqv? drc-matrix-value #\e)) (not (eqv? drc-matrix-value #\w)))
		 (begin
		   (display "INTERNAL ERROR: DRC matrix has unknown value on position " port)
		   (display type1 port)
		   (display "," port)
		   (display type2 port)
		   (newline port)
		   (error "INTERNAL ERROR: DRC matrix has unknown value. See output for more information"))
		 
		 (begin 
		   (if (eqv? drc-matrix-value #\w) 
		       (begin
			 (display "WARNING: " port)
			 (set! warnings_number (+ warnings_number 1)))
		     (begin 
		       (display "ERROR: " port)
		       (set! errors_number (+ errors_number 1))
		       ))	  
		   (display "Pin(s) with pintype '" port)
		   (display (drc2:get-full-name-of-pintype-by-number type1) port)
		   (display "': " port)
		   (display (drc2:display-pins-of-type port type1 
							 connections) port)
		   (display (string-append "\n\tare connected by net '" netname) port)
                   (display "'\n\tto pin(s) with pintype '" port)
		   (display (drc2:get-full-name-of-pintype-by-number type2) port)
		   (display "': " port)
		   (display (drc2:display-pins-of-type port type2
							 connections) port)
		   (newline port)
		   )
		 ))))))

;;
;; Check pintypes of the pins connected to a single net
;;
;; type1,type2: number of the position of the type in the vector.
;; connections: ((U100 1) (U101 1)), for example.
;; pintype-count: vector with the number of pins connected to a single net, by pintype.
;;     (1 2 3 4 ... 10), for example.
(define drc2:check-pintypes-of-single-net
  (lambda (port connections pintypes pintype-count type1 type2 netname)
    (define type1-count (list-ref pintype-count type1))
    (define type2-count (list-ref pintype-count type2))
    (define next-type1 
      (lambda (port connections pintypes pintype-count type1 type2 netname)
	(if (< type1 (- (length pintype-names) 2))
	    (drc2:check-pintypes-of-single-net port connections pintypes pintype-count 
						 (+ type1 1) (+ type1 1) netname)	
	    )
	))
    (define next-type2
      (lambda (port connections pintypes pintype-count type1 type2 netname)
	(if (< type2 (- (length pintype-names) 2))
	    (drc2:check-pintypes-of-single-net port connections pintypes pintype-count 
						 type1 (+ type2 1) netname)
	    (next-type1 port connections pintypes pintype-count type1 type1 netname)
	    )))
    
					; Check type1 with type1 first
    (if (= type1-count 0)
					; if no pins of type1 connected, then continue with (+ type1 1)
	(begin
	  (next-type1 port connections pintypes pintype-count type1 type2 netname))
	  
    (if (= type1 type2)
	(if (> type1-count 1)
	    (begin
	      (drc2:check-connection-of-two-pintypes port type1 type1 connections netname)
	      (next-type2 port connections pintypes pintype-count type1 type2 netname)
	      
	      )
	      (next-type2 port connections pintypes pintype-count type1 type2 netname))
	(begin
      (if (= type2-count 0)
					; if no pins of type2 connected, then continue with (+ type2 1)
	  (next-type2 port connections pintypes pintype-count type1 type2 netname)
	  )
      (if (and (> type1-count 0) (> type2-count 0))
	  (begin	  
					; Check connections between type1 and type2.
	    (drc2:check-connection-of-two-pintypes port type1 type2 connections netname)
					; and continue with the next type2 if within the limits
	    (next-type2 port connections pintypes pintype-count type1 type2 netname)
	    ))
    )
    ))))

;; 
;; Check if a net has a pintype which can drive the net.
;;
;; pintype-count: vector with the number of pins connected to a single net, by pintype.
;;     (1 2 3 4 ... 10), for example.
;; position: number of the position the function is checking.
(define drc2:check-if-net-is-driven
  (lambda (pintype-count position)
    (if (< position (- (length pintype-names) 1))
	(if (and (> (list-ref pintype-count position) 0)
		 (= (list-ref pintype-can-drive position) 1))
	    #t
	    (drc2:check-if-net-is-driven pintype-count (+ position 1)))
	#f)))

;;
;; Check pintype of the pins connected to every net in the design.
;;
;; all-nets: (net1 net2 net3), for example
(define drc2:check-pintypes-of-nets
  (lambda (port all-nets)
      (if (not (null? all-nets))
	  (let ((netname (car all-nets)))
	    (begin	
	      (let*  ( (connections (gnetlist:get-all-connections netname))
		       (pintypes    (drc2:get-pintypes-of-net-connections 
				     connections
				     '()))
		       (pintype-count (drc2:count-pintypes-of-net pintypes port))
		       (directives (gnetlist:graphical-objs-in-net-with-attrib-get-attrib
				    netname
				    "device=DRC_Directive"
				    "value"))
		       )
		; If some directives are defined, then it shouldn't be checked.
		(if (not (member "DontCheckPintypes" directives))
		    (drc2:check-pintypes-of-single-net port connections pintypes pintype-count 0 0 netname))
		(if (not (defined? 'dont-check-not-driven-nets))
		    (begin
		      (if (and (not (member "DontCheckIfDriven" directives))
			       (not (member "NoConnection" directives)))
			  (if (eqv? (drc2:check-if-net-is-driven pintype-count 0) #f)
			      (begin
				(set! errors_number (+ errors_number 1))
				(display "ERROR: Net " port)
				(display netname port)
				(display " is not driven." port)
				(newline port)
				))
			  )
		      ))
		
		)
	      (drc2:check-pintypes-of-nets port (cdr all-nets))
  )))
))

;;
;; Check unconnected pins
;;
;; ref-list: ("U1" "U2"), for example.
;; pin-net: ( (pin net) (pin net) ... )
(define drc2:check-unconnected-pins
  (lambda (port ref-list pin-net)
    (define ref "")
    (if (not (null? ref-list))
	(begin
	  (set! ref (car ref-list))
	  (if (not (null? pin-net))
	      (let* ( (pair (car pin-net)) 
		      (pin (car pair)) 
		      (connection (cdr pair))
		      )
		(begin
		  (if (strncmp? connection "unconnected_pin" 15)
		      (begin
			(let* ((position (drc2:position-of-pintype 
					  (gnetlist:get-attribute-by-pinnumber ref pin "pintype")))
			       (drc-matrix-value (drc2:get-drc-matrix-element undefined position)))
			  (begin
			    (if (eqv? drc-matrix-value #\c)
				#t
				(begin
				  (if (eqv? drc-matrix-value #\w) 
				      (begin
					(display "WARNING: " port)
					(set! warnings_number (+ warnings_number 1)))
				      (begin 
					(display "ERROR: " port)
					(set! errors_number (+ errors_number 1))
					))	
				  (display "Unconnected pin " port)
				  (display ref port)
				  (display ":" port)
				  (display pin port)
				  (newline port)
				  (drc2:check-unconnected-pins port ref-list (cdr pin-net))
				  ))
			  ))
			)
		      (drc2:check-unconnected-pins port ref-list (cdr pin-net))
		  )
		))
	      (if (> (length ref-list) 1)
		  (drc2:check-unconnected-pins port (cdr ref-list) 
					       (gnetlist:get-pins-nets (car (cdr ref-list)))))
	    ))
	)
    ))

; Report pins without the 'pintype' attribute (pintype=unknown)
(define (drc2:report-unknown-pintypes port nets)
  (define (count-unknown-pintypes nets)
    (fold
     (lambda (netname count)
       (let* ((connections (gnetlist:get-all-connections netname))
              (pintypes (drc2:get-pintypes-of-net-connections connections '()))
              (pintype-count (drc2:count-pintypes-of-net pintypes port)))
         (+ count
            (list-ref pintype-count (drc2:position-of-pintype "unknown")))))
     0 nets))
  (define (display-unknown-pintypes nets)
    (for-each
     (lambda (netname)
       (drc2:display-pins-of-type port
                                  (drc2:position-of-pintype "unknown")
                                  (gnetlist:get-all-connections netname)))
     nets))
  (and (> (count-unknown-pintypes nets) 0)
       (begin
         (display "NOTE: Found pins without the 'pintype' attribute: " port)
         (display-unknown-pintypes nets)
         (display "\n"))))

;
;  End of Net checking functions
;-----------------------------------------------------------------------




;;; Highest level function
;;; Write my special testing netlist format
;;;
(define drc2
   (lambda (output-filename)
      (let ((port (if (string=? "-" output-filename)
		      (current-output-port)
		      (open-output-file output-filename))))
         (begin
		    
	    ;; Perform DRC-matrix sanity checks.
	    ; See if all elements of the matrix are chars
	    (if (not (drc2:drc-matrix-elements-are-correct?))
		(begin (display "INTERNAL ERROR: DRC matrix elements are NOT all chars." port)
		       (newline port)
		       (newline port)
		       (error "INTERNAL ERROR. DRC matrix elements are NOT all chars.")))

 	    ;; Check non-numbered symbols
	    (if (not (defined? 'dont-check-non-numbered-parts))
		(begin
		  (display "Checking non-numbered parts..." port)
		  (newline port)
		  (drc2:check-non-numbered-items port packages)
		  (newline port)))

	    ;; Check for duplicated references   
	    (if (not (defined? 'dont-check-duplicated-references))
		(begin
		  (display "Checking duplicated references..." port)
		  (newline port)
		  (drc2:check-duplicated-references port packages)
		  (newline port)))

	    ;; Check for NoConnection nets with more than one pin connected.
	    (if (not (defined? 'dont-check-connected-noconnects))
		(begin
		  (display "Checking NoConnection nets for connections..." port)
		  (newline port)
		  (drc2:check-connected-noconnects port (gnetlist:get-all-unique-nets "dummy"))
		  (newline port)))

	    ;; Check nets with only one connection
	    (if (not (defined? 'dont-check-one-connection-nets))
		(begin
		  (display "Checking nets with only one connection..." port)
		  (newline port)
		  (drc2:check-single-nets port (gnetlist:get-all-unique-nets "dummy"))
		  (newline port)))

	    ;; Check "unknown" pintypes
	    (if (not (defined? 'dont-report-unknown-pintypes))
		(begin
		  (display "Checking pins without the 'pintype' attribute..." port)
		  (newline port)
		  (drc2:report-unknown-pintypes port (gnetlist:get-all-unique-nets "dummy"))
		  (newline port)))
	    
	    ;; Check pintypes of the pins connected to every net
	    (if (not (defined? 'dont-check-pintypes-of-nets))
		(begin
		  (display "Checking type of pins connected to a net..." port)
		  (newline port)
		  (drc2:check-pintypes-of-nets port (gnetlist:get-all-unique-nets "dummy"))
		  (newline port)))
	    
	    ;; Check unconnected pins
	    (if (not (defined? 'dont-check-unconnected-pins))
		(begin
		  (display "Checking unconnected pins..." port)
		  (newline port)
		  (if (not (null? packages))
		      (drc2:check-unconnected-pins port packages (gnetlist:get-pins-nets (car packages))))
		  (newline port)))

	    ;; Check slots   
	    (if (not (defined? 'dont-check-slots))
		(begin
		  (display "Checking slots..." port)
		  (newline port)
		  (drc2:check-slots port)
		  (newline port)))

	    ;; Check for duplicated slots   
	    (if (not (defined? 'dont-check-duplicated-slots))
		(begin
		  (display "Checking duplicated slots..." port)
		  (newline port)
		  (drc2:check-duplicated-slots port)
		  (newline port)))

	    ;; Check for unused slots
	    (if (not (defined? 'dont-check-unused-slots))
		(begin
		  (display "Checking unused slots..." port)
		  (newline port)
		  (drc2:check-unused-slots port)
		  (newline port)))

	    ;; Display total number of warnings
	    (if (> warnings_number 0)
		(begin
		  (display "Found " port)
		  (display warnings_number port)
		  (display " warnings." port)
		  (newline port))
		(begin
		  (display "No warnings found. " port)
		  (newline port)))

	    ;; Display total number of errors
	    (if (> errors_number 0)
		(begin
		  (display "Found " port)
		  (display errors_number port)
		  (display " errors." port)
		  (newline port))
		(begin
		  (display "No errors found. " port)
		  (newline port)))

         (close-output-port port)
	 
	 ;; Make gnetlist return an error if there are DRC errors.
	 ;; If there are only warnings and it's in quiet mode, then
	 ;; do not return an error.
	 (if (> errors_number 0)
	     (begin (display "DRC errors found. See output file.")
                    (newline))
	     (if (> warnings_number 0)
		 (if (not (calling-flag? "ignore-warnings-in-return-value" (gnetlist:get-calling-flags)))
		     (begin (display "DRC warnings found. See output file.")
                            (newline)))))

	 ))))


;;
;; DRC backend written by Carlos Nieves Onega ends here.
;;
;; --------------------------------------------------------------------------

