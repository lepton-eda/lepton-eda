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

;; --------------------------------------------------------------------------
;;
;; DRC backend written by Carlos Nieves Onega starts here.
;;
;;
;;  2003-06-17: Added configuration support and slots check.
;;  2003-06-05: Now checking for unconnected pins look into the DRC matrix if 
;;              it should issue an error, warning, or do nothing.
;;              If the drc-matrix is defined before the execution of the backend,
;;              then it's not overwritten. It allows backend configuration.
;;
;;  2003-06-04: Added check for unconnected pins and fix one small error (index limit error).
;;  2003-06-03: First release

;; Configuration
;; -------------
;; 
;; Some test can be disabled defining some variables. Following is a list with a pair of check
;; and variable. If the variable is defined, then that check is not performed.
;;
;;       Check                                    Variable                       Value
;; -----------------------------------------------------------------------------------------------
;; Not numbered parts.                     dont-check-non-numbered-parts         whatever you want
;; Nets with only one connection.          dont-check-one-connection-nets        whatever you want
;; Type of pins connected to each net.     dont-check-pintypes-of-nets           whatever you want
;; Net not driven.                         dont-check-not-driven-nets            whatever you want
;; Unconnected pins                        dont-check-unconnected-pins           whatever you want
;; Slot is used more than one time.        dont-check-duplicated-slots           whatever you want
;; Reports unused slots                    dont-check-unused-slots               whatever you want
;;     Don't report anything               action-unused-slots                   #\c
;;     Report them as a warning            action-unused-slots                   #\w
;;     Report them as an error             action-unused-slots                   #\w
;;
;; Example:
;; (define dont-check-non-numbered-parts 1)
;; (define dont-check-one-connection-nets 1)
;; (define dont-check-pintypes-of-nets 1)
;; (define dont-check-not-driven-nets 1)
;; (define dont-check-unconnected-pins 1)
;; (define dont-check-duplicated-slots 1)
;; (define dont-check-unused-slots 1)
;; (define action-unused-slots #\w)
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
;;  '(            #\e   #\e   #\e   #\e   #\e   #\e   #\e   #\e   #\e   #\e   #\e   #\e )
;;;in
;;  '(            #\e   #\c   #\c   #\c   #\c   #\c   #\c   #\c   #\c   #\c   #\c   #\e )
;;;out
;;  '(            #\e   #\c   #\e   #\w   #\e   #\e   #\c   #\e   #\e   #\c   #\e   #\e )
;;;io
;;  '(            #\e   #\c   #\w   #\c   #\w   #\w   #\c   #\w   #\c   #\c   #\w   #\e )
;;;oc
;;  '(            #\e   #\c   #\e   #\w   #\e   #\c   #\c   #\e   #\c   #\c   #\e   #\e )
;;;oe
;;  '(            #\e   #\c   #\e   #\w   #\c   #\e   #\c   #\e   #\c   #\c   #\e   #\e )
;;;pas
;;  '(            #\e   #\c   #\c   #\c   #\c   #\c   #\c   #\c   #\c   #\c   #\c   #\e )
;;;tp
;;  '(            #\e   #\c   #\e   #\w   #\e   #\e   #\c   #\e   #\e   #\c   #\e   #\e )
;;;tri
;;  '(            #\e   #\c   #\e   #\c   #\c   #\c   #\c   #\e   #\c   #\c   #\e   #\e )
;;;clk
;;  '(            #\e   #\c   #\c   #\c   #\c   #\c   #\c   #\c   #\c   #\c   #\e   #\e )
;;;pwr
;;  '(            #\e   #\c   #\e   #\w   #\e   #\e   #\c   #\e   #\e   #\e   #\c   #\e )
;;;unconnected
;;  '(            #\e   #\e   #\e   #\e   #\e   #\e   #\e   #\e   #\e   #\e   #\e   #\e )))



;; -------------------------------------------------------------------------------
;; IMPORTANT: Don't modify anything below unless you know what you are doing.
;; -------------------------------------------------------------------------------

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
(define pintype-full-names (list "unknown" "input" "output" "input/output" "open collector" "open emitter" "totem-pole" "tristate" "clock" "power" "unconnected"))

; define if a specified pin can drive a net
(if (defined? 'pintype-can-drive)
    (begin
      (define is-integer-list?
	(lambda (list)
	  (if (not (null? list))
	      (if (integer? (car list))
		  (if (or (< (car list) 0)
			  (> (car list) 1))
		      #f
		      (is-integer-list? (cdr list)))
		  #f)
	      #t)))
      (if (or (not (list? pintype-can-drive))
	      (not (= (length pintype-can-drive) (length pintype-names)))
	      (not (is-integer-list? pintype-can-drive)))
	  (begin
	    (display "INTERNAL ERROR: List of pins which can drive a net bad specified. Using default value.")
	    (newline)
	    (define pintype-can-drive 1))))
    (define pintype-can-drive 1))     ; Later is redefined if it's not a list.

(if (not (list? pintype-can-drive))
    (define pintype-can-drive (list 0 0 1 1 1 1 1 1 1 0 1 0 )))

; DRC matrix
;
; #\e: error    #\w: warning   #\c: correct
(if (not (defined? 'drc-matrix))
    (define drc-matrix (list
;  Order is important !
;             unknown in    out   io    oc    oe    pas   tp    tri   clk   pwr unconnected
;unknown
  '(            #\e   #\e   #\e   #\e   #\e   #\e   #\e   #\e   #\e   #\e   #\e   #\e )
;in
  '(            #\e   #\c   #\c   #\c   #\c   #\c   #\c   #\c   #\c   #\c   #\c   #\e )
;out
  '(            #\e   #\c   #\e   #\w   #\e   #\e   #\c   #\e   #\e   #\c   #\e   #\e )
;io
  '(            #\e   #\c   #\w   #\c   #\w   #\w   #\c   #\w   #\c   #\c   #\w   #\e )
;oc
  '(            #\e   #\c   #\e   #\w   #\e   #\c   #\c   #\e   #\c   #\c   #\e   #\e )
;oe
  '(            #\e   #\c   #\e   #\w   #\c   #\e   #\c   #\e   #\c   #\c   #\e   #\e )
;pas
  '(            #\e   #\c   #\c   #\c   #\c   #\c   #\c   #\c   #\c   #\c   #\c   #\e )
;tp
  '(            #\e   #\c   #\e   #\w   #\e   #\e   #\c   #\e   #\e   #\c   #\e   #\e )
;tri
  '(            #\e   #\c   #\e   #\c   #\c   #\c   #\c   #\e   #\c   #\c   #\e   #\e )
;clk
  '(            #\e   #\c   #\c   #\c   #\c   #\c   #\c   #\c   #\c   #\c   #\e   #\e )
;pwr
  '(            #\e   #\c   #\e   #\w   #\e   #\e   #\c   #\e   #\e   #\e   #\c   #\e )
;unconnected
  '(            #\e   #\e   #\e   #\e   #\e   #\e   #\e   #\e   #\e   #\e   #\e   #\e )
)))

;; Number of errors and warnings found
(define errors_number 0)
(define warnings_number 0)

(if (not (defined? 'action-unused-slots))
    (define action-unused-slots #\w)
    (begin
      (if (or (not (char? action-unused-slots))
 	      (not (or (char=? action-unused-slots #\w) (char=? action-unused-slots #\c)
		       (char=? action-unused-slots #\e))))
	  (begin
	    (display "INTERNAL ERROR: Action when unused slots are found has a wrong value. Using default.")
	    (newline)
	    (define action-unused-slots #\w))
	  )
      )
    )

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
	  (list-ref (list-ref drc-matrix row) column)))
  
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

; Check if the DRC matrix is simetric.
(define drc2:is-simetric-drc-matrix
  (lambda ()
    (let check-row ((row 1))
      (if (let check-column ((column 0))    
	    (if (not (eqv? (drc2:get-drc-matrix-element row column)
			   (drc2:get-drc-matrix-element column row)))
		#f
		(if (< column (- row 1))
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
	    (begin
	      (display (string-append "INTERNAL ERROR: Uref " uref " has no numslots attribute."))
	      (newline)))
	))

    (for-each check-unused-slots-of-package packages)
    ))


;
;  End of symbol checking functions
;-----------------------------------------------------------------------


;-----------------------------------------------------------------------
;  NETs checking functions
;

;;
;; Check for nets with less than two pins connected.
;;
;; Example of all-nets: (net1 net2 net3 net4)
(define drc2:check-single-nets
  (lambda (port all-nets)
      (if (not (null? all-nets))
	  (let ((netname (car all-nets)))
	    (begin
	      ; If netname is NoConnection, then it shouldn't be checked.
 	      (if (not (string-ci=? netname "NoConnection"))
		  (begin
		    (if (eq? (length (gnetlist:get-all-connections netname)) '0)
			(begin (display "ERROR: Net has no connections: " port)
			       (display netname port)
			       (newline port)
			       (set! errors_number (+ errors_number 1))
			       )                      
			)
		    (if (eq? (length (gnetlist:get-all-connections netname)) '1)
			(begin (display "ERROR: Net has only one connected pin: " port)
			       (display netname port)
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
;; type: number of the position of the type in the vector.
;; connections: ((U100 1) (U101 1)), for example.
(define drc2:display-pins-of-type
  (lambda (port type connections)
    (if (not (null? connections))
	(begin
	  (let ((device (car (car connections)))
		(pin (car (cdr (car connections)))))
	    (if (string-ci=? (list-ref pintype-names type)
			     (gnetlist:get-attribute-by-pinnumber device pin "pintype"))
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
  (lambda (port type1 type2 connections)
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
		   (display (drc2:get-full-name-of-pintype-by-number type1) port)
		   (display ": " port)
		   (display (drc2:display-pins-of-type port type1 
							 connections) port)
		   (display "connected to " port)
		   (display (drc2:get-full-name-of-pintype-by-number type2) port)
		   (display ": " port)
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
  (lambda (port connections pintypes pintype-count type1 type2)
    (define type1-count (list-ref pintype-count type1))
    (define type2-count (list-ref pintype-count type2))
    (define next-type1 
      (lambda (port connections pintypes pintype-count type1 type2)
	(if (< type1 (- (length pintype-names) 2))
	    (drc2:check-pintypes-of-single-net port connections pintypes pintype-count 
						 (+ type1 1) (+ type1 1))	
	    )
	))
    (define next-type2
      (lambda (port connections pintypes pintype-count type1 type2)
	(if (< type2 (- (length pintype-names) 2))
	    (drc2:check-pintypes-of-single-net port connections pintypes pintype-count 
						 type1 (+ type2 1))
	    (next-type1 port connections pintypes pintype-count type1 type1)
	    )))
    
					; Check type1 with type1 first
    (if (= type1-count 0)
					; if no pins of type1 connected, then continue with (+ type1 1)
	(begin
	  (next-type1 port connections pintypes pintype-count type1 type2))
	  
    (if (= type1 type2)
	(if (> type1-count 1)
	    (begin
	      (drc2:check-connection-of-two-pintypes port type1 type1 connections)
	      (next-type2 port connections pintypes pintype-count type1 type2)
	      
	      )
	      (next-type2 port connections pintypes pintype-count type1 type2))
	(begin
      (if (= type2-count 0)
					; if no pins of type2 connected, then continue with (+ type2 1)
	  (next-type2 port connections pintypes pintype-count type1 type2)
	  )
      (if (and (> type1-count 0) (> type2-count 0))
	  (begin	  
					; Check connections between type1 and type2.
	    (drc2:check-connection-of-two-pintypes port type1 type2 connections)
					; and continue with the next type2 if within the limits
	    (next-type2 port connections pintypes pintype-count type1 type2)
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
		       )
		(drc2:check-pintypes-of-single-net port connections pintypes pintype-count 0 0)
		(if (not (defined? 'dont-check-not-driven-nets))
		    (begin
		      (if (not (string-ci=? netname "NoConnection"))
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
		  (if (string-ci=? connection "unconnected_pin")
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


;
;  End of Net checking functions
;-----------------------------------------------------------------------




;;; Highest level function
;;; Write my special testing netlist format
;;;
(define drc2
   (lambda (output-filename)
      (let ((port (open-output-file output-filename)))
         (begin
	    ;; Perform DRC-matrix sanity checks.
	    ; See if the matrix is simetric.
	    (if (not (drc2:is-simetric-drc-matrix))
		(begin (display "INTERNAL ERROR: DRC matrix is NOT simetric." port)
		       (newline port)
		       (newline port)
		       (error "INTERNAL ERROR. DRC matrix is NOT simetric")))
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

	    ;; Check nets with only one connection
	    (if (not (defined? 'dont-check-one-connection-nets))
		(begin
		  (display "Checking nets with only one connection..." port)
		  (newline port)
		  (drc2:check-single-nets port (gnetlist:get-all-unique-nets "dummy"))
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
		  (drc2:check-unconnected-pins port packages (gnetlist:get-pins-nets (car packages)))
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

         (close-output-port port)))))


;;
;; DRC backend written by Carlos Nieves Onega ends here.
;;
;; --------------------------------------------------------------------------

