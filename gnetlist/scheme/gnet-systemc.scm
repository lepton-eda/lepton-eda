;;; gEDA - GPL Electronic Design Automation
;;; gnetlist - gEDA Netlist
;;; Copyright (C) 1998-2008 Ales Hvezda
;;; Copyright (C) 1998-2008 gEDA Contributors (see ChangeLog for details)
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
;; SystemC netlist backend written by Jaume Masip 
;; (based on gnet-verilog.scm by Mike Jarabek)

;; some useful regexes for working with net-names
;;
(use-modules (ice-9 regex))

(define id-regexp "[a-zA-Z_][a-zA-Z0-9_$]*")
(define numeric  "[0-9]+")
;; match on a systemc identifier like:  netname[x:y]
(define bit-range-reg (make-regexp
		       (string-append "^(" id-regexp ")[[:space:]]*" 
				      "\\["
				      "[[:space:]]*(" numeric ")[[:space:]]*"
				      ":"
				      "[[:space:]]*(" numeric ")[[:space:]]*"
				      "\\]")))

;; match on a systemc identifier like:  netname[x]
(define single-bit-reg (make-regexp 
			(string-append "^(" id-regexp ")[[:space:]]*"
				       "\\["
				       "[[:space:]]*(" numeric ")[[:space:]]*"
				       "\\]" )))

;; match on a systemc identifier like:  netname<type>
(define systemc-reg (make-regexp
                        (string-append "^(" id-regexp ")[[:space:]]*"
                                       "<"
                                       "[[:space:]]*(" id-regexp ")[[:space:]]*"
                                       ">" )))

;; match on a systemc identifier like:  netname
(define simple-id-reg (make-regexp 
		       ( string-append "^(" id-regexp ")$" )))


;; return the top level block name for the module
(define systemc:get-module-name
  ( gnetlist:get-toplevel-attribute "module_name" ))

;; return a list of nets whose pins have the desired attribute name/value
;; pair
(define systemc:get-matching-nets
  (lambda (attribute value)
    (map car (systemc:filter attribute value packages))))

;; This function takes an attribute name, desired value, and a list of
;; packages.  For each of the packages, it looks up that attribute, and
;; if it matches, that package name is added to the list, and the function
;; recurses on the remaining packages.  If the attribute does not match, 
;; the function just recuses on the remaing packages. Thanks to Mohina Lal
;; for this trick.
;;

(define systemc:filter 
  (lambda (attribute value package-list)
    (cond ((null? package-list) '())
	  ((string=? (gnetlist:get-package-attribute (car package-list) 
						      attribute) value)
	   (cons 
	    (map (lambda (pin)
		   (car (gnetlist:get-nets (car package-list) pin)))
		 (pins (car package-list)))
	    (systemc:filter attribute value (cdr package-list))))
	  (else (systemc:filter attribute value (cdr package-list)))))
)


;;
;; Output the guts of the module ports here
;;
;; Scan through the list of components, and pins from each one, finding the
;; pins that have PINTYPE == CHIPIN, CHIPOUT, CHIPTRI (for inout)
;; build three lists one each for the inputs, outputs and inouts
;; return the a list of three lists that contain the pins in the order 
;; we want.
(define systemc:get-port-list
  (lambda ()
    ;; construct list
    (list (systemc:get-matching-nets "device" "IPAD")
	  (systemc:get-matching-nets "device" "OPAD")
	  (systemc:get-matching-nets "device" "IOPAD"))))

;;
;; output the meat of the module port section
;;
;; each line in the declaration is formatted like this:
;;
;;       PORTNAME , <newline>
;;
(define systemc:write-module-declaration
  (lambda (module-name port-list p)
    (begin

      (display "#include \"systemc.h\"\n" p) 

      (for-each (lambda (package)         ; loop on packages
                  (begin
                    (let ((device (get-device package)))
                      (if (not (memv (string->symbol device) ; ignore specials
                                     (map string->symbol (list "IOPAD" "IPAD" "OPAD" "HIGH" "LOW"))))
                          (begin
                            (display "#include \"" p)
                            (systemc:display-escaped-identifier (get-device package) p) 
                            (display ".h\"\n" p))))))
                packages)
      (newline p)
      (display "SC_MODULE (" p) (systemc:display-escaped-identifier module-name p) (display ")\n{\n" p)
    )
  )
)
;;
;; output the module direction section
;;
(define systemc:write-port-directions
  (lambda (port-list p)
    (let ((in    (car   port-list))    ; extract list of pins 
	  (out   (cadr  port-list))
	  (inout (caddr port-list)))
      (begin
	(display "/* Port directions begin here */" p)
	(newline p)
	(for-each (lambda (pin)
		    (begin
      (display "sc_in<bool> " p)(systemc:display-escaped-identifier (systemc:netname pin) p)(display ";" p)(newline p)

;; (display "sc_in<" p)(display (cadr (cadr pin)) p) (display "> " p) (systemc:display-wire pin p)(display ";" p)(newline p)
;; (display "  /* " )(display(car pin) ) (display " */ " ) (display(cdr pin))
;; (display "  /* " )(systemc:display-escaped-identifier (systemc:netname pin) ) (display " */ " ) 
                    )) in)       ; do each input

	(for-each (lambda (pin)
		    (begin
		      (display "sc_out<bool> " p)
		      (systemc:display-escaped-identifier 
		       (systemc:netname pin) p)
		      (display ";" p)
		      (newline p))) out)      ; do each output

	(for-each (lambda (pin)
		    (begin
		      (display "sc_inout<bool> " p)
		      (systemc:display-escaped-identifier 
		       (systemc:netname pin) p)
		      (display ";" p)
		      (newline p))) inout)    ; do each inout
		      
	))))
;;
;; Top level header
;;

(define systemc:write-top-header
	(lambda (p)
	  (let ((port-list (systemc:get-port-list)))
	    (begin
	      (display "/* structural SystemC generated by gnetlist */\n" p)
	      (display "/* WARNING: This is a generated file, edits */\n" p)
	      (display "/*        made here will be lost next time  */\n" p)
	      (display "/*        you run gnetlist!                 */\n" p)
	      (display "/* Id ........gnet-systemc.scm (04/09/2003) */\n" p)
	      (display "/* Source...../home/geda/gnet-systemc.scm   */\n" p)
	      (display "/* Revision...0.3 (23/09/2003)              */\n" p)
	      (display "/* Author.....Jaume Masip                   */\n" p)
	      (newline p)
	      (systemc:write-module-declaration systemc:get-module-name
						port-list p)
	      (newline p)
	      (systemc:write-port-directions port-list p)
	      (newline p)))))

;;
;; Footer for file
;;
(define systemc:write-bottom-footer
  (lambda (p)
    (display "  }\n};\n" p)
    (newline p)
    )
)

;;
;; Take a netname and parse it into a structure that describes the net:
;;
;;    (   netname            ; name of the wire
;;      ( N1                 ; first limit
;;        N2                 ; second limit
;;        Increasing_order   ; #t if N2>N1
;;        sure               ; #t if we are sure about the order
;;      ))
(define systemc:net-parse
  (lambda (netname)
    (let 
	((bit-range (regexp-exec bit-range-reg netname))
	 (single-bit (regexp-exec single-bit-reg netname))
	 (simple-id (regexp-exec simple-id-reg netname))
	 (systemc   (regexp-exec systemc-reg netname)))

;;      (newline)
;;      (display "    systemc:net-parse ")
;;      (if systemc (begin (display systemc) (display "->") (display (match:substring systemc 2) )))
;;      (if simple-id (display simple-id))
;;      (newline)

      ;; check over each expression type, and build the appropriate
      ;; result
      ;(display netname) (display ": ")
      (cond
       ;; is it a bit range?
       (bit-range
	;(display "bit-range" )
	(list (match:substring bit-range 1) 
	      (list (string->number (match:substring bit-range 2))
		    (string->number (match:substring bit-range 3))
		    (> (string->number (match:substring bit-range 3))
		       (string->number (match:substring bit-range 2))) 
		    '#t netname)))

       ;; just a single bit?
       (single-bit 
	;(display "single-bit")
	(list (match:substring single-bit 1) 
	      (list (string->number (match:substring single-bit 2))
		    (string->number (match:substring single-bit 2))
		    '#f '#f netname)))

       ;; just a systemc signal?
       (systemc
         (begin 
;;            (display "done systemc")(newline)
           (list (match:substring systemc 1)
             (list (string->number (match:substring systemc 2))
               (match:substring systemc 2)
;;                (string->number (match:substring systemc 2))
                    '#f '#f netname)))
)

       ;; or a net without anything
       (simple-id   
	;(display "bare-net")
	(list (match:substring simple-id 1) (list 0 0 #f #f netname)))

       (else       
	(display 
	 (string-append "Warning: `" netname 
			"' is not likely a valid Verilog identifier"))
	(newline)
	(list netname (list 0 0 #f #f netname)))
       )))
)

;;
;; Return #t if the passed name is something that might pass as a
;; systemc identifier.
;;
(define systemc:identifier?
  (lambda (netname)
    (let 
	((bit-range (regexp-exec bit-range-reg netname))
	 (single-bit (regexp-exec single-bit-reg netname))
	 (simple-id (regexp-exec simple-id-reg netname))
	 (systemc (regexp-exec systemc-reg netname)))

      ;; check over each expression type, return
      ;; result
      ;(display netname) (display ": ")
      (cond
       (bit-range  `#t )
       (single-bit `#t )
       (simple-id  `#t )
       (systemc    `#t )
       (else       `#f )
       ))))

;;
;; Display a systemc identifier that is escaped if needed 
;;
(define systemc:display-escaped-identifier
  (lambda (netname port)
    (if (systemc:identifier? netname)
	(display netname port) ; just display the identifier
	;;(display (string-append "\\" netname " ") port)))) ; need to escape
	(display netname port)))) ; need to escape
    

;;
;; return just the netname part of a systemc identifier
;;
(define systemc:netname
  (lambda (netname)
    (car (systemc:net-parse netname))))
      
;;  Update the given bit range with data passed.  Take care
;;  of ordering issues.
;;  
;;   n1     : new first range
;;   n2     : new second range
;;   old-n1 : first range to be updated
;;   old-n2 : second range to be updated
;;   increasing : original order was increasing
(define systemc:update-range
  (lambda (n1 n2 old-n1 old-n2 increasing)
    (let ((rn1 (if increasing
		   (min n1 old-n1)     ; originally increasing
		   (max n1 old-n1)))   ; originally decreasing

	  (rn2 (if increasing
		   (max n2 old-n2)     ; originally increasing
		   (min n2 old-n2))))
;      (display (string-append "increasing:" 
;			      (if increasing "increasing" "decreasing")
;			      " rn1:" (number->string rn1) 
;			      " rn2:" (number->string rn2)
;			      " n1:" (number->string n1)
;			      " n2:" (number->string n2)
;			      " old-n1:" (number->string old-n1)
;			      " old-n2:" (number->string old-n2))) (newline)
      (list rn1 rn2)
	
      )))


;; return a record that has been updated with the given
;; parameters
(define systemc:update-record
  (lambda (n1
	   n2 
	   list-n1 
	   list-n2
	   increasing
	   sure
	   real)
    (list
     (append (systemc:update-range 
	      n1 n2 list-n1 list-n2
	      increasing)
	     (list increasing
		   sure
		   real)))))

;;
;;  Work over the list of `unique' nets in the design,
;;  extracting names, and bit ranges, if appropriate.
;;  return a list of net description objects
;;

(define systemc:get-nets '())

(define systemc:get-nets-once!
  (lambda nil
    (define the-nets '())
    (set! systemc:get-nets
      (begin
        (for-each
          (lambda (netname)
            ; parse the netname, and see if it is already on the list
            (let* ((parsed (systemc:net-parse netname))
                   (listed (assoc (car parsed) the-nets)))

;;             (display  "systemc:get-nets(parsed)-> ")
;;             (display parsed)(display " (listed)-> ")
;;             (display listed)
;;             (newline)

             (if listed
                 (begin ; it is, do some checks, and update the record
                   ;; extract fields from list
                   (let* ((list-name       (car listed))
                          (list-n1         (car (cadr listed)))
                          (list-n2         (cadr (cadr listed)))
                          (list-increasing (caddr (cadr listed)))
                          (list-sure       (cadddr (cadr listed)))
                          (list-real       (cadddr (cdr (cadr listed))))

                          (name            (car parsed))
                          (n1              (car (cadr parsed)))
                          (n2              (cadr (cadr parsed)))
                          (increasing      (caddr (cadr parsed)))
                          (sure            (cadddr (cadr parsed)))
                          (real            (cadddr (cdr (cadr parsed))))

                          (consistant      (or (and list-increasing increasing)
                                               (and (not list-increasing)
                                                    (not increasing))))

                         )

                     (cond
                      ((and list-sure consistant)
                       (begin
                         (set-cdr! listed
                                   (systemc:update-record n1 n2
                                                          list-n1 list-n2
                                                          increasing
                                                          #t
                                                          real)
                                   )))
                       ((and list-sure (not sure) (zero? n1) (zero? n2))
                        '() ;; this is a net without any expression, leave it
                        )
                      ((and list-sure (not consistant))
                       (begin      ;; order is inconsistent
                         (display
                          (string-append "Warning: Net `" real "' has a "
                                         "bit order that conflicts with "
                                         "the original definition of `"
                                         list-real "', ignoring `"
                                         real "'"
                                         ))
                         (newline)))
                       ((and (not list-sure) sure consistant)
                        (begin
                          (set-cdr! listed
                                    (systemc:update-record n1 n2
                                                           list-n1 list-n2
                                                           increasing
                                                           #t
                                                           real))))

                       ((and (not list-sure) sure (not consistant))
                        (begin
                          (set-cdr! listed
                                    (systemc:update-record n1 n2
                                                           list-n2 list-n1
                                                           increasing
                                                           #t
                                                           real))))
                       ((and (not list-sure) (not sure))
                        (begin
                          (set-cdr! listed
                                    (systemc:update-record n1 n2
                                                           list-n1 list-n2
                                                           increasing
                                                           #f
                                                           real))))
                       (else
                        (begin
                          (display "This should never happen!")
                          (newline)))
                       )
                 )
             )
           (begin ; it is not, just add it to the end
             (set! the-nets
                   (append the-nets
                           (list parsed))))
           ))
;;         (display  "systemc:get-nets(parsed)-> ")
         )

        all-unique-nets)
      the-nets)
    )
    systemc:get-nets
))

;;
;;  Display wires from the design
;;
;;  Display a net in a legal systemc format, based on the object passed
(define systemc:display-wire
  (lambda (wire p)
    ;; figure out if we need a bit range
    (let ((name            (car wire))
	  (n1              (car (cadr wire)))
	  (n2              (cadr (cadr wire)))
          (increasing      (caddr (cadr wire)))
	  )
      
;;      (if (not (and (zero? n1) (zero? n2)))
;;	  (begin     ;; yes, print it
;;	    (display "[ " p)(display n1 p)(display " : " p)(display n2 p)(display " ] " p) ) )
    ;; print the wire name
      (systemc:display-escaped-identifier name p)
      ;;(systemc:display-escaped-identifier n1 p)
      ;;(systemc:display-escaped-identifier n2 p)
      ;;(systemc:display-escaped-identifier increasing p)
    )
  )
)

;;
;;  Loop over the list of nets in the design, writing one by one
;;
(define systemc:write-wires
  (lambda (p)
    (display "/* Wires from the design */" p)
    (newline p)
    (for-each (lambda (wire)          ; print a wire statement for each
    ;;            (let ((name (car wire)) (n1 (car (cadr wire))) (n2 (cadr (cadr wire))) (increasing (caddr (cadr wire)))))
;;    (display "/* Wires from the design */")(newline)
;;                (display "systemc:write-wires -> ")(display wire)(newline)
		(display "sc_signal<" p)
                (display (cadr (cadr wire)) p)        
		(display "> " p)
		(systemc:display-wire wire p)
		(display ";" p)
		(newline p))
	      systemc:get-nets ) 
    (newline p)))

;;
;;  Output any continuous assignment statements generated
;; by placing `high' and `low' components on the board 
;;
(define systemc:write-continuous-assigns
  (lambda (p)
;;    (display "/* continuous assignments */" p) (newline p)
    (for-each (lambda (wire)             ; do high values
		(begin
		  (display "assign " p) 	
		  ;; XXX fixme, multiple bit widths!
		  (systemc:display-escaped-identifier wire p) 
		  (display " = 1'b1;" p) 
		  (newline p)))
	      (systemc:get-matching-nets "device" "HIGH"))

    (for-each (lambda (wire)
		(begin
		  (display "assign " p) 
		  ;; XXX fixme, multiple bit widths!
		  (systemc:display-escaped-identifier wire p)
		  (display " = 1'b0;" p)
		  (newline p)))
	      (systemc:get-matching-nets "device" "LOW"))
    (newline p))
)



;;
;; Top level component writing 
;;
;; Output a compoment instatantiation for each of the
;; components on the board
;; 
;; use the format:
;;
;;  device-attribute refdes (
;;        .pinname ( net_name ),
;;        ...
;;    );
;;

(define c_p #f)

(define systemc:components
  (lambda (packages port)
    (begin
      (set! c_p #f)
      (display "/* Package instantiations */" port) (newline port)

      (for-each (lambda (package)         ; loop on packages
                  (begin
                    (let ((device (get-device package)))
                      (if (not (memv (string->symbol device) ; ignore specials
                                     (map string->symbol (list "IOPAD" "IPAD" "OPAD" "HIGH" "LOW"))))
                          (begin
                            (systemc:display-escaped-identifier (get-device package) port) (display " " port)
                            (systemc:display-escaped-identifier package port) (display ";" port)
                            (newline port))))))
                packages)

      (newline port)
      (display "SC_CTOR(" port) (systemc:display-escaped-identifier systemc:get-module-name port) 
      (display "):\n" port) 

      (for-each (lambda (package)         ; loop on packages
                  (begin
                    (let ((device (get-device package)))
                      (if (not (memv (string->symbol device) ; ignore specials
                                     (map string->symbol (list "IOPAD" "IPAD" "OPAD" "HIGH" "LOW"))))
                          (begin
                            (if c_p (begin (display "," port) (newline port)) (set! c_p #t))
                            (display "    " port)
                            (systemc:display-escaped-identifier package port) 
                            (display "(\"" port)
                            (systemc:display-escaped-identifier package port)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(do ((i n (- i 1))) ((zero? i)) (move-n-turn (/ 360 n)))
(do ((lp 1 (+ lp 1))) ((> lp 32)) 
;;  (begin (display lp)(newline)))
  (let* ((attr (string-append "attr" (number->string lp)))
       (description (gnetlist:get-package-attribute package attr)))
      (begin
          (if (not (string=? description "unknown"))
               (begin (display "\",\"" port) (display description port)))))
) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                             (display "\")" port)
                            )))))
                packages)
      (display "\n  {" port) 

      (for-each (lambda (package)         ; loop on packages
		  (begin
		    (let ((device (get-device package)))
 		      (if (not (memv (string->symbol device) ; ignore specials
 				     (map string->symbol (list "IOPAD" "IPAD" "OPAD" "HIGH" "LOW"))))
 			  (begin
			    ; if this module wants positional pins, 
			    ; then output that format, otherwise
			    ; output normal named declaration
			    (systemc:display-connections package 
			     (string=? (gnetlist:get-package-attribute package "VERILOG_PORTS" ) "POSITIONAL") port)
 			    )))))
 		packages))))

;;
;; output a module connection for the package given to us with named ports
;;
(define systemc:display-connections
   (lambda (package positional port)
     (begin
       (let ( (pin-list (gnetlist:get-pins-nets package)) 
	      (comma_pending #f) )
 	(if (not (null? pin-list))
 	    (begin
	      (newline port)
 	      (for-each (lambda (pin)
 			  (if (not  (strncmp? (cdr pin) "unconnected_pin" 15) )
			      (begin 
 			        (display "    " port)(systemc:display-escaped-identifier package port) 
				(systemc:display-pin pin positional port)
                                (display ";" port) (newline port))))
 			pin-list)
 	      )))))
)

;;
;; Display the individual net connections
;;  in this format if positional is true:
;;
;;    /* PINNAME */ NETNAME
;;
;;  otherwise emit:
;; 
;;      .PINNAME ( NETNAME )
;;
(define systemc:display-pin
    (lambda (pin positional port)
      (let
          ((systemc (regexp-exec systemc-reg (cdr pin))))
          (begin
            (if positional
                (begin    ; output a positional port instanace
                  (display "  /* " port)
                  (display (car pin) port)  ; add in name for debugging
                  (display " */ " port )
                  (display (cdr pin) port))
                (begin    ; else output a named port instance
                  (display "." port)
                  ; Display the escaped version of the identifier
                  (systemc:display-escaped-identifier (car pin) port)
                  (display "(" port)
                  (if systemc
                    (display (match:substring systemc 1) port)
                    (systemc:display-escaped-identifier (cdr pin) port))
                  (display ")" port)))))))
    
	 

;;; Highest level function
;;; Write Structural systemc representation of the schematic
;;;
(define systemc 
  (lambda (output-filename)
    (let ((port (open-output-file output-filename)))
      (begin
        (systemc:get-nets-once!)
	(systemc:write-top-header port)
;;        (display "***** start write-wires ********")(newline)
	(systemc:write-wires port)
;;        (display "***** end write-wires ********")(newline)
	(systemc:write-continuous-assigns port)
	(systemc:components packages port)
	(systemc:write-bottom-footer port)
	)
      (close-output-port port)
      )
    )
) 

