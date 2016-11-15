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
;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
;;; MA 02111-1301 USA.

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


(define port-symbols '(IOPAD IPAD OPAD HIGH LOW))

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
  (lambda (module-name port-list)
    (begin

      (display "#include \"systemc.h\"\n")

      (for-each (lambda (package)         ; loop on packages
                  (begin
                    (let ((device (get-device package)))
                      (if (not (memq (string->symbol device) ; ignore specials
                                     port-symbols))
                          (begin
                            (display "#include \"")
                            (display (get-device package))
                            (display ".h\"\n"))))))
                packages)
      (newline)
      (display "SC_MODULE (") (display module-name) (display ")\n{\n")
    )
  )
)
;;
;; output the module direction section
;;
(define systemc:write-port-directions
  (lambda (port-list)
    (let ((in    (car   port-list))    ; extract list of pins
          (out   (cadr  port-list))
          (inout (caddr port-list)))
      (begin
        (display "/* Port directions begin here */")
        (newline)
        (for-each (lambda (pin)
                    (begin
                      (display "sc_in<bool> ")
                      (display (systemc:netname pin))
                      (display ";")
                      (newline)
                    )) in)       ; do each input

        (for-each (lambda (pin)
                    (begin
                      (display "sc_out<bool> ")
                      (display
                       (systemc:netname pin))
                      (display ";")
                      (newline))) out)      ; do each output

        (for-each (lambda (pin)
                    (begin
                      (display "sc_inout<bool> ")
                      (display
                       (systemc:netname pin))
                      (display ";")
                      (newline))) inout)    ; do each inout

        ))))
;;
;; Top level header
;;

(define (systemc:write-top-header)
  (let ((port-list (systemc:get-port-list)))
    (begin
      (display "/* structural SystemC generated by gnetlist */\n")
      (display "/* WARNING: This is a generated file, edits */\n")
      (display "/*        made here will be lost next time  */\n")
      (display "/*        you run gnetlist!                 */\n")
      (display "/* Id ........gnet-systemc.scm (04/09/2003) */\n")
      (display "/* Source...../home/geda/gnet-systemc.scm   */\n")
      (display "/* Revision...0.3 (23/09/2003)              */\n")
      (display "/* Author.....Jaume Masip                   */\n")
      (newline)
      (systemc:write-module-declaration systemc:get-module-name
                                        port-list)
      (newline)
      (systemc:write-port-directions port-list)
      (newline))))

;;
;; Footer for file
;;
(define (systemc:write-bottom-footer)
  (display "  }\n};\n")
  (newline)
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

      ;; check over each expression type, and build the appropriate
      ;; result
      (cond
       ;; is it a bit range?
       (bit-range
        (list (match:substring bit-range 1)
              (list (string->number (match:substring bit-range 2))
                    (string->number (match:substring bit-range 3))
                    (> (string->number (match:substring bit-range 3))
                       (string->number (match:substring bit-range 2)))
                    '#t netname)))

       ;; just a single bit?
       (single-bit
        (list (match:substring single-bit 1)
              (list (string->number (match:substring single-bit 2))
                    (string->number (match:substring single-bit 2))
                    '#f '#f netname)))

       ;; just a systemc signal?
       (systemc
         (begin
           (list (match:substring systemc 1)
             (list (string->number (match:substring systemc 2))
               (match:substring systemc 2)
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
  (lambda (wire)
    ;; figure out if we need a bit range
    (let ((name            (car wire))
          (n1              (car (cadr wire)))
          (n2              (cadr (cadr wire)))
          (increasing      (caddr (cadr wire)))
          )

      (display name)
    )
  )
)

;;
;;  Loop over the list of nets in the design, writing one by one
;;
(define (systemc:write-wires)
  (display "/* Wires from the design */")
  (newline)
  (for-each (lambda (wire)          ; print a wire statement for each
              (display "sc_signal<")
              (display (cadr (cadr wire)))
              (display "> ")
              (systemc:display-wire wire)
              (display ";")
              (newline))
            systemc:get-nets )
  (newline))

;;
;;  Output any continuous assignment statements generated
;; by placing `high' and `low' components on the board
;;
(define (systemc:write-continuous-assigns)
  (for-each (lambda (wire)             ; do high values
              (begin
                (display "assign ")
                ;; XXX fixme, multiple bit widths!
                (display wire)
                (display " = 1'b1;")
                (newline)))
            (systemc:get-matching-nets "device" "HIGH"))

  (for-each (lambda (wire)
              (begin
                (display "assign ")
                ;; XXX fixme, multiple bit widths!
                (display wire)
                (display " = 1'b0;")
                (newline)))
            (systemc:get-matching-nets "device" "LOW"))
  (newline))



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
  (lambda (packages)
    (begin
      (set! c_p #f)
      (display "/* Package instantiations */") (newline)

      (for-each (lambda (package)         ; loop on packages
                  (begin
                    (let ((device (get-device package)))
                      (if (not (memq (string->symbol device) ; ignore specials
                                     port-symbols))
                          (begin
                            (display (get-device package)) (display " ")
                            (display package) (display ";")
                            (newline))))))
                packages)

      (newline)
      (display "SC_CTOR(") (display systemc:get-module-name)
      (display "):\n")

      (for-each (lambda (package)         ; loop on packages
                  (begin
                    (let ((device (get-device package)))
                      (if (not (memq (string->symbol device) ; ignore specials
                                     port-symbols))
                          (begin
                            (if c_p (begin (display ",") (newline)) (set! c_p #t))
                            (display "    ")
                            (display package)
                            (display "(\"")
                            (display package)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(do ((lp 1 (+ lp 1))) ((> lp 32))
  (let* ((attr (string-append "attr" (number->string lp)))
       (description (gnetlist:get-package-attribute package attr)))
      (begin
          (if (not (string=? description "unknown"))
               (begin (display "\",\"") (display description)))))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                             (display "\")")
                            )))))
                packages)
      (display "\n  {")

      (for-each (lambda (package)         ; loop on packages
                  (begin
                    (let ((device (get-device package)))
                      (if (not (memq (string->symbol device) ; ignore specials
                                     port-symbols))
                          (begin
                            ; if this module wants positional pins,
                            ; then output that format, otherwise
                            ; output normal named declaration
                            (systemc:display-connections package
                             (string=? (gnetlist:get-package-attribute package "VERILOG_PORTS" ) "POSITIONAL"))
                            )))))
                packages))))

;;
;; output a module connection for the package given to us with named ports
;;
(define systemc:display-connections
   (lambda (package positional)
     (begin
       (let ( (pin-list (gnetlist:get-pins-nets package))
              (comma_pending #f) )
        (if (not (null? pin-list))
            (begin
              (newline)
              (for-each (lambda (pin)
                          (if (not (string-prefix-ci? "unconnected_pin" (cdr pin)))
                              (begin
                                (display "    ")(display package)
                                (systemc:display-pin pin positional)
                                (display ";") (newline))))
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
    (lambda (pin positional)
      (let
          ((systemc (regexp-exec systemc-reg (cdr pin))))
          (begin
            (if positional
                (begin    ; output a positional port instance
                  (display "  /* ")
                  (display (car pin))  ; add in name for debugging
                  (display " */ " )
                  (display (cdr pin)))
                (begin    ; else output a named port instance
                  (display ".")
                  ; Display the escaped version of the identifier
                  (display (car pin))
                  (display "(")
                  (if systemc
                    (display (match:substring systemc 1))
                    (display (cdr pin)))
                  (display ")")))))))



;;; Highest level function
;;; Write Structural systemc representation of the schematic
;;;
(define (systemc output-filename)
  (set-current-output-port (gnetlist:output-port output-filename))
  (begin
    (systemc:get-nets-once!)
    (systemc:write-top-header)
    (systemc:write-wires)
    (systemc:write-continuous-assigns)
    (systemc:components packages)
    (systemc:write-bottom-footer)
    )
  (close-output-port (current-output-port)))
