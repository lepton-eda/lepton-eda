;;; Lepton EDA netlister
;;; Copyright (C) 1998-2010 Ales Hvezda
;;; Copyright (C) 1998-2017 gEDA Contributors
;;; Copyright (C) 2017-2020 Lepton EDA Contributors
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
;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.

;;
;; Verilog netlist backend written by Mike Jarabek starts here
;;
;; 14 July 2011 - VERY basic concatenation support added by Frank Thomson
;;
;; --------------------------------------------------------------------------

;; some useful regexes for working with net-names
;;
(use-modules (ice-9 regex)
             (srfi srfi-1)
             (netlist port)
             (netlist schematic)
             (netlist schematic toplevel))

(define id-regexp "[a-zA-Z_][a-zA-Z0-9_$]*")
(define numeric  "[0-9]+")
;; match on a verilog concatened net like:  {netname[x:y],othernet}
;; Will not expand a bus for replication so
(define concat-bus-reg (make-regexp
                       (string-append "^[[:space:]]*"
                                      "\\{"
                                      ".*"
                                      "\\}")))

;; match on a verilog identifier like:  netname[x:y]
(define bit-range-reg (make-regexp
                       (string-append "^(" id-regexp ")[[:space:]]*"
                                      "\\["
                                      "[[:space:]]*(" numeric ")[[:space:]]*"
                                      ":"
                                      "[[:space:]]*(" numeric ")[[:space:]]*"
                                      "\\]")))

;; match on a verilog identifier like:  netname[x]
(define single-bit-reg (make-regexp
                        (string-append "^(" id-regexp ")[[:space:]]*"
                                       "\\["
                                       "[[:space:]]*(" numeric ")[[:space:]]*"
                                       "\\]" )))

;; match on a verilog identifier like:  netname
(define simple-id-reg (make-regexp
                       ( string-append "^(" id-regexp ")$" )))


;;; Returns a list of first pin netnames for those packages in
;;; PACKAGE-LIST that have an attribute named ATTRIBUTE which
;;; value is VALUE.
(define (verilog:filter attribute value package-list)
  (define (get-package-pin-netnames package)
    (and (string=? (gnetlist:get-package-attribute package attribute)
                   value)
         (pin-netname package (car (get-pins package)))))

  (filter-map get-package-pin-netnames package-list))


;;
;; Output the guts of the module ports here
;;
;; Scan through the list of components, and pins from each one, finding the
;; pins that have PINTYPE == CHIPIN, CHIPOUT, CHIPTRI (for inout)
;; build three lists one each for the inputs, outputs and inouts
;; return the a list of three lists that contain the pins in the order
;; we want.
(define (verilog:get-port-list packages)
  (list (verilog:filter "device" "IPAD" packages)
        (verilog:filter "device" "OPAD" packages)
        (verilog:filter "device" "IOPAD" packages)))

;;
;; output the meat of the module port section
;;
;; each line in the declaration is formatted like this:
;;
;;       PORTNAME , <newline>
;;
(define (verilog:write-module-declaration module-name port-list)
  ;; build up list of pins
  (let ((netnames (map verilog:netname (apply append port-list))))
    (format #t
            "module ~A (\n~A~A\n      );\n"
            (escape-identifier module-name)
            ;; Indentation.
            (if (null? netnames) "" "       ")
            (string-join netnames " ,\n       "))))

;;
;; output the module direction section
;;
(define verilog:write-port-directions
  (lambda (port-list)
    (let ((in    (car   port-list))    ; extract list of pins
          (out   (cadr  port-list))
          (inout (caddr port-list)))
      (begin
        (display "/* Port directions begin here */")
        (newline)
        (for-each (lambda (pin)
                    (begin
                      (display "input ")
                      (verilog:display-wire (verilog:get-net
                                             (verilog:netname pin)))
                      (display " ;\n")
                      )) in)       ; do each input

        (for-each (lambda (pin)
                    (begin
                      (display "output ")
                      (verilog:display-wire (verilog:get-net
                                             (verilog:netname pin)))
                      (display " ;\n")
                      )) out)      ; do each output

        (for-each (lambda (pin)
                    (begin
                      (display "inout ")
                      (verilog:display-wire (verilog:get-net
                                             (verilog:netname pin)))
                      (display " ;\n")
                      )) inout)    ; do each inout

        (newline)))))
;;
;; Top level header
;;

(define (verilog:write-top-header module-name packages)
  (let ((port-list (verilog:get-port-list packages)))
    (begin
      (display "/* structural Verilog generated by lepton-netlist */\n")
      (display "/* WARNING: This is a generated file, edits       */\n")
      (display "/*          made here will be lost next time      */\n")
      (display "/*          you run netlister!                    */\n")
      (newline)
      (verilog:write-module-declaration module-name port-list)
      (newline)
      (verilog:write-port-directions port-list)
      (newline))))

;;
;; Footer for file
;;
(define (verilog:write-bottom-footer)
  (display "endmodule\n"))

;;
;; Take a netname and parse it into a structure that describes the net:
;;
;;    (   netname            ; name of the wire
;;      ( N1                 ; first limit
;;        N2                 ; second limit
;;        Increasing_order   ; #t if N2>N1
;;        sure               ; #t if we are sure about the order
;;      ))
(define verilog:net-parse
  (lambda (netname)
    (let
        ((bit-range (regexp-exec bit-range-reg netname))
         (concat-bus (regexp-exec concat-bus-reg netname))
         (single-bit (regexp-exec single-bit-reg netname))
         (simple-id (regexp-exec simple-id-reg netname)))

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

       ;; or a net without anything
       (simple-id
        (list (match:substring simple-id 1) (list 0 0 #f #f netname)))

       (else
        (message
         (string-append "Warning: `" netname
                        "' is not likely a valid Verilog identifier\n"))
        (list netname (list 0 0 #f #f netname)))
       ))))

;;
;; Return #t if the passed name is something that might pass as a
;; verilog identifier.
;;
(define (verilog:identifier? netname)
  (let ((bit-range (regexp-exec bit-range-reg netname))
        (concat-bus (regexp-exec concat-bus-reg netname))
        (single-bit (regexp-exec single-bit-reg netname))
        (simple-id (regexp-exec simple-id-reg netname)))

    ;; check over each expression type, return
    ;; result
    (or bit-range
        concat-bus
        single-bit
        simple-id)))

;;; Escapes NETNAME if it is a verilog identifier, otherwise
;;; returns it as is.
(define (escape-identifier netname)
  (if (verilog:identifier? netname)
      netname
      (string-append "\\" netname " ")))


;;
;; return just the netname part of a verilog identifier
;;
(define verilog:netname
  (lambda (netname)
    (car (verilog:net-parse netname))))

;;  Update the given bit range with data passed.  Take care
;;  of ordering issues.
;;
;;   n1     : new first range
;;   n2     : new second range
;;   old-n1 : first range to be updated
;;   old-n2 : second range to be updated
;;   increasing : original order was increasing
(define verilog:update-range
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
(define verilog:update-record
  (lambda (n1
           n2
           list-n1
           list-n2
           increasing
           sure
           real)
    (list
     (append (verilog:update-range
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


(define verilog:get-nets '())


(define (verilog:get-nets-once! nets)
  (define the-nets '())
  (set! verilog:get-nets
        (begin
          (for-each
           (lambda (netname)
             ;; parse the netname, and see if it is already on the list
             (let* ((parsed (verilog:net-parse netname))
                    (listed (assoc (car parsed) the-nets)))
               (if listed
                   ;; it is, do some checks, and update the record
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
                       (set-cdr! listed
                                 (verilog:update-record n1 n2
                                                        list-n1 list-n2
                                                        increasing
                                                        #t
                                                        real)
                                 ))
                      ((and list-sure (not sure) (zero? n1) (zero? n2))
                       '() ;; this is a net without any expression, leave it
                       )
                      ((and list-sure (not consistant))
                       (message ;; order is inconsistent
                        (string-append "Warning: Net `" real "' has a "
                                       "bit order that conflicts with "
                                       "the original definition of `"
                                       list-real "', ignoring `"
                                       real "'\n"
                                       )))
                      ((and (not list-sure) sure consistant)
                       (set-cdr! listed
                                 (verilog:update-record n1 n2
                                                        list-n1 list-n2
                                                        increasing
                                                        #t
                                                        real)))

                      ((and (not list-sure) sure (not consistant))
                       (set-cdr! listed
                                 (verilog:update-record n1 n2
                                                        list-n2 list-n1
                                                        increasing
                                                        #t
                                                        real)))
                      ((and (not list-sure) (not sure))
                       (set-cdr! listed
                                 (verilog:update-record n1 n2
                                                        list-n1 list-n2
                                                        increasing
                                                        #f
                                                        real)))
                      (else (message "This should never happen!\n"))))

                   ;; it is not, just add it to the end
                   (set! the-nets
                         (append the-nets
                                 (list parsed))))))

           nets)
          the-nets))
  verilog:get-nets)

;; Retrieve the requested net record from the database.

(define verilog:get-net
  (lambda (wire)
    (begin
      (assoc wire verilog:get-nets)
      )))

;;
;;  Display wires from the design
;;
;;  Display a net in a legal verilog format, based on the object passed
(define verilog:display-wire
  (lambda (wire)
    ;; figure out if we need a bit range
    (let ((name            (car wire))
          (n1              (car (cadr wire)))
          (n2              (cadr (cadr wire)))
          )

      (if (not (and (zero? n1) (zero? n2)))
          ;; yes, print it
          (format #t "[ ~A : ~A ] " n1 n2))
      ;; print the wire name
      (display (escape-identifier name)))))

;;
;;  Loop over the list of nets in the design, writing one by one
;;
(define (verilog:write-wires)
  (display "/* Wires from the design */\n")
  (for-each (lambda (wire)          ; print a wire statement for each
              ; if it does not appear to be a concatenated bus then print it
              (if (not (regexp-exec concat-bus-reg (car(cdr(cdr(cdr(cdr(cadr wire))))))))
                      (begin
                      (display "wire ")   ; net in the design
                      (verilog:display-wire wire)
                      (display " ;")
                      (newline)
                      ) ;; begin
               ) ;; if
              ) ;; lambda
            verilog:get-nets ) ;; for-each
  (newline)
) ;; define

;;
;;  Output any continuous assignment statements generated
;; by placing `high' and `low' components on the board
;;
(define (verilog:write-continuous-assigns packages)
  (display "/* continuous assignments */\n")
  (for-each (lambda (wire)             ; do high values
              ;; XXX fixme, multiple bit widths!
              (format #t
                      "assign ~A = 1'b1;\n"
                      (escape-identifier wire)))
            (verilog:filter "device" "HIGH" packages))

  (for-each (lambda (wire)
              ;; XXX fixme, multiple bit widths!
              (format #t
                      "assign ~A = 1'b0;\n"
                      (escape-identifier wire)))
            (verilog:filter "device" "LOW" packages))
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
(define verilog:components
  (lambda (packages)
    (begin
      (display "/* Package instantiations */\n")
      (for-each (lambda (package)         ; loop on packages
                  (begin
                    (let ((device (get-device package)))
                      (if (not (schematic-port-device-string? device))
                          (format #t
                                  "~A ~A (~A    );\n\n"
                                  (escape-identifier (get-device package))
                                  (escape-identifier package)
                                  ;; if this module wants positional pins,
                                  ;; then output that format, otherwise
                                  ;; output normal named declaration
                                  (pin-connections->string
                                   package
                                   (string=? (gnetlist:get-package-attribute
                                              package "VERILOG_PORTS" )
                                             "POSITIONAL")))))))
                packages))))

;;
;; output a module connection for the package given to us with named ports
;;
(define (pin-connections->string package positional)
  (define pinnumber car)
  (define netname cdr)
  (let ((pin-net-list (get-pins-nets package)))
    (if (null? pin-net-list)
        ""
        (string-append
         "\n"
         (string-join
          (filter-map
           (lambda (pin-net)
             (and (not (string-prefix-ci? "unconnected_pin"
                                          (netname pin-net)))
                  (verilog:display-pin-net pin-net positional)))
           pin-net-list)
          ",\n")
         "\n"))))

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
(define (verilog:display-pin-net pin-net positional)
  (define pinnumber car)
  (define netname cdr)
  (if positional
      ;; output a positional port instance
      ;; add in name for debugging
      (format #f
              "  /* ~A */ ~A"
              (pinnumber pin-net)
              (netname pin-net))
      ;; else output a named port instance
      (format #f
              "    .~A ( ~A )"
              (escape-identifier (verilog:netname (pinnumber pin-net)))
              (escape-identifier (netname pin-net)))))



;;; Highest level function
;;; Write Structural verilog representation of the schematic
;;;
(define (verilog output-filename)
  (let ((nets (schematic-nets (toplevel-schematic)))
        (packages (schematic-package-names (toplevel-schematic)))
        ;; top level block name for the module
        (module-name (or (schematic-toplevel-attrib (toplevel-schematic)
                                                    'module_name)
                         "not found")))
    (verilog:get-nets-once! nets)
    (verilog:write-top-header module-name packages)
    (verilog:write-wires)
    (verilog:write-continuous-assigns packages)
    (verilog:components packages)
    (verilog:write-bottom-footer)))
;;
;; Verilog netlist backend written by Mike Jarabek ends here
;;
;; --------------------------------------------------------------------------
