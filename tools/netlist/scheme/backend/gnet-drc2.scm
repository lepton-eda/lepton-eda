;;; Lepton EDA netlister
;;; Copyright (C) 1998-2010 Ales Hvezda
;;; Copyright (C) 1998-2017 gEDA Contributors
;;; Copyright (C) 2018-2020 Lepton EDA Contributors
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
;;     Don't report anything               action-unused-slots                   'correct
;;     Report them as a warning            action-unused-slots                   'warning
;;     Report them as an error             action-unused-slots                   'error
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
;; (define action-unused-slots 'warning)
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
;; See the drc-matrix variable below for an example.


;; -------------------------------------------------------------------------------
;; IMPORTANT: Don't modify anything below unless you know what you are doing.
;; -------------------------------------------------------------------------------

(use-modules (srfi srfi-1)
             (srfi srfi-26)
             (lepton object)
             (lepton page)
             (netlist schematic-component)
             (netlist schematic)
             (netlist schematic toplevel))


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
(define (pintype-can-drive-valid? lst)
  (define (int01? x)
    (and (integer? x)
         (or (= x 0)
             (= x 1))))
  (and (list? lst)
       (= (length lst) (length pintype-names))
       (every int01? lst)))

(define pintype-can-drive
  (if (defined? 'pintype-can-drive)
    (if (pintype-can-drive-valid? pintype-can-drive)
        pintype-can-drive
        (begin
          (message "INTERNAL ERROR: List of pins which can drive a net bad specified. Using default value.\n")
          #f))
    #f))

(if (not pintype-can-drive)
;                                unk in out io oc oe pas tp tri clk pwr undef
    (set! pintype-can-drive (list 1   0  1   1  1  1  1   1  1   0   1    0 )))


;;; DRC matrix
;;  Order is important !
(define-undefined drc-matrix
  '(
    ;; unknown in    out     io      oc      oe      pas     tp      tri      clk     pwr    unconnected
    ;; unknown
    (correct)
    ;; in
    (correct correct)
    ;; out
    (correct correct error)
    ;; io
    (correct correct warning correct)
    ;; oc
    (correct correct error   warning error)
    ;; oe
    (correct correct error   warning correct error)
    ;; pas
    (correct correct correct correct correct correct correct)
    ;; tp
    (correct correct error   warning error   error   correct error)
    ;; tri
    (correct correct error   correct correct correct correct error   correct)
    ;; clk
    (correct correct correct correct correct correct correct correct correct correct)
    ;; pwr
    (correct correct error   warning error   error   correct error   error   error   correct)
    ;; unconnected
    (error   error   error   error   error   error   error   error   error   error   error   error)))
    ;; unknown in    out     io      oc      oe      pas     tp      tri      clk     pwr    unconnected

;; Number of errors and warnings found
(define errors_number 0)
(define warnings_number 0)
(define (drc2:error msg . params)
  (set! errors_number (1+ errors_number))
  (format #t "ERROR: ~A\n" (apply format #f msg params)))

(define (drc2:warning msg . params)
  (set! warnings_number (1+ warnings_number))
  (format #t "WARNING: ~A\n" (apply format #f msg params)))

(define-undefined action-unused-slots 'warning)

(if (or (not (or (eq? action-unused-slots 'warning)
                 (eq? action-unused-slots 'correct)
                 (eq? action-unused-slots 'error))))
    (begin
      (message "INTERNAL ERROR: Action when unused slots are found has a wrong value. Using default.\n")
      (set! action-unused-slots 'warning)))

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
(define (drc2:drc-matrix-elements-are-correct?)
  (define (valid? element)
    (memq element '(correct warning error)))

  (let check-row ((row 0))
    (and (let check-column ((column 0))
           (and (valid? (drc2:get-drc-matrix-element row column))
                (or (not (< column (- (length pintype-names) 1)))
                    (check-column (+ column 1)))))
         (or (not (< row (- (length pintype-names) 1)))
             (check-row (+ row 1))))))

;
; End of DRC matrix functions
;-----------------------------------------------------------------------

;-----------------------------------------------------------------------
; SYMBOLS checking functions
;

;;; Check for not numbered symbols.
(define (drc2:check-non-numbered-items packages)
  (define (non-numbered? package)
    (let ((refdes (schematic-component-refdes package)))
      (and refdes
           (string-index refdes #\?)
           (let ((object (schematic-component-object package)))
             (drc2:error "Not numbered refdes: ~A (~A at ~A on page ~S)."
                         refdes
                         (component-basename object)
                         (component-position object)
                         (basename (page-filename (object-page object))))))))

  (display "Checking non-numbered parts...\n")
  (for-each non-numbered? packages)
  (newline))


;;
;; Check for duplicated slots
;;
;; Check if a slot of a package is used more than one time. Checks all packages in the design.
(define (drc2:check-duplicated-slots packages)
  (define (check-duplicated-slots-of-package refdes)
    (define (check-slots-loop slots-list)
      (when (> (length slots-list) 1)
        (when (member (car slots-list) (cdr slots-list))
          (drc2:error "duplicated slot ~A of refdes ~A"
                  (number->string (car slots-list))
                  refdes))
        (check-slots-loop (cdr slots-list))))

    (check-slots-loop (gnetlist:get-slots refdes)))

  (display "Checking duplicated slots...\n")
  (for-each check-duplicated-slots-of-package packages)
  (newline))



;;
;; Checks for slots not used.
;;
(define (drc2:check-unused-slots packages)
  (define (check-unused-slots-of-package refdes)
    (define (check-slots-loop slot_number slots-list)
      (let ( (numslots (string->number (gnetlist:get-package-attribute refdes "numslots"))) )
        (when (not (member slot_number slots-list))
          (when (not (eq? action-unused-slots 'correct))
            (let ((error-func (if (eq? action-unused-slots 'error)
                                  drc2:error
                                  drc2:warning)))
              (error-func "Unused slot ~A of refdes ~A"
                          (number->string slot_number)
                          refdes))))
        (when (< slot_number numslots)
          (check-slots-loop (+ slot_number 1) slots-list))))

    (when (integer? (string->number (gnetlist:get-package-attribute refdes "numslots")))
      (check-slots-loop 1 (gnetlist:get-unique-slots refdes))))

  (display "Checking unused slots...\n")
  (for-each check-unused-slots-of-package packages)
  (newline))

;;
;; Check slot number is greater or equal than numslots for all packages
;;
(define (drc2:check-slots packages)
  (define (check-slots-of-package refdes)
    (let* ((numslots-string (gnetlist:get-package-attribute refdes "numslots"))
           (numslots (string->number numslots-string))
           (slot-string (let ((slots (get-all-package-attributes refdes "slot")))
                          (if (or (null? slots) (not (car slots)))
                              "unknown" (car slots))))
           (slot (string->number slot-string)))
      (define (check-slots-loop slots-list)
        (if (not (null? slots-list))
            (let ((this-slot (car slots-list)))
              (if (and (integer? this-slot)
                       (not (and (<= this-slot numslots) (>= this-slot 1))))
                  ;; If slot is not between 1 and numslots, then report an error.
                  (drc2:error "Reference ~A: Slot out of range (~A)."
                              refdes
                              (number->string this-slot)))

              (check-slots-loop (cdr slots-list)))))
      (if (string-ci=? slot-string "unknown")
          (begin
            ;; If slot attribute is not defined.
            (if (or (string-ci=? numslots-string "unknown") (= numslots 0))
                ;; No slot neither numslots (or set to zero) attributes defined.
                ;; This is correct.
                (display "")
                ;; Slot not defined, but numslots defined or different than 0.
                ;; This is incorrect. Check if numslots is a number and
                ;; report the situation to the user.
                (if (integer? numslots)
                    ;; If no slot attribute, but numslots is defined and not zero.
                    ;; If numslots is a number, then slot should be defined.
                    (drc2:error "Multislotted reference ~A has no slot attribute defined."
                                refdes)
                    (drc2:error "Reference ~A: Incorrect value of numslots attribute (~A)."
                                refdes
                                numslots-string))))
          ;; Slot attribute defined.
          ;; If it's a number, then check slots. If it's not, then report an error.
          (if (integer? slot)
              (if (integer? numslots)
                  (check-slots-loop (gnetlist:get-unique-slots refdes))
                  ;; Slot is defined and it's a number, but numslots it's not a number.
                  (drc2:error "Reference ~A: Incorrect value of numslots attribute (~A)."
                              refdes
                              numslots-string))
              ;; Slot attribute is not a number.
              (drc2:error "Reference ~A: Incorrect value of slot attribute (~A)."
                          refdes
                          slot-string)))))


  (display "Checking slots...\n")
  (for-each check-slots-of-package packages)
  (newline))

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
(define (drc2:check-duplicated-references packages non-unique-packages)
  (define (duplicated? package)
    (and (> (drc2:count-reference-in-list package non-unique-packages)
            (length (gnetlist:get-unique-slots package)))
         (drc2:error "Duplicated reference ~A." package)))

  (display "Checking duplicated references...\n")
  (for-each duplicated? packages)
  (newline))


;
;  End of symbol checking functions
;-----------------------------------------------------------------------


;-----------------------------------------------------------------------
;  NETs checking functions
;

;;; Check for "no-connect" nets with more than one pin connected.
;;; Example of nets: (net1 net2 net3 net4)
(define (drc2:check-connected-noconnects nc-nets)
  (display "Checking NoConnection nets for connections...\n")
  (for-each
   (lambda (netname)
     (let ((connections (get-all-connections netname)))
       ;; Only check nets with a NoConnection directive
       (and
        (>  (length connections) 1)
        (drc2:error "Net '~A' has connections, but has the NoConnection DRC directive: ~A."
                    netname
                    (pins-of-type->string 'all connections)))))
   nc-nets)
  (newline))

;;
;; Check for nets with less than two pins connected.
;;
;; Example of all-nets: (net1 net2 net3 net4)
(define (drc2:check-single-nets nets)
  (define (check-net netname)
    (let ((connections (get-all-connections netname)))
      (and (= (length connections) 0)
           (drc2:error "Net '~A' has no connections." netname))
      (and (= (length connections) 1)
           (drc2:error "Net '~A' is connected to only one pin: ~A."
                       netname
                       (pins-of-type->string 'all connections)))))

  (display "Checking nets with only one connection...\n")
  (for-each check-net nets)
  (newline))

;;; Returns a list of pintype attributes for CONNECTIONS.
;;; Example of CONNECTIONS: ((U100 1) (U101 1))
;;; Example of output list: ("in" "out" "in")
(define (connections-pintypes connections)
  (define package car)
  (define pinnumber cdr)
  (map
   (lambda (connection)
     (let ((pack (package connection))
           (pin (pinnumber connection)))
       (gnetlist:get-attribute-by-pinnumber pack pin "pintype")))
   connections))

;;
;;  Count pintypes of a net.
;;
;; net: "in", "out", for example.
(define drc2:count-pintypes-of-net
  (lambda (net)
    (define output-list (make-list (length pintype-names) 0))
    (define add-pintype
      (lambda (type)
           (if (not (member (string-downcase type) pintype-names))
               (begin
                 (display "INTERNAL ERROR: unknown pin type : ")
                 (display type)
                 (newline))
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
;;       the symbol 'all to display all the pins.
;; connections: ((U100 1) (U101 1)), for example.
(define (pins-of-type->string type connections)
  (define package car)
  (define pinnumber cdr)
  (define (wanted-type? pack pin)
    (or (eq? type 'all)
        (string-ci=? (list-ref pintype-names type)
                     (gnetlist:get-attribute-by-pinnumber pack pin "pintype"))))

  (define (package-pinnumber-of-type conn)
    (let ((pack (package conn))
          (pin (pinnumber conn)))
      (and (wanted-type? pack pin)
           (format #f "~A:~A" pack pin))))

  (string-join (filter-map package-pinnumber-of-type connections)))

;;
;; Check connection between two pintypes
;;
;; type1,type2: number of the position of the type in the vector.
;; connections: ((U100 1) (U101 1)), for example.
(define (drc2:check-connection-of-two-pintypes type1 type2 connections netname)
  (let ((drc-matrix-value (drc2:get-drc-matrix-element type1 type2)))
    (if (eq? drc-matrix-value 'correct)
        1
        (if (and (not (eq? drc-matrix-value 'error))
                 (not (eq? drc-matrix-value 'warning)))
            (begin
              (format #t "INTERNAL ERROR: DRC matrix has unknown value on position ~A,~A\n"
                      type1
                      type2)
              (error "INTERNAL ERROR: DRC matrix has unknown value. See output for more information"))
            (let ((error-func (if (eq? drc-matrix-value 'warning)
                                  drc2:warning
                                  drc2:error)))
              (error-func "Pin(s) with pintype '~A': ~A\n\tare connected by net '~A'\n\tto pin(s) with pintype '~A': ~A"
                          (drc2:get-full-name-of-pintype-by-number type1)
                          (pins-of-type->string type1 connections)
                          netname
                          (drc2:get-full-name-of-pintype-by-number type2)
                          (pins-of-type->string type2 connections)))))))

;;
;; Check pintypes of the pins connected to a single net
;;
;; type1,type2: number of the position of the type in the vector.
;; connections: ((U100 1) (U101 1)), for example.
;; pintype-count: vector with the number of pins connected to a single net, by pintype.
;;     (1 2 3 4 ... 10), for example.
(define drc2:check-pintypes-of-single-net
  (lambda (connections pintypes pintype-count type1 type2 netname)
    (define type1-count (list-ref pintype-count type1))
    (define type2-count (list-ref pintype-count type2))
    (define next-type1
      (lambda (connections pintypes pintype-count type1 type2 netname)
        (if (< type1 (- (length pintype-names) 2))
            (drc2:check-pintypes-of-single-net connections pintypes pintype-count
                                                 (+ type1 1) (+ type1 1) netname)
            )
        ))
    (define next-type2
      (lambda (connections pintypes pintype-count type1 type2 netname)
        (if (< type2 (- (length pintype-names) 2))
            (drc2:check-pintypes-of-single-net connections pintypes pintype-count
                                                 type1 (+ type2 1) netname)
            (next-type1 connections pintypes pintype-count type1 type1 netname)
            )))

                                        ; Check type1 with type1 first
    (if (= type1-count 0)
                                        ; if no pins of type1 connected, then continue with (+ type1 1)
        (begin
          (next-type1 connections pintypes pintype-count type1 type2 netname))

    (if (= type1 type2)
        (if (> type1-count 1)
            (begin
              (drc2:check-connection-of-two-pintypes type1 type1 connections netname)
              (next-type2 connections pintypes pintype-count type1 type2 netname)

              )
              (next-type2 connections pintypes pintype-count type1 type2 netname))
        (begin
      (if (= type2-count 0)
                                        ; if no pins of type2 connected, then continue with (+ type2 1)
          (next-type2 connections pintypes pintype-count type1 type2 netname)
          )
      (if (and (> type1-count 0) (> type2-count 0))
          (begin
                                        ; Check connections between type1 and type2.
            (drc2:check-connection-of-two-pintypes type1 type2 connections netname)
                                        ; and continue with the next type2 if within the limits
            (next-type2 connections pintypes pintype-count type1 type2 netname)
            ))
    )
    ))))

;;
;; Check if a net has a pintype which can drive the net.
;;
;; pintype-count: vector with the number of pins connected to a single net, by pintype.
;;     (1 2 3 4 ... 10), for example.
;; position: number of the position the function is checking.
(define (drc2:check-if-net-is-driven pintype-count position)
  (and (< position (- (length pintype-names) 1))
       (or (and (> (list-ref pintype-count position) 0)
                (= (list-ref pintype-can-drive position) 1))
           (drc2:check-if-net-is-driven pintype-count (+ position 1)))))

;;
;; Check pintype of the pins connected to every net in the design.
;;
;; all-nets: (net1 net2 net3), for example
(define (drc2:check-pintypes-of-nets nets)
  (define (check-net-pintype netname)
    (let* ((connections (get-all-connections netname))
           (pintypes (connections-pintypes connections))
           (pintype-count (drc2:count-pintypes-of-net pintypes))
           (directives (gnetlist:graphical-objs-in-net-with-attrib-get-attrib
                        netname
                        "device=DRC_Directive"
                        "value")))

      ;; If some directives are defined, then it shouldn't be checked.
      (and (not (member "DontCheckPintypes" directives))
           (drc2:check-pintypes-of-single-net connections
                                              pintypes
                                              pintype-count
                                              0
                                              0
                                              netname))
      (and (not (defined? 'dont-check-not-driven-nets))
           (not (member "DontCheckIfDriven" directives))
           (not (drc2:check-if-net-is-driven pintype-count 0))
           (drc2:error "Net ~A is not driven." netname))))
  (display "Checking type of pins connected to a net...\n")
  (for-each check-net-pintype nets)
  (newline))

;;
;; Check unconnected pins
;;
;; ref-list: ("U1" "U2"), for example.
;; pin-net-list: ( (pin net) (pin net) ... )
(define (drc2:check-unconnected-pins packages)
  (define (check-connection package pin-net)
    (let ((pin (car pin-net))
          (net (cdr pin-net)))
      (if (string-prefix-ci? "unconnected_pin" net)
          (let* ((position (drc2:position-of-pintype
                            (gnetlist:get-attribute-by-pinnumber package pin "pintype")))
                 (drc-matrix-value (drc2:get-drc-matrix-element undefined position)))
            (or (eq? drc-matrix-value 'correct)
                (let ((error-func (if (eq? drc-matrix-value 'warning)
                                      drc2:warning
                                      drc2:error)))
                  (error-func "Unconnected pin ~A:~A" package pin)))))))

  (display "Checking unconnected pins...\n")
  (for-each
   (lambda (package)
     (let ((pin-net-list (get-pins-nets package)))
       (for-each (cut check-connection package <>) pin-net-list)))
   packages)
  (newline))

; Report pins without the 'pintype' attribute (pintype=unknown)
(define (drc2:report-unknown-pintypes nets)
  (define (count-unknown-pintypes nets)
    (fold
     (lambda (netname count)
       (let* ((connections (get-all-connections netname))
              (pintypes (connections-pintypes connections))
              (pintype-count (drc2:count-pintypes-of-net pintypes)))
         (+ count
            (list-ref pintype-count (drc2:position-of-pintype "unknown")))))
     0 nets))

  (define (all-nets-connections nets)
    (sort (append-map (cut get-all-connections <>) nets)
          pair<?))

  (define (get-unknown-pintypes nets)
    (pins-of-type->string (drc2:position-of-pintype "unknown")
                          (all-nets-connections nets)))

  (display "Checking pins without the 'pintype' attribute...\n")
  (and (> (count-unknown-pintypes nets) 0)
       (display "NOTE: Found pins without the 'pintype' attribute: ")
       (display (get-unknown-pintypes nets))
       (message "\n"))
  (newline))

;
;  End of Net checking functions
;-----------------------------------------------------------------------


;;; Run expressions if var is not defined
(define-syntax not-defined?
  (syntax-rules (=>)
    ((_ var => exp ...)
     (when (not (defined? var))
       (begin exp ...)))))



;;; Highest level function
;;; Write my special testing netlist format
(define (drc2 output-filename)
  (let ((nets (schematic-nets (toplevel-schematic)))
        (nc-nets (schematic-nc-nets (toplevel-schematic)))
        (non-unique-packages (schematic-non-unique-package-names
                              (schematic-components (toplevel-schematic))))
        (packages (schematic-package-names (toplevel-schematic)))
        (netlist (schematic-components (toplevel-schematic))))

    ;; Perform DRC-matrix sanity checks.
    ;; See if all elements of the matrix are valid.
    (when (not (drc2:drc-matrix-elements-are-correct?))
      (display "INTERNAL ERROR: DRC matrix elements are NOT all valid.\n\n")
      (error "INTERNAL ERROR. DRC matrix elements are NOT all valid."))

    ;; Check non-numbered symbols
    (not-defined? 'dont-check-non-numbered-parts
                  => (drc2:check-non-numbered-items netlist))

    ;; Check for duplicated references
    (not-defined? 'dont-check-duplicated-references
                  => (drc2:check-duplicated-references
                      packages
                      non-unique-packages))

    ;; Check for NoConnection nets with more than one pin connected.
    (not-defined? 'dont-check-connected-noconnects
                  => (drc2:check-connected-noconnects nc-nets))

    ;; Check nets with only one connection
    (not-defined? 'dont-check-one-connection-nets
                  => (drc2:check-single-nets nets))

    ;; Check "unknown" pintypes
    (not-defined? 'dont-report-unknown-pintypes
                  => (drc2:report-unknown-pintypes nets))

    ;; Check pintypes of the pins connected to every net
    (not-defined? 'dont-check-pintypes-of-nets
                  => (drc2:check-pintypes-of-nets nets))

    ;; Check unconnected pins
    (not-defined? 'dont-check-unconnected-pins
                  => (drc2:check-unconnected-pins packages))

    ;; Check slots
    (not-defined? 'dont-check-slots
                  => (drc2:check-slots packages))

    ;; Check for duplicated slots
    (not-defined? 'dont-check-duplicated-slots
                  => (drc2:check-duplicated-slots packages))

    ;; Check for unused slots
    (not-defined? 'dont-check-unused-slots
                  => (drc2:check-unused-slots packages))

    ;; Display total number of warnings
    (if (> warnings_number 0)
        (format #t "Found ~A warnings.\n" warnings_number)
        (display "No warnings found.\n"))

    ;; Display total number of errors
    (if (> errors_number 0)
        (format #t "Found ~A errors.\n" errors_number)
        (display "No errors found.\n")))

  ;; Make gnetlist return an error if there are DRC errors.
  ;; If there are only warnings and it's in quiet mode, then
  ;; do not return an error.
  (if (and output-filename (> errors_number 0))
      (message "DRC errors found. See output file.\n")
      (and (> warnings_number 0)
           (not (calling-flag? "ignore-warnings-in-return-value"
                               (gnetlist:get-calling-flags)))
           (message "DRC warnings found. See output file.\n"))))


;;
;; DRC backend written by Carlos Nieves Onega ends here.
;;
;; --------------------------------------------------------------------------
