;;; Lepton EDA netlister
;;; Copyright (C) 1998-2010 Ales Hvezda
;;; Copyright (C) 1998-2017 gEDA Contributors
;;; Copyright (C) 2017-2022 Lepton EDA Contributors
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

(define-module (netlist)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)

  #:use-module (geda deprecated)
  #:use-module (lepton file-system)
  #:use-module (lepton gettext)
  #:use-module (lepton library)
  #:use-module (lepton log)
  #:use-module (lepton object)
  #:use-module (lepton rc)
  #:use-module (lepton repl)
  #:use-module (lepton version)
  #:use-module (netlist attrib compare)
  #:use-module (netlist attrib refdes)
  #:use-module (netlist backend)
  #:use-module (netlist config)
  #:use-module (netlist deprecated)
  #:use-module (netlist duplicate)
  #:use-module (netlist error)
  #:use-module (netlist mode)
  #:use-module (netlist net)
  #:use-module (netlist option)
  #:use-module (netlist package-pin)
  #:use-module (netlist schematic toplevel)
  #:use-module (netlist schematic)
  #:use-module (netlist schematic-component)
  #:use-module (netlist schematic-connection)
  #:use-module (netlist verbose)

  #:export (main
            calling-flag?
            get-device
            get-connections
            get-all-connections
            get-all-package-attributes
            get-component-text
            get-nets
            get-pins-nets
            message
            pin-netname
            gnetlist:alias-net
            gnetlist:alias-refdes
            gnetlist:build-net-aliases
            gnetlist:build-refdes-aliases
            gnetlist:get-all-package-attributes
            gnetlist:get-attribute-by-pinnumber
            gnetlist:get-attribute-by-pinseq
            gnetlist:get-backend-arguments
            gnetlist:get-calling-flags
            gnetlist:get-renamed-nets
            gnetlist:get-slots
            gnetlist:get-package-attribute
            gnetlist:get-unique-slots
            gnetlist:graphical-objs-in-net-with-attrib-get-attrib
            gnetlist:wrap
            known?
            unknown?
            pair<?)

  #:re-export (;; (lepton library)
               source-library
               ;; (netlist deprecated)
               ;; deprecated procedures
               get-pins
               gnetlist:get-pins
               ;; deprecated variables
               non-unique-packages
               packages
               all-unique-nets
               all-nets
               all-pins))

;;----------------------------------------------------------------------
;; The below functions added by SDB in Sept 2003 to support command-line flag
;; processing.
;;----------------------------------------------------------------------

(define (unknown? value)
  (string-ci=? value "unknown"))

(define (known? value)
  (not (unknown? value)))


(define (gnetlist:get-calling-flags) ; DEPRECATED
  "Returns a list of `-O' arguments in the form:

  ((ARGUMENT #t) ...)

This function is deprecated, and should not be used in new code.  New
code should use `gnetlist:get-backend-arguments' directly."
  (map (lambda (x) (list x #t)) (gnetlist:get-backend-arguments)))

;;---------------------------------------------------------------
;; calling-flag?
;;   Returns #t or #f depending upon the corresponding flag
;;   was set in the calling flags given to gnetlist.
;;   9.7.2003 -- SDB.
;;---------------------------------------------------------------
(define calling-flag?
  (lambda (searched-4-flag calling-flag-list)

    (if (null? calling-flag-list)
          '#f                                             ;; return #f if null list -- sort_mode not found.
          (let* ((calling-pair (car calling-flag-list))   ;; otherwise look for sort_mode in remainder of list.
                 (calling-flag (car calling-pair))
                 (flag-value (cadr calling-pair))  )

            ;; (display (string-append "examining calling-flag = " calling-flag "\n" ))
            ;; (display (string-append "flag-value = " (if flag-value "true" "false") "\n" ))

            (if (string=? calling-flag searched-4-flag)
                flag-value                                                 ;; return flag-value if sort_mode found
                (calling-flag? searched-4-flag (cdr calling-flag-list))    ;; otherwise recurse until sort_mode is found
            )  ;; end if
          )  ;; end of let*
     )  ;; end of if (null?
))

;;-------------  End of SDB's command line flag functions ----------------

;; Support functions

;;; Default resolver: Returns the first valid (non-#F) value from
;;; VALUES, or #F, if there is no valid attribute value. If any
;;; other valid value in the list is different, yields a warning
;;; reporting REFDES of affected symbol instances and attribute
;;; NAME.
(define (unique-attribute refdes name values)
  (let ((values (filter-map identity values)))
    (and (not (null? values))
         (let ((value (car values)))
           (or (every (lambda (x) (equal? x value)) values)
               (format (current-error-port) (G_ "\
Possible attribute conflict for refdes: ~A
name: ~A
values: ~A
") refdes name values))
           value))))


(define (get-all-package-attributes package-name attribute-name)
  "Get values of attribute named ATTRIBUTE-NAME from packages with
given PACKAGE-NAME.

This function returns the values of a specific attribute type
attached to the symbol instances with the given refdes.

Every first attribute value found is added to the return list. #F
is added if the instance has no such attribute.

Note: The order of the values in the return list is the order of
symbol instances within gnetlist (the first element is the value
associated with the first symbol instance)."
  (define sname (string->symbol attribute-name))

  (define (found-package? package)
    (let ((name (schematic-component-refdes package)))
      (and name
           (string=? name package-name)
           package)))

  (map
   (lambda (package)
     (schematic-component-attribute package sname))
   (filter-map found-package?
               (schematic-components (toplevel-schematic)))))


(define (gnetlist:get-package-attribute refdes name)
  "Return the value associated with attribute NAME on package
identified by REFDES.

It actually computes a single value from the full list of values
produced by 'get-all-package-attributes' as that list is
passed through 'unique-attribute'.

The default behavior is to return the value associated with the
first symbol instance for REFDES having the attribute NAME. If
some of the instances of REFDES have different value for NAME, it
prints a warning."
  (let* ((values (get-all-package-attributes refdes name))
         (value  (unique-attribute refdes name values)))
    (or value "unknown")))

(define (gnetlist:get-slots refdes)
  "Return a sorted list of slots used by package REFDES.

It collects the slot attribute values of each symbol instance of
REFDES. As a result, slots may be repeated in the returned list."
  (sort-list!
   (filter-map
    (lambda (slot)
      (if slot
          ;; convert string attribute value to number
          (or (string->number slot)
              ;; conversion failed, invalid slot, ignore value
              (begin
                (format (current-error-port)
                        (G_ "Refdes ~a: Bad slot number: ~a.\n") refdes slot)
                #f))
          ;; no slot attribute, assume slot number is 1
          1))
    (get-all-package-attributes refdes "slot"))
   <))

(define (gnetlist:get-unique-slots refdes)
  "Return a sorted list of unique slots used by package REFDES."
  (delete-duplicates! (gnetlist:get-slots refdes)))

;;
;; Given a uref, returns the device attribute value (unknown if not defined)
;;
(define get-device
   (lambda (package)
      (gnetlist:get-package-attribute package "device")))

;; Shorthand for get component values
(define get-value
   (lambda (package)
      (gnetlist:get-package-attribute package "value")))

(define get-component-text
   (lambda (package)
      (let ((value (gnetlist:get-package-attribute package "value"))
            (label (gnetlist:get-package-attribute package "label"))
            (device (gnetlist:get-package-attribute package "device")))
         (if (not (string=? "unknown" value))
            value
            (if (not (string=? "unknown" label))
               label
               device)))))


;; Wrap a string into lines no longer than wrap-length
;; wrap-char is put on the end-of-the-wrapped-line, before the return
;; (from Stefan Petersen)
(define (gnetlist:wrap string-to-wrap wrap-length wrap-char)
  (if (> wrap-length (string-length string-to-wrap))
      string-to-wrap ; Last snippet of string
      (let ((pos (string-rindex string-to-wrap #\space 0 wrap-length)))
        (cond ((not pos)
               (display (G_ "Couldn't wrap string  at requested position\n"))
               " Wrap error!")
              (else
               (string-append
                (substring string-to-wrap 0 pos)
                wrap-char
                "\n "
                (gnetlist:wrap (substring string-to-wrap (+ pos 1)) wrap-length wrap-char)))))))

;; example use
; (define (run-test test-string wrap-len)
;   (display (string-append "Wrapping \"" test-string "\" into "))
;   (display wrap-len)
;   (newline)
;   (display (gnetlist:wrap test-string wrap-len " \\"))
;   (newline)
;   (newline))

; (run-test "one two three four five six seven eight nine ten" 5)
; (run-test "one two three four five six seven eight nine ten" 10)
; (run-test "one two three four five six seven eight nine ten" 20)

;;; Determines refdes= for a particular OBJECT.
;;; Returns first value of first attrib found with given name, or #f.
(define (gnetlist:get-uref object)
  (let ((attrib-lst (get-attrib-value-by-attrib-name object "refdes")))
    (and (not (null? attrib-lst))
         (car attrib-lst))))

;; Custom get-uref function to append ".${SLOT}" where a component
;; has a "slot=${SLOT}" attribute attached.
;;
;; NOTE: Original test for appending the ".<SLOT>" was this:
;;   (let ((numslots (gnetlist:get-package-attribute package "numslots"))
;;        (slot-count (length (gnetlist:get-unique-slots package)))
;;     (if (or (string=? numslots "unknown") (string=? numslots "0"))
;;
(define (get-spice-refdes object)
  (let ((real-refdes (gnetlist:get-uref object)))
    (if (null? (get-attrib-value-by-attrib-name object "slot"))
        real-refdes
        (string-append real-refdes "."
                       (car (get-attrib-value-by-attrib-name object "slot"))))))

;; define the default handler for get-uref
(define get-uref gnetlist:get-uref)

;; Where to output messages for the user
(define message-port (current-error-port))
;; Procedure to output messages to message-port
(define (message output-string)
  (display output-string message-port)
  )


;;; Helper function for sorting connections.
(define (pair<? a b)
  (let ((refdes-a (hierarchical-refdes->string (car a)))
        (refdes-b (hierarchical-refdes->string (car b)))
        (pin-a (cdr a))
        (pin-b (cdr b)))
    (or (refdes<? refdes-a refdes-b)
        (and (string=? refdes-a refdes-b)
             (refdes<? pin-a pin-b)))))



(define (get-connections netname schematic)
  "Returns all connections in the form of ((refdes pin) ...) for
NETNAME in SCHEMATIC."
  (define (found? x)
    (and x
         (string=? x netname)))

  (define netlist (schematic-components schematic))

  (define (pin->refdes-pinnumber-pair pin)
    (let* ((component (package-pin-parent pin))
           (refdes (hierarchical-refdes->string
                    (schematic-component-refdes component)))
           (pinnumber (package-pin-number pin)))
      (and (not (schematic-component-graphical? component))
           refdes
           pinnumber
           (cons refdes pinnumber))))

  (define (get-found-pin-connections pin)
    (if (found? (package-pin-name pin))
        (filter-map pin->refdes-pinnumber-pair
                    (if (package-pin-connection pin)
                        (schematic-connection-pins (package-pin-connection pin))
                        '()))
        '()))

  (define (get-netlist-connections netlist)
    (append-map
     (lambda (package)
       (append-map get-found-pin-connections (schematic-component-pins package)))
     netlist))

  (sort-remove-duplicates (get-netlist-connections netlist)
                          pair<?))

(define (get-all-connections netname)
  "Returns all connections in the form of ((refdes pin) ...) for
NETNAME."
  (get-connections netname (toplevel-schematic)))


(define (get-pins-nets refdes)
  "For specified REFDES, returns a list of strings defining
connection pairs in the form (\"pin-number\" . \"net-name\")."

  (define (found? x)
    (string= x refdes))

  (define (get-pin-netname-pair pin)
    (let ((pin-number (package-pin-number pin))
          (pin-name (package-pin-name pin)))
      (and pin-number
           pin-name
           (cons pin-number pin-name))))

  (define (get-pin-netname-list component)
     (if (and=> (schematic-component-refdes component) found?)
         (filter-map get-pin-netname-pair (schematic-component-pins component))
         '()))

  ;; Currently, netlist can contain many `packages' with the same
  ;; name, so we have to deal with this.
  (let ((result-list (append-map get-pin-netname-list
                                 (schematic-components (toplevel-schematic)))))
    (sort-remove-duplicates result-list pair<?)))


;;; This procedure is buggy in the same way as gnetlist:get-nets.
;;; It should first search for netname, and then get all
;;; package-pin pairs by that netname.
(define (get-nets package pin-number)
  (define (pin-connections pin)
    (let ((pins (schematic-connection-pins (package-pin-connection pin))))
      (filter-map
       (lambda (pin)
         (let ((refdes (hierarchical-refdes->string
                        (schematic-component-refdes (package-pin-parent pin))))
               (pinnumber (package-pin-number pin)))
           (and refdes
                pinnumber
                (or (package-pin-net-map pin)
                    (not (null? (cdr pins))))
                (cons refdes pinnumber))))
       pins)))

  (define (lookup-through-connections pin package pin-number)
    (let ((connections (pin-connections pin)))
      (and (not (null? connections))
           (member (cons package pin-number) connections)
           connections)))

  (define (found-pin-number? x)
    (and x
         (string=? x pin-number)))

  (define (lookup-through-pins pins)
    (filter-map
     (lambda (pin)
       (and (found-pin-number? (package-pin-number pin))
            (cons (package-pin-name pin)
                  (lookup-through-connections pin
                                              package
                                              pin-number))))
     pins))

  (define (found-package? x)
    (and x
         (string=? x package)))

  (define (lookup-through-netlist netlist)
    (append-map
     (lambda (package)
       (if (found-package? (schematic-component-refdes package))
           (lookup-through-pins (schematic-component-pins package))
           '()))
     netlist))

  (let ((found (lookup-through-netlist (schematic-components (toplevel-schematic)))))
    (match found
      (((netname . rest) ..1)
       (cons (car netname) (apply append (delq #f rest))))
      (_ '("ERROR_INVALID_PIN")))))


(define (pin-netname package pinnumber)
  (or (assoc-ref (get-pins-nets package) pinnumber)
      "ERROR_INVALID_PIN"))


(define (gnetlist:get-toplevel-attribute attrib)
  (or (assq-ref (schematic-toplevel-attribs (toplevel-schematic))
                (string->symbol attrib))
      "not found"))

(define gnetlist:get-all-package-attributes get-all-package-attributes)
(define gnetlist:get-nets get-nets)
(define gnetlist:get-pins-nets get-pins-nets)
(define (gnetlist:get-verbosity)
  (if (netlist-option-ref 'verbose)
      1
      (if (netlist-option-ref 'quiet)
          -1
          0)))
(define (gnetlist:get-input-files)
  (netlist-option-ref '()))
(define (gnetlist:get-command-line)
  (string-join (command-line) " "))
(define (gnetlist:get-packages level)
  (schematic-package-names (toplevel-schematic)))
(define (gnetlist:get-non-unique-packages level)
  (schematic-non-unique-package-names (toplevel-schematic)))
(define (gnetlist:get-all-unique-nets level)
  (schematic-nets (toplevel-schematic)))
(define (gnetlist:get-all-nets level)
  (schematic-non-unique-nets (toplevel-schematic)))
(define (gnetlist:get-all-connections netname)
  (map (lambda (pair) (list (car pair) (cdr pair)))
       (get-all-connections netname)))
(define (gnetlist:get-backend-arguments)
  (netlist-option-ref 'backend-option))


;; Returns the least joint coordinate of CONNECTION.  A simple
;; method of coordinate comparison is used for objects of the
;; connection.  The coordinate is considered less if it has lesser
;; X value or, if the X values are equal, its Y value is less.
(define (connection-least-coord connection)
  (define (min-coord a b)
    (let ((x1 (car a))
          (y1 (cdr a))
          (x2 (car b))
          (y2 (cdr b)))
      (if (or (< x1 x2)
              (and (= x1 x2) (< y1 y2)))
          a
          b)))

  (define (coords object)
    (if (pin? object)
        (list (line-start object))
        ;; net
        (list (line-start object) (line-end object))))

  (reduce min-coord
          #f
          (append-map coords (schematic-connection-objects connection))))

(define (gnetlist:get-renamed-nets level)
  "Return the sorted list of net renames in toplevel schematic.
The argument LEVEL is dummy."
  (define (make-special-netname connection hname)
    (let ((coord (connection-least-coord connection)))
      (create-net-name
       (format #f "unnamed_net_at_~Ax~A" (car coord) (cdr coord))
       hname
       #f)))

  (define (unnamed? name)
    (eq? 'unnamed (car name)))

  (define (name<? a b)
    (refdes<? (schematic-connection-override-name a)
              (schematic-connection-override-name b)))

  (define (net? name)
    (eq? 'net (car name)))

  (define (create-net-name* connection hname)
    (if (unnamed? hname)
        (make-special-netname connection (cdr hname))
        (create-net-name (cadr hname) (cddr hname) (net? hname))))

  (let ((connections (sort (schematic-connections (toplevel-schematic))
                           name<?)))
    (append-map
     (lambda (connection)
       (let ((common-name (schematic-connection-override-name connection))
             (other-names (map (cut create-net-name* connection <>)
                               (cdr (schematic-connection-name connection)))))

         (map (cut list <> common-name) other-names)))
     connections)))

;;
;; Functions for dealing with naming requirements for different
;; output netlist formats which may be more restrictive than
;; gEDA's internals.
;;

;; These will become hash tables which provide the mapping
;; from gEDA net name to netlist net name and from netlist
;; net name to gEDA net name.
(define gnetlist:net-hash-forward (make-hash-table 512))
(define gnetlist:net-hash-reverse (make-hash-table 512))

;; These will become hash tables which provide the mapping
;; from gEDA refdes to netlist refdes and from netlist
;; refdes to gEDA refdes.
(define gnetlist:refdes-hash-forward (make-hash-table 512))
(define gnetlist:refdes-hash-reverse (make-hash-table 512))

;; build the hash tables with the net name mappings and
;; while doing so, check for any shorts which are created
;; by modifying the netnames.  If a short occurs, error out
;; with a descriptive message.
;;
;; This function should be called as one of the first steps
;; in a netlister which needs to alias nets.
(define gnetlist:build-net-aliases
  (lambda (mapfn nets)
    (if (not (null? nets))
        (begin
          (let ( (net (car nets))
                 (alias (mapfn (car nets)))
                 )

            (when (hash-ref gnetlist:net-hash-reverse alias)
              (netlist-error 1
                      (G_ "There is a net name collision!
The net called \"~A\" will be remapped
to \"~A\" which is already used
by the net called \"~A\".
This may be caused by netname attributes colliding with other netnames
due to truncation of the name, case insensitivity, or
other limitations imposed by this netlist format.
")
                      net
                      alias
                      (hash-ref gnetlist:net-hash-reverse alias)))
            (hash-create-handle! gnetlist:net-hash-forward net   alias)
            (hash-create-handle! gnetlist:net-hash-reverse alias net  )
            (gnetlist:build-net-aliases mapfn (cdr nets))
            )
          )
        )
    )
  )

;; build the hash tables with the refdes mappings and
;; while doing so, check for any name clashes which are created
;; by modifying the refdes's.  If a name clash occurs, error out
;; with a descriptive message.
;;
;; This function should be called as one of the first steps
;; in a netlister which needs to alias refdes's.
(define gnetlist:build-refdes-aliases
  (lambda (mapfn refdeses)
    (if (not (null? refdeses))
        (begin
          (let ( (refdes (car refdeses))
                 (alias (mapfn (car refdeses)))
                 )

            (when (hash-ref gnetlist:refdes-hash-reverse alias)
              (netlist-error 1
                             (G_ "There is a refdes name collision!
The refdes \"~A\" will be mapped\nto \"~A\" which is already used
by \"~A\".
This may be caused by refdes attributes colliding with others
due to truncation of the refdes, case insensitivity, or
other limitations imposed by this netlist format.
")
                             refdes
                             alias
                             (hash-ref gnetlist:refdes-hash-reverse alias)))
            (hash-create-handle! gnetlist:refdes-hash-forward refdes alias)
            (hash-create-handle! gnetlist:refdes-hash-reverse alias  refdes  )
            (gnetlist:build-refdes-aliases mapfn (cdr refdeses))
            )
          )
        )
    )
  )

;; convert a gEDA netname into an output netlist net name
(define gnetlist:alias-net
  (lambda (net)
    (hash-ref gnetlist:net-hash-forward net)
    )
  )

;; convert a gEDA refdes into an output netlist refdes
(define gnetlist:alias-refdes
  (lambda (refdes)
    (hash-ref gnetlist:refdes-hash-forward refdes)
    )
  )

;; convert an output netlist net name into a gEDA netname
(define gnetlist:unalias-net
  (lambda (net)
    (hash-ref gnetlist:net-hash-reverse net)
    )
  )

;; convert an output netlist refdes into a gEDA refdes
(define gnetlist:unalias-refdes
  (lambda (refdes)
    (hash-ref gnetlist:refdes-hash-reverse refdes)
    )
  )


(define (gnetlist:get-attribute-by-pin-attrib refdes
                                              pin-attrib-name
                                              pin-attrib-value
                                              name
                                              func)
  (define (found-refdes? x)
    (and x
         (string=? x refdes)))

  (define (find-pin-by-attrib pins name value)
    (and (not (null? pins))
         (let* ((pin (car pins))
                (attrib (assq-ref (package-pin-attribs pin)
                                  name)))
           (if (and attrib
                    (string=? attrib value))
               pin
               (find-pin-by-attrib (cdr pins) name value)))))

  (let loop ((netlist (schematic-components (toplevel-schematic))))
    (if (null? netlist)
        "unknown"
        (or (and (found-refdes? (schematic-component-refdes (car netlist)))
                 (let ((pin (find-pin-by-attrib (schematic-component-pins (car netlist))
                                                (string->symbol pin-attrib-name)
                                                pin-attrib-value)))
                   (if pin
                       (assq-ref (package-pin-attribs pin)
                                 (string->symbol name))
                       (and func (func (schematic-component-pins (car netlist))
                                       name
                                       pin-attrib-value)))))
            (loop (cdr netlist))))))


;;; Supplies pintype 'pwr' for artificial pins having pinnumber.
(define (cheat-pintype pins name value)
  (define (in-pin-list? pin-list pin-number-to-search)
    (and (not (null? pin-list))
         (let ((pin (car pin-list)))
           (or (and (package-pin-number pin)
                    (string=? (package-pin-number pin) pin-number-to-search))
               (in-pin-list? (cdr pin-list) pin-number-to-search)))))

  (and (string=? name "pintype")
       (in-pin-list? pins value)
       "pwr"))

;; takes a uref and pinseq number and returns wanted_attribute associated
;; with that pinseq pin and component
(define (gnetlist:get-attribute-by-pinseq refdes pinseq-value name)
  (gnetlist:get-attribute-by-pin-attrib refdes "pinseq" pinseq-value name #f))


;; this takes a pin number and returns the appropriate attribute on that pin
;; scm_pin is the value associated with the pinnumber= attribute and uref
(define (gnetlist:get-attribute-by-pinnumber refdes pinnumber-value name)
  (gnetlist:get-attribute-by-pin-attrib refdes "pinnumber" pinnumber-value name cheat-pintype))

;; given a net name, an attribute, and a wanted attribute, return all
;; the given attribute of all the graphical objects connected to that
;; net name
(define (gnetlist:graphical-objs-in-net-with-attrib-get-attrib netname in-attrib out-attrib-name)
  (define (has-netname? pin)
    (let ((name (package-pin-name pin)))
      (and name
           (string=? name netname))))

  (define (have-netname? pins)
    (and (not (null? pins))
         (or (has-netname? (car pins))
             (have-netname? (cdr pins)))))

  (define (parse-attrib-string s)
    (let ((position (string-index s #\=)))
      (and position
           (let ((name (string-take s position))
                 (value (string-drop s (1+ position))))
             (and (not (string=? value ""))
                  (not (string-suffix? " " name))
                  (not (string-prefix? " " value))
                  (cons name value))))))

  (define (have-attrib? attribs attrib)
    (let* ((name-value (parse-attrib-string attrib))
           (name (string->symbol (car name-value)))
           (value (cdr name-value))
           (attrib-values (assq-ref attribs name)))
      (and attrib-values (member value attrib-values))))

  (let ((out-attrib (string->symbol out-attrib-name)))
    (and netname
        (append-map
         (lambda (package)
           (if (and (have-netname? (schematic-component-pins package))
                    (have-attrib? (schematic-component-attribs package) in-attrib))
               (assq-ref (schematic-component-attribs package) out-attrib)
               '()))
         (schematic-graphicals (toplevel-schematic))))))

(define (get-output-filename)
  ;; Name is file name or "-" which means stdout.
  (let ((name (netlist-option-ref 'output)))
    (and (not (string=? name "-"))
         name)))


(define (usage)
  (format #t (G_
    "Usage: ~A [OPTION ...] [-g BACKEND] [--] FILE ...

Generate a netlist from one or more Lepton EDA schematic FILEs.

General options:
  -q, --quiet             Quiet mode.
  -v, --verbose           Verbose mode.
  -o, --output=FILE       Filename for netlist data output.
  -L, --load-path=DIR     Add DIR to Scheme search path.
  -g, --backend=BACKEND   Specify netlist backend to use.
  -f, --file-backend=FILE Specify path to netlist backend file to use.
  -O, --backend-option=STRING
                          Pass an option string to backend.
  -l, --pre-load=FILE     Load Scheme file before loading backend.
  -m, --post-load=FILE    Load Scheme file after loading backend.
  -c, --eval-code=EXPR    Evaluate Scheme expression at startup.
  -i, --interactive       Enter interactive Scheme REPL after loading.
  -b, --list-backends     Print a list of available netlist backends.
  -h, --help              Help; this message.
  -V, --version           Show version information.
  --                      Treat all remaining arguments as filenames.

Report bugs at <https://github.com/lepton-eda/lepton-eda/issues>
Lepton EDA homepage: <https://github.com/lepton-eda/lepton-eda>
")
          (basename (car (program-arguments))))
  (primitive-exit 0))


;;; Set lepton-netlist toplevel schematic based on schematic FILES
;;; and NETLIST-MODE which must be either "'geda", or "'spice".
(define (set-ln-toplevel-schematic! files)
  (define (process-gafrc* name)
    (process-gafrc "lepton-netlist" name))

  (and (eq? (netlist-mode) 'spice)
       (set! get-uref get-spice-refdes))
  (for-each process-gafrc* files)
  (catch 'system-error
    (lambda () (set-toplevel-schematic! (make-toplevel-schematic files)))
    (lambda (key subr message args rest)
      (netlist-error 1
                     (format #f
                             (G_ "Failed to open schematic files: ~?\n")
                             message
                             args)))))


(define (catch-handler tag . args)
  (format (current-error-port)
          (G_ "\nJust got an error '~A':\n        ~A\n\n")
          tag
          args)
  #f)


(define (load-scheme-script filename)
  (define (load-and-log name)
    (log! 'message (G_ "Loading ~S") name)
    (primitive-load name))

  ;; If the file exists in the current directory, or its name is
  ;; absolute, just load it.
  (if (file-readable? filename)
      (load-and-log filename)
      ;; Otherwise, try to find it in %load-path.
      (let ((file (%search-load-path filename)))
        (if file
            (load-and-log file)
            (log! 'warning (G_ "Could not find file ~S in %load-path.") filename)))))


;;; Loads the list of Scheme scripts LS reporting ERROR-MSG if
;;; something went wrong.  In the latter case, the program exits
;;; with exit status 1.
(define (load-scheme-scripts ls error-msg)
  (catch #t
    (lambda ()
      (for-each load-scheme-script ls))
    (lambda (tag . args)
      (catch-handler tag args)
      (netlist-error 1 error-msg))))


;;; Main program
;;;
( define ( main )
( let
  (
  ( output-filename   (get-output-filename) )
  ( files             (netlist-option-ref '()) )            ; schematics
  ( opt-backend       (netlist-option-ref 'backend) )       ; -g
  ( opt-file-backend  (netlist-option-ref 'file-backend) )  ; -f
  ( opt-interactive   (netlist-option-ref 'interactive) )   ; -i
  ( opt-verbose       (netlist-option-ref 'verbose) )       ; --verbose (-v)
  ( opt-code-to-eval  (netlist-option-ref 'eval-code) )     ; -c
  ( opt-help          (netlist-option-ref 'help) )          ; --help (-h)
  ( opt-version       (netlist-option-ref 'version) )       ; --version (-V)
  ( opt-list-backends (netlist-option-ref 'list-backends) ) ; --list-backends
  ( opt-pre-load      (netlist-option-ref 'pre-load) )      ; -l
  ( opt-post-load     (netlist-option-ref 'post-load) )     ; -m
  ( backend-path      #f )
  ( schematic         #f )
  ( backend-proc-name #f )
  )

  ; local functions:

  ( define ( error-no-backend )
    (netlist-error 1 (G_ "You gave neither backend to execute nor interactive mode!\n"))
  )

  ( define ( error-no-sch )
    (netlist-error 1 (G_ "No schematic files specified for processing.\n~
                         Run `~A --help' for more information.\n")
                     (car (program-arguments)))
  )

  ( define ( error-backend-not-found backend )
    (netlist-error 1 (G_ "Could not find backend `~A' in load path.\n~
                         Run `~A --list-backends' for a full list of available backends.\n")
                     backend (car (program-arguments)))
  )

  ( define ( error-backend-file-name name )
    (netlist-error 1 (G_ "Can't load backend file ~S.\n~
                         Backend files are expected to have names like \"gnet-NAME.scm\"\n~
                         and contain entry point function NAME (where NAME is the backend's name).\n")
                     name)
  )


  ; Parse configuration:
  ;
  (parse-rc "lepton-netlist" "gnetlistrc")

  ; Set default netlist mode:
  ;
  ( set-netlist-mode! (default-netlist-mode) )

  ; Evaluate Scheme expression at startup (-c EXPR):
  ;
  ( unless ( null? opt-code-to-eval )
    ( catch #t
      ( lambda()
        (for-each eval-string opt-code-to-eval)
      )
      ( lambda( tag . args )
        ( catch-handler tag args )
        ( netlist-error 1 (G_ "Failed to evaluate Scheme expression at startup.\n") )
      )
    )
  )

  ( when opt-help
    ( usage )
  )

  ( when opt-version
    ( display-lepton-version #:print-name #t #:copyright #t )
    ( primitive-exit 0 )
  )

  ( when opt-list-backends
    ( lookup-backends )
    ( primitive-exit 0 )
  )

  ; Check input schematics:
  ;
  ( when ( and (null? files) (not opt-interactive) )
    ( error-no-sch )
  )


  ; Load Scheme FILE before loading backend (-l FILE):
  (load-scheme-scripts opt-pre-load
                       (G_ "Failed to load Scheme file before loading backend.\n"))

  ; Backend specified by name:
  ;
  ( when opt-backend
    ( set! backend-path ( %search-load-path (format #f "gnet-~A.scm" opt-backend) ) )
    ( set! backend-proc-name opt-backend )
  )

  ; Backend specified by file name:
  ;
  ( when opt-file-backend
    ( set! backend-path opt-file-backend )
    (let ((proc-name (backend-filename->proc-name backend-path)))
      (if proc-name
          (set! backend-proc-name proc-name)
          (error-backend-file-name backend-path))))


  ; Load backend file:
  ;
  ( when backend-path
    ( catch #t
      ( lambda()
        ( primitive-load backend-path )
        ( query-backend-mode )
      )
      ( lambda( tag . args )
        ( catch-handler tag args )
        ( netlist-error 1 (G_ "Failed to load backend file.\n") )
      )
    )
  )

  ; Load Scheme FILE after loading backend (-m FILE):
  (load-scheme-scripts opt-post-load
                       (G_ "Failed to load Scheme file after loading backend.\n"))

  ; Verbose mode (-v): print configuration:
  ;
  ( when opt-verbose
    ( print-netlist-config )
  )

  ; Neither backend (-g or -f), nor interactive mode (-i) specified:
  ;
  ( unless ( or opt-backend opt-file-backend opt-interactive )
    ( error-no-backend )
  )

  ; This sets [toplevel-schematic] global variable:
  ;
  (set! schematic (set-ln-toplevel-schematic! files))

  ; Verbose mode (-v): print internal netlist representation:
  ;
  ( when opt-verbose
    ( verbose-print-netlist (schematic-components schematic) )
  )


  ; Do actual work:
  ;
  ( if opt-interactive
    ( lepton-repl )                             ; if
    ( if ( and backend-path backend-proc-name ) ; else
      ( run-backend backend-proc-name output-filename ) ; if
      ( error-backend-not-found opt-backend )           ; else
    )
  )

) ; let
) ; main()
