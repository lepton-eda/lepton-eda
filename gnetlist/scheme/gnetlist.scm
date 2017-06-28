;;; Lepton EDA Netlister
;;; Copyright (C) 1998-2010 Ales Hvezda
;;; Copyright (C) 1998-2017 gEDA Contributors (see ChangeLog for details)
;;; Copyright (C) 2017 Lepton EDA Contributors
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

(define-module (gnetlist)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)
  #:use-module ((ice-9 rdelim) #:select (read-string) #:prefix rdelim:)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 i18n)
  #:use-module (gnetlist option)
  #:use-module (gnetlist package)
  #:use-module (gnetlist sort)
  #:use-module (gnetlist attrib compare)
  #:use-module (geda library)
  #:use-module (geda page)
  #:use-module (geda deprecated)
  #:use-module (geda log)
  #:use-module (geda repl)
  #:use-module (lepton version)
  #:use-module (gnetlist core gettext)
  #:use-module (gnetlist config)
  #:use-module (gnetlist schematic)
  #:use-module (gnetlist package-pin)
  #:use-module (gnetlist pin-net)
  #:use-module (gnetlist verbose)
  #:use-module ((gnetlist rename) #:select (get-rename-list))

  #:export (main
            toplevel-schematic
            calling-flag?
            get-device
            get-all-connections
            get-all-package-attributes
            get-component-text
            get-nets
            get-pins
            get-pins-nets
            message
            package-pin-netname
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

  #:re-export (source-library))

;;; Create log file right away even if logging is enabled.
(init-log "gnetlist")

(match (lepton-version)
  ((prepend-string dotted-version date-version git-commit)
   (log! 'message
         (_
          "gEDA/gnetlist version ~A~A.~A
gEDA/gnetlist comes with ABSOLUTELY NO WARRANTY; see COPYING for more details.
This is free software, and you are welcome to redistribute it under certain
conditions; please see the COPYING file for more details.
")
         prepend-string
         dotted-version
         date-version))
  (_ #f))

;;; Print version info and exit.
;;; Print gEDA version, and copyright/warranty notices, and exit with
;;; exit status 0.
(define (version)
  (match (lepton-version)
    ((prepend dotted date commit)
     (format #t (_ "Lepton EDA ~A (g~A)
Copyright (C) 1998-2016 gEDA developers
Copyright (C) 2017 Lepton EDA developers
This is free software, and you are welcome to redistribute it under
certain conditions. For details, see the file `COPYING', which is
included in the Lepton EDA distribution.
There is NO WARRANTY, to the extent permitted by law.
")
             dotted (string-take commit 7))))
  (primitive-exit 0))

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

;;; Returns the list of attached attributes of PACKAGE with given
;;; NAME which must be a symbol (not string). If no attached
;;; attributes found, returns the list of inherited attributes
;;; with given NAME. If neither attached nor inherited attributes
;;; have been found, returns #f.
(define (package-attributes package name)
  (or (assq-ref (package-attribs package) name)
      (assq-ref (package-iattribs package) name)))

;;; Returns first attached attribute of PACKAGE with given NAME
;;; which must be a symbol (not string). If no attached attribute
;;; found, returns first inherited attribute with NAME. If neither
;;; attached nor inherited attribute found, returns #f.
(define (package-attribute package name)
  (and=> (package-attributes package name) car))


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
               (format (current-error-port) (_ "\
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
    (let ((name (package-refdes package)))
      (and name
           (string=? name package-name)
           package)))

  (map
   (lambda (package)
     (package-attribute package sname))
   (filter-map found-package? (schematic-netlist toplevel-schematic))))


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
                        (_ "Uref ~a: Bad slot number: ~a.\n") refdes slot)
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
               (display (_ "Couldn't wrap string  at requested position\n"))
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

(define toplevel-schematic #f)


;;; Helper function for sorting connections.
(define (pair<? a b)
  (or (refdes<? (car a) (car b))
      (and (string=? (car a) (car b))
           (refdes<? (cdr a) (cdr b)))))



(define (get-all-connections netname)
  "Returns all connections in the form of ((refdes pin) ...) for
NETNAME."
  (define (found? x)
    (and x
         (string=? x netname)))

  (define (get-found-pin-connections pin)
    (if (found? (package-pin-name pin))
        (filter-map
         (lambda (net) (let ((package (pin-net-connection-package net))
                        (pinnumber (pin-net-connection-pinnumber net)))
                    (and package
                         pinnumber
                         (cons package pinnumber))))
         (package-pin-nets pin))
        '()))

  (define (get-netlist-connections netlist)
    (append-map
     (lambda (package)
       (append-map get-found-pin-connections (package-pins package)))
     netlist))

  (sort-remove-duplicates (get-netlist-connections (schematic-netlist toplevel-schematic))
                          pair<?))


(define (get-pins-nets package)
  "Returns a list of pairs (pin-name . net-name) where net-name is
the name of the net connected to the pin pin-name for specified
PACKAGE."

  (define (found? x)
    (and x
         (string=? x package)))

  (define (get-pin-netname-pair pin)
    (let ((pin-number (package-pin-number pin))
          (pin-name (package-pin-name pin)))
      (and pin-number
           pin-name
           (cons pin-number pin-name))))

  (define (get-pin-netname-list package)
     (if (found? (package-refdes package))
         (filter-map get-pin-netname-pair (package-pins package))
         '()))

  ;; Currently, netlist can contain many `packages' with the same
  ;; name, so we have to deal with this.
  (let ((result-list (append-map get-pin-netname-list
                                 (schematic-netlist toplevel-schematic))))
    (sort-remove-duplicates result-list pair<?)))


(define (get-pins refdes)
  (define (found? x)
    (and x
         (string=? x refdes)))

  (sort-remove-duplicates
   (append-map
    (lambda (package)
      (if (found? (package-refdes package))
          (filter-map package-pin-number (package-pins package))
          '()))
    (schematic-netlist toplevel-schematic))
   refdes<?))


;;; This procedure is buggy in the same way as gnetlist:get-nets.
;;; It should first search for netname, and then get all
;;; package-pin pairs by that netname.
(define (get-nets package pin-number)
  (define (net-connections nets)
    (filter-map
     (lambda (net)
       (let ((package (pin-net-connection-package net))
             (pinnumber (pin-net-connection-pinnumber net)))
         (and package
              pinnumber
              (cons package pinnumber))))
     nets))

  (define (lookup-through-nets nets package pin-number)
    (let ((connections (net-connections nets)))
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
                  (lookup-through-nets (package-pin-nets pin)
                                       package
                                       pin-number))))
     pins))

  (define (found-package? x)
    (and x
         (string=? x package)))

  (define (lookup-through-netlist netlist)
    (append-map
     (lambda (package)
       (if (found-package? (package-refdes package))
           (lookup-through-pins (package-pins package))
           '()))
     netlist))

  (let ((found (lookup-through-netlist (schematic-netlist toplevel-schematic))))
    (match found
      (() '("ERROR_INVALID_PIN"))
      (((netname . rest) ...)
       (cons (car netname) (append-map identity (filter-map identity rest))))
      (_ '("ERROR_INVALID_PIN")))))


(define (package-pin-netname package pinnumber)
  (or (assoc-ref (get-pins-nets package) pinnumber)
      "ERROR_INVALID_PIN"))

;;; Backward compatibility variables and procedures
(define packages #f)
(define all-unique-nets #f)
(define all-nets #f)
(define all-pins #f)

(define (gnetlist:get-toplevel-attribute attrib)
  (or (assq-ref (schematic-toplevel-attribs toplevel-schematic)
                (string->symbol attrib))
      "not found"))

(define gnetlist:get-pins get-pins)
(define gnetlist:get-all-package-attributes get-all-package-attributes)
(define gnetlist:get-nets get-nets)
(define gnetlist:get-pins-nets get-pins-nets)
(define (gnetlist:get-verbosity)
  (if (gnetlist-option-ref 'verbose)
      1
      (if (gnetlist-option-ref 'quiet)
          -1
          0)))
(define (gnetlist:get-input-files)
  (gnetlist-option-ref '()))
(define (gnetlist:get-command-line)
  (string-join (command-line) " "))
(define (gnetlist:get-packages level)
  (schematic-packages toplevel-schematic))
(define (gnetlist:get-non-unique-packages level)
  (schematic-non-unique-packages toplevel-schematic))
(define (gnetlist:get-all-unique-nets level)
  (schematic-nets toplevel-schematic))
(define (gnetlist:get-all-nets level)
  (schematic-non-unique-nets toplevel-schematic))
(define (gnetlist:get-all-connections netname)
  (map (lambda (pair) (list (car pair) (cdr pair)))
       (get-all-connections netname)))
(define (gnetlist:get-backend-arguments)
  (gnetlist-option-ref 'backend-option))
(define (gnetlist:get-renamed-nets level)
  (map
   (lambda (rename)
     (match rename
       ((src . dest)
        (list src dest))
       (_ #f)))
   (get-rename-list)))

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

            (if (hash-ref gnetlist:net-hash-reverse alias)
                (error (format #f
                               (_ "There is a net name collision!
The net called \"~A\" will be remapped
to \"~A\" which is already used
by the net called \"~A\".
This may be caused by netname attributes colliding with other netnames
due to truncation of the name, case insensitivity, or
other limitations imposed by this netlist format.
")
                               net
                               alias
                               (hash-ref gnetlist:net-hash-reverse alias))))
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

            (if (hash-ref gnetlist:refdes-hash-reverse alias)
                (error
                 (format #f
                         (_ "There is a refdes name collision!
The refdes \"~A\" will be mapped\nto \"~A\" which is already used
by \"~A\".
This may be caused by refdes attributes colliding with others
due to truncation of the refdes, case insensitivity, or
other limitations imposed by this netlist format.
")
                         refdes
                         alias
                         (hash-ref gnetlist:refdes-hash-reverse alias))))
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

  (let loop ((netlist (schematic-netlist toplevel-schematic)))
    (if (null? netlist)
        "unknown"
        (or (and (found-refdes? (package-refdes (car netlist)))
                 (let ((pin (find-pin-by-attrib (package-pins (car netlist))
                                                (string->symbol pin-attrib-name)
                                                pin-attrib-value)))
                   (if pin
                       (assq-ref (package-pin-attribs pin)
                                 (string->symbol name))
                       (and func (func (package-pins (car netlist))
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
  (define (found? x)
    (and x
         (string=? x netname)))

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
           (if (and (have-netname? (package-pins package))
                    (have-attrib? (package-attribs package) in-attrib))
               (assq-ref (package-attribs package) out-attrib)
               '()))
         (schematic-graphicals toplevel-schematic)))))

(define (get-output-filename)
  ;; Name is file name or "-" which means stdout.
  (let ((name (gnetlist-option-ref 'output)))
    (and (not (string=? name "-"))
         name)))

(define quiet-mode (gnetlist-option-ref 'quiet))

;;; List of processed rc directories.
(define %rc-dirs (make-hash-table))

;;; Backward compatibility stuff.
;;; Process "gafrc" file in SCHEMATIC-NAME's directory.
(define (process-gafrc schematic-name)
  (let ((cwd (getcwd)))
    (unless (hash-ref %rc-dirs cwd)
      (chdir (dirname schematic-name))
      ((@@ (guile-user) parse-rc) "gnetlist" "gafrc")
      (hash-set! %rc-dirs cwd cwd)
      (chdir cwd))))

;;; Reads file NAME and outputs a page named NAME
(define (file->page name)
  (with-input-from-file name
    (lambda ()
      (when (not quiet-mode)
        (log! 'message (_ "Loading schematic ~S\n") name))
      (string->page name (rdelim:read-string)))))

;;; Prints a list of available backends.
(define (gnetlist-backends)
  "Prints a list of available gnetlist backends by searching for
files in each of the directories in the current Guile %load-path.
A file is considered to be a gnetlist backend if its basename
begins with \"gnet-\" and ends with \".scm\"."
  (define backend-prefix "gnet-")
  (define backend-suffix ".scm")
  (define prefix-length (string-length backend-prefix))
  (define suffix-length (string-length backend-suffix))

  (define (backend? filename)
    (and (string-prefix? backend-prefix filename)
         (string-suffix? backend-suffix filename)))

  (define (backend-name filename)
    (string-drop-right (string-drop filename prefix-length) suffix-length))

  (define (path-backends path)
    (or (scandir path backend?)
        (begin
          (log! 'warning (_ "Can't open directory ~S.\n") path)
          '())))

  (let ((backend-files (append-map path-backends %load-path)))
    (display (_ "List of available backends: \n\n"))
    (display (string-join
              (sort! (map backend-name backend-files) string-locale<?)
              "\n"
              'suffix))))

(define (usage)
  (format #t (_
    "Usage: ~A [OPTION ...] [-g BACKEND] [--] FILE ...

Generate a netlist from one or more gEDA schematic FILEs.

General options:
  -q              Quiet mode.
  -v, --verbose   Verbose mode.
  -o FILE         Filename for netlist data output.
  -L DIR          Add DIR to Scheme search path.
  -g BACKEND      Specify netlist backend to use.
  -O STRING       Pass an option string to backend.
  -l FILE         Load Scheme file before loading backend.
  -m FILE         Load Scheme file after loading backend.
  -c EXPR         Evaluate Scheme expression at startup.
  -i              Enter interactive Scheme REPL after loading.
  --list-backends Print a list of available netlist backends.
  -h, --help      Help; this message.
  -V, --version   Show version information.
  --              Treat all remaining arguments as filenames.

Report bugs at <https://github.com/lepton-eda/lepton-eda/issues>
Lepton EDA homepage: <https://github.com/lepton-eda/lepton-eda>
")
          (car (program-arguments)))
  (primitive-exit 0))


(define (set-toplevel-schematic! files netlist-mode)
  (and (eq? netlist-mode 'spice)
       (set! get-uref get-spice-refdes))
  (for-each process-gafrc files)
  (set! toplevel-schematic
        (make-toplevel-schematic (map file->page files)
                                 netlist-mode))
  ;; Backward compatibility variables. Don't use them in your code!!!
  (set! packages (schematic-packages toplevel-schematic))
  (set! all-unique-nets (schematic-nets toplevel-schematic))
  (set! all-nets (schematic-non-unique-nets toplevel-schematic))
  (set! all-pins (map gnetlist:get-pins packages))
  toplevel-schematic)


(define (catch-handler tag . args)
  (format (current-error-port)
          (_ "\nJust got an error '~A':\n        ~A\n\n")
          tag
          args)
  #f)

(define (run-backend backend output-filename)
  (let ((backend-proc (primitive-eval (string->symbol backend))))
    (if output-filename
        ;; output-filename is defined, output to it.
        (with-output-to-file output-filename
          (lambda () (backend-proc output-filename)))
        ;; output-filename is #f, output to stdout.
        (backend-proc output-filename))))

;;; Main program
;;;
(define (main)
  (let ((code-to-eval (gnetlist-option-ref 'eval-code)))
    (when (not (null? code-to-eval))
      (catch #t
        (lambda () (apply eval-string code-to-eval))
        catch-handler)))

  (when (gnetlist-option-ref 'help)
    (usage))

  (when (gnetlist-option-ref 'version)
    (version))

  ((@@ (guile-user) parse-rc) "gnetlist" "gnetlistrc")
  (if (gnetlist-option-ref 'list-backends)
      (gnetlist-backends)
      (let ((files (gnetlist-option-ref '())))
        (if (null? files)
            (error (format #f
                           (_ "No schematic files specified for processing.
Run `~A --help' for more information.
")
                           (car (program-arguments))))
            (let* ((backend (gnetlist-option-ref 'backend))
                   ;; this is a kludge to make sure that spice mode gets set
                   (netlist-mode (if (and backend (string-prefix? "spice" backend))
                                     'spice
                                     'geda))

                   ;; Search for backend scm file in load path
                   (backend-path (and backend
                                      (%search-load-path (format #f
                                                                 "gnet-~A.scm"
                                                                 backend))))
                   (output-filename (get-output-filename))
                   (interactive-mode? (gnetlist-option-ref 'interactive))
                   (verbose-mode? (gnetlist-option-ref 'verbose)))

              ;; Evaluate the first set of Scheme expressions.
              (for-each primitive-load (gnetlist-option-ref 'pre-load))
              ;; Load backend.
              (when backend-path (primitive-load backend-path))
              ;; Evaluate second set of Scheme expressions.
              (for-each primitive-load (gnetlist-option-ref 'post-load))

              (when verbose-mode? (print-gnetlist-config))

              (if (or backend interactive-mode?)
                  ;; Load backend code.
                  (let ((schematic (set-toplevel-schematic! files netlist-mode)))
                    (verbose-print-netlist (schematic-netlist schematic))
                    (if interactive-mode?
                        (lepton-repl)
                        (if backend-path
                            (run-backend backend output-filename)
                            ;; If the backend couldn't be found, fail.
                            (error (format #f (_ "Could not find backend `~A' in load path.

Run `~A --list-backends' for a full list of available backends.
")
                                           backend
                                           (car (program-arguments)))))))
                  ;; No backend given on the command line.
                  (format (current-error-port)
                          (_ "You gave neither backend to execute nor interactive mode!\n"))))))))
