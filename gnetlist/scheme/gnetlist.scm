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

(use-modules (srfi srfi-1))
(use-modules (geda deprecated))

;;----------------------------------------------------------------------
;; The below functions added by SDB in Sept 2003 to support command-line flag
;; processing.
;;----------------------------------------------------------------------

;;---------------------------------------------------------------
;;  debug-spew
;;  Wrapper which spews debug messages if -v flag is set, otherwise
;;  does nothing.
;;  Calling form:  (debug-spew "verbose debug text")
;;--------------------------------------------------------------
(define debug-spew
  (lambda (debug-string)
    (if (= 1 (gnetlist:get-verbosity))
        (message debug-string)
)))


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
               (format (current-error-port) "\
Possible attribute conflict for refdes: ~A
name: ~A
values: ~A
" refdes name values))
           value))))

(define (gnetlist:get-package-attribute refdes name)
  "Return the value associated with attribute NAME on package
identified by REFDES.

It actually computes a single value from the full list of values
produced by 'gnetlist:get-all-package-attributes' as that list is
passed through 'unique-attribute'.

The default behavior is to return the value associated with the
first symbol instance for REFDES having the attribute NAME. If
some of the instances of REFDES have different value for NAME, it
prints a warning."
  (let* ((values (gnetlist:get-all-package-attributes refdes name))
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
                        "Uref ~a: Bad slot number: ~a.\n" refdes slot)
                #f))
          ;; no slot attribute, assume slot number is 1
          1))
    (gnetlist:get-all-package-attributes refdes "slot"))
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


;; this is really crude, but I'm tired... :)
(define display-nl
   (lambda (list)
      (display list)
      (newline)))


;; ah.. wonder what use this is...
(define display-pin
   (lambda (pin-list)
      (for-each display-nl pin-list)))


;; ha. I'm playing with scheme here.. don't mind me
(define display-all-pins
   (lambda ()
      (for-each display-pin all-pins)))


;; another misc function
(define print-packages
   (lambda (plist)
      (for-each display-nl plist)))

;; ETTUS
;; find-device
;; Usage:  (find-device packages devicename)
;; Returns the first package which matches the devicename
(define find-device
   (lambda (components devicename)
      (if (not (null? components))
         (if (string=? devicename (get-device (car components)))
            (car components)
            (find-device (cdr components) devicename)))))


;; ETTUS
;; find-devices
;; Usage:  (find-devices packages devicename '())
;; Returns a list of packages which match the device name
(define find-devices
   (lambda (components devicename list)
      (if (not (null? components))
         (if (string=? devicename (get-device (car components)))
            (find-devices (cdr components)
                                devicename
                                (cons (car components) list))
            (find-devices (cdr components)
                                devicename
                                list))
         list)))

;; ETTUS
;; contains?
;; Usage (contains? list item)
;; True if the list contains the item, according to string=?
(define contains?
   (lambda (ls item)
      (cond
         ((null? ls) #f)
         ((string=? item (car ls)) #t)
         (#t (contains? (cdr ls) item)))))

;; ETTUS
;; Usage: (number-nets all-unique-nets 1)
;; Returns a list of pairs of form (netname . number)
(define (number-nets nets number)
  (define (number-nets-impl in i out)
    (if (null? in)
        (reverse! out) ; Return value
        (let ((netname (car in)))
          (if (string=? "GND" netname)
              (number-nets-impl (cdr in) i (cons (cons netname 0) out))
              (number-nets-impl (cdr in) (1+ i) (cons (cons netname i) out))))))
  (number-nets-impl nets number '()))

;; ETTUS
;; Usage: (get-net-number netname numberlist)
;; numberlist should be from (number-nets) above
;; Returns the number corresponding to the net
(define get-net-number
   (lambda (netname numberlist)
      (if (not (null? numberlist))
         (if (string=? netname (car (car numberlist)))
            (cdr (car numberlist))
            (get-net-number netname (cdr numberlist))))))

;;
;; Useful output functions contributed by Andrew Bardsley
;;
(define (print-to-port port . l)
    (for-each (lambda (elem) (display elem port)) l))

(define (print . l)
    (apply print-to-port (cons (current-output-port) l)))

;;
;; Wrap a string into lines no longer than wrap-length
;; wrap-char is put on the end-of-the-wrapped-line, before the return
;; (from Stefan Petersen)
(define (gnetlist:wrap string-to-wrap wrap-length wrap-char)
  (if (> wrap-length (string-length string-to-wrap))
      string-to-wrap ; Last snippet of string
      (let ((pos (string-rindex string-to-wrap #\space 0 wrap-length)))
	(cond ((not pos)
	       (display "Couldn't wrap string  at requested position\n")
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

;; determine the uref to use for a particular OBJECT
(define (gnetlist:get-uref object)
  ; Returns first value of first attrib found with given name, or #f.
  (define (attrib-first-value object name)
    (let ((attrib-lst (get-attrib-value-by-attrib-name object name)))
      (if (null? attrib-lst) #f (car attrib-lst))))
  ; Handler if we find uref=
  (define (handle-uref value)
    (simple-format (current-output-port)
                   "WARNING: Found uref=~A" value)
    (newline)
    (simple-format (current-output-port)
                   "uref= is deprecated, please use refdes=~A" value)
    (newline)
    value)

  ; Actually find attribute: check refdes, then uref, then return #f.
  (cond
   ((attrib-first-value object "refdes") => (lambda (x) x))
   ((attrib-first-value object "uref") => handle-uref)
   (else #f)))

;; define the default handler for get-uref
(define get-uref gnetlist:get-uref)

(define (gnetlist:stdout? output-filename)
  (string=? output-filename "-"))

;; If the output file name is "-", use stdout instead
(define (gnetlist:output-port output-filename)
  (if (gnetlist:stdout? output-filename)
    (current-output-port)
    (open-output-file output-filename)))

;; Where to output messages for the user
(define message-port (current-error-port))
;; Procedure to output messages to message-port
(define (message output-string)
  (display output-string message-port)
  )
