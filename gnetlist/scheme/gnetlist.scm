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
    (if (calling-flag? "verbose_mode" (gnetlist:get-calling-flags))
        (display debug-string) 
)))


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

;; get all packages for a particular schematic page 
;; eventually placeholder will be either the hierarchical level or something 
;; of the sort
(define packages 
  (gnetlist:get-packages "placeholder"))

;; return a list of all unique the nets in the design
(define all-unique-nets
  (gnetlist:get-all-unique-nets "placeholder"))


;; return a list of all the nets in the design
;; Might return duplicates
(define all-nets
  (gnetlist:get-all-nets "placeholder"))

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


;; return all pins for a particular package 
(define pins
   (lambda (package)
      (gnetlist:get-pins package)))

;; not very useful, but amusing 
(define all-pins
   (map gnetlist:get-pins packages))

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
(define number-nets
   (lambda (nets number)
      (if (null? nets)
         '()
         (if (string=? "GND" (car nets))
            (cons (cons "GND" 0) (number-nets (cdr nets) number))
            (cons
               (cons (car nets) number)
               (number-nets (cdr nets)(+ number 1)))))))

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


