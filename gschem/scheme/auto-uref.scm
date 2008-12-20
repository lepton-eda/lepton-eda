;; gEDA - GPL Electronic Design Automation
;; gschem - gEDA Schematic Capture
;; Copyright (C) 1998-2008 Ales Hvezda
;; Copyright (C) 1998-2008 gEDA Contributors (see ChangeLog for details)
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111 USA

(use-modules (ice-9 regex))

(define prefix-list '())

(define (auto-uref attribs)

  (define (get-next-uref prefix)
    (let ((available-prefix (assoc prefix prefix-list)))
      (cond (available-prefix 
	     (assoc-set! prefix-list
			 (car available-prefix)
			 (+ (cdr available-prefix) 1))
	     (cdr available-prefix))
	    (else ; First time prefix was seen
	     (set! prefix-list (acons  prefix 1 prefix-list))
	     1))))
  
  
  ;; Total Guile
  (define (get-prefix value)
    (let ((prefix (string-match "^[A-Z]*" value)))
      (if (= 0 (match:end prefix))
	  #f
	  (match:substring prefix))))
  

  (for-each 
   (lambda (attrib) 
     (let* ((name-value (get-attribute-name-value attrib))
	    (name (car name-value))
	    (value (cdr name-value))
	    (prefix (get-prefix value)))
       ; If get-prefix fails (returns #f) there is no ? in the string
       (if (and prefix (string=? name "refdes"))
	   (set-attribute-value! attrib (string-append 
					 prefix 
					 (number->string (get-next-uref prefix)))))))
   attribs))
