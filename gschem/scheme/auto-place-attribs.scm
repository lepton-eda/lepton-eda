;;; gEDA - GNU Electronic Design Automation
;;; gschem - gEDA Schematic Capture
;;; Copyright (C) 1998-2005 Ales V. Hvezda
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
;; Code to place new text attributes automatically 
;; written by Carlos Nieves Onega starts here.
;;

; Copyright (C) 2006 Carlos Nieves Onega
; This function returns the pin direction of the pin object parameter.
; It returns a one character string: "^", "v", "<" or ">". The arrow
; points the pin's end, which is NOT the active connection end.
; This function takes care of the pin's whichend property: if it's 1,
; then the pin ends should be reversed.
(define get-pin-direction
   (lambda (pin)
     (let* ( (pin-ends (get-pin-ends pin))
	     (pin-beginning (car pin-ends))
	     (pin-end (cdr pin-ends)) )
       (begin
	 (if (eq? (car pin-beginning) (car pin-end) )
	     (if (<= (cdr pin-beginning) (cdr pin-end))
		    ; The x coords are equal. The pin is vertical.
		    "^"
		    "v")
	     (if (<= (car pin-beginning) (car pin-end))
		    ; The x coords are not equal. The pin is horizontal.
		    ">"
		    "<"))))))

; This function gets the reference point of an object.
; The position string is the reference to return. It has the format:
;   "horizontal vertical", where: 
;     - "horizontal" is one of the following: "Left", "Middle", "Right".
;     - "vertical" is one of the following: "Lower", "Middle", "Upper".
;   Example: "Lower Right".
(define (get-reference object position-string)
  (if (not (string-index position-string #\ )) 
      (error "get-reference : Wrong reference format"))
  (let* ( (bounds (get-object-bounds object #f))
	  (horiz-bounds (car bounds))
	  (vertical-bounds (cdr bounds)) 
	  (space-pos (string-index position-string #\ ))
	  (vertical-string (substring position-string 0 space-pos))
	  (horiz-string (substring position-string (+ space-pos 1))) 
	  (horiz-pos (if (string=? horiz-string "Left") 
			 (min (car horiz-bounds) (cdr horiz-bounds))
			 (if (string=? horiz-string "Middle")
			     (inexact->exact (/ (+ (car horiz-bounds)
						   (cdr horiz-bounds)) 2))
			     (if (string=? horiz-string "Right")
				 (max (car horiz-bounds) (cdr horiz-bounds))
				 (error (string-append 
					 "get-reference : Unknown reference (horizontal): " 
					 horiz-string))))))
	  (vertical-pos (if (string=? vertical-string "Lower") 
			    (min (car vertical-bounds) (cdr vertical-bounds))
			    (if (string=? vertical-string "Middle")
				(inexact->exact (/ (+ (car vertical-bounds)
						      (cdr vertical-bounds)) 2))
				(if (string=? vertical-string "Upper")
				    (max (car vertical-bounds) 
					 (cdr vertical-bounds))
				    (error (string-append 
					    "get-reference : Unknown reference (vertical): " 
					    vertical-string)))))) )
    (cons horiz-pos vertical-pos)))
	 
    

; This function sets the default parameters of each attribute,
; provided it is specified in the default-position-of-text-attributes.
; It gets the attrib name from the attribute and sets 
; the text properties as specified in default-position-of-text-attributes.
(define (set-default-position object attribute direction defaults)
  (if (null? defaults)
      0
      (let* ( (attrib-name-value (get-attribute-name-value attribute))
	      (attrib-name (car attrib-name-value)) ; Attribute name
	      (default-def (car defaults)) ; Default definition
	      (def-attrib-name (list-ref default-def ; Default attrib name
					 def-attrib-name-pos))
	      (def-direction (list-ref default-def ; Default direction
					   def-direction-pos)) )
	; Check if the attribute's name and direction matches.
	(if (and (string=? attrib-name def-attrib-name)
		 (string=? def-direction
			   direction))
	    (begin
	      ; It maches, so change the text parameters
	      (let* ( (ref (get-reference object (list-ref default-def 
							   def-reference-pos)))
		      (new-alignment (list-ref default-def 
					       def-alignment-pos)) 
		      (new-angle (list-ref default-def 
					   def-angle-pos))
		      (new-x (+ (list-ref default-def
					  def-x-offset-pos)
				(car ref))) 
		      (new-y (+ (list-ref default-def
					  def-y-offset-pos)
				(cdr ref)))
		      )
		(set-attribute-text-properties! attribute
						"" ; keep previous color
						-1 ; keep previous size
						new-alignment
						new-angle
						new-x
						new-y)
		)
	      )
	    
	    )
	(set-default-position object attribute direction 
			      (cdr defaults)) ; process the rest
	))
  ) ; End of definition of set-default-position

; This function processes the attribute list and calls
; set-default-position for each attribute
(define autoplace-text 
  (lambda (object direction attrib-list)
    (if (not (eq? (length attrib-list) 0))
	(begin
	  (set-default-position object (car attrib-list) direction 
				default-position-of-text-attributes)
	  (autoplace-text object direction (cdr attrib-list))
	  )))) ; End of definition of autoplace-pin-text

	  
; Autoplace the attributes of the given pin object.
(define (autoplace-pin-attributes pin)
  (let ((pin-direction (get-pin-direction pin))
	(attribute-list (get-object-attributes pin)) )
    (autoplace-text pin pin-direction attribute-list)))
								  
								  
; Get the pin directions of the given list of pins.
; It returns a list with all the pin directions of the pins.
(define get-pin-directions 
  (lambda (pins)
    (if (eq? (length pins) 0)
	(list)
	(cons (get-pin-direction (car pins)) 
	      (get-pin-directions (cdr pins))))))

; Get the connection sides where there are pins.
; The parameter pin-directions is a list with the directions of 
; all the pins. (As given by get-pin-directions).
; It returns a string with the sides where there are pins.
; It is needed that the return value doesn't depend on the order of the pins.
; (Notice the arrow always points to the inside of the symbol).
; Examples of return values: "<>^v", "<>", "^v".
(define get-connection-sides
  (lambda (pin-directions)
    (define (check-side side-list pin-directions)
      (if (eq? (length side-list) 0)
	  ""
	  (if (member (car side-list) pin-directions)
	      (string-append (car side-list) 
			     (check-side (cdr side-list) pin-directions))
	      (check-side (cdr side-list) pin-directions))))
    (check-side (list "<" ">" "^" "v") pin-directions)))

; Autoplace the attributes of the given object.
; This function gets some info of the object and calls autoplace-text.
(define (autoplace-object-attributes object)
  (let* ((pin-list (get-object-pins object))
	 (pin-directions (get-pin-directions pin-list))
	 (connection-sides (get-connection-sides pin-directions))
	 (attribute-list (get-object-attributes object)) )
    (autoplace-text object connection-sides attribute-list)))


;;
;; Code to place new text attributes automatically 
;; written by Carlos Nieves Onega ends here.
;;
;; --------------------------------------------------------------------------

