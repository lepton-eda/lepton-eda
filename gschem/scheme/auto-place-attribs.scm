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

; Define object types, as in libgeda/include/o_types.h
; TODO: Do this inside libgeda?
(define OBJ_LINE        "L")
(define OBJ_BOX         "B")
(define OBJ_PICTURE     "G")
(define OBJ_CIRCLE      "V")
(define OBJ_NET         "N")
(define OBJ_BUS         "U")
(define OBJ_COMPLEX     "C")
(define OBJ_TEXT        "T")
(define OBJ_PIN         "P")
(define OBJ_ARC         "A")
(define OBJ_ROUTE       "R")
(define OBJ_THRU_HOLE   "H") 
(define OBJ_PLACEHOLDER "X")


; Given a bound,  defined as a list of the form ( (x1 x2) (y1 y2) ) with:
;   - (x1, y1): bottom left corner.
;   - (x2, y2): upper right corner.
; Returns:
;   - The minimum x value if point is "min-x".
;   - The maximum x value if point is "max-x".
;   - The minimum y value if point is "min-y".
;   - The maximum y value if point is "max-y".
(define get-point-of-bound
  (lambda (point bound)
    (if (string=? point "min-x")
	(min (car (car bound))
	     (cdr (car bound)))
	(if (string=? point "max-x")
	    (max (car (car bound))
		 (cdr (car bound)))
	    (if (string=? point "min-y")
		(min (car (cdr bound))
		     (cdr (cdr bound)))
		(if (string=? point "max-y")
		    (max (car (cdr bound))
			 (cdr (cdr bound)))
		    (error (string-append 
			    "get-point-of-bound : Unknown point to get: "
			    point))
		    ))))))

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

; This function returns a list with the end coordinate of the pins, 
; if they are in the desired side.
;   - desired_side: is a one character string: "^", "v", "<" or ">".
;   - coordinate: is a one character string: 
;     - "B" if the pin beginnings are desired.
;     - "E" if the pin ends are desired.
(define get-bound-of-pins 
  (lambda (desired_side coordinate pins)
    (if (eq? (length pins) 0)
	(list)
	(let* ( (pin (car pins))
		(pin-ends (get-pin-ends pin))
		(pin-beginning (car pin-ends))
		(pin-end (cdr pin-ends)) 
		)
	  (begin
	    (if (string=? (get-pin-direction pin) desired_side)
		(if (string=? coordinate "B")
		    (cons (car pin-beginning)
			  (cons (car pin-end)
				(get-bound-of-pins desired_side
						   coordinate
						   (cdr pins))))
		    (if (string=? coordinate "E")
			(cons (cdr pin-beginning)
			      (cons (cdr pin-end)
				    (get-bound-of-pins desired_side
						       coordinate
						       (cdr pins))))
			(error (string-append 
				"get-bound-of-pin : Unknown coordinate: "
				coordinate))))
		(get-bound-of-pins desired_side coordinate (cdr pins))))
	  )
	)))

; This function returns the bounds of the pins in the given side of the object
; The side is a one character string: "^", "v", "<" or ">". The arrow
; points the pin's end, which is NOT the active connection end.
(define get-bounds-of-pins-in-side
   (lambda (object desired_side)
     (let* ( (pins (get-object-pins object))
 	     (pins-beginning (get-bound-of-pins desired_side "B" pins))
 	     (pins-beginning-sorted (stable-sort pins-beginning <))
 	     (pins-end (get-bound-of-pins desired_side "E" pins))
 	     (pins-end-sorted (stable-sort pins-end <))	     
	     )
       (begin
	 (if (or (eq? (length pins-beginning-sorted) 0)
		 (eq? (length pins-end-sorted) 0))
	     (list)
	     (let* ( (min-x (car pins-beginning-sorted))
		     (max-x (list-ref pins-beginning-sorted 
				      (- (length pins-beginning-sorted) 1)))
		     (min-y (car pins-end-sorted))
		     (max-y (list-ref pins-end-sorted 
				      (- (length pins-end-sorted) 1))))
	       (cons (cons min-x max-x) (cons min-y max-y)))
	     )
       ))))

; This function returns the bounds of the pins in the given side of the object
; The side is a one character string: "^", "v", "<" or ">". The arrow
; points the pin's end, which is NOT the active connection end.
(define get-bounds-of-pins-with-attribs-in-side
   (lambda (object desired_side)
     (define get-bound-of-list-of-pins-with-attribs
       (lambda (bounds desired-side pin-list)
	 (if (null? pin-list)
	     bounds
	     (begin 
	       (let* ( (pin (car pin-list))
		       (pin-direction (get-pin-direction pin))
		       (pin-bounds (get-object-bounds pin (list) (list)))
		       (new-bounds bounds)		     
		       (old-bounds bounds)
		       )
		 (begin
		   (if (string=? pin-direction desired-side)
		       (begin
			 (if (null? bounds)
			     (begin 
			       (set! old-bounds pin-bounds)
			       ))
			 (if (not (null? pin-bounds))
			     (set! new-bounds
				   (cons (cons
					  (min (get-point-of-bound 
						"min-x" pin-bounds)
					       (get-point-of-bound 
						"min-x" old-bounds))
					  (max (get-point-of-bound 
						"max-x" pin-bounds)
					       (get-point-of-bound 
						"max-x" old-bounds)))
					 (cons
					  (min (get-point-of-bound 
						"min-y" pin-bounds)
					       (get-point-of-bound 
						"min-y" old-bounds))
					  (max (get-point-of-bound 
						"max-y" pin-bounds)
					       (get-point-of-bound 
						"max-y" old-bounds))))))))
		   (get-bound-of-list-of-pins-with-attribs 
		    new-bounds desired-side (cdr pin-list))
		   ))))))

     (get-bound-of-list-of-pins-with-attribs
      (list) 
      desired_side 
      (get-object-pins object))
))

; Check if a point (x,y) if inside a region with the given bounds.
;   - bounds is a list of the form ( (x1 x2) (y1 y2) ) with:
;      - (x1, y1): bottom left corner.
;      - (x2, y2): upper right corner.
; Return true if the point is inside the region, or false otherwise.
(define inside-region 
  (lambda (bounds x y)
    (let* ( (right (get-point-of-bound "max-x" bounds))
	    (left  (get-point-of-bound "min-x" bounds))
	    (top   (get-point-of-bound "max-y" bounds))
	    (bottom (get-point-of-bound "min-y" bounds))
	    (collision (and (>= x left) (<= x right) (<= y top) (>= y bottom)))
	    )
      (begin 
	collision))))
  
; Chech if two regions are overlapping.
; Each bound is defined as a list of the form ( (x1 x2) (y1 y2) ) with:
;   - (x1, y1): bottom left corner.
;   - (x2, y2): upper right corner.
; Return true if the regions are overlapping, or false otherwise.
(define check-collision-of-bounds
  (lambda (bounds1 bounds2)
    (let* ( (bounds1_x1 (get-point-of-bound "min-x" bounds1))
	    (bounds1_x2 (get-point-of-bound "max-x" bounds1))
	    (bounds1_y1 (get-point-of-bound "min-y" bounds1))
	    (bounds1_y2 (get-point-of-bound "max-y" bounds1))

	    (bounds2_x1 (get-point-of-bound "min-x" bounds2))
	    (bounds2_x2 (get-point-of-bound "max-x" bounds2))
	    (bounds2_y1 (get-point-of-bound "min-y" bounds2))
	    (bounds2_y2 (get-point-of-bound "max-y" bounds2))

	    )
      (begin
	(or (inside-region bounds1 bounds2_x1 bounds2_y1)
	    (inside-region bounds1 bounds2_x2 bounds2_y2)
	    (inside-region bounds1 bounds2_x1 bounds2_y2)
	    (inside-region bounds1 bounds2_x2 bounds2_y1)
	    
	    (inside-region bounds2 bounds1_x1 bounds1_y1)
	    (inside-region bounds2 bounds1_x2 bounds1_y2)
	    (inside-region bounds2 bounds1_x1 bounds1_y2)
	    (inside-region bounds2 bounds1_x2 bounds1_y1)

	    ; horizontal bounds or region 1 are within
	    ; horizontal bounds of region 2 and 
	    ; vertical bounds of region 1 are within 
	    ; vertical bounds of region 2
	    (and (< bounds1_x1 bounds2_x1)
		 (< bounds1_x1 bounds2_x2)
		 (> bounds1_x2 bounds2_x1)
		 (> bounds1_x2 bounds2_x2)
		 (> bounds1_y1 bounds2_y1)
		 (< bounds1_y2 bounds2_y2))

	    ; horizontal bounds or region 2 are within
	    ; horizontal bounds of region 1 and 
	    ; vertical bounds of region 2 are within 
	    ; vertical bounds of region 1
	    (and (< bounds2_x1 bounds1_x1)
		 (< bounds2_x1 bounds1_x2)
		 (> bounds2_x2 bounds1_x1)
		 (> bounds2_x2 bounds1_x2)
		 (> bounds2_y1 bounds1_y1)
		 (< bounds2_y2 bounds1_y2)))))))

; Chech if the attribute bounds may overlap the net conections of
; the pin bounds.
; Each bound is defined as a list of the form ( (x1 x2) (y1 y2) ) with:
;   - (x1, y1): bottom left corner.
;   - (x2, y2): upper right corner.
; Return true if the regions are overlapping, or false otherwise.
(define check-overlapping-of-pin-connections
  (lambda (pins-bounds pin-direction attrib-bounds spacing)
    (let* ( (pins-min-x (get-point-of-bound "min-x" pins-bounds))
	    (pins-max-x (get-point-of-bound "max-x" pins-bounds))
	    (pins-min-y (get-point-of-bound "min-y" pins-bounds))
	    (pins-max-y (get-point-of-bound "max-y" pins-bounds))
	    (attrib-min-x (get-point-of-bound "min-x" attrib-bounds))
	    (attrib-max-x (get-point-of-bound "max-x" attrib-bounds))
	    (attrib-min-y (get-point-of-bound "min-y" attrib-bounds))
	    (attrib-max-y (get-point-of-bound "max-y" attrib-bounds)) )
      (if (string=? pin-direction "^")  
	  (and (>= pins-min-y attrib-max-y)
	       (check-collision-of-bounds 
		; Calcule the collision as if the attribute has the same
		; vertical coordinates as the pins (including spacing).
		(cons (cons attrib-min-x attrib-max-x)
		      (cons pins-min-y pins-max-y))
		(cons (cons (- pins-min-x spacing) (+ pins-max-x spacing))
		      (cons pins-min-y pins-max-y)) ) )
	  (if (string=? pin-direction "v")
	      (and (<= pins-max-y attrib-min-y)
		   (check-collision-of-bounds 
	            ; Calcule the collision as if the attribute has the same
	            ; vertical coordinates as the pins (including spacing).
		    (cons (cons attrib-min-x attrib-max-x)
			  (cons pins-min-y pins-max-y))
		    (cons (cons (- pins-min-x spacing) (+ pins-max-x spacing))
			  (cons pins-min-y pins-max-y)) ) )
	      (if (string=? pin-direction "<")
		  (and (<= pins-max-x attrib-min-x)
		       (check-collision-of-bounds 
	                ; Calcule the collision as if the attribute has 
			; the same  horizontal coordinates as the pins 
			; (including spacing).
			(cons (cons pins-min-x pins-max-x)
			      (cons attrib-min-y attrib-max-y))
			(cons (cons pins-min-x 
				    pins-max-x)
			      (cons (- pins-min-y spacing)
				    (+ pins-max-y spacing)) ) ) )
		  (if (string=? pin-direction ">")
		      (and (>= pins-min-x attrib-max-x)
			   (check-collision-of-bounds 
	                    ; Calcule the collision as if the attribute has 
			    ; the same  horizontal coordinates as the pins 
			    ; (including spacing).
			    (cons (cons pins-min-x pins-max-x)
				  (cons attrib-min-y attrib-max-y))
			    (cons (cons pins-min-x 
					pins-max-x)
				  (cons (- pins-min-y spacing)
					(+ pins-max-y spacing)) ) ) )
		      (error (string-append 
			      "check-overlapping-of-pin-connections : Unknown pin-direction: "
			      pin-direction)))))))))


; Given a coordinate, snap it to the nearest point in the grid.
(define snap-coord-to-grid
  (lambda (coord)
    (if (> autoplace-attributes-grid 0)
	(if (<= coord 0) 
	    (inexact->exact (* (floor (/ coord
					 autoplace-attributes-grid))
			       autoplace-attributes-grid))
	    (inexact->exact (* (ceiling (/ coord
					   autoplace-attributes-grid))
			       autoplace-attributes-grid)))
	coord)
))

; Given the new desired bounds of an object's attribute, 
; calcule the new bounds so the new position don't overlap with pins
; or pin attributes.
; Returns the new bounds of the attribute.
(define adjust-pos-to-avoid-collision 
  (lambda (new-attrib-bounds object move-direction spacing)
    (let* ( (pin-directions-list (list ">" "<" "v" "^"))
	    (pin-directions-list-index 0)
	    (new-attrib-bounds-adjusted new-attrib-bounds)
	    (pass 1)
	    )
      ; For each pin-direction in the pin-directions-list, make a 2 pass loop.
      ; The first one checks the attribute bounds with the pin bounds (without
      ; attributes like pinname, pinnumber,...), and taking care of not overlap
      ; the pin connections side, so the nets connecting to the pins don't
      ; overlap the attribute.
      ; The second one checks the attribute bounds with the pin bounds,
      ; this time including all the pin attributes.
      (while (<= pin-directions-list-index (- (length pin-directions-list) 1))
	     (let* ( (pin-direction (list-ref pin-directions-list 
					      pin-directions-list-index))
		     (pins-bounds 
		      (if (eq? pass 1)
			  (get-bounds-of-pins-in-side object pin-direction)
			  (get-bounds-of-pins-with-attribs-in-side 
			   object pin-direction)))
		     (x_offset 0)
		     (y_offset 0)
		     )
	       (begin
		 (if (not (null? pins-bounds))
		     (if (if (eq? pass 1)
 			     (check-overlapping-of-pin-connections
 			      pins-bounds
 			      pin-direction
 			      new-attrib-bounds-adjusted
 			      spacing)
			     (check-collision-of-bounds 
			      new-attrib-bounds-adjusted
			      pins-bounds)
			     )
			 (begin
			   ; Calcule the offset for vertical pins.
			   (if (or (string=? pin-direction "^") 
				   (string=? pin-direction "v") )
			       (begin
				 (if (string-index move-direction #\<)
				     (set! x_offset
					   (- (- (get-point-of-bound 
						  "min-x" 
						  pins-bounds)
2						 (get-point-of-bound 
						  "max-x" 
						  new-attrib-bounds-adjusted)
						 )
					      spacing )) ;; add spacing
				     (if (string-index move-direction #\>)
					 (set! x_offset 
					       (+ (- (get-point-of-bound 
						      "min-x" 
						      new-attrib-bounds-adjusted)
						     (get-point-of-bound 
						      "max-x" 
						      pins-bounds)) 
						  spacing))))
				 ; Snap the offset to the grid.
				 (set! x_offset (snap-coord-to-grid x_offset))
				 ; Loop again from the beginning
				 (set! pin-directions-list-index -1)
				 (set! pass 2)

				 ; Set the new attrib bounds.
				 (set! new-attrib-bounds-adjusted 
				       (cons (cons (+ (get-point-of-bound 
						       "min-x"
						       new-attrib-bounds-adjusted)
						      x_offset)
						   (+ (get-point-of-bound 
						       "max-x"
						       new-attrib-bounds-adjusted)
						      x_offset))
					     (cons (get-point-of-bound 
						    "min-y"
						    new-attrib-bounds-adjusted)
						   (get-point-of-bound 
						    "max-y"
						    new-attrib-bounds-adjusted))))
				 )
			       ; Calcule the offset for horizontal pins.
			       (if (or (string=? pin-direction "<") 
				       (string=? pin-direction ">") )
				   (begin
				     (if (string-index move-direction #\^)
					 (set! y_offset 
					       (+ y_offset
						  (+ (- (get-point-of-bound 
							 "max-y" 
							 pins-bounds)
							(get-point-of-bound 
							 "min-y" 
							 new-attrib-bounds-adjusted)
							)
						     spacing)))
					 (if (string-index move-direction #\v)
					     (set! y_offset 
						   (+ y_offset 
						      (+ (- (get-point-of-bound
							     "min-y" 
							     new-attrib-bounds-adjusted)
							    (get-point-of-bound
							     "max-y" 
							     pins-bounds))
							 spacing)))))
				     ; Snap the offset to the grid.
				     (set! y_offset 
					   (snap-coord-to-grid y_offset))
				     ; Loop again from the beginning
				     (set! pin-directions-list-index -1)
				     (set! pass 2)
				     
				     ; Set the new attrib bounds.
				     (set! new-attrib-bounds-adjusted
					   (cons 
					    (cons (get-point-of-bound 
						   "min-x" 
						   new-attrib-bounds-adjusted)
						  (get-point-of-bound 
						   "max-x" 
						   new-attrib-bounds-adjusted))
					    (cons (+ (get-point-of-bound 
						      "min-y" 
						      new-attrib-bounds-adjusted)
						     y_offset)
						  (+ (get-point-of-bound 
						      "max-y"
						      new-attrib-bounds-adjusted)
						     y_offset)
						  )))

				     )
				   (error "adjust-pos-to-avoid-collision: Wrong pin-direction format")
				   ))))
		     )

		 ; Update the index and pass number for the next loop.
		 (if (not (eq? pass 1))
		     (begin
		       (set! pin-directions-list-index 
			     (+ pin-directions-list-index 1))
		       (set! pass 1))
		     (set! pass (+ pass 1)))
		 )))
	     
      new-attrib-bounds-adjusted
)))
       

; This function gets the reference point of an object.
; The position string is the reference to return. It has the format:
;   "horizontal vertical", where: 
;     - "horizontal" is one of the following: "Left", "Middle", "Right".
;     - "vertical" is one of the following: "Lower", "Middle", "Upper".
;   Example: "Lower Right".
(define (get-reference object position-string)
  (if (not (string-index position-string #\ )) 
      (error "get-reference : Wrong reference format"))
  (let* ( (object-type (get-object-type object))
	  ; Get the object bounds:
          ;  - If it's a pin: including everything.
          ;  - otherwise: without attributes neither pins.
	  (bounds (if (string=? object-type OBJ_PIN)
		      (get-object-bounds object (list "all") (list))
		      (get-object-bounds object (list "all") (list OBJ_PIN))))
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
	      ; It matches, so change the text parameters
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
		      (attrib-move-dir (list-ref default-def def-move-pos))
		      (attrib-spacing (list-ref default-def def-spacing-pos))
 		      (new-attrib-bounds (calcule-new-attrib-bounds attribute
 								    new-alignment
 								    new-angle
 								    new-x
 								    new-y))
		      (new-attrib-bounds-adjusted 
		       (adjust-pos-to-avoid-collision new-attrib-bounds 
						      object 
						      attrib-move-dir 
						      attrib-spacing))
		      (x_offset 
		       (if (null? new-attrib-bounds-adjusted)
			   0
			   (- (get-point-of-bound "min-x" 
						  new-attrib-bounds-adjusted)
			      (get-point-of-bound "min-x" new-attrib-bounds))))
		      (y_offset 
		       (if (null? new-attrib-bounds-adjusted)
			   0
			   (- (get-point-of-bound "min-y" 
						  new-attrib-bounds-adjusted)
			      (get-point-of-bound "min-y" new-attrib-bounds))))
		      )
		(set-attribute-text-properties! attribute
						"" ; keep previous color
						-1 ; keep previous size
						new-alignment
						new-angle
						(+ new-x x_offset)
						(+ new-y y_offset))
		)
	      )
	    
	    )
	(set-default-position object attribute direction 
			      (cdr defaults)) ; process the rest
	))
  ) ; End of definition of set-default-position

; Calcule the attribute bounds in the new position
; Returns a list of the form ( (x1 x2) (y1 y2) ) with:
;  - (x1, y1): bottom left corner.
;  - (x2, y2): upper right corner.
(define calcule-new-attrib-bounds 
  (lambda (attribute alignment angle x y)
    (let* ( (old-bounds (get-attribute-bounds attribute))
	    (length (- (get-point-of-bound "max-x" old-bounds)
		       (get-point-of-bound "min-x" old-bounds)))
	    (height (- (get-point-of-bound "max-y" old-bounds)
		       (get-point-of-bound "min-y" old-bounds)))
	    (old-angle (get-attribute-angle attribute))			    
	    (x_size (if (or (eq? (abs (- old-angle angle)) 0)
			    (eq? (abs (- old-angle angle)) 180))
			length
			height))
	    (y_size (if (or (eq? (abs (- old-angle angle)) 0)
			    (eq? (abs (- old-angle angle)) 180))
			height
			length))
	    (space-pos (string-index alignment #\space))
	    (vertical-pos (substring alignment 0 space-pos))
	    (horiz-pos (substring alignment (+ space-pos 1)))
	    ; Calcule the x of the left bottom point of the text.
	    (lb_x (if (string=? horiz-pos "Left")
		      x
		      (if (string=? horiz-pos "Middle")
			  (- x (inexact->exact (/ x_size 2)))
			  (if (string=? horiz-pos "Right")
			      (- x x_size)
			      (error (string-append 
				      "calcule-new-attrib-bounds : Unknown reference (horizontal): " 
					 horiz-pos))))))
	    ; Calcule the y of the left bottom point of the text.
	    (lb_y (if (string=? vertical-pos "Lower")
		      y
		      (if (string=? vertical-pos "Middle")
			  (- y (inexact->exact (/ y_size 2)))
			  (if (string=? vertical-pos "Upper")
			      (- y y_size)
			      (error (string-append 
				      "calcule-new-attrib-bounds : Unknown reference (vertical): " 
					 vertical-pos))))))
	    )
      (cons (cons lb_x (+ lb_x x_size)) (cons lb_y (+ lb_y y_size))))))

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

