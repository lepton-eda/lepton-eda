;;; Lepton EDA Schematic Capture
;;; Scheme API
;;; Copyright (C) 2006-2007 Carlos Nieves Onega
;;; Copyright (C) 2010-2011 Peter Brett
;;; Copyright (C) 2013-2014 Patrick Bernaud <patrickb@chez.com>
;;; Copyright (C) 2006-2014 gEDA Contributors
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

(define-module (lepton autoplace)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)

  #:use-module (geda deprecated)
  #:use-module (lepton attrib)
  #:use-module (lepton object)

  #:export (autoplace-pin-attributes
            autoplace-object-attributes
            place-netname-attribute-handler))

; Convert a character into a string
(define char2str
  (lambda (char)
    (list->string (list char))))

; Attribute autoplacement grid
(define autoplace-attributes-grid 50)


(define default-position-of-text-attributes
   ; Specifies the default position of text attributes.
   ; Each list entry is a list specifying the position of a text attribute.
   ; Dividing each list entry in two lines:
   ;   - the first line is the attribute matching. It specifies the properties
   ;     of the object and the attribute that should match in order to use
   ;     that entry (all the specified properties should match).
   ;     It has the following items:
   ;       - attrib_name: the attribute's name for this entry.
   ;       - direction: direction of the pin(s) (the arrow points to the inside
   ;         of the symbol. The direction is specified by one character of
   ;         v, ^, < or >. This item is a string with one or several direction
   ;         characters. Including several directions here means that the
   ;         object should have pins in all those directions if using this
   ;         entry.
   ;
   ;       - list of object attribute matching properties. It's a list with
   ;         attribute_name, attribute_value pairs. A regular expression can be
   ;         used in the attribute_values field.
   ;         There is a special attribute name: "OBJ_TYPE" if you want to check
   ;         the object type. However, the attribute_value field should be
   ;         one of the already defined variables:
   ;         OBJ_LINE, OBJ_BOX, OBJ_PICTURE, OBJ_CIRCLE, OBJ_NET, OBJ_BUS,
   ;         OBJ_COMPLEX, OBJ_TEXT, OBJ_PIN, OBJ_ARC, OBJ_PLACEHOLDER
   ;         converted into strings (for example using the function char2str).
   ;
   ;    - the second line defines where and how the attribute will be placed.
   ;      The attribute is going to be placed at a position of the
   ;      component, given by the "reference" item.
   ;      It has the following items:
   ;        - x_offset, y_offset: the position offset to the reference point.
   ;          The attribute position is offset by these amounts from the
   ;          reference point. They can be positive or negative integers.
   ;        - reference: the component's reference point where to place
   ;          the attribute. The reference point is calculated using
   ;          component's bounds, so pin's line width, for example, matter.
   ;          Example: Pin from (100,50) to (400,50), pin's line width is 10.
   ;            Pin bounds:
   ;              top: 55
   ;              bottom: 45
   ;              left: 95
   ;              right: 405
   ;            "Lower left": (95,45)
   ;          It is a string with the format "horizontal vertical", where:
   ;            - "horizontal" is one of: "Left",  "Middle", "Right".
   ;            - "vertical"   is one of: "Lower", "Middle", "Upper".
   ;          Example: "Lower Right".
   ;        - alignment: the attribute text's alignment.
   ;        - angle: the attribute's angle.
   ;
   ;       As a special case, the attributes can be automaticaly moved if
   ;       they overlap with the pins or the pin connection direction, in
   ;       order to avoid cluttering the schematic. Note that pinnames are
   ;       also included when calculing the new position.
   ;       By pin connection direction I mean the space in front of a pin
   ;       where a net connecting to that pin is supposed to be drawn
   ;       afterwards.
   ;
   ;       For this purpose, two more items are added:
   ;         - mov_dir: specify the movement directions, or where can the
   ;           attribute be moved if overlapping.
   ;           It is an empty string if no movement is allowed, or a string
   ;           containing one or several horizontal/vertical directions.
   ;           Each direction is specified with one character of v,^,< or >.
   ;
   ;           When overlapping with vertical pins, the attribute will be
   ;           moved horizontally (< or > characters).
   ;           When overlapping with horizontal pins, the attribute will be
   ;           moved vertically (v or ^ characters).
   ;
   ;           Example: "<^" means that the attribute will be moved to the
   ;           left if overlapping with vertical pins, or to the top if
   ;           overlapping horizontal pins.
   ;
   ;         - spacing: minimum spacing between the attributes and the pins.
   ;           It sould be a positive integer number.
   ;
   ;         - color: the color to assign to the attribute. A -1 will leave
   ;           the color unchanged.
   ;
   ;Attrib_name Direct. Attribute_match  <--- first line of each entry.
   ;X_offset Y_offset Reference       Alignment    Angle Mov_dir  Spacing (>0) Color
   ;
  (list
   (list
    "pinlabel"  ">"  (list "OBJ_TYPE" (char2str OBJ_PIN))
      50      0       "Lower Right"   "Lower Left"     0      ""   0   9)
   (list
    "pinlabel"  "<"  (list "OBJ_TYPE" (char2str OBJ_PIN))
     -50      0       "Lower Left"    "Lower Right"    0      ""   0   9)
   (list
    "pinlabel"  "^"  (list "OBJ_TYPE" (char2str OBJ_PIN))
       0     50       "Upper Middle"  "Lower Left"    90      ""   0   9)
   (list
    "pinlabel"  "v"  (list "OBJ_TYPE" (char2str OBJ_PIN))
       0    -50       "Lower Middle"  "Lower Right"   90      ""   0   9)
   (list
    "pinnumber" ">"  (list "OBJ_TYPE" (char2str OBJ_PIN))
    -100     50       "Lower Right"   "Lower Right"    0      ""   0  -1)
   (list
    "pinnumber" "<"  (list "OBJ_TYPE" (char2str OBJ_PIN))
     100     50       "Lower Left"    "Lower Left"     0      ""   0  -1)
   (list
    "pinnumber" "^"  (list "OBJ_TYPE" (char2str OBJ_PIN))
     -50   -100       "Upper Middle"  "Lower Right"   90      ""   0  -1)
   (list
    "pinnumber" "v"  (list "OBJ_TYPE" (char2str OBJ_PIN))
     -50    100       "Lower Middle"  "Lower Left"    90      ""   0  -1)

     ; Component attributes
     ;   One direction
   (list
    "refdes"    "<"  (list "OBJ_TYPE" (char2str OBJ_COMPLEX))
       0     50       "Upper Middle"  "Lower Middle"   0      ""   0  -1)
   (list
    "value"     "<"  (list "OBJ_TYPE" (char2str OBJ_COMPLEX))
       0    -50       "Lower Middle"  "Upper Middle"   0      ""   0  -1)
   (list
    "device"    "<"  (list "OBJ_TYPE" (char2str OBJ_COMPLEX))
       0    -50       "Lower Middle"  "Upper Middle"   0      ""   0  -1)
   (list
    "refdes"    ">" (list "OBJ_TYPE" (char2str OBJ_COMPLEX))
       0     50       "Upper Middle"  "Lower Middle"   0      ""   0  -1)
   (list
    "value"     ">" (list "OBJ_TYPE" (char2str OBJ_COMPLEX))
       0    -50       "Lower Middle"  "Upper Middle"   0      ""   0  -1)
   (list
    "device"    ">" (list "OBJ_TYPE" (char2str OBJ_COMPLEX))
       0    -50       "Lower Middle"  "Upper Middle"   0      ""   0  -1)
   (list
    "refdes"    "^" (list "OBJ_TYPE" (char2str OBJ_COMPLEX))
       0    100       "Middle Left"   "Lower Right"    0      ""   0  -1)
   (list
    "value"     "^" (list "OBJ_TYPE" (char2str OBJ_COMPLEX))
       0   -100       "Middle Left"   "Lower Right"    0      ""   0  -1)
   (list
    "device"    "^" (list "OBJ_TYPE" (char2str OBJ_COMPLEX))
       0   -100       "Middle Left"   "Lower Right"    0      ""   0  -1)
   (list
    "refdes"    "v" (list "OBJ_TYPE" (char2str OBJ_COMPLEX))
       0    100       "Middle Left"   "Lower Right"    0      ""   0  -1)
   (list
    "value"     "v" (list "OBJ_TYPE" (char2str OBJ_COMPLEX))
       0   -100       "Middle Left"   "Lower Right"    0      ""   0  -1)
   (list
    "device"    "v" (list "OBJ_TYPE" (char2str OBJ_COMPLEX))
       0   -100       "Middle Left"   "Lower Right"    0      ""   0  -1)
        ;   Two directions
   (list
    "refdes"   "<>" (list "OBJ_TYPE" (char2str OBJ_COMPLEX))
       0     50       "Upper Middle"  "Lower Middle"   0      ""   0  -1)
   (list
    "value"    "<>" (list "OBJ_TYPE" (char2str OBJ_COMPLEX))
       0    -50       "Lower Middle"  "Upper Middle"   0      ""   0  -1)
   (list
    "device"   "<>" (list "OBJ_TYPE" (char2str OBJ_COMPLEX))
       0    -50       "Lower Middle"  "Upper Middle"   0      ""   0  -1)
   (list
    "refdes"   "^v" (list "OBJ_TYPE" (char2str OBJ_COMPLEX))
      50    100       "Middle Right"  "Lower Left"     0      ""   0  -1)
   (list
    "value"    "^v" (list "OBJ_TYPE" (char2str OBJ_COMPLEX))
      50   -100       "Middle Right"  "Lower Left"     0      ""   0  -1)
   (list
    "device"   "^v" (list "OBJ_TYPE" (char2str OBJ_COMPLEX))
      50   -100       "Middle Right"  "Lower Left"     0      ""   0  -1)
        ;   Three directions
   (list
    "refdes"  "<^v" (list "OBJ_TYPE" (char2str OBJ_COMPLEX))
     -50    100       "Middle Left"   "Lower Right"    0      ""   0  -1)
   (list
    "value"   "<^v" (list "OBJ_TYPE" (char2str OBJ_COMPLEX))
     -50   -100       "Middle Left"   "Lower Right"    0      ""   0  -1)
   (list
    "device"  "<^v" (list "OBJ_TYPE" (char2str OBJ_COMPLEX))
     -50   -100       "Middle Left"   "Lower Right"    0      ""   0  -1)
   (list
    "refdes"  ">^v" (list "OBJ_TYPE" (char2str OBJ_COMPLEX))
      50    100       "Middle Right"  "Lower Left"     0      ""   0  -1)
   (list
    "value"   ">^v" (list "OBJ_TYPE" (char2str OBJ_COMPLEX))
      50   -100       "Middle Right"  "Lower Left"     0      ""   0  -1)
   (list
    "device"  ">^v" (list "OBJ_TYPE" (char2str OBJ_COMPLEX))
      50   -100       "Middle Right"  "Lower Left"     0      ""   0  -1)
   (list
    "refdes"  "<>v" (list "OBJ_TYPE" (char2str OBJ_COMPLEX))
     -50    -50       "Lower Middle"  "Upper Right"    0      ""   0  -1)
   (list
    "value"   "<>v" (list "OBJ_TYPE" (char2str OBJ_COMPLEX))
      50    -50       "Lower Middle"  "Upper Left"     0      ""   0  -1)
   (list
    "device"  "<>v" (list "OBJ_TYPE" (char2str OBJ_COMPLEX))
      50    -50       "Lower Middle"  "Upper Left"     0      ""   0  -1)
   (list
    "refdes"  "<>^" (list "OBJ_TYPE" (char2str OBJ_COMPLEX))
     -50     50       "Upper Middle"  "Lower Right"    0      ""   0  -1)
   (list
    "value"   "<>^" (list "OBJ_TYPE" (char2str OBJ_COMPLEX))
      50     50       "Upper Middle"  "Lower Left"     0      ""   0  -1)
   (list
    "device"  "<>^" (list "OBJ_TYPE" (char2str OBJ_COMPLEX))
      50     50       "Upper Middle"  "Lower Left"     0      ""   0  -1)
        ;   Four directions
   (list
    "refdes" "<>^v"(list "OBJ_TYPE" (char2str OBJ_COMPLEX))
      50     50      "Upper Left"    "Lower Left"     0      "^<" 50  -1)
   (list
    "value"  "<>^v"(list "OBJ_TYPE" (char2str OBJ_COMPLEX))
      50    -50      "Lower Right"   "Upper Right"    0      "v>" 50  -1)
   (list
    "device" "<>^v"(list "OBJ_TYPE" (char2str OBJ_COMPLEX))
      50    -50      "Lower Right"   "Upper Right"    0      "v>" 50  -1)

     ; Net attributes
     ;   Two directions
   (list
    "netname" "<>" (list "OBJ_TYPE" (char2str OBJ_NET))
       0     50       "Upper Middle"  "Lower Middle"   0      ""   0  -1)
   (list
    "netname" "^v" (list "OBJ_TYPE" (char2str OBJ_NET))
     -50      0       "Middle Middle" "Lower Middle"  90      ""   0  -1)
     ; Bus attributes
     ;   Two directions
   (list
    "netname" "<>" (list "OBJ_TYPE" (char2str OBJ_BUS))
       0     50       "Upper Middle"  "Lower Middle"   0      ""   0  -1)
   (list
    "netname" "^v" (list "OBJ_TYPE" (char2str OBJ_BUS))
     -50      0       "Middle Middle" "Lower Middle"  90      ""   0  -1)
   ))

; Position of parameters inside default-position-of-text-attributes
(define def-attrib-name-pos   0)
(define def-direction-pos     1)
(define def-attrib-match      2)
(define def-x-offset-pos      3)
(define def-y-offset-pos      4)
(define def-reference-pos     5)
(define def-alignment-pos     6)
(define def-angle-pos         7)
(define def-move-pos          8)
(define def-spacing-pos       9)
(define def-color-pos        10)


;; --------------------------------------------------------------------------
;;
;; Code to place new text attributes automatically
;; written by Carlos Nieves Onega starts here.
;;

; Copyright (C) 2006 Carlos Nieves Onega

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


;; get-pin-ends pin
;;
;; Return the coordinates of the endpoints of a pin, in the format:
;;
;;   ((x1 . y1) x2 . y2)
(define (get-pin-ends pin)
  (let ((params (line-info pin)))
    (cons (list-ref params 0) (list-ref params 1))))


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


;; get-object-bounds object exclude-attribs exclude-types
;;
;; Return the bounds of an object, excluding attributes with
;; particular names or certain object types.
;;
;; The exclude-attribs should be a list of attribute names to be
;; omitted, as strings. If the special string "all" appears in the
;; list, all attributes are excluded.
;;
;; The exclude-types should be a list of single-character strings
;; containing object type characters (as returned by the deprecated
;; get-object-type function).
;;
;; Note that attributes attached to pins (but not attached to anything
;; else) are included in the bounds.
;;
;; The bounds are returned in the form:
;;
;;   ((left . right) . (bottom . top))
;;
;; N.b. that this is a different form to that returned by
;; object-bounds, so you can't use fold-bounds directly with bounds
;; returned by this function.
(define (get-object-bounds object exclude-attribs exclude-types)
  (define no-attribs (member "all" exclude-attribs))

  (define (exclude? object)
    (or
     ;; Is it an excluded type?
     (member (string (get-object-type object)) exclude-types)
     ;; Is it invisible text?
     (and (text? object) (not (text-visible? object)))
     ;; Is it an excluded attribute?
     (and (attribute? object)
          (or no-attribs
              (member (attrib-name object) exclude-attribs)))))

  (define (excluding-bounds object)
    (cond
     ;; If it's excluded, no bounds!
     ((exclude? object) #f)
     ;; If it's a component, recurse
     ((component? object)
      (fold fold-bounds #f
            (map excluding-bounds (component-contents object))))
     ;; If it's a pin, include its attributes
     ((pin? object)
      (fold fold-bounds #f
            (cons (object-bounds object)
                  (map excluding-bounds (object-attribs object)))))
     ;; Otherwise, just return the object bounds
     (else (object-bounds object))))

  (let ((bounds (excluding-bounds object)))
    (if bounds
        ;; Re-arrange the bounds into the format expected
        (cons (cons (caar bounds) (cadr bounds))
              (cons (cddr bounds) (cdar bounds)))
        ;; Stupid default
        '((1000000 . 0) . (1000000 . 0)))))


; This function returns the net direction of the net object parameter.
; It returns a string :
;   "^v": vertical net
;   "<>": horizontal net
(define get-net-connection-sides
  (lambda (object)
    (let ( (bounds (get-object-bounds object (list "all") (list)))
           )
      (begin
        (if (or (char=? (get-object-type object) OBJ_NET)
                (char=? (get-object-type object) OBJ_BUS))
            (let ( ; Get the net bounds without the attribute
                   (min-x (get-point-of-bound "min-x" bounds))
                   (max-x (get-point-of-bound "max-x" bounds))
                   (min-y (get-point-of-bound "min-y" bounds))
                   (max-y (get-point-of-bound "max-y" bounds))
                   )
              ; Line's width needs to be considered here.
              (if (eq? (- max-x min-x) (get-line-width object))
                  ; If the x bounds are the same, this is a vertical segment.
                  "^v"
                  (if (eq? (- max-y min-y) (get-line-width object))
                      ; If the y bounds are, this is a horizontal segment.
                      "<>"
                      ; X or Y bounds are not the same. We don't know.
                      (begin
                        (display "Warning: get-net-connection-sides: Can't guess net direction.\n")
                        "")
                      )
                  )
              )
            ; This is not a OBJ_NET object. Return an empty list.
            (list)
            )
        )
      )
    )
  )

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

;; get-object-pins object
;;
;; Return the pin objects from a component's contents, in reverse
;; order, or the empty list if object is not a component.
(define (get-object-pins object)
  (if (component? object)
      (reverse! (filter! pin? (component-contents object)))
      '()))


; This function returns the bounds of the pins in the given side of the object
; The side is a one character string: "^", "v", "<" or ">". The arrow
; points the pin's end, which is NOT the active connection end.
(define get-bounds-of-pins-in-side
   (lambda (object desired_side)
     (let* ( (pins (get-object-pins object))
             (pins-beginning (get-bound-of-pins desired_side "B" pins))
             (pins-beginning-sorted (if (eq? (length pins-beginning) 0)
                                        (list)
                                        (stable-sort pins-beginning <)))
             (pins-end (get-bound-of-pins desired_side "E" pins))
             (pins-end-sorted (if (eq? (length pins-end) 0)
                                  (list)
                                  (stable-sort pins-end <)))
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
2                                                (get-point-of-bound
                                                  "max-x"
                                                  new-attrib-bounds-adjusted)
                                                 )
                                              spacing )) ;; add spacing
                                     (if (string-index move-direction #\>)
                                         (set! x_offset
                                               (+ (- (get-point-of-bound
                                                      "max-x"
                                                      pins-bounds)
                                                     (get-point-of-bound
                                                      "min-x"
                                                      new-attrib-bounds-adjusted)
                                                     )
                                                  spacing))))

                                 ; If the offset is zero, there is probably
                                 ; an overlap with pin connections, so add
                                 ; one grid spacing to the offset.
                                 (if (eq? x_offset 0)
                                     (if (string-index move-direction #\<)
                                         (set! x_offset (- 0
                                                           autoplace-attributes-grid))
                                         (set! x_offset
                                               autoplace-attributes-grid))
                                     )

                                 ; Snap the offset to the grid.
                                 (set! x_offset (snap-coord-to-grid x_offset))

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
                                                      (- (- (get-point-of-bound
                                                             "min-y"
                                                             pins-bounds)
                                                            (get-point-of-bound
                                                             "max-y"
                                                             new-attrib-bounds-adjusted))
                                                         spacing)))))

                                     ; If the offset is zero, there is probably
                                     ; an overlap with pin connections, so add
                                     ; one grid spacing to the offset.
                                     (if (eq? y_offset 0)
                                         (if (string-index move-direction #\v)
                                             (set! y_offset (- 0
                                                               autoplace-attributes-grid))
                                             (set! y_offset
                                                   autoplace-attributes-grid))
                                             )

                                     ; Snap the offset to the grid.
                                     (set! y_offset
                                           (snap-coord-to-grid y_offset))

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
          (bounds (if (char=? object-type OBJ_PIN)
                      (get-object-bounds object (list "all") (list))
                      (get-object-bounds object (list "all")
                                         (list (list->string (list OBJ_PIN)))))
                  )
          (horiz-bounds (car bounds))
          (vertical-bounds (cdr bounds))
          (space-pos (string-index position-string #\ ))
          (vertical-string (substring position-string 0 space-pos))
          (horiz-string (substring position-string (+ space-pos 1)))
          (horiz-pos (if (string=? horiz-string "Left")
                         (min (car horiz-bounds) (cdr horiz-bounds))
                         (if (string=? horiz-string "Middle")
                             (ceiling (/ (+ (car horiz-bounds)
                                            (cdr horiz-bounds)) 2))
                             (if (string=? horiz-string "Right")
                                 (max (car horiz-bounds) (cdr horiz-bounds))
                                 (error (string-append
                                         "get-reference : Unknown reference (horizontal): "
                                         horiz-string))))))
          (vertical-pos (if (string=? vertical-string "Lower")
                            (min (car vertical-bounds) (cdr vertical-bounds))
                            (if (string=? vertical-string "Middle")
                                (ceiling (/ (+ (car vertical-bounds)
                                               (cdr vertical-bounds)) 2))
                                (if (string=? vertical-string "Upper")
                                    (max (car vertical-bounds)
                                         (cdr vertical-bounds))
                                    (error (string-append
                                            "get-reference : Unknown reference (vertical): "
                                            vertical-string)))))) )
      (cons horiz-pos vertical-pos)))


; Given a matching pattern and a list, return false if no member of the list
; matches the pattern, or true if any does.
(define (list-string-match matching-pattern attributes_list)
  (if (null? attributes_list)
      #f
      (if (list? attributes_list)
          (if (string-match matching-pattern (car attributes_list))
              #t
              (list-string-match matching-pattern (cdr attributes_list)))
          (if (string-match matching-pattern attributes_list)
              #t
              #f)
          )))

; Given an object and an attribute matching pattern, this function checks
; if the object attributes match the pattern.
; The attributes_list has the form ( [attribute-name attribute-pattern]* )
(define (check-object-attributes object attributes_list)
  (if (null? attributes_list)
      #t
      (if (< (length attributes_list) 2)
          (error (string-append "check-object-attributes: Odd number in attributes list."))
          (let* ( (attribute-name (car attributes_list))
                  (attribute-pattern (car (cdr attributes_list)))
                  (attribute-values (if (string=? attribute-name
                                                  "OBJ_TYPE")
                                        (list
                                         (list->string
                                          (list (get-object-type object))))
                                        (get-attrib-value-by-attrib-name
                                         object attribute-name)))
                   )
            (begin
              (if (null? attribute-values)
                  #f
                  (if (list-string-match attribute-pattern attribute-values)
                      (check-object-attributes object
                                               (cdr (cdr attributes_list)))
                      #f
                      )
                  )
              )
            )
          )
      )
  )


;; set-attribute-text-properties! attrib color size alignment
;;                                rotation x y
;;
;; Sets several parameters of the text object attrib.  x and y are the
;; coordinates of the text anchor.  color is the colormap index of the
;; color with which to draw the text.  size is the font size, and
;; angle is the angle at which to draw the text.
;;
;; All of the former parameters may be set to -1 to leave the current
;; value unchanged.
;;
;; alignment should be one of the following strings:
;;
;;    "Lower Left"
;;    "Middle Left"
;;    "Upper Left"
;;    "Lower Middle"
;;    "Middle Middle"
;;    "Upper Middle"
;;    "Lower Right"
;;    "Middle Right"
;;    "Upper Right"
;;
;; or the empty string "" to leave the alignment unchanged.
(define (set-attribute-text-properties!
                  attrib color size alignment rotation x y)
  (set-text! attrib
             ;; anchor
             (let ((anchor (text-anchor attrib)))
               (cons (if (= x -1) (car anchor) x)
                     (if (= y -1) (cdr anchor) y)))
             ;; align
             (or
              (assoc-ref
               '(("Lower Left" . lower-left)
                 ("Middle Left" . middle-left)
                 ("Upper Left" . upper-left)
                 ("Lower Middle" . lower-center)
                 ("Middle Middle" . middle-center)
                 ("Upper Middle" . upper-center)
                 ("Lower Right" . lower-right)
                 ("Middle Right" . middle-right)
                 ("Upper Right" . upper-right))
               alignment)
              (and (string=? "" alignment) (text-align attrib))
              (error "Invalid text alignment ~A." alignment))
             ;; angle
             (if (= rotation -1) (text-angle attrib) rotation)
             ;; string
             (text-string attrib)
             ;; size
             (if (= size -1) (text-size attrib) size)
             ;; visible
             (text-visible? attrib)
             ;; show
             (text-attribute-mode attrib)))


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
                           direction)
                 (check-object-attributes object
                                          (list-ref default-def ; attrib match
                                                    def-attrib-match)))
            (begin
              ; It matches, so change the text parameters
              (let* ( (ref (get-reference object (list-ref default-def
                                                           def-reference-pos)))
                      (new-alignment (list-ref default-def
                                               def-alignment-pos))
                      (new-angle (list-ref default-def
                                           def-angle-pos))
                      (new-color (list-ref default-def
                                           def-color-pos))
                      (new-x (+ (list-ref default-def
                                          def-x-offset-pos)
                                (car ref)))
                      (new-y (+ (list-ref default-def
                                          def-y-offset-pos)
                                (cdr ref)))
                      (attrib-move-dir (list-ref default-def def-move-pos))
                      (attrib-spacing (abs (list-ref default-def
                                                     def-spacing-pos)))
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
                                                new-color
                                                -1 ; keep previous size
                                                new-alignment
                                                new-angle
                                                (+ new-x x_offset)
                                                (+ new-y y_offset))
                (if (not (= new-color -1))
                    (set-object-color! attribute
                                       new-color))
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
  (and (component? object)
       (let* ((pin-list (get-object-pins object))
              (pin-directions (get-pin-directions pin-list))
              (connection-sides (if (or (char=? (get-object-type object)
                                                OBJ_NET)
                                        (char=? (get-object-type object)
                                                OBJ_BUS))
                                    (get-net-connection-sides object)
                                    (get-connection-sides pin-directions)))
              (attribute-list (get-object-attributes object)) )
         (autoplace-text object connection-sides attribute-list))))


;;
;; Code to place new text attributes automatically
;; written by Carlos Nieves Onega ends here.
;;
;; --------------------------------------------------------------------------


(define (place-netname-attribute attribute)
  ;; Only attached netname attributes
  (and=> (attrib-attachment attribute)
         (lambda (object)
           (set-default-position object attribute
                                 (get-net-connection-sides object)
                                 default-position-of-text-attributes))))

(define (place-netname-attribute-handler objects)
  (for-each (lambda (o)
              (and (attribute? o)
                   (string=? (attrib-name o) "netname")
                   (place-netname-attribute o)))
            objects))
