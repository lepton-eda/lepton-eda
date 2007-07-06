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
   ;         OBJ_COMPLEX, OBJ_TEXT, OBJ_PIN, OBJ_ARC, OBJ_ROUTE, OBJ_THRU_HOLE,
   ;         OBJ_PLACEHOLDER
   ;         converted into strings (for example using the function char2str).
   ;
   ;    - the second line defines where and how the attribute will be placed.
   ;      The attribute is going to be placed at a position of the
   ;      component, given by the "reference" item.
   ;      It has the following items:
   ;        - reference: the component's reference point where to place 
   ;          the attribute.
   ;          It is a string with the format "horizontal vertical", where:
   ;            - "horizontal" is one of: "Left",  "Middle", "Right".
   ;            - "vertical"   is one of: "Lower", "Middle", "Upper".
   ;          Example: "Lower Right".
   ;        - x_offset, y_offset: the position offset to the reference point.
   ;          The attribute position is offset by these amounts from the
   ;          reference point. They can be positive or negative integers.
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
   ;Attrib_name Direct. Attribute_match  <--- first line of each entry.
   ;X_offset Y_offset Reference       Alignment    Angle Mov_dir  Spacing (>0)
   ;
  (list 
   (list 
    "pinlabel"  ">"  (list "OBJ_TYPE" (char2str OBJ_PIN))
      50      0       "Lower Right"   "Lower Left"     0      ""   0)
   (list
    "pinlabel"  "<"  (list "OBJ_TYPE" (char2str OBJ_PIN))
     -50      0       "Lower Left"    "Lower Right"    0      ""   0)
   (list
    "pinlabel"  "^"  (list "OBJ_TYPE" (char2str OBJ_PIN))
       0     50       "Upper Middle"  "Lower Left"    90      ""   0)
   (list
    "pinlabel"  "v"  (list "OBJ_TYPE" (char2str OBJ_PIN))
       0    -50       "Lower Middle"  "Lower Right"   90      ""   0)
   (list
    "pinnumber" ">"  (list "OBJ_TYPE" (char2str OBJ_PIN))
    -100     50       "Lower Right"   "Lower Right"    0      ""   0)
   (list
    "pinnumber" "<"  (list "OBJ_TYPE" (char2str OBJ_PIN))
     100     50       "Lower Left"    "Lower Left"     0      ""   0)
   (list
    "pinnumber" "^"  (list "OBJ_TYPE" (char2str OBJ_PIN))
     -50   -100       "Upper Middle"  "Lower Right"   90      ""   0)
   (list 
    "pinnumber" "v"  (list "OBJ_TYPE" (char2str OBJ_PIN))
     -50    100       "Lower Middle"  "Lower Left"    90      ""   0)
   
     ; Component attributes
     ;   One direction
   (list 
    "refdes"    "<"  (list "OBJ_TYPE" (char2str OBJ_COMPLEX))
       0     50       "Upper Middle"  "Lower Middle"   0      ""   0)
   (list 
    "value"     "<"  (list "OBJ_TYPE" (char2str OBJ_COMPLEX))
       0    -50       "Lower Middle"  "Upper Middle"   0      ""   0)
   (list 
    "device"    "<"  (list "OBJ_TYPE" (char2str OBJ_COMPLEX))
       0    -50       "Lower Middle"  "Upper Middle"   0      ""   0)
   (list 
    "refdes"    ">" (list "OBJ_TYPE" (char2str OBJ_COMPLEX))
       0     50       "Upper Middle"  "Lower Middle"   0      ""   0)
   (list 
    "value"     ">" (list "OBJ_TYPE" (char2str OBJ_COMPLEX))
       0    -50       "Lower Middle"  "Upper Middle"   0      ""   0)
   (list 
    "device"    ">" (list "OBJ_TYPE" (char2str OBJ_COMPLEX))
       0    -50       "Lower Middle"  "Upper Middle"   0      ""   0)
   (list 
    "refdes"    "^" (list "OBJ_TYPE" (char2str OBJ_COMPLEX))
       0    100       "Middle Left"   "Lower Right"    0      ""   0)
   (list 
    "value"     "^" (list "OBJ_TYPE" (char2str OBJ_COMPLEX))
       0   -100       "Middle Left"   "Lower Right"    0      ""   0)
   (list 
    "device"    "^" (list "OBJ_TYPE" (char2str OBJ_COMPLEX))
       0   -100       "Middle Left"   "Lower Right"    0      ""   0)
   (list 
    "refdes"    "v" (list "OBJ_TYPE" (char2str OBJ_COMPLEX))
       0    100       "Middle Left"   "Lower Right"    0      ""   0)
   (list 
    "value"     "v" (list "OBJ_TYPE" (char2str OBJ_COMPLEX))
       0   -100       "Middle Left"   "Lower Right"    0      ""   0)
   (list 
    "device"    "v" (list "OBJ_TYPE" (char2str OBJ_COMPLEX))
       0   -100       "Middle Left"   "Lower Right"    0      ""   0)
	;   Two directions
   (list 
    "refdes"   "<>" (list "OBJ_TYPE" (char2str OBJ_COMPLEX))
       0     50       "Upper Middle"  "Lower Middle"   0      ""   0)
   (list 
    "value"    "<>" (list "OBJ_TYPE" (char2str OBJ_COMPLEX))
       0    -50       "Lower Middle"  "Upper Middle"   0      ""   0)
   (list 
    "device"   "<>" (list "OBJ_TYPE" (char2str OBJ_COMPLEX))
       0    -50       "Lower Middle"  "Upper Middle"   0      ""   0)
   (list 
    "refdes"   "^v" (list "OBJ_TYPE" (char2str OBJ_COMPLEX))
      50    100       "Middle Right"  "Lower Left"     0      ""   0)
   (list 
    "value"    "^v" (list "OBJ_TYPE" (char2str OBJ_COMPLEX))
      50   -100       "Middle Right"  "Lower Left"     0      ""   0)
   (list 
    "device"   "^v" (list "OBJ_TYPE" (char2str OBJ_COMPLEX))
      50   -100       "Middle Right"  "Lower Left"     0      ""   0)
	;   Three directions
   (list 
    "refdes"  "<^v" (list "OBJ_TYPE" (char2str OBJ_COMPLEX))
     -50    100       "Middle Left"   "Lower Right"    0      ""   0)
   (list 
    "value"   "<^v" (list "OBJ_TYPE" (char2str OBJ_COMPLEX))
     -50   -100       "Middle Left"   "Lower Right"    0      ""   0)
   (list 
    "device"  "<^v" (list "OBJ_TYPE" (char2str OBJ_COMPLEX))
     -50   -100       "Middle Left"   "Lower Right"    0      ""   0)
   (list 
    "refdes"  ">^v" (list "OBJ_TYPE" (char2str OBJ_COMPLEX))
      50    100       "Middle Right"  "Lower Left"     0      ""   0)
   (list 
    "value"   ">^v" (list "OBJ_TYPE" (char2str OBJ_COMPLEX))
      50   -100       "Middle Right"  "Lower Left"     0      ""   0)
   (list 
    "device"  ">^v" (list "OBJ_TYPE" (char2str OBJ_COMPLEX))
      50   -100       "Middle Right"  "Lower Left"     0      ""   0)
   (list 
    "refdes"  "<>v" (list "OBJ_TYPE" (char2str OBJ_COMPLEX))
     -50    -50       "Lower Middle"  "Upper Right"    0      ""   0)
   (list 
    "value"   "<>v" (list "OBJ_TYPE" (char2str OBJ_COMPLEX))
      50    -50       "Lower Middle"  "Upper Left"     0      ""   0)
   (list 
    "device"  "<>v" (list "OBJ_TYPE" (char2str OBJ_COMPLEX))
      50    -50       "Lower Middle"  "Upper Left"     0      ""   0)
   (list 
    "refdes"  "<>^" (list "OBJ_TYPE" (char2str OBJ_COMPLEX))
     -50     50       "Upper Middle"  "Lower Right"    0      ""   0)
   (list 
    "value"   "<>^" (list "OBJ_TYPE" (char2str OBJ_COMPLEX))
      50     50       "Upper Middle"  "Lower Left"     0      ""   0)
   (list 
    "device"  "<>^" (list "OBJ_TYPE" (char2str OBJ_COMPLEX))
      50     50       "Upper Middle"  "Lower Left"     0      ""   0)
	;   Four directions
   (list 
    "refdes" "<>^v"(list "OBJ_TYPE" (char2str OBJ_COMPLEX))
      50     50      "Upper Left"    "Lower Left"     0      "^<" 50)
   (list 
    "value"  "<>^v"(list "OBJ_TYPE" (char2str OBJ_COMPLEX))
      50    -50      "Lower Right"   "Upper Right"    0      "v>" 50)
   (list 
    "device" "<>^v"(list "OBJ_TYPE" (char2str OBJ_COMPLEX))
      50    -50      "Lower Right"   "Upper Right"    0      "v>" 50)
   
     ; Net attributes
     ;   Two directions
   (list 
    "netname" "<>" (list "OBJ_TYPE" (char2str OBJ_NET))
       0     50       "Upper Middle"  "Lower Middle"   0      ""   0)
   (list 
    "netname" "^v" (list "OBJ_TYPE" (char2str OBJ_NET))
     -50      0       "Middle Middle" "Lower Middle"  90      ""   0)
     ; Bus attributes
     ;   Two directions
   (list 
    "netname" "<>" (list "OBJ_TYPE" (char2str OBJ_BUS))
       0     50       "Upper Middle"  "Lower Middle"   0      ""   0)
   (list 
    "netname" "^v" (list "OBJ_TYPE" (char2str OBJ_BUS))
     -50      0       "Middle Middle" "Lower Middle"  90      ""   0)
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
(define def-move-pos 	      8)
(define def-spacing-pos       9)

