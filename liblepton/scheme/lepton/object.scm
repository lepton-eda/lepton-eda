;; Lepton EDA library - Scheme API
;; Copyright (C) 2010-2011 Peter Brett <peter@peter-b.co.uk>
;; Copyright (C) 2012-2016 gEDA Contributors
;; Copyright (C) 2017-2021 Lepton EDA Contributors
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
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
;;

(define-module (lepton object)
  ;; Optional arguments
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-4 gnu)
  #:use-module (system foreign)
  #:use-module (rnrs bytevectors)

  ;; Import C procedures
  #:use-module (lepton core component)

  #:use-module (lepton color-map)
  #:use-module (lepton ffi)
  #:use-module (lepton object foreign)
  #:use-module (lepton object type)

  #:export (copy-object
            object-bounds
            object-color
            set-object-color!
            object-component
            object-connections
            object-embedded?
            set-object-embedded!
            object-id
            object-selectable?
            set-object-selectable!

            object-fill
            set-object-fill!

            object-stroke
            object-stroke-cap
            object-stroke-dash
            object-stroke-width
            set-object-stroke!

            arc-info
            arc-center
            arc-radius
            arc-start-angle
            arc-sweep-angle
            arc-end-angle
            make-arc
            set-arc!

            box-info
            box-bottom-right
            box-top-left
            make-box
            set-box!

            component-contents
            component-filename

            circle-center
            circle-info
            circle-radius
            make-circle
            set-circle!

            make-bus

            line-end
            line-info
            line-start
            make-line
            set-line!

            make-net

            make-bus-pin
            make-net-pin

            make-path
            path-info
            path-insert!
            path-length
            path-ref
            path-remove!

            make-picture/vector
            picture-angle
            picture-bottom-right
            picture-filename
            picture-info
            picture-mirror?
            picture-top-left
            set-picture!

            text-info
            text-align
            text-anchor
            text-angle
            text-attribute-mode
            text-size
            text-string
            text-visible?
            make-text
            set-text!
            set-text-string!
            set-text-visibility!)

  #:re-export (arc?
               box?
               bus?
               circle?
               component?
               line?
               net?
               path?
               picture?
               pin?
               bus-pin?
               net-pin?
               text?

               object?
               object-type
               object-type?))


(define (object-id object)
  "Returns an internal id number of the OBJECT."
  (define pointer (geda-object->pointer* object 1))

  (let ((id (lepton_object_get_id pointer)))
    (and (not (= id -1))
         id)))


(define (copy-object object)
  "Returns a copy of OBJECT."
  (define pointer (geda-object->pointer* object 1))

  (pointer->geda-object (lepton_object_copy pointer)))


(define (object-color object)
  "Returns the colormap index of the color used to draw
OBJECT. Note that the color may not be meaningful for some object
types."
  (define pointer (geda-object->pointer* object 1))

  (lepton_object_get_color pointer))


(define (set-object-color! object color)
  "Set the colormap index of the color used to draw OBJECT to COLOR.
Note that the color may not be meaningful for some object types.
Returns OBJECT."
  (define pointer (geda-object->pointer* object 1))

  (lepton_object_set_color pointer color)

  object)


(define (object-connections object)
  "Returns a list of other objects that are directly connected to
OBJECT.  If OBJECT is not included in a page, raises an
'object-state error.  The connections reported are independent of
inclusion in components and includes only primitive objects such
as pins, nets, or buses."
  (define pointer (geda-object->pointer* object 1))

  (when (null-pointer? (lepton_object_get_page pointer))
    (scm-error 'object-state
               'object-connections
               "Object ~A is not included in a page."
               (list object)
               '()))

  (let* ((gls (s_conn_return_others %null-pointer pointer))
         (ls (glist->object-list gls)))

    (g_list_free gls)
    ls))


(define (object-component object)
  "Returns the component object that contains OBJECT.
If OBJECT is not part of a component, returns #f."
  (define pointer (geda-object->pointer* object 1))

  (let ((parent-pointer (lepton_object_get_parent pointer)))
    (and (not (null-pointer? parent-pointer))
         (pointer->geda-object parent-pointer))))


;;;; Lines

;;; Checks if OBJECT consist of a line segment.
(define (linear-object? object)
  (or (line? object)
      (net? object)
      (bus? object)
      (pin? object)))

(define* (set-line! object start end #:optional color)
  "Sets the parameters of line OBJECT (which may be a line, net,
bus or pin object). START is the position of the start of the new
line in the form '(x . y) and END is the position of the end of
the line.  For pins,the start is the connectable point on the
pin.  If COLOR is specified, it should be the integer color map
index of the color with which to draw the line.  If COLOR is not
specified, the default line color is used.  Returns OBJECT."
  (define pointer (geda-object->pointer* object 1 linear-object? 'line))

  (check-coord start 2)
  (check-coord end 3)
  (and color (check-integer color 4))

  ;; We may need to update connectivity.
  (s_conn_remove_object_connections pointer)

  (let ((info (line-info object))
        (x1 (car start))
        (y1 (cdr start))
        (x2 (car end))
        (y2 (cdr end))
        (whichend (true? (lepton_object_get_whichend pointer)))
        (color (or color (object-color object))))
    (cond
     ((line? object)
      (lepton_line_object_modify pointer x1 y1 0)
      (lepton_line_object_modify pointer x2 y2 1))
     ((net? object)
      (lepton_net_object_modify pointer x1 y1 0)
      (lepton_net_object_modify pointer x2 y2 1))
     ((bus? object)
      (lepton_bus_object_modify pointer x1 y1 0)
      (lepton_bus_object_modify pointer x2 y2 1))
     ((pin? object)
      ;; Swap ends according to pin's whichend flag.
      (lepton_pin_object_modify pointer x1 y1 (if whichend 1 0))
      (lepton_pin_object_modify pointer x2 y2 (if whichend 0 1)))
     ;; Should never be reached.
     (else #f))

    (lepton_object_set_color pointer color)

    ;; We may need to update connectivity.
    (let ((page (lepton_object_get_page pointer)))
      (unless (null-pointer? page)
        (s_conn_update_object page pointer)

        (unless (equal? info (line-info object))
          (lepton_object_page_set_changed pointer)))

      object)))

(define* (make-line start end #:optional color)
  "Make and return a new line object with given START and END
coordinates and given COLOR.  The coordinates must be pairs of the
form '(x . y).  All its other parameters are set to default
values."
  (check-coord start 1)
  (check-coord end 2)
  (and color (check-integer color 3))

  (let ((x1 (car start))
        (y1 (cdr start))
        (x2 (car end))
        (y2 (cdr end))
        (color (or color (default_color_id))))
    (pointer->geda-object
     (lepton_line_object_new color x1 y1 x2 y2))))

;;; Helpers for linear two point objects.
(define (line-pointer-first-coord pointer)
  (cons (lepton_line_object_get_x0 pointer)
        (lepton_line_object_get_y0 pointer)))

(define (line-pointer-second-coord pointer)
  (cons (lepton_line_object_get_x1 pointer)
        (lepton_line_object_get_y1 pointer)))

;;; Helper for pin-swap-whichend?().
(define (object-whichend object)
  (define pointer (geda-object->pointer* object 1))
  (true? (lepton_object_get_whichend pointer)))

;;; The function checks if OBJECT is a pin and its ends,
;;; connectible and not connectible, should be swapped, according
;;; to the pin's whichend flag.
(define (pin-swap-whichend? object)
  (and (pin? object)
       (object-whichend object)))


(define (line-start object)
  "Returns the coordinate of the start of linear OBJECT (which may
be a line, net, bus, or pin object) as a pair '(x . y).  For pin
objects, this is the position of the connectable point on the
pin."
  (define pointer (geda-object->pointer* object 1 linear-object? 'line))

  (define swap-coords? (pin-swap-whichend? object))

  (define get-coord-func
    (if swap-coords? line-pointer-second-coord line-pointer-first-coord))

  (get-coord-func pointer))


(define (line-end object)
  "Returns the coordinate of the end of linear OBJECT (which may
be a line, net, bus, or pin object) as a pair '(x . y).  For pin
objects, this is the position of the non-connectable point on the
pin."
  (define pointer (geda-object->pointer* object 1 linear-object? 'line))

  (define swap-coords? (pin-swap-whichend? object))

  (define get-coord-func
    (if swap-coords? line-pointer-first-coord line-pointer-second-coord))

  (get-coord-func pointer))


(define (line-info object)
  "Retrieves and returns parameters of line OBJECT as a list of
the form '((X0 . Y0) (X1 . Y1) COLOR). The return list includes
parameters as follows: coordinate of the start of the line,
coordinate of the end of the line, and colormap index of color to
be used for drawing the line.  This function works on line, net,
bus, and pin objects.  For pins, the start is the connectible
point on the pin."
  (list (line-start object)
        (line-end object)
        (object-color object)))


;;;; Nets

(define* (make-net start end #:optional color)
  "Make and return a new net object with given START and END
coordinates and given COLOR.  The coordinates must be pairs of the
form '(x . y).  All its other parameters are set to default
values."
  (check-coord start 1)
  (check-coord end 2)
  (and color (check-integer color 3))

  (let ((x1 (car start))
        (y1 (cdr start))
        (x2 (car end))
        (y2 (cdr end))
        (color (or color (color-map-name-to-index 'net))))
    (pointer->geda-object
     (lepton_net_object_new color x1 y1 x2 y2))))


;;;; Buses

(define* (make-bus start end #:optional color)
  "Make and return a new bus object with given START and END
coordinates and given COLOR.  The coordinates must be pairs of the
form '(x . y).  All its other parameters are set to default
values."
  (check-coord start 1)
  (check-coord end 2)
  (and color (check-integer color 3))

  (let ((x1 (car start))
        (y1 (cdr start))
        (x2 (car end))
        (y2 (cdr end))
        (color (or color (color-map-name-to-index 'bus)))
        (ripper-direction 0))
    (pointer->geda-object
     (lepton_bus_object_new color x1 y1 x2 y2 ripper-direction))))


;;;; Pins

;;; This is always 0 in recent file format versions.
(define default-pin-whichend 0)

(define default-pin-color (color-map-name-to-index 'pin))

;;; Creates a new pin object of given CONNECTION-TYPE with
;;; coordinates START and END and optional COLOR.  If the
;;; connection type is 'bus, a bus pin will be created, otherwise
;;; a net pin.
(define* (make-pin connection-type start end #:optional color)
  (let ((make-func (if (eq? connection-type 'bus)
                       lepton_pin_object_new_bus_pin
                       lepton_pin_object_new_net_pin)))
    (pointer->geda-object (make-func (or color
                                         default-pin-color)
                                     (car start)
                                     (cdr start)
                                     (car end)
                                     (cdr end)
                                     default-pin-whichend))))

(define* (make-net-pin start end #:optional color)
  "Creates and returns a new net pin object with given parameters.
START is the position of the start of the new pin (its connectible
end) in the form '(x . y) and END is the position of the end of
the pin.  If COLOR is specified, it should be the integer color
map index of the color with which to draw the pin.  If COLOR is
not specified, the default pin color is used."
  (check-coord start 1)
  (check-coord end 2)
  (and color (check-integer color 3))

  (make-pin 'net start end color))

(define* (make-bus-pin start end #:optional color)
  "Creates and returns a new bus pin object with given parameters.
START is the position of the start of the new pin (its connectible
end) in the form '(x . y) and END is the position of the end of
the pin.  If COLOR is specified, it should be the integer color
map index of the color with which to draw the pin.  If COLOR is
not specified, the default pin color is used."
  (check-coord start 1)
  (check-coord end 2)
  (and color (check-integer color 3))

  (make-pin 'bus start end color))


;;;; Boxes

(define* (set-box! object top-left bottom-right #:optional color)
  "Modifies box OBJECT by setting its parameters to new values.
TOP-LEFT is the new coordinate of the top-left corner and
BOTTOM-RIGHT is the new coordinate of the bottom-right corner.  If
optional COLOR is specified, it shoud be the integer color map
index of the color to be used for drawing the box.  If COLOR is
not specified, the default box color is used.  Returns the
modified box object."
  (define pointer (geda-object->pointer* object 1 box? 'box))

  (check-coord top-left 2)
  (check-coord bottom-right 3)
  (and color (check-integer color 4))

  (let ((info (box-info object))
        (x1 (car top-left))
        (y1 (cdr top-left))
        (x2 (car bottom-right))
        (y2 (cdr bottom-right))
        (color (or color
                   (lepton_object_get_color pointer))))

    (lepton_object_emit_pre_change_notify pointer)

    (lepton_box_object_set_lower_x pointer (max x1 x2))
    (lepton_box_object_set_lower_y pointer (min y1 y2))

    (lepton_box_object_set_upper_x pointer (min x1 x2))
    (lepton_box_object_set_upper_y pointer (max y1 y2))

    (lepton_object_emit_change_notify pointer)

    (lepton_object_set_color pointer color)

    (unless (equal? info (box-info object))
      (lepton_object_page_set_changed pointer))

    object))

(define* (make-box top-left bottom-right #:optional color)
  "Creates and returns a new box object with given parameters.
TOP-LEFT is the position of the top left of the new box in the
form '(x . y), and BOTTOM-RIGHT is the position of the bottom
right of the box.  If optional COLOR is specified, it should be
the integer color map index of the color with which to draw the
box.  If COLOR is not specified, the default box color is used."
  (define default-box-color (default_color_id))

  (check-coord top-left 1)
  (check-coord bottom-right 2)
  (and color (check-integer color 3))

  (let* ((x0 (car top-left))
         (y0 (cdr top-left))
         (x1 (car bottom-right))
         (y1 (cdr bottom-right))
         (left-x (min x0 x1))
         (right-x (max x0 x1))
         (bottom-y (min y0 y1))
         (top-y (max y0 y1))
         (color (or color default-box-color)))
    (pointer->geda-object
     (lepton_box_object_new color
                            left-x
                            top-y
                            right-x
                            bottom-y))))

(define (box-info object)
  "Retrieves and returns the coordinates and color of a box
OBJECT. The return value is a list of the parameters in the form:
'((upper_x . upper_y) (lower_x . lower_y) color)"
  (list (box-top-left object)
        (box-bottom-right object)
        (object-color object)))

(define (box-top-left object)
  "Returns the top left corner coordinate of box OBJECT."
  (define pointer (geda-object->pointer* object 1 box? 'box))

  (cons (lepton_box_object_get_upper_x pointer)
        (lepton_box_object_get_upper_y pointer)))


(define (box-bottom-right object)
  "Returns the bottom right corner coordinate of box OBJECT."
  (define pointer (geda-object->pointer* object 1 box? 'box))

  (cons (lepton_box_object_get_lower_x pointer)
        (lepton_box_object_get_lower_y pointer)))


;;;; Circles

(define* (set-circle! object center radius #:optional color)
  "Modifies circle OBJECT by setting its parameters to new values.
CENTER is the new coordinate of the circle center in the form '(x
. y).  RADIUS is the new radius of the circle.  COLOR is the new
colormap index of the color to be used for drawing the circle.
Returns the modified circle object."
  (define pointer (geda-object->pointer* object 1 circle? 'circle))

  (check-coord center 2)
  (check-integer radius 3)
  (and color (check-integer color 4))

  (let ((info (circle-info object))
        (x (car center))
        (y (cdr center))
        (radius radius)
        (color (or color
                   (lepton_object_get_color pointer))))
    (lepton_circle_object_set_center_x pointer x)
    (lepton_circle_object_set_center_y pointer y)
    (lepton_circle_object_set_radius pointer radius)
    (lepton_object_set_color pointer color)

    (unless (equal? info (circle-info object))
      (lepton_object_page_set_changed pointer))

    object))

(define* (make-circle center radius #:optional color)
  "Creates and returns a new circle object, with all its
parameters set to default values."
  (check-coord center 1)
  (check-integer radius 2)
  (and color (check-integer color 3))

  (pointer->geda-object
   (lepton_circle_object_new (or color
                                 (default_color_id))
                             (car center)
                             (cdr center)
                             radius)))

(define (circle-center object)
  "Returns the coordinate of the center of circle OBJECT as a pair
of integers in the form '(x . y)."
  (define pointer (geda-object->pointer* object 1 circle? 'circle))

  (cons (lepton_circle_object_get_center_x pointer)
        (lepton_circle_object_get_center_y pointer)))

(define (circle-radius object)
  "Returns the radius of circle OBJECT as an integer."
  (define pointer (geda-object->pointer* object 1 circle? 'circle))

  (lepton_circle_object_get_radius pointer))

(define (circle-info object)
  "Retrieves and returns the parameters of a circle OBJECT. The
return value is a list of parameters in the form: '(CENTER RADIUS
COLOR).  CENTER is the coordinate of the circle which is a pair of
integers, RADIUS is integer, and COLOR is the colormap index of
color to be used for drawing the circle."
  (list (circle-center object)
        (circle-radius object)
        (object-color object)))

;;;; Arcs

(define* (set-arc! object center radius start-angle sweep-angle
                   #:optional color)
  "Modifies arc OBJECT by setting its parameters to new values and
returns the modified object.  CENTER is the coordinate of the
center of the arc in the form '(x . y). RADIUS, START-ANGLE, and
SWEEP-ANGLE correspondingly represent its radius, start and sweep
angle.  If optional COLOR is specified, it should be the integer
color map index of the color with which to draw the arc.  If COLOR
is not specified, the default arc color is used.  Returns the
modified arc object."
  (define pointer (geda-object->pointer* object 1 arc? 'arc))

  (check-coord center 2)
  (check-integer radius 3)
  (check-integer start-angle 4)
  (check-integer sweep-angle 5)
  (and color (check-integer color 6))

  (let ((info (arc-info object))
        (center-x (car center))
        (center-y (cdr center)))
    (lepton_arc_object_set_center_x pointer center-x)
    (lepton_arc_object_set_center_y pointer center-y)
    (lepton_arc_object_set_radius pointer radius)
    (lepton_arc_object_set_start_angle pointer start-angle)
    (lepton_arc_object_set_sweep_angle pointer sweep-angle)
    (and color (lepton_object_set_color pointer color))

    ;; Check if arc info has been changed and update its page.
    (unless (equal? info (arc-info object))
      (lepton_object_page_set_changed pointer))
    ;; Return the modified object.
    object))

(define* (make-arc center radius start-angle sweep-angle #:optional color)
  "Creates and returns a new arc object with given parameters.
CENTER is the position of the center of the new arc in the form
'(x . y), and RADIUS is the integer radius of the arc.
START-ANGLE is the angle at which to start the arc, in degrees.
SWEEP-ANGLE is the number of degrees between the start and end
angles.  If optional COLOR is specified, it should be the integer
color map index of the color with which to draw the arc.  If COLOR
is not specified, the default arc color is used."
  (define default-arc-color (default_color_id))

  (check-coord center 1)
  (check-integer radius 2)
  (check-integer start-angle 3)
  (check-integer sweep-angle 4)
  (and color (check-integer color 5))

  (pointer->geda-object
   (lepton_arc_object_new (or color
                              default-arc-color)
                          ;; Center X.
                          (car center)
                          ;; Center Y.
                          (cdr center)
                          radius
                          start-angle
                          sweep-angle)))

(define-public (arc-info object)
  "Returns the parameters of arc OBJECT as a list of its center
coordinate, radius, start and sweep angles, and color in the form:
'((center-x . center-y) radius start-angle sweep-angle color)"
  (list (arc-center object)
        (arc-radius object)
        (arc-start-angle object)
        (arc-sweep-angle object)
        (object-color object)))

(define (arc-center object)
  "Returns the position of the center of arc OBJECT as a pair of
two integers in the form '(x . y)."
  (define pointer (geda-object->pointer* object 1 arc? 'arc))
  (cons (lepton_arc_object_get_center_x pointer)
        (lepton_arc_object_get_center_y pointer)))

(define (arc-radius object)
  "Returns the radius of arc OBJECT as an integer."
  (define pointer (geda-object->pointer* object 1 arc? 'arc))
  (lepton_arc_object_get_radius pointer))

(define (arc-start-angle object)
  "Returns the start angle of arc OBJECT as an integer number of
degrees."
  (define pointer (geda-object->pointer* object 1 arc? 'arc))
  (lepton_arc_object_get_start_angle pointer))

(define (arc-sweep-angle object)
  "Returns the sweep angle of arc OBJECT as an integer number of
degrees."
  (define pointer (geda-object->pointer* object 1 arc? 'arc))
  (lepton_arc_object_get_sweep_angle pointer))

(define (arc-end-angle object)
  "Returns the end angle of arc OBJECT as an integer number of
degrees.  The end angle is the sum of the start and sweep angles
of the arc."
  (define pointer (geda-object->pointer* object 1 arc? 'arc))
  (+ (lepton_arc_object_get_start_angle pointer)
     (lepton_arc_object_get_sweep_angle pointer)))

;;;; Paths

(define* (make-path #:optional color)
  "Creates and returns a new, empty path object with default
stroke and fill options.  If COLOR is specified, it should be the
integer color map index of the color with which to draw the path.
If COLOR is not specified, the default path color is used."
  (pointer->geda-object
   (lepton_path_object_new (or color (default_color_id))
                           (string->pointer ""))))

(define (path-length object)
  "Returns the number of path elements in path OBJECT."
  (define pointer (geda-object->pointer* object 1 path? 'path))

  (lepton_path_object_get_num_sections pointer))

(define (path-ref object index)
  "Retrieves and returns a path element at INDEX from path OBJECT
.  If INDEX is not a valid index, raises a Scheme
'out-of-range error.

The return value is a list.  The first element in the list is a
symbol indicating the type of path element ('moveto, 'lineto,
'curveto or 'closepath), and the remainder of the list contains
zero or more control point coordinates, depending on the type of
path element.  Each element is evaluated relative to the current
path position.
- moveto: x and y coordinates of position to step to.
- lineto: x and y coordinates of straight line endpoint.
- curveto: coordinates of first Bezier control point; coordinates
  of second control point; and coordinates of curve endpoint.
- closepath: No coordinate parameters.

All coordinates are absolute."

  (define pointer (geda-object->pointer* object 1 path? 'path))

  (check-integer index 2)

  ;; Check index is not valid for path.
  (when (or (< index 0)
            (>= index (lepton_path_object_get_num_sections pointer)))
    (scm-error 'out-of-range
               'path-ref
               "Argument ~A out of range: ~A"
               (list 2 index)
               (list index)))

  (let* ((section (lepton_path_object_get_section pointer index))
         (code-string-pointer
          (lepton_path_section_code_to_string
           (lepton_path_section_get_code section)))
         (code (and (not (null-pointer? code-string-pointer))
                    (string->symbol (pointer->string code-string-pointer))))
         (x1 (lepton_path_section_get_x1 section))
         (y1 (lepton_path_section_get_y1 section))
         (x2 (lepton_path_section_get_x2 section))
         (y2 (lepton_path_section_get_y2 section))
         (x3 (lepton_path_section_get_x3 section))
         (y3 (lepton_path_section_get_y3 section)))
    (match code
      ('moveto
       (list 'moveto (cons x3 y3)))
      ('lineto
       (list 'lineto (cons x3 y3)))
      ('curveto
       (list 'curveto (cons x1 y1) (cons x2 y2) (cons x3 y3)))
      ('closepath
       (list 'closepath))
      (_ (error "Path object ~A has invalid element type ~A at index ~A."
                object code index)))))

(define (path-remove! object index)
  "Removes the path element at INDEX from path OBJECT. If INDEX is
not a valid index, raises a Scheme 'out-of-range error.  Returns
modified OBJECT."
  (define pointer (geda-object->pointer* object 1 path? 'path))

  (check-integer index 2)

  ;; Check index is not valid for path.
  (when (or (< index 0)
            (>= index (lepton_path_object_get_num_sections pointer)))
    (scm-error 'out-of-range
               'path-ref
               "Argument ~A out of range: ~A"
               (list 2 index)
               (list index)))

  (lepton_path_object_remove_section pointer index)

  (lepton_object_page_set_changed pointer)

  object)

(define* (path-insert! object index type #:optional c1 c2 c3)
  "Inserts a path element into the path OBJECT at INDEX.  The type
of element to be inserted is specified by TYPE which must be a
symbol, and the remaining optional arguments C1, C2, and C3
provide as many absolute coordinate pairs in the form '(x . y) as
are required by that element type:
- 'closepath elements require no coordinate arguments;
- 'moveto and 'lineto elements require one coordinate pair, for
  the endpoint;
- 'curveto elements require the coordinates of the first control
  point, coordinates of the second control point, and coordinates
  of the endpoint.
If INDEX is negative, or is greater than or equal to the number
of elements currently in the path, the new element will be appended
to the path. Returns modified OBJECT."
  ;; For convenience.
  (define c0 '(0 . 0))

  (define (make-c-section code c1 c2 c3)
    (let ((x1 (car c1))
          (y1 (cdr c1))
          (x2 (car c2))
          (y2 (cdr c2))
          (x3 (car c3))
          (y3 (cdr c3)))
      (make-c-struct (list int int int int int int int)
                     (list code x1 y1 x2 y2 x3 y3))))

  (define (error-invalid-path-element)
    (error "Invalid path element type ~A." type))

  (define pointer (geda-object->pointer* object 1 path? 'path))

  (check-integer index 2)
  (check-symbol type 3)

  ;; Check & extract path element type.
  ;; Check the right number of coordinates have been provided.
  (let* ((code
          (lepton_path_section_code_from_string
           (string->pointer (symbol->string type))))
         (section
          (match type
            ('curveto
             (unless (and c1 c2 c3)
               (error-invalid-path-element))
             (check-coord c1 4)
             (check-coord c2 5)
             (check-coord c3 6)
             (make-c-section code c1 c2 c3))
            ((or 'lineto 'moveto)
             (unless c1
               (error-invalid-path-element))
             (check-coord c1 4)
             (make-c-section code c0 c0 c1))
            ('closepath
             (make-c-section code c0 c0 c0))
            (_ (error-invalid-path-element)))))

    (lepton_path_object_insert_section pointer section index)

    (lepton_object_page_set_changed pointer)

    object))

(define (path-info object)
  "Returns info on all elements of path OBJECT."
  (let ((len (path-length object)))
    (let loop ((i 0)
               (info '()))
      (if (< i len)
          (loop (1+ i) (cons (path-ref object i) info))
          (reverse info)))))


;;;; Pictures

;;; Check arguments.
(define (check-picture-angle angle pos)
  (define (check-angle-value angle)
    (match angle
      ((or 0 90 180 270) angle)
      (_ (error "Invalid picture angle: ~A. Must be 0, 90, 180, or 270 degrees."
                angle))))

  (check-integer angle pos)
  (check-angle-value angle))


(define (set-picture! object top-left bottom-right angle mirror)
  "Sets the parameters of picture OBJECT.  TOP-LEFT and
BOTTOM-RIGHT are the new coordinates of the picture in the form
'(x . y).  ANGLE is the new rotation angle.  MIRROR is the boolean
value which sets whether the picture object should be mirrored."
  (define pointer (geda-object->pointer* object 1 picture? 'picture))

  (check-coord top-left 2)
  (check-coord bottom-right 3)
  (check-picture-angle angle 4)
  (check-boolean mirror 5)

  (if (equal? (picture-info object)
              (list (picture-filename object)
                    top-left
                    bottom-right
                    angle
                    mirror))
      ;; If nothing changed, return the object as is.
      object
      ;; Otherwise apply new arguments.
      (let ((x1 (car top-left))
            (y1 (cdr top-left))
            (x2 (car bottom-right))
            (y2 (cdr bottom-right))
            (angle angle)
            (mirror (if mirror 1 0)))

        (lepton_object_emit_pre_change_notify pointer)

        (lepton_picture_object_set_angle pointer angle)
        (lepton_picture_object_set_mirrored pointer mirror)

        ;; Normalise the requested rectangle.
        (lepton_picture_object_set_lower_x pointer (max x1 x2))
        (lepton_picture_object_set_lower_y pointer (min y1 y2))
        (lepton_picture_object_set_upper_x pointer (min x1 x2))
        (lepton_picture_object_set_upper_y pointer (max y1 y2))

        (lepton_object_page_set_changed pointer)
        (lepton_object_emit_change_notify pointer)

        object)))


(define (make-picture/vector vector
                             filename
                             top-left
                             bottom-right
                             angle
                             mirror)
  "Creates and returns a new picture object for FILENAME, by
reading image data from VECTOR (which should be in a standard
image file format).  If VECTOR could not be loaded, an error is
raised. TOP-LEFT, BOTTOM-RIGHT, ANGLE and MIRROR specify the
picture transformation. The points TOP-LEFT and BOTTOM-RIGHT
should be specified in the form '(x . y).  ANGLE should be an
integer value one of 0, 90, 180, or 270 degrees.  MIRROR is a
boolean flag which specifies if the picture should be mirrored."
  ;; C boolean values.
  (define TRUE 1)
  (define FALSE 0)

  (define (pixbuf-missing? pointer)
    (let* ((pixbuf-pointer (lepton_picture_object_get_pixbuf pointer))
           (null-pixbuf-pointer? (null-pointer? pixbuf-pointer))
           (fallback-pointer? (equal? pixbuf-pointer
                                      (lepton_picture_get_fallback_pixbuf))))
      ;; Ensure the object will properly finalized.  See comments
      ;; in the C function
      ;; lepton_picture_object_get_pixbuf_pointer().
      (g_object_unref pixbuf-pointer)
      (or null-pixbuf-pointer?
          fallback-pointer?)))

  (check-vector vector 1)
  (check-string filename 2)
  (check-coord top-left 3)
  (check-coord bottom-right 4)
  (check-picture-angle angle 5)
  (check-boolean mirror 6)

  ;; First assign default values.
  (let* ((file-content (string->pointer
                        (list->string
                         (map integer->char vector))))
         (file-length (s8vector-length (any->s8vector vector)))
         (filename-pointer (string->pointer filename))
         (x1 (min (car top-left) (car bottom-right)))
         (y1 (max (cdr top-left) (cdr bottom-right)))
         (x2 (max (car top-left) (car bottom-right)))
         (y2 (min (cdr top-left) (cdr bottom-right)))
         (mirrored (if mirror TRUE FALSE))
         (embedded TRUE)
         (pointer (lepton_picture_object_new file-content
                                             file-length
                                             filename-pointer
                                             x1
                                             y1
                                             x2
                                             y2
                                             angle
                                             mirrored
                                             embedded)))
    (if (pixbuf-missing? pointer)
        (error "Failed to set picture image data from vector.")
        ;; Return picture object.
        (pointer->geda-object pointer))))

(define (picture-filename object)
  "Returns the filename associated with picture OBJECT."
  (define pointer (geda-object->pointer* object 1 picture? 'picture))

  (let ((filename-pointer (lepton_picture_object_get_filename pointer)))
    (and (not (null-pointer? filename-pointer))
         (pointer->string filename-pointer))))

(define (picture-top-left object)
  "Returns the top left corner coordinate of picture OBJECT as a
pair in the form '(x . y)."
  (define pointer (geda-object->pointer* object 1 picture? 'picture))

  (cons (lepton_picture_object_get_upper_x pointer)
        (lepton_picture_object_get_upper_y pointer)))

(define (picture-bottom-right object)
  "Returns the bottom right corner coordinate of picture OBJECT as
a pair in the form '(x . y)."
  (define pointer (geda-object->pointer* object 1 picture? 'picture))

  (cons (lepton_picture_object_get_lower_x pointer)
        (lepton_picture_object_get_lower_y pointer)))

(define (picture-angle object)
  "Returns the rotation angle of picture OBJECT as an integer
number of degrees."
  (define pointer (geda-object->pointer* object 1 picture? 'picture))

  (lepton_picture_object_get_angle pointer))

(define (picture-mirror? object)
  "Returns #t if picture OBJECT is mirrored.  Otherwise returns
#f."
  (define pointer (geda-object->pointer* object 1 picture? 'picture))

  (true? (lepton_picture_object_get_mirrored pointer)))


(define (picture-info object)
  "Retrieves the parameters of PICTURE.  Returns the list
of parameters in the form:
  (filename (x1 . y1) (x2 . y2) angle mirrored)
where:
  filename - filename of PICTURE.
  x1 - X-coordinate of top left of PICTURE.
  y1 - Y-coordinate of top left of PICTURE.
  x2 - X-coordinate of bottom right of PICTURE.
  y2 - Y-coordinate of bottom right of PICTURE.
  angle - rotation angle.
  mirrored - whether PICTURE object is mirrored."
  (define pointer (geda-object->pointer* object 1 picture? 'picture))

  (list (picture-filename object)
        (picture-top-left object)
        (picture-bottom-right object)
        (picture-angle object)
        (picture-mirror? object)))


;;;; Text

;;; Returns text alignment symbol SYM if it is valid.  Otherwise
;;; returns #f.
(define (check-text-alignment-symbol sym)
  (match sym
    ((or 'upper-left 'upper-center 'upper-right
         'middle-left 'middle-center 'middle-right
         'lower-left 'lower-center 'lower-right)
     sym)
    (_ #f)))

;;; Returns text attribute show mode symbol SYM if it is valid.
;;; Otherwise returns #f.
(define (check-text-attribute-show-mode sym)
  (match sym
    ((or 'name 'value 'both) sym)
    (_ #f)))

(define (check-text-alignment align pos)
  (check-symbol align pos)
  (unless (check-text-alignment-symbol align)
    (error "Invalid text alignment: ~A." align)))

(define (check-text-show show pos)
  (check-symbol show pos)
  (unless (check-text-attribute-show-mode show)
    (error "Invalid text name/value visibility: ~A." show)))

(define (check-text-angle angle pos)
  (define (check-angle-value angle)
    (match angle
      ((or 0 90 180 270) angle)
      (_ (error "Invalid text angle: ~A. Must be 0, 90, 180, or 270 degrees."
                angle))))

  (check-integer angle pos)
  (check-angle-value angle))


(define (symbol->text-alignment sym)
  (lepton_text_object_alignment_from_string
   (string->pointer (symbol->string sym))))

(define (text-visibility->integer visible?)
  (if visible? 1 0))

(define (symbol->text-attribute-show-mode sym)
  (lepton_text_object_show_from_string
   (string->pointer (symbol->string sym))))


(define* (set-text! object anchor align angle string size visible? show
                    #:optional color)
  "Sets the parameters of text OBJECT.  Returns the modified
OBJECT.  ANCHOR is the position of the anchor of the new text in
the form '(x . y), and ALIGN is a symbol determining how the text
should be aligned relative to the anchor.  ALIGN must be one of
the following symbols:
  - 'lower-left
  - 'middle-left
  - 'upper-left
  - 'lower-center
  - 'middle-center
  - 'upper-center
  - 'lower-right
  - 'middle-right
  - 'upper-right
For example, if ALIGN is 'upper-center, the anchor will be located
at the top center of the rendered text block.

ANGLE should be an integer multiple of 90 degrees, determining the
angle which the text should be displayed at.  STRING is the string
contents for the text object.  SIZE is the font size to use.  If
VISIBLE is #f, the text will be invisible; otherwise, it will be
visible.

When the STRING is in an attribute format, the SHOW argument
determines which parts of the STRING will be displayed.  It must
be one of the following symbols:
  - 'name
  - 'value
  - 'both
If COLOR is specified, it should be the integer color map index of
the color with which to draw the text.  If COLOR is not specified,
the default text color is used."
  (define pointer (geda-object->pointer* object 1 text? 'text))

  (check-coord anchor 2)
  (check-text-alignment align 3)
  (check-text-angle angle 4)
  (check-string string 5)
  (check-integer size 6)
  (check-boolean visible? 7)
  (check-text-show show 8)
  (and color (check-integer color 9))

  (let ((color (or color (object-color object)))
        (new-info (list anchor align angle string size visible? show color)))

    ;; Compare current and new parameters. Do nothing if nothing
    ;; changes.
    (unless (equal? (text-info object) new-info)
      (let ((x (car anchor))
            (y (cdr anchor))
            (align (symbol->text-alignment align))
            (string (string->pointer string))
            (visibility (text-visibility->integer visible?))
            (show (symbol->text-attribute-show-mode show)))

        ;; Actually make changes
        (lepton_object_emit_pre_change_notify pointer)

        (lepton_text_object_set_x pointer x)
        (lepton_text_object_set_y pointer y)
        (lepton_text_object_set_alignment pointer align)
        (lepton_text_object_set_angle pointer angle)
        (lepton_text_object_set_size pointer size)
        (lepton_text_object_set_visibility pointer visibility)
        (lepton_text_object_set_show pointer show)
        (lepton_object_set_color pointer color)

        (lepton_text_object_set_string pointer string)
        (lepton_text_object_recreate pointer)

        (lepton_object_page_set_changed pointer)

        (lepton_object_emit_change_notify pointer))))

  ;; Return the same object.
  object)


(define* (make-text anchor align angle string size visible? show
                    #:optional color)
  "Creates and returns a new text object.  ANCHOR is the position
of the anchor of the new text in the form '(x . y), and ALIGN is a
symbol determining how the text should be aligned relative to the
anchor.  ALIGN must be one of the following symbols:
  - 'lower-left
  - 'middle-left
  - 'upper-left
  - 'lower-center
  - 'middle-center
  - 'upper-center
  - 'lower-right
  - 'middle-right
  - 'upper-right
For example, if ALIGN is 'upper-center, the anchor will be located
at the top center of the rendered text block.

ANGLE should be an integer multiple of 90 degrees, determining the
angle which the text should be displayed at.  STRING is the string
contents for the text object.  SIZE is the font size to use.  If
VISIBLE is #f, the text will be invisible; otherwise, it will be
visible.

When the STRING is in an attribute format, the SHOW argument
determines which parts of the STRING will be displayed.  It must
be one of the following symbols:
  - 'name
  - 'value
  - 'both
If COLOR is specified, it should be the integer color map index of
the color with which to draw the text.  If COLOR is not specified,
the default text color is used."
  (check-coord anchor 1)
  (check-text-alignment align 2)
  (check-text-angle angle 3)
  (check-string string 4)
  (check-integer size 5)
  (check-boolean visible? 6)
  (check-text-show show 7)
  (and color (check-integer color 8))

  (let ((x (car anchor))
        (y (cdr anchor))
        (align (symbol->text-alignment align))
        (string (string->pointer string))
        (visibility (text-visibility->integer visible?))
        (show (symbol->text-attribute-show-mode show)))
    (pointer->geda-object
     (lepton_text_object_new (or color
                                 (default_color_id))
                             x
                             y
                             align
                             angle
                             string
                             size
                             visibility
                             show))))

(define (text-info object)
  "Returns the parameters of text OBJECT as a list in the form:
  '(anchor align angle string size visible show color)
where anchor is a pair of integers in the form '(x . y)."
  (list (text-anchor object)
        (text-align object)
        (text-angle object)
        (text-string object)
        (text-size object)
        (text-visible? object)
        (text-attribute-mode object)
        (object-color object)))

(define (text-anchor object)
  "Returns the position of the anchor of text OBJECT in the form
'(x . y)."
  (define pointer (geda-object->pointer* object 1 text? 'text))
  (cons (lepton_text_object_get_x pointer)
        (lepton_text_object_get_y pointer)))

(define (text-align object)
  "Returns the alignment of text OBJECT as one of the following
symbols:
  - lower-left
  - middle-left
  - upper-left
  - lower-center
  - middle-center
  - upper-center
  - lower-right
  - middle-right
  - upper-right"
  (define pointer (geda-object->pointer* object 1 text? 'text))

  (let* ((align (lepton_text_object_get_alignment pointer))
         (string-pointer (lepton_text_object_alignment_to_string align))
         (sym (and (not (null-pointer? string-pointer))
                   (string->symbol (pointer->string string-pointer)))))
    (or (check-text-alignment-symbol sym)
        (error "Text object ~A has invalid text alignment ~A" object sym))))

(define (text-angle object)
  "Returns the angle that text OBJECT is displayed at as an
integer."
  (define pointer (geda-object->pointer* object 1 text? 'text))
  (lepton_text_object_get_angle pointer))

(define (text-string object)
  "Returns the string content of text OBJECT."
  (define pointer (geda-object->pointer* object 1 text? 'text))
  (pointer->string (lepton_text_object_get_string pointer)))

(define (set-text-string! object str)
  "Set the string content of text OBJECT to STR."
  (define pointer (geda-object->pointer* object 1 text? 'text))

  (check-string str 2)

  (unless (string= str (text-string object))
    (lepton_object_emit_pre_change_notify pointer)

    (lepton_text_object_set_string pointer
                                   (string->pointer str))
    ;; Update internals of text object to properly display the new
    ;; string.
    (lepton_text_object_recreate pointer)
    (lepton_object_page_set_changed pointer)

    (lepton_object_emit_change_notify pointer))

  object)

(define (text-size object)
  "Return the font size of text OBJECT as an integer."
  (define pointer (geda-object->pointer* object 1 text? 'text))
  (lepton_text_object_get_size pointer))

(define (text-visible? object)
  "Returns #t if text OBJECT is set to be visible.  Otherwise
returns #f."
  (define pointer (geda-object->pointer* object 1 text? 'text))
  (true? (lepton_text_object_is_visible pointer)))

(define (set-text-visibility! object visible?)
  "If VISIBLE? is #f, sets text OBJECT to be invisible;
otherwise, sets it to be visible."
  (define pointer (geda-object->pointer* object 1 text? 'text))

  (check-boolean visible? 2)

  (unless (eq? visible? (text-visible? object))
    (lepton_object_emit_pre_change_notify pointer)

    (lepton_text_object_set_visibility
     pointer
     (text-visibility->integer visible?))
    (lepton_object_page_set_changed pointer)

    (lepton_object_emit_change_notify pointer))

  object)

(define (text-attribute-mode object)
  "Returns a symbol indicating which parts of TEXT will be
displayed when text OBJECT is a valid attribute.  The returned
value will be one of the following symbols:
  - name
  - value
  - both"
  (define pointer (geda-object->pointer* object 1 text? 'text))

  (let* ((show (lepton_text_object_get_show pointer))
         (string-pointer (lepton_text_object_show_to_string show))
         (sym (and (not (null-pointer? string-pointer))
                   (string->symbol (pointer->string string-pointer)))))
    (or (check-text-attribute-show-mode sym)
        (error "Text object ~A has invalid text attribute visibility ~A" object sym))))


;;;; Component objects

(define-public (set-component! c position angle mirror locked)
  (%set-component! c (car position) (cdr position) angle mirror locked))

(define-public (set-component-with-transform! c position angle mirror locked)
  (let ((obj (%set-component! c 0 0 0 #f locked)))
    (translate-object!
      (rotate-object!
        (if mirror (mirror-object! obj 0) obj)
        0 0 angle)
    (car position)
    (cdr position))))


(define (%make-component basename)
  "Creates and returns a new, empty component object, with given
BASENAME and with all other parameters set to default values.  It
is initially set to be embedded."
  (define TRUE 1)
  (define FALSE 0)

  (check-string basename 1)

  (let ((color (default_color_id))
        (x 0)
        (y 0)
        (angle 0)
        (mirror FALSE)
        (selectable? TRUE))
    (pointer->geda-object
     (lepton_component_new_embedded color
                                    x
                                    y
                                    angle
                                    mirror
                                    (string->pointer basename)
                                    selectable?))))

(define-public (make-component basename . args)
  (let ((c (%make-component basename)))
    (apply set-component! c args)))

(define-public (make-component/library basename . args)
  (let ((c (%make-component/library basename)))
    (if c (apply set-component! c args) #f)))


(define (%component-info component)
  "Return the parameters of COMPONENT as a list of values:
  - Basename.
  - Base x-coordinate.
  - Base y-coordinate.
  - Rotation angle.
  - Whether object is mirrored.
  - Whether object is locked."
  (define pointer (geda-object->pointer* component 1 component? 'component))

  (list (pointer->string (lepton_component_object_get_basename pointer))
        (lepton_component_object_get_x pointer)
        (lepton_component_object_get_y pointer)
        (lepton_component_object_get_angle pointer)
        (true? (lepton_component_object_get_mirror pointer))
        (not (true? (lepton_object_get_selectable pointer)))))


(define-public (component-info c)
  (let* ((params (%component-info c))
         (tail (list-tail params 3))
         (position (list-tail params 1)))
    (set-car! position (cons (list-ref position 0)
                             (list-ref position 1)))
    (set-cdr! position tail)
    params))

(define-public (component-basename c)
  (list-ref (component-info c) 0))

(define (component-filename object)
  "Returns symbol's full file name for component OBJECT, if the
component has a symbol file associated with it.  Otherwise returns
#f."
  (define pointer (geda-object->pointer* object 1 component? 'component))

  (let ((sym (s_clib_get_symbol_by_name
              (lepton_component_object_get_basename pointer))))
    (and (not (null-pointer? sym))
         (let ((fname (s_clib_symbol_get_filename sym)))
            (and (not (null-pointer? fname))
                 (pointer->string fname))))))

(define-public (component-position c)
  (list-ref (component-info c) 1))

(define-public (component-angle c)
  (list-ref (component-info c) 2))

(define-public (component-mirror? c)
  (list-ref (component-info c) 3))

(define-public (component-locked? c)
  (list-ref (component-info c) 4))


(define (component-contents component)
  "Returns a list of the primitive objects that make up
COMPONENT."
  (define component-pointer
    (geda-object->pointer* component 1 component? 'component))

  (glist->object-list
   (lepton_component_object_get_contents component-pointer)))


(define (%component-append! component object)
  "Modifies COMPONENT by adding primitive OBJECT to it. If OBJECT
is already included in another component object or in a
object-page, or if the object is itself a component object, raises
an 'object-state error. If OBJECT is already included in
COMPONENT, does nothing.  Returns COMPONENT."
  (define component-pointer
    (geda-object->pointer* component 1 component? 'component))

  (define object-pointer
    (geda-object->pointer* object 2))

  (let ((object-parent (lepton_object_get_parent object-pointer))
        (object-page (lepton_object_get_page object-pointer)))

    ;; Check that object is not already attached to a page or a
    ;; different component.
    (when (or (not (null-pointer? object-page))
              (and (not (null-pointer? object-parent))
                   (not (equal? object-parent component-pointer))))
      (scm-error 'object-state
                 'component-append!
                 "Object ~A is already included in something."
                 (list object)
                 '()))

    (if (equal? object-parent component-pointer)
        component

        (let ((primitives
               (lepton_component_object_get_contents component-pointer)))
          ;; Don't need to emit change notifications for the
          ;; object-pointer because it's guaranteed not to be
          ;; present in a page at this point.
          (lepton_object_emit_pre_change_notify component-pointer)
          (lepton_component_object_set_contents
           component-pointer
           (g_list_append primitives object-pointer))
          (lepton_object_set_parent object-pointer component-pointer)

          (let ((parent-page (lepton_object_get_page component-pointer)))
            ;; We may need to update connections.
            (unless (null-pointer? parent-page)
              (s_conn_update_object parent-page object-pointer))

            (lepton_object_emit_change_notify component-pointer)

            (lepton_object_page_set_changed component-pointer)

            component)))))

(define-public (component-append! component . objects)
  (for-each (lambda (x) (%component-append! component x)) objects)
  component)

(define (%component-remove! component object)
  "Removes primitive OBJECT from COMPONENT and return COMPONENT.
Raises an 'object-state error if OBJECT is included in a page or a
component other than COMPONENT.  If OBJECT is not included
anywhere, it does nothing."
  (define component-pointer
    (geda-object->pointer* component 1 component? 'component))

  (define object-pointer
    (geda-object->pointer* object 2))

  (let ((object-page (lepton_object_get_page object-pointer))
        (object-parent (lepton_object_get_parent object-pointer)))

    ;; Check that object is not included in a different component.
    (when (and (not (null-pointer? object-parent))
               (not (equal? object-parent component-pointer)))
      (scm-error 'object-state
                 'component-remove!
                 "Object ~A is attached to a different component"
                 (list object)
                 '()))

    ;; Check that object is not included in a page.
    (when (and (null-pointer? object-parent)
               (not (null-pointer? object-page)))
      (scm-error 'object-state
                 'component-remove!
                 "Object ~A is attached to a page"
                 (list object)
                 '()))

    ;; Check that object is not attached as an attribute.
    (unless (null-pointer? (lepton_object_get_attached_to object-pointer))
      (scm-error 'object-state
                 'component-remove!
                 "Object ~A is attached as an attribute"
                 (list object)
                 '()))

    ;; Check that object doesn't have attributes.
    (unless (null-pointer? (lepton_object_get_attribs object-pointer))
      (scm-error 'object-state
                 'component-remove!
                 "Object ~A has attributes"
                 (list object)
                 '()))

    (if (null-pointer? object-parent)
        component

        (let ((primitives
               (lepton_component_object_get_contents component-pointer)))
          ;; Don't need to emit change notifications for the
          ;; object-pointer because only the component-pointer
          ;; will remain in the page.
          (lepton_object_emit_pre_change_notify component-pointer)

          (lepton_component_object_set_contents
           component-pointer
           (g_list_remove_all primitives object-pointer))
          (lepton_object_set_parent object-pointer %null-pointer)

          ;; We may need to update connections.
          (s_conn_remove_object object-page object-pointer)
          (s_conn_remove_object_connections object-pointer)

          (lepton_object_emit_change_notify component-pointer)

          (lepton_object_page_set_changed component-pointer)

          component))))


(define-public (component-remove! component . objects)
  (for-each (lambda (x) (%component-remove! component x)) objects)
  component)

;;;; Fill and stroke

;;; Helper function to check if OBJECT supports stroke
;;; modification.
(define (strokable? object)
  (or (line? object)
      (box? object)
      (circle? object)
      (arc? object)
      (path? object)))

(define (stroke-cap-type? cap)
  (match cap
    ((or 'none 'square 'round) cap)
    (_ #f)))

(define (stroke-dash-type? dash)
  (match dash
    ((or 'solid 'dotted 'dashed 'center 'phantom) dash)
    (_ #f)))

(define (object-stroke object)
  "Returns the stroke properties of OBJECT.  If OBJECT is not a
line, box, circle, arc, or path, throws a Scheme error.  The
return value is a list of parameters:
  - stroke width
  - cap style (a symbol: 'none, 'square or 'round)
  - dash style (a symbol: 'solid, 'dotted, 'dashed, 'center, or
    'phantom)
  - up to two dash parameters, depending on the dash style:
    - For solid lines, no parameters.
    - For dotted lines, dot spacing.
    - For other styles, dot/dash spacing and dash length.
The dash parameters are ignored in case they are not supported for
the dash style."
  (define pointer (geda-object->pointer* object 1 strokable? 'strokable))

  (let ((cap-type (object-stroke-cap object))
        (line-type (object-stroke-line-type object))
        (width (object-stroke-width object))
        (dash-length (object-stroke-dash-length object))
        (space-length (object-stroke-dash-space object)))
    (case line-type
      ((solid) (list width cap-type line-type))
      ((dotted) (list width cap-type line-type space-length))
      ;; dashed, center, and phantom.
      (else  (list width cap-type line-type space-length dash-length)))))

(define* (set-object-stroke! object
                             width
                             cap-type
                             dash-type
                             #:optional
                             space-length
                             dash-length)
  "Sets and updates the stroke properties of OBJECT.
If OBJECT is not a line, box, circle, arc, or path, throws a
Scheme error.  Parameters SPACE-LENGTH and DASH-LENGTH are
optional.  The following parameters are used:
  - OBJECT is the object to set stroke settings for.
  - WIDTH is a new stroke integer width.
  - CAP-TYPE is a new stroke cap type.  It may be 'none, 'square,
    or 'round.
  - DASH-TYPE is a new dash type.  It may be 'solid, 'dotted,
    'dashed, 'center, or 'phantom
  - SPACE-LENGTH is an integer dot/dash spacing for dash styles
    other than 'solid.
  - DASH-LENGTH is an integer dash length for dash styles other
    than 'solid or 'dotted.
Returns modified OBJECT."
  (define with-dashes?
    (or (eq? dash-type 'dashed)
        (eq? dash-type 'center)
        (eq? dash-type 'phantom)))

  (define with-spaces?
    (or (eq? dash-type 'dashed)
        (eq? dash-type 'center)
        (eq? dash-type 'phantom)
        (eq? dash-type 'dotted)))

  (define pointer (geda-object->pointer* object 1 strokable? 'strokable))

  (check-symbol cap-type 3)
  (unless (stroke-cap-type? cap-type)
    (error "Invalid stroke cap style ~A."
           cap-type))

  (check-symbol dash-type 4)
  (unless (stroke-dash-type? dash-type)
    (error "Invalid stroke dash style ~A."
           dash-type))

  (when with-spaces?
    (unless space-length
      (error "Missing dot/dash space parameter for dash style ~A."
             space-length))
    (check-integer space-length 5))

  (when with-dashes?
    (unless dash-length
      (error "Missing dash length parameter for dash style ~A."
             dash-length))
    (check-integer dash-length 6))

  (let ((old-object-stroke (object-stroke object))
        (cap-type (lepton_stroke_cap_type_from_string
                   (string->pointer (symbol->string cap-type))))
        (dash-type (lepton_stroke_type_from_string
                    (string->pointer (symbol->string dash-type)))))

    (lepton_object_set_stroke_type pointer dash-type)
    (lepton_object_set_stroke_cap_type pointer cap-type)
    (lepton_object_set_stroke_width pointer width)
    (when with-spaces?
      (lepton_object_set_stroke_space_length pointer space-length))

    (when with-dashes?
      (lepton_object_set_stroke_dash_length pointer dash-length))

    ;; Check if stroke info has been changed and update object's
    ;; page.
    (unless (equal? old-object-stroke (object-stroke object))
      (lepton_object_page_set_changed pointer))
    ;; Return the modified object.
    object))

(define (object-stroke-width object)
  "Returns the integer stroke width of OBJECT, which must be a
line, box, circle, arc, or path object."
  (define pointer
    (geda-object->pointer* object 1 strokable? 'strokable))

  (lepton_object_get_stroke_width pointer))

(define (object-stroke-cap object)
  "Returns the stroke cap style of OBJECT, which must be a line,
box, circle, arc, or path object.  The returned value is one of the
symbols 'none, 'square or 'round."
  ;; Check if CAP is a valid cap type symbol.
  (define (check-stroke-cap-type cap)
    (if (stroke-cap-type? cap)
        cap
        (error "Unsupported cap style for object ~A: ~A." object cap)))

  ;; Transforms cap type integer value obtained from C code into a
  ;; symbol.  Reports an error if the value is invalid.
  (define (cap-type->symbol cap-type)
    (let ((c-string-pointer (lepton_stroke_cap_type_to_string cap-type)))
      (if (null-pointer? c-string-pointer)
          (error "Invalid stroke cap style for object ~A." object)
          (check-stroke-cap-type
           (string->symbol (pointer->string c-string-pointer))))))

  (define pointer
    (geda-object->pointer* object 1 strokable? 'strokable))

  (cap-type->symbol (lepton_object_get_stroke_cap_type pointer)))

(define (object-stroke-line-type object)
  ;; Check if STROKE-TYPE is a valid stroke type symbol.
  (define (check-stroke-dash-type stroke-type)
    (if (stroke-dash-type? stroke-type)
        stroke-type
        (error "Unsupported line type for object ~A: ~A." object stroke-type)))

  ;; Transforms stroke type integer value obtained from C code
  ;; into a symbol.  Reports an error if the value is invalid.
  (define (stroke-type->symbol stroke-type)
    (let ((c-string-pointer (lepton_stroke_type_to_string stroke-type)))
      (if (null-pointer? c-string-pointer)
          (error "Invalid stroke line type for object ~A." object)
          (check-stroke-dash-type
           (string->symbol (pointer->string c-string-pointer))))))

  (define pointer
    (geda-object->pointer* object 1 strokable? 'strokable))

  (stroke-type->symbol (lepton_object_get_stroke_type pointer)))

(define (object-stroke-dash-length object)
  (define pointer
    (geda-object->pointer* object 1 strokable? 'strokable))
  (lepton_object_get_stroke_dash_length pointer))

(define (object-stroke-dash-space object)
  (define pointer
    (geda-object->pointer* object 1 strokable? 'strokable))
  (lepton_object_get_stroke_space_length pointer))

(define (object-stroke-dash object)
  "Returns the dash style of OBJECT, which must be a line, box,
circle, arc, or path object.  The return value is a list of between
one and three parameters:
  - dash style, one of the symbols 'solid, 'dotted,
    'dashed, 'center or 'phantom.
  - for styles other than 'solid, dot/dash spacing;
  - for 'dashed, 'center and 'phantom, dash length."
  (let ((line-type (object-stroke-line-type object))
        (dash-length (object-stroke-dash-length object))
        (space-length (object-stroke-dash-space object)))
    (case line-type
      ((solid) (list line-type))
      ((dotted) (list line-type space-length))
      ;; dashed, center, and phantom.
      (else  (list line-type space-length dash-length)))))


;;; Helper function to check if OBJECT supports filling
;;; modification.
(define (fillable? object)
  (or (box? object)
      (circle? object)
      (path? object)))

(define (fill-type? type)
  (match type
    ((or 'hollow 'solid 'hatch 'mesh) type)
    (_ #f)))

(define (object-fill object)
  "Returns the fill properties of OBJECT.  If OBJECT is
not a box, circle, or path, throws a Scheme error.  The return
value is a list of parameters:
  - fill style (a symbol: 'hollow, 'solid, 'mesh, or 'hatch)
  - up to five fill parameters, depending on fill style:
    - none for hollow or solid fills;
    - line width, line angle, and line spacing for hatch fills;
    - line width, first angle and spacing, and second angle and
      spacing for mesh fills."
  (define (fill-type->symbol fill_type)
    (let ((c_string (lepton_fill_type_to_string fill_type)))
      (if (null-pointer? c_string)
          (error ("Invalid fill type for object ~A.") object)
          (let ((fill-type (string->symbol (pointer->string c_string))))
            (if (fill-type? fill-type)
                fill-type
                (error ("Unsupported fill type for object ~A: ~A." object fill-type)))))))

  (define pointer (geda-object->pointer* object 1 fillable? 'fillable))

  (let ((fill-type
         (fill-type->symbol (lepton_object_get_fill_type pointer)))
        (width (lepton_object_get_fill_width pointer))
        (pitch1 (lepton_object_get_fill_pitch1 pointer))
        (angle1 (lepton_object_get_fill_angle1 pointer))
        (pitch2 (lepton_object_get_fill_pitch2 pointer))
        (angle2 (lepton_object_get_fill_angle2 pointer)))

    (unless (fill-type? fill-type)
      (error "Object ~A has invalid fill style ~A"
             object fill-type))

    (case fill-type
      ((mesh) (list fill-type width pitch1 angle1 pitch2 angle2))
      ((hatch) (list fill-type width pitch1 angle1))
      (else (list fill-type)))))

(define* (set-object-fill! object
                           type
                           #:optional
                           width
                           space1
                           angle1
                           space2
                           angle2)
  "Sets the fill properties of OBJECT.  If OBJECT is not a box,
circle, or path, throws a Scheme error.  The TYPE parameter
defines the type of the filling which can be 'hollow, 'solid,
'hatch, or 'mesh.  Optional arguments specify properties of
two possible filling strokes if 'hatch or 'mesh types are
used. WIDTH defines the width of the strokes, ANGLE1 and SPACE1
define the angle and pitch of the first hatch lines, ANGLE2 and
SPACE2 define the angle and pitch of the second hatch lines.  The
second hatch is used for the 'mesh type only.  Returns OBJECT."
  (define with-first-hatch?
    (or (eq? type 'mesh)
        (eq? type 'hatch)))

  (define with-second-hatch?
    (eq? type 'mesh))

  (define pointer (geda-object->pointer* object 1 fillable? 'fillable))

  (unless (fill-type? type)
    (error "Invalid fill style ~A." type))

  (when with-first-hatch?
    (unless width
      (error "Missing stroke width parameter for fill style ~A."
             width))
    (check-integer width 3)

    (unless space1
      (error "Missing space parameter for fill style ~A."
             space1))
    (check-integer space1 4)

    (unless angle1
      (error "Missing angle parameter for fill style ~A."
             angle1))
    (check-integer angle1 5))

  (when with-second-hatch?
    (unless space2
      (error "Missing second space parameter for fill style ~A."
             space2))
    (check-integer space2 6)

    (unless angle2
      (error "Missing second angle parameter for fill style ~A."
             angle2))
    (check-integer angle2 7))

  (let ((type (lepton_fill_type_from_string
               (string->pointer (symbol->string type))))
        (width (or width -1))
        (space1 (or space1 -1))
        (angle1 (or angle1 -1))
        (space2 (or space2 -1))
        (angle2 (or angle2 -1)))

    (lepton_object_set_fill_options pointer
                                    type
                                    width
                                    space1
                                    angle1
                                    space2
                                    angle2)
    (lepton_object_page_set_changed pointer)

    object))

;;;; Object bounds

;;; Get bounds of one object.
(define (one-object-bounds object)
  (define pointer (geda-object->pointer* object 1))
  (define xmin (make-bytevector (sizeof int)))
  (define ymin (make-bytevector (sizeof int)))
  (define xmax (make-bytevector (sizeof int)))
  (define ymax (make-bytevector (sizeof int)))

  (let ((result (lepton_object_calculate_visible_bounds
                 pointer
                 ;; If to include hidden text objects.
                 1
                 (bytevector->pointer xmin)
                 (bytevector->pointer ymin)
                 (bytevector->pointer xmax)
                 (bytevector->pointer ymax))))
    (and (= result 1)
         (cons (cons (bytevector-sint-ref xmin 0 (native-endianness) (sizeof int))
                     (bytevector-sint-ref ymax 0 (native-endianness) (sizeof int)))
               (cons (bytevector-sint-ref xmax 0 (native-endianness) (sizeof int))
                     (bytevector-sint-ref ymin 0 (native-endianness) (sizeof int)))))))


(define-public (fold-bounds . bounds)
  (fold
   (lambda (a b)
     (if (and a b)
         ;; Calculate bounds.
         (cons (cons (min (caar a) (caar b))    ; left
                     (max (cdar a) (cdar b)))   ; top
               (cons (max (cadr a) (cadr b))    ; right
                     (min (cddr a) (cddr b))))  ; bottom

         ;; Return whichever isn't #f.
         (or a b)))
   ;; Default.
   #f
   bounds))

(define (object-bounds . objects)
  "Returns the bounds of the objects in the variable-length
argument list OBJECTS.  The bounds are returned as a pair
structure of the form: ((left . top) . (right . bottom)).  If the
list OBJECTS is empty, or none of the objects has any
bounds (e.g. because they are all empty components and/or text
strings), returns #f.  Warning: This function always returns the
actual bounds of the objects, not the visible bounds."
  (apply fold-bounds (map one-object-bounds objects)))


;;;; Object transformations

(define-public (translate-objects! vector . objects)
  (for-each
   (lambda (x) (translate-object! x (car vector) (cdr vector)))
   objects)
  objects)

(define-public (rotate-objects! center angle . objects)
  (for-each
   (lambda (x) (rotate-object! x (car center) (cdr center) angle))
   objects)
  objects)

(define-public (mirror-objects! x . objects)
  (for-each
   (lambda (obj) (mirror-object! obj x))
   objects)
  objects)


(define (object-selectable? object)
  "Checks the state of OBJECT's selectable flag: if it's true, the
object is considered to be unlocked, otherwise it is locked.
Returns #t if OBJECT is selectable, otherwise returns #f."
  (define pointer (geda-object->pointer* object 1))

  (true? (lepton_object_get_selectable pointer)))


(define (set-object-selectable! object selectable?)
  "Sets OBJECT's selectable flag to SELECTABLE?, thus locking or
unlocking OBJECT.  Locked objects cannot be selected in GUI.
Returns OBJECT."
  (define pointer (geda-object->pointer* object 1))

  (unless (eq? (object-selectable? object) selectable?)
    (lepton_object_set_selectable pointer (if selectable? 1 0))
    (lepton_object_page_set_changed pointer))

  object)



(define (object-embedded? object)
  "Check whether OBJECT is embedded."
  (define pointer (geda-object->pointer* object 1))

  (or (and (component? object)
           (true? (lepton_component_object_get_embedded pointer)))
      (and (picture? object)
           (true? (lepton_picture_object_get_embedded pointer)))))


(define (set-object-embedded! object embed?)
  "Unembeds OBJECT if EMBED? is #f, otherwise embeds it.  OBJECT
must be a component or a picture, otherwise the procedure does
nothing.  Returns OBJECT."
  (define (embed-func embed unembed)
    (if embed? embed unembed))

  (define (embed-component x)
    ((embed-func lepton_component_object_embed
                 lepton_component_object_unembed) x))

  (define (embed-picture x)
    ((embed-func lepton_picture_object_embed
                 lepton_picture_object_unembed) x))

  (define pointer (geda-object->pointer* object 1))

  (or (and (component? object) (embed-component pointer))
      (and (picture? object) (embed-picture pointer)))

  object)

;;; Mirrors OBJECT using X as x-coordinate of centre of rotation.
;;; Returns OBJECT.
(define (mirror-object! object x)
  (define pointer (geda-object->pointer* object 1))

  (lepton_object_emit_pre_change_notify pointer)
  (lepton_object_mirror x 0 pointer)
  (lepton_object_emit_change_notify pointer)
  (lepton_object_page_set_changed pointer)

  object)


;;; Rotates OBJECT anti-clockwise by ANGLE about the coordinate of
;;; centre of rotation specified by X and Y.  ANGLE must be an
;;; integer multiple of 90 degrees.  Returns OBJECT.
(define (rotate-object! object x y angle)
  (define pointer (geda-object->pointer* object 1))

  (let ((angle (euclidean-remainder angle 360)))
    (when (not (zero? (euclidean-remainder angle 90)))
      (error "Wrong rotation angle: ~A" angle))

    (lepton_object_emit_pre_change_notify pointer)
    (lepton_object_rotate x y angle pointer)
    (lepton_object_emit_change_notify pointer)
    (lepton_object_page_set_changed pointer)

    object))

;;; Translate OBJECT by DX in the x-axis and DY in the y-axis.  DX
;;; and DY are integer distances along corresponding axes.
;;; Returns OBJECT.
(define (translate-object! object dx dy)
  (define pointer (geda-object->pointer* object 1))

  (lepton_object_emit_pre_change_notify pointer)
  (lepton_object_translate pointer dx dy)
  (lepton_object_emit_change_notify pointer)
  (lepton_object_page_set_changed pointer)

  object)
