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
  #:use-module (ice-9 optargs)
  #:use-module (srfi srfi-1)
  #:use-module (system foreign)
  #:use-module (rnrs bytevectors)

  ;; Import C procedures
  #:use-module (lepton core component)
  #:use-module (lepton core object)

  #:use-module (lepton color-map)
  #:use-module (lepton ffi)
  #:use-module (lepton object type)

  #:export (copy-object
            object-bounds
            object-color
            set-object-color!
            object-component
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


(define-public object-connections %object-connections)


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

(define*-public (make-path #:optional color)
  (let ((p (%make-path)))
    (and color (set-object-color! p color))
    p))

(define-public path-length %path-length)

(define-public (path-ref p idx)

  ;; This recursive function transforms the list of coordinates thus:
  ;;
  ;; (x y ...) --> ((x . y) ...)
  (define (transform-points lst acc)
    (if (null? lst)
        acc
        (transform-points
         (cddr lst)
         (append! acc
                  (list (cons (car lst) (cadr lst)))))))

  (let ((element (%path-ref p idx)))
    (cons (car element)
          (transform-points (cdr element) '()))))

(define-public path-remove! %path-remove!)

(define-public (path-insert! p idx type . points)

  ;; This recursive function transforms the list of coordinates thus:
  ;;
  ;; ((x . y) ...) --> (x y ...)
  (define (transform-points lst acc)
    (if (null? lst)
        acc
        (transform-points
         (cdr lst)
         (append acc (list (caar lst) (cdar lst))))))

  (apply %path-insert p idx type (transform-points points '())))


;;;; Pictures

(define-public (set-picture! p top-left bottom-right angle mirror)
  (%set-picture! p (car top-left) (cdr top-left)
                 (car bottom-right) (cdr bottom-right) angle mirror))

(define-public (make-picture/vector vec filename . args)
  (let ((p (%make-picture)))
    (%set-picture-data/vector! p vec filename)
    (apply set-picture! p args)))

(define-public (picture-info p)
  (let* ((params (%picture-info p))
         (filename (car params))
         (tail (cdr params)))
    (apply list filename
           (cons (list-ref tail 0)
                 (list-ref tail 1))
           (cons (list-ref tail 2)
                 (list-ref tail 3))
           (list-tail tail 4))))

(define-public (picture-filename p)
  (list-ref (picture-info p) 0))

(define-public (picture-top-left p)
  (list-ref (picture-info p) 1))

(define-public (picture-bottom-right p)
  (list-ref (picture-info p) 2))

(define-public (picture-angle p)
  (list-ref (picture-info p) 3))

(define-public (picture-mirror? p)
  (list-ref (picture-info p) 4))


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

  (let ((x (car anchor))
        (y (cdr anchor))
        (align (symbol->text-alignment align))
        (string (string->pointer string))
        (visibility (text-visibility->integer visible?))
        (show (symbol->text-attribute-show-mode show))
        (color (or color (object-color object))))

    ;; Actually make changes
    (lepton_object_emit_pre_change_notify pointer)

    (lepton_text_object_set_x pointer x)
    (lepton_text_object_set_y pointer y)
    (lepton_text_object_set_alignment pointer align)
    (lepton_text_object_set_angle pointer angle)
    (lepton_text_object_set_size pointer size)
    (lepton_text_object_set_visibility pointer visibility)
    (lepton_text_object_set_show pointer show)

    (lepton_object_emit_change_notify pointer)

    (lepton_text_object_set_string pointer string)

    (lepton_text_object_recreate pointer)

    ;; Color
    (lepton_object_set_color pointer color)

    (lepton_object_page_set_changed pointer)

    ;; Return the same object.
    object))


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
  (let ((i (text-info object)))
    (list-set! i 3 str)
    (apply set-text! object i)))

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
  (let ((i (text-info object)))
    (list-set! i 5 (not (not visible?)))
    (apply set-text! object i)))

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

(define-public (make-component basename . args)
  (let ((c (%make-component basename)))
    (apply set-component! c args)))

(define-public (make-component/library basename . args)
  (let ((c (%make-component/library basename)))
    (if c (apply set-component! c args) #f)))

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

(define-public (component-filename c)
  (%component-filename c))

(define-public (component-position c)
  (list-ref (component-info c) 1))

(define-public (component-angle c)
  (list-ref (component-info c) 2))

(define-public (component-mirror? c)
  (list-ref (component-info c) 3))

(define-public (component-locked? c)
  (list-ref (component-info c) 4))

(define-public component-contents %component-contents)

(define-public (component-append! component . objects)
  (for-each (lambda (x) (%component-append! component x)) objects)
  component)

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
