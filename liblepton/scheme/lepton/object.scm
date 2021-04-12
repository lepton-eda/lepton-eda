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
  #:use-module (ice-9 optargs)
  #:use-module (srfi srfi-1)
  #:use-module (system foreign)
  #:use-module (rnrs bytevectors)

  ;; Import C procedures
  #:use-module (lepton core component)
  #:use-module (lepton core object)
  #:use-module (lepton ffi)

  #:export (object?
            object-id
            object-type
            object-type?
            arc?
            box?
            bus?
            circle?
            component?
            line?
            net?
            path?
            picture?
            pin?
            text?
            copy-object
            object-bounds
            object-color
            set-object-color!
            object-component
            object-embedded?
            set-object-embedded!
            object-selectable?
            set-object-selectable!

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
            set-box!))

(define (object? object)
  "Returns #t if OBJECT is a #<geda-object> instance, otherwise
returns #f."
  (true? (edascm_is_object (scm->pointer object))))


(define (object-id object)
  "Returns an internal id number of the OBJECT."
  (let ((id (lepton_object_get_id (geda-object->pointer object))))
    (and (not (= id -1))
         id)))


(define (object-type object)
  "Returns a Scheme symbol representing the type of OBJECT."
  (define pointer (geda-object->pointer* object 1))

  (cond
   ((arc? object) 'arc)
   ((box? object) 'box)
   ((bus? object) 'bus)
   ((circle? object) 'circle)
   ((component? object) 'complex)
   ((line? object) 'line)
   ((net? object) 'net)
   ((path? object) 'path)
   ((picture? object) 'picture)
   ((pin? object) 'pin)
   ((text? object) 'text)
   (else (error "Object ~A has bad type '~A'"
                object
                (integer->char (lepton_object_get_type pointer))))))

(define (object-type? x type)
  (and (object? x)
       (eq? (object-type x) type)))

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

(define (line? object)
  "Returns #t if OBJECT is a line object, otherwise returns #f."
  (true? (lepton_object_is_line (geda-object->pointer object))))

(define*-public (set-line! l start end #:optional color)
  (%set-line! l
              (car start) (cdr start)
              (car end) (cdr end)
              (if (not color)
                  (object-color l)
                  color)))

(define*-public (make-line start end #:optional color)
  (let ((l (%make-line)))
    (set-line! l start end color)))

(define-public (line-info l)
  (let ((params (%line-info l)))
    (list (cons (list-ref params 0)
                (list-ref params 1))
          (cons (list-ref params 2)
                (list-ref params 3))
          (list-ref params 4))))

(define-public (line-start l)
  (list-ref (line-info l) 0))

(define-public (line-end l)
  (list-ref (line-info l) 1))

;;;; Nets

(define-public (net? object)
  "Returns #t if OBJECT is a net object, otherwise returns #f."
  (true? (lepton_object_is_net (geda-object->pointer object))))

(define*-public (make-net start end #:optional color)
  (let ((l (%make-net)))
    (set-line! l start end color)))

;;;; Buses

(define (bus? object)
  "Returns #t if OBJECT is a bus object, otherwise returns #f."
  (true? (lepton_object_is_bus (geda-object->pointer object))))

(define*-public (make-bus start end #:optional color)
  (let ((l (%make-bus)))
    (set-line! l start end color)))

;;;; Pins

(define (pin? object)
  "Returns #t if OBJECT is a pin object, otherwise returns #f."
  (true? (lepton_object_is_pin (geda-object->pointer object))))

(define-public (net-pin? l)
  (and (pin? l) (equal? (%pin-type l) 'net)))

(define-public (bus-pin? l)
  (and (pin? l) (equal? (%pin-type l) 'bus)))

(define*-public (make-net-pin start end #:optional color)
  (let ((l (%make-pin 'net)))
    (set-line! l start end color)))

(define*-public (make-bus-pin start end #:optional color)
  (let ((l (%make-pin 'bus)))
    (set-line! l start end color)))

;;;; Boxes


(define (box? object)
  "Returns #t if OBJECT is a box object, otherwise returns #f."
  (true? (lepton_object_is_box (geda-object->pointer object))))

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
  (let* ((init-color (default_color_id))
         (init-left-x 0)
         (init-upper-y 0)
         (init-right-x 0)
         (init-bottom-y 0)
         (object (pointer->geda-object
                  (lepton_box_object_new init-color
                                         init-left-x
                                         init-upper-y
                                         init-right-x
                                         init-bottom-y))))
    (set-box! object
              top-left
              bottom-right
              (or color init-color))))

(define (box-info object)
  "Retrieves and returns the coordinates and color of a box
OBJECT. The return value is a list of the parameters in the form:
'((upper_x . upper_y) (lower_x . lower_y) color)"
  (list (box-top-left object)
        (box-bottom-right object)
        (object-color object)))

(define (box-top-left object)
  "Returns the top left corner coordinate of box OBJECT."
  (define pointer (geda-object->pointer* object 1))

  (cons (lepton_box_object_get_upper_x pointer)
        (lepton_box_object_get_upper_y pointer)))


(define (box-bottom-right object)
  "Returns the bottom right corner coordinate of box OBJECT."
  (define pointer (geda-object->pointer* object 1))

  (cons (lepton_box_object_get_lower_x pointer)
        (lepton_box_object_get_lower_y pointer)))


;;;; Circles

(define (circle? object)
  "Returns #t if OBJECT is a circle object, otherwise returns #f."
  (true? (lepton_object_is_circle (geda-object->pointer object))))

(define*-public (set-circle! c center radius #:optional color)
  (%set-circle! c
                (car center) (cdr center)
                radius
                (if (not color)
                    (object-color c)
                    color)))

(define*-public (make-circle center radius #:optional color)
  (let ((c (%make-circle)))
    (set-circle! c center radius color)))

(define-public (circle-info c)
  (let* ((params (%circle-info c))
         (tail (cddr params)))
    (cons (cons (list-ref params 0)
                (list-ref params 1))
          tail)))

(define-public (circle-center c)
  (list-ref (circle-info c) 0))

(define-public (circle-radius c)
  (list-ref (circle-info c) 1))

;;;; Arcs

(define (arc? object)
  "Returns #t if OBJECT is a arc object, otherwise returns #f."
  (true? (lepton_object_is_arc (geda-object->pointer object))))

(define* (set-arc! object center radius start-angle sweep-angle
                   #:optional color)
  "Modifies arc OBJECT by setting its parameters to new values and
returns the modified object.  CENTER is the coordinate of the
center of the arc in the form '(x . y). RADIUS, START-ANGLE, and
SWEEP-ANGLE correspondingly represent its radius, start and sweep
angle.  If optional COLOR is specified, it should be the integer
color map index of the color with which to draw the arc.  If COLOR
is not specified, the default arc color is used."
  (define pointer (geda-object->pointer* object 1))

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

  (let* ((init-color (default_color_id))
         (init-center-x 0)
         (init-center-y 0)
         (init-radius 1)
         (init-start-angle 0)
         (init-sweep-angle 0)
         (object (pointer->geda-object
                  (lepton_arc_object_new init-color
                                         init-center-x
                                         init-center-y
                                         init-radius
                                         init-start-angle
                                         init-sweep-angle))))
    (set-arc! object
              center
              radius
              start-angle
              sweep-angle
              (or color init-color))))

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
  (define pointer (geda-object->pointer* object 1))
  (cons (lepton_arc_object_get_center_x pointer)
        (lepton_arc_object_get_center_y pointer)))

(define (arc-radius object)
  "Returns the radius of arc OBJECT as an integer."
  (define pointer (geda-object->pointer* object 1))
  (lepton_arc_object_get_radius pointer))

(define (arc-start-angle object)
  "Returns the start angle of arc OBJECT as an integer number of
degrees."
  (define pointer (geda-object->pointer* object 1))
  (lepton_arc_object_get_start_angle pointer))

(define (arc-sweep-angle object)
  "Returns the sweep angle of arc OBJECT as an integer number of
degrees."
  (define pointer (geda-object->pointer* object 1))
  (lepton_arc_object_get_sweep_angle pointer))

(define (arc-end-angle object)
  "Returns the end angle of arc OBJECT as an integer number of
degrees.  The end angle is the sum of the start and sweep angles
of the arc."
  (+ (arc-start-angle object) (arc-sweep-angle object)))

;;;; Paths

(define (path? object)
  "Returns #t if OBJECT is a path object, otherwise returns #f."
  (true? (lepton_object_is_path (geda-object->pointer object))))

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

(define (picture? object)
  "Returns #t if OBJECT is a picture object, otherwise returns #f."
  (true? (lepton_object_is_picture (geda-object->pointer object))))

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

(define (text? object)
  "Returns #t if OBJECT is a text object, otherwise returns #f."
  (true? (lepton_object_is_text (geda-object->pointer object))))

(define*-public (set-text! t anchor align angle string size visible show
                           #:optional color)
  (%set-text! t (car anchor) (cdr anchor) align angle string size visible show
              (if (not color) (object-color t) color)))

(define*-public (make-text . args)
  (let ((t (%make-text)))
    (apply set-text! t args)))

(define-public (text-info t)
  (let* ((params (%text-info t))
         (tail (cddr params)))
    (cons (cons (list-ref params 0)
                (list-ref params 1))
          tail)))

(define-public (text-anchor t)
  (list-ref (text-info t) 0))

(define-public (text-align t)
  (list-ref (text-info t) 1))

(define-public (text-angle t)
  (list-ref (text-info t) 2))

(define-public (text-string t)
  (list-ref (text-info t) 3))

(define-public (set-text-string! t str)
  (let ((i (text-info t)))
    (list-set! i 3 str)
    (apply set-text! t i)))

(define-public (text-size t)
  (list-ref (text-info t) 4))

(define-public (text-visible? t)
  (list-ref (text-info t) 5))

(define-public (set-text-visibility! t visible)
  (let ((i (text-info t)))
    (list-set! i 5 (not (not visible)))
    (apply set-text! t i)))

(define-public (text-attribute-mode t)
  (list-ref (text-info t) 6))

;;;; Component objects

(define (component? object)
  "Returns #t if OBJECT is a component object, otherwise returns #f."
  (true? (lepton_object_is_component (geda-object->pointer object))))

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

(define-public object-stroke %object-stroke)
(define-public set-object-stroke! %set-object-stroke!)

(define-public (object-stroke-width obj)
  (list-ref (object-stroke obj) 0))

(define-public (object-stroke-cap obj)
  (list-ref (object-stroke obj) 1))

(define-public (object-stroke-dash obj)
  (list-tail (object-stroke obj) 2))

(define-public object-fill %object-fill)
(define-public set-object-fill! %set-object-fill!)

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
