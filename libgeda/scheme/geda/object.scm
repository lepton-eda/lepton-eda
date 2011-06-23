;; gEDA - GPL Electronic Design Automation
;; libgeda - gEDA's library - Scheme API
;; Copyright (C) 2010-2011 Peter Brett <peter@peter-b.co.uk>
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
;;

(define-module (geda object)

  ; Import C procedures
  #:use-module (geda core smob)
  #:use-module (geda core object)
  #:use-module (geda core complex)

  ; Optional arguments
  #:use-module (ice-9 optargs)

  #:use-module (srfi srfi-1))

(define-public object-type %object-type)
(define-public object? %object?)

(define-public (object-type? x type)
  (if (object? x)
      (eq? (object-type x) type)
      #f))

(define-public copy-object %copy-object)

(define-public object-color %object-color)
(define-public set-object-color! %set-object-color!)

(define-public object-connections %object-connections)

(define-public object-component %object-complex)

;;;; Lines

(define-public (line? l)
  (object-type? l 'line))

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

(define-public (net? l)
  (object-type? l 'net))

(define*-public (make-net start end #:optional color)
  (let ((l (%make-net)))
    (set-line! l start end color)))

;;;; Buses

(define-public (bus? l)
  (object-type? l 'bus))

(define*-public (make-bus start end #:optional color)
  (let ((l (%make-bus)))
    (set-line! l start end color)))

;;;; Pins

(define-public (pin? l)
  (object-type? l 'pin))

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


(define-public (box? l)
  (object-type? l 'box))

(define*-public (set-box! b top-left bottom-right #:optional color)
  (%set-box! b
             (car top-left) (cdr top-left)
             (car bottom-right) (cdr bottom-right)
             (if (not color)
                 (object-color b)
                 color)))

(define*-public (make-box top-left bottom-right #:optional color)
  (let ((l (%make-box)))
    (set-box! l top-left bottom-right color)))

(define-public (box-info b)
  (let ((params (%box-info b)))
    (list (cons (list-ref params 0)
                (list-ref params 1))
          (cons (list-ref params 2)
                (list-ref params 3))
          (list-ref params 4))))

(define-public (box-top-left l)
  (list-ref (box-info l) 0))

(define-public (box-bottom-right l)
  (list-ref (box-info l) 1))

;;;; Circles

(define-public (circle? c)
  (object-type? c 'circle))

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

(define-public (arc? a)
  (object-type? a 'arc))

(define*-public (set-arc! a center radius start-angle end-angle
                          #:optional color)
  (%set-arc! a
             (car center) (cdr center)
             radius start-angle end-angle
             (if (not color)
                 (object-color a)
                 color)))

(define*-public (make-arc center radius start-angle end-angle #:optional color)
  (let ((c (%make-arc)))
    (set-arc! c center radius start-angle end-angle color)))

(define-public (arc-info c)
  (let* ((params (%arc-info c))
         (tail (cddr params)))
    (cons (cons (list-ref params 0)
                (list-ref params 1))
          tail)))

(define-public (arc-center a)
  (list-ref (arc-info a) 0))

(define-public (arc-radius a)
  (list-ref (arc-info a) 1))

(define-public (arc-start-angle a)
  (list-ref (arc-info a) 2))

(define-public (arc-end-angle a)
  (list-ref (arc-info a) 3))

;;;; Paths

(define-public (path? x)
  (object-type? x 'path))

;;;; Pictures

(define-public (picture? x)
  (object-type? x 'picture))

;;;; Text

;; text? t
;;
;; Returns #t if t is a gEDA text object.
(define-public (text? t)
  (object-type? t 'text))

;; set-text! t anchor align angle string size visible show [color]
;;
;; Sets the parameters of a text object.  anchor is the point (x . y)
;; at which the text is anchored.  align is the position of the anchor
;; relative to the text, and must be one of the following symbols:
;;
;;   lower-left
;;   middle-left
;;   upper-left
;;   lower-center
;;   middle-center
;;   upper-center
;;   lower-right
;;   middle-right
;;   upper-right
;;
;; string is the new value of the text object.  size is the font size.
;; If visible is #f, the text object will be flagged as invisible;
;; otherwise, it will be visible.  When the text is an attribute, show
;; determines which parts of the string will be displayed, and must be
;; one of the following symbols:
;;
;;   name
;;   value
;;   both
;;
;;  The optional color argument is the colormap index of the color
;;  with which to draw the text.  If color is not specified, the
;;  default color is used.
(define*-public (set-text! t anchor align angle string size visible show
                           #:optional color)
  (%set-text! t (car anchor) (cdr anchor) align angle string size visible show
              (if (not color) (object-color t) color)))

;; make-text! anchor align angle string size visible show [color]
;;
;; Create a new text object.  See set-text! for description of arguments.
(define*-public (make-text . args)
  (let ((t (%make-text)))
    (apply set-text! t args)))

;; text-info t
;;
;; Returns the parameters of the text object t as a list of the form:
;;
;;   ((anchor-x . anchor-y) align angle string size visible show color)
;;
;; See set-text! for description of these parameters.
(define-public (text-info t)
  (let* ((params (%text-info t))
         (tail (cddr params)))
    (cons (cons (list-ref params 0)
                (list-ref params 1))
          tail)))

;; text-anchor t
;;
;; Returns the anchor point of the text object t.
(define-public (text-anchor t)
  (list-ref (text-info t) 0))

;; text-align t
;;
;; Returns the text alignment of the text object t.  The returned
;; value will be one of the following symbols:
;;
;;   lower-left
;;   middle-left
;;   upper-left
;;   lower-center
;;   middle-center
;;   upper-center
;;   lower-right
;;   middle-right
;;   upper-right
(define-public (text-align t)
  (list-ref (text-info t) 1))

;; text-angle t
;;
;; Returns the angle of the text object t.
(define-public (text-angle t)
  (list-ref (text-info t) 2))

;; text-string t
;;
;; Returns the string contained in the text object t.
(define-public (text-string t)
  (list-ref (text-info t) 3))

;; set-text-string! t str
;;
;; Set the string contained by the text object t.
(define-public (set-text-string! t str)
  (let ((i (text-info t)))
    (list-set! i 3 str)
    (apply set-text! t i)))

;; text-size t
;;
;; Returns the font size of the text object t.
(define-public (text-size t)
  (list-ref (text-info t) 4))

;; text-visible? t
;;
;; Returns #t if the text object t is set to be visible.
(define-public (text-visible? t)
  (list-ref (text-info t) 5))

;; set-text-visibility! t visible
;;
;; If visible is #f, sets object t to be invisible; otherwise, sets t
;; to be visible.
(define-public (set-text-visibility! t visible)
  (let ((i (text-info t)))
    (list-set! i 5 (not (not visible)))
    (apply set-text! t i)))

;; text-attribute-mode t
;;
;; Returns the visibility mode of the text object t when the string
;; contained in t is a valid attribute.  The returned value will be
;; one of the following symbols:
;;
;;   name
;;   value
;;   both
(define-public (text-attribute-mode t)
  (list-ref (text-info t) 6))

;;;; Component objects
;;
;; In the gEDA source code, these are normally called "complex"
;; objects.  However, as Guile supports complex numbers, and the
;; procedures related to working with complex numbers use the word
;; "complex" to describe them, this API uses "component" in order to
;; remove the ambiguity.

;; component? c
;;
;; Returns #t if c is a gEDA component object.
(define-public (component? c)
  (object-type? c 'complex))

;; set-component! c position angle mirror locked
;;
;; Sets the parameters of a component object c.  position is the point
;; (x . y) at which the component object is located.  angle is the
;; rotation angle of the component object in degrees, and must be 0, 90,
;; 180, or 270.  If mirror is true, the component object will be
;; flipped, and if locked is true, it will be non-selectable in an
;; editor.
(define-public (set-component! c position angle mirror locked)
  (%set-complex! c (car position) (cdr position) angle mirror locked))

;; make-component basename position angle mirror locked
;;
;; Make a new, empty embedded component object with the given basename
;; and parameters.  See set-component! for full description of
;; arguments.
(define-public (make-component basename . args)
  (let ((c (%make-complex basename)))
    (apply set-component! c args)))

;; make-component/library basename position angle mirror locked
;;
;; Make a new component object by searching the component library for
;; the given basename, and instatiate it with the given parameters.
;; See set-component! for full description of arguments.  Returns #f
;; if basename was not found in the component library.  The component
;; is not initially embedded.
(define-public (make-component/library basename . args)
  (let ((c (%make-complex/library basename)))
    (if c (apply set-component! c args) #f)))

;; component-info c
;;
;; Returns the parameters of the component object c as a list of the
;; form:
;;
;; (basename (x . y) angle mirror locked)
(define-public (component-info c)
  (let* ((params (%complex-info c))
         (tail (list-tail params 3))
         (position (list-tail params 1)))
    (set-car! position (cons (list-ref position 0)
                             (list-ref position 1)))
    (set-cdr! position tail)
    params))

;; component-basename c
;;
;; Returns the basename of the component object c.
(define-public (component-basename c)
  (list-ref (component-info c) 0))

;; component-position c
;;
;; Returns the position of the component object c.
(define-public (component-position c)
  (list-ref (component-info c) 1))

;; component-angle c
;;
;; Returns the rotation angle of the component object c.
(define-public (component-angle c)
  (list-ref (component-info c) 2))

;; component-mirror? c
;;
;; Returns #t if the component object c is mirrored.
(define-public (component-mirror? c)
  (list-ref (component-info c) 3))

;; component-locked? c
;;
;; Returns #t if the component object c is non-selectable.
(define-public (component-locked? c)
  (list-ref (component-info c) 4))

;; component-contents c
;;
;; Returns a list of the primitive objects which make up the component
;; object c.
(define-public component-contents %complex-contents)

;; component-append! c obj ...
;;
;; Adds obj (and any additional objects) to the primitive objects of
;; the component c. Returns c.
(define-public (component-append! component . objects)
  (for-each (lambda (x) (%complex-append! component x)) objects)
  component)

;; component-remove! c obj ...
;;
;; Adds obj (and any additional objects) from the primitive objects of
;; the component c. Returns c.
(define-public (component-remove! component . objects)
  (for-each (lambda (x) (%complex-remove! component x)) objects)
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

(define-public object-bounds %object-bounds)

(define-public (fold-bounds . bounds)
  (fold
   (lambda (a b)
     (if (and a b)
         ;; calc bounds
         (cons (cons (min (caar a) (caar b))    ; left
                     (max (cdar a) (cdar b)))   ; top
               (cons (max (cadr a) (cadr b))    ; right
                     (min (cddr a) (cddr b)))) ; bottom

         ;; return whichever isn't #f
         (or a b)))
   #f ;; default
   bounds))
