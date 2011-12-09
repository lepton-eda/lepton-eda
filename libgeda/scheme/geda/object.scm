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
         (list (caar lst) (cdar lst)))))

  (apply %path-insert p idx type (transform-points points '())))


;;;; Pictures

(define-public (picture? x)
  (object-type? x 'picture))

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

(define-public (text? t)
  (object-type? t 'text))

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

(define-public (component? c)
  (object-type? c 'complex))

(define-public (set-component! c position angle mirror locked)
  (%set-complex! c (car position) (cdr position) angle mirror locked))

(define-public (make-component basename . args)
  (let ((c (%make-complex basename)))
    (apply set-component! c args)))

(define-public (make-component/library basename . args)
  (let ((c (%make-complex/library basename)))
    (if c (apply set-component! c args) #f)))

(define-public (component-info c)
  (let* ((params (%complex-info c))
         (tail (list-tail params 3))
         (position (list-tail params 1)))
    (set-car! position (cons (list-ref position 0)
                             (list-ref position 1)))
    (set-cdr! position tail)
    params))

(define-public (component-basename c)
  (list-ref (component-info c) 0))

(define-public (component-position c)
  (list-ref (component-info c) 1))

(define-public (component-angle c)
  (list-ref (component-info c) 2))

(define-public (component-mirror? c)
  (list-ref (component-info c) 3))

(define-public (component-locked? c)
  (list-ref (component-info c) 4))

(define-public component-contents %complex-contents)

(define-public (component-append! component . objects)
  (for-each (lambda (x) (%complex-append! component x)) objects)
  component)

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

;;;; Object transformations

(define-public (translate-objects! vector . objects)
  (for-each
   (lambda (x) (%translate-object! x (car vector) (cdr vector)))
   objects)
  objects)

(define-public (rotate-objects! center angle . objects)
  (for-each
   (lambda (x) (%rotate-object! x (car center) (cdr center) angle))
   objects)
  objects)

(define-public (mirror-objects! x . objects)
  (for-each
   (lambda (obj) (%mirror-object! obj x))
   objects)
  objects)
