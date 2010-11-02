;; gEDA - GPL Electronic Design Automation
;; libgeda - gEDA's library - Scheme API
;; Copyright (C) 2010 Peter Brett
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

  ; Optional arguments
  #:use-module (ice-9 optargs))

(define-public object-type %object-type)
(define-public object? %object?)

(define-public (object-type? x type)
  (if (object? x)
      (eq? (object-type x) type)
      #f))

(define-public copy-object %copy-object)

(define-public object-color %object-color)
(define-public set-object-color! %set-object-color!)

;;;; Lines

;; line? x
;;
;; Returns #t if x is a gEDA line object.
(define-public (line? l)
  (object-type? l 'line))

;; set-line! l start end [color]
;;
;; Sets the parameters of a line, net or bus object l. start is the
;; new coordinates (x . y) of the start of the line, and end is the
;; new coordinates of the end of the line. The optional color argument
;; is the new colormap index of the line's color. Returns l after
;; modifications.
(define*-public (set-line! l start end #:optional color)
  (%set-line! l
              (car start) (cdr start)
              (car end) (cdr end)
              (if (not color)
                  (object-color l)
                  color)))

;; make-line start end [color]
;;
;; Creates a new line. start is the coordinates (x . y) of the start
;; of the line, and end is the coordinates (x . y) of the end of the
;; line.  The optional color argument is the color index of the color
;; with which to draw the line.  If color is not specified, the
;; default color is used.
(define*-public (make-line start end #:optional color)
  (let ((l (%make-line)))
    (set-line! l start end color)))

;; line-info l
;;
;; Returns the parameters of the line, net or bus l as a list of the
;; form:
;;
;;  ((start-x . start-y) (end-x . end-y) color)
(define-public (line-info l)
  (let ((params (%line-info l)))
    (list (cons (list-ref params 0)
                (list-ref params 1))
          (cons (list-ref params 2)
                (list-ref params 3))
          (list-ref params 4))))

;; line-start l
;;
;; Returns the coordinates (x . y) of the start of the gEDA line, net
;; or bus object l.
(define-public (line-start l)
  (list-ref (line-info l) 0))

;; line-end l
;;
;; Returns the coordinates (x . y) of the end of the gEDA line, net or
;; bus object l.
(define-public (line-end l)
  (list-ref (line-info l) 1))

;;;; Nets

;; net? x
;;
;; Returns #t if x is a gEDA net object.
(define-public (net? l)
  (object-type? l 'net))

;; make-net start end [color]
;;
;; Creates a new net. start is the coordinates (x . y) of the start
;; of the net, and end is the coordinates (x . y) of the end of the
;; net.  The optional color argument is the color index of the color
;; with which to draw the net.  If color is not specified, the
;; default color is used.
(define*-public (make-net start end #:optional color)
  (let ((l (%make-net)))
    (set-line! l start end color)))

;;;; Buses

;; bus? x
;;
;; Returns #t if x is a gEDA bus object.
(define-public (bus? l)
  (object-type? l 'bus))

;; make-bus start end [color]
;;
;; Creates a new bus. start is the coordinates (x . y) of the start
;; of the bus, and end is the coordinates (x . y) of the end of the
;; bus.  The optional color argument is the color index of the color
;; with which to draw the bus.  If color is not specified, the
;; default color is used.
(define*-public (make-bus start end #:optional color)
  (let ((l (%make-bus)))
    (set-line! l start end color)))

;;;; Boxes

;; box? x
;;
;; Returns #t if x is a gEDA box object.
(define-public (box? l)
  (object-type? l 'box))

;; set-box! b top-left bottom-right [color]
;;
;; Sets the parameters of a box b. top-left is the new coordinates (x
;; . y) of the top left corner of the box, and bottom-right is the new
;; coordinates of the bottom right corner. The optional color argument
;; is the new colormap index of the box's color. Returns b after
;; modifications.
(define*-public (set-box! b top-left bottom-right #:optional color)
  (%set-box! b
             (car top-left) (cdr top-left)
             (car bottom-right) (cdr bottom-right)
             (if (not color)
                 (object-color b)
                 color)))

;; make-box top-left bottom-right [color]
;;
;; Creates a new box. top-left is the coordinates (x . y) of the top
;; left corner of the box, and bottom-right is the coordinates (x . y)
;; of the bottom right corner.  The optional color argument is the
;; color index of the color with which to draw the box.  If color is
;; not specified, the default color is used.
(define*-public (make-box top-left bottom-right #:optional color)
  (let ((l (%make-box)))
    (set-box! l top-left bottom-right color)))

;; box-info b
;;
;; Returns the parameters of the box b as a list of the form:
;;
;;  ((top-left-x . top-left-y) (bottom-right-x . bottom-right-y) color)
(define-public (box-info b)
  (let ((params (%box-info b)))
    (list (cons (list-ref params 0)
                (list-ref params 1))
          (cons (list-ref params 2)
                (list-ref params 3))
          (list-ref params 4))))

;; box-top-left b
;;
;; Returns the coordinates (x . y) of the top left of the gEDA box
;; object b.
(define-public (box-top-left l)
  (list-ref (box-info l) 0))

;; box-bottom-right b
;;
;; Returns the coordinates (x . y) of the bottom right of the gEDA box
;; object b.
(define-public (box-bottom-right l)
  (list-ref (box-info l) 1))
