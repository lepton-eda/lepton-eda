;;; Lepton EDA library - Scheme API
;;; Copyright (C) 1998-2016 gEDA Contributors
;;; Copyright (C) 2020-2022 Lepton EDA Contributors
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

;;; Colors in schematic.

(define-module (lepton color-map)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (system foreign)

  #:use-module (lepton ffi)
  #:use-module (lepton gettext)
  #:use-module (lepton log)

  #:export (%color-name-map
            display-color-map
            display-outline-color-map
            print-color-map
            color-map-name-to-index
            color-map-name-from-index))

;;; Map between color index numbers and symbolic color names.
(define %color-name-map
  '((background . 0)
    (pin . 1)
    (net-endpoint . 2)
    (graphic . 3)
    (net . 4)
    (attribute . 5)
    (logic-bubble . 6)
    (dots-grid . 7)
    (detached-attribute . 8)
    (text . 9)
    (bus . 10)
    (select . 11)
    (bounding-box . 12)
    (zoom-box . 13)
    (stroke . 14)
    (lock . 15)
    (output-background . 16)
    (freestyle1 . 17)
    (freestyle2 . 18)
    (freestyle3 . 19)
    (freestyle4 . 20)
    (junction . 21)
    (mesh-grid-major . 22)
    (mesh-grid-minor . 23)))

(define %color-name-reverse-map
  (map (lambda (x) (cons (cdr x) (car x))) %color-name-map))


(define (color-map-name-to-index x)
  "Returns an integer index given one of a color
symbols like 'background, 'net, 'text, etc.
Returns #f (false) if wrong symbol is specified."
  (assq-ref %color-name-map x))


;; Look up the symbolic color for an internal system ID.
(define (color-map-name-from-index id)
  "Returns one of a color symbols like 'background,
'net, 'text, etc., given an integer index.
Returns #f (false) if wrong index is specified."
  (assq-ref %color-name-reverse-map id))


;;; Transforms COLOR-MAP into a list of elements '(id color) where
;;; each color is a string representing the color in a hexadecimal
;;; "#RRGGBB" or "#RRGGBBAA" code. The shorter form is used when
;;; the alpha component is 0xff (opaque).
(define (color-map->scm color-map)

  (define (id->color id)
    (let ((color (parse-c-struct (lepton_colormap_color_by_id color-map id)
                                 ;; '(r g b a)
                                 (list uint8 uint8 uint8 uint8))))
      (match color
        ((r g b a)
         (list (color-map-name-from-index id)
               (and (not (zero? a))
                    (if (= a #xff)
                        (format #f "#~2,'0x~2,'0x~2,'0x" r g b)
                        (format #f "#~2,'0x~2,'0x~2,'0x~2,'0x" r g b a)))))
        (_ (throw 'wrong-type-arg
                  (G_ "Wrong C color struct: ~S")
                  color)))))

  (map id->color (iota (colors_count))))


;;; Decode a hexadecimal RGB or RGBA color code.
;;;
;;; The function accepts a hexadecimal color code of either the
;;; form #RRGGBB or #RRGGBBAA, parses it to extract the numerical
;;; color values, and returns the list of values '(RR GG BB AA).
;;; If the six-digit form is used, the alpha channel is set to
;;; full opacity. If an error occurs during parsing, the return
;;; value is #f.
(define (color-rgba-decode s)
  ;; Check that the string has a valid length and starts with a
  ;; '#'.
  (define (check-string s)
    (and (string-prefix? "#" s)
         (let ((s (string-drop s 1)))
           (or (and (= 6 (string-length s))
                    (string-append s "ff"))
               (and (= 8 (string-length s))
                    s)))))

  (let ((s (check-string s)))
    (and s
         ;; Conversion with radix 16 will work for proper hex
         ;; chars only.
         (let ((r (string->number (substring s 0 2) 16))
               (g (string->number (substring s 2 4) 16))
               (b (string->number (substring s 4 6) 16))
               (a (string->number (substring s 6 8) 16)))
           (and r g b a (list r g b a))))))

(define (valid-color-id? id)
  (and (>= id 0)
       (< id (colors_count))))

(define (scm->color-map color-map ls)

  (define (parse-entry entry)
    ;; Check if map entry has correct type.
    (when (or (not (list? entry))
              (not (= (length entry) 2)))
      (throw 'wrong-type-arg
             (G_ "Color map entry must be a two-element list: ~S\n")
             entry))
    ;; Check color index has correct type and extract it.
    (let ((id (color-map-name-to-index (car entry)))
          (color (cadr entry)))
      (when (not (integer? id))
        (throw 'wrong-type-arg
               (G_ "Index in color map entry must be an integer or a symbol defined in %color-name-map: ~S\n")
               (car entry)))
      ;; Check color index is within bounds.
      ;; FIXME: one day we will have dynamically-expanding
      ;; colorspace.  One day.
      (if (valid-color-id? id)
          (if color
              (if (string? color)
                  (let ((result (color-rgba-decode color)))
                    (if result
                        (cons id result)
                        (throw 'wrong-type-arg
                               (G_ "Invalid color map value: ~S\n")
                               color)))
                  ;; Color must be a string or #f.
                  (throw 'wrong-type-arg
                         (G_ "Value in color map entry must be #f or a string: ~S\n")
                         color))
              ;; If color value is #f, disable the color.
              (list id #x00 #x00 #x00 #x00))
          (throw 'out-of-range
                 (G_ "Color map index out of bounds: ~A\n")
                 id))))

  (define (process-entry entry)
    (match (parse-entry entry)
      ((id r g b a)
       (if (zero? a)
           (lepton_colormap_disable_color color-map id)
           (lepton_colormap_set_color color-map id r g b a)))
      (_ #f)))

  (for-each process-entry ls))


(define display-colors
  (dynamic-pointer "display_colors" liblepton))
(define display-outline-colors
  (dynamic-pointer "display_outline_colors" liblepton))


(define (process-color-map color-map color-array proc-name)
  (define (check-color-map color-map)
    (or (list? color-map)
        (throw 'wrong-type-arg
               (G_ "Color-map must be a list: ~S")
               color-map)))

  (catch #t
    (lambda ()
      (if color-map
          (and (check-color-map color-map)
               (scm->color-map color-array color-map)
               ;; Return the resulting list.
               (color-map->scm color-array))
          (color-map->scm color-array)))
    (lambda (key . args)
      (log! 'critical "~A: ~A\n" proc-name (apply format #f args))
      #f)))


(define* (display-color-map #:optional color-map)
  (process-color-map color-map
                     display-colors
                     "display-color-map"))

(define* (display-outline-color-map #:optional color-map)
  (process-color-map color-map
                     display-outline-colors
                     "display-outline-color-map"))

(define* (print-color-map #:optional color-map)
  (process-color-map color-map
                     (print_colors_array)
                     "print-color-map"))
