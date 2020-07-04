;;; Lepton EDA library - Scheme API
;;; Copyright (C) 2020 Lepton EDA Contributors
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

(define-module (schematic color-map)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (system foreign)

  #:use-module (lepton core gettext)
  #:use-module (lepton log)

  #:export (display-color-map
            display-outline-color-map
            print-color-map))

(define liblepton (dynamic-link "liblepton"))
(define libleptongui (dynamic-link "libleptongui"))

(define colors-count
  (pointer->procedure
   size_t
   (dynamic-func "colors_count" liblepton)
   '()))

(define color-map-id->color
  (pointer->procedure
   '*
   (dynamic-func "lepton_colormap_color_by_id" liblepton)
   (list '* size_t)))

;;; Transforms COLOR-MAP into a list of elements '(id color) where
;;; each color is a string representing the color in a hexadecimal
;;; "#RRGGBB" or "#RRGGBBAA" code. The shorter form is used when
;;; the alpha component is 0xff.
(define (color-map->scm color-map)
  (define (id->color id)
    (let ((color (parse-c-struct (color-map-id->color color-map id)
                                 ;; '(r g b a enabled)
                                 (list uint8 uint8 uint8 uint8 int))))
      (match color
        ((r g b a enabled)
         (list id
               (and (not (zero? enabled))
                    (if (= a #xff)
                        (format #f "#~2,'0x~2,'0x~2,'0x" r g b)
                        (format #f "#~2,'0x~2,'0x~2,'0x~2,'0x" r g b a)))))
        (_ (error "Wrong C color struct: ~S" color)))))

  (map id->color (iota (colors-count))))


(define scm->color-map
  (pointer->procedure
   void
   (dynamic-func "s_color_map_from_scm" liblepton)
   (list '* '* '*)))

(define display-colors
  (dynamic-pointer "display_colors" libleptongui))
(define display-outline-colors
  (dynamic-pointer "display_outline_colors" libleptongui))
(define print-colors
  (dynamic-pointer "print_colors" libleptongui))


(define (check-color-map color-map)
  (or (list? color-map)
      (throw 'wrong-type-arg)))

(define (process-color-map color-map color-array proc-name)
  (if color-map
      (and (catch 'wrong-type-arg
             (lambda ()
               (check-color-map color-map))
             (lambda (key . args)
               (log! 'warning (G_ "~A: color-map must be a list.\n") proc-name)
               #f))
           (not (not (scm->color-map color-array
                                     (scm->pointer color-map)
                                     (string->pointer proc-name)))))
      (color-map->scm color-array)))


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
                     print-colors
                     "print-color-map"))
