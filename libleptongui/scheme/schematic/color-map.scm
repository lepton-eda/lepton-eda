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
  #:use-module (system foreign)

  #:use-module (lepton core gettext)
  #:use-module (lepton log)

  #:export (display-color-map
            display-outline-color-map
            print-color-map))

(define liblepton (dynamic-link "liblepton"))
(define libleptongui (dynamic-link "libleptongui"))

(define color-map->scm
  (pointer->procedure
   '*
   (dynamic-func "s_color_map_to_scm" liblepton)
   (list '*)))

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
      (pointer->scm (color-map->scm color-array))))


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
