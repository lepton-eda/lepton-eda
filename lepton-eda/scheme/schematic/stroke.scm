;;; Lepton EDA Schematic Capture
;;; Scheme API
;;; Copyright (C) 2026 Lepton EDA Contributors
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


(define-module (schematic stroke)
  #:use-module (system foreign)

  #:use-module (lepton gettext)
  #:use-module (lepton log)

  #:use-module (schematic ffi)

  #:export (init-libstroke))


(define libstroke (false-if-exception (dynamic-link "libstroke")))
(define stroke_init
  (and libstroke
       (pointer->procedure void (dynamic-func "stroke_init" libstroke) '())))
(define stroke_trans
  (and libstroke
       (pointer->procedure int (dynamic-func "stroke_trans" libstroke) '(*))))
(define stroke_record
  (and libstroke
       (pointer->procedure void (dynamic-func "stroke_record" libstroke) (list int int))))

;;; Largest number of points allowed to be sampled.
(define STROKE_MAX_POINTS 10000)


(define (init-libstroke)
  (if libstroke
      (begin
        (stroke_init)
        (x_stroke_init))
      (log! 'message (G_ "No libstroke found."))))
