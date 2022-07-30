;;; Lepton EDA library - Scheme API
;;; Copyright (C) 2017-2022 Lepton EDA Contributors
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

;;; Helpers for working with text objects.

(define-module (lepton object text)
  #:use-module (ice-9 match)
  #:use-module (system foreign)

  #:use-module (lepton ffi check-args)
  #:use-module (lepton ffi)

  #:export (check-text-alignment
            check-text-angle
            check-text-show
            check-text-attribute-show-mode
            check-text-alignment-symbol
            symbol->text-alignment
            symbol->text-attribute-show-mode
            text-visibility->integer))

;;; Returns text alignment symbol SYM if it is valid.  Otherwise
;;; returns #f.
(define (check-text-alignment-symbol sym)
  (match sym
    ((or 'upper-left 'upper-center 'upper-right
         'middle-left 'middle-center 'middle-right
         'lower-left 'lower-center 'lower-right)
     sym)
    (_ #f)))

(define (check-text-alignment align pos)
  (check-symbol align pos)
  (unless (check-text-alignment-symbol align)
    (error "Invalid text alignment: ~A." align)))


;;; Returns text attribute show mode symbol SYM if it is valid.
;;; Otherwise returns #f.
(define (check-text-attribute-show-mode sym)
  (match sym
    ((or 'name 'value 'both) sym)
    (_ #f)))

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
