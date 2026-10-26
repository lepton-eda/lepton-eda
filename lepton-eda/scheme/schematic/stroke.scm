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
  #:use-module (rnrs bytevectors)

  #:use-module (lepton ffi boolean)
  #:use-module (lepton gettext)
  #:use-module (lepton log)

  #:use-module (schematic ffi)

  #:export (record-stroke
            translate-stroke))


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


(define %libstroke-initialized #f)


;;; Init the libstroke library once.  If it is absent, log a
;;; message about it.
(define (init-libstroke)
  (if libstroke
      (stroke_init)
      (log! 'message (G_ "No libstroke found.")))
  (set! %libstroke-initialized #t))


(define (record-stroke point)
  "Records new POINT for the stroke.  The point is a pair of the
form (X . Y).  If libstroke is missing, does nothing and returns
#f.  When running for the first time, initializes the library and,
if it is missing, logs a message about it."
  (unless %libstroke-initialized
    (init-libstroke))
  (and libstroke
       (stroke_record (car point) (cdr point))))


(define (translate-stroke)
  "Translates the stroke input into a string and returns a string
consisting of numbers if the stroke has been successfully
translated, or #f if libstroke failed to transform the stroke. The
stroke sequence is to be evaluated in Scheme code into a possible
action by the Guile procedure eval-stroke().  If libstroke is
missing, does nothing and returns #f."
  (define *bv (bytevector->pointer (make-bytevector (sizeof '*) 0) 0))

  (and libstroke
       ;; Return translated stroke.
       (let ((result (stroke_trans *bv)))
         (and (true? result)
              (pointer->string *bv)))))
