;;; Lepton EDA Schematic Capture
;;; Copyright (C) 1998-2010 Ales Hvezda
;;; Copyright (C) 1998-2013 gEDA Contributors
;;; Copyright (C) 2017-2020 Lepton EDA Contributors
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

(define-module (schematic gui stroke)
  #:use-module (schematic action)

  #:export (eval-stroke))

(define (eval-stroke stroke)
  ;; It's a work-around allowing using "strokes" defined somewhere
  ;; else.  "(assoc stroke (primitive-eval 'strokes))" can also be
  ;; used here, though it is a bit slower ;)
  (let ((action (assoc stroke (@@ (guile-user) strokes))))
    (and action
         (eval-action! (cdr action)))))
