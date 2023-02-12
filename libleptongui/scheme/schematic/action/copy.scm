;;; Lepton EDA Schematic Capture
;;; Scheme API
;;; Copyright (C) 2023 Lepton EDA Contributors
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not write to the Free Software
;;; Foundation Inc. 51 Franklin Street Fifth Floor Boston MA 02110-1301 USA.


(define-module (schematic action copy)
  #:use-module (system foreign)

  #:use-module (lepton ffi boolean)

  #:use-module (schematic action-mode)
  #:use-module (schematic ffi)

  #:export (finish-copy))

(define* (finish-copy *window #:optional keep-on?)
  "Finish copy action in *WINDOW."
  (define continue-placement? (if keep-on? TRUE FALSE))

  (o_place_end *window
               (schematic_window_get_second_wx *window)
               (schematic_window_get_second_wy *window)
               continue-placement?
               (string->pointer "paste-objects-hook")))
