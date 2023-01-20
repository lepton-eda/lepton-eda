;;; Lepton EDA Schematic Capture
;;; Scheme API
;;; Copyright (C) 2022-2023 Lepton EDA Contributors
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


(define-module (schematic window global)
  #:use-module (lepton ffi boolean)
  #:use-module (lepton toplevel)

  #:use-module (schematic ffi)
  #:use-module (schematic window foreign)

  #:export (%lepton-window
            current-window
            *current-window
            with-window
            in-action?))


;;; This is a fluid that is initialized with pointer to a new
;;; lepton-schematic window when it is created.  Any Scheme
;;; callback procedure called inside the window may use the value
;;; of the fluid to reference its window, thus avoiding the need
;;; of any additional arguments.  In any window, the fluid points
;;; exactly to it.
(define %lepton-window (make-fluid))


;;; Execute forms in the dynamic context of WINDOW and its
;;; toplevel.  We have to dynwind LeptonToplevel here as well
;;; since there are functions that depend on it and should know
;;; what its current value is.
(define-syntax-rule (with-window window form form* ...)
  (with-fluids ((%lepton-window window)
                (%lepton-toplevel
                 (gschem_toplevel_get_toplevel window)))
    form form* ...))


(define (current-window)
  "Returns the <window> instance associated with the current
dynamic context."
  (and=> (fluid-ref %lepton-window) pointer->window))


;;; This macro checks if the current window is available and
;;; produces a C pointer to its instance applicable for use in FFI
;;; code.
(define-syntax *current-window
  (syntax-rules ()
    ((_)
     (let ((*window (and=> (current-window) window->pointer)))
       (or *window
           (error "Current window is unavailable."))))))


(define* (in-action? #:optional (window #f))
  "Return #t if an object editing action is underway in the
current window, and #f otherwise.  If optional WINDOW argument is
specified, the result corresponds to the state of that window
instead of the current one."
  (define (window-in-action? win)
    (true? (schematic_window_get_inside_action (window->pointer win))))

  (window-in-action? (or window (current-window))))
