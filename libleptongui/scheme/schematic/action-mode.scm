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


(define-module (schematic action-mode)
  #:use-module (system foreign)

  #:use-module (lepton ffi check-args)

  #:use-module (schematic ffi)
  #:use-module (schematic window foreign)
  #:use-module (schematic window global)

  #:export (action-mode->symbol
            symbol->action-mode
            set-action-mode!))

(define (action-mode->symbol mode)
  "Returns a Scheme symbol corresponding to integer MODE value."
  (string->symbol (pointer->string (schematic_action_mode_to_string mode))))

(define (symbol->action-mode sym)
  "Returns integer action mode value corresponding to symbol SYM."
  (schematic_action_mode_from_string (string->pointer (symbol->string sym))))


(define %valid-action-modes
  '(arc-mode
    box-mode
    box-select-mode
    bus-mode
    circle-mode
    component-mode
    copy-mode
    grips-mode
    line-mode
    mirror-mode
    move-mode
    multiple-copy-mode
    net-mode
    pan-mode
    paste-mode
    path-mode
    picture-mode
    pin-mode
    rotate-mode
    select-mode
    text-mode
    zoom-box-mode))


(define-syntax check-action-mode
  (syntax-rules ()
    ((_ mode pos)
     (begin
       (check-symbol mode pos)
       (if (memq mode %valid-action-modes)
           mode
           (scm-error 'misc-error
                      (frame-procedure-name (stack-ref (make-stack #t) 1))
                      "Invalid mode symbol in position ~A: ~A"
                      (list pos mode)
                      #f))))))


(define* (set-action-mode! mode #:key (window (current-window)))
  "Set action mode to MODE.  If WINDOW is specified, the mode is
set in it, otherwise it is set in the currently active window as
returned by the function current-window()."
  (define _mode (check-action-mode mode 1))
  (define *window (window->pointer window))

  (i_set_state *window (symbol->action-mode _mode)))
