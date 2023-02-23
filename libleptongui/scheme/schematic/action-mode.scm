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

  #:use-module (schematic ffi)
  #:use-module (schematic window foreign)

  #:export (action-mode->symbol
            symbol->action-mode
            set-action-mode!))

(define (action-mode->symbol mode)
  "Returns a Scheme symbol corresponding to integer MODE value."
  (string->symbol (pointer->string (schematic_action_mode_to_string mode))))

(define (symbol->action-mode sym)
  "Returns integer action mode value corresponding to symbol SYM."
  (schematic_action_mode_from_string (string->pointer (symbol->string sym))))


(define* (set-action-mode! #:window (window #f) mode)
  "Set action mode to MODE.  If WINDOW is specified, the mode is
set in it, otherwise it is set in the currently active window as
returned by the function current-window()."
  (define *window (check-window (or window (current-window)) 1))

  (i_set_state *window (symbol->action-mode mode)))
