;;; Lepton EDA Schematic Capture
;;; Scheme API
;;; Copyright (C) 2021-2025 Lepton EDA Contributors
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

(define-module (schematic rc)
  #:use-module (system foreign)

  #:use-module (lepton ffi)

  #:use-module (schematic ffi)

  #:export (parse-gschemrc))


(define toplevel-initialized? #f)

(define (parse-gschemrc *toplevel)
  "Loads old (system, user, etc.) \"gschemrc\" files and new
configuration \".conf\" files.  Saves the values in the foreign
LeptonToplevel structure *TOPLEVEL and returns it.  Instead of
exiting on error as CLI tools do, displays error dialogs with
explanatory messages."
  (unless toplevel-initialized?
    (g_rc_parse_handler *toplevel
                        (string->pointer "gschemrc")
                        %null-pointer
                        (procedure->pointer void x_rc_parse_gschem_error '(* *))
                        *toplevel)
    (set! toplevel-initialized? #t))
  *toplevel)
