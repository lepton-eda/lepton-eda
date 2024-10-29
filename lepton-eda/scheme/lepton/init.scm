;;; Lepton EDA library - Scheme API
;;; Copyright (C) 2024-2025 Lepton EDA Contributors
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


(define-module (lepton init)
  #:use-module (lepton ffi)

  #:export (init-liblepton))

(define (init-liblepton)
  "Perform runtime initialization of liblepton library.  This
function is responsible for making sure that any runtime
initialization is done for all the liblepton routines. It should
be called before any other liblepton functions are called."
  (liblepton_init)
  (unless (getenv "LEPTON_INHIBIT_RC_FILES")
    (register-data-dirs)))
