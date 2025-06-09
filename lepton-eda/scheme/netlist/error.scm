;;; Lepton EDA netlister
;;; Copyright (C) 2018 Lepton EDA Contributors
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

(define-module (netlist error)
  #:export (netlist-error))

(define (netlist-error exit-code . args)
  "Formats output to the 'current-error-port' using ARGS and exits
with EXIT-CODE. ARGS are the same as for 'format' procedure
excluding the first one (port)."
  (unless (null? args)
    (apply format (current-error-port) args))
  (primitive-exit exit-code))
