;; Lepton EDA Schematic Capture
;; Scheme API
;; Copyright (C) 2015 gEDA Contributors
;; Copyright (C) 2017-2023 Lepton EDA Contributors
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
;;

(define-module (schematic repl)
  #:use-module (ice-9 threads)
  #:use-module (lepton repl)

  #:export (start-repl-in-background-terminal))


(define (start-repl-in-background-terminal)
  (begin-thread (lepton-repl)))
