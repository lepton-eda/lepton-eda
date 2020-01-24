;; Lepton EDA Schematic Capture
;; Scheme API
;; Copyright (C) 2015 gEDA Contributors
;; Copyright (C) 2017 Lepton EDA Contributors
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
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111 USA
;;

(define-module (gschem repl)
  #:use-module (ice-9 threads)
  #:use-module (lepton repl))

(define-public (start-repl-in-background-terminal)
  (begin-thread (lepton-repl)))
