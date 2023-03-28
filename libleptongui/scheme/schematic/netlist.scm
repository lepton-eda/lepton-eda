;;; Lepton EDA Schematic Capture
;;; Scheme API
;;; Copyright (C) 2018-2020 Lepton EDA Contributors
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

;;; Netlist export actions in schematic editor.

(define-module (schematic netlist)
  #:use-module (lepton log)
  #:use-module (lepton page)
  #:use-module (netlist)
  #:use-module (netlist schematic)
  #:use-module (backend allegro)

  #:export (&netlist-allegro))


;;; FIXME: We have either to somehow define schematic using given
;;; pages, or get schematic files using `lepton-schematic' options
;;; (that is, file names on the command line).
(define (%schematic)
  (make-toplevel-schematic (map page-filename (active-pages)))
)

;;; Allegro backend
(define (&netlist-allegro)
  (catch #t
    (lambda()
      (with-output-to-file "allegro.out"
        (lambda () (allegro* (%schematic))))
      (log! 'message "allegro: the output is written to [allegro.out]"))
    (lambda(ex . args)
      (log! 'warning "allegro: error launching backend ('~a):~%  ~a" ex args))))
