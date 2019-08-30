;;; Lepton EDA Schematic Capture
;;; Scheme API
;;; Copyright (C) 2018-19 Lepton EDA Contributors
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
;;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111 USA

;;; Netlist export actions in schematic editor.

(define-module (schematic netlist)
  #:use-module (geda log)
  #:use-module (lepton page)
  #:use-module (netlist)
  #:use-module (netlist schematic)

  #:export (&netlist-allegro))


;;; FIXME: We have either to somehow define schematic using given
;;; pages, or get schematic files using `lepton-schematic' options
;;; (that is, file names on the command line).
(define (%schematic)
  ;; FIXME: Just now only 'geda mode is used.
  (make-toplevel-schematic (map page-filename (active-pages))
                           'geda))

;;; First load allegro backend code in order to use `allegro*'
;;; below.
(let
  ((fpath (%search-load-path "gnet-allegro.scm")))
  (if fpath
    (primitive-load fpath)
    (log! 'warning "allegro: cannot load backend file")))

;;; Allegro backend
(define (&netlist-allegro)
  (catch #t
    (lambda()
      (with-output-to-file "allegro.out"
        (lambda () (allegro* (%schematic))))
      (log! 'message "allegro: the output is written to [allegro.out]"))
    (lambda(ex . args)
      (log! 'warning "allegro: error launching backend ('~a):~%  ~a" ex args))))
