;;; Lepton EDA netlister
;;; Copyright (C) 2017 Lepton EDA Contributors
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
;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
;;; MA 02111-1301 USA.

(define-module (gnetlist hierarchy)
  #:use-module (gnetlist config)
  #:export (hierarchy-create-refdes))

(define (hierarchy-create-refdes basename hierarchy-tag)
  (let ((reverse-refdes-order? (gnetlist-config-ref 'reverse-refdes-order))
        (refdes-separator (gnetlist-config-ref 'refdes-separator)))
    (if hierarchy-tag
        (and basename
             (if reverse-refdes-order?
                 (string-append basename refdes-separator hierarchy-tag)
                 (string-append hierarchy-tag refdes-separator basename)))
        basename)))
