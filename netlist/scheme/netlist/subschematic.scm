;;; Lepton EDA netlister
;;; Copyright (C) 2019 Lepton EDA Contributors
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

;;; Subschematic is schematic of a list of pages usually defined
;;; in the "source=" attributes of a component.

(define-module (netlist subschematic)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)

  #:export-syntax (make-subschematic subschematic?
                   subschematic-name set-subschematic-name!
                   subschematic-pages set-subschematic-pages!
                   subschematic-components set-subschematic-components!
                   subschematic-connections set-subschematic-connections!))

(define-record-type <subschematic>
  (make-subschematic name pages components connections)
  subschematic?
  (name subschematic-name set-subschematic-name!)
  (pages subschematic-pages set-subschematic-pages!)
  (components subschematic-components set-subschematic-components!)
  (connections subschematic-connections set-subschematic-connections!))

;;; Sets default printer for <subschematic>
(set-record-type-printer!
 <subschematic>
 (lambda (record port) (format port
                          "#<subschematic-~A>"
                          (subschematic-name record))))
