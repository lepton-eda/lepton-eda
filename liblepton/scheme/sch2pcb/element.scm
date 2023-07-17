;;; Lepton EDA Schematic to PCB conversion
;;; Scheme API
;;; Copyright (C) 2023-2024 Lepton EDA Contributors
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

(define-module (sch2pcb element)
  #:use-module (system foreign)

  #:use-module (lepton ffi sch2pcb)

  #:export (pcb-element-description
            pcb-element-refdes
            pcb-element-value
            pkg-line->element))


(define (pcb-element-get-string *element *c-getter)
  (define *s (*c-getter *element))
  (if (null-pointer? *s)
      ;; What should the function return if *s is NULL?
      "<null>"
      (pointer->string *s)))

(define (pcb-element-description *element)
  (pcb-element-get-string *element pcb_element_get_description))

(define (pcb-element-refdes *element)
  (pcb-element-get-string *element pcb_element_get_refdes))

(define (pcb-element-value *element)
  (pcb-element-get-string *element pcb_element_get_value))


(define (pkg-line->element *line)
  (pcb_element_pkg_to_element *line))
