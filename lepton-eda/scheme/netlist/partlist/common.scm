;;; Lepton EDA netlister
;;; Copyright (C) 2016 gEDA Contributors
;;; Copyright (C) 2018 Lepton EDA Contributors
;
; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 2 of the License, or
; (at your option) any later version.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this program; if not, write to the Free Software
; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA


(define-module (netlist partlist common)
  #:use-module (srfi srfi-26)
  #:use-module (netlist partlist)
  #:export (make-partlist))

(define (make-partlist packages attrib-list)
  "Transforms PACKAGES into part list based on ATTRIB-LIST. Each
part in the part list has the form:
  (package . attrib-alist)
and each member of attrib-alist has the form:
  (name . value)
where \"name\" is an attrib name converted to a symbol and \"value\"
is the value of the corresponding attribute which must be a string."
  (define get-package-attribute
    (@@ (guile-user) gnetlist:get-package-attribute))

  ;; Gets attributes of PACKAGE which names are given in ATTRIB-LIST
  ;; that must be a list of symbols. Returns a pair consisting of
  ;; PACKAGE and alist of its attributes.
  (define (get-attrib-value name package)
    `(,name . ,(get-package-attribute package
                                      (symbol->string name))))

  (define (get-attribs package attrib-list)
    `(,package . ,(map (cut get-attrib-value <> package)
                       attrib-list)))

  (map (cut get-attribs <> attrib-list) packages))
