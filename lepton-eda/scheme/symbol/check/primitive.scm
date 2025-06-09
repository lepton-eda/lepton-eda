;;; Lepton EDA Symbol Checker
;;; Scheme API
;;; Copyright (C) 2017-2022 Lepton EDA Contributors
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

(define-module (symbol check primitive)
  #:use-module (lepton attrib)
  #:use-module (lepton gettext)
  #:use-module (lepton object)
  #:use-module (symbol blame)
  #:use-module (symbol check alignment)
  #:use-module (symbol check arc)
  #:use-module (symbol check box)
  #:use-module (symbol check bus)
  #:use-module (symbol check circle)
  #:use-module (symbol check component)
  #:use-module (symbol check line)
  #:use-module (symbol check net)
  #:use-module (symbol check path)
  #:use-module (symbol check picture)
  #:use-module (symbol check pin)
  #:use-module (symbol check text)
  #:use-module (symbol check connection)

  #:export (check-primitive))

(define (object-info object)
  "Returns OBJECT info string."
  (case (object-type object)
    ((arc) (arc-info object))
    ((box) (box-info object))
    ((bus net line pin) (line-info object))
    ((circle) (circle-info object))
    ((complex) (component-info object))
    ((path) (path-info object))
    ((picture) (picture-info object))
    ((text) (text-info object))))


(define (check-primitive object)
  "Checks OBJECT.
Carries out appropriate checks for each type of objects. Returns
OBJECT if it needs other checks. Otherwise returns #f."
  (blame-object object
                'info
                (format #f (G_ "Object: ~A") (cons (object-type object)
                                                  (object-info object))))
  ;; Specific checks for every object.
  ;; #f means no other check is needed.
  (case (object-type object)
    ((arc) (check-arc object) #f)
    ((box) (check-box object) #f)
    ((net) (check-net object) #f)
    ((bus) (check-bus object) #f)
    ((circle) (check-circle object) #f)
    ((complex) (check-component object) #f)
    ((line) (check-line object) #f)
    ((path) (check-path object) #f)
    ((picture) (check-picture object) #f)
    ((pin) (check-pin object))
    ((text)
     (check-text object)
     (and (attribute? object) object))
    (else (error "Unknown object ~A" object))))
