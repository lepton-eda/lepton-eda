;;; Lepton EDA netlister
;;; Copyright (C) 2017-2018 Lepton EDA Contributors
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

(define-module (netlist rename)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-69)

  #:use-module (geda object)

  #:use-module (netlist core gettext)

  #:use-module (netlist attrib compare)
  #:use-module (netlist net)
  #:use-module (netlist package-pin)
  #:use-module (netlist schematic toplevel)
  #:use-module (netlist schematic)
  #:use-module (netlist schematic-component)
  #:use-module (netlist schematic-connection)
  #:use-module (netlist verbose)

  #:export (get-rename-list))


;; Return the alist of renames.  Sort it so that it's in a canonical
;; order no matter what the internal implementation of the hash table
;; is.
(define (get-rename-list)
  (define (make-special-netname connection hname)
    (let* ((object (car (schematic-connection-objects connection)))
           (coord (line-start object)))
      (create-net-name
       (format #f "unnamed_net_at_~Ax~A" (car coord) (cdr coord))
       hname
       #f)))

  (define (unnamed? name)
    (eq? 'unnamed (car name)))

  (define (name<? a b)
    (refdes<? (schematic-connection-override-name a)
              (schematic-connection-override-name b)))

  (define (net? name)
    (eq? 'net (car name)))

  (define (create-net-name* connection hname)
    (if (unnamed? hname)
        (make-special-netname connection (cdr hname))
        (create-net-name (cadr hname) (cddr hname) (net? hname))))

  (let ((connections (sort (schematic-connections (toplevel-schematic))
                           name<?)))
    (append-map
     (lambda (connection)
       (let ((common-name (schematic-connection-override-name connection))
             (other-names (map (cut create-net-name* connection <>)
                               (cdr (schematic-connection-name connection)))))

         (map (lambda (n) (cons n common-name)) other-names)))
     connections)))
