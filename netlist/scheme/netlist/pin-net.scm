;;; Lepton EDA netlister
;;; Copyright (C) 2016-2017 gEDA Contributors
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

(define-module (netlist pin-net)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)

  #:export (make-pin-net pin-net?
            pin-net-id set-pin-net-id!
            pin-net-object set-pin-net-object!
            pin-net-name set-pin-net-name!
            set-pin-net-printer!))

(define-record-type <pin-net>
  (make-pin-net id object name)
  pin-net?
  (id pin-net-id set-pin-net-id!)
  (object pin-net-object set-pin-net-object!)
  (name pin-net-name set-pin-net-name!))

;;; Sets default printer for <pin-net>
(set-record-type-printer!
 <pin-net>
 (lambda (record port) (format port "#<pin-net ~A>" (pin-net-id record))))

(define (set-pin-net-printer! format-string . args)
  "Adjust pretty-printing of <pin-net> records.
FORMAT-STRING must be in the form required by the procedure
`format'. The following ARGS may be used:
  'id
  'object
  'name
Any other unrecognized argument will lead to yielding '?' in the
corresponding place.
Example usage:
  (set-pin-net-printer! \"<pin-net-~A (~A)>\" 'id 'name)"
  (set-record-type-printer!
   <pin-net>
   (lambda (record port)
     (apply format port format-string
            (map
             (lambda (arg)
               (match arg
                 ('id (pin-net-id record))
                 ('object (pin-net-object record))
                 ('name (pin-net-name record))
                 (_ #\?)))
             args)))))
