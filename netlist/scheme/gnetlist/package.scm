;;; Lepton EDA netlister
;;; Copyright (C) 2016-2017 gEDA Contributors
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

(define-module (gnetlist package)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:export (make-package package?
            package-id set-package-id!
            package-refdes set-package-refdes!
            package-tag set-package-tag!
            package-composite? set-package-composite!
            package-object set-package-object!
            package-iattribs set-package-iattribs!
            package-attribs set-package-attribs!
            package-pins set-package-pins!
            package-attributes
            package-attribute
            package-graphical?
            set-package-printer!))

(define-record-type <package>
  (make-package id refdes tag composite object iattribs attribs pins)
  package?
  (id package-id set-package-id!)
  (refdes package-refdes set-package-refdes!)
  (tag package-tag set-package-tag!)
  (composite package-composite? set-package-composite!)
  (object package-object set-package-object!)
  (iattribs package-iattribs set-package-iattribs!)
  (attribs package-attribs set-package-attribs!)
  (pins package-pins set-package-pins!))

;;; Sets default printer for <package>
(set-record-type-printer!
 <package>
 (lambda (record port) (format port "#<geda-package ~A>" (package-id record))))

(define (set-package-printer! format-string . args)
  "Adjust pretty-printing of <package> records.
FORMAT-STRING must be in the form required by the procedure
`format'. The following ARGS may be used:
  'id
  'refdes
  'tag
  'composite
  'object
  'iattribs
  'attribs
  'pins
Any other unrecognized argument will lead to yielding '?' in the
corresponding place.
Example usage:
  (set-package-printer! \"<package-~A (~A)>\" 'id 'refdes)"
  (set-record-type-printer!
   <package>
   (lambda (record port)
     (apply format port format-string
            (map
             (lambda (arg)
               (match arg
                 ('id (package-id record))
                 ('refdes (package-refdes record))
                 ('tag (package-tag record))
                 ('composite (package-composite? record))
                 ('object (package-object record))
                 ('iattribs (package-iattribs record))
                 ('attribs (package-attribs record))
                 ('pins (package-pins record))
                 (_ #\?)))
             args)))))


(define (package-attributes package name)
  "Returns the list of attached attributes called NAME for
PACKAGE. NAME must be a Scheme symbol (not string). If no attached
attributes found, returns the list of inherited attributes with
the same name. If neither attached nor inherited attributes have
been found, returns #f."
  (or (assq-ref (package-attribs package) name)
      (assq-ref (package-iattribs package) name)))


(define (package-attribute package name)
  "Returns first attached attribute of PACKAGE called NAME. NAME
must be a Scheme symbol (not string). If no attached attribute
found, returns first inherited attribute with NAME. If neither
attached nor inherited attribute found, returns #f."
  (and=> (package-attributes package name) car))

(define (package-graphical? package)
  "Returns #t if PACKAGE is graphical, that is, it has attribute
\"graphical=1\", otherwise returns #f."
  (and=> (package-attribute package 'graphical)
         (lambda (x) (string=? x "1"))))
