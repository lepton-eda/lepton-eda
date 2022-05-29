;;; Lepton EDA netlister
;;; Copyright (C) 2016-2017 gEDA Contributors
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

(define-module (netlist package)
  #:use-module (ice-9 match)
  #:use-module (ice-9 receive)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)

  #:use-module (lepton gettext)
  #:use-module (lepton log)
  #:use-module (netlist schematic-component)
  #:use-module (symbol check duplicate)

  #:export-syntax (make-package package?
                   package-id set-package-id!
                   package-refdes set-package-refdes!
                   package-attribs set-package-attribs!
                   package-components set-package-components!
                   package-pins set-package-pins!)

  #:export (package-attributes
            package-attribute
            package-attribute-string=?
            set-package-printer!
            make-package-list))

(define-record-type <package>
  (make-package id refdes attribs components pins)
  package?
  (id package-id set-package-id!)
  (refdes package-refdes set-package-refdes!)
  (attribs package-attribs set-package-attribs!)
  (components package-components set-package-components!)
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
  'attribs
  'components
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
                 ('attribs (package-attribs record))
                 ('components (package-components record))
                 ('pins (package-pins record))
                 (_ #\?)))
             args)))))


(define (package-attributes package name)
  "Returns the list of attached attributes called NAME for
PACKAGE. NAME must be a Scheme symbol (not string). If no attached
attributes found, returns the list of inherited attributes with
the same name. If neither attached nor inherited attributes have
been found, returns #f."
  (assq-ref (package-attribs package) name))


(define (package-attribute package name)
  "Returns first attached attribute of PACKAGE called NAME. NAME
must be a Scheme symbol (not string). If no attached attribute
found, returns first inherited attribute with NAME. If neither
attached nor inherited attribute found, returns #f."
  (and=> (package-attributes package name) car))


(define (package-attribute-string=? package name value)
  "Returns #t if PACKAGE has attribute NAME equal to VALUE,
otherwise returns #f. NAME must be a symbol, while VALUE should be
a string."
  (and=> (package-attribute package name)
         (lambda (x) (string=? x value))))


(define (make-package-list schematic-component-list)
  "Transforms SCHEMATIC-COMPONENT-LIST into package list."
  (define (schematic-component-refdes<? a b)
    (string<? (schematic-component-refdes a)
              (schematic-component-refdes b)))

  (define (schematic-component-refdes=? a b)
    (string=? (schematic-component-refdes a)
              (schematic-component-refdes b)))

  (define (blame-conflicting-attribs refdes attrib-type name value other-value)
    (log! 'warning
          (G_ "Possible ~A attribute conflict for ~A: ~A=~A ~A=~A\n")
          attrib-type
          refdes
          name
          value
          name
          other-value))

  (define (check-attribs attrib-type ls)
    (let loop ((attribs (if (eq? attrib-type 'inherited)
                            (append-map schematic-component-iattribs ls)
                            (append-map schematic-component-attribs ls)))
               (result '()))
      (if (null? attribs)
          result
          (let* ((attrib (car attribs))
                 (name (car attrib))
                 (value (cdr attrib)))
            (loop (cdr attribs)
                  (if (or (eq? name 'slot)
                          (eq? name 'slotdef)
                          (eq? name 'net))
                      result
                      (let ((other-value (assq-ref result name)))
                        (if other-value
                            (begin
                              (unless (equal? value other-value)
                                (blame-conflicting-attribs (schematic-component-refdes (car ls))
                                                           attrib-type
                                                           name
                                                           value
                                                           other-value))
                              result)
                            (cons attrib result)))))))))

  (define (check-override-attribs inherited attached)
    (let loop ((rest inherited)
               (result attached))
      (if (null? rest)
          result
          (loop (cdr rest)
                (let ((attrib (car rest)))
                  (if (assq-ref result (car attrib))
                      result
                      (cons attrib result)))))))

  (define (get-attribs ls)
    (let ((attrib-defaults (check-attribs 'inherited ls))
          (attrib-overrides (check-attribs 'attached ls)))
      (check-override-attribs attrib-defaults attrib-overrides)))

  (define (merge-component-pins ls)
    (append-map schematic-component-pins ls))

  (define (schematic-components->package ls-or-component)
    (let* ((components (if (list? ls-or-component)
                           ls-or-component
                           (list ls-or-component)))
           (id (schematic-component-id (car components)))
           (refdes (schematic-component-refdes (car components)))
           (attribs (get-attribs components))
           (pins (merge-component-pins components)))
      (make-package id refdes attribs components pins)))

  ;; The first inherited "refdes=" attrib value is considered to
  ;; be the default refdes of COMPONENT.
  (define (schematic-component-default-refdes component)
    (and=> (assq-ref (schematic-component-iattribs component)
                     'refdes)
           car))

  ;; COMPONENT refdes is considered to be default if either there
  ;; is no attached refdes, or there is an attached refdes having
  ;; the suffix "?" and being the same as inherited one.
  (define (is-schematic-component-refdes-default? component)
    (let ((refdes (schematic-component-refdes component))
          (inherited-refdes (schematic-component-default-refdes component)))
      (and inherited-refdes
           (or (not refdes)
               (and refdes
                    (string-suffix? "?" refdes)
                    (string= refdes inherited-refdes))))))

  ;; Check if COMPONENT's refdes is not #f.
  (define (is-schematic-component-refdes-valid? component)
    (schematic-component-refdes component))

  (let ((components (filter is-schematic-component-refdes-valid?
                            schematic-component-list)))
    (receive (default-named-components named-components)
        (partition is-schematic-component-refdes-default? components)
      (map schematic-components->package
           (append
            (list->duplicate-list named-components
                                  schematic-component-refdes<?
                                  schematic-component-refdes=?)
            default-named-components)))))
