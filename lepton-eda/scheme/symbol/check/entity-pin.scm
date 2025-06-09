;;; Lepton EDA Symbol Checker
;;; Scheme API
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
;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.

(define-module (symbol check entity-pin)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-26)
  #:use-module (symbol check pin)
  #:use-module (symbol check slot)

  #:export-syntax (make-entity-pin entity-pin?
                   entity-pin-object set-entity-pin-object!
                   entity-pin-source set-entity-pin-source!
                   entity-pin-seq set-entity-pin-seq!
                   entity-pin-type set-entity-pin-type!
                   entity-pin-number set-entity-pin-number!
                   entity-pin-label set-entity-pin-label!
                   entity-pin-netname set-entity-pin-netname!
                   entity-pin-attribs set-entity-pin-attribs!)

  #:export (set-entity-pin-printer!
            symbol-pin->entity-pin))

(define-record-type <entity-pin>
  (make-entity-pin object source seq type number label netname attribs)
  entity-pin?
  (object entity-pin-object set-entity-pin-object!)
  (source entity-pin-source set-entity-pin-source!)
  (seq entity-pin-seq set-entity-pin-seq!)
  (type entity-pin-type set-entity-pin-type!)
  (number entity-pin-number set-entity-pin-number!)
  (label entity-pin-label set-entity-pin-label!)
  (netname entity-pin-netname set-entity-pin-netname!)
  (attribs entity-pin-attribs set-entity-pin-attribs!))

;;; Sets default printer for <entity-pin>
(set-record-type-printer!
 <entity-pin>
 (lambda (record port) (format port "#<lepton-entity-pin ~A>" (entity-pin-number record))))

(define (set-entity-pin-printer! format-string . args)
  "Adjust pretty-printing of <entity-pin> records.
FORMAT-STRING must be in the form required by the procedure
`format'. The following ARGS may be used:
  'object
  'source
  'seq
  'type
  'number
  'label
  'netname
  'attribs
Any other unrecognized argument will lead to yielding '?' in the
corresponding place.
Example usage:
  (set-entity-pin-printer! \"<entity-pin-~A:~A>\" 'number 'type)"
  (set-record-type-printer!
   <entity-pin>
   (lambda (record port)
     (apply format port format-string
            (map
             (lambda (arg)
               (match arg
                 ('object (entity-pin-object record))
                 ('source (entity-pin-source record))
                 ('seq (entity-pin-seq record))
                 ('type (entity-pin-type record))
                 ('number (entity-pin-number record))
                 ('label (entity-pin-label record))
                 ('netname (entity-pin-label record))
                 ('attribs (entity-pin-attribs record))
                 (_ #\?)))
             args)))))

(define (symbol-pin->entity-pin symbol-pin slot)
  (define (slot-pin pinseq)
    (list-ref (slot-pins slot)
              (1- (string->number (symbol-pin-seq symbol-pin)))))

  (make-entity-pin
   (symbol-pin-object symbol-pin)
   ;; Source is symbol's slot attribute or, if no slot info
   ;; available, pin's pinnumber= attribute.
   (or slot (assq-ref (symbol-pin-attribs symbol-pin) 'pinnumber))
   (symbol-pin-seq symbol-pin)
   (symbol-pin-type symbol-pin)
   ;; Pin number.
   (if slot
       (and=> (symbol-pin-seq symbol-pin) slot-pin)
       ;; Leave it as is if no slotting info available.
       (symbol-pin-number symbol-pin))
   (symbol-pin-label symbol-pin)
   ;; No netname.
   #f
   (symbol-pin-attribs symbol-pin)))
