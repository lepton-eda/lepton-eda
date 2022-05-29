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

(define-module (symbol check net-attrib)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-26)

  #:use-module (lepton attrib)
  #:use-module (lepton gettext)
  #:use-module (lepton object)
  #:use-module (symbol blame)
  #:use-module (symbol check duplicate)
  #:use-module (symbol check entity-pin)
  #:use-module (symbol check slot)

  #:export-syntax (make-net-map net-map?
                   net-map-object set-net-map-object!
                   net-map-pinnumber set-net-map-pinnumber!
                   net-map-netname set-net-map-netname!)

  #:export (check-net-attrib
            check-duplicates/slot
            make-net-maps))

;;; object is the net= attribute the net information was found in.
(define-record-type <net-map>
  (make-net-map object pinnumber netname)
  net-map?
  (object net-map-object set-net-map-object!)
  (pinnumber net-map-pinnumber set-net-map-pinnumber!)
  (netname net-map-netname set-net-map-netname!))

(define (check-net-attrib-pinnumber object pin)
  (if (string-null? pin)
      (begin
        (blame-object object
                      'error
                      (format #f
                              (G_ "Missing pin number after \":\" or \",\": ~A")
                              (attrib-value object)))
        #f)
      pin))

(define (net-pinnumber<? a b)
  (string<? (net-map-pinnumber a) (net-map-pinnumber b)))

(define (net-pinnumber=? a b)
  (string=? (net-map-pinnumber a) (net-map-pinnumber b)))

(define (check-duplicates/one-net net-maps)
  "Checks for duplicate pinnumbers in NET-MAPS."
  (define (blame-duplicate net-map)
    (let ((net-object (net-map-object net-map)))
      (blame-object net-object
                    'error
                    (format #f
                            (G_ "Duplicate pin number in one net= attribute ~A: ~A")
                            (net-map-pinnumber net-map)
                            (text-string net-object)))))

  (define (blame-if-list ls)
    (if (list? ls)
        (begin (blame-duplicate (car ls))
               (car ls))
        ls))

  (map blame-if-list
       (list->duplicate-list net-maps
                             net-pinnumber<?
                             net-pinnumber=?)))

(define (check-net-attrib net)
  (let* ((value (attrib-value net))
         (colon-position (string-index value #\:)))
    (if colon-position
        (let ((name (string-take value colon-position))
              (pinnumbers (string-split (string-drop value
                                                     (1+ colon-position))
                                        #\,)))
          (check-duplicates/one-net
           (map (lambda (pinnumber) (make-net-map net pinnumber name))
                (filter (cut check-net-attrib-pinnumber net <>) pinnumbers))))
        (begin
          (blame-object net
                        'error
                        (format #f
                                (G_ "Invalid net attribute: net=~A")
                                value))
          '()))))


(define (check-duplicates/net net-maps)
  "Checks for duplicate pinnumbers in NET-MAPS."
  (define (blame-duplicate net-map)
    (let ((net-object (net-map-object net-map)))
      (blame-object net-object
                    'error
                    (format #f
                            (G_ "Duplicate pin number in net= attribute ~A: ~A")
                            (net-map-pinnumber net-map)
                            (text-string net-object)))))

  (define (blame-if-list ls)
    (if (list? ls)
        (begin (for-each blame-duplicate ls)
               (car ls))
        ls))

  (map blame-if-list
       (list->duplicate-list net-maps
                             net-pinnumber<?
                             net-pinnumber=?)))

(define (make-net-maps net-ls)
  (if (null? net-ls)
      '()
      (check-duplicates/net (append-map check-net-attrib net-ls))))

(define (pinnumber<? a b)
  (string<? (entity-pin-number a) (entity-pin-number b)))

(define (pinnumber=? a b)
  (string=? (entity-pin-number a) (entity-pin-number b)))

(define (check-duplicates/slot entity-pins)
  "Checks for duplicate entity pinnumbers in ENTITY-PINS."
  (define (blame-duplicate entity-pin)
    ;; 'source' is either <slot> or pinnumber attribute object for
    ;; not slotted symbols.
    (let* ((source (entity-pin-source entity-pin))
           (source-object (if (slot? source)
                              (slot-object source)
                              source)))
      (blame-object source-object
                    'error
                    (format #f
                            (G_ "Duplicate pin number ~A: ~A")
                            (entity-pin-number entity-pin)
                            (text-string source-object)))))

  (define (blame-if-list ls)
    (if (list? ls)
        (begin
          (for-each blame-duplicate ls)
          (car ls))
        ls))

  (map blame-if-list
       (list->duplicate-list (filter entity-pin-number entity-pins)
                             pinnumber<?
                             pinnumber=?)))
