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
;;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111 USA

(define-module (symbol check pin-attrib)
  #:use-module (srfi srfi-1)
  #:use-module (geda object)
  #:use-module (geda attrib)
  #:use-module (symbol gettext)
  #:use-module (symbol blame)
  #:use-module (symbol check attrib)
  #:use-module (symbol check duplicate)
  #:use-module (symbol check pin)

  #:export (pin-attrib?
            net-numbers
            check-duplicate-net-pinnumbers
            check-duplicate-net-pinnumber-numbers
            check-pin-attrib-duplicates
            check-duplicates/pinseq
            check-duplicates/pinnumber))

(define (pin-attrib? object)
  "Returns #t if OBJECT is a pin attribute, otherwise
returns #f."
  (and (attribute? object)
       (and=> (attrib-attachment object) pin?)))

;;; Collect all net= pin numbers.
(define (net-numbers objects)
  (define (add-net-pin-attribute net-attrib)
    (let ((net (attrib-value net-attrib)))
      (let ((net-tokens (string-split net #\:)))
        ;; length of net tokens has to be 2
        (if (not (= 2 (length net-tokens)))
            (begin (blame-object net-attrib
                                 'error
                                 (format #f
                                         (_ "Bad ~A= attribute [~A]\n")
                                         'net
                                         net))
                   '())
            (let ((pin-tokens (string-split (cadr net-tokens) #\,)))
              (for-each
               (lambda (pin)
                 (blame-object net-attrib
                               'info
                               (format #f
                                       (_ "Found pin number ~A=~A in net attribute\n")
                                       'pinnumber
                                       pin)))
               pin-tokens)
              pin-tokens)))))

  (append-map add-net-pin-attribute objects))


(define (check-duplicate-net-pinnumbers page ls)
  "Checks list of pin numbers LS (typically obtained from net=
attributes) for duplicates and reports errors for PAGE."
  (and (not (null? ls))
       (not (null? (cdr ls)))
       (if (string=? (car ls) (cadr ls))
           (blame-object page
                         'error
                         (format #f
                                 (_ "Found duplicate pin in net= attributes [~A]\n")
                                 (car ls))))
       (if (string=? (car ls) "0")
           (blame-object page
                         'error
                         (format #f
                                 (_ "Found pinnumber ~A=~A in net= attribute\n")
                                 'pinnumber
                                 0)))
       (check-duplicate-net-pinnumbers page (cdr ls))))

(define (check-duplicate-net-pinnumber-numbers page pins nets)
  "Compares sorted pin number lists PINS and NETS taken
accordingly from pinnumber= and net= attributes and reports errors
for PAGE if some numbers match."
  (and (not (null? pins))
       (not (null? nets))
       (let ((pin (car pins))
             (net (car nets)))
         (if (string=? pin net)
             (and (blame-object page
                                'warning
                                (format #f (_ "Found the same number in a pinnumber attribute and in a net attribute [~A]\n") pin))
                  (check-duplicate-net-pinnumber-numbers page (cdr pins) (cdr nets)))
             (if (string>? pin net)
                 (check-duplicate-net-pinnumber-numbers page pins (cdr nets))
                 (check-duplicate-net-pinnumber-numbers page (cdr pins) nets))))))


(define (pinseq<? a b)
  (string<? (symbol-pin-seq a) (symbol-pin-seq b)))

(define (pinseq=? a b)
  (string=? (symbol-pin-seq a) (symbol-pin-seq b)))

(define (pinnumber<? a b)
  (string<? (symbol-pin-number a) (symbol-pin-number b)))

(define (pinnumber=? a b)
  (string=? (symbol-pin-number a) (symbol-pin-number b)))

(define (check-pin-attrib-duplicates name getter less-func equal-func symbol-pins)
  "Checks for duplicated attributes in LS."
  (define (blame-duplicate symbol-pin)
    (blame-object (symbol-pin-object symbol-pin)
                  'error
                  (format #f
                          (_ "Duplicate pin attribute in the symbol: ~A=~A")
                          name
                          (getter symbol-pin))))
  (define (blame-if-list ls)
    (when (list? ls)
      (for-each blame-duplicate ls)))

  (for-each blame-if-list
            (list->duplicate-list (filter getter symbol-pins)
                                  less-func
                                  equal-func)))

(define (check-duplicates/pinseq symbol-pins)
  (check-pin-attrib-duplicates 'pinseq
                               symbol-pin-seq
                               pinseq<?
                               pinseq=?
                               symbol-pins))

(define (check-duplicates/pinnumber symbol-pins)
  (check-pin-attrib-duplicates 'pinnumber
                               symbol-pin-number
                               pinnumber<?
                               pinnumber=?
                               symbol-pins))
