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

(define-module (symbol check pin-attrib)
  #:use-module (srfi srfi-1)

  #:use-module (lepton attrib)
  #:use-module (lepton gettext)
  #:use-module (lepton object)
  #:use-module (symbol blame)
  #:use-module (symbol check attrib)
  #:use-module (symbol check duplicate)
  #:use-module (symbol check pin)

  #:export (pin-attrib?
            check-pin-attrib-duplicates
            check-duplicates/pinseq
            check-duplicates/pinnumber))

(define (pin-attrib? object)
  "Returns #t if OBJECT is a pin attribute, otherwise
returns #f."
  (and (attribute? object)
       (and=> (attrib-attachment object) pin?)))

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

  (define (reset-pin-seq symbol-pin)
    (set-symbol-pin-seq! symbol-pin #f))
  (define (blame-duplicate symbol-pin)
    (blame-object (symbol-pin-object symbol-pin)
                  'error
                  (format #f
                          (G_ "Duplicate pin attribute in the symbol: ~A=~A")
                          name
                          (getter symbol-pin))))
  (define (blame-if-list ls)
    (when (list? ls)
      (for-each blame-duplicate ls)
      ;; Reset duplicate pinseq values to avoid slotting
      ;; checks for them.
      (when (eq? name 'pinseq)
        (for-each reset-pin-seq (cdr ls)))))

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
