;;; Lepton EDA netlister
;;; Copyright (C) 2016 gEDA Contributors
;;; Copyright (C) 2018 Lepton EDA Contributors
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

(define-module (netlist attrib compare)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:use-module (netlist attrib compare)
  #:export (refdes<?
            value<?))


;;; The table of suffices used to compare value= attribute values
(define %value-suffix-factor-table
  '((p . 10e-12)
    (n . 10e-9)
    (u . 10e-6)
    (m . 10e-3)
    (k . 10e+3)
    (M . 10e+6)
    (G . 10e+9)
    (T . 10e+12)))

(define refdes-regex
  (make-regexp "([^0-9]+|[0-9]+)"))

(define leading-zeros-regex
  (make-regexp "(^[0]+|[^0].*)"))

(define (refdes<? a b)
  "Compares two strings A and B alphanumerically. Parts of the
strings consisting of digits are transformed into numbers and are
compared as numbers. Other parts are compared as strings. Starting
number is considered to be less than starting string. Zero padded
numbers are considered to be more than plain ones to avoid
ambiguity. Example usage is sorting of reference designators."

  (define (alphanumeric-split s)
    (map match:substring (list-matches refdes-regex s)))
  (define (zero-pad-split s)
    (map match:substring (list-matches leading-zeros-regex s)))

  (define (split s)
    (append-map zero-pad-split (alphanumeric-split s)))

  (define (less? xs ys)
    (and (not (string=? xs ys))
         (let ((x (string->number xs))
               (y (string->number ys)))
           (if x                        ; x is a number
               (or (not y)              ; but y is a string
                   (< x y)              ; or y is a number and less than x
                   (and (= x y)         ; or they are equal
                        (string>? xs ys))) ; then the first string must
                                        ; be greater since the
                                        ; second one has to be
                                        ; zero padded if the numbers
                                        ; are equal (like "x5" vs "x05")

               (and (not y)             ; both are strings,
                    (string<? xs ys)))))) ; then just compare them

  ;; first split both initial strings
  (let loop ((a-ls (split a))
             (b-ls (split b)))
    (and (not (null? b-ls))             ; #f if second list is shorter
         (or (null? a-ls)               ; #t if first is shorter
             (less? (first a-ls) (first b-ls)) ; check for less
             (and (string=? (first a-ls) (first b-ls)) ; if first elems are equal
                  (loop (cdr a-ls) (cdr b-ls))))))) ; continue with others


(define (value<? a b)
  "Compares two string A and B, typically values of the attribute
\"value=\", trying to get the numbers they present accounting for
their suffix and numerical value. If it is not possible, compares
them by less as strings using case sensitive string comparison
function."
  (define (suffix->factor s)
    (assq-ref %value-suffix-factor-table (string->symbol s)))

  (define (value-string->number s)
    (let ((num (string->number s)))
      (or num
          (let* ((num-length (1- (string-length s)))
                 (suffix (suffix->factor (substring s num-length)))
                 (prefix (string->number (substring s 0 num-length))))
            (and suffix prefix (* prefix suffix))))))

  (let ((anum (value-string->number a))
        (bnum (value-string->number b)))
    (if (and anum bnum)
        (< anum bnum)
        (string<? a b))))
