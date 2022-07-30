;;; Lepton EDA library - Scheme API
;;; Copyright (C) 2020-2022 Lepton EDA Contributors
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


;;; Macros for verifying consistency of foreign function arguments.


(define-module (lepton ffi check-args)
  #:export (error-wrong-type-arg
            check-boolean
            check-coord
            check-integer
            check-string
            check-symbol
            check-vector
            check-procedure))


(define-syntax-rule (error-wrong-type-arg pos type object)
  (scm-error 'wrong-type-arg
             (frame-procedure-name (stack-ref (make-stack #t) 1))
             "Wrong type argument in position ~A (expecting ~A): ~A"
             (list pos type object)
             #f))


(define (check-boolean val pos)
  ;; This function is defined just for consistency.  Someone may
  ;; get confused if we miss some argument checks. Since any value
  ;; in Scheme is a boolean in a sense, that is, it is considered
  ;; to be true if not #f, there is no point to check for real
  ;; boolean values, #t and #f, using Scheme boolean? function.
  ;; So we just return #t here.
  #t)


(define-syntax-rule (check-integer val pos)
  (unless (integer? val)
    (error-wrong-type-arg pos 'integer val)))


(define-syntax-rule (check-coord val pos)
  (unless (and (pair? val)
               (integer? (car val))
               (integer? (cdr val)))
    (error-wrong-type-arg pos "a pair of integers" val)))


(define-syntax-rule (check-string val pos)
  (unless (string? val)
    (error-wrong-type-arg pos 'string val)))


(define-syntax-rule (check-symbol val pos)
  (unless (symbol? val)
    (error-wrong-type-arg pos 'symbol val)))


(define-syntax-rule (check-vector val pos)
  (unless (and (list? val)
               (every integer? val))
    (error-wrong-type-arg pos "list of integers" val)))


(define-syntax-rule (check-procedure val pos)
  (unless (procedure? val)
    (error-wrong-type-arg pos 'procedure val)))
