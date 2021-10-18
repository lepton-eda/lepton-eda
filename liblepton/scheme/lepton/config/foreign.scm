;;; Lepton EDA library - Scheme API
;;; Copyright (C) 2021 Lepton EDA Contributors
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


(define-module (lepton config foreign)
  #:use-module (system foreign)

  #:use-module (lepton ffi)

  #:export (geda-config-pointer?
            geda-config->pointer
            geda-config->pointer*
            pointer->geda-config))

;;; Helper transformers between #<geda-config> smobs and C config
;;; pointers.
(define (geda-config->pointer smob)
  (or (false-if-exception (edascm_to_config (scm->pointer smob)))
      ;; Return NULL if the SMOB is not the #<geda-config> smob.
      %null-pointer))

(define (pointer->geda-config pointer)
  ;; Return #f if the pointer is wrong.
  (false-if-exception (pointer->scm (edascm_from_config pointer))))

(define (geda-config-pointer? pointer)
  (true? (edascm_is_config pointer)))

(define-syntax geda-config->pointer*
  (syntax-rules ()
    ((_ config pos)
     (let ((pointer (geda-config->pointer config)))
       (if (null-pointer? pointer)
           (let ((proc-name (frame-procedure-name (stack-ref (make-stack #t) 1))))
             (scm-error 'wrong-type-arg
                        proc-name
                        "Wrong type argument in position ~A: ~A"
                        (list pos config)
                        #f))
           pointer)))))
