;;; Lepton EDA library - Scheme API
;;; Copyright (C) 2022 Lepton EDA Contributors
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


(define-module (lepton toplevel)
  #:use-module (ice-9 format)
  #:use-module (system foreign)

  #:use-module (lepton ffi)

  #:export (%current-toplevel
            %make-toplevel
            %with-toplevel))

(define-wrapped-pointer-type <lepton-toplevel>
  lepton-toplevel?
  wrap-lepton-toplevel
  unwrap-lepton-toplevel
  (lambda (toplevel port)
    (format port "#<lepton-toplevel-0x~x>"
            (pointer-address (unwrap-lepton-toplevel toplevel)))))

(define (%make-toplevel)
  "Make new toplevel."
  (lepton_toplevel_new))

(define (%current-toplevel)
  "Get toplevel for the current dynamic context."
  (let ((*toplevel (false-if-exception (edascm_current_toplevel))))
    ;; This is the first approximation.  Really, toplevel fluid
    ;; itself should be tested for non-#f value, something like:
    ;; (fluid-ref %lepton-toplevel)
    (and *toplevel
         (not (null-pointer? *toplevel))
         (wrap-lepton-toplevel *toplevel))))

(define (%with-toplevel toplevel thunk)
  "Call THUNK, setting the toplevel fluid to TOPLEVEL."
  (pointer->scm (edascm_with_toplevel (scm->pointer toplevel)
                                      (scm->pointer thunk))))
