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

  #:export (%lepton-toplevel
            pointer->toplevel
            %current-toplevel
            %make-toplevel
            %with-toplevel))

(define-wrapped-pointer-type <lepton-toplevel>
  lepton-toplevel?
  pointer->toplevel
  toplevel->pointer
  (lambda (toplevel port)
    (format port "#<lepton-toplevel-0x~x>"
            (pointer-address (toplevel->pointer toplevel)))))

(define %lepton-toplevel #f)

;;; Initialize %lepton-toplevel with a new fluid variable for
;;; Scheme and C code.  Do it once.
(when (not %lepton-toplevel)
  (set! %lepton-toplevel (make-fluid))
  (lepton_init_toplevel_fluid (scm->pointer %lepton-toplevel)))


(define (%make-toplevel)
  "Make new toplevel."
  (pointer->toplevel (lepton_toplevel_new)))

(define (%current-toplevel)
  "Get toplevel for the current dynamic context."
  (and=> (fluid-ref %lepton-toplevel) pointer->toplevel))

(define (%with-toplevel toplevel thunk)
  "Call THUNK, setting the toplevel fluid to TOPLEVEL."
  (with-fluid* %lepton-toplevel
    (toplevel->pointer toplevel) thunk))
