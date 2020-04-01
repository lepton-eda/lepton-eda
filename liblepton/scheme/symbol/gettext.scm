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

(define-module (symbol gettext)

  #:export (_ N_))

(define %symcheck-gettext-domain "lepton-symcheck")

(define (_ msg) (gettext msg %symcheck-gettext-domain))

(define (N_ msgid msgid-plural n)
  (ngettext msgid msgid-plural n %symcheck-gettext-domain))
