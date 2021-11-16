;; Lepton EDA library - Scheme API
;; Copyright (C) 2010-2011 Peter Brett <peter@peter-b.co.uk>
;; Copyright (C) 2017-2021 Lepton EDA Contributors
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
;;

;; I18n for libgeda Scheme API.
;;
;; This module is for internal use only.

(define-module (lepton core gettext)
  #:use-module (lepton m4))

(define-public (G_ msg) (gettext msg LIBLEPTON_GETTEXT_DOMAIN))
(define-public (N_ msgid msgid-plural n)
  (ngettext msgid msgid-plural n LIBLEPTON_GETTEXT_DOMAIN))
