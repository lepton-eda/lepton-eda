;; Lepton EDA Schematic Capture
;; Scheme API
;; Copyright (C) 2011 Peter Brett <peter@peter-b.co.uk>
;; Copyright (C) 2011-2012 gEDA Contributors
;; Copyright (C) 2017-2022 Lepton EDA Contributors
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

;; I18n for lepton-schematic Scheme API.
;;
;; This module is for internal use only.

(define-module (schematic gettext)
  #:export (%schematic-gettext-domain
            G_))

(define %schematic-gettext-domain "libleptongui")
(define (G_ msg) (gettext msg %schematic-gettext-domain))
