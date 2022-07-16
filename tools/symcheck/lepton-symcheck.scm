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

(use-modules (lepton ffi)
             (lepton toplevel)
             (symcheck check))

;;; Initialize liblepton library.
(liblepton_init)
(unless (getenv "LEPTON_INHIBIT_RC_FILES")
  (register-data-dirs))


;;; Localization.
(define %textdomain "lepton-symcheck")
(textdomain %textdomain)
(bindtextdomain %textdomain %lepton-localedir)
(bind-textdomain-codeset %textdomain "UTF-8")

(with-toplevel (make-toplevel) check-all-symbols)
