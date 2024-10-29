;;; Lepton EDA library - Scheme API
;;; Copyright (C) 2024-2025 Lepton EDA Contributors
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


(define-module (lepton init)
  #:use-module (lepton ffi)
  #:use-module (lepton m4)

  #:export (init-liblepton))

(define (init-liblepton)
  "Perform runtime initialization of liblepton library.  This
function is responsible for making sure that any runtime
initialization is done for all the liblepton routines. It should
be called before any other liblepton functions are called."
  ;; Initialise Lepton EDA data and configuration search paths.
  (define (init-paths)
    ;; These functions store their data in static local variables.
    ;; Calling them here forces data initialization.
    (eda_get_system_data_dirs)
    (eda_get_system_config_dirs)
    (eda_get_user_data_dir)
    (eda_get_user_config_dir)
    (eda_get_user_cache_dir))

  ;; Initialize liblepton gettext domain.
  (bindtextdomain %m4-liblepton-gettext-domain %lepton-localedir)
  (textdomain %m4-liblepton-gettext-domain)
  (bind-textdomain-codeset %m4-liblepton-gettext-domain "UTF-8")

  (init-paths)
  (s_clib_init)
  (s_attrib_init)
  (lepton_color_init)
  (unless (getenv "LEPTON_INHIBIT_RC_FILES")
    (register-data-dirs)))
