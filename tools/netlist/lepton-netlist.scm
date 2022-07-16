;;; Lepton EDA netlister
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

(use-modules (ice-9 getopt-long)
             (srfi srfi-26)
             (geda deprecated)
             (lepton ffi)
             (lepton library)
             (lepton log)
             (lepton toplevel)
             (lepton version)
             (netlist option)
             (netlist))

;;; Initialize liblepton library.
(liblepton_init)
(unless (getenv "LEPTON_INHIBIT_RC_FILES")
  (register-data-dirs))


;;; Localization.
(define %textdomain "lepton-netlist")
(textdomain %textdomain)
(bindtextdomain %textdomain %lepton-localedir)
(bind-textdomain-codeset %textdomain "UTF-8")

;;; Process lepton-netlist options.

;;; Specification.
(define %option-spec
  '((quiet (single-char #\q))
    (verbose (single-char #\v))
    (load-path (single-char #\L) (value #t))
    (backend (single-char #\g) (value #t))
    (file-backend (single-char #\f) (value #t))
    (backend-option (single-char #\O) (value #t))
    (list-backends (single-char #\b))
    (output (single-char #\o) (value #t))
    (pre-load (single-char #\l) (value #t))
    (post-load (single-char #\m) (value #t))
    (eval-code (single-char #\c) (value #t))
    (interactive (single-char #\i))
    (help (single-char #\h))
    (version (single-char #\V))))

;;; Options.
(define %options
  (getopt-long (program-arguments) %option-spec))

;;; Initialize netlister options.
(init-netlist-options! %options)

;;; Evaluate Scheme expressions that need to be run before rc
;;; files are loaded.
(for-each (cut add-to-load-path <>)
          (netlist-option-ref/toplevel %options 'load-path '()))

;;; Run netlister in new toplevel environment.
(with-toplevel (make-toplevel)
  (lambda ()
    ;; Init log domain and create log file right away even if
    ;; logging is enabled.
    (init-log "netlist")
    (display-lepton-version #:print-name #t #:log #t)
    (main)))
