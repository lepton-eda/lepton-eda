;;; Lepton EDA library - Scheme API
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

;;; Lepton REPL with readline support

(define-module (lepton repl)
  #:use-module (system repl repl)
  #:use-module (system repl common)

  #:use-module (lepton gettext)
  #:use-module ((lepton os) #:select (user-config-dir))

  #:export (lepton-repl
            lepton-repl-save-history
            lepton-repl-welcome
            lepton-repl-readline-warning))

;;; The current user's readline history file.
(define %default-history-filename ".lepton_history")

;;; Absolute name of history file.
(define history-filename (string-append (user-config-dir)
                                        file-name-separator-string
                                        %default-history-filename))

(define (lepton-repl-save-history)
  "Saves readline history of the current REPL."
  (and (provided? 'readline)
       ((@ (ice-9 readline) write-history) history-filename)))

;;; Outputs Lepton REPL greeting.
(define (lepton-repl-welcome)
  (display (G_ "Welcome to Lepton REPL!\n")))

;;; Warning on systems where the 'readline feature is not
;;; supported.
(define (lepton-repl-readline-warning)
  (display (G_ "WARNING: Readline library is not supported in your configuration.\n")))

(define (lepton-repl)
  "Runs interactive REPL in a terminal."
  (let ((repl (make-repl (current-language) #f)))
    (repl-eval repl
               `(begin
                  (use-modules (ice-9 session) ; help, apropos and such
                               (system repl command) ; guile meta-commands
                               (lepton repl)) ; this module
                  (lepton-repl-welcome)
                  (resolve-module '(ice-9 readline))
                  ;; After resolving that module the variable
                  ;; *features* should contain 'readline.
                  (if (provided? 'readline)
                      (begin
                        ;; Readline is not loaded automatically,
                        ;; so let's force its loading.
                        (use-modules (ice-9 readline))
                        ((@ (ice-9 readline) activate-readline))
                        ((@ (ice-9 readline) clear-history))
                        ((@ (ice-9 readline) read-history) ,history-filename))
                      (lepton-repl-readline-warning))))
    (run-repl repl)
    ;; Save history on normal REPL exit.
    (lepton-repl-save-history)))
