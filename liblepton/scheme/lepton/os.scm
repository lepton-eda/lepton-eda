;;; Lepton EDA library - Scheme API
;;; Copyright (C) 2011 Peter Brett <peter@peter-b.co.uk>
;;; Copyright (C) 2019-2022 Lepton EDA Contributors
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


(define-module (lepton os)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:use-module (system foreign)

  #:use-module (lepton ffi)

  #:export (expand-env-variables
            sys-config-dirs
            sys-data-dirs
            user-cache-dir
            user-config-dir
            user-data-dir
            ;; Deprecated functions.
            path-separator
            path-separator-char
            platform
            platform?
            separator
            separator-char
            separator-char?))


(define %platform
  (cond
   ((string-contains %host-type "cygwin") '(cygwin win32))
   ((string-contains %host-type "linux") '(linux))
   ((string-contains %host-type "mingw") '(win32 win32-native))
   (else '(unknown))))

(define (platform)
  "Return a list of symbols describing the host operating system.
The symbols may include:
- 'win32 -- Windows
- 'win32-native -- Windows, not via Cygwin
- 'cygwin -- Cygwin
- 'linux -- Linux"
  %platform)

(define (platform? x)
  (member x (platform)))

; Deprecated and must be removed after version 1.10.
(define separator-char
  (car (char-set->list (string->char-set file-name-separator-string))))

; Deprecated and must be removed after version 1.10.
(define separator file-name-separator-string)

(define path-separator-char
  (if (platform? 'win32-native) #\; #\:))

(define path-separator (string path-separator-char))

(define separator-char? file-name-separator?)


(define (sys-data-dirs)
  "Returns a list of search directories for system data."
  (c-string-array->list (eda_get_system_data_dirs)))

(define (sys-config-dirs)
  "Returns a list of search directories for system configuration."
  (c-string-array->list (eda_get_system_config_dirs)))

(define (user-data-dir)
  "Returns the directory where per-user data are stored."
  (pointer->string (eda_get_user_data_dir)))

(define (user-config-dir)
  "Returns the directory where per-user configuration information
is stored."
  (pointer->string (eda_get_user_config_dir)))

(define (user-cache-dir)
  "Returns the directory where per-user cache data are stored."
  (pointer->string (eda_get_user_cache_dir)))


;;; To get the user home directory, the recommended approach is
;;; to first check the $HOME environment variable and use
;;; getpwuid() as a last resort only.
(define (user-home-dir)
  (or (getenv "HOME")
      ;; Fall back to using low level functions if $HOME is not
      ;; set.
      (passwd:dir (getpwuid (getuid)))))


(define expand-env-variables
  ;; Only compile regular expression once
  (let ((rx (make-regexp "\\$\\{(\\w*)\\}")))
    ;; This is the actual expand-env-variables function -- it's a
    ;; closure around rx.
    (lambda (str)
      "Expands environment variables in STR usually representing a
path and returns the result of expansion.  To be expanded, a
variable must be enclosed in curly braces and prefixed with the
dollar sign (e.g. ${HOME}).  Additionally, the function replaces
tilda prefix (~) with the user home directory."
      ;; Returns result of expanding the environment variable name
      ;; found in match, or "".
      (define (match-getenv m)
        (or (getenv (match:substring m 1)) ""))
      ;; Carries out a single round of environment variable expansion
      ;; on str
      (define (expand-once str)
        (regexp-substitute/global #f rx str 'pre match-getenv 'post))
      ;; Transform prefix "~/" to "${HOME}/" for subsequent
      ;; expansion.
      (define (tilda-prefix->home-prefix s)
        (if (and (char=? (string-ref s 0) #\~)
                 (file-name-separator? (string-ref s 1)))
            (string-append (user-home-dir)
                           file-name-separator-string
                           (substring s 2))
            s))

      ;; Tail-recursively expands str until no more environment variables
      ;; can be expanded.
      (let ((result (expand-once (tilda-prefix->home-prefix str))))
        (if (string=? str result)
            result
            (expand-env-variables result))))))
