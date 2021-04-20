;;; Lepton EDA library - Scheme API
;;; Copyright (C) 2011 Peter Brett <peter@peter-b.co.uk>
;;; Copyright (C) 2019-2021 Lepton EDA Contributors
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

  ;; Import C procedures and variables
  #:use-module (lepton core os)

  #:export (sys-data-dirs
            sys-config-dirs))

(define-public platform %platform)

(define-public (platform? x)
  (member x (platform)))

; Deprecated and must be removed after version 1.10.
(define-public separator-char
  (car (char-set->list (string->char-set file-name-separator-string))))

; Deprecated and must be removed after version 1.10.
(define-public separator file-name-separator-string)

(define-public path-separator-char
  (if (platform? 'win32-native) #\; #\:))

(define-public path-separator (string path-separator-char))

(define-public separator-char? file-name-separator?)

(define (c-string-array->list pointer)
  "Returns a list of search directories for system data."
  (let ((pointer (eda_get_system_data_dirs)))
    (let loop ((num 0)
               (ls '()))
      (let ((string-pointer
             (dereference-pointer
              (make-pointer (+ (pointer-address pointer)
                               (* num (sizeof '*)))))))
        (if (null-pointer? string-pointer)
            (reverse ls)
            (loop (1+ num)
                  (cons (pointer->string string-pointer)
                        ls)))))))

(define (sys-data-dirs)
  "Returns a list of search directories for system data."
  (c-string-array->list (eda_get_system_data_dirs)))

(define (sys-config-dirs)
  "Returns a list of search directories for system configuration."
  (c-string-array->list (eda_get_system_config_dirs)))

(define-public user-data-dir %user-data-dir)
(define-public user-config-dir %user-config-dir)
(define-public user-cache-dir %user-cache-dir)

(define-public expand-env-variables
  ;; Only compile regular expression once
  (let ((rx (make-regexp "\\$\\{(\\w*)\\}")))
    ;; This is the actual expand-env-variables function -- it's a
    ;; closure around rx.
    (lambda (str)
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
            (string-append "${HOME}"
                           file-name-separator-string
                           (substring s 2))
            s))

      ;; Tail-recursively expands str until no more environment variables
      ;; can be expanded.
      (let ((result (expand-once (tilda-prefix->home-prefix str))))
        (if (string=? str result)
            result
            (expand-env-variables result))))))
