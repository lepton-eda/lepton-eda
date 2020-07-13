;;; Lepton EDA library - Scheme API
;;; Copyright (C) 2017-2020 Lepton EDA Contributors
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

;;; Lepton version

(define-module (lepton version)
  #:use-module (system foreign)

  #:export (lepton-version))

(define liblepton (dynamic-link "liblepton"))

(define-syntax define-getter
  (syntax-rules ()
    ((_ <name>)
     (define <name>
       (let* ((proc (delay (pointer->procedure
                           '*
                           (dynamic-func (symbol->string (quote <name>))
                                         liblepton)
                           '())))
              (result (delay (pointer->string ((force proc))))))
         (force result))))))


(define-getter lepton_version_prepend)
(define-getter lepton_version_dotted)
(define-getter lepton_version_date)
(define-getter lepton_version_git_commit)
(define-getter lepton_version_bugreport)
(define-getter lepton_version_url)
;;; This procedure returns version message that can be used in the
;;; --version output.
(define-getter lepton_version_message)


;;; Return Lepton EDA version string list.
(define %lepton-version
  (list lepton_version_prepend
        lepton_version_dotted
        lepton_version_date
        lepton_version_git_commit
        lepton_version_bugreport
        lepton_version_url
        lepton_version_message))

(define lepton_version_git7
  (string-take lepton_version_git_commit 7))

(define %lepton-version-alist
  `((prepend . ,lepton_version_prepend)
    (dotted  . ,lepton_version_dotted)
    (date    . ,lepton_version_date)
    (git     . ,lepton_version_git_commit)
    (git7    . ,lepton_version_git7)
    (bugs    . ,lepton_version_bugreport)
    (url     . ,lepton_version_url)
    (msg     . ,lepton_version_message)))

; public:
;
; [what]: symbol, what information to retrieve:
;   'prepend => get PREPEND_VERSION_STRING (defined in liblepton/defines.h)
;   'dotted  => get PACKAGE_DOTTED_VERSION (defined in version.h)
;   'date    => get PACKAGE_DATE_VERSION   (defined in version.h)
;   'git     => get PACKAGE_GIT_COMMIT     (defined in version.h)
;   'git7    => get first 7 symbols of PACKAGE_GIT_COMMIT
;   'bugs    => get PACKAGE_BUGREPORT (defined in config.h)
;   'url     => get PACKAGE_URL       (defined in config.h)
;   'msg     => get message from lepton_version_message()
;
; If [what] is #f, return a list of strings:
; - PREPEND_VERSION_STRING
; - PACKAGE_DOTTED_VERSION
; - PACKAGE_DATE_VERSION
; - PACKAGE_GIT_COMMIT
; - PACKAGE_BUGREPORT
; - PACKAGE_URL
; - message from lepton_version_message()
;
( define* ( lepton-version #:optional (what #f) )
  ; return:
  ( if what
    (assq-ref %lepton-version-alist what)
    %lepton-version
  )
)
