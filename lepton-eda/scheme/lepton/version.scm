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

  #:use-module (lepton ffi)
  #:use-module (lepton log)

  #:export (display-lepton-version
            lepton-version
            lepton-version-data
            lepton-version-ref))

(define-syntax-rule (define-getter <name>)
  (define <name>
    (let* ((proc (pointer->procedure
                  '*
                  (dynamic-func (symbol->string (quote <name>))
                                liblepton)
                  '()))
           (result (delay (pointer->string (proc)))))
      (force result))))


(define-getter lepton_version_prepend)
(define-getter lepton_version_dotted)
(define-getter lepton_version_date)
(define-getter lepton_version_git_commit)
(define-getter lepton_version_bugreport)
(define-getter lepton_version_url)
(define-getter lepton_version_copyright)

;;; Return Lepton EDA version string list.
(define %lepton-version
  (list lepton_version_prepend
        lepton_version_dotted
        lepton_version_date
        lepton_version_git_commit
        lepton_version_bugreport
        lepton_version_url
        lepton_version_copyright))

(define lepton_version_git7
  (string-take lepton_version_git_commit 7))

(define %lepton-version-alist
  `((prepend   . ,lepton_version_prepend)
    (dotted    . ,lepton_version_dotted)
    (date      . ,lepton_version_date)
    (git       . ,lepton_version_git_commit)
    (git7      . ,lepton_version_git7)
    (bugs      . ,lepton_version_bugreport)
    (url       . ,lepton_version_url)
    (copyright . ,lepton_version_copyright)))


(define (lepton-version-ref name)
  "Reference and return one element of lepton-version-data by
NAME."
  (assq-ref %lepton-version-alist name))


(define (lepton-version-data . args)
  "Return Lepton version data list.  Optional arguments ARGS may
define the sequence and contents of the list to retrieve.  The following
symbols are supported:
  'prepend - get PREPEND_VERSION_STRING defined in liblepton
  'dotted  - get PACKAGE_DOTTED_VERSION string defined in liblepton
  'date    - get PACKAGE_DATE_VERSION string defined in liblepton
  'git     - get PACKAGE_GIT_COMMIT string defined in liblepton
  'git7    - get first 7 symbols of PACKAGE_GIT_COMMIT
  'bugs    - get PACKAGE_BUGREPORT string defined in liblepton
  'url     - get PACKAGE_URL string defined in liblepton

The default call for (lepton-version-data) without arguments is
equivalent to the call (lepton-version-data 'prepend 'dotted 'date
'git 'bugs 'url 'copyright)."
  (if (null? args)
      %lepton-version
      (map lepton-version-ref args)))


(define-syntax-rule (~lepton-version <string> <args>)
  (apply format #f <string> (apply lepton-version-data <args>)))

(define* (lepton-version #:optional fmt . args)
  "If no arguments given, returns a canonical string
representation of Lepton version.  Otherwise the first argument
FMT should be a format string followed by ARGS which is a list of
symbols as defined for lepton-version-data."
  (if (null? args)
      (apply format #f "Lepton EDA ~A~A.~A (git: ~A)"
             (lepton-version-data 'prepend 'dotted 'date 'git7))
      (~lepton-version fmt args)))

(define* (display-lepton-version #:key
                                 (print-name #f)
                                 (log #f)
                                 (copyright #f))
  "Output Lepton tool version to the current output port or
log. The optional keyword PRINT-NAME specifies if program's
basename should be output along with other information.  The
optional keyword LOG specifies if the output should go to log.
The optional keyword COPYRIGHT enables output of copyright
information.  All keywords are set to #f by default."
  (define program-basename
    (basename (car (program-arguments))))

  (define suite-name "Lepton EDA")

  (define version-prefix
    (if print-name
        (string-append suite-name "/" program-basename)
        suite-name))

  (define version-suffix
    (lepton-version "~A~A.~A (git: ~A)\n"
                    'prepend 'dotted 'date 'git7))

  (define version-message
    (string-append version-prefix
                   " "
                   version-suffix
                   (if copyright
                       (lepton-version-ref 'copyright)
                       "")))

  (if log
      (log! 'message version-message)
      (display version-message)))
