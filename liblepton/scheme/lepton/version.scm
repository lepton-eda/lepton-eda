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
       (pointer->procedure
        '*
        (dynamic-func (symbol->string (quote <name>))
                      liblepton)
        '())))))


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
(define (%lepton-version)
  (map pointer->string
       (list (lepton_version_prepend)
             (lepton_version_dotted)
             (lepton_version_date)
             (lepton_version_git_commit)
             (lepton_version_bugreport)
             (lepton_version_url)
             (lepton_version_message))))

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
    ( lepton-version-ex what ) ; if
    ( %lepton-version )        ; else
  )
)



; private:
;
( define* ( lepton-version-ex what #:optional ( ver (%lepton-version) ) )

  ( define ( item ndx )
    ( list-ref ver ndx )
  )

  ( define ( git7 )
    ( string-take (item 3) 7 )
  )

  ( let
    (
    ( items
      ( list
        ( cons 'prepend ( item 0 ) )
        ( cons 'dotted  ( item 1 ) )
        ( cons 'date    ( item 2 ) )
        ( cons 'git     ( item 3 ) )
        ( cons 'git7    ( git7 )   )
        ( cons 'bugs    ( item 4 ) )
        ( cons 'url     ( item 5 ) )
        ( cons 'msg     ( item 6 ) )
      )
    )
    )

    ; return:
    ( assoc-ref items what )

  ) ; let

) ; lepton-version-ex()

