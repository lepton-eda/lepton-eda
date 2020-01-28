;;; Lepton EDA
;;; liblepton - Lepton's library - Scheme API
;;; Copyright (C) 2017-2019 Lepton EDA Contributors
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
;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
;;; MA 02111-1301 USA.

;;; Lepton version

(define-module (lepton version)
  #:use-module (lepton core version)

  #:export (lepton-version))



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
;   'msg     => get message from version_message()
;
; If [what] is #f, return a list of strings:
; - PREPEND_VERSION_STRING
; - PACKAGE_DOTTED_VERSION
; - PACKAGE_DATE_VERSION
; - PACKAGE_GIT_COMMIT
; - PACKAGE_BUGREPORT
; - PACKAGE_URL
; - message from version_message()
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

