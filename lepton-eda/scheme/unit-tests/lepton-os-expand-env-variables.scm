;                                                         -*-Scheme-*-
;    This program is free software: you can redistribute it and/or modify
;    it under the terms of the GNU General Public License as published by
;    the Free Software Foundation, either version 2 of the License, or
;    (at your option) any later version.
;
;    This program is distributed in the hope that it will be useful,
;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;    GNU General Public License for more details.
;
;    You should have received a copy of the GNU General Public License
;    along with this program.  If not, see <http://www.gnu.org/licenses/>.
;
;
; Copyright (C) 2011  L.S.P. <ultrabit@gmail.com>
;
(use-modules (lepton os))

(test-begin "expand-env-variables")

(and
 ;; Bad expression samples

 ;; FreeBSD: setenv() has no effect on existing env vars,
 ;;   unless putenv() is called first:
 ;;
 (putenv "USER")
 (putenv "HOME")

 (setenv "USER" "myuser")
 (setenv "HOME" "myhome")
 (test-equal "/a/${USER myhome }/b/c"
   (expand-env-variables "/a/${USER ${HOME} }/b/c"))
 (test-equal "/a/${USER=myhome }/b/c"
   (expand-env-variables "/a/${USER=${HOME} }/b/c"))
 (test-equal "/a/${USER=myhome}/b/c"
   (expand-env-variables "/a/${USER=${HOME}}/b/c"))
 (test-equal "/a/${=USER=}/b/c"
   (expand-env-variables "/a/${=USER=}/b/c"))
 (test-equal "/a//b/c"
   (expand-env-variables "/a/${}/b/c"))
 (test-equal "/a/${-USER-}/b/c"
   (expand-env-variables "/a/${-USER-}/b/c"))
 ;; Good expression samples
 (test-equal "myhome/a/b/c"
   (expand-env-variables "~/a/b/c"))
 (setenv "EXPAND_ENV_VARS_TEST" "abc")
 (setenv "VARS_TEST" "_VARS_TEST")
 (test-equal "/a/abc/b/c"
   (expand-env-variables "/a/${EXPAND_ENV_VARS_TEST}/b/c"))
 (test-equal "/a/abcabc/b/c"
   (expand-env-variables "/a/${EXPAND_ENV_VARS_TEST}${EXPAND_ENV_VARS_TEST}/b/c"))
 (test-equal "/a/abc/abc/b/c"
   (expand-env-variables "/a/${EXPAND_ENV_VARS_TEST}/${EXPAND_ENV_VARS_TEST}/b/c"))
 ;; Recursed expansion
 (test-equal "/a/abc/abc/b/c"
   (expand-env-variables "/a/${EXPAND_ENV${VARS_TEST}}/${EXPAND_ENV_VARS_TEST}/b/c"))
 )

(test-end "expand-env-variables")
