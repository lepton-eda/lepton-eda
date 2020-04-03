;; Lepton EDA Schematic Capture
;; Scheme API
;; Copyright (C) 2017 dmn <graahnul.grom@gmail.com>
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
;;

( define-module  ( schematic undo )

    ; see schematic/src/scheme_undo.c:
    ;
    #:use-module ( schematic core undo )

    ; public:
    ;
    #:export     ( undo-save-state )

) ; define-module



;
; wrappers for functions defined in scheme_undo.c:
;

( define undo-save-state %undo-save-state )

