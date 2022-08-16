;;; Lepton EDA Schematic Capture
;;; Scheme API
;;; Copyright (C) 2020-2022 Lepton EDA Contributors
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

(define-module (schematic ffi gtk)
  #:use-module (system foreign)
  #:use-module (lepton ffi lff)
  #:use-module (lepton ffi)

  #:export (gtk_init
            gtk_main_iteration
            gtk_main_level
            gtk_main_quit
            gtk_accelerator_parse
            gtk_accelerator_name
            gtk_accelerator_get_label
            gtk_events_pending
            gtk_rc_parse
            gtk_icon_theme_get_default
            gtk_icon_theme_append_search_path
            gtk_window_set_default_icon_name
            gtk_tearoff_menu_item_new
            gtk_menu_item_new_with_mnemonic
            gtk_widget_destroy
            gtk_widget_show
            gtk_menu_new
            gtk_menu_bar_new
            gtk_menu_item_set_submenu
            gtk_menu_shell_append
            gtk_notebook_get_n_pages
            gtk_notebook_page_num
            gtk_notebook_set_current_page
            gtk_notebook_set_tab_reorderable
            gtk_scrolled_window_new))

;;; Simplify definition of functions by omitting the library
;;; argument.
(define-syntax-rule (define-lff arg ...)
  (define-lff-lib arg ... libgtk))


(define-lff gtk_accelerator_parse void '(* * *))
(define GdkModifierType uint32)
(define-lff gtk_accelerator_name '* (list int GdkModifierType))
(define-lff gtk_accelerator_get_label '* (list int GdkModifierType))

(define-lff gtk_events_pending int '())

(define-lff gtk_icon_theme_append_search_path void '(* *))
(define-lff gtk_icon_theme_get_default '* '())

(define-lff gtk_init void '(* *))

(define-lff gtk_main_iteration int '())
(define-lff gtk_main_level int '())
(define-lff gtk_main_quit void '())

(define-lff gtk_menu_item_new_with_mnemonic '* '(*))
(define-lff gtk_menu_new '* '())
(define-lff gtk_menu_bar_new '* '())
(define-lff gtk_menu_item_set_submenu void '(* *))
(define-lff gtk_menu_shell_append void '(* *))

(define-lff gtk_notebook_get_n_pages int '(*))
(define-lff gtk_notebook_page_num int '(* *))
(define-lff gtk_notebook_set_current_page void (list '* int))
(define-lff gtk_notebook_set_tab_reorderable void (list '* '* int))

(define-lff gtk_rc_parse void '(*))

(define-lff gtk_scrolled_window_new '* '(* *))

(define-lff gtk_tearoff_menu_item_new '* '())

(define-lff gtk_widget_destroy void '(*))
(define-lff gtk_widget_show void '(*))

(define-lff gtk_window_set_default_icon_name void '(*))
