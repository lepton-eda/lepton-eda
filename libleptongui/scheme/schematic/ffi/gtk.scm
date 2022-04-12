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
  #:use-module (lepton ffi)

  #:export (gtk_init
            gtk_accelerator_parse
            gtk_accelerator_name
            gtk_accelerator_get_label
            gtk_rc_parse
            gtk_icon_theme_get_default
            gtk_icon_theme_append_search_path
            gtk_window_set_default_icon_name
            gtk_tearoff_menu_item_new
            gtk_menu_item_new_with_mnemonic
            gtk_widget_show
            gtk_menu_new
            gtk_menu_bar_new
            gtk_menu_item_set_submenu
            gtk_menu_shell_append))

(define-syntax define-lff
  (syntax-rules ()
    ((_ name type args)
     (define name
       (let ((proc (delay (pointer->procedure
                           type
                           (dynamic-func (symbol->string (quote name)) libgtk)
                           args))))
         (force proc))))))

(define gtk_init
  (pointer->procedure
   void
   (dynamic-func "gtk_init" libgtk)
   (list '* '*)))

(define (gtk_rc_parse filename)
  (define proc
    (delay (pointer->procedure
            void
            (dynamic-func "gtk_rc_parse" libgtk)
            (list '*))))
  ((force proc) (string->pointer filename)))


(define (gtk_window_set_default_icon_name name)
  (let ((proc (delay
                (pointer->procedure
                 void
                 (dynamic-func "gtk_window_set_default_icon_name"
                               libgtk)
                 (list '*)))))
    ((force proc) (string->pointer name))))


(define (gtk_icon_theme_get_default)
  (define proc
    (delay
      (pointer->procedure
       '*
       (dynamic-func "gtk_icon_theme_get_default" libgtk)
       '())))
  ((force proc)))


(define (gtk_icon_theme_append_search_path icon-theme path)
  (define proc
    (delay
      (pointer->procedure
       void
       (dynamic-func "gtk_icon_theme_append_search_path" libgtk)
       (list '* '*))))
  ((force proc) icon-theme (string->pointer path)))

(define-lff gtk_accelerator_parse void '(* * *))
(define GdkModifierType uint32)
(define-lff gtk_accelerator_name '* (list int GdkModifierType))
(define-lff gtk_accelerator_get_label '* (list int GdkModifierType))
(define-lff gtk_menu_item_new_with_mnemonic '* '(*))
(define-lff gtk_widget_show void '(*))
(define-lff gtk_tearoff_menu_item_new '* '())
(define-lff gtk_menu_new '* '())
(define-lff gtk_menu_bar_new '* '())
(define-lff gtk_menu_item_set_submenu void '(* *))
(define-lff gtk_menu_shell_append void '(* *))
