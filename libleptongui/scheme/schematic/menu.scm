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

;;; Procedures for working with menus.

(define-module (schematic menu)
  #:use-module (system foreign)
  #:use-module (srfi srfi-1)

  #:use-module (lepton config)
  #:use-module (lepton eval)
  #:use-module (lepton gettext)
  #:use-module (lepton log)
  #:use-module (schematic ffi gobject)
  #:use-module (schematic ffi gtk)
  #:use-module (schematic ffi)

  #:export (add-menu
            make-main-menu))

(define %main-menu-list '())

(define (main-menu-list)
  %main-menu-list)

(define (set-main-menu-list! ls)
  (set! %main-menu-list ls)
  %main-menu-list)

(define (add-menu name items)
  (and (string? name)
       (list? items)
       (set-main-menu-list!
        (append %main-menu-list (list (cons name items))))))


(define (make-main-menu window)
  "Create and return the main menu widget for WINDOW."
  (define (make-menu-action-item func name stock menu-bar)
    (if (not func)
        (gtk_menu_item_new_with_mnemonic (string->pointer name))
        ;; else
        (let* ((scheme-expression `(find-key (quote ,func)))
               ;; Look up key binding in global keymap
               (keys (eval-protected scheme-expression
                                     (interaction-environment)))
               (menu-item-keys (if (not keys) "" keys))
               (menu-item-stock
                (if (not stock) %null-pointer (string->pointer stock)))
               (action-name (string->pointer (symbol->string func)))
               (action (make_menu_action action-name
                                         (string->pointer name)
                                         (string->pointer menu-item-keys)
                                         menu-item-stock
                                         window))
               (menu-item (lepton_action_create_menu_item action
                                                          (string->pointer name)
                                                          (string->pointer menu-item-keys))))
          (lepton_menu_set_action_data menu-bar action-name menu-item action)
          menu-item)))

  (define (item->menu-item item menu-bar)
    ;; check if [item] is valid it must be a
    ;; ( list TEXT ACTION ICON ):
    (and (list? item)
         (= 3 (length item))

         (let* ((raw-name (first item))
                (item-func (second item))
                (item-stock (third item))
                (item-name (gettext raw-name))
                (menu-item
                 (if (string=? item-name "SEPARATOR")
                     (make_separator_menu_item)
                     ;; not separator
                     (make-menu-action-item item-func
                                            item-name
                                            item-stock
                                            menu-bar))))
           menu-item)))

  (define RECENT_MENU_ITEM_NAME "Open Recen_t")

  (define (get-max-recent-files)
    (catch 'config-error
      (lambda ()
        (config-int (path-config-context (getcwd))
                    "schematic.gui"
                    "max-recent-files"))
      (lambda (key subr message args rest)
        (log! 'warning (G_ "ERROR: ~?.\n") message args)
        ;; Default value.
        10)))

  (define (append-menu-item raw-name menu menu-item window)
    (and menu-item
         (gtk_menu_shell_append menu menu-item)
         (gtk_widget_show menu-item))
    (when (string= raw-name RECENT_MENU_ITEM_NAME)
      (x_menu_attach_recent_files_submenu
       window
       menu-item
       *recent_chooser_item_activated
       ;; Set maximum number of recent files from config.
       (get-max-recent-files))))

  (define (append-tearoff-menu-item menu)
    (let ((tearoff-menu-item (gtk_tearoff_menu_item_new)))
      (gtk_menu_shell_append menu tearoff-menu-item)
      (gtk_widget_show tearoff-menu-item)))

  (define (make-root-menu menu-name submenu)
    (let ((root-menu
           (gtk_menu_item_new_with_mnemonic (string->pointer menu-name))))
      (gtk_widget_show root-menu)
      (gtk_menu_item_set_submenu root-menu submenu)
      root-menu))

  (define (section-list->root-menu menu-section-list menu-bar)
    (let ((submenu (gtk_menu_new))
          (menu-name (gettext (car menu-section-list)))
          (items (cdr menu-section-list)))

      (append-tearoff-menu-item submenu)

      ;; cycle through submenu items:
      (for-each
       (lambda (item)
         (and (list? item)
              (= (length item) 3)
              (append-menu-item (first item)
                                submenu
                                (item->menu-item item menu-bar)
                                window)))
       items)
      (make-root-menu menu-name submenu)))

  (let ((menu-bar (gtk_menu_bar_new)))

    (map
     (lambda (menu-section-list)
       (gtk_menu_shell_append menu-bar
                              (section-list->root-menu menu-section-list
                                                       menu-bar)))
     (main-menu-list))

    ;; return
    menu-bar))
