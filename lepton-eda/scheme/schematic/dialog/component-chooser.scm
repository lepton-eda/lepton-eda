;;; Lepton EDA Schematic Capture
;;; Scheme API
;;; Copyright (C) 2026 Lepton EDA Contributors
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


(define-module (schematic dialog component-chooser)
  #:use-module (system foreign)

  #:use-module (lepton ffi boolean)
  #:use-module (lepton ffi gobject)
  #:use-module (lepton gettext)
  #:use-module (lepton m4)

  #:use-module (schematic ffi gtk)
  #:use-module (schematic ffi)
  #:use-module (schematic gtk helper)
  #:use-module (schematic window foreign)

  #:export (make-component-chooser-dialog))


(define (make-component-chooser-dialog window)
  "Create and return a new component selection dialog in WINDOW."
  (define *window (check-window window 1))
  (define *main-window (schematic_window_get_main_window *window))
  (define *widget (schematic_compselect_new *window))

  ;; Vertical pane containing preview and attributes.
  (define *vpaned (if %m4-use-gtk3
                      (gtk_paned_new GTK_ORIENTATION_VERTICAL)
                      (gtk_vpaned_new)))
  ;; Horizontal pane containing selection and preview.
  (define *hpaned (if %m4-use-gtk3
                      (gtk_paned_new GTK_ORIENTATION_HORIZONTAL)
                      (gtk_hpaned_new)))
  ;; Notebook for library and in-use views.
  (define *notebook (gtk_notebook_new))
  ;; In-use view.
  (define *in-use-view (schematic_component_chooser_create_inuse_treeview *widget))
  ;; Library view.
  (define *lib-view (schematic_component_chooser_create_lib_treeview *widget))
  ;; Preview area.
  (define *preview-frame
    (gtk_frame_new (string->pointer (G_ "Preview"))))
  ;; Preview widget.
  (define *preview (schematic_preview_new))
  ;; Frame for the list of available attributes.
  (define *attributes-frame
    (gtk_frame_new (string->pointer (G_ "Attributes"))))
  ;; Available attributes tree view.
  (define attributes (schematic_component_chooser_create_attributes_treeview *widget))
  ;; Behavior combo box.
  (define *behavior-combobox (schematic_component_chooser_create_behaviors_combo_box))

  ;; Dialog initialization.
  (gtk_window_set_title *widget
                        (string->pointer (G_ "Add Component")))
  (gtk_window_set_default_size *widget 300 400)

  (gtk_container_set_border_width *hpaned 5)

  (schematic_component_chooser_set_vpaned *widget *vpaned)
  (schematic_component_chooser_set_hpaned *widget *hpaned)

  (schematic_component_chooser_set_notebook *widget *notebook)

  (gtk_notebook_append_page
   *notebook
   *in-use-view
   (gtk_label_new_with_mnemonic (string->pointer (G_ "In Us_e"))))

  (gtk_notebook_append_page
   *notebook
   *lib-view
   (gtk_label_new_with_mnemonic (string->pointer (G_ "Lib_raries"))))

  ;; Include the vertical box in horizontal box.
  (gtk_paned_pack1 *hpaned *notebook TRUE FALSE)

  (if %m4-use-gtk3
      (begin
        (gtk_container_set_border_width *preview-frame 5)
        (gtk_container_add *preview-frame *preview))
      (let ((*alignment (gtk_alignment_new 0.5 0.5 1.0 1.0)))
        (gtk_container_set_border_width *alignment 5)
        (gtk_container_add *alignment *preview)
        (gtk_container_add *preview-frame *alignment)))

  ;; Set *preview of the Component chooser.
  (schematic_component_chooser_set_preview *widget *preview)
  (gtk_widget_set_size_request *preview 160 120)

  (gtk_paned_pack1 *vpaned *preview-frame FALSE FALSE)

  (schematic_component_chooser_set_attrframe *widget
                                             *attributes-frame)
  (gtk_paned_pack2 *vpaned *attributes-frame FALSE FALSE)
  (gtk_container_add *attributes-frame attributes)

  (gtk_paned_pack2 *hpaned *vpaned FALSE FALSE)

  ;; Add the horizontal paned to dialog's vbox.
  (gtk_box_pack_start (gtk_dialog_get_content_area *widget)
                      *hpaned
                      TRUE
                      TRUE
                      0)
  (gtk_widget_show_all *hpaned)

  (g_signal_connect *behavior-combobox
                    (string->pointer "changed")
                    *schematic_component_chooser_callback_behavior_changed
                    *widget)
  (gtk_box_pack_start (gtk_dialog_get_content_area *widget)
                      *behavior-combobox
                      FALSE
                      FALSE
                      10)
  (gtk_widget_show_all *behavior-combobox)
  ;; Set the Behavior combo box of the dialog.
  (schematic_component_chooser_set_combobox_behaviors *widget
                                                      *behavior-combobox)
  ;; Add buttons in the action area.
  (gtk_dialog_add_button *widget
                         (string->pointer (G_ "_Close"))
                         (symbol->gtk-response 'close))
  (gtk_dialog_add_button *widget
                         (string->pointer (G_ "_OK"))
                         (symbol->gtk-response 'hide))

  (gtk_window_set_transient_for *widget *main-window)

  (gtk_widget_show *widget)

  *widget)
