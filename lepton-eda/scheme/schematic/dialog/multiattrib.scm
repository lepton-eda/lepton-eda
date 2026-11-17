;;; Lepton EDA Schematic Capture
;;; Scheme API
;;; Copyright (C) 2024-2026 Lepton EDA Contributors
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


(define-module (schematic dialog multiattrib)
  #:use-module (system foreign)

  #:use-module (lepton ffi boolean)
  #:use-module (lepton ffi gobject)
  #:use-module (lepton gettext)
  #:use-module (lepton m4)

  #:use-module (schematic ffi gtk)
  #:use-module (schematic ffi)
  #:use-module (schematic gtk helper)
  #:use-module (schematic window foreign)

  #:export (multiattrib-dialog))

;;; Process the "response" signal returned by the Multiattrib
;;; widget.
(define (response-callback *widget response-id *window)
  (define response (gtk-response->symbol response-id))
  (when (or (eq? response 'close)
            (eq? response 'delete-event))
    (gtk_widget_hide *widget)))

(define *response-callback
  (procedure->pointer void response-callback (list '* int '*)))


(define (delete-event-callback *widget *event *window)
  (if (null-pointer? *window)
      (error "NULL window.")
      (gtk_widget_hide *widget))
  ;; Stop further propagation of the "delete-event" signal for the
  ;; widget.  Otherwise the widget will be destroyed in the code
  ;; of the parent GtkDialog structure.
  TRUE)

;;; The signal is emitted when you click the close button on the
;;; widget dialog window or hit Escape.
(define *delete-event-callback
  (procedure->pointer int delete-event-callback '(* * *)))


;;; Create a new Multiattrib dialog widget.
(define (make-multiattrib-dialog *window *selection)
  (define *dialog
    (schematic_multiattrib_widget_new *window *selection))
  ;; Create the attribute list frame.
  (define *list-frame (gtk_frame_new %null-pointer))
  ;; Create the model for the treeview.
  (define *store (schematic_multiattrib_widget_list_store_new))
  ;; Create a scrolled window for the treeview.
  (define *scrolled-window
    (gtk_scrolled_window_new %null-pointer %null-pointer))
  ;; Create the treeview.
  (define *tree-view (gtk_tree_view_new_with_model *store))
  ;; The columns of the treeview.
  ;;
  ;; Column 1: attribute name.
  (define *name-column-renderer (gtk_cell_renderer_text_new))
  (define *name-column (gtk_tree_view_column_new))
  ;; Column 2: attribute value.
  (define *value-column-renderer
    (schematic_multiattrib_widget_cell_renderer_multi_line_text_new))
  (define *value-column (gtk_tree_view_column_new))
  ;; Column 3: visibility.
  (define *visibility-renderer (gtk_cell_renderer_toggle_new))
  (define *visibility-column (gtk_tree_view_column_new))
  ;; Column 4: show name.
  (define *show-name-renderer (gtk_cell_renderer_toggle_new))
  (define *show-name-column (gtk_tree_view_column_new))
  ;; Column 5: show value.
  (define *show-value-renderer (gtk_cell_renderer_toggle_new))
  (define *show-value-column (gtk_tree_view_column_new))
  ;; Vertical box in the attribute list frame.
  (define *attrib-vbox
    (if %m4-use-gtk3
        (gtk_box_new GTK_ORIENTATION_VERTICAL 0)
        (gtk_vbox_new FALSE 0)))

  (gtk_frame_set_shadow_type *list-frame
                             (symbol->gtk-shadow-type 'none))
  (schematic_multiattrib_widget_set_list_frame *dialog
                                               *list-frame)

  (gtk_container_set_border_width *scrolled-window 3)
  (gtk_scrolled_window_set_policy *scrolled-window
                                  GTK_POLICY_AUTOMATIC
                                  GTK_POLICY_AUTOMATIC)
  (gtk_scrolled_window_set_shadow_type *scrolled-window
                                       (symbol->gtk-shadow-type 'etched-in))

  (unless %m4-use-gtk3
    (gtk_tree_view_set_rules_hint *tree-view TRUE))

  (g_signal_connect *tree-view
                    (string->pointer "key-press-event")
                    *schematic_multiattrib_widget_callback_key_pressed
                    *dialog)
  (g_signal_connect *tree-view
                    (string->pointer "button-press-event")
                    *schematic_multiattrib_widget_callback_button_pressed
                    *dialog)
  (g_signal_connect *tree-view
                    (string->pointer "popup-menu")
                    *schematic_multiattrib_widget_callback_popup_menu
                    *dialog)

  (let ((*selection (gtk_tree_view_get_selection *tree-view)))
    (gtk_tree_selection_set_mode *selection
                                 GTK_SELECTION_SINGLE))

  (g_signal_connect *name-column-renderer
                    (string->pointer "edited")
                    *schematic_multiattrib_widget_callback_edited_name
                    *dialog)
  (gtk_tree_view_column_set_title *name-column
                                  (string->pointer (G_ "Name")))
  (gtk_tree_view_column_set_min_width *name-column 100)
  (gtk_tree_view_column_set_resizable *name-column TRUE)
  (gtk_tree_view_column_pack_start *name-column
                                   *name-column-renderer
                                   TRUE)
  (gtk_tree_view_column_set_cell_data_func
   *name-column
   *name-column-renderer
   *schematic_multiattrib_widget_column_set_data_name
   *dialog
   %null-pointer)
  (gtk_tree_view_append_column *tree-view *name-column)

  (g_signal_connect *value-column-renderer
                    (string->pointer "edited")
                    *schematic_multiattrib_widget_callback_edited_value
                    *dialog)
  (gtk_tree_view_column_set_title *value-column
                                  (string->pointer (G_ "Value")))
  (gtk_tree_view_column_set_min_width *value-column 140)
  (gtk_tree_view_column_set_resizable *value-column TRUE)
  (gtk_tree_view_column_pack_start *value-column
                                   *value-column-renderer
                                   TRUE)
  (gtk_tree_view_column_set_cell_data_func
   *value-column
   *value-column-renderer
   *schematic_multiattrib_widget_column_set_data_value
   *dialog
   %null-pointer)
  (gtk_tree_view_append_column *tree-view *value-column)


  (g_signal_connect *visibility-renderer
                    (string->pointer "toggled")
                    *schematic_multiattrib_widget_callback_toggled_visible
                    *dialog)
  (gtk_tree_view_column_set_title *visibility-column
                                  (string->pointer (G_ "Vis?")))
  (gtk_tree_view_column_pack_start *visibility-column
                                   *visibility-renderer
                                   TRUE)
  (gtk_tree_view_column_set_cell_data_func
   *visibility-column
   *visibility-renderer
   *schematic_multiattrib_widget_column_set_data_visible
   *dialog
   %null-pointer)

  (gtk_tree_view_append_column *tree-view *visibility-column)

  (g_signal_connect *show-name-renderer
                    (string->pointer "toggled")
                    *schematic_multiattrib_widget_callback_toggled_show_name
                    *dialog)
  (gtk_tree_view_column_set_title *show-name-column
                                  (string->pointer (G_ "N")))
  (gtk_tree_view_column_pack_start *show-name-column
                                   *show-name-renderer
                                   TRUE)
  (gtk_tree_view_column_set_cell_data_func
   *show-name-column
   *show-name-renderer
   *schematic_multiattrib_widget_column_set_data_show_name
   %null-pointer
   %null-pointer)

  (gtk_tree_view_append_column *tree-view *show-name-column)

  (g_signal_connect *show-value-renderer
                    (string->pointer "toggled")
                    *schematic_multiattrib_widget_callback_toggled_show_value
                    *dialog)

  (gtk_tree_view_column_set_title *show-value-column
                                  (string->pointer (G_ "V")))
  (gtk_tree_view_column_pack_start *show-value-column
                                   *show-value-renderer
                                   TRUE)
  (gtk_tree_view_column_set_cell_data_func
   *show-value-column
   *show-value-renderer
   *schematic_multiattrib_widget_column_set_data_show_value
   %null-pointer
   %null-pointer)

  (gtk_tree_view_append_column *tree-view *show-value-column)

  ;; Add the tree view to the scrolled window.
  (gtk_container_add *scrolled-window *tree-view)
  ;; Set tree view of the dialog.
  (schematic_multiattrib_widget_set_tree_view *dialog *tree-view)

  ;; Pack the attrib vbox into the frame.
  (gtk_container_add *list-frame *attrib-vbox)
  ;; Add the scrolled window to the box.
  (gtk_box_pack_start *attrib-vbox *scrolled-window TRUE TRUE 0)

  (schematic_multiattrib_widget_setup *dialog
                                      *list-frame
                                      *store
                                      *scrolled-window
                                      *tree-view
                                      *attrib-vbox)

  ;; Now add the close button to the action area.
  (gtk_dialog_add_button *dialog
                         (string->pointer (G_ "_Close"))
                         GTK_RESPONSE_CLOSE)
  (schematic_multiattrib_widget_refresh *dialog)

  (gtk_box_set_spacing (gtk_dialog_get_content_area *dialog) 5)
  (gtk_window_set_title *dialog
                        (string->pointer (G_ "Edit Attributes")))
  (gtk_container_set_border_width *dialog 0)
  (gtk_window_set_default_size *dialog 320 350)
  (gtk_window_set_position *dialog
                           (symbol->gtk-window-position 'mouse))

  (schematic_multiattrib_widget_set_object_list *dialog *selection)

  *dialog)


(define (multiattrib-dialog window)
  "Open multiple attribute editor dialog to edit selected objects in
WINDOW."
  (define *window (check-window window 1))
  (define *widget (schematic_window_get_multiattrib_widget *window))
  (define *selection (schematic_window_get_selection_list *window))
  (define *main-window (schematic_window_get_main_window *window))

  (if (null-pointer? *widget)
      (let ((*multiattrib (make-multiattrib-dialog *window
                                                   *selection)))
        (schematic_window_set_multiattrib_widget *window *multiattrib)
        (gtk_window_set_transient_for (gtk_widget_get_gtk_window *multiattrib)
                                      (gtk_widget_get_gtk_window *main-window))

        (g_signal_connect *multiattrib
                          (string->pointer "delete-event")
                          *delete-event-callback
                          *window)

        (g_signal_connect *multiattrib
                          (string->pointer "response")
                          *response-callback
                          *window)
        (gtk_widget_show *multiattrib))
      (let ((*dialog-window (gtk_widget_get_gtk_window *widget)))
        (gtk_window_present *dialog-window))))
