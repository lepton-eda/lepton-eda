/* Lepton EDA Schematic Capture
 * Copyright (C) 2017-2019 dmn <graahnul.grom@gmail.com>
 * Copyright (C) 2017-2022 Lepton EDA Contributors
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */

/*!
 * \file x_widgets.c
 *
 * \brief Widgets management
 *
 * lepton-schematic widgets:
 *
 * - object properties    (GschemObjectPropertiesWidget)  [right dock]
 * - text properties      (GschemTextPropertiesWidget)    [right dock]
 * - options              (GschemOptionsWidget)           [right dock]
 *
 * - find text results    (GschemFindTextState)           [bottom dock]
 * - log                  (GschemLogWidget)               [bottom dock]
 *
 * - page manager         (PageSelectWidget)
 * - font selector        (FontSelectWidget)
 * - color scheme editor  (ColorEditWidget)
 *
 */

#include "config.h"
#include "gschem.h"




static gboolean
g_x_widgets_use_docks = FALSE;

static gboolean
g_x_widgets_use_toplevel_windows = FALSE;



static void
x_widgets_show_in_dock (GtkWidget* wbook, GtkWidget* widget);


static void
x_widgets_show_in_dialog (GschemToplevel* w_current,
                          GtkWidget*      widget,
                          GtkWidget**     dialog,
                          const gchar*    title,
                          const gchar*    ini_group);



gboolean x_widgets_use_docks()
{
  return g_x_widgets_use_docks;
}



static gboolean
x_widgets_use_toplevel_windows()
{
  return !x_widgets_use_docks() && g_x_widgets_use_toplevel_windows;
}



/*! \brief Initialize widgets management
 *
 * \par Function Description
 *
 * Read widgets configuration.
 * Call this before any other functions from this file.
 *
 * Configuration settings for widgets:
 *
 * key:   use-docks
 * group: schematic.gui
 * type:  boolean
 * default value: false
 * description: whether to use docking GUI
 *
 * key:   use-toplevel-windows
 * group: schematic.gui
 * type:  boolean
 * default value: false
 * description: when docking GUI is off, whether to display
 *              widgets as toplevel windows (true) or
 *              dialogs (false)
 */
void x_widgets_init()
{
  static gsize initialized = 0;

  if (g_once_init_enter (&initialized))
  {
    cfg_read_bool ("schematic.gui", "use-docks",
                   FALSE, &g_x_widgets_use_docks);

    cfg_read_bool ("schematic.gui", "use-toplevel-windows",
                   FALSE, &g_x_widgets_use_toplevel_windows);

    g_once_init_leave (&initialized, 1);
  }
}



void x_widgets_create (GschemToplevel* w_current)
{
  g_return_if_fail (w_current != NULL);

  w_current->find_text_state =
      gschem_find_text_state_new();

  g_signal_connect (w_current->find_text_state, "select-object",
                    G_CALLBACK (&x_window_select_object), w_current);

} /* x_widgets_create() */



void x_widgets_show_options (GschemToplevel* w_current)
{
  g_return_if_fail (w_current != NULL);

  if (x_widgets_use_docks())
  {
    x_widgets_show_in_dock (w_current->right_notebook,
                            w_current->options_widget);
  }
  else
  {
    x_widgets_show_in_dialog (w_current,
                              w_current->options_widget,
                              &w_current->options_widget_dialog,
                              _("Options"),
                              "options");
  }
}



void x_widgets_show_text_properties (GschemToplevel* w_current)
{
  g_return_if_fail (w_current != NULL);

  if (x_widgets_use_docks())
  {
    x_widgets_show_in_dock (w_current->right_notebook,
                            w_current->text_properties);
  }
  else
  {
    x_widgets_show_in_dialog (w_current,
                              w_current->text_properties,
                              &w_current->text_properties_dialog,
                              _("Edit Text"),
                              "txtprops");
  }

  gschem_text_properties_widget_adjust_focus(
    GSCHEM_TEXT_PROPERTIES_WIDGET (w_current->text_properties));
}



void x_widgets_show_object_properties (GschemToplevel* w_current)
{
  g_return_if_fail (w_current != NULL);

  if (x_widgets_use_docks())
  {
    x_widgets_show_in_dock (w_current->right_notebook,
                            w_current->object_properties);
  }
  else
  {
    x_widgets_show_in_dialog (w_current,
                              w_current->object_properties,
                              &w_current->object_properties_dialog,
                              _("Object Properties"),
                              "objprops");
  }
}



void x_widgets_show_log (GschemToplevel* w_current)
{
  g_return_if_fail (w_current != NULL);

  if (x_widgets_use_docks())
  {
    x_widgets_show_in_dock (w_current->bottom_notebook,
                            w_current->log_widget);
  }
  else
  {
    x_widgets_show_in_dialog (w_current,
                              GTK_WIDGET (w_current->log_widget),
                              &w_current->log_widget_dialog,
                              _("Log"),
                              "log");
  }
}



void x_widgets_show_find_text_state (GschemToplevel* w_current)
{
  g_return_if_fail (w_current != NULL);

  if (x_widgets_use_docks())
  {
    x_widgets_show_in_dock (w_current->bottom_notebook,
                            w_current->find_text_state);
  }
  else
  {
    x_widgets_show_in_dialog (w_current,
                              GTK_WIDGET (w_current->find_text_state),
                              &w_current->find_text_state_dialog,
                              _("Find Text"),
                              "findtext");
  }
}



void x_widgets_show_color_edit (GschemToplevel* w_current)
{
  g_return_if_fail (w_current != NULL);

  x_widgets_show_in_dialog (w_current,
                            GTK_WIDGET (w_current->color_edit_widget),
                            &w_current->color_edit_dialog,
                            _("Color Scheme Editor"),
                            "colored");
}



void x_widgets_show_font_select (GschemToplevel* w_current)
{
  g_return_if_fail (w_current != NULL);

  x_widgets_show_in_dialog (w_current,
                            GTK_WIDGET (w_current->font_select_widget),
                            &w_current->font_select_dialog,
                            _("Select Schematic Font"),
                            "fontsel");
}



void x_widgets_show_page_select (GschemToplevel* w_current)
{
  g_return_if_fail (w_current != NULL);

  x_widgets_show_in_dialog (w_current,
                            GTK_WIDGET (w_current->page_select_widget),
                            &w_current->page_select_dialog,
                            _("Page Manager"),
                            "pagesel");
}




static void
x_widgets_show_in_dock (GtkWidget* wbook, GtkWidget* widget)
{
  g_return_if_fail (wbook != NULL);
  g_return_if_fail (widget != NULL);

  GtkNotebook* nbook = GTK_NOTEBOOK (wbook);

  int page = gtk_notebook_page_num (nbook, widget);
  if (page >= 0)
  {
    gtk_notebook_set_current_page (nbook, page);
    gtk_widget_set_visible (wbook, TRUE);
  }
}



/*! \brief Shows a widget as a dialog box
 *
 *  \param [in]     w_current The toplevel environment.
 *  \param [in]     widget Widget to show
 *  \param [in,out] dialog Dialog which will be a parent of the widget
 *  \param [in]     title  Dialog's title
 *  \param [in]     ini_group Config file section for dialog geometry
 */
static void
x_widgets_show_in_dialog (GschemToplevel* w_current,
                          GtkWidget*      widget,
                          GtkWidget**     dialog,
                          const gchar*    title,
                          const gchar*    ini_group)
{
  g_return_if_fail (widget != NULL);

  if (*dialog != NULL)
  {
    gtk_window_present (GTK_WINDOW (*dialog));
    return;
  }

  GtkWidget* dlg = gschem_dialog_new_with_buttons(
    title,
    GTK_WINDOW (w_current->main_window),
    (GtkDialogFlags) GTK_DIALOG_DESTROY_WITH_PARENT,
    ini_group,
    w_current,
    _("_Close"), GTK_RESPONSE_NONE,
    NULL);

  if (x_widgets_use_toplevel_windows())
  {
    gtk_window_set_transient_for (GTK_WINDOW (dlg), NULL);
    gtk_window_set_type_hint (GTK_WINDOW (dlg), GDK_WINDOW_TYPE_HINT_NORMAL);
  }

  g_signal_connect (G_OBJECT (dlg),
                    "response",
                    G_CALLBACK (&gtk_widget_hide),
                    NULL);

  g_signal_connect (G_OBJECT (dlg),
                    "delete-event",
                    G_CALLBACK (&gtk_widget_hide_on_delete),
                    NULL);

  GtkWidget* content_area = gtk_dialog_get_content_area (GTK_DIALOG (dlg));
  gtk_container_add (GTK_CONTAINER (content_area), widget);

  gtk_widget_show_all (dlg);
  gtk_window_present (GTK_WINDOW (dlg));

  *dialog = dlg;

} /* x_widgets_show_in_dialog() */



/*! \brief Destroy dialogs created in "toplevel" mode
 *
 *  \param [in] w_current  The toplevel environment.
 */
void
x_widgets_destroy_dialogs (GschemToplevel* w_current)
{
  g_return_if_fail (w_current != NULL);

  if (!x_widgets_use_toplevel_windows())
    return;

  if (w_current->options_widget_dialog != NULL)
  {
    gtk_widget_destroy (w_current->options_widget_dialog);
    w_current->options_widget_dialog = NULL;
  }

  if (w_current->text_properties_dialog != NULL)
  {
    gtk_widget_destroy (w_current->text_properties_dialog);
    w_current->text_properties_dialog = NULL;
  }

  if (w_current->object_properties_dialog != NULL)
  {
    gtk_widget_destroy (w_current->object_properties_dialog);
    w_current->object_properties_dialog = NULL;
  }

  if (w_current->log_widget_dialog != NULL)
  {
    gtk_widget_destroy (w_current->log_widget_dialog);
    w_current->log_widget_dialog = NULL;
  }

  if (w_current->find_text_state_dialog != NULL)
  {
    gtk_widget_destroy (w_current->find_text_state_dialog);
    w_current->find_text_state_dialog = NULL;
  }

  if (w_current->color_edit_dialog != NULL)
  {
    gtk_widget_destroy (w_current->color_edit_dialog);
    w_current->color_edit_dialog = NULL;
  }

  if (w_current->font_select_dialog != NULL)
  {
    gtk_widget_destroy (w_current->font_select_dialog);
    w_current->font_select_dialog = NULL;
  }

  if (w_current->page_select_dialog != NULL)
  {
    gtk_widget_destroy (w_current->page_select_dialog);
    w_current->page_select_dialog = NULL;
  }

} /* x_widgets_destroy_dialogs() */


void
x_widgets_toggle_widget_visibility (GtkWidget *widget)
{
  gboolean visible = gtk_widget_get_visible (widget);
  gtk_widget_set_visible (widget, !visible);
}
