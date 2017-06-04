/* Lepton EDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2016 gEDA Contributors (see ChangeLog for details)
 * Copyright (C) 2017 dmn <graahnul.grom@gmail.com>
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
 * Now there are 5 of them:
 * - in right dock:
 *   - obj props
 *   - txt props
 *   - options
 * - in bottom dock:
 *   - find text results
 *   - log
 */

#include "gschem.h"




static gboolean
g_x_widgets_use_docks = TRUE;



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



/*! \par Function Description
 *
 * Initialize widgets management.
 * Call this before any other functions from this file.
 * This function reads the value of "use-docks" configuration
 * setting in "gschem.gui" group, which determines
 * if widgets will be shown in docks (if true) or as
 * a dialog boxes (if false).
 *
 * Configuration setting description:
 * key:   use-docks
 * group: gschem.gui
 * type:  boolean
 * default value: true
 *
 * \return TRUE if use-docks options is set to true, FALSE otherwise.
 */
void x_widgets_init()
{
  gchar* cwd = g_get_current_dir();

  EdaConfig* cfg = eda_config_get_context_for_path (cwd);

  g_free (cwd);

  if (cfg != NULL)
  {
    GError* err = NULL;
    gboolean val = eda_config_get_boolean (cfg,
                                           "gschem.gui",
                                           "use-docks",
                                           &err);
    if (err == NULL)
    {
      g_x_widgets_use_docks = val;
    }

    g_clear_error (&err);
  }
}



void x_widgets_create (GschemToplevel* w_current)
{
  g_return_if_fail (w_current != NULL);

  w_current->object_properties =
      gschem_object_properties_widget_new (w_current);

  w_current->text_properties =
      gschem_text_properties_widget_new (w_current);

  w_current->options_widget =
      gschem_options_widget_new (w_current);


  w_current->log_widget =
      gschem_log_widget_new();

  w_current->find_text_state =
      gschem_find_text_state_new();

  g_signal_connect (w_current->find_text_state, "select-object",
                    G_CALLBACK (&x_window_select_object), w_current);

  if (x_widgets_use_docks())
  {
    gtk_widget_set_size_request (GTK_WIDGET (w_current->find_text_state),
                                 default_width, default_height / 4);
  }
}



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
                              _("Text"),
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
                              _("Object"),
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
    0,
    ini_group,
    w_current,
    GTK_STOCK_CLOSE, GTK_RESPONSE_NONE,
    NULL);

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
