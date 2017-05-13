/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 * Copyright (C) 1998-2016 Ales Hvezda
 * Copyright (C) 2016 gEDA Contributors (see ChangeLog for details)
 * Copyright (C) 2016-2017 dmn
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
 * There are 5 of them:
 * - in right dock:
 *   - obj props
 *   - txt props
 *   - options
 * - in bottom dock:
 *   - find text results
 *   - log
 */

#include "gschem.h"




static void
x_widgets_show_in_dock (GtkWidget* wbook, GtkWidget* widget);




void x_widgets_show_options (GschemToplevel* w_current)
{
  g_return_if_fail (w_current != NULL);

  x_widgets_show_in_dock (w_current->right_notebook,
                          w_current->options_widget);
}



void x_widgets_show_text_properties (GschemToplevel* w_current)
{
  g_return_if_fail (w_current != NULL);

  x_widgets_show_in_dock (w_current->right_notebook,
                          w_current->text_properties);

  gschem_text_properties_widget_adjust_focus(
    GSCHEM_TEXT_PROPERTIES_WIDGET (w_current->text_properties));
}



void x_widgets_show_object_properties (GschemToplevel* w_current)
{
  g_return_if_fail (w_current != NULL);

  x_widgets_show_in_dock (w_current->right_notebook,
                          w_current->object_properties);
}



void x_widgets_show_log (GschemToplevel* w_current)
{
  g_return_if_fail (w_current != NULL);

  x_widgets_show_in_dock (w_current->bottom_notebook,
                          GTK_WIDGET (w_current->log_widget));
}



void x_widgets_show_find_text_state (GschemToplevel* w_current)
{
  g_return_if_fail (w_current != NULL);

  x_widgets_show_in_dock (w_current->bottom_notebook,
                          GTK_WIDGET (w_current->find_text_state));
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
    gtk_widget_set_visible (GTK_WIDGET (nbook), TRUE);
  }
}
