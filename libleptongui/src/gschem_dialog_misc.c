/* Lepton EDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2014 gEDA Contributors
 * Copyright (C) 2017-2021 Lepton EDA Contributors
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
 * \file gschem_dialog_misc.c
 *
 * \brief Common dialog functions
 */

#include <config.h>

#include <stdio.h>
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include "gschem.h"
#include <gdk/gdkkeysyms.h>



/*! \brief Create a property label widget
 *
 *  \param [in] label The label text for this property
 *  \return A new property label widget
 */
GtkWidget*
gschem_dialog_misc_create_property_label (const char *label)
{
  GtkWidget *widget = gtk_label_new_with_mnemonic (label);

  gtk_misc_set_alignment (GTK_MISC (widget),
                          0.0,                  /* xalign */
                          0.0);                 /* yalign */

  return widget;
}



/*! \brief Create a property table
 *
 *  \par Function Description
 *
 *  If \a label[i] is GtkLabel and has a mnemonic key
 *  in its text, corresponding \a widget[i] will be set
 *  as a mnemonic widget for the label
 *
 *  \param [in] label The array of label widgets
 *  \param [in] widget The array of widgets
 *  \param [in] count The number of rows in the table
 *  \return A new property table
 */
GtkWidget*
gschem_dialog_misc_create_property_table (GtkWidget *label[], GtkWidget *widget[], int count)
{
  int index;
#ifdef ENABLE_GTK3
  GtkWidget *grid = gtk_grid_new ();

  gtk_grid_set_row_spacing (GTK_GRID (grid), DIALOG_V_SPACING);
  gtk_grid_set_column_spacing (GTK_GRID (grid), DIALOG_H_SPACING);

  for (index=0; index<count; index++) {
    gtk_grid_attach (GTK_GRID (grid),
                     label[index],
                     0,
                     index,
                     1,
                     1);

    gtk_grid_attach (GTK_GRID (grid),
                     widget[index],
                     1,
                     index,
                     1,
                     1);

    if (GTK_IS_LABEL (label[index]) &&
        gtk_label_get_mnemonic_keyval (GTK_LABEL (label[index])) != GDK_KEY_VoidSymbol)
    {
      gtk_label_set_mnemonic_widget (GTK_LABEL (label[index]), widget[index]);
    }
  }

  return grid;

#else /* GTK2 */

  GtkWidget *table = gtk_table_new (count, 2, FALSE);

  gtk_table_set_row_spacings (GTK_TABLE (table), DIALOG_V_SPACING);
  gtk_table_set_col_spacings (GTK_TABLE (table), DIALOG_H_SPACING);

  for (index=0; index<count; index++) {
    gtk_table_attach (GTK_TABLE (table),
                      label[index],      /* child         */
                      0,                 /* left_attach   */
                      1,                 /* right_attach  */
                      index,             /* top_attach    */
                      index+1,           /* bottom_attach */
                      GTK_FILL,          /* xoptions      */
                      (GtkAttachOptions) 0, /* yoptions      */
                      0,                 /* xpadding      */
                      0);                /* ypadding      */

    gtk_table_attach_defaults (GTK_TABLE (table),
                               widget[index],     /* child         */
                               1,                 /* left_attach   */
                               2,                 /* right_attach  */
                               index,             /* top_attach    */
                               index+1);          /* bottom_attach */

    if (GTK_IS_LABEL (label[index]) &&
        gtk_label_get_mnemonic_keyval (GTK_LABEL (label[index])) != GDK_KEY_VoidSymbol)
    {
      gtk_label_set_mnemonic_widget (GTK_LABEL (label[index]), widget[index]);
    }
  }

  return table;
#endif
}


/*! \brief Create a section widget
 *
 *  Creates a widget to represent a section in the property editor. This
 *  function wraps the child widget with additional widgets to generate the
 *  proper layout.
 *
 *  \param [in] label The markup text for this section
 *  \param [in] child The child widget for this section
 *  \return A new section widget
 */
GtkWidget*
gschem_dialog_misc_create_section_widget (const char *label, GtkWidget *child)
{
  GtkWidget *alignment;
  GtkWidget *expander;

  alignment = gtk_alignment_new (0.0,     /* xalign */
                                 0.0,     /* yalign */
                                 1.0,     /* xscale */
                                 1.0);    /* yscale */

  gtk_alignment_set_padding (GTK_ALIGNMENT(alignment),
                            0,                     /* padding_top    */
                            0,                     /* padding_bottom */
                            DIALOG_INDENTATION,    /* padding_left   */
                            0);                    /* padding_right  */

  gtk_container_add (GTK_CONTAINER (alignment), child);

  expander = gtk_expander_new (label);

#ifdef ENABLE_GTK3
  gtk_widget_set_vexpand (expander, TRUE);
#endif

  gtk_expander_set_expanded (GTK_EXPANDER (expander), TRUE);
  gtk_expander_set_spacing (GTK_EXPANDER (expander), DIALOG_V_SPACING);
  gtk_expander_set_use_markup (GTK_EXPANDER (expander), TRUE);

  gtk_container_add (GTK_CONTAINER (expander), alignment);

  return expander;
}



/*! \brief A signal handler for when the user presses enter in an entry
 *
 *  Pressing the enter key in an entry moves the focus to the next control
 *  in the column of values and applies the current value. This function
 *  moves the focus to the next control, and the focus-out-event applies the
 *  value.
 *
 *  This signal hander operates for multiple entry widgets.
 *
 *  \param [in] widget The widget emitting the event
 *  \param [in] dialog The dialog containing the widget
 */
void
gschem_dialog_misc_entry_activate (GtkWidget *widget, GtkDialog *dialog)
{
  g_return_if_fail (dialog != NULL);

  gtk_widget_child_focus (GTK_WIDGET (dialog), GTK_DIR_DOWN);
}



/*! \brief Handles user responses from non-modal dialogs
 *
 *  Destroys the non-modal dialog upon any user response.
 *
 *  Relies on the caller using signals or weak references to know when the
 *  dialog is destroyed.
 *
 *  \param [in,out] dialog   The non-modal dialog
 *  \param [in]     response The id of the user response
 *  \param [na]     unused   An unused parameter
 */
void
gschem_dialog_misc_response_non_modal (GtkDialog *dialog, gint response, gpointer unused)
{
  switch(response) {
    case GTK_RESPONSE_CLOSE:
    case GTK_RESPONSE_DELETE_EVENT:
       break;

    default:
      printf("gtk_dialog_misc_response_non_modal(): strange signal %1$d\n", response);
  }

  gtk_widget_destroy (GTK_WIDGET (dialog));
}



/*! \brief Creates and/or shows a non-modal dialog
 *
 *  \param [in,out] w_current The toplevel
 *  \param [in]     widget    Where the result dialog pointer is placed
 *  \param [na]     create    A pointer to the function that creates the dialog
 */
void
gschem_dialog_misc_show_non_modal (GschemToplevel *w_current, GtkWidget **widget, CreateNonModalDialog create)
{
  g_return_if_fail (create != NULL);
  g_return_if_fail (w_current != NULL);
  g_return_if_fail (widget != NULL);

  if (*widget == NULL) {
    *widget = GTK_WIDGET (create (w_current));

    g_object_add_weak_pointer (G_OBJECT (*widget), (void**) widget);

    g_signal_connect (G_OBJECT (*widget),
                      "response",
                      G_CALLBACK (gschem_dialog_misc_response_non_modal),
                      NULL);

    gtk_window_set_transient_for (GTK_WINDOW (*widget),
                                  GTK_WINDOW (w_current->main_window));

    gtk_widget_show_all (*widget);
  }
  else {
    gtk_window_present (GTK_WINDOW (*widget));
  }
}
