/* Lepton EDA Schematic Capture
 * Copyright (C) 2013 Ales Hvezda
 * Copyright (C) 2013-2015 gEDA Contributors
 * Copyright (C) 2017-2025 Lepton EDA Contributors
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
 * \file new_text_dialog.c
 *
 * \brief A dialog box for adding new text to a schematic.
 */

#include <config.h>

#include <stdio.h>
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include "schematic.h"
#include <gdk/gdkkeysyms.h>


G_DEFINE_TYPE (SchematicNewText,
               schematic_newtext,
               SCHEMATIC_TYPE_DIALOG);


/*! \brief Get the parent window instance of a Newtext dialog.
 *  \par Function description
 *  Gets the parent #SchematicWindow instance of a
 *  #SchematicNewText dialog.
 *
 *  \param [in] dialog The #SchematicNewText dialog instance.
 *  \return The parent schematic window of the dialog.
 */
SchematicWindow*
schematic_newtext_dialog_get_window (SchematicNewText *dialog)
{
  g_return_val_if_fail (dialog != NULL, NULL);

  SchematicWindow *w_current = NULL;
  g_object_get (SCHEMATIC_DIALOG (dialog),
                "schematic-window", &w_current,
                NULL);
  return w_current;
}


/*! \brief Get the field 'aligncb' of #SchematicNewText instance.
 *
 *  \param [in] dialog The #SchematicNewText instance.
 *  \return The value of the field 'aligncb'.
 */
GtkWidget*
schematic_newtext_dialog_get_aligncb (SchematicNewText *dialog)
{
  g_return_val_if_fail (dialog != NULL, NULL);

  return dialog->aligncb;
}

/*! \brief Set the 'aligncb' field of #SchematicNewText instance.
 *
 *  \param [in] dialog The #SchematicNewText instance.
 *  \param [in] widget The new value of the field 'aligncb'.
 */
void
schematic_newtext_dialog_set_aligncb (SchematicNewText *dialog,
                                      GtkWidget *widget)
{
  g_return_if_fail (dialog != NULL);

  dialog->aligncb = widget;
}


/*! \brief Get the field 'colorcb' of #SchematicNewText instance.
 *
 *  \param [in] dialog The #SchematicNewText instance.
 *  \return The value of the field 'colorcb'.
 */
GtkWidget*
schematic_newtext_dialog_get_colorcb (SchematicNewText *dialog)
{
  g_return_val_if_fail (dialog != NULL, NULL);

  return dialog->colorcb;
}

/*! \brief Set the 'colorcb' field of #SchematicNewText instance.
 *
 *  \param [in] dialog The #SchematicNewText instance.
 *  \param [in] widget The new value of the field 'colorcb'.
 */
void
schematic_newtext_dialog_set_colorcb (SchematicNewText *dialog,
                                      GtkWidget *widget)
{
  g_return_if_fail (dialog != NULL);

  dialog->colorcb = widget;
}


/*! \brief Get the field 'rotatecb' of #SchematicNewText instance.
 *
 *  \param [in] dialog The #SchematicNewText instance.
 *  \return The value of the field 'rotatecb'.
 */
GtkWidget*
schematic_newtext_dialog_get_rotatecb (SchematicNewText *dialog)
{
  g_return_val_if_fail (dialog != NULL, NULL);

  return dialog->rotatecb;
}

/*! \brief Set the 'rotatecb' field of #SchematicNewText instance.
 *
 *  \param [in] dialog The #SchematicNewText instance.
 *  \param [in] widget The new value of the field 'rotatecb'.
 */
void
schematic_newtext_dialog_set_rotatecb (SchematicNewText *dialog,
                                       GtkWidget *widget)
{
  g_return_if_fail (dialog != NULL);

  dialog->rotatecb = widget;
}


/*! \brief Get the field 'textsizecb' of #SchematicNewText instance.
 *
 *  \param [in] dialog The #SchematicNewText instance.
 *  \return The value of the field 'textsizecb'.
 */
GtkWidget*
schematic_newtext_dialog_get_textsizecb (SchematicNewText *dialog)
{
  g_return_val_if_fail (dialog != NULL, NULL);

  return dialog->textsizecb;
}

/*! \brief Set the 'textsizecb' field of #SchematicNewText instance.
 *
 *  \param [in] dialog The #SchematicNewText instance.
 *  \param [in] widget The new value of the field 'textsizecb'.
 */
void
schematic_newtext_dialog_set_textsizecb (SchematicNewText *dialog,
                                         GtkWidget *widget)
{
  g_return_if_fail (dialog != NULL);

  dialog->textsizecb = widget;
}


/*! \brief Get the field 'text_view' of #SchematicNewText instance.
 *
 *  \param [in] dialog The #SchematicNewText instance.
 *  \return The value of the field 'text_view'.
 */
GtkWidget*
schematic_newtext_dialog_get_text_view (SchematicNewText *dialog)
{
  g_return_val_if_fail (dialog != NULL, NULL);

  return dialog->text_view;
}

/*! \brief Set the 'text_view' field of #SchematicNewText instance.
 *
 *  \param [in] dialog The #SchematicNewText instance.
 *  \param [in] widget The new value of the field 'text_view'.
 */
void
schematic_newtext_dialog_set_text_view (SchematicNewText *dialog,
                                        GtkWidget *widget)
{
  g_return_if_fail (dialog != NULL);

  dialog->text_view = widget;
}


static int
text_view_calculate_real_tab_width (GtkTextView *textview, int tab_size)
{
  if (tab_size <= 0)
    return -1;

  gchar* tab_string = g_strnfill (tab_size, ' ');
  PangoLayout* layout =
    gtk_widget_create_pango_layout (GTK_WIDGET (textview), tab_string);
  g_free (tab_string);

  gint tab_width = -1;
  if (layout != NULL)
  {
    pango_layout_get_pixel_size (layout, &tab_width, NULL);
    g_object_unref (G_OBJECT (layout));
  }

  return tab_width;
}



static void
select_all_text_in_textview (GtkTextView *textview)
{
  GtkTextBuffer *textbuffer =
    gtk_text_view_get_buffer (GTK_TEXT_VIEW (textview));

  GtkTextIter start, end;
  gtk_text_buffer_get_bounds (textbuffer, &start, &end);
  gtk_text_buffer_select_range (textbuffer, &start, &end);
}


/*! \brief Get text string from the New text dialog.
 *  \par Function description
 *  Gets the text string from the text view widget of the New text
 *  dialog.
 *
 *  \note The caller must g_free() return value.
 *
 *  \param [in] text_view The widget to get the string from.
 *  \return The text string.
 */
char*
schematic_newtext_dialog_get_text (GtkWidget *text_view)
{
  GtkTextBuffer *textbuffer;
  GtkTextIter start, end;

  textbuffer = gtk_text_view_get_buffer (GTK_TEXT_VIEW (text_view));
  gtk_text_buffer_get_bounds (textbuffer, &start, &end);

  return gtk_text_iter_get_text (&start, &end);
}


/*! \brief Handles the user response when apply is selected
 *
 *  \par Function Description
 *  This function applies the text from the text entry dialog.
 *
 *  \param [in] dialog The new text dialog
 *  \param [in] w_current The pointer to the parent window of the
 *              dialog.
 */
void
schematic_newtext_dialog_response_apply (SchematicNewText *dialog,
                                         SchematicWindow *w_current)
{
  int size = 0;
  int align = LOWER_LEFT;
  int color = TEXT_COLOR;
  int rotate = 0;
  gchar *string = NULL;
  gchar *tmp = NULL;
  int value;

  GtkWidget *text_view = schematic_newtext_dialog_get_text_view (dialog);

  string = schematic_newtext_dialog_get_text (text_view);

  if (string[0] == '\0' )
    return;

  switch (schematic_window_get_text_caps (w_current))
  {
    case(LOWER):
      tmp = g_utf8_strdown (string, -1);
      break;

    case(UPPER):
      tmp = g_utf8_strup (string, -1);
      break;

    case(BOTH):
    default:
      /* do nothing */
      break;
  }

  value = x_colorcb_get_index (schematic_newtext_dialog_get_colorcb (dialog));
  if (value >= 0) {
    color = value;
  }

  value = schematic_alignment_combo_get_align (schematic_newtext_dialog_get_aligncb (dialog));
  if (value >= 0) {
    align = value;
  }

  value = schematic_integer_combo_box_get_value (schematic_newtext_dialog_get_textsizecb (dialog));
  if (value > 0) {
    size = value;
  }
  else
  {
    size = schematic_window_get_text_size (w_current);
  }

  value = schematic_rotation_combo_get_angle (schematic_newtext_dialog_get_rotatecb (dialog));
  if (value >= 0) {
    rotate = value;
  }

  /* select the text, so you can continue immediatly writing the next text */
  select_all_text_in_textview (GTK_TEXT_VIEW (text_view));
  gtk_widget_grab_focus (text_view);

  o_text_prepare_place (w_current,
                        tmp == NULL ? string : tmp,
                        color,
                        align,
                        rotate,
                        size);

  g_free (string);
  g_free (tmp);
}


/*! \brief Initialize SchematicNewText class
 *
 *  \par Function Description
 *
 *  GType class initialiser for SchematicNewText.  We override our
 *  parent virtual class methods as needed and register our
 *  GObject properties.
 *
 *  \param [in] klass
 */
static void
schematic_newtext_class_init (SchematicNewTextClass *klass)
{
}



/*! \brief Initialize SchematicNewText instance
 *
 *  \param [in,out] dialog The new text dialog
 */
static void
schematic_newtext_init (SchematicNewText *dialog)
{
  GtkWidget *vbox;
  GtkWidget *label = NULL;
  GtkWidget *viewport1 = NULL;
  GtkWidget *scrolled_window = NULL;
  PangoTabArray *tab_array;
  int real_tab_width;
#ifdef ENABLE_GTK3
  GtkWidget *grid;
#else
  GtkWidget *table;
#endif

  gtk_dialog_add_button (GTK_DIALOG (dialog),
                         _("_Close"), GTK_RESPONSE_CLOSE);

  gtk_dialog_add_button (GTK_DIALOG (dialog),
                         _("_Apply"), GTK_RESPONSE_APPLY);

#ifndef ENABLE_GTK3
  /* Set the alternative button order (ok, cancel, help) for other systems */
  gtk_dialog_set_alternative_button_order(GTK_DIALOG(dialog),
                                          GTK_RESPONSE_APPLY,
                                          GTK_RESPONSE_CLOSE,
                                          -1);
#endif

  gtk_window_set_position (GTK_WINDOW (dialog), GTK_WIN_POS_NONE);

  gtk_dialog_set_default_response(GTK_DIALOG(dialog),
                                  GTK_RESPONSE_ACCEPT);

  vbox = gtk_dialog_get_content_area (GTK_DIALOG (dialog));
  gtk_box_set_spacing(GTK_BOX(vbox),DIALOG_V_SPACING);

#ifdef ENABLE_GTK3
  grid = gtk_grid_new ();
  gtk_grid_set_row_spacing (GTK_GRID (grid), DIALOG_V_SPACING);
  gtk_grid_set_column_spacing (GTK_GRID (grid), DIALOG_H_SPACING);
#else
  table = gtk_table_new(4, 2, FALSE);
  gtk_table_set_row_spacings(GTK_TABLE(table), DIALOG_V_SPACING);
  gtk_table_set_col_spacings(GTK_TABLE(table), DIALOG_H_SPACING);
#endif

  label = gtk_label_new(_("<b>Text Properties</b>"));
  gtk_label_set_use_markup(GTK_LABEL(label), TRUE);
#ifdef ENABLE_GTK3
  gtk_label_set_xalign (GTK_LABEL (label), 0.0);
  gtk_label_set_yalign (GTK_LABEL (label), 0.0);
#else
  gtk_misc_set_alignment(GTK_MISC(label),0,0);
#endif
  gtk_box_pack_start(GTK_BOX(vbox), label, FALSE, FALSE, 0);

#ifdef ENABLE_GTK3
  gtk_box_pack_start (GTK_BOX (vbox), grid, FALSE, FALSE, 0);
#else
  GtkWidget *alignment = gtk_alignment_new (0, 0, 1, 1);
  gtk_alignment_set_padding(GTK_ALIGNMENT(alignment), 0, 0,
                            DIALOG_INDENTATION, 0);
  gtk_box_pack_start(GTK_BOX(vbox), alignment, FALSE, FALSE, 0);

  gtk_container_add(GTK_CONTAINER(alignment), table);
#endif

  label = gtk_label_new (_("<b>Text Content</b>"));
  gtk_label_set_use_markup (GTK_LABEL (label), TRUE);
#ifdef ENABLE_GTK3
  gtk_label_set_xalign (GTK_LABEL (label), 0.0);
  gtk_label_set_yalign (GTK_LABEL (label), 0.0);
#else
  gtk_misc_set_alignment(GTK_MISC(label),0,0);
#endif
  gtk_box_pack_start(GTK_BOX(vbox), label, FALSE, FALSE, 0);


  label = gtk_label_new_with_mnemonic (_("Colo_r:"));
#ifdef ENABLE_GTK3
  gtk_label_set_xalign (GTK_LABEL (label), 0.0);
  gtk_label_set_yalign (GTK_LABEL (label), 0.5);
  gtk_grid_attach (GTK_GRID (grid), label, 0, 0, 1, 1);
#else
  gtk_misc_set_alignment(GTK_MISC(label),0,0);
  gtk_table_attach (GTK_TABLE(table),
                    label,
                    0,
                    1,
                    0,
                    1,
                    GTK_FILL,
                    (GtkAttachOptions) 0,
                    0,
                    0);
#endif

  dialog->colorcb = x_colorcb_new ();
  x_colorcb_set_index(dialog->colorcb, TEXT_COLOR);

  gtk_label_set_mnemonic_widget (GTK_LABEL (label), dialog->colorcb);

#ifdef ENABLE_GTK3
  gtk_widget_set_hexpand (GTK_WIDGET (dialog->colorcb), TRUE);
  gtk_grid_attach (GTK_GRID (grid), dialog->colorcb, 1, 0, 1, 1);
#else
  gtk_table_attach_defaults(GTK_TABLE(table), dialog->colorcb, 1,2,0,1);
#endif

  label = gtk_label_new_with_mnemonic (_("_Size:"));
#ifdef ENABLE_GTK3
  gtk_label_set_xalign (GTK_LABEL (label), 0.0);
  gtk_label_set_yalign (GTK_LABEL (label), 0.5);
  gtk_grid_attach (GTK_GRID (grid), label, 0, 1, 1, 1);
#else
  gtk_misc_set_alignment(GTK_MISC(label),0,0);
  gtk_table_attach (GTK_TABLE(table),
                    label,
                    0,
                    1,
                    1,
                    2,
                    GTK_FILL,
                    (GtkAttachOptions) 0,
                    0,
                    0);
#endif

  dialog->textsizecb = schematic_integer_combo_box_new ();

  gtk_label_set_mnemonic_widget (GTK_LABEL (label), dialog->textsizecb);

#ifdef ENABLE_GTK3
  gtk_grid_attach (GTK_GRID (grid), dialog->textsizecb, 1, 1, 1, 1);
#else
  gtk_table_attach_defaults(GTK_TABLE(table), dialog->textsizecb, 1,2,1,2);
#endif

  label = gtk_label_new_with_mnemonic (_("Ali_gnment:"));
#ifdef ENABLE_GTK3
  gtk_label_set_xalign (GTK_LABEL (label), 0.0);
  gtk_label_set_yalign (GTK_LABEL (label), 0.5);
  gtk_grid_attach (GTK_GRID (grid), label, 0, 2, 1, 1);
#else
  gtk_misc_set_alignment(GTK_MISC(label),0,0);
  gtk_table_attach (GTK_TABLE(table),
                    label,
                    0,
                    1,
                    2,
                    3,
                    GTK_FILL,
                    (GtkAttachOptions) 0,
                    0,
                    0);
#endif

  dialog->aligncb = schematic_alignment_combo_new ();
  schematic_alignment_combo_set_align (dialog->aligncb, LOWER_LEFT);

  gtk_label_set_mnemonic_widget (GTK_LABEL (label), dialog->aligncb);

#ifdef ENABLE_GTK3
  gtk_grid_attach (GTK_GRID (grid), dialog->aligncb, 1, 2, 1, 1);
#else
  gtk_table_attach_defaults(GTK_TABLE(table), dialog->aligncb, 1,2,2,3);
#endif

  label = gtk_label_new_with_mnemonic (_("Ro_tation:"));
#ifdef ENABLE_GTK3
  gtk_label_set_xalign (GTK_LABEL (label), 0.0);
  gtk_label_set_yalign (GTK_LABEL (label), 0.5);
  gtk_grid_attach (GTK_GRID (grid), label, 0, 3, 1, 1);
#else
  gtk_misc_set_alignment(GTK_MISC(label),0,0);
  gtk_table_attach (GTK_TABLE(table),
                    label,
                    0,
                    1,
                    3,
                    4,
                    GTK_FILL,
                    (GtkAttachOptions) 0,
                    0,
                    0);
#endif

  dialog->rotatecb = schematic_rotation_combo_new ();
  schematic_rotation_combo_set_angle (dialog->rotatecb, 0);

  gtk_label_set_mnemonic_widget (GTK_LABEL (label), dialog->rotatecb);

#ifdef ENABLE_GTK3
  gtk_grid_attach (GTK_GRID (grid), dialog->rotatecb, 1, 3, 1, 1);
#else
  gtk_table_attach_defaults(GTK_TABLE(table), dialog->rotatecb, 1,2,3,4);
#endif

  viewport1 = gtk_viewport_new (NULL, NULL);
  gtk_widget_show (viewport1);

  scrolled_window = gtk_scrolled_window_new(NULL, NULL);
  gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolled_window),
                                   GTK_POLICY_AUTOMATIC,
                                   GTK_POLICY_AUTOMATIC);
  gtk_container_add (GTK_CONTAINER (viewport1), scrolled_window);
  gtk_box_pack_start( GTK_BOX(vbox), viewport1, TRUE, TRUE, 0);

  dialog->text_view = gtk_text_view_new();
  gtk_text_view_set_editable(GTK_TEXT_VIEW(dialog->text_view), TRUE);
  select_all_text_in_textview(GTK_TEXT_VIEW(dialog->text_view));

  /* Set the tab width, using pango tab array */
  /*! \bug FIXME: This doesn't work. Why? */
  tab_array = pango_tab_array_new (1, TRUE);
  real_tab_width = text_view_calculate_real_tab_width(GTK_TEXT_VIEW(dialog->text_view),
                                                        tab_in_chars);
  if (real_tab_width >= 0) {
    pango_tab_array_set_tab (tab_array, 0, PANGO_TAB_LEFT, real_tab_width);
    /* printf("Real tab width: %i\n", real_tab_width);*/
    gtk_text_view_set_tabs (GTK_TEXT_VIEW (dialog->text_view),
                            tab_array);
  }
  else {
    g_warning ("schematic_newtext_init: Impossible to set tab width.\n");
  }

  pango_tab_array_free (tab_array);
  gtk_container_add(GTK_CONTAINER(scrolled_window), dialog->text_view);
}


GtkWidget*
schematic_newtext_dialog_new (SchematicWindow *w_current)
{
  GtkWidget *dialog =
    GTK_WIDGET (g_object_new (SCHEMATIC_TYPE_NEWTEXT,
                              /* GtkContainer */
                              "border-width",     DIALOG_BORDER_SPACING,
                              /* GtkWindow */
                              "title",            _("Add Text"),
                              "default-width",    320,
                              "default-height",   350,
                              "window-position",  GTK_WIN_POS_MOUSE,
                              "modal",            FALSE,
#ifndef ENABLE_GTK3
                              "allow-grow",       TRUE,
                              "allow-shrink",     FALSE,
                              /* GtkDialog */
                              "has-separator",    TRUE,
#endif
                              /* SchematicDialog */
                              "settings-name",    "text-entry",
                              "schematic-window",  w_current,
                              NULL));

  GtkWidget *main_window =
    schematic_window_get_main_window (w_current);
  gtk_window_set_transient_for (GTK_WINDOW (dialog),
                                GTK_WINDOW (main_window));

  schematic_integer_combo_box_set_model (SCHEMATIC_NEWTEXT (dialog)->textsizecb,
                                         schematic_window_get_text_size_list_store (w_current));

  schematic_integer_combo_box_set_value (SCHEMATIC_NEWTEXT (dialog)->textsizecb,
                                         schematic_window_get_text_size (w_current));

  gtk_widget_show_all (dialog);

  return dialog;
}


/*! \brief Open the dialog box to add new text
 *
 *  \par Function Description
 *  This function creates or raises the modal text entry dialog
 *
 *  \param [in] widget The dialog widget.
 */
void
schematic_newtext_dialog_run (GtkWidget *widget)
{
  gtk_window_present (GTK_WINDOW (widget));

  /* always select the text in the entry */
  select_all_text_in_textview (GTK_TEXT_VIEW (SCHEMATIC_NEWTEXT (widget)->text_view));
  gtk_widget_grab_focus (SCHEMATIC_NEWTEXT (widget)->text_view);
}
