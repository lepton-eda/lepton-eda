/* Lepton EDA Schematic Capture
 * Copyright (C) 2013 Ales Hvezda
 * Copyright (C) 2013-2015 gEDA Contributors
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
 * \file x_newtext.c
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

#include "gschem.h"
#include <gdk/gdkkeysyms.h>



#define TYPE_NEWTEXT           (newtext_get_type())
#define NEWTEXT(obj)           (G_TYPE_CHECK_INSTANCE_CAST ((obj), TYPE_NEWTEXT, NewText))
#define NEWTEXT_CLASS(klass)   (G_TYPE_CHECK_CLASS_CAST ((klass),  TYPE_NEWTEXT, NewTextClass))
#define IS_NEWTEXT(obj)        (G_TYPE_CHECK_INSTANCE_TYPE ((obj), TYPE_NEWTEXT))

typedef struct _NewTextClass NewTextClass;
typedef struct _NewText NewText;

struct _NewTextClass {
  GschemDialogClass parent_class;
};

struct _NewText {
    GschemDialog parent;

    GtkWidget *aligncb;
    GtkWidget *colorcb;
    GtkWidget *rotatecb;
    GtkWidget *textsizecb;
    GtkWidget *text_view;
};



/*! \brief Handles the user response when apply is selected
 *
 *  \par Function Description
 *  This function applies the text from the text entry dialog.
 *
 *  \param [in] dialog The new text dialog
 */
static void
dialog_response_apply (NewText *dialog)
{
  g_return_if_fail (dialog != NULL);

  GschemToplevel *w_current = NULL;
  g_object_get (GSCHEM_DIALOG (dialog), "gschem-toplevel", &w_current, NULL);
  g_return_if_fail (w_current != NULL);

  int size = w_current->text_size;

  int align = LOWER_LEFT;
  int color = TEXT_COLOR;
  int rotate = 0;
  gchar *string = NULL;
  gchar *tmp = NULL;
  GtkTextBuffer *textbuffer;
  GtkTextIter start, end;
  int value;

  textbuffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(dialog->text_view));
  gtk_text_buffer_get_bounds (textbuffer, &start, &end);
  string =  gtk_text_iter_get_text (&start, &end);

  if (string[0] == '\0' )
    return;

  switch(dialog->parent.w_current->text_caps) {
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

  value = x_colorcb_get_index (dialog->colorcb);
  if (value >= 0) {
    color = value;
  }

  value = gschem_alignment_combo_get_align (dialog->aligncb);
  if (value >= 0) {
    align = value;
  }

  value = gschem_integer_combo_box_get_value (dialog->textsizecb);
  if (value > 0) {
    size = value;
  }

  value = gschem_rotation_combo_get_angle (dialog->rotatecb);
  if (value >= 0) {
    rotate = value;
  }

  /* select the text, so you can continue immediatly writing the next text */
  select_all_text_in_textview(GTK_TEXT_VIEW(dialog->text_view));
  gtk_widget_grab_focus(dialog->text_view);

  o_text_prepare_place (dialog->parent.w_current,
                        tmp == NULL ? string : tmp,
                        color,
                        align,
                        rotate,
                        size);

  g_free (string);
  g_free (tmp);
}



/*! \brief Handles the user response when cancel is selected
 *
 *
 *
 *  \param [in,out] dialog The new text dialog
 */
static void dialog_response_cancel(NewText *dialog)
{
  i_callback_cancel (NULL, dialog->parent.w_current);
  gtk_widget_destroy(dialog->parent.w_current->tiwindow);
  dialog->parent.w_current->tiwindow=NULL;
}



/*! \brief Handles user responses from the new text dialog box

 *  \par Function Description
 *  Callback function for the text entry dialog.
 *
 *  \param [in,out] widget The new text dialog
 *  \param [na]     unused Unused parameter
 */
static void text_input_dialog_response(NewText *dialog, gint response, gpointer unused)
{
  switch(response) {
    case GTK_RESPONSE_APPLY:
      dialog_response_apply(dialog);
      break;
    case GTK_RESPONSE_CLOSE:
    case GTK_RESPONSE_DELETE_EVENT:
      dialog_response_cancel(dialog);
      break;
    default:
      printf("text_edit_dialog_response(): strange signal %d\n", response);
  }
}


/*! \brief Initialize NewText class
 *
 *  \par Function Description
 *
 *  GType class initialiser for Multiattrib. We override our parent
 *  virtual class methods as needed and register our GObject properties.
 *
 *  \param [in] klass
 */
static void newtext_class_init(NewTextClass *klass)
{
}



/*! \brief Initialize NewText instance
 *
 *  \param [in,out] dialog The new text dialog
 */
static void newtext_init(NewText *dialog)
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

  g_signal_connect (G_OBJECT (dialog), "response",
                    G_CALLBACK (text_input_dialog_response),
                    NULL);

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
  gtk_misc_set_alignment(GTK_MISC(label),0,0);
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
  gtk_misc_set_alignment(GTK_MISC(label),0,0);
  gtk_box_pack_start(GTK_BOX(vbox), label, FALSE, FALSE, 0);


  label = gtk_label_new_with_mnemonic (_("Colo_r:"));
  gtk_misc_set_alignment(GTK_MISC(label),0,0);
#ifdef ENABLE_GTK3
  gtk_grid_attach (GTK_GRID (grid), label, 0, 0, 1, 1);
#else
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
  gtk_misc_set_alignment(GTK_MISC(label),0,0);
#ifdef ENABLE_GTK3
  gtk_grid_attach (GTK_GRID (grid), label, 0, 1, 1, 1);
#else
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

  dialog->textsizecb = gschem_integer_combo_box_new();

  gtk_label_set_mnemonic_widget (GTK_LABEL (label), dialog->textsizecb);

#ifdef ENABLE_GTK3
  gtk_grid_attach (GTK_GRID (grid), dialog->textsizecb, 1, 1, 1, 1);
#else
  gtk_table_attach_defaults(GTK_TABLE(table), dialog->textsizecb, 1,2,1,2);
#endif

  label = gtk_label_new_with_mnemonic (_("Ali_gnment:"));
  gtk_misc_set_alignment(GTK_MISC(label),0,0);
#ifdef ENABLE_GTK3
  gtk_grid_attach (GTK_GRID (grid), label, 0, 2, 1, 1);
#else
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

  dialog->aligncb = gschem_alignment_combo_new ();
  gschem_alignment_combo_set_align(dialog->aligncb, LOWER_LEFT);

  gtk_label_set_mnemonic_widget (GTK_LABEL (label), dialog->aligncb);

#ifdef ENABLE_GTK3
  gtk_grid_attach (GTK_GRID (grid), dialog->aligncb, 1, 2, 1, 1);
#else
  gtk_table_attach_defaults(GTK_TABLE(table), dialog->aligncb, 1,2,2,3);
#endif

  label = gtk_label_new_with_mnemonic (_("Ro_tation:"));
  gtk_misc_set_alignment(GTK_MISC(label),0,0);
#ifdef ENABLE_GTK3
  gtk_grid_attach (GTK_GRID (grid), label, 0, 3, 1, 1);
#else
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

  dialog->rotatecb = gschem_rotation_combo_new ();
  gschem_rotation_combo_set_angle(dialog->rotatecb, 0);

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
    g_warning ("text_input_dialog: Impossible to set tab width.\n");
  }

  pango_tab_array_free (tab_array);
  gtk_container_add(GTK_CONTAINER(scrolled_window), dialog->text_view);
}



/*! \brief Get/register NewText type.
 */
GType newtext_get_type()
{
  static GType type = 0;

  if (type == 0) {
    static const GTypeInfo info = {
      sizeof(NewTextClass),
      NULL,                                   /* base_init */
      NULL,                                   /* base_finalize */
      (GClassInitFunc) newtext_class_init,
      NULL,                                   /* class_finalize */
      NULL,                                   /* class_data */
      sizeof(NewText),
      0,                                      /* n_preallocs */
      (GInstanceInitFunc) newtext_init,
    };

    type = g_type_register_static (GSCHEM_TYPE_DIALOG,
                                   "NewText",
                                   &info,
                                   (GTypeFlags) 0);
  }

  return type;
}



/*! \brief Open the dialog box to add new text
 *
 *  \par Function Description
 *  This function creates or raises the modal text entry dialog
 *
 *  \param [in] w_current The gschem toplevel
 */
void
text_input_dialog (GschemToplevel *w_current)
{
  if (w_current->tiwindow == NULL) {
    /* dialog not created yet */
    w_current->tiwindow =
      GTK_WIDGET (g_object_new (TYPE_NEWTEXT,
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
                                /* GschemDialog */
                                "settings-name",    "text-entry",
                                "gschem-toplevel",  w_current,
                                NULL));

    gtk_window_set_transient_for (GTK_WINDOW (w_current->tiwindow),
                                  GTK_WINDOW (w_current->main_window));

    gschem_integer_combo_box_set_model (NEWTEXT (w_current->tiwindow)->textsizecb,
                           gschem_toplevel_get_text_size_list_store (w_current));

    gschem_integer_combo_box_set_value (NEWTEXT (w_current->tiwindow)->textsizecb,
                                        w_current->text_size);

    gtk_widget_show_all (w_current->tiwindow);
  }
  else {
    /* dialog already created */
    gtk_window_present (GTK_WINDOW(w_current->tiwindow));
  }

  /* always select the text in the entry */
  select_all_text_in_textview (GTK_TEXT_VIEW (NEWTEXT (w_current->tiwindow)->text_view));
  gtk_widget_grab_focus (NEWTEXT (w_current->tiwindow)->text_view);
}
