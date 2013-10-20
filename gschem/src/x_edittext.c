/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 * Copyright (C) 2013 Ales Hvezda
 * Copyright (C) 2013 gEDA Contributors (see ChangeLog for details)
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
 * \file x_edittext.c
 *
 * \brief A dialog box for adding editing text on a schematic.
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



#define TYPE_EDITTEXT           (edittext_get_type())
#define EDITTEXT(obj)           (G_TYPE_CHECK_INSTANCE_CAST ((obj), TYPE_EDITTEXT, EditText))
#define EDITTEXT_CLASS(klass)   (G_TYPE_CHECK_CLASS_CAST ((klass),  TYPE_EDITTEXT, EditTextClass))
#define IS_EDITTEXT(obj)        (G_TYPE_CHECK_INSTANCE_TYPE ((obj), TYPE_EDITTEXT))

typedef struct _EditTextClass EditTextClass;
typedef struct _EditText EditText;

struct _EditTextClass {
  GschemDialogClass parent_class;
};

struct _EditText {
    GschemDialog parent;

    GtkWidget *aligncb;
    GtkWidget *colorcb;
    GtkWidget *contentvb;
    GtkWidget *rotatecb;
    GtkWidget *textsizecb;
    GtkWidget *text_view;
};



/*! \brief Handles the user response when apply is selected
 *
 *  \par Function Description
 *  This function applies the text from the text entry dialog.
 *
 *  \param [in] dialog The edit text dialog
 */
static void
dialog_response_ok (EditText *dialog)
{
  int align;
  int color;
  int rotate;
  int size;
  char *string = NULL;

  if (dialog->text_view != NULL) {
    GtkTextBuffer *textbuffer;
    GtkTextIter start, end;
    textbuffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(dialog->text_view));
    gtk_text_buffer_get_bounds (textbuffer, &start, &end);
    string =  gtk_text_iter_get_text (&start, &end);
  } /* else the string will be null which is okay */

  color = x_colorcb_get_index (dialog->colorcb);

  size = gschem_integer_combo_box_get_value (dialog->textsizecb);

  align = x_aligncb_get_align (dialog->aligncb);

  rotate = x_rotatecb_get_angle (dialog->rotatecb);

  o_text_edit_end(dialog->parent.w_current, string, color, align, rotate, size);
}



/*! \brief Handles user responses from the edit text dialog box

 *  \par Function Description
 *  Callback function for the edit text dialog.
 *
 *  \param [in,out] dialog The edit text dialog
 *  \param [na]     unused Unused parameter
 */
static void
dialog_response (EditText *dialog, gint response, gpointer unused)
{
  switch(response) {
    case GTK_RESPONSE_OK:
      dialog_response_ok(dialog);
      break;
    case GTK_RESPONSE_CLOSE:
    case GTK_RESPONSE_DELETE_EVENT:
       break;
    default:
      printf("text_edit_dialog_response(): strange signal %d\n", response);
  }

  i_set_state(dialog->parent.w_current, SELECT);
  i_update_toolbar(dialog->parent.w_current);
  gtk_widget_destroy(dialog->parent.w_current->tewindow);
  dialog->parent.w_current->tewindow=NULL;
}


/*! \brief Initialize EditText class
 *
 *  \par Function Description
 *
 *  GType class initialiser for Multiattrib. We override our parent
 *  virtual class methods as needed and register our GObject properties.
 *
 *  \param [in] klass
 */
static void edittext_class_init(EditTextClass *klass)
{
}



/*! \brief Initialize EditText instance
 *
 *  \param [in,out] dialog The edit text dialog
 */
static void edittext_init(EditText *dialog)
{
  GtkWidget *alignment;
  GtkWidget *vbox;
  GtkWidget *label = NULL;
  GtkWidget *viewport1 = NULL;
  GtkWidget *scrolled_window = NULL;
  GtkWidget *table;

  gtk_dialog_add_button (GTK_DIALOG (dialog),
                         GTK_STOCK_CLOSE,
                         GTK_RESPONSE_CLOSE);

  gtk_dialog_add_button (GTK_DIALOG (dialog),
                         GTK_STOCK_OK,
                         GTK_RESPONSE_OK);

  /* Set the alternative button order (ok, cancel, help) for other systems */
  gtk_dialog_set_alternative_button_order(GTK_DIALOG(dialog),
                                          GTK_RESPONSE_ACCEPT,
                                          GTK_RESPONSE_REJECT,
                                          -1);

  gtk_window_position(GTK_WINDOW (dialog),
                      GTK_WIN_POS_NONE);

  g_signal_connect (G_OBJECT (dialog),
                    "response",
                    G_CALLBACK (dialog_response),
                    NULL);

  gtk_dialog_set_default_response(GTK_DIALOG(dialog),
                                  GTK_RESPONSE_ACCEPT);

  gtk_container_border_width(GTK_CONTAINER (dialog),
                             DIALOG_BORDER_SPACING);

  vbox = GTK_DIALOG(dialog)->vbox;
  gtk_box_set_spacing(GTK_BOX(vbox), DIALOG_V_SPACING);

  dialog->contentvb = gtk_vbox_new (FALSE, DIALOG_V_SPACING);
  gtk_box_pack_start(GTK_BOX(vbox), dialog->contentvb, TRUE, TRUE, 0);

  label = gtk_label_new (_("<b>Text Content</b>"));
  gtk_label_set_use_markup (GTK_LABEL (label), TRUE);
  gtk_misc_set_alignment(GTK_MISC(label),0,0);
  gtk_box_pack_start(GTK_BOX(dialog->contentvb), label, FALSE, FALSE, 0);

  alignment = gtk_alignment_new(0,0,1,1);
  gtk_alignment_set_padding(GTK_ALIGNMENT(alignment), 0, 0,
                            DIALOG_INDENTATION, 0);
  gtk_box_pack_start(GTK_BOX(dialog->contentvb), alignment, TRUE, TRUE, 0);

  viewport1 = gtk_viewport_new (NULL, NULL);
  gtk_widget_set_size_request(GTK_WIDGET(viewport1),-1,75);

  scrolled_window = gtk_scrolled_window_new(NULL, NULL);
  gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolled_window),
                                   GTK_POLICY_AUTOMATIC,
                                   GTK_POLICY_AUTOMATIC);
  gtk_container_add (GTK_CONTAINER (viewport1), scrolled_window);
  gtk_container_add( GTK_CONTAINER(alignment), viewport1);

  dialog->text_view = gtk_text_view_new();
  gtk_text_view_set_editable (GTK_TEXT_VIEW (dialog->text_view), TRUE);

    /*! \bug FIXME: Set tab's width in the textview widget. */
    /* See first the code in text_input_dialog and get it working before adding it here. */

  gtk_container_add(GTK_CONTAINER(scrolled_window), dialog->text_view);

  label = gtk_label_new(_("<b>Text Properties</b>"));
  gtk_label_set_use_markup(GTK_LABEL(label), TRUE);
  gtk_misc_set_alignment(GTK_MISC(label),0,0);
  gtk_box_pack_start(GTK_BOX(vbox), label, FALSE, FALSE, 0);

  alignment = gtk_alignment_new(0,0,1,1);
  gtk_alignment_set_padding(GTK_ALIGNMENT(alignment), 0, 0,
                          DIALOG_INDENTATION, 0);
  gtk_box_pack_start(GTK_BOX(vbox), alignment, FALSE, FALSE, 0);

  table = gtk_table_new (4, 2, FALSE);
  gtk_table_set_row_spacings(GTK_TABLE(table), DIALOG_V_SPACING);
  gtk_table_set_col_spacings(GTK_TABLE(table), DIALOG_H_SPACING);
  gtk_container_add(GTK_CONTAINER(alignment), table);

  label = gtk_label_new(_("Color:"));
  gtk_misc_set_alignment(GTK_MISC(label),0,0);
  gtk_table_attach(GTK_TABLE(table), label, 0,1,0,1, GTK_FILL,0,0,0);

  dialog->colorcb = x_colorcb_new ();
  gtk_table_attach_defaults(GTK_TABLE(table), dialog->colorcb, 1,2,0,1);

  label = gtk_label_new(_("Size:"));
  gtk_misc_set_alignment(GTK_MISC(label),0,0);
  gtk_table_attach(GTK_TABLE(table), label, 0,1,1,2, GTK_FILL,0,0,0);

  dialog->textsizecb = gschem_integer_combo_box_new ();
  gtk_table_attach_defaults(GTK_TABLE(table), dialog->textsizecb, 1,2,1,2);
  //gtk_entry_set_activates_default(GTK_ENTRY(dialog->textsizecb), TRUE);

  label = gtk_label_new(_("Alignment:"));
  gtk_misc_set_alignment(GTK_MISC(label),0,0);
  gtk_table_attach(GTK_TABLE(table), label, 0,1,2,3, GTK_FILL,0,0,0);

  dialog->aligncb = x_aligncb_new();
  //x_aligncb_set_align(dialog->aligncb, text_alignment);
  gtk_table_attach_defaults(GTK_TABLE(table), dialog->aligncb, 1,2,2,3);

  label = gtk_label_new(_("Rotation:"));
  gtk_misc_set_alignment(GTK_MISC(label),0,0);
  gtk_table_attach(GTK_TABLE(table), label, 0,1,3,4, GTK_FILL,0,0,0);

  dialog->rotatecb = x_rotatecb_new();
  //x_aligncb_set_align(dialog->aligncb, text_alignment);
  gtk_table_attach_defaults(GTK_TABLE(table), dialog->rotatecb, 1,2,3,4);
}



/*! \brief Get/register NewText type.
 */
GType edittext_get_type()
{
  static GType type = 0;

  if (type == 0) {
    static const GTypeInfo info = {
      sizeof(EditTextClass),
      NULL,                                   /* base_init */
      NULL,                                   /* base_finalize */
      (GClassInitFunc) edittext_class_init,
      NULL,                                   /* class_finalize */
      NULL,                                   /* class_data */
      sizeof(EditText),
      0,                                      /* n_preallocs */
      (GInstanceInitFunc) edittext_init,
    };

    type = g_type_register_static (GSCHEM_TYPE_DIALOG, "EditText", &info, 0);
  }

  return type;
}




static void
setup_initial_values (EditText *dialog)
{
  int align = -1;
  int color = -1;
  int rotate = -1;
  GList *s_current;
  int size = -1;
  char *string = NULL;

  /* Find the first text object in the selection */

  s_current = geda_list_get_glist( dialog->parent.w_current->toplevel->page_current->selection_list );

  while (s_current != NULL) {
    OBJECT* object = (OBJECT *) s_current->data;
    s_current = g_list_next(s_current);
    if ((object != NULL) && (object->type == OBJ_TEXT)) {
      string = g_strdup (object->text->string);
      color = object->color;
      size = object->text->size;
      align = object->text->alignment;
      rotate = object->text->angle;
      break;
    }
  }

  /* Check if all other text objects have the same properties */

  while (s_current != NULL) {
    OBJECT* object = (OBJECT *) s_current->data;
    if ((object != NULL) && (object->type == OBJ_TEXT)) {
      if (string != NULL) {
        g_free (string);
        string = NULL;
      }
      if (color != object->color) {
        color = -1;
      }
      if (size != object->text->size) {
        size = -1;
      }
      if (align != object->text->alignment) {
        align = -1;
      }
      if (rotate != object->text->angle) {
        rotate = -1;
      }
    }
    s_current = g_list_next(s_current);
  }

  /* Setup the values in the dialog box */

  if (string != NULL) {
    GtkTextBuffer *textbuffer = gtk_text_view_get_buffer (GTK_TEXT_VIEW (dialog->text_view));
    gtk_text_buffer_set_text (GTK_TEXT_BUFFER (textbuffer), string, -1);
    g_free(string);
  }
  else {
    gtk_widget_destroy (GTK_WIDGET (dialog->contentvb));
    dialog->contentvb = NULL;
    dialog->text_view = NULL;
  }

  if (color >= 0) {
    x_colorcb_set_index(dialog->colorcb, color);
  }

  if (size > 0) {
    gschem_integer_combo_box_set_value(dialog->textsizecb, size);
  }

  if (align >= 0) {
    x_aligncb_set_align(dialog->aligncb, align);
  }

  if (rotate >= 0) {
    x_rotatecb_set_angle(dialog->rotatecb, rotate);
  }
}



/*! \brief Open the dialog box to edit text
 *
 *  \par Function Description
 *  This function creates or raises the modal text entry dialog
 *
 *  \param [in] w_current The gschem toplevel
 */
void
text_edit_dialog (GschemToplevel *w_current, const char *string, int text_size,
                  int text_alignment)
{
  if (w_current->tewindow == NULL) {
    /* dialog not created yet */
    w_current->tewindow = g_object_new (TYPE_EDITTEXT,
                                        /* GtkContainer */
                                        "border-width",     DIALOG_BORDER_SPACING,
                                        /* GtkWindow */
                                        "title",            _("Edit Text Properties"),
                                        "default-width",    320,
                                        "default-height",   350,
                                        "window-position",  GTK_WIN_POS_MOUSE,
                                        "allow-grow",       TRUE,
                                        "allow-shrink",     FALSE,
                                        "modal",            TRUE,
                                        /* GtkDialog */
                                        "has-separator",    TRUE,
                                        /* GschemDialog */
                                        "settings-name",    "text-edit",
                                        "gschem-toplevel",  w_current,
                                        NULL);

    gtk_window_set_transient_for (GTK_WINDOW (w_current->tewindow),
                                  GTK_WINDOW (w_current->main_window));

    gschem_integer_combo_box_set_model (EDITTEXT (w_current->tewindow)->textsizecb,
                           gschem_toplevel_get_text_size_list_store (w_current));

    setup_initial_values (EDITTEXT (w_current->tewindow));

    gtk_widget_show_all (w_current->tewindow);
  }
  else {
    /* dialog already created */
    setup_initial_values (EDITTEXT (w_current->tewindow));

    gtk_window_present (GTK_WINDOW(w_current->tewindow));
  }

  if (EDITTEXT (w_current->tewindow)->text_view != NULL) {
    select_all_text_in_textview (GTK_TEXT_VIEW (EDITTEXT (w_current->tewindow)->text_view));
    gtk_widget_grab_focus(EDITTEXT (w_current->tewindow)->text_view);
  }
  else {
    gtk_widget_grab_focus(EDITTEXT (w_current->tewindow)->colorcb);
  }
}
