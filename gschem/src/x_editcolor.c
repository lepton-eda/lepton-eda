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
 * \file x_editcolor.c
 *
 * \brief A dialog box for adding editing object color.
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

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif



#define TYPE_EDITCOLOR           (editcolor_get_type())
#define EDITCOLOR(obj)           (G_TYPE_CHECK_INSTANCE_CAST ((obj), TYPE_EDITCOLOR, EditColor))
#define EDITCOLOR_CLASS(klasse)  (G_TYPE_CHECK_CLASS_CAST ((klasse), TYPE_EDITCOLOR, EditColorClass))
#define IS_EDITCOLOR(obj)        (G_TYPE_CHECK_INSTANCE_TYPE ((obj), TYPE_EDITCOLOR))

typedef struct _EditColorClass EditColorClass;
typedef struct _EditColor EditColor;

struct _EditColorClass
{
  GschemDialogClass parent_class;
};

struct _EditColor
{
    GschemDialog parent;

    GtkWidget *colorcb;
};



/*! \brief Handles the user response when apply is selected
 *
 *  \par Function Description
 *  This function applies the color from the color edit dialog.
 *
 *  \param [in] dialog The edit color dialog
 */
static void
dialog_response_apply (EditColor *dialog)
{
  int color;
  GList *s_current = NULL;

  color = x_colorcb_get_index (dialog->colorcb);

  s_current = geda_list_get_glist (dialog->parent.w_current->toplevel->page_current->selection_list);

  while (s_current != NULL) {
    OBJECT *object = (OBJECT *) s_current->data;

    if (object == NULL) {
      fprintf(stderr, _("ERROR: NULL object in color_edit_dialog_apply!\n"));
      exit(-1);
    }

    o_set_color (dialog->parent.w_current->toplevel, object, color);
    dialog->parent.w_current->toplevel->page_current->CHANGED = 1;

    s_current = g_list_next(s_current);
  }

  o_undo_savestate(dialog->parent.w_current, UNDO_ALL);
}



/*! \brief Handles user responses from the edit color dialog box

 *  \par Function Description
 *  Callback function for the edit color dialog.
 *
 *  \param [in,out] dialog The edit color dialog
 *  \param [na]     unused Unused parameter
 */
static void
dialog_response (EditColor *dialog, gint response, gpointer unused)
{
  switch(response) {
    case GTK_RESPONSE_APPLY:
      dialog_response_apply(dialog);
      break;
    case GTK_RESPONSE_CLOSE:
    case GTK_RESPONSE_DELETE_EVENT:
      //i_set_state(dialog->parent.w_current, SELECT);
      //i_update_toolbar(dialog->parent.w_current);
      gtk_widget_destroy(dialog->parent.w_current->clwindow);
      dialog->parent.w_current->clwindow=NULL;
      break;
    default:
      printf("dialog_response(): strange signal %d\n", response);
  }
}



/*! \brief Initialize EditColor class
 *
 *  \par Function Description
 *
 *  GType class initialiser for Multiattrib. We override our parent
 *  virtual class methods as needed and register our GObject properties.
 *
 *  \param [in] klasse
 */
static void editcolor_class_init(EditColorClass *klasse)
{
}



/*! \brief Initialize EditColor instance
 *
 *  \param [in,out] dialog The edit color dialog
 */
static void editcolor_init(EditColor *dialog)
{
  GtkWidget *alignment;
  GtkWidget *vbox;
  GtkWidget *label = NULL;
  GtkWidget *table;

  /* dialog initialization */
  g_object_set (G_OBJECT (dialog),
                /* GtkContainer */
                "border-width",    0,
                /* GtkWindow */
                "title",           _("Color Edit"),
                "default-width",   320,
                "default-height",  350,
                "window-position", GTK_WIN_POS_MOUSE,
                "allow-grow",      TRUE,
                "allow-shrink",    FALSE,
                "modal",           FALSE,
                /* GtkDialog */
                "has-separator",   TRUE,
                NULL);

  gtk_dialog_add_button (GTK_DIALOG (dialog),
                         GTK_STOCK_CLOSE,
                         GTK_RESPONSE_CLOSE);

  gtk_dialog_add_button (GTK_DIALOG (dialog),
                         GTK_STOCK_APPLY,
                         GTK_RESPONSE_APPLY);

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
  gtk_container_set_border_width(GTK_CONTAINER(dialog),DIALOG_BORDER_SPACING);
  gtk_box_set_spacing(GTK_BOX(vbox), DIALOG_V_SPACING);

  label = gtk_label_new(_("<b>Object Properties</b>"));
  gtk_label_set_use_markup(GTK_LABEL(label), TRUE);
  gtk_misc_set_alignment(GTK_MISC(label),0,0);
  gtk_box_pack_start(GTK_BOX(vbox), label, FALSE, FALSE, 0);

  alignment = gtk_alignment_new(0,0,1,1);
  gtk_alignment_set_padding(GTK_ALIGNMENT(alignment), 0, 0,
                          DIALOG_INDENTATION, 0);
  gtk_box_pack_start(GTK_BOX(vbox), alignment, FALSE, FALSE, 0);

  table = gtk_table_new (1, 2, FALSE);
  gtk_table_set_row_spacings(GTK_TABLE(table), DIALOG_V_SPACING);
  gtk_table_set_col_spacings(GTK_TABLE(table), DIALOG_H_SPACING);
  gtk_container_add(GTK_CONTAINER(alignment), table);

  label = gtk_label_new(_("Color:"));
  gtk_misc_set_alignment(GTK_MISC(label),0,0);
  gtk_table_attach(GTK_TABLE(table), label, 0,1,0,1, GTK_FILL,0,0,0);

  dialog->colorcb = x_colorcb_new ();
  gtk_table_attach_defaults(GTK_TABLE(table), dialog->colorcb, 1,2,0,1);
}



/*! \brief Get/register NewColor type.
 */
GType editcolor_get_type()
{
  static GType type = 0;

  if (type == 0) {
    static const GTypeInfo info = {
      sizeof(EditColorClass),
      NULL,                                   /* base_init */
      NULL,                                   /* base_finalize */
      (GClassInitFunc) editcolor_class_init,
      NULL,                                   /* class_finalize */
      NULL,                                   /* class_data */
      sizeof(EditColor),
      0,                                      /* n_preallocs */
      (GInstanceInitFunc) editcolor_init,
    };

    type = g_type_register_static (GSCHEM_TYPE_DIALOG, "EditColor", &info, 0);
  }

  return type;
}




static void
setup_initial_values (EditColor *dialog)
{
  int color = -1;
  GList *s_current;

  /* Find the first object in the selection */

  s_current = geda_list_get_glist( dialog->parent.w_current->toplevel->page_current->selection_list );

  while (s_current != NULL) {
    OBJECT* object = (OBJECT *) s_current->data;
    s_current = g_list_next(s_current);
    if ((object != NULL) && (
        (object->type == OBJ_ARC)    ||
        (object->type == OBJ_BOX)    ||
        (object->type == OBJ_CIRCLE) ||
        (object->type == OBJ_LINE)   ||
        (object->type == OBJ_PATH)   ||
        (object->type == OBJ_TEXT))) {
      color = object->color;
      break;
    }
  }

  /* Check if all other objects have the same properties */

  while (s_current != NULL) {
    OBJECT* object = (OBJECT *) s_current->data;
    if ((object != NULL) && (
        (object->type == OBJ_ARC)    ||
        (object->type == OBJ_BOX)    ||
        (object->type == OBJ_CIRCLE) ||
        (object->type == OBJ_LINE)   ||
        (object->type == OBJ_PATH)   ||
        (object->type == OBJ_TEXT))) {
      if (color != object->color) {
        color = -1;
      }
    }
    s_current = g_list_next(s_current);
  }

  /* Setup the values in the dialog box */

  if (color >= 0) {
    x_colorcb_set_index(dialog->colorcb, color);
  }
}



/*! \brief Open the dialog box to edit color
 *
 *  \par Function Description
 *  This function creates or raises the modal color dialog
 *
 *  \param [in] w_current The gschem toplevel
 */
void
color_edit_dialog (GSCHEM_TOPLEVEL *w_current)
{
  if (w_current->clwindow == NULL) {
    /* dialog not created yet */
    w_current->clwindow = g_object_new (TYPE_EDITCOLOR,
                                        /* GschemDialog */
                                        "settings-name",    "color-edit",
                                        "gschem-toplevel",  w_current,
                                        NULL);

    gtk_window_set_transient_for (GTK_WINDOW (w_current->clwindow),
                                  GTK_WINDOW (w_current->main_window));

    setup_initial_values (EDITCOLOR (w_current->clwindow));

    gtk_widget_show_all (w_current->clwindow);
  }
  else {
    /* dialog already created */
    setup_initial_values (EDITCOLOR (w_current->clwindow));

    gtk_window_present (GTK_WINDOW(w_current->clwindow));
  }
}
