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



#define TYPE_EDITCOLOR           (editcolor_get_type())
#define EDITCOLOR(obj)           (G_TYPE_CHECK_INSTANCE_CAST ((obj), TYPE_EDITCOLOR, EditColor))
#define EDITCOLOR_CLASS(klasse)  (G_TYPE_CHECK_CLASS_CAST ((klasse), TYPE_EDITCOLOR, EditColorClass))
#define IS_EDITCOLOR(obj)        (G_TYPE_CHECK_INSTANCE_TYPE ((obj), TYPE_EDITCOLOR))
#define EDITCOLOR_GET_CLASS(obj) (G_TYPE_INSTANCE_GET_CLASS ((obj), TYPE_EDITCOLOR, EditColorClass))

typedef struct _EditColorClass EditColorClass;
typedef struct _EditColor EditColor;

struct _EditColorClass
{
  GschemDialogClass parent_class;
};

struct _EditColor
{
  GschemDialog parent;

  GschemSelectionAdapter *adapter;
  GtkWidget *colorcb;
};



static void
dispose (GObject *object);

GType
editcolor_get_type ();

static void
notify_object_color (GschemSelectionAdapter *adapter, GParamSpec *pspec, EditColor *dialog);

static void
object_color_changed (GtkWidget *widget, EditColor *dialog);

static void
update_object_color (EditColor *dialog);

void
x_editcolor_set_selection_adapter (EditColor *dialog, GschemSelectionAdapter *adapter);


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



static void
dispose (GObject *object)
{
  EditColor *dialog;
  EditColorClass *klasse;
  GObjectClass *parent_klasse;

  g_return_if_fail (object != NULL);
  dialog = EDITCOLOR (object);
  g_return_if_fail (dialog != NULL);

  x_editcolor_set_selection_adapter (dialog, NULL);

  /* lastly, chain up to the parent dispose */

  klasse = EDITCOLOR_GET_CLASS (object);
  g_return_if_fail (klasse != NULL);
  parent_klasse = g_type_class_peek_parent (klasse);
  g_return_if_fail (parent_klasse != NULL);
  parent_klasse->dispose (object);

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
  GObjectClass *object_klasse;

  g_return_if_fail (klasse != NULL);

  object_klasse = G_OBJECT_CLASS (klasse);

  g_return_if_fail (object_klasse != NULL);

  object_klasse->dispose = dispose;
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

  gtk_window_position(GTK_WINDOW (dialog),
                      GTK_WIN_POS_NONE);

  g_signal_connect (G_OBJECT (dialog),
                    "response",
                    G_CALLBACK (dialog_response),
                    NULL);

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

  g_signal_connect(G_OBJECT (dialog->colorcb),
                   "changed",
                   G_CALLBACK (object_color_changed),
                   dialog);
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



/*! \brief Signal handler for when the selection cap style changes
 *
 *  \param [in]     adapter
 *  \param [in]     pspec
 *  \param [in,out] dialog
 */
static void
notify_object_color (GschemSelectionAdapter *adapter, GParamSpec *pspec, EditColor *dialog)
{
  g_return_if_fail (dialog != NULL);
  g_return_if_fail (adapter != NULL);

  if (dialog->adapter != NULL) {
    g_return_if_fail (adapter == dialog->adapter);

    update_object_color (dialog);
  }
}



static void
object_color_changed (GtkWidget *widget, EditColor *dialog)
{
  TOPLEVEL *toplevel;
  GschemToplevel *w_current;

  g_return_if_fail (dialog != NULL);
  g_return_if_fail (widget != NULL);

  w_current = dialog->parent.w_current;
  g_return_if_fail (w_current != NULL);

  toplevel = gschem_toplevel_get_toplevel (w_current);
  g_return_if_fail (toplevel != NULL);

  if ((dialog->adapter != NULL) && (dialog->colorcb != NULL)) {
    int color;

    g_return_if_fail (widget == dialog->colorcb);

    color = x_colorcb_get_index (dialog->colorcb);

    if (color >= 0) {
      gschem_selection_adapter_set_object_color (dialog->adapter, color);

      toplevel->page_current->CHANGED = 1;
      o_undo_savestate(w_current, UNDO_ALL);
    }
  }
}



/*! \brief Set the selection this dialog manipulates
 *
 *  \param [in,out] dialog
 *  \param [in]     selection
 */
void
x_editcolor_set_selection_adapter (EditColor *dialog, GschemSelectionAdapter *adapter)
{
  g_return_if_fail (dialog != NULL);

  if (dialog->adapter != NULL) {
    g_signal_handlers_disconnect_by_func (dialog->adapter,
                                          G_CALLBACK (notify_object_color),
                                          dialog);

    g_object_unref (dialog->adapter);
  }

  dialog->adapter = adapter;

  if (dialog->adapter != NULL) {
    g_object_ref (dialog->adapter);

    g_signal_connect (dialog->adapter,
                      "notify::object-color",
                      G_CALLBACK (notify_object_color),
                      dialog);
  }

  update_object_color (dialog);
}




static void
update_object_color (EditColor *dialog)
{
  g_return_if_fail (dialog != NULL);

  if ((dialog->adapter != NULL) && (dialog->colorcb != NULL)) {
    int color = gschem_selection_adapter_get_object_color (dialog->adapter);

    g_signal_handlers_block_by_func(G_OBJECT (dialog->colorcb),
                                    G_CALLBACK (object_color_changed),
                                    dialog);

    x_colorcb_set_index(dialog->colorcb, color);

    g_signal_handlers_unblock_by_func(G_OBJECT (dialog->colorcb),
                                      G_CALLBACK (object_color_changed),
                                      dialog);
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
color_edit_dialog (GschemToplevel *w_current)
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

    x_editcolor_set_selection_adapter (EDITCOLOR (w_current->clwindow),
                                       gschem_toplevel_get_selection_adapter (w_current));

    gtk_widget_show_all (w_current->clwindow);
  }
  else {
    /* dialog already created */
    x_editcolor_set_selection_adapter (EDITCOLOR (w_current->clwindow),
                                       gschem_toplevel_get_selection_adapter (w_current));

    gtk_window_present (GTK_WINDOW(w_current->clwindow));
  }
}
