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
 * \file x_editfprop.c
 *
 * \brief A dialog box for editing an object's fill properties.
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



#define TYPE_EDITFPROP           (editfprop_get_type())
#define EDITFPROP(obj)           (G_TYPE_CHECK_INSTANCE_CAST ((obj), TYPE_EDITFPROP, EditFProp))
#define EDITFPROP_CLASS(klasse)  (G_TYPE_CHECK_CLASS_CAST ((klasse), TYPE_EDITFPROP, EditFPropClass))
#define IS_EDITFPROP(obj)        (G_TYPE_CHECK_INSTANCE_TYPE ((obj), TYPE_EDITFPROP))

typedef struct _EditFPropClass EditFPropClass;
typedef struct _EditFProp EditFProp;

struct _EditFPropClass
{
  GschemDialogClass parent_class;
};

struct _EditFProp
{
  GschemDialog parent;

  GtkWidget *fstylecb;
  GtkWidget *widthe;
  GtkWidget *angle1e;
  GtkWidget *pitch1e;
  GtkWidget *angle2e;
  GtkWidget *pitch2e;
};


/*! \brief get the filltype data from selected objects
 *  \par Function Description
 *  Get filltype information over all selected objects.
 *  If a object property is different to the other objects, then
 *  return -2 in that variable.
 *  \param [in]   selection the selection list
 *  \param [out]  type      OBJECT_FILLING type
 *  \param [out]  width     fill width.
 *  \param [out]  pitch1    cross hatch line distance
 *  \param [out]  angle1    cross hatch angle
 *  \param [out]  pitch2    cross hatch line distance
 *  \param [out]  angle2    cross hatch angle
 *  \returns TRUE if filltype found, FALSE otherwise
 */
static gboolean selection_get_fill_type(GList *selection,
                                        OBJECT_FILLING *type, gint *width,
                                        gint *pitch1, gint *angle1,
                                        gint *pitch2, gint *angle2)
{
  GList *iter;
  OBJECT *object;
  gboolean found = FALSE;
  OBJECT_FILLING otype;
  gint owidth, opitch1, oangle1, opitch2, oangle2;


  for (iter = selection; iter != NULL; iter = g_list_next(iter)) {
    object = (OBJECT *) iter->data;
    if (! o_get_fill_options(object, &otype, &owidth,
                             &opitch1, &oangle1, &opitch2, &oangle2))
      continue;

    if (found == FALSE) {  /* first object with filltype */
      found = TRUE;
      *type = otype;
      *width = owidth;
      *pitch1 = opitch1;
      *angle1 = oangle1;
      *pitch2 = opitch2;
      *angle2 = oangle2;
    } else {
      /* indicate different values with the value -2 */
      if (*type != otype) *type = -2;
      if (*width != owidth) *width = -2;
      if (*pitch1 != opitch1) *pitch1 = -2;
      if (*angle1 != oangle1) *angle1 = -2;
      if (*pitch2 != opitch2) *pitch2 = -2;
      if (*angle2 != oangle2) *angle2 = -2;
    }
  }

  return found;
}


/*! \brief set the filltype in the filltype dialog
 *  \par Function Description
 *  Set all widgets in the filltype dialog. Variables marked with the
 *  invalid value -2 are set to *unchanged*.
 *  \param [in]   fill_type_data dialog structure
 *  \param [in]   type      OBJECT_FILLING type
 *  \param [in]   width     fill width.
 *  \param [in]   pitch1    cross hatch line distance
 *  \param [in]   angle1    cross hatch angle
 *  \param [in]   pitch2    cross hatch line distance
 *  \param [in]   angle2    cross hatch angle
 */
static void fill_type_dialog_set_values(EditFProp *dialog,
                                        OBJECT_FILLING type, gint width,
                                        gint pitch1, gint angle1,
                                        gint pitch2, gint angle2)
{
  gchar *text;

  if (width == -2)
    text = g_strdup(_("*unchanged*"));
  else
    text = g_strdup_printf ("%d", width);
  gtk_entry_set_text (GTK_ENTRY (dialog->widthe), text);
  gtk_entry_select_region (GTK_ENTRY (dialog->widthe),
                           0, strlen (text));
  g_free(text);

  if (pitch1 == -2)
    text = g_strdup(_("*unchanged*"));
  else
    text = g_strdup_printf ("%d", pitch1);
  gtk_entry_set_text (GTK_ENTRY (dialog->pitch1e), text);
  gtk_entry_select_region (GTK_ENTRY (dialog->pitch1e),
                           0, strlen (text));
  g_free(text);

  if (angle1 == -2)
    text = g_strdup(_("*unchanged*"));
  else
    text = g_strdup_printf ("%d", angle1);
  gtk_entry_set_text (GTK_ENTRY (dialog->angle1e), text);
  gtk_entry_select_region (GTK_ENTRY (dialog->angle1e),
                           0, strlen (text));
  g_free(text);

  if (pitch2 == -2)
    text = g_strdup(_("*unchanged*"));
  else
    text = g_strdup_printf ("%d", pitch2);
  gtk_entry_set_text (GTK_ENTRY (dialog->pitch2e), text);
  gtk_entry_select_region (GTK_ENTRY (dialog->pitch2e),
                           0, strlen (text));
  g_free(text);

  if (angle2 == -2)
    text = g_strdup(_("*unchanged*"));
  else
    text = g_strdup_printf ("%d", angle2);
  gtk_entry_set_text (GTK_ENTRY (dialog->angle2e), text);
  gtk_entry_select_region (GTK_ENTRY (dialog->angle2e),
                           0, strlen (text));
  g_free(text);

  /* Change the value of the combo box last, so the signal handler can
   * set the sensitivity of the other widgets after their values have
   * been set. Setting the value beforehand resulted in a widget with the
   * appearance of focus, but without sentitivity.
   */

  x_fstylecb_set_index (dialog->fstylecb, type);
}


/*! \brief Callback function for the filltype menu in the filltype dialog
 *  \par Function Description
 *  This function sets the entry activity according to the selected
 *  filltype of the filltype dialog.
 *
 *  \param [in] widget The fill style widget emitting the signal
 *  \param [in] dialog The edit fill properties dialog
 */
static gint
fill_type_dialog_filltype_change(GtkWidget *widget, EditFProp *dialog)
{
  gboolean activate_width_entry;
  gboolean activate_anglepitch1_entries;
  gboolean activate_anglepitch2_entries;

  activate_width_entry = x_fstylecb_get_use_width (widget);
  activate_anglepitch1_entries = x_fstylecb_get_use1 (widget);
  activate_anglepitch2_entries = x_fstylecb_get_use2 (widget);

  gtk_widget_set_sensitive (dialog->widthe,
                            activate_width_entry);
  gtk_widget_set_sensitive (dialog->angle1e,
                            activate_anglepitch1_entries);
  gtk_widget_set_sensitive (dialog->pitch1e,
                            activate_anglepitch1_entries);
  gtk_widget_set_sensitive (dialog->angle2e,
                            activate_anglepitch2_entries);
  gtk_widget_set_sensitive (dialog->pitch2e,
                            activate_anglepitch2_entries);

  return(0);
}

/*! \brief Handles the user response when apply is selected
 *
 *  \param [in] dialog The edit fill properties dialog
 */
static void
dialog_response_ok (EditFProp *dialog)
{
  GSCHEM_TOPLEVEL *w_current = dialog->parent.w_current;
  TOPLEVEL *toplevel = w_current->toplevel;
  GList *selection, *iter;
  OBJECT *object;
  const gchar *width_str, *angle1_str, *pitch1_str, *angle2_str, *pitch2_str;
  OBJECT_FILLING type;
  gint width, angle1, pitch1, angle2, pitch2;
  OBJECT_FILLING otype;
  gint owidth, oangle1, opitch1, oangle2, opitch2;

  /* get the selection */
  if (! o_select_selected(w_current))
    return;
  selection =
    geda_list_get_glist(w_current->toplevel->page_current->selection_list);

  /* get the new values from the text entries of the dialog */
  width_str  = gtk_entry_get_text (GTK_ENTRY (dialog->widthe));
  angle1_str = gtk_entry_get_text (GTK_ENTRY (dialog->angle1e));
  pitch1_str = gtk_entry_get_text (GTK_ENTRY (dialog->pitch1e));
  angle2_str = gtk_entry_get_text (GTK_ENTRY (dialog->angle2e));
  pitch2_str = gtk_entry_get_text (GTK_ENTRY (dialog->pitch2e));

  type = x_fstylecb_get_index (dialog->fstylecb);

  if (type == FILLING_VOID)
    type = -1;

  /* convert the options to integers (-1 means unchanged) */
  width  = g_utf8_collate (g_utf8_casefold (width_str, -1),
                           g_utf8_casefold(_("*unchanged*"), -1))
    ? atoi (width_str)  : -1;
  angle1 = g_utf8_collate (g_utf8_casefold (angle1_str, -1),
                           g_utf8_casefold (_("*unchanged*"), -1))
    ? atoi (angle1_str) : -1;
  pitch1 = g_utf8_collate (g_utf8_casefold (pitch1_str, -1),
                           g_utf8_casefold (_("*unchanged*"), -1))
    ? atoi (pitch1_str) : -1;
  angle2 = g_utf8_collate (g_utf8_casefold (angle2_str, -1),
                           g_utf8_casefold (_("*unchanged*"), -1))
    ? atoi (angle2_str) : -1;
  pitch2 = g_utf8_collate (g_utf8_casefold (pitch2_str, -1),
                           g_utf8_casefold(_("*unchanged*"), -1))
    ? atoi (pitch2_str) : -1;

  for (iter = selection; iter != NULL; iter = g_list_next(iter)) {
    object = (OBJECT *) iter->data;
    if (! o_get_fill_options(object, &otype, &owidth,
                             &opitch1, &oangle1, &opitch2, &oangle2))
      continue;

    otype = type == -1 ? otype : type;
    owidth = width == -1 ? owidth : width;
    opitch1 = pitch1 == -1 ? opitch1 : pitch1;
    oangle1 = angle1 == -1 ? oangle1 : angle1;
    opitch2 = pitch2 == -1 ? opitch2 : pitch2;
    oangle2 = angle2 == -1 ? oangle2 : angle2;

    /* set all not required options to -1 and
       set nice parameters if not provided by the user */
    switch (otype) {
    case (FILLING_HOLLOW):
    case (FILLING_FILL):
      owidth = opitch1 = oangle1 = opitch2 = oangle2 = -1;
      break;
    case (FILLING_HATCH):
      if (owidth < 1) owidth = 1;
      if (opitch1 < 1) opitch1 = 100;
      opitch2 = oangle2 = -1;
      break;
    case (FILLING_MESH):
      if (owidth < 1) owidth = 1;
      if (opitch1 < 1) opitch1 = 100;
      if (opitch2 < 1) opitch2 = 100;
      break;
    default:
      g_assert_not_reached();
    }

    o_set_fill_options (toplevel, object, otype, owidth,
                        opitch1, oangle1, opitch2, oangle2);
  }

  toplevel->page_current->CHANGED = 1;
  o_undo_savestate(w_current, UNDO_ALL);
}



/*! \brief Handles user responses from the edit text dialog box

 *  \par Function Description
 *  Callback function for the edit text dialog.
 *
 *  \param [in,out] dialog The edit text dialog
 *  \param [na]     unused Unused parameter
 */
static void
dialog_response (EditFProp *dialog, gint response, gpointer unused)
{
  switch(response) {
    case GTK_RESPONSE_OK:
      dialog_response_ok(dialog);
      break;
    case GTK_RESPONSE_CLOSE:
    case GTK_RESPONSE_DELETE_EVENT:
       break;
    default:
      printf("%s: dialog_response(): strange signal %d\n", __FILE__, response);
  }

  i_set_state(dialog->parent.w_current, SELECT);
  i_update_toolbar(dialog->parent.w_current);
  gtk_widget_destroy(dialog->parent.w_current->fpwindow);
  dialog->parent.w_current->fpwindow=NULL;
}



/*! \brief Initialize EditFProp class
 *
 *  \par Function Description
 *
 *  GType class initialiser for Multiattrib. We override our parent
 *  virtual class methods as needed and register our GObject properties.
 *
 *  \param [in] klasse
 */
static void editfprop_class_init(EditFPropClass *klasse)
{
}



/*! \brief Initialize EditFProp instance
 *
 *  \param [in,out] dialog The edit text dialog
 */
static void editfprop_init(EditFProp *dialog)
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
                "title",           _("Edit Fill Type"),
                "default-width",   320,
                "default-height",  350,
                "window-position", GTK_WIN_POS_MOUSE,
                "allow-grow",      TRUE,
                "allow-shrink",    FALSE,
                "modal",           TRUE,
                /* GtkDialog */
                "has-separator",   TRUE,
                NULL);

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
  gtk_container_set_border_width(GTK_CONTAINER(dialog),DIALOG_BORDER_SPACING);
  gtk_box_set_spacing(GTK_BOX(vbox), DIALOG_V_SPACING);

  label = gtk_label_new (_("<b>Fill Properties</b>"));
  gtk_label_set_use_markup (GTK_LABEL (label), TRUE);
  gtk_misc_set_alignment (GTK_MISC (label), 0, 0);
  gtk_box_pack_start (GTK_BOX (vbox), label, FALSE, FALSE, 0);

  alignment = gtk_alignment_new(0,0,1,1);
  gtk_alignment_set_padding(GTK_ALIGNMENT(alignment), 0, 0,
                            DIALOG_INDENTATION, 0);
  gtk_box_pack_start(GTK_BOX(vbox), alignment, TRUE, TRUE, 0);

  table = gtk_table_new (6, 2, FALSE);
  gtk_table_set_row_spacings(GTK_TABLE(table), DIALOG_V_SPACING);
  gtk_table_set_col_spacings(GTK_TABLE(table), DIALOG_H_SPACING);
  gtk_container_add(GTK_CONTAINER(alignment), table);

  label = gtk_label_new (_("Fill Type:"));
  gtk_misc_set_alignment(GTK_MISC(label), 0, 0);
  gtk_table_attach(GTK_TABLE(table), label, 0,1,0,1, GTK_FILL,0,0,0);

  label = gtk_label_new (_("Line Width:"));
  gtk_misc_set_alignment(GTK_MISC(label), 0, 0);
  gtk_table_attach(GTK_TABLE(table), label, 0,1,1,2, GTK_FILL,0,0,0);

  label = gtk_label_new (_("Angle 1:"));
  gtk_misc_set_alignment(GTK_MISC(label), 0, 0);
  gtk_table_attach(GTK_TABLE(table), label, 0,1,2,3, GTK_FILL,0,0,0);

  label = gtk_label_new (_("Pitch 1:"));
  gtk_misc_set_alignment(GTK_MISC(label), 0, 0);
  gtk_table_attach(GTK_TABLE(table), label, 0,1,3,4, GTK_FILL,0,0,0);

  label = gtk_label_new (_("Angle 2:"));
  gtk_misc_set_alignment(GTK_MISC(label), 0, 0);
  gtk_table_attach(GTK_TABLE(table), label, 0,1,4,5, GTK_FILL,0,0,0);

  label = gtk_label_new (_("Pitch 2:"));
  gtk_misc_set_alignment(GTK_MISC(label), 0, 0);
  gtk_table_attach(GTK_TABLE(table), label, 0,1,5,6, GTK_FILL,0,0,0);

  dialog->fstylecb = x_fstylecb_new ();
  gtk_table_attach_defaults(GTK_TABLE(table), dialog->fstylecb,
                            1,2,0,1);

  dialog->widthe = gtk_entry_new();
  gtk_entry_set_activates_default (GTK_ENTRY(dialog->widthe), TRUE);
  gtk_table_attach_defaults(GTK_TABLE(table), dialog->widthe,
                            1,2,1,2);

  dialog->angle1e = gtk_entry_new ();
  gtk_entry_set_activates_default (GTK_ENTRY(dialog->angle1e), TRUE);
  gtk_table_attach_defaults(GTK_TABLE(table), dialog->angle1e,
                            1,2,2,3);

  dialog->pitch1e = gtk_entry_new ();
  gtk_entry_set_activates_default (GTK_ENTRY(dialog->pitch1e), TRUE);
  gtk_table_attach_defaults(GTK_TABLE(table), dialog->pitch1e,
                            1,2,3,4);

  dialog->angle2e = gtk_entry_new ();
  gtk_entry_set_activates_default (GTK_ENTRY(dialog->angle2e), TRUE);
  gtk_table_attach_defaults(GTK_TABLE(table), dialog->angle2e,
                            1,2,4,5);

  dialog->pitch2e = gtk_entry_new ();
  gtk_entry_set_activates_default (GTK_ENTRY(dialog->pitch2e), TRUE);
  gtk_table_attach_defaults(GTK_TABLE(table), dialog->pitch2e,
                            1,2,5,6);


  g_signal_connect (G_OBJECT (dialog->fstylecb), "changed",
                    G_CALLBACK (fill_type_dialog_filltype_change),
                    dialog);
}



/*! \brief Get/register EditFProp type.
 */
GType editfprop_get_type()
{
  static GType type = 0;

  if (type == 0) {
    static const GTypeInfo info = {
      sizeof(EditFPropClass),
      NULL,                                   /* base_init */
      NULL,                                   /* base_finalize */
      (GClassInitFunc) editfprop_class_init,
      NULL,                                   /* class_finalize */
      NULL,                                   /* class_data */
      sizeof(EditFProp),
      0,                                      /* n_preallocs */
      (GInstanceInitFunc) editfprop_init,
    };

    type = g_type_register_static (GSCHEM_TYPE_DIALOG, "EditFProp", &info, 0);
  }

  return type;
}



/*! \brief Open the dialog box to edit fill properties
 *
 *  \par Function Description
 *  This function creates or raises the modal text entry dialog
 *
 *  \param [in] w_current The gschem toplevel
 */
void
fill_type_dialog (GSCHEM_TOPLEVEL *w_current)
{
  gboolean success;
  OBJECT_FILLING type;
  gint width;
  gint pitch1;
  gint angle1;
  gint pitch2;
  gint angle2;
  GList *selection;

  if (!o_select_selected (w_current)) {
    return;
  }

  selection = geda_list_get_glist(w_current->toplevel->page_current->selection_list);

  success = selection_get_fill_type(selection,
                                    &type,
                                    &width,
                                    &pitch1,
                                    &angle1,
                                    &pitch2,
                                    &angle2);

  if (!success) {
    return;
  }

  if (w_current->fpwindow == NULL) {
    /* dialog not created yet */
    w_current->fpwindow = g_object_new (TYPE_EDITFPROP,
                                        /* GschemDialog */
                                        "settings-name",    "fill-type",
                                        "gschem-toplevel",  w_current,
                                        NULL);

    gtk_window_set_transient_for (GTK_WINDOW (w_current->main_window),
                                  GTK_WINDOW (w_current->fpwindow));

    fill_type_dialog_set_values(EDITFPROP (w_current->fpwindow),
                                type,
                                width,
                                pitch1,
                                angle1,
                                pitch2,
                                angle2);

    gtk_widget_show_all (w_current->fpwindow);
  }
  else {
    /* dialog already created */
    fill_type_dialog_set_values(EDITFPROP (w_current->fpwindow),
                                type,
                                width,
                                pitch1,
                                angle1,
                                pitch2,
                                angle2);

    gtk_window_present (GTK_WINDOW(w_current->fpwindow));
  }
}
