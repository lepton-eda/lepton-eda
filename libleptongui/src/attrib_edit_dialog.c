/* Lepton EDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2016 gEDA Contributors
 * Copyright (C) 2017-2026 Lepton EDA Contributors
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
/*! \todo STILL NEED to clean up line lengths in aa and tr
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

/***************** Start of Attrib Edit dialog box ********************/
/*! \section attrib-edit-dialog-box Atrib Edit Dialog Box */


/*! \brief Create the attribute add/edit dialog
 *  \par Function Description
 *  This function creates the single attribute edit dialog.
 */
void
attrib_edit_dialog (SchematicWindow *w_current,
                    GtkWidget *aewindow,
                    GtkWidget *vbox,
                    GtkWidget *name_label,
                    GtkWidget *value_label,
                    GtkWidget *attrib_combo_box_entry,
                    GtkWidget *attrib_combo_entry,
                    GtkWidget *value_entry,
                    GtkWidget *visbutton,
                    GtkWidget *show_options,
                    LeptonObject *attr_obj,
                    int flag,
                    int nsel)
{
  GtkWidget *attach_options_label;
  GSList *hbox2_group = NULL;
  GtkWidget *addtoallbutton;
  GtkWidget *addtocompsbutton;
  GtkWidget *addtonetsbutton;
  GtkWidget *overwritebutton;
  GtkEntryCompletion *attrib_combo_entry_completion;

  /* gschem specific */
  char* string = NULL;
  int i, len;
  char *name = NULL;
  char *val = NULL;
  LeptonObject *attrib = NULL;
  gint wx, wy;

  if (nsel > 1) { /* gschem specific */

    attach_options_label = gtk_label_new(_("<b>Attach Options</b>"));
    gtk_label_set_use_markup (GTK_LABEL (attach_options_label), TRUE);
#ifdef ENABLE_GTK3
    gtk_label_set_xalign (GTK_LABEL (attach_options_label), 0.0);
    gtk_label_set_yalign (GTK_LABEL (attach_options_label), 0.0);
#else
    gtk_misc_set_alignment(GTK_MISC(attach_options_label),0,0);
#endif
    gtk_box_pack_start(GTK_BOX(vbox), attach_options_label, FALSE, FALSE, 0);

#ifdef ENABLE_GTK3
    GtkWidget *grid = gtk_grid_new ();
    gtk_grid_set_row_spacing (GTK_GRID (grid), DIALOG_V_SPACING);
    gtk_grid_set_column_spacing (GTK_GRID (grid), DIALOG_H_SPACING);
    gtk_box_pack_start(GTK_BOX(vbox), grid, TRUE, TRUE, 0);
#else
    GtkWidget *alignment = gtk_alignment_new(0,0,1,1);
    gtk_alignment_set_padding(GTK_ALIGNMENT(alignment), 0, 0,
                              DIALOG_INDENTATION, 0);
    gtk_box_pack_start(GTK_BOX(vbox), alignment, TRUE, TRUE, 0);

    GtkWidget *table = gtk_table_new (2, 3, FALSE);
    gtk_table_set_row_spacings(GTK_TABLE(table), DIALOG_V_SPACING);
    gtk_table_set_col_spacings(GTK_TABLE(table), DIALOG_H_SPACING);
    gtk_container_add (GTK_CONTAINER (alignment), table);
#endif

    addtoallbutton = gtk_radio_button_new_with_label (hbox2_group, _("All"));
    hbox2_group = gtk_radio_button_get_group (GTK_RADIO_BUTTON (addtoallbutton));
    g_object_ref (addtoallbutton);
    g_object_set_data_full (G_OBJECT (aewindow), "addtoallbutton", addtoallbutton,
                            (GDestroyNotify) g_object_unref);
#ifdef ENABLE_GTK3
    gtk_grid_attach (GTK_GRID (grid), addtoallbutton, 0, 0, 1, 1);
#else
    gtk_table_attach(GTK_TABLE(table),
                     addtoallbutton,
                     0,
                     1,
                     0,
                     1,
                     (GtkAttachOptions) (GTK_FILL),
                     (GtkAttachOptions) 0,
                     0,
                     0);
#endif

    addtocompsbutton = gtk_radio_button_new_with_label (hbox2_group, _("Components"));
    hbox2_group = gtk_radio_button_get_group (GTK_RADIO_BUTTON (addtocompsbutton));
    g_object_ref (addtocompsbutton);
    g_object_set_data_full (G_OBJECT (aewindow), "addtocompsbutton", addtocompsbutton,
                            (GDestroyNotify) g_object_unref);
#ifdef ENABLE_GTK3
    gtk_grid_attach (GTK_GRID (grid), addtocompsbutton, 1, 0, 1, 1);
#else
    gtk_table_attach(GTK_TABLE(table),
                     addtocompsbutton,
                     1,
                     2,
                     0,
                     1,
                     (GtkAttachOptions) (GTK_FILL),
                     (GtkAttachOptions) 0,
                     0,
                     0);
#endif

    addtonetsbutton = gtk_radio_button_new_with_label (hbox2_group, _("Nets"));
    hbox2_group = gtk_radio_button_get_group (GTK_RADIO_BUTTON (addtonetsbutton));
    g_object_ref (addtonetsbutton);
    g_object_set_data_full (G_OBJECT (aewindow), "addtonetsbutton", addtonetsbutton,
                            (GDestroyNotify) g_object_unref);
#ifdef ENABLE_GTK3
    gtk_grid_attach (GTK_GRID (grid), addtonetsbutton, 2, 0, 1, 1);
#else
    gtk_table_attach(GTK_TABLE(table),
                     addtonetsbutton,
                     2,
                     3,
                     0,
                     1,
                     (GtkAttachOptions) (GTK_FILL),
                     (GtkAttachOptions) 0,
                     0,
                     0);
#endif

    overwritebutton = gtk_check_button_new_with_label (_("Replace existing attributes"));
    gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON(overwritebutton), TRUE);
    g_object_ref (overwritebutton);
    g_object_set_data_full (G_OBJECT (aewindow), "overwritebutton", overwritebutton,
                            (GDestroyNotify) g_object_unref);
#ifdef ENABLE_GTK3
    gtk_grid_attach (GTK_GRID (grid), overwritebutton, 0, 1, 1, 1);
#else
    gtk_table_attach(GTK_TABLE(table),
                     overwritebutton,
                     0,
                     3,
                     1,
                     2,
                     GTK_FILL,
                     (GtkAttachOptions) 0,
                     0,
                     0);
#endif
  }

  /* gschem specific */
  if (attr_obj) {
    name = g_strdup (lepton_text_object_get_name (attr_obj));
    val = g_strdup (lepton_text_object_get_value (attr_obj));
    attrib = attr_obj;
    if (lepton_text_object_is_visible (attrib)) {
      gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(visbutton), TRUE);
    } else {
      gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(visbutton), FALSE);
    }

    if (lepton_text_object_get_show (attrib) == SHOW_VALUE)
    {
      gtk_combo_box_set_active (GTK_COMBO_BOX (show_options), 0);
    }
    else if (lepton_text_object_get_show (attrib) == SHOW_NAME)
    {
      gtk_combo_box_set_active (GTK_COMBO_BOX (show_options), 1);
    }
    else
    {
      gtk_combo_box_set_active (GTK_COMBO_BOX (show_options), 2);
    }
  } else {
    LeptonObject *object;

    attrib = NULL;

    if ((object = o_select_return_first_object(w_current))) {
      if (lepton_object_is_net (object))
        name = g_strdup("netname");
    }

    gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(visbutton), TRUE);
    /* show value only */
    gtk_combo_box_set_active (GTK_COMBO_BOX (show_options), 0);
  }
  g_object_set_data (G_OBJECT (aewindow), "attrib", attrib);
  if (name) {
    gtk_entry_set_text(GTK_ENTRY(attrib_combo_entry), name);
  }
  if (val) {
    gtk_entry_set_text(GTK_ENTRY(value_entry), val);
    len = strlen(val);
    gtk_editable_select_region (GTK_EDITABLE (value_entry), 0, len);
  }
  g_object_set_data (G_OBJECT (aewindow), "invocation_flag",
                     GINT_TO_POINTER(flag));

  if (!x_event_get_pointer_position(w_current, TRUE, &wx, &wy)) {
    wx = wy = -1;
  }
  g_object_set_data (G_OBJECT (aewindow), "position_wx", GINT_TO_POINTER(wx));
  g_object_set_data (G_OBJECT (aewindow), "position_wy", GINT_TO_POINTER(wy));

  /* gschem specific */
  i = 0;
  string = (char *) s_attrib_get(i);
  while (string != NULL) {
    gtk_combo_box_text_append_text (GTK_COMBO_BOX_TEXT (attrib_combo_box_entry),
                                    string);
    i++;
    string = (char *) s_attrib_get(i);
  }

  /* Add completion to attribute combo box entry */
  attrib_combo_entry_completion = gtk_entry_completion_new();
  gtk_entry_completion_set_model(attrib_combo_entry_completion,
          gtk_combo_box_get_model(GTK_COMBO_BOX(attrib_combo_box_entry)));
  gtk_entry_completion_set_text_column(attrib_combo_entry_completion, 0);
  gtk_entry_completion_set_inline_completion(attrib_combo_entry_completion, TRUE);
  gtk_entry_completion_set_popup_single_match(attrib_combo_entry_completion, FALSE);
  gtk_entry_set_completion(GTK_ENTRY(attrib_combo_entry), attrib_combo_entry_completion);

  /* gschem specific */
  gtk_widget_show_all(aewindow);
  schematic_window_set_attrib_edit_widget (w_current, aewindow);

  gtk_grab_add (aewindow);

  if (attrib || (name && strcmp(name, "netname") == 0)) {
    gtk_widget_grab_focus(value_entry);
  } else {
    gtk_widget_grab_focus(attrib_combo_entry);
  }

  g_free(name);
  g_free(val);

}
/***************** End of Attrib Edit dialog box **********************/
