/* Lepton EDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2016 gEDA Contributors
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

#include "gschem.h"

/***************** Start of Attrib Edit dialog box ********************/
/*! \section attrib-edit-dialog-box Atrib Edit Dialog Box */

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Documentation
 *
 */
void attrib_edit_dialog_ok(GtkWidget * w, GschemToplevel *w_current)
{
  LeptonToplevel *toplevel = gschem_toplevel_get_toplevel (w_current);
  const char *value, *label;
  char *newtext;
  GtkEntry *value_entry, *name_entry;
  GtkWidget *visbutton, *show_options;
  GtkWidget *addtocompsbutton, *addtonetsbutton, *addtoallbutton;
  GtkWidget *overwritebutton;
  LeptonObject *attribptr;
  LeptonObject *object;
  GList *s_current = NULL;
  LeptonObject *a_current;
  GList *a_iter;
  int vis, show;
  int invocation_flag;
  int nsel=0, addto=0, replace=0, addmask=0;
  int option_index;
  gint wx, wy;

  value_entry =
    GTK_ENTRY (g_object_get_data (G_OBJECT (w_current->aewindow),
                                  "value_entry"));
  name_entry =
    GTK_ENTRY (g_object_get_data (G_OBJECT (w_current->aewindow),
                                  "attrib_combo_entry"));
  visbutton =
    GTK_WIDGET (g_object_get_data (G_OBJECT (w_current->aewindow),
                                   "visbutton"));
  show_options =
    GTK_WIDGET (g_object_get_data (G_OBJECT (w_current->aewindow),
                                   "show_options"));

  value = gtk_entry_get_text(value_entry);
  label = gtk_entry_get_text(name_entry);
  newtext = g_strconcat (label, "=", value, NULL);

  if (!x_dialog_validate_attribute(GTK_WINDOW(w_current->aewindow), newtext))
  {
    g_free(newtext);
    return;
  }

  if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(visbutton)))
  vis = VISIBLE;
  else
  vis = INVISIBLE;

  option_index = gtk_combo_box_get_active (GTK_COMBO_BOX (show_options));
  switch(option_index) {
    case(0):
      show = SHOW_VALUE;
      break;

    case(1):
      show = SHOW_NAME;
      break;

    case(2):
      show = SHOW_NAME_VALUE;
      break;

    default:
      fprintf(stderr, _("Got invalid show option; defaulting to show both\n"));
      show = SHOW_NAME_VALUE;
      break;
  }

  attribptr =
    (LeptonObject*) g_object_get_data (G_OBJECT (w_current->aewindow), "attrib");
  if (!attribptr) {
    LeptonObject *new_object = NULL;

    s_current = lepton_list_get_glist( toplevel->page_current->selection_list );
    while (s_current != NULL) {
      object = (LeptonObject *)s_current->data;
      if (object == NULL) {
        fprintf (stderr, "attrib_edit_dialog_ok: ERROR: Got an unexpected NULL\n");
        exit(-1);
      }
      if (!lepton_object_get_attached_to (object))
      {
        nsel++;
      }
      s_current = g_list_next(s_current);
    }
    s_current = lepton_list_get_glist( toplevel->page_current->selection_list );
    if (nsel > 1) {

      addtoallbutton =
        GTK_WIDGET (g_object_get_data (G_OBJECT (w_current->aewindow),
                                       "addtoallbutton"));

      addtocompsbutton =
        GTK_WIDGET (g_object_get_data (G_OBJECT (w_current->aewindow),
                                       "addtocompsbutton"));

      addtonetsbutton =
        GTK_WIDGET (g_object_get_data (G_OBJECT (w_current->aewindow),
                                       "addtonetsbutton"));

      overwritebutton =
        GTK_WIDGET (g_object_get_data (G_OBJECT (w_current->aewindow),
                                       "overwritebutton"));

      if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(overwritebutton))) {
        replace = 1;
      } else {
        replace = 0;
      }

      if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(addtoallbutton))) {
        addto = 7;
      }
      if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(addtocompsbutton))) {
        addto = 2;
      }
      if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(addtonetsbutton))) {
        addto = 1;
      }

      while (s_current != NULL) {
        gboolean replaced;

        object = (LeptonObject *) s_current->data;
        if (object &&
            !lepton_object_get_attached_to (object) &&
            !lepton_object_is_text (object))
        {
          addmask = 4;
          if (lepton_object_is_component (object))
          {
            addmask = 2;
          }
          if (lepton_object_is_net (object)) {
            addmask = 1;
          }
          replaced = FALSE;
          if (addmask & addto) {
            a_iter = lepton_object_get_attribs (object);
            if (replace) {
              while (a_iter != NULL) {
                a_current = (LeptonObject*) a_iter->data;
                const gchar *str = lepton_text_object_get_string (a_current);
                if (str) {
                  if (!strncmp (str, newtext, strchr (newtext, '=') - newtext)) {
                    o_text_change(w_current, a_current, newtext, vis, show);
                    replaced = TRUE;
                    gschem_toplevel_page_content_changed (w_current,
                                                          schematic_window_get_active_page (w_current));
                  }
                }
                a_iter = g_list_next (a_iter);
              }
            }
            if (!replaced) {
              o_attrib_add_attrib(w_current, newtext, vis, show, object);
            }
          }
        }
        s_current = g_list_next (s_current);
      }
      o_undo_savestate_old(w_current, UNDO_ALL);
    } else {
      object = o_select_return_first_object(w_current);
      new_object = o_attrib_add_attrib(w_current, newtext, vis, show, object);

      invocation_flag =
        GPOINTER_TO_INT (g_object_get_data (G_OBJECT (w_current->aewindow),
                                            "invocation_flag"));
      wx =
        GPOINTER_TO_INT (g_object_get_data (G_OBJECT (w_current->aewindow),
                                            "position_wx"));
      wy =
        GPOINTER_TO_INT (g_object_get_data (G_OBJECT (w_current->aewindow),
                                            "position_wy"));

#if DEBUG
      printf("invocation flag: %d\n", invocation_flag);
#endif
      if (invocation_flag == FROM_HOTKEY
          && wx != -1 && wy != -1) {
        o_invalidate (w_current, new_object);
        lepton_text_object_set_x (new_object, wx);
        lepton_text_object_set_y (new_object, wy);
        lepton_text_object_recreate (new_object);
        gschem_toplevel_page_content_changed (w_current,
                                              schematic_window_get_active_page (w_current));
        o_undo_savestate_old(w_current, UNDO_ALL);
      }
    }
  } else {
    o_text_change(w_current, attribptr, newtext, vis, show);
    gschem_toplevel_page_content_changed (w_current,
                                          schematic_window_get_active_page (w_current));
    o_undo_savestate_old(w_current, UNDO_ALL);
  }
  gtk_grab_remove(w_current->aewindow);
  gtk_widget_destroy(w_current->aewindow);
  w_current->aewindow = NULL;
  g_free(newtext);
}

/*! \brief Response function for the attribute add/edit dialog
 *  \par Function Description
 *  This function catches the user response for the add and edit
 *  attribute dialog.
 */
void attribute_edit_dialog_response(GtkWidget *w, gint response,
                                    GschemToplevel *w_current)
{
  switch(response) {
  case GTK_RESPONSE_APPLY:
    attrib_edit_dialog_ok(NULL, w_current);
    break;
  case GTK_RESPONSE_REJECT:
  case GTK_RESPONSE_DELETE_EVENT:
    gtk_grab_remove(w_current->aewindow);
    gtk_widget_destroy(w_current->aewindow);
    w_current->aewindow = NULL;
    break;
  default:
    printf("attrib_edit_dialog_response(): strange signal %d\n", response);
  }
  /* clean up */
}



/*! \brief Create the attribute add/edit dialog
 *  \par Function Description
 *  This function creates the single attribute edit dialog.
 */
void attrib_edit_dialog (GschemToplevel *w_current, LeptonObject *attr_obj, int flag)
{
  LeptonToplevel *toplevel = gschem_toplevel_get_toplevel (w_current);
  GtkWidget *aewindow;
  GtkWidget *vbox, *label, *alignment;
  GtkWidget *show_options;
  GtkWidget *attrib_combo_box_entry;
  GtkWidget *attrib_combo_entry;
  GtkWidget *value_entry;
  GtkWidget *visbutton;
  GSList *hbox2_group = NULL;
  GtkWidget *addtoallbutton;
  GtkWidget *addtocompsbutton;
  GtkWidget *addtonetsbutton;
  GtkWidget *overwritebutton;
  GtkEntryCompletion *attrib_combo_entry_completion;

  /* gschem specific */
  GList *s_current = NULL;
  char* string = NULL;
  int nsel=0, i, len;
  char *name = NULL;
  char *val = NULL;
  LeptonObject *attrib = NULL;
  gint wx, wy;

  /* gschem specific */
  if (w_current->aewindow)
    return;

  /* gschem specific: What do we count here? (Werner)  */
  for (s_current = lepton_list_get_glist( toplevel->page_current->selection_list );
       s_current != NULL;
       s_current = g_list_next(s_current)) {
    if (!lepton_object_get_attached_to ((LeptonObject *) s_current->data))
    {
      nsel++;
    }
  }

  aewindow = gschem_dialog_new_with_buttons(NULL,
                                            GTK_WINDOW(w_current->main_window),
                                            GTK_DIALOG_MODAL,
                                            "singleattrib", w_current,
                                            GTK_STOCK_CANCEL,
                                            GTK_RESPONSE_REJECT,
                                            GTK_STOCK_OK,
                                            GTK_RESPONSE_APPLY,
                                            NULL);
  /* Set the alternative button order (ok, cancel, help) for other systems */
  gtk_dialog_set_alternative_button_order(GTK_DIALOG(aewindow),
                                          GTK_RESPONSE_APPLY,
                                          GTK_RESPONSE_REJECT,
                                          -1);

  g_signal_connect (G_OBJECT (aewindow), "response",
                    G_CALLBACK (attribute_edit_dialog_response),
                    w_current);

  gtk_window_set_position (GTK_WINDOW (aewindow), GTK_WIN_POS_MOUSE);

  gtk_dialog_set_default_response(GTK_DIALOG(aewindow),
                                  GTK_RESPONSE_APPLY);

  vbox = gtk_dialog_get_content_area (GTK_DIALOG (aewindow));
  gtk_container_set_border_width(GTK_CONTAINER(aewindow),
                                 DIALOG_BORDER_SPACING);
  gtk_box_set_spacing(GTK_BOX(vbox), DIALOG_V_SPACING);

  if (attr_obj != NULL)
  {
    gtk_window_set_title (GTK_WINDOW(aewindow), _("Edit Attribute"));
  }
  else
  {
    gtk_window_set_title (GTK_WINDOW(aewindow), _("Add Attribute"));
  }

  alignment = gtk_alignment_new(0,0,1,1);
  gtk_alignment_set_padding(GTK_ALIGNMENT(alignment), 0, 0,
                            DIALOG_INDENTATION, 0);
  gtk_box_pack_start(GTK_BOX(vbox), alignment, TRUE, TRUE, 0);

#ifdef ENABLE_GTK3
  GtkWidget *grid = gtk_grid_new ();
  gtk_grid_set_row_spacing (GTK_GRID (grid), DIALOG_V_SPACING);
  gtk_grid_set_column_spacing (GTK_GRID (grid), DIALOG_H_SPACING);
  gtk_container_add (GTK_CONTAINER (alignment), grid);
#else
  GtkWidget *table = gtk_table_new (3, 2, FALSE);
  gtk_table_set_row_spacings(GTK_TABLE(table), DIALOG_V_SPACING);
  gtk_table_set_col_spacings(GTK_TABLE(table), DIALOG_H_SPACING);
  gtk_container_add (GTK_CONTAINER (alignment), table);
#endif

  /* Name selection */
  label = gtk_label_new_with_mnemonic (_("N_ame:"));
  gtk_misc_set_alignment (GTK_MISC (label), 0, 0.5);
#ifdef ENABLE_GTK3
  gtk_grid_attach (GTK_GRID (grid), label, 0, 0, 1, 1);
#else
  gtk_table_attach (GTK_TABLE (table), label, 0, 1, 0, 1,
                    (GtkAttachOptions) (GTK_FILL),
                    (GtkAttachOptions) (GTK_FILL), 0, 0);
#endif

  attrib_combo_box_entry = gtk_combo_box_text_new_with_entry ();
  attrib_combo_entry = gtk_bin_get_child(GTK_BIN(attrib_combo_box_entry));

  gtk_label_set_mnemonic_widget (GTK_LABEL (label), attrib_combo_box_entry);

#ifdef ENABLE_GTK3
  gtk_grid_attach (GTK_GRID (grid), attrib_combo_box_entry, 1, 0, 1, 1);
#else
  gtk_table_attach (GTK_TABLE (table), attrib_combo_box_entry, 1, 2, 0, 1,
                    (GtkAttachOptions) (GTK_EXPAND | GTK_FILL),
                    (GtkAttachOptions) (0), 0, 0);
#endif
  g_object_ref (attrib_combo_entry);
  g_object_set_data_full (G_OBJECT (aewindow),
                         "attrib_combo_entry", attrib_combo_entry,
                            (GDestroyNotify) g_object_unref);
  gtk_entry_set_activates_default (GTK_ENTRY (attrib_combo_entry), TRUE);

  /* Value entry */
  label = gtk_label_new_with_mnemonic (_("_Value:"));
  gtk_misc_set_alignment (GTK_MISC (label), 0, 0.5);
#ifdef ENABLE_GTK3
  gtk_grid_attach (GTK_GRID (grid), label, 0, 1, 1, 1);
#else
  gtk_table_attach (GTK_TABLE (table), label, 0, 1, 1, 2,
                    (GtkAttachOptions) (GTK_FILL),
                    (GtkAttachOptions) (0), 0, 0);
#endif

  value_entry = gtk_entry_new ();
  g_object_ref (value_entry);
  g_object_set_data_full (G_OBJECT (aewindow), "value_entry", value_entry,
                          (GDestroyNotify) g_object_unref);

  gtk_label_set_mnemonic_widget (GTK_LABEL (label), value_entry);

#ifdef ENABLE_GTK3
  gtk_grid_attach (GTK_GRID (grid), value_entry, 1, 1, 1, 1);
#else
  gtk_table_attach (GTK_TABLE (table), value_entry, 1, 2, 1, 2,
                    (GtkAttachOptions) (GTK_EXPAND | GTK_FILL),
                    (GtkAttachOptions) (0), 0, 0);
#endif
  gtk_entry_set_activates_default(GTK_ENTRY(value_entry), TRUE);

  /* Visibility */
  visbutton = gtk_check_button_new_with_mnemonic (_("Vi_sible"));
  g_object_ref (visbutton);
  g_object_set_data_full (G_OBJECT (aewindow), "visbutton", visbutton,
                          (GDestroyNotify) g_object_unref);

  gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (visbutton), TRUE);
#ifdef ENABLE_GTK3
  gtk_grid_attach (GTK_GRID (grid), visbutton, 0, 2, 1, 1);
#else
  gtk_table_attach (GTK_TABLE (table), visbutton, 0, 1, 2, 3,
                    (GtkAttachOptions) (GTK_FILL),
                    (GtkAttachOptions) (0), 0, 0);
#endif

  show_options = gtk_combo_box_text_new ();
  GtkListStore *store = gtk_list_store_new (1, G_TYPE_STRING);
  gtk_combo_box_set_model (GTK_COMBO_BOX (show_options), GTK_TREE_MODEL(store));

  g_object_ref (show_options);
  g_object_set_data_full (G_OBJECT (aewindow), "show_options", show_options,
                          (GDestroyNotify) g_object_unref);

#ifdef ENABLE_GTK3
  gtk_grid_attach (GTK_GRID (grid), show_options, 1, 2, 1, 1);
#else
  gtk_table_attach (GTK_TABLE (table), show_options, 1, 2, 2, 3,
                    (GtkAttachOptions) (GTK_FILL | GTK_EXPAND),
                    (GtkAttachOptions) (0), 0, 0);
#endif

  gtk_combo_box_text_append_text (GTK_COMBO_BOX_TEXT (show_options),
                                  _("Show Value Only"));
  gtk_combo_box_text_append_text (GTK_COMBO_BOX_TEXT (show_options),
                                  _("Show Name Only"));
  gtk_combo_box_text_append_text (GTK_COMBO_BOX_TEXT (show_options),
                                  _("Show Name & Value"));
  gtk_combo_box_set_active (GTK_COMBO_BOX (show_options), 0);

  if (nsel > 1) { /* gschem specific */

    label = gtk_label_new(_("<b>Attach Options</b>"));
    gtk_label_set_use_markup (GTK_LABEL (label), TRUE);
    gtk_misc_set_alignment(GTK_MISC(label),0,0);
    gtk_box_pack_start(GTK_BOX(vbox), label, FALSE, FALSE, 0);

    alignment = gtk_alignment_new(0,0,1,1);
    gtk_alignment_set_padding(GTK_ALIGNMENT(alignment), 0, 0,
                              DIALOG_INDENTATION, 0);
    gtk_box_pack_start(GTK_BOX(vbox), alignment, TRUE, TRUE, 0);

#ifdef ENABLE_GTK3
    grid = gtk_grid_new ();
    gtk_grid_set_row_spacing (GTK_GRID (grid), DIALOG_V_SPACING);
    gtk_grid_set_column_spacing (GTK_GRID (grid), DIALOG_H_SPACING);
    gtk_container_add (GTK_CONTAINER (alignment), grid);
#else
    table = gtk_table_new (2, 3, FALSE);
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
  w_current->aewindow = aewindow;

  gtk_grab_add(w_current->aewindow);

  if (attrib || (name && strcmp(name, "netname") == 0)) {
    gtk_widget_grab_focus(value_entry);
  } else {
    gtk_widget_grab_focus(attrib_combo_entry);
  }

  g_free(name);
  g_free(val);

}
/***************** End of Attrib Edit dialog box **********************/
