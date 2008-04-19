/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 * Copyright (C) 1998-2007 Ales Hvezda
 * Copyright (C) 1998-2007 gEDA Contributors (see ChangeLog for details)
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
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111 USA
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

#include <libgeda/libgeda.h>

#include "../include/gschem_struct.h"
#include "../include/i_vars.h"
#include "../include/globals.h"
#include "../include/prototype.h"
#include "../include/gschem_dialog.h"
#include "../include/x_dialog.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

/***************** Start of Attrib Edit dialog box ********************/
/*! \section attrib-edit-dialog-box Atrib Edit Dialog Box */

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Documentation
 *
 */
gint option_menu_get_history (GtkOptionMenu *option_menu) 
{ 
  GtkWidget *active_widget; 
         
  g_return_val_if_fail (GTK_IS_OPTION_MENU (option_menu), -1); 
         
  active_widget = gtk_menu_get_active (GTK_MENU (option_menu->menu)); 

  if (active_widget) 
    return g_list_index (GTK_MENU_SHELL (option_menu->menu)->children, 
			 active_widget); 
  else 
    return -1; 
} 

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Documentation
 *
 */
void attrib_edit_dialog_ok(GtkWidget * w, GSCHEM_TOPLEVEL *w_current)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  const char *value, *label;
  char *newtext;
  GtkEntry *value_entry, *name_entry;
  GtkWidget *visbutton, *show_options;
  GtkWidget *addtocompsbutton, *addtonetsbutton, *addtoallbutton;
  GtkWidget *overwritebutton;
  OBJECT *attribptr;
  OBJECT *object;
  GList *s_current = NULL;
  ATTRIB *a_current;
  GList *a_iter;
  int vis, show;
  int invocation_flag;
  int nsel=0, addto=0, replace=0, addmask=0;
  int option_index;

  i_set_state(w_current, SELECT);

  value_entry =
  gtk_object_get_data(GTK_OBJECT(w_current->aewindow), "value_entry");
  name_entry =
  gtk_object_get_data(GTK_OBJECT(w_current->aewindow), "attrib_combo_entry");
  visbutton =
  gtk_object_get_data(GTK_OBJECT(w_current->aewindow), "visbutton");
  show_options =
  gtk_object_get_data(GTK_OBJECT(w_current->aewindow), "show_options");

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

  option_index = option_menu_get_history(GTK_OPTION_MENU (show_options));
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
  gtk_object_get_data(GTK_OBJECT(w_current->aewindow), "attrib");
  if (!attribptr) {
    OBJECT *new = NULL;

    s_current = geda_list_get_glist( toplevel->page_current->selection_list );
    while (s_current != NULL) {
      object = (OBJECT *)s_current->data;
      if (object == NULL) {
	fprintf(stderr, _("ERROR: NULL object!\n"));
	exit(-1);
      }
      if (!object->attached_to) {
	nsel++;
      }
      s_current = g_list_next(s_current);
    }
    s_current = geda_list_get_glist( toplevel->page_current->selection_list );
    if (nsel > 1) {

      addtoallbutton =
        gtk_object_get_data(GTK_OBJECT(w_current->aewindow),
                            "addtoallbutton");

      addtocompsbutton =
        gtk_object_get_data(GTK_OBJECT(w_current->aewindow),
                            "addtocompsbutton");

      addtonetsbutton =
        gtk_object_get_data(GTK_OBJECT(w_current->aewindow),
                            "addtonetsbutton");

      overwritebutton =
        gtk_object_get_data(GTK_OBJECT(w_current->aewindow),
                            "overwritebutton");

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

        object = (OBJECT *) s_current->data;
        if (object && !object->attached_to && object->type != OBJ_TEXT ) {
          addmask = 4;
          if (object->type == OBJ_COMPLEX || object->type == OBJ_PLACEHOLDER) {
            addmask = 2;
          }
          if (object->type == OBJ_NET) {
            addmask = 1;
          }
          replaced = FALSE;
          if (addmask & addto) {
            a_iter = object->attribs;
            if (replace) {
              while (a_iter != NULL) {
                a_current = a_iter->data;

                if (a_current->object->text != NULL) {
                  if (!strncmp(a_current->object->text->string, newtext,
                               strchr(newtext, '=') - newtext)) {
                    o_text_change(w_current, a_current->object,
                                  newtext, vis, show);
                    replaced = TRUE;
                    toplevel->page_current->CHANGED = 1;
                  }
                }
                a_iter = g_list_next (a_iter);
              }
            }
            if (!replaced) {
              new = o_attrib_add_attrib(w_current, newtext, vis, show, object);
            }
          }
        }
        s_current = g_list_next (s_current);
      }
      o_undo_savestate(w_current, UNDO_ALL);
    } else {
      object = o_select_return_first_object(w_current);
      new = o_attrib_add_attrib(w_current, newtext, vis, show, object);

      invocation_flag =
	GPOINTER_TO_INT( gtk_object_get_data(GTK_OBJECT(w_current->aewindow),
				  "invocation_flag") );
      
#if DEBUG
      printf("invocation flag: %d\n", invocation_flag);
#endif
      if (invocation_flag == FROM_HOTKEY) {
	new->text->x = snap_grid(toplevel, mouse_wx);
	new->text->y = snap_grid(toplevel, mouse_wy);
	o_erase_single(w_current, new);
	o_text_recreate(toplevel, new);
	o_text_draw(w_current, new);
	toplevel->page_current->CHANGED = 1;
	o_undo_savestate(w_current, UNDO_ALL);
      }
    }
  } else {
    o_text_change(w_current, attribptr, newtext, vis, show);
    toplevel->page_current->CHANGED = 1;
    o_undo_savestate(w_current, UNDO_ALL);
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
				 GSCHEM_TOPLEVEL *w_current)
{
  switch(response) {
  case GTK_RESPONSE_APPLY:
    attrib_edit_dialog_ok(NULL, w_current);
    break;
  case GTK_RESPONSE_REJECT:
  case GTK_RESPONSE_DELETE_EVENT:
    i_set_state(w_current, SELECT);
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
void attrib_edit_dialog(GSCHEM_TOPLEVEL *w_current, OBJECT * list, int flag)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  GtkWidget *aewindow;
  GtkWidget *vbox, *label, *table, *alignment;
  GtkWidget *show_options;
  GtkWidget *show_options_menu;
  GtkWidget *glade_menuitem;
  GtkWidget *attrib_combo;
  GtkWidget *attrib_combo_entry;
  GtkWidget *value_entry;
  GtkWidget *visbutton;
  GSList *hbox2_group = NULL;
  GtkWidget *addtoallbutton;
  GtkWidget *addtocompsbutton;
  GtkWidget *addtonetsbutton;
  GtkWidget *overwritebutton;

  /* gschem specific */
  GList *s_current = NULL;
  GList *combo_items = NULL;
  char* string = NULL;
  int nsel=0, i, len;
  char *name = NULL;
  char *val = NULL;
  OBJECT *attrib = NULL;
  
  /* gschem specific */
  if (w_current->aewindow)
    return;

  /* gschem specific: What do we count here? (Werner)  */
  for (s_current = geda_list_get_glist( toplevel->page_current->selection_list );
       s_current != NULL;
       s_current = g_list_next(s_current)) {
    if (!((OBJECT *) s_current->data)->attached_to) {
      nsel++;
    }
  }

  aewindow = gschem_dialog_new_with_buttons(_("Single Attribute Editor"),
                                            GTK_WINDOW(w_current->main_window),
                                            GTK_DIALOG_MODAL,
                                            "singleattrib", w_current,
                                            GTK_STOCK_CANCEL,
                                            GTK_RESPONSE_REJECT,
                                            GTK_STOCK_OK,
                                            GTK_RESPONSE_APPLY,
                                            NULL);
#if GTK_CHECK_VERSION (2,6,0)
  /* Set the alternative button order (ok, cancel, help) for other systems */
  gtk_dialog_set_alternative_button_order(GTK_DIALOG(aewindow),
					  GTK_RESPONSE_APPLY,
					  GTK_RESPONSE_REJECT,
					  -1);
#endif
					 
  gtk_signal_connect(GTK_OBJECT(aewindow), "response",
		     GTK_SIGNAL_FUNC(attribute_edit_dialog_response), w_current);

  gtk_window_set_position (GTK_WINDOW (aewindow), GTK_WIN_POS_MOUSE);

  gtk_dialog_set_default_response(GTK_DIALOG(aewindow),
                                  GTK_RESPONSE_APPLY);

  vbox = GTK_DIALOG(aewindow)->vbox;
  gtk_container_set_border_width(GTK_CONTAINER(aewindow), 
				 DIALOG_BORDER_SPACING);
  gtk_box_set_spacing(GTK_BOX(vbox), DIALOG_V_SPACING);

  if (list)
    label = gtk_label_new(_("<b>Edit Attribute</b>"));
  else
    label = gtk_label_new(_("<b>Add Attribute</b>"));
  gtk_label_set_use_markup (GTK_LABEL (label), TRUE);
  gtk_misc_set_alignment(GTK_MISC(label),0,0);
  gtk_box_pack_start(GTK_BOX(vbox), label, FALSE, FALSE, 0);  
  
  alignment = gtk_alignment_new(0,0,1,1);
  gtk_alignment_set_padding(GTK_ALIGNMENT(alignment), 0, 0, 
			    DIALOG_INDENTATION, 0);
  gtk_box_pack_start(GTK_BOX(vbox), alignment, TRUE, TRUE, 0);

  table = gtk_table_new (3, 2, FALSE);
  gtk_table_set_row_spacings(GTK_TABLE(table), DIALOG_V_SPACING);
  gtk_table_set_col_spacings(GTK_TABLE(table), DIALOG_H_SPACING);
  gtk_container_add (GTK_CONTAINER (alignment), table);

  /* Name selection */
  label = gtk_label_new (_("Name:"));
  gtk_misc_set_alignment (GTK_MISC (label), 0, 0.5);
  gtk_table_attach (GTK_TABLE (table), label, 0, 1, 0, 1,
                    (GtkAttachOptions) (GTK_FILL),
                    (GtkAttachOptions) (GTK_FILL), 0, 0);

  attrib_combo = gtk_combo_new ();
  gtk_table_attach (GTK_TABLE (table), attrib_combo, 1, 2, 0, 1,
                    (GtkAttachOptions) (GTK_EXPAND | GTK_FILL),
                    (GtkAttachOptions) (0), 0, 0);
  attrib_combo_entry = GTK_COMBO (attrib_combo)->entry;
  gtk_widget_ref (attrib_combo_entry);
  gtk_object_set_data_full (GTK_OBJECT (aewindow), "attrib_combo_entry", attrib_combo_entry,
                            (GtkDestroyNotify) gtk_widget_unref);

  /* Value entry */
  label = gtk_label_new (_("Value:"));
  gtk_misc_set_alignment (GTK_MISC (label), 0, 0.5);
  gtk_table_attach (GTK_TABLE (table), label, 0, 1, 1, 2,
                    (GtkAttachOptions) (GTK_FILL),
                    (GtkAttachOptions) (0), 0, 0);

  value_entry = gtk_entry_new ();
  gtk_widget_ref (value_entry);
  gtk_object_set_data_full (GTK_OBJECT (aewindow), "value_entry", value_entry,
                            (GtkDestroyNotify) gtk_widget_unref);
  gtk_table_attach (GTK_TABLE (table), value_entry, 1, 2, 1, 2,
                    (GtkAttachOptions) (GTK_EXPAND | GTK_FILL),
                    (GtkAttachOptions) (0), 0, 0);
  gtk_entry_set_activates_default(GTK_ENTRY(value_entry), TRUE);

  /* Visibility */
  visbutton = gtk_check_button_new_with_label (_("Visible"));
  gtk_widget_ref (visbutton);
  gtk_object_set_data_full (GTK_OBJECT (aewindow), "visbutton", visbutton,
                            (GtkDestroyNotify) gtk_widget_unref);

  gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (visbutton), TRUE);
  gtk_table_attach (GTK_TABLE (table), visbutton, 0, 1, 2, 3,
                    (GtkAttachOptions) (GTK_FILL),
                    (GtkAttachOptions) (0), 0, 0);

  show_options = gtk_option_menu_new ();
  gtk_widget_ref (show_options);
  gtk_object_set_data_full (GTK_OBJECT (aewindow), "show_options",
                            show_options,
                            (GtkDestroyNotify) gtk_widget_unref);
  gtk_widget_show (show_options);
  gtk_table_attach (GTK_TABLE (table), show_options, 1, 2, 2, 3,
                    (GtkAttachOptions) (GTK_FILL | GTK_EXPAND),
                    (GtkAttachOptions) (0), 0, 0);
  show_options_menu = gtk_menu_new ();
  glade_menuitem = gtk_menu_item_new_with_label (_("Show Value Only"));
  gtk_menu_append (GTK_MENU (show_options_menu), glade_menuitem);
  glade_menuitem = gtk_menu_item_new_with_label (_("Show Name Only"));
  gtk_menu_append (GTK_MENU (show_options_menu), glade_menuitem);
  glade_menuitem = gtk_menu_item_new_with_label (_("Show Name & Value"));
  gtk_menu_append (GTK_MENU (show_options_menu), glade_menuitem);
  gtk_option_menu_set_menu (GTK_OPTION_MENU (show_options), show_options_menu);
  gtk_option_menu_set_history (GTK_OPTION_MENU (show_options), 0);
  
  if (nsel > 1) { /* gschem specific */
    
    label = gtk_label_new(_("<b>Attach Options</b>"));
    gtk_label_set_use_markup (GTK_LABEL (label), TRUE);
    gtk_misc_set_alignment(GTK_MISC(label),0,0);
    gtk_box_pack_start(GTK_BOX(vbox), label, FALSE, FALSE, 0);  
    
    alignment = gtk_alignment_new(0,0,1,1);
    gtk_alignment_set_padding(GTK_ALIGNMENT(alignment), 0, 0, 
			    DIALOG_INDENTATION, 0);
    gtk_box_pack_start(GTK_BOX(vbox), alignment, TRUE, TRUE, 0);

    table = gtk_table_new (2, 3, FALSE);
    gtk_table_set_row_spacings(GTK_TABLE(table), DIALOG_V_SPACING);
    gtk_table_set_col_spacings(GTK_TABLE(table), DIALOG_H_SPACING);
    gtk_container_add (GTK_CONTAINER (alignment), table);

    addtoallbutton = gtk_radio_button_new_with_label (hbox2_group, _("All"));
    hbox2_group = gtk_radio_button_group (GTK_RADIO_BUTTON (addtoallbutton));
    gtk_widget_ref (addtoallbutton);
    gtk_object_set_data_full (GTK_OBJECT (aewindow), "addtoallbutton", addtoallbutton,
                              (GtkDestroyNotify) gtk_widget_unref);
    gtk_table_attach(GTK_TABLE(table), addtoallbutton, 0, 1, 0, 1,
		     (GtkAttachOptions) (GTK_FILL), 0, 0, 0);
		     
    addtocompsbutton = gtk_radio_button_new_with_label (hbox2_group, _("Components"));
    hbox2_group = gtk_radio_button_group (GTK_RADIO_BUTTON (addtocompsbutton));
    gtk_widget_ref (addtocompsbutton);
    gtk_object_set_data_full (GTK_OBJECT (aewindow), "addtocompsbutton", addtocompsbutton,
                              (GtkDestroyNotify) gtk_widget_unref);
    gtk_table_attach(GTK_TABLE(table), addtocompsbutton, 1, 2, 0, 1,
		     (GtkAttachOptions) (GTK_FILL), 0, 0, 0);

    addtonetsbutton = gtk_radio_button_new_with_label (hbox2_group, _("Nets"));
    hbox2_group = gtk_radio_button_group (GTK_RADIO_BUTTON (addtonetsbutton));
    gtk_widget_ref (addtonetsbutton);
    gtk_object_set_data_full (GTK_OBJECT (aewindow), "addtonetsbutton", addtonetsbutton,
                              (GtkDestroyNotify) gtk_widget_unref);
    gtk_table_attach(GTK_TABLE(table), addtonetsbutton, 2, 3, 0, 1,
		     (GtkAttachOptions) (GTK_FILL), 0, 0, 0);

    overwritebutton = gtk_check_button_new_with_label (_("Replace existing attributes"));
    gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON(overwritebutton), TRUE);
    gtk_widget_ref (overwritebutton);
    gtk_object_set_data_full (GTK_OBJECT (aewindow), "overwritebutton", overwritebutton,
                              (GtkDestroyNotify) gtk_widget_unref);
    gtk_table_attach(GTK_TABLE(table), overwritebutton, 0, 3, 1, 2,
		     (GtkAttachOptions) (GTK_FILL), 0, 0, 0);
  }

  /* gschem specific */
  if (list) {
    o_attrib_get_name_value(list->text->string, &name, &val);
    attrib = list;
    if (attrib->visibility == VISIBLE) {
      gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(visbutton), TRUE);
    } else {
      gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(visbutton), FALSE);
    }

    if (attrib->show_name_value == SHOW_VALUE) {
      gtk_option_menu_set_history (GTK_OPTION_MENU (show_options), 0);
    } else if (attrib->show_name_value == SHOW_NAME) {
      gtk_option_menu_set_history (GTK_OPTION_MENU (show_options), 1);
    } else {
      gtk_option_menu_set_history (GTK_OPTION_MENU (show_options), 2);
    }
  } else {
    OBJECT *object;

    attrib = NULL;

    if ((object = o_select_return_first_object(w_current))) {
      if (object->type == OBJ_NET)
	name = g_strdup("netname");
    }

    gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(visbutton), TRUE);
    /* show value only */
    gtk_option_menu_set_history (GTK_OPTION_MENU (show_options), 0);
  }
  gtk_object_set_data(GTK_OBJECT(aewindow), "attrib", attrib);
  if (name) {
    gtk_entry_set_text(GTK_ENTRY(attrib_combo_entry), name);
  }
  if (val) {
    gtk_entry_set_text(GTK_ENTRY(value_entry), val);
    len = strlen(val);
    gtk_entry_select_region(GTK_ENTRY(value_entry), 0, len);
  }
  gtk_object_set_data(GTK_OBJECT(aewindow), "invocation_flag",
                      GINT_TO_POINTER(flag));
  
  /* gschem specific */
  i = 0;
  string = (char *) s_attrib_get(i);
  while (string != NULL) {
    combo_items = g_list_append(combo_items, string);
    i++;
    string = (char *) s_attrib_get(i);
  }
  combo_items = g_list_prepend(combo_items, name);
  gtk_combo_set_popdown_strings(GTK_COMBO(attrib_combo), combo_items);
  g_list_free(combo_items);
  
  /* gschem specific */
  gtk_widget_show_all(aewindow);
  w_current->aewindow = aewindow;

  gtk_grab_add(w_current->aewindow);

  if (attrib || (name && strcmp(name, "netname") == 0)) { 
    gtk_widget_grab_focus(value_entry);
  } else {  
    gtk_widget_grab_focus(attrib_combo_entry);
  }

  if (name) g_free(name);
  if (val) g_free(val);

}
/***************** End of Attrib Edit dialog box **********************/
