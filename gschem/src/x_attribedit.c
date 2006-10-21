/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 * Copyright (C) 1998-2000 Ales V. Hvezda
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

#include "../include/i_vars.h"
#include "../include/globals.h"
#include "../include/prototype.h"

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
int attrib_edit_dialog_keypress(GtkWidget * widget, GdkEventKey * event, 
				TOPLEVEL * w_current)
{
  if (strcmp(gdk_keyval_name(event->keyval), "Escape") == 0) {
    attrib_edit_dialog_cancel(NULL, w_current);	
    return TRUE;
  }
  
  return FALSE;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Documentation
 *
 */
void attrib_edit_dialog_ok(GtkWidget * w, TOPLEVEL * w_current)
{
  const char *value, *label;
  char *newtext;
  GtkEntry *value_entry, *name_entry;
  GtkWidget *visbutton, *show_options;
  GtkWidget *addtocompsbutton, *addtonetsbutton, *addtoallbutton;
  GtkWidget *overwritebutton;
  OBJECT *attribptr;
  OBJECT *object;
  SELECTION *s_current = NULL;
  ATTRIB *a_current = NULL, *a_sav;
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
    int world_x, world_y;
    OBJECT *new = NULL;

    s_current = w_current->page_current->selection2_head->next;
    while (s_current != NULL) {
      object = s_current->selected_object;
      if (object == NULL) {
	fprintf(stderr, _("ERROR: NULL object!\n"));
	exit(-1);
      }
      if (!s_current->selected_object->attached_to) {
	nsel++;
      }
      s_current = s_current->next;
    }
    s_current = w_current->page_current->selection2_head->next;
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

	object = s_current->selected_object;
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
	    a_current = object->attribs;
	    if (replace) {
	      while (a_current != NULL) {
		a_sav = a_current;
		a_current = a_current->next;

		if (a_sav->object->text != NULL) {
		  if (!strncmp
		      (a_sav->object->text->string, newtext,
		       strchr(newtext, '=') - newtext)) {
		    o_text_change(w_current, a_sav->object, 
				  newtext, vis, show);
		    replaced = TRUE;
		    w_current->page_current->CHANGED = 1;
		  }
		}
	      }
	    }
	    if (!replaced) {
	      new = o_attrib_add_attrib(w_current, newtext, vis, show, object);
	    }
	  }
	}
	s_current = s_current->next;
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
	SCREENtoWORLD(w_current, mouse_x, mouse_y, &world_x, &world_y);
	new->text->x = world_x;
	new->text->y = world_y;
	o_text_erase(w_current, new);
	o_text_recreate(w_current, new);
	o_text_draw(w_current, new);
	w_current->page_current->CHANGED = 1;
	o_undo_savestate(w_current, UNDO_ALL);
      }
    }
  } else {
    o_text_change(w_current, attribptr, newtext, vis, show);
    w_current->page_current->CHANGED = 1;
    o_undo_savestate(w_current, UNDO_ALL);
  }
  gtk_grab_remove(w_current->aewindow);
  gtk_widget_destroy(w_current->aewindow);
  w_current->aewindow = NULL;
  g_free(newtext);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Documentation
 *
 */
void attrib_edit_dialog_cancel(GtkWidget *w, TOPLEVEL *w_current)
{
  i_set_state(w_current, SELECT);
  gtk_grab_remove(w_current->aewindow);
  gtk_widget_destroy(w_current->aewindow);
  w_current->aewindow = NULL;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Documentation
 *
 */
void attrib_edit_dialog_delete(GtkWidget *w, TOPLEVEL *w_current)
{
  OBJECT *object;

  /* for now unselect everything, but in the future you really ought 
   * to just unselect a single object */
  o_unselect_all(w_current);

  object = gtk_object_get_data(GTK_OBJECT(w_current->aewindow),"attrib");
  o_delete_text(w_current, object);
  w_current->page_current->CHANGED=1;
  o_undo_savestate(w_current, UNDO_ALL);

  i_set_state(w_current, SELECT);
  i_update_menus(w_current);
  gtk_grab_remove(w_current->aewindow);
  gtk_widget_destroy(w_current->aewindow);
  w_current->aewindow = NULL;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Documentation
 *
 */
void attrib_edit_dialog(TOPLEVEL * w_current, OBJECT * list, int flag)
{
  GtkWidget *aewindow;
  GtkWidget *table1;
  GtkWidget *hbox1;
  GtkWidget *hbuttonbox1;
  GtkWidget *okbutton;
  GtkWidget *deletebutton=NULL;
  GtkWidget *cancelbutton;
  GtkWidget *frame1;
  GtkWidget *table3;
  GtkWidget *show_options;
  GtkWidget *show_options_menu;
  GtkWidget *glade_menuitem;
  GtkWidget *attrib_combo;
  GtkWidget *attrib_combo_entry;
  GtkWidget *value_entry;
  GtkWidget *visbutton;
  GtkWidget *label2;
  GtkWidget *label1;
  GtkWidget *frame2;
  GtkWidget *hbox2;
  GSList *hbox2_group = NULL;
  GtkWidget *addtoallbutton;
  GtkWidget *addtocompsbutton;
  GtkWidget *addtonetsbutton;
  GtkWidget *overwritebutton;

  /* gschem specific */
  SELECTION *s_current = NULL;
  GList *combo_items = NULL;
  char* string = NULL;
  int nsel=0, i, len;
  char *name = NULL;
  char *val = NULL;
  OBJECT *attrib = NULL;
  
  /* gschem specific */
  if (w_current->aewindow)
  return;

  /* gschem specific */
  s_current = w_current->page_current->selection2_head->next;
  while (s_current != NULL) {
    if (!s_current->selected_object->attached_to) {
      nsel++;
    }
    s_current = s_current->next;
  }
  aewindow = gtk_window_new (GTK_WINDOW_TOPLEVEL);
  gtk_object_set_data (GTK_OBJECT (aewindow), "aewindow", aewindow);
  gtk_window_set_title (GTK_WINDOW (aewindow), _("Single Attribute Editor"));
  gtk_window_set_position (GTK_WINDOW (aewindow), GTK_WIN_POS_MOUSE);
  gtk_window_set_modal (GTK_WINDOW (aewindow), TRUE);

  table1 = gtk_table_new (3, 1, FALSE);
  gtk_widget_ref (table1);
  gtk_object_set_data_full (GTK_OBJECT (aewindow), "table1", table1,
                            (GtkDestroyNotify) gtk_widget_unref);
  gtk_widget_show (table1);
  gtk_container_add (GTK_CONTAINER (aewindow), table1);
  gtk_container_set_border_width (GTK_CONTAINER (aewindow), 5);

  hbox1 = gtk_hbox_new (FALSE, 0);
  gtk_widget_ref (hbox1);
  gtk_object_set_data_full (GTK_OBJECT (aewindow), "hbox1", hbox1,
                            (GtkDestroyNotify) gtk_widget_unref);
  gtk_widget_show (hbox1);
  gtk_table_attach (GTK_TABLE (table1), hbox1, 0, 1, 2, 3,
                    (GtkAttachOptions) (GTK_FILL),
                    (GtkAttachOptions) (GTK_EXPAND | GTK_FILL), 0, 0);

  hbuttonbox1 = gtk_hbutton_box_new ();
  gtk_widget_ref (hbuttonbox1);
  gtk_object_set_data_full (GTK_OBJECT (aewindow), "hbuttonbox1", hbuttonbox1,
                            (GtkDestroyNotify) gtk_widget_unref);
  gtk_widget_show (hbuttonbox1);
  gtk_box_pack_start (GTK_BOX (hbox1), hbuttonbox1, TRUE, TRUE, 0);
  gtk_container_set_border_width (GTK_CONTAINER (hbuttonbox1), 1);

  if (list) { /* gschem specific */
    deletebutton = gtk_button_new_from_stock (GTK_STOCK_DELETE);
    gtk_widget_ref (deletebutton);
    gtk_object_set_data_full (GTK_OBJECT (aewindow), "deletebutton", deletebutton,
                              (GtkDestroyNotify) gtk_widget_unref);
    gtk_widget_show (deletebutton);
    gtk_container_add (GTK_CONTAINER (hbuttonbox1), deletebutton);
    gtk_container_set_border_width (GTK_CONTAINER (deletebutton), 3);
    GTK_WIDGET_SET_FLAGS (deletebutton, GTK_CAN_DEFAULT);
  }

  cancelbutton = gtk_button_new_from_stock (GTK_STOCK_CANCEL);
  gtk_widget_ref (cancelbutton);
  gtk_object_set_data_full (GTK_OBJECT (aewindow), "cancelbutton", cancelbutton,
                            (GtkDestroyNotify) gtk_widget_unref);
  gtk_widget_show (cancelbutton);
  gtk_container_add (GTK_CONTAINER (hbuttonbox1), cancelbutton);
  gtk_container_set_border_width (GTK_CONTAINER (cancelbutton), 3);
  GTK_WIDGET_SET_FLAGS (cancelbutton, GTK_CAN_DEFAULT);

  okbutton = gtk_button_new_from_stock (GTK_STOCK_OK);
  gtk_widget_ref (okbutton);
  gtk_object_set_data_full (GTK_OBJECT (aewindow), "okbutton", okbutton,
                            (GtkDestroyNotify) gtk_widget_unref);
  gtk_widget_show (okbutton);
  gtk_container_add (GTK_CONTAINER (hbuttonbox1), okbutton);
  gtk_container_set_border_width (GTK_CONTAINER (okbutton), 3);
  GTK_WIDGET_SET_FLAGS (okbutton, GTK_CAN_DEFAULT);
  gtk_button_set_relief (GTK_BUTTON (okbutton), GTK_RELIEF_HALF);
  
  frame1 = gtk_frame_new (_("Add/Edit Attribute"));
  gtk_widget_ref (frame1);
  gtk_object_set_data_full (GTK_OBJECT (aewindow), "frame1", frame1,
                            (GtkDestroyNotify) gtk_widget_unref);
  gtk_widget_show (frame1);
  gtk_table_attach (GTK_TABLE (table1), frame1, 0, 1, 0, 1,
                    (GtkAttachOptions) (GTK_EXPAND | GTK_FILL),
                    (GtkAttachOptions) (GTK_FILL), 0, 0);
  gtk_container_set_border_width (GTK_CONTAINER (frame1), 6);

  table3 = gtk_table_new (3, 2, FALSE);
  gtk_widget_ref (table3);
  gtk_object_set_data_full (GTK_OBJECT (aewindow), "table3", table3,
                            (GtkDestroyNotify) gtk_widget_unref);
  gtk_widget_show (table3);
  gtk_container_add (GTK_CONTAINER (frame1), table3);
  gtk_container_set_border_width (GTK_CONTAINER (table3), 6);

  attrib_combo = gtk_combo_new ();
  gtk_widget_ref (attrib_combo);
  gtk_object_set_data_full (GTK_OBJECT (aewindow), "attrib_combo", attrib_combo,
                            (GtkDestroyNotify) gtk_widget_unref);
  gtk_widget_show (attrib_combo);
  gtk_table_attach (GTK_TABLE (table3), attrib_combo, 1, 2, 0, 1,
                    (GtkAttachOptions) (GTK_EXPAND | GTK_FILL),
                    (GtkAttachOptions) (0), 0, 0);

  attrib_combo_entry = GTK_COMBO (attrib_combo)->entry;
  gtk_widget_ref (attrib_combo_entry);
  gtk_object_set_data_full (GTK_OBJECT (aewindow), "attrib_combo_entry", attrib_combo_entry,
                            (GtkDestroyNotify) gtk_widget_unref);
  gtk_widget_show (attrib_combo_entry);

  value_entry = gtk_entry_new ();
  gtk_widget_ref (value_entry);
  gtk_object_set_data_full (GTK_OBJECT (aewindow), "value_entry", value_entry,
                            (GtkDestroyNotify) gtk_widget_unref);
  gtk_widget_show (value_entry);
  gtk_table_attach (GTK_TABLE (table3), value_entry, 1, 2, 1, 2,
                    (GtkAttachOptions) (GTK_EXPAND | GTK_FILL),
                    (GtkAttachOptions) (0), 0, 8);

  visbutton = gtk_check_button_new_with_label (_("Visible"));
  gtk_widget_ref (visbutton);
  gtk_object_set_data_full (GTK_OBJECT (aewindow), "visbutton", visbutton,
                            (GtkDestroyNotify) gtk_widget_unref);
  gtk_widget_show (visbutton);
  gtk_table_attach (GTK_TABLE (table3), visbutton, 0, 1, 2, 3,
                    (GtkAttachOptions) (GTK_FILL),
                    (GtkAttachOptions) (0), 3, 0);
  gtk_container_set_border_width (GTK_CONTAINER (visbutton), 3);
  gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (visbutton), TRUE);

  label2 = gtk_label_new (_("Value:"));
  gtk_widget_ref (label2);
  gtk_object_set_data_full (GTK_OBJECT (aewindow), "label2", label2,
                            (GtkDestroyNotify) gtk_widget_unref);
  gtk_widget_show (label2);
  gtk_table_attach (GTK_TABLE (table3), label2, 0, 1, 1, 2,
                    (GtkAttachOptions) (0),
                    (GtkAttachOptions) (0), 0, 3);
  gtk_misc_set_alignment (GTK_MISC (label2), 0, 0.5);

  label1 = gtk_label_new (_("Name:"));
  gtk_widget_ref (label1);
  gtk_object_set_data_full (GTK_OBJECT (aewindow), "label1", label1,
                            (GtkDestroyNotify) gtk_widget_unref);
  gtk_widget_show (label1);
  gtk_table_attach (GTK_TABLE (table3), label1, 0, 1, 0, 1,
                    (GtkAttachOptions) (0),
                    (GtkAttachOptions) (0), 0, 3);
  gtk_label_set_justify (GTK_LABEL (label1), GTK_JUSTIFY_RIGHT);
  gtk_misc_set_alignment (GTK_MISC (label1), 0, 0.5);
  gtk_misc_set_padding (GTK_MISC (label1), 4, 0);

  show_options = gtk_option_menu_new ();
  gtk_widget_ref (show_options);
  gtk_object_set_data_full (GTK_OBJECT (aewindow), "show_options",
                            show_options,
                            (GtkDestroyNotify) gtk_widget_unref);
  gtk_widget_show (show_options);
  gtk_table_attach (GTK_TABLE (table3), show_options, 1, 2, 2, 3,
                    (GtkAttachOptions) (GTK_FILL),
                    (GtkAttachOptions) (0), 0, 0);
  show_options_menu = gtk_menu_new ();
  glade_menuitem = gtk_menu_item_new_with_label (_("Show Value Only"));
  gtk_widget_show (glade_menuitem);
  gtk_menu_append (GTK_MENU (show_options_menu), glade_menuitem);
  glade_menuitem = gtk_menu_item_new_with_label (_("Show Name Only"));
  gtk_widget_show (glade_menuitem);
  gtk_menu_append (GTK_MENU (show_options_menu), glade_menuitem);
  glade_menuitem = gtk_menu_item_new_with_label (_("Show Name & Value"));
  gtk_widget_show (glade_menuitem);
  gtk_menu_append (GTK_MENU (show_options_menu), glade_menuitem);
  gtk_option_menu_set_menu (GTK_OPTION_MENU (show_options), show_options_menu);
  gtk_option_menu_set_history (GTK_OPTION_MENU (show_options), 0);
  
  if (nsel > 1) { /* gschem specific */
    frame2 = gtk_frame_new (_("Multiple Attach"));
    gtk_widget_ref (frame2);
    gtk_object_set_data_full (GTK_OBJECT (aewindow), "frame2", frame2,
                              (GtkDestroyNotify) gtk_widget_unref);
    gtk_widget_show (frame2);
    gtk_table_attach (GTK_TABLE (table1), frame2, 0, 1, 1, 2,
                      (GtkAttachOptions) (GTK_EXPAND | GTK_FILL),
                      (GtkAttachOptions) (GTK_FILL), 0, 0);
    gtk_container_set_border_width (GTK_CONTAINER (frame2), 6);

    hbox2 = gtk_hbox_new (FALSE, 0);
    gtk_widget_ref (hbox2);
    gtk_object_set_data_full (GTK_OBJECT (aewindow), "hbox2", hbox2,
                              (GtkDestroyNotify) gtk_widget_unref);
    gtk_widget_show (hbox2);
    gtk_container_add (GTK_CONTAINER (frame2), hbox2);

    addtoallbutton = gtk_radio_button_new_with_label (hbox2_group, _("All"));
    hbox2_group = gtk_radio_button_group (GTK_RADIO_BUTTON (addtoallbutton));
    gtk_widget_ref (addtoallbutton);
    gtk_object_set_data_full (GTK_OBJECT (aewindow), "addtoallbutton", addtoallbutton,
                              (GtkDestroyNotify) gtk_widget_unref);
    gtk_widget_show (addtoallbutton);
    gtk_box_pack_start (GTK_BOX (hbox2), addtoallbutton, FALSE, FALSE, 0);
    gtk_container_set_border_width (GTK_CONTAINER (addtoallbutton), 3);

    addtocompsbutton = gtk_radio_button_new_with_label (hbox2_group, _("Components"));
    hbox2_group = gtk_radio_button_group (GTK_RADIO_BUTTON (addtocompsbutton));
    gtk_widget_ref (addtocompsbutton);
    gtk_object_set_data_full (GTK_OBJECT (aewindow), "addtocompsbutton", addtocompsbutton,
                              (GtkDestroyNotify) gtk_widget_unref);
    gtk_widget_show (addtocompsbutton);
    gtk_box_pack_start (GTK_BOX (hbox2), addtocompsbutton, FALSE, FALSE, 0);
    gtk_container_set_border_width (GTK_CONTAINER (addtocompsbutton), 3);

    addtonetsbutton = gtk_radio_button_new_with_label (hbox2_group, _("Nets"));
    hbox2_group = gtk_radio_button_group (GTK_RADIO_BUTTON (addtonetsbutton));
    gtk_widget_ref (addtonetsbutton);
    gtk_object_set_data_full (GTK_OBJECT (aewindow), "addtonetsbutton", addtonetsbutton,
                              (GtkDestroyNotify) gtk_widget_unref);
    gtk_widget_show (addtonetsbutton);
    gtk_box_pack_start (GTK_BOX (hbox2), addtonetsbutton, FALSE, FALSE, 0);

    overwritebutton = gtk_check_button_new_with_label (_("Replace"));
    gtk_widget_ref (overwritebutton);
    gtk_object_set_data_full (GTK_OBJECT (aewindow), "overwritebutton", overwritebutton,
                              (GtkDestroyNotify) gtk_widget_unref);
    gtk_widget_show (overwritebutton);
    gtk_box_pack_start (GTK_BOX (hbox2), overwritebutton, FALSE, FALSE, 0);
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
  gtk_widget_show(aewindow);
  w_current->aewindow = aewindow;

  /* gschem specific */
  gtk_signal_connect(GTK_OBJECT(aewindow), "destroy",
		     GTK_SIGNAL_FUNC(destroy_window),
		     &w_current->aewindow);
  gtk_signal_connect(GTK_OBJECT(okbutton), "clicked",
		     GTK_SIGNAL_FUNC(attrib_edit_dialog_ok), w_current);
  gtk_signal_connect(GTK_OBJECT(cancelbutton), "clicked",
		     GTK_SIGNAL_FUNC(attrib_edit_dialog_cancel),
		     w_current);
  gtk_signal_connect(GTK_OBJECT(value_entry), "activate",
		     GTK_SIGNAL_FUNC(attrib_edit_dialog_ok), w_current);

  gtk_signal_connect(GTK_OBJECT(aewindow), "key_press_event",
                     (GtkSignalFunc) attrib_edit_dialog_keypress, w_current);

  if (list) {
    gtk_signal_connect(GTK_OBJECT(deletebutton), "clicked",
		       GTK_SIGNAL_FUNC(attrib_edit_dialog_delete),
		       w_current);
  }

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
