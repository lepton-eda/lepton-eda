/* gEDA - GNU Electronic Design Automation
 * gschem - GNU Schematic Capture
 * Copyright (C) 1998 Ales V. Hvezda
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

#include <config.h>

#include <stdio.h>
#include <stdlib.h>
#ifdef HAVE_STRINGS_H
#include <strings.h>
#endif

#include <libgeda/libgeda.h>

#include "../include/x_states.h"
#include "../include/prototype.h"

static const gchar *list_item_data_key = "list_item_data";

gint
change_attr(GtkWidget *gtklist, TOPLEVEL *w_current)
{
        GList		*dlist;
        GtkObject       *listitem;
        gchar           *item_data_string;
	int len;

        dlist = GTK_LIST(w_current->attr_list)->selection;

        if (!dlist) {
		/* g_print("Selection cleared\n");*/
		return(0);
        }

        listitem = GTK_OBJECT(dlist->data);
        item_data_string = gtk_object_get_data(listitem, list_item_data_key);

#if DEBUG
	printf("%s\n", item_data_string);
#endif

#if 0
	strcpy(current_attr_name, item_data_string);
#endif

	len = strlen(item_data_string);
        gtk_entry_set_text(GTK_ENTRY(w_current->asentry_name),
			   item_data_string);
        gtk_entry_select_region(GTK_ENTRY(w_current->asentry_name), 0, len);

	return(0);
}

gint
attr_set_show_name(GtkWidget *w, TOPLEVEL *w_current)
{
	o_attrib_set_show(w_current, SHOW_NAME);
	return(0);
}

gint
attr_set_show_value(GtkWidget *w, TOPLEVEL *w_current )
{
	o_attrib_set_show(w_current, SHOW_VALUE);
	return(0);
}

gint
attr_set_show_both(GtkWidget *w, TOPLEVEL *w_current )
{
	o_attrib_set_show(w_current, SHOW_NAME_VALUE);
	return(0);
}

gint
attr_set_visible(GtkWidget *w, TOPLEVEL *w_current )
{
	o_attrib_set_visible(w_current, VISIBLE);
	return(0);
}

gint
attr_set_invisible(GtkWidget *w, TOPLEVEL *w_current )
{
	o_attrib_set_visible(w_current, INVISIBLE);
	return(0);
}

gint
attr_apply(GtkWidget *w, TOPLEVEL *w_current)
{
	char *current_value = NULL;
	char *current_name = NULL;
#if 0
	int name_len;
	int value_len;
#endif

	current_name  =
		gtk_entry_get_text(GTK_ENTRY(w_current->asentry_name));
	current_value =
		gtk_entry_get_text(GTK_ENTRY(w_current->asentry_value));

	if ((current_value[0] != '\0') && (current_name[0] != '\0')) {
		char *combined =
			u_basic_strdup_multiple(current_name, "=",
						current_value, NULL);

		o_attrib_set_string(w_current, combined);
		w_current->event_state = DRAWATTRIB;

		if (combined) {
			free(combined);
		}
	}
	return(0);
}

gint
attr_cancel(GtkWidget *w, TOPLEVEL *w_current)
{
	gtk_widget_destroy(w_current->aswindow);
	w_current->aswindow = NULL;
	return(0);
}

void
setup_matt_attr_selector (TOPLEVEL *w_current)
{
char *text[3];
int i;
OBJECT **attriblist=NULL;


GtkWidget *window1;
  GtkWidget *vbox1;
  GtkWidget *scrolledwindow1;
  GtkWidget *clist;
  GtkWidget *attribhead;
  GtkWidget *vishead;
  GtkWidget *valhead;
  GtkWidget *hbox1;
  GtkWidget *hbox2;
  GtkWidget *label10;
  GtkWidget *combo2;
  GList *combo2_items = NULL;
  GtkWidget *combo_entry2;
  GtkWidget *hbox5;
  GSList *vis_group = NULL;
  GtkWidget *radiobutton1;
  GtkWidget *radiobutton2;
  GtkWidget *hbox3;
  GtkWidget *hbox4;
  GtkWidget *label11;
  GtkWidget *entry2;
  GtkWidget *hbox6;
  GSList *show_group = NULL;
  GtkWidget *radiobutton3;
  GtkWidget *radiobutton4;
  GtkWidget *radiobutton5;
  GtkWidget *hbuttonbox1;
  GtkWidget *addbutton;
  GtkWidget *updbutton;
  GtkWidget *delbutton;
  GtkWidget *closebutton;

  window1 = gtk_window_new (GTK_WINDOW_TOPLEVEL);
  gtk_object_set_data (GTK_OBJECT (window1), "window1", window1);
  gtk_window_set_title (GTK_WINDOW (window1), "window1");

  vbox1 = gtk_vbox_new (FALSE, 0);
  gtk_widget_show (vbox1);
  gtk_container_add (GTK_CONTAINER (window1), vbox1);

  scrolledwindow1 = gtk_scrolled_window_new (NULL, NULL);
  gtk_widget_show (scrolledwindow1);
  gtk_box_pack_start (GTK_BOX (vbox1), scrolledwindow1, TRUE, TRUE, 0);
  gtk_container_set_border_width (GTK_CONTAINER (scrolledwindow1), 3);
  gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (scrolledwindow1), GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);

  clist = gtk_clist_new (3);
  gtk_widget_show (clist);
  gtk_container_add (GTK_CONTAINER (scrolledwindow1), clist);
  gtk_clist_set_column_width (GTK_CLIST (clist), 0, 80);
  gtk_clist_set_column_width (GTK_CLIST (clist), 1, 80);
  gtk_clist_set_column_width (GTK_CLIST (clist), 2, 80);
  gtk_clist_column_titles_show (GTK_CLIST (clist));

  attribhead = gtk_label_new ("Attribute");
  gtk_widget_show (attribhead);
  gtk_clist_set_column_widget (GTK_CLIST (clist), 0, attribhead);

  vishead = gtk_label_new ("Visibility");
  gtk_widget_show (vishead);
  gtk_clist_set_column_widget (GTK_CLIST (clist), 1, vishead);

  valhead = gtk_label_new ("Value");
  gtk_widget_show (valhead);
  gtk_clist_set_column_widget (GTK_CLIST (clist), 2, valhead);


  hbox1 = gtk_hbox_new (TRUE, 0);
  gtk_widget_show (hbox1);
  gtk_box_pack_start (GTK_BOX (vbox1), hbox1, FALSE, FALSE, 0);

  hbox2 = gtk_hbox_new (FALSE, 0);
  gtk_widget_show (hbox2);
  gtk_box_pack_start (GTK_BOX (hbox1), hbox2, TRUE, TRUE, 0);
  gtk_container_set_border_width (GTK_CONTAINER (hbox2), 3);

  label10 = gtk_label_new ("Attribute");
  gtk_widget_show (label10);
  gtk_box_pack_start (GTK_BOX (hbox2), label10, TRUE, TRUE, 0);

  combo2 = gtk_combo_new ();
  gtk_widget_show (combo2);
  gtk_box_pack_start (GTK_BOX (hbox2), combo2, TRUE, FALSE, 0);
  combo2_items = g_list_append (combo2_items, "ref");
  combo2_items = g_list_append (combo2_items, "value");
  gtk_combo_set_popdown_strings (GTK_COMBO (combo2), combo2_items);
  g_list_free (combo2_items);

  combo_entry2 = GTK_COMBO (combo2)->entry;
  gtk_widget_show (combo_entry2);
  gtk_entry_set_text (GTK_ENTRY (combo_entry2), "ref");

  hbox5 = gtk_hbox_new (FALSE, 0);
  gtk_widget_show (hbox5);
  gtk_box_pack_start (GTK_BOX (hbox1), hbox5, TRUE, TRUE, 0);

  radiobutton1 = gtk_radio_button_new_with_label (vis_group, "Visible");
  vis_group = gtk_radio_button_group (GTK_RADIO_BUTTON (radiobutton1));
  gtk_widget_show (radiobutton1);
  gtk_box_pack_start (GTK_BOX (hbox5), radiobutton1, FALSE, FALSE, 0);
  gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (radiobutton1), TRUE);

  radiobutton2 = gtk_radio_button_new_with_label (vis_group, "Invisible");
  vis_group = gtk_radio_button_group (GTK_RADIO_BUTTON (radiobutton2));
  gtk_widget_show (radiobutton2);
  gtk_box_pack_start (GTK_BOX (hbox5), radiobutton2, FALSE, FALSE, 0);

  hbox3 = gtk_hbox_new (TRUE, 0);
  gtk_widget_show (hbox3);
  gtk_box_pack_start (GTK_BOX (vbox1), hbox3, FALSE, TRUE, 0);

  hbox4 = gtk_hbox_new (FALSE, 0);
  gtk_widget_show (hbox4);
  gtk_box_pack_start (GTK_BOX (hbox3), hbox4, TRUE, TRUE, 0);
  gtk_container_set_border_width (GTK_CONTAINER (hbox4), 3);

  label11 = gtk_label_new ("Value");
  gtk_widget_show (label11);
  gtk_box_pack_start (GTK_BOX (hbox4), label11, TRUE, FALSE, 0);

  entry2 = gtk_entry_new ();
  gtk_widget_show (entry2);
  gtk_box_pack_start (GTK_BOX (hbox4), entry2, TRUE, TRUE, 0);

  hbox6 = gtk_hbox_new (FALSE, 0);
  gtk_widget_show (hbox6);
  gtk_box_pack_start (GTK_BOX (hbox3), hbox6, TRUE, TRUE, 0);

  radiobutton3 = gtk_radio_button_new_with_label (show_group, "Label");
  show_group = gtk_radio_button_group (GTK_RADIO_BUTTON (radiobutton3));
  gtk_widget_show (radiobutton3);
  gtk_box_pack_start (GTK_BOX (hbox6), radiobutton3, FALSE, FALSE, 0);

  radiobutton4 = gtk_radio_button_new_with_label (show_group, "Value");
  show_group = gtk_radio_button_group (GTK_RADIO_BUTTON (radiobutton4));
  gtk_widget_show (radiobutton4);
  gtk_box_pack_start (GTK_BOX (hbox6), radiobutton4, FALSE, FALSE, 0);

  radiobutton5 = gtk_radio_button_new_with_label (show_group, "Both");
  show_group = gtk_radio_button_group (GTK_RADIO_BUTTON (radiobutton5));
  gtk_widget_show (radiobutton5);
  gtk_box_pack_start (GTK_BOX (hbox6), radiobutton5, FALSE, FALSE, 0);

  hbuttonbox1 = gtk_hbutton_box_new ();
  gtk_widget_show (hbuttonbox1);
  gtk_box_pack_end (GTK_BOX (vbox1), hbuttonbox1, FALSE, FALSE, 0);

  addbutton = gtk_button_new_with_label ("Add");
  gtk_widget_show (addbutton);
  gtk_container_add (GTK_CONTAINER (hbuttonbox1), addbutton);
  GTK_WIDGET_SET_FLAGS (addbutton, GTK_CAN_DEFAULT);

  updbutton = gtk_button_new_with_label ("Update");
  gtk_widget_show (updbutton);
  gtk_container_add (GTK_CONTAINER (hbuttonbox1), updbutton);
  GTK_WIDGET_SET_FLAGS (updbutton, GTK_CAN_DEFAULT);

  delbutton = gtk_button_new_with_label ("Delete");
  gtk_widget_show (delbutton);
  gtk_container_add (GTK_CONTAINER (hbuttonbox1), delbutton);
  GTK_WIDGET_SET_FLAGS (delbutton, GTK_CAN_DEFAULT);

  closebutton = gtk_button_new_with_label ("Close");
  gtk_widget_show (closebutton);
  gtk_container_add (GTK_CONTAINER (hbuttonbox1), closebutton);
  GTK_WIDGET_SET_FLAGS (closebutton, GTK_CAN_DEFAULT);


if(!w_current)return;
if(!w_current->page_current)return;
if(!w_current->page_current->object_head)return;
if(!w_current->page_current->selection_head)return;
if(!w_current->page_current->selection_head->next)return;

attriblist=o_attrib_return_attribs(w_current->page_current->object_head,
w_current->page_current->selection_head->next);
text[0] = malloc(1000);
text[1] = malloc(1000);
text[2] = malloc(1000);
i=0;
	while(attriblist[i] != NULL)
	{
		o_attrib_get_name_value(attriblist[i]->text_string,text[0],text[2]);
		gtk_clist_append(GTK_CLIST(clist),text);
		i++;
	}

	
  gtk_widget_show (window1);
o_attrib_free_returned(attriblist);



}

void
setup_attr_selector (TOPLEVEL *w_current)
{
	GtkWidget *label;
	GtkWidget *separator;
	GtkWidget *box;
	GtkWidget *box2;
	GSList *group;
	GtkWidget *button;
	GtkWidget *buttonapply;
	GtkWidget *buttonclose;
	GtkWidget *scrolled_win;
	GtkWidget *list_item;
	GtkWidget *vbox, *action_area;
	char *string = NULL;
	int i;

	/* freeze the window_current pointer so that it doesn't change */

	if (!w_current->aswindow) {
		w_current->aswindow = x_create_dialog_box(&vbox, &action_area);

		gtk_window_position(GTK_WINDOW(w_current->aswindow),
				    GTK_WIN_POS_NONE);

		gtk_signal_connect(GTK_OBJECT(w_current->aswindow),
				   "destroy",
				   GTK_SIGNAL_FUNC(destroy_window),
				   &w_current->aswindow);

#if 0 /* this was causing the dialog box to not die */
		gtk_signal_connect(GTK_OBJECT(w_current->aswindow),
				   "delete_event",
				   GTK_SIGNAL_FUNC(destroy_window),
				   &w_current->aswindow);
#endif

		gtk_window_set_title(GTK_WINDOW(w_current->aswindow),
				     "Add Attribute");

		buttonapply = gtk_button_new_with_label("Apply");
		GTK_WIDGET_SET_FLAGS(buttonapply, GTK_CAN_DEFAULT);
		gtk_box_pack_start(GTK_BOX(action_area),
				   buttonapply, TRUE, TRUE, 0);
		gtk_signal_connect(GTK_OBJECT(buttonapply), "clicked",
				   GTK_SIGNAL_FUNC(attr_apply), w_current);
		gtk_widget_show(buttonapply);

		buttonclose = gtk_button_new_with_label("Close");
		GTK_WIDGET_SET_FLAGS(buttonclose, GTK_CAN_DEFAULT);
		gtk_box_pack_start(GTK_BOX(action_area),
				   buttonclose, TRUE, TRUE, 0);
		gtk_signal_connect(GTK_OBJECT(buttonclose),
				   "clicked", GTK_SIGNAL_FUNC(attr_cancel),
				   w_current);
		gtk_widget_show(buttonclose);

		scrolled_win = gtk_scrolled_window_new (NULL, NULL);
		gtk_scrolled_window_set_policy(
			GTK_SCROLLED_WINDOW(scrolled_win),
			GTK_POLICY_AUTOMATIC,
			GTK_POLICY_AUTOMATIC);

      		gtk_box_pack_start(GTK_BOX(vbox), 
			           scrolled_win, TRUE, TRUE, 10);
		gtk_widget_show (scrolled_win);
		box2 = gtk_vbox_new (FALSE, 0);
		gtk_scrolled_window_add_with_viewport(
			GTK_SCROLLED_WINDOW (scrolled_win), box2);

		gtk_widget_show(box2);

		w_current->attr_list = gtk_list_new ();

		gtk_container_add(GTK_CONTAINER(box2), w_current->attr_list);
		gtk_widget_show(w_current->attr_list);

		i = 0;
		string = (char *) s_attrib_get(i);
		while (string != NULL) {
			GtkWidget       *label;

			label=gtk_label_new(string);
			gtk_misc_set_alignment(GTK_MISC(label), 0, 0);

		        list_item = gtk_list_item_new();
			gtk_container_add(GTK_CONTAINER(list_item), label);
			gtk_widget_show(label);
          		gtk_container_add(GTK_CONTAINER(w_current->attr_list),
					  list_item);
          		gtk_widget_show(list_item);
			gtk_label_get(GTK_LABEL(label), &string);
            		gtk_object_set_data(GTK_OBJECT(list_item),
					    list_item_data_key,
					    string);
			i++;
			string = (char *) s_attrib_get(i);
        	}

		gtk_signal_connect(GTK_OBJECT(w_current->attr_list),
				   "selection_changed",
				   GTK_SIGNAL_FUNC(change_attr),
				   w_current);

		box = gtk_hbox_new(FALSE, 0);
        	gtk_container_border_width(GTK_CONTAINER(box), 5);
        	gtk_container_add(GTK_CONTAINER(vbox), box);
        	gtk_widget_show(box);

		label = gtk_label_new("Name");
		gtk_misc_set_alignment(GTK_MISC(label), 0, 0);
                gtk_misc_set_padding(GTK_MISC(label), 0, 0);
                gtk_box_pack_start(GTK_BOX (box),
				   label, FALSE, FALSE, 0);
                gtk_widget_show(label);

		w_current->asentry_name = gtk_entry_new_with_max_length (79);
                gtk_editable_select_region(
			GTK_EDITABLE(w_current->asentry_name), 0, -1);
                gtk_box_pack_start(GTK_BOX (box),
				   w_current->asentry_name, TRUE, TRUE, 10);
                gtk_widget_show(w_current->asentry_name);

		box = gtk_hbox_new(FALSE, 0);
        	gtk_container_border_width(GTK_CONTAINER(box), 5);
        	gtk_container_add(GTK_CONTAINER(vbox), box);
        	gtk_widget_show(box);

		label = gtk_label_new("Value");
		gtk_misc_set_alignment(GTK_MISC (label), 0, 0);
                gtk_misc_set_padding(GTK_MISC (label), 0,0);
                gtk_box_pack_start(GTK_BOX (box),
				   label, FALSE, FALSE, 0);
                gtk_widget_show(label);

		w_current->asentry_value = gtk_entry_new_with_max_length (79);
                gtk_editable_select_region(
			GTK_EDITABLE(w_current->asentry_value), 0, -1);
                gtk_box_pack_start(GTK_BOX (box),
				   w_current->asentry_value, TRUE, TRUE, 10);
                gtk_signal_connect(GTK_OBJECT(w_current->asentry_value),
				   "activate",
				   GTK_SIGNAL_FUNC(attr_apply),
				   w_current);

                gtk_widget_show(w_current->asentry_value);

		separator = gtk_hseparator_new ();
	        gtk_box_pack_start(GTK_BOX(vbox), separator, FALSE, TRUE, 0);
  		gtk_widget_show(separator);

		button = gtk_radio_button_new_with_label(NULL, "Show Value");
  		gtk_box_pack_start(GTK_BOX(vbox), button, TRUE, TRUE, 0);
		/* TODO: rewrite callbacks to use third parameter to be
		 * clean */
		gtk_signal_connect(GTK_OBJECT (button), "clicked",
				   GTK_SIGNAL_FUNC (attr_set_show_value),
				   w_current);
  		gtk_widget_show(button);

		group = gtk_radio_button_group(GTK_RADIO_BUTTON (button));
		button = gtk_radio_button_new_with_label(group, "Show Both");
  		gtk_box_pack_start(GTK_BOX(vbox), button, TRUE, TRUE, 0);
		/* TODO: rewrite callbacks to use third parameter to
		 * be clean */
		gtk_signal_connect(GTK_OBJECT (button), "clicked",
				   GTK_SIGNAL_FUNC (attr_set_show_both),
				   w_current);
  		gtk_widget_show(button);

		group = gtk_radio_button_group(GTK_RADIO_BUTTON(button));
		button = gtk_radio_button_new_with_label(group, "Show Name");
  		gtk_box_pack_start( GTK_BOX(vbox), button, TRUE, TRUE, 0);
		gtk_signal_connect(GTK_OBJECT(button),
				   "clicked",
				   GTK_SIGNAL_FUNC(attr_set_show_name),
				   w_current);
  		gtk_widget_show(button);

		button = gtk_radio_button_new_with_label(NULL, "Visible");
  		gtk_box_pack_start(GTK_BOX(vbox), button, TRUE, TRUE, 0);
		/* TODO: rewrite callbacks to use third parameter to
		 * be clean */
		gtk_signal_connect(GTK_OBJECT (button), "clicked",
				   GTK_SIGNAL_FUNC (attr_set_visible),
				   w_current);
  		gtk_widget_show(button);

		group = gtk_radio_button_group(GTK_RADIO_BUTTON (button));
		button = gtk_radio_button_new_with_label(group, "Invisible");
  		gtk_box_pack_start(GTK_BOX(vbox), button, TRUE, TRUE, 0);
		/* TODO: rewrite callbacks to use third parameter to
		 * be clean */
		gtk_signal_connect(GTK_OBJECT (button), "clicked",
				   GTK_SIGNAL_FUNC (attr_set_invisible),
				   w_current);
  		gtk_widget_show (button);

		/* set some defaults */
		/* TODO: this should be whatever the last value was
		 * set to??? */
		o_attrib_set_show(w_current, SHOW_VALUE);
		o_attrib_set_visible(w_current, VISIBLE);

	}

	if (!GTK_WIDGET_VISIBLE (w_current->aswindow)) {
		gtk_widget_show (w_current->aswindow);
		gdk_window_raise(w_current->aswindow->window);
 	} else {
		/* window should already be mapped, otherwise this
		 * will core */
		gdk_window_raise(w_current->aswindow->window);
	}
}

