/* STILL NEED to clean up line lengths in aa and tr */

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

#include "../include/i_vars.h"
#include "../include/x_states.h"
#include "../include/globals.h"
#include "../include/prototype.h"

void
destroy_window(GtkWidget *widget, GtkWidget **window)
{
        *window = NULL;
}


/***************** Start of Multiple Attrib Edit dialog box ***********/

void
multi_attrib_edit_add (GtkWidget *w, TOPLEVEL *w_current)
{
	GtkWidget *window;
	GtkWidget *label;
	GtkWidget *value;
	GtkWidget *clist;

	char *text[3];
	
	text[1]=malloc(4);

	
	window = gtk_object_get_data(GTK_OBJECT(w),"window");
	label = gtk_object_get_data(GTK_OBJECT(window),"lab_entry");
	value = gtk_object_get_data(GTK_OBJECT(window),"val_entry");

	text[0]=gtk_entry_get_text(GTK_ENTRY(label));
	text[2]=gtk_entry_get_text(GTK_ENTRY(value));
	
	clist = gtk_object_get_data(GTK_OBJECT(window),"clist");
	gtk_clist_append(GTK_CLIST(clist),text);
}

void
multi_attrib_edit_change (GtkWidget *w, GtkCList *clist)
{
	char *text[]= { "","V V","" };
	gtk_clist_append(clist,text);
}

void
multi_attrib_edit_clear (GtkWidget *w, GtkCList *clist)
{
	char *text[]= { "","V V","" };
	gtk_clist_append(clist,text);
}

void
multi_attrib_edit_delete (GtkWidget *w, GtkCList *clist)
{
	char *text[]= { "","V V","" };
	gtk_clist_append(clist,text);
}

void
multi_attrib_edit_close (GtkWidget *w, TOPLEVEL *w_current)
{
        i_update_status(w_current, "Select Mode");
        w_current->event_state = SELECT;
        gtk_grab_remove(w_current->tewindow);
        gtk_widget_destroy(w_current->tewindow);
        w_current->tewindow = NULL;
}

void
multi_attrib_edit (TOPLEVEL *w_current, OBJECT *list)
{

	char *text[3];
	char name[1000];
	char *string;
	int i;
	OBJECT **attriblist=NULL;

	GtkWidget *window1;
	GtkWidget *table;
	GtkWidget *vbox;
	GtkWidget *hbox2;
	GtkWidget *hbox4;
	GtkWidget *hbox5;
	GtkWidget *hbox6;
	GtkWidget *scrolledwindow1;
	GtkWidget *clist;
	GtkWidget *attribhead;
	GtkWidget *vishead;
	GtkWidget *valhead;

	GtkWidget *label10;
	GtkWidget *label11;

	GtkWidget *combo2;
	GList *combo2_items = NULL;
	GtkWidget *lab_entry;
	GtkWidget *visbutton;
	GtkWidget *val_entry;
	GSList *show_group = NULL;
	GtkWidget *namebutton;
	GtkWidget *valbutton;
	GtkWidget *bothbutton;

	GtkWidget *hbuttonbox1;
	GtkWidget *addbutton;
	GtkWidget *changebutton;
	GtkWidget *clearbutton;
	GtkWidget *delbutton;
	GtkWidget *closebutton;

	gchar *titles[]= {"Attribute","Visibility","Value"};


	/* Do basic checks first */	
	if(!w_current)return;
	if(!w_current->page_current)return;
	if(!w_current->page_current->object_head)return;
	if(!w_current->page_current->selection_head)return;
	if(!w_current->page_current->selection_head->next)return;
	
	attriblist=o_attrib_return_attribs(w_current->page_current->object_head,
		w_current->page_current->selection_head->next);
	text[0] = malloc(1000);
	text[1] = malloc(4);
	text[2] = malloc(1000);
	text[1][1]=' ';
	text[1][3]='\0';

	/* Make sure attriblist isn't NULL */
	if (attriblist == NULL) {
		free(text[0]);
		free(text[1]);
		free(text[2]);
		return;
	}
	
	window1 = gtk_window_new (GTK_WINDOW_TOPLEVEL);
	gtk_object_set_data (GTK_OBJECT (window1), "window1", window1);
	gtk_window_set_title (GTK_WINDOW (window1), "window1");
  
	gtk_window_position(GTK_WINDOW (window1), GTK_WIN_POS_MOUSE);

	vbox = gtk_vbox_new (FALSE, 0);
	gtk_widget_show (vbox);
	gtk_container_add (GTK_CONTAINER (window1), vbox);
	
	scrolledwindow1 = gtk_scrolled_window_new (NULL, NULL);
	gtk_widget_show (scrolledwindow1);
	gtk_box_pack_start (GTK_BOX (vbox), scrolledwindow1, TRUE, TRUE, 0);
	gtk_container_set_border_width (GTK_CONTAINER (scrolledwindow1), 3);
	gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (scrolledwindow1),
		GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
	
	clist = gtk_clist_new (3);
	gtk_object_set_data(GTK_OBJECT(window1),"clist",clist);
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
	
	table = gtk_table_new (2,2, FALSE);
	gtk_widget_show (table);
	gtk_box_pack_start (GTK_BOX (vbox), table, FALSE, TRUE, 0);

	hbox2 = gtk_hbox_new (FALSE, 0);
	gtk_widget_show (hbox2);

	gtk_table_attach (GTK_TABLE (table), hbox2,0,1,0,1,
	                  (GtkAttachOptions) (GTK_EXPAND | GTK_FILL),
                    (GtkAttachOptions) (GTK_EXPAND | GTK_FILL), 0, 0);

	gtk_container_set_border_width (GTK_CONTAINER (hbox2), 3);


	label10 = gtk_label_new ("Attribute");
	gtk_widget_show (label10);
	gtk_box_pack_start (GTK_BOX (hbox2), label10, TRUE, TRUE, 0);
	
	combo2 = gtk_combo_new ();
	gtk_widget_show (combo2);
	gtk_box_pack_start (GTK_BOX (hbox2), combo2, TRUE, TRUE, 0);
	
	lab_entry = GTK_COMBO (combo2)->entry;
	gtk_widget_show (lab_entry);
	gtk_object_set_data(GTK_OBJECT(window1),"lab_entry",lab_entry);
	
	hbox5 = gtk_hbox_new (FALSE, 0);
	gtk_widget_show (hbox5);
	
	visbutton = gtk_check_button_new_with_label ("Visible");
	gtk_object_set_data (GTK_OBJECT (window1), "visbutton", visbutton);
	gtk_widget_show (visbutton);
	gtk_box_pack_start (GTK_BOX (hbox5), visbutton, FALSE, FALSE, 0);
	
	gtk_table_attach (GTK_TABLE (table), hbox5,1,2,0,1,
	                  (GtkAttachOptions) (GTK_EXPAND | GTK_FILL),
                    (GtkAttachOptions) ( GTK_FILL), 0, 0);
	
	hbox4 = gtk_hbox_new (FALSE, 0);
	gtk_widget_show (hbox4);
	gtk_container_set_border_width (GTK_CONTAINER (hbox4), 3);
	gtk_table_attach (GTK_TABLE (table), hbox4,0,1,1,2,
	                  (GtkAttachOptions) ( GTK_FILL),
                    (GtkAttachOptions) (GTK_EXPAND | GTK_FILL), 0, 0);
	
	label11 = gtk_label_new ("Value");
	gtk_widget_show (label11);
	gtk_box_pack_start (GTK_BOX (hbox4), label11, TRUE, FALSE, 0);
	
	val_entry = gtk_entry_new ();
	gtk_widget_show (val_entry);
	gtk_box_pack_start (GTK_BOX (hbox4), val_entry, TRUE, TRUE, 0);
	gtk_object_set_data(GTK_OBJECT(window1),"val_entry",val_entry);
	
	hbox6 = gtk_hbox_new (FALSE, 0);
	gtk_widget_show (hbox6);
	
	gtk_table_attach (GTK_TABLE (table), hbox6,1,2,1,2,
	                  (GtkAttachOptions) (GTK_EXPAND | GTK_FILL),
                    (GtkAttachOptions) (GTK_EXPAND | GTK_FILL), 0, 0);

	namebutton = gtk_radio_button_new_with_label (show_group, "Name");
	show_group = gtk_radio_button_group (GTK_RADIO_BUTTON (namebutton));
	gtk_widget_show (namebutton);
	gtk_box_pack_start (GTK_BOX (hbox6), namebutton, FALSE, FALSE, 0);
	
	valbutton = gtk_radio_button_new_with_label (show_group, "Value");
	show_group = gtk_radio_button_group (GTK_RADIO_BUTTON (valbutton));
	gtk_widget_show (valbutton);
	gtk_box_pack_start (GTK_BOX (hbox6), valbutton, FALSE, FALSE, 0);
	
	bothbutton = gtk_radio_button_new_with_label (show_group, "Both");
	show_group = gtk_radio_button_group (GTK_RADIO_BUTTON (bothbutton));
	gtk_widget_show (bothbutton);
	gtk_box_pack_start (GTK_BOX (hbox6), bothbutton, FALSE, FALSE, 0);
	
	hbuttonbox1 = gtk_hbutton_box_new ();
	gtk_widget_show (hbuttonbox1);
	gtk_box_pack_end (GTK_BOX (vbox), hbuttonbox1, FALSE, FALSE, 0);
	
	addbutton = gtk_button_new_with_label ("Add");
	gtk_widget_show (addbutton);
	gtk_container_add (GTK_CONTAINER (hbuttonbox1), addbutton);
	GTK_WIDGET_SET_FLAGS (addbutton, GTK_CAN_DEFAULT);

	gtk_object_set_data(GTK_OBJECT(addbutton),"window",window1);

	changebutton = gtk_button_new_with_label ("Change");
	gtk_widget_show (changebutton);
	gtk_container_add (GTK_CONTAINER (hbuttonbox1), changebutton);
	GTK_WIDGET_SET_FLAGS (changebutton, GTK_CAN_DEFAULT);

	clearbutton = gtk_button_new_with_label ("Clear");
	gtk_widget_show (clearbutton);
	gtk_container_add (GTK_CONTAINER (hbuttonbox1), clearbutton);
	GTK_WIDGET_SET_FLAGS (clearbutton, GTK_CAN_DEFAULT);
	
	delbutton = gtk_button_new_with_label ("Delete");
	gtk_widget_show (delbutton);
	gtk_container_add (GTK_CONTAINER (hbuttonbox1), delbutton);
	GTK_WIDGET_SET_FLAGS (delbutton, GTK_CAN_DEFAULT);
	
	closebutton = gtk_button_new_with_label ("Close");
	gtk_widget_show (closebutton);
	gtk_container_add (GTK_CONTAINER (hbuttonbox1), closebutton);
	GTK_WIDGET_SET_FLAGS (closebutton, GTK_CAN_DEFAULT);
	
	i=0;
       	while(attriblist[i] != NULL)
        {
                o_attrib_get_name_value(attriblist[i]->text_string,
			text[0],text[2]);
		if(attriblist[i]->visibility == VISIBLE)
			text[1][0]='V';
		else
			text[1][0]='I';

		if(attriblist[i]->show_name_value == SHOW_NAME)
			text[1][2]='N';
		else if(attriblist[i]->show_name_value == SHOW_VALUE)
			text[1][2]='V';
		else
			text[1][2]='B';
                gtk_clist_append(GTK_CLIST(clist),text);
                i++;
        }

	i = 0;
        string = (char *) s_attrib_get(i);
        while (string != NULL)
        {
                combo2_items = g_list_append (combo2_items, string);
                i++;
                string = (char *) s_attrib_get(i);
        }
        combo2_items = g_list_prepend (combo2_items, NULL);
        gtk_combo_set_popdown_strings (GTK_COMBO (combo2), combo2_items);
        g_list_free (combo2_items);

	gtk_widget_show (window1);
	o_attrib_free_returned(attriblist);
	w_current->tewindow=window1;

        gtk_window_position(GTK_WINDOW (window1), GTK_WIN_POS_MOUSE);

	gtk_signal_connect(GTK_OBJECT (window1), "destroy",
				GTK_SIGNAL_FUNC(destroy_window),
				&w_current->tewindow);

        gtk_signal_connect(GTK_OBJECT(addbutton),"clicked",
                        GTK_SIGNAL_FUNC(multi_attrib_edit_add),w_current);
        gtk_signal_connect(GTK_OBJECT(changebutton),"clicked",
                        GTK_SIGNAL_FUNC(multi_attrib_edit_change),w_current);
        gtk_signal_connect(GTK_OBJECT(clearbutton),"clicked",
                        GTK_SIGNAL_FUNC(multi_attrib_edit_clear),w_current);
        gtk_signal_connect(GTK_OBJECT(delbutton),"clicked",
                        GTK_SIGNAL_FUNC(multi_attrib_edit_delete),w_current);
        gtk_signal_connect(GTK_OBJECT(closebutton),"clicked",
                        GTK_SIGNAL_FUNC(multi_attrib_edit_close),w_current);


	free(text[0]);
	free(text[1]);
	free(text[2]);
}


/***************** End of Multiple Attrib Edit dialog box *********************/

/***************** Start of Text Input dialog box *********************/
void
text_input_dialog_apply(GtkWidget *w, TOPLEVEL *w_current)
{
	int len;
	char *string = NULL;

	string = gtk_entry_get_text(GTK_ENTRY(w_current->tientry));

	if (string[0] != '\0' ) {
		len = strlen(string);
#if DEBUG
		printf("text was: _%s_ %d\n", string, len);
#endif
		/* TODO: length is hack */
		if (len < 80) {
			switch(w_current->text_caps) {
			case(LOWER):
				string_tolower(string, string);
				break;

			case(UPPER):
				string_toupper(string, string);
				break;

			case(BOTH):
			default:
				/* do nothing */
				break;
			}

			o_attrib_set_string(w_current, string);
			gtk_entry_set_text(GTK_ENTRY(w_current->tientry),
					   w_current->current_attribute);
			gtk_entry_select_region(GTK_ENTRY(w_current->tientry),
						0, len);
			gtk_widget_grab_focus(w_current->tientry);

		} else {
			/* TODO: you should have limits */
			fprintf(stderr, "String too long... hack!\n");
		}

		w_current->event_state = DRAWTEXT;
		w_current->inside_action = 1;
	}

#if 0
	gtk_grab_remove(w_current->tiwindow);
   	gtk_widget_destroy(w_current->tiwindow);
	w_current->tiwindow = NULL;
#endif
}

void
text_input_dialog_close(GtkWidget *w, TOPLEVEL *w_current)
{
	i_update_status(w_current, "Select Mode");
	w_current->event_state = SELECT;
#if 0
	gtk_grab_remove(w_current->tiwindow);
#endif
	gtk_widget_destroy(w_current->tiwindow);
	w_current->tiwindow=NULL;
}

void
text_input_dialog (TOPLEVEL *w_current)
{
	GtkWidget *label = NULL;
	GtkWidget *buttonok     = NULL;
	GtkWidget *buttoncancel = NULL;
	GtkWidget *vbox, *action_area;

	if (!w_current->tiwindow) {
		w_current->tiwindow = x_create_dialog_box(&vbox, &action_area);

#if 0
		gtk_window_position(GTK_WINDOW (w_current->tiwindow),
				    GTK_WIN_POS_MOUSE);
#endif

		gtk_widget_set_usize(w_current->tiwindow, 400,130);

		gtk_window_position(GTK_WINDOW (w_current->tiwindow),
				    GTK_WIN_POS_NONE);

		gtk_signal_connect(GTK_OBJECT (w_current->tiwindow),
				   "destroy", GTK_SIGNAL_FUNC(destroy_window),
				   &w_current->tiwindow);

#if 0 /* removed because it was causing the dialog box to not close */
      		gtk_signal_connect(GTK_OBJECT (w_current->tiwindow),
				   "delete_event",
				   GTK_SIGNAL_FUNC(destroy_window),
				   &w_current->tiwindow);
#endif

		gtk_window_set_title(GTK_WINDOW (w_current->tiwindow),
				     "Enter Text");
                gtk_container_border_width(
			GTK_CONTAINER (w_current->tiwindow), 0);

		label = gtk_label_new ("Enter Text");
		gtk_box_pack_start(
			GTK_BOX(vbox),
			label, TRUE, TRUE, 0);
      		gtk_widget_show (label);

		w_current->tientry = gtk_entry_new_with_max_length (79);
		gtk_signal_connect(GTK_OBJECT(w_current->tientry), "activate",
                       		   GTK_SIGNAL_FUNC(text_input_dialog_apply),
                                   w_current);
    		gtk_box_pack_start(
			GTK_BOX(vbox),
			w_current->tientry,
			TRUE, TRUE, 10);
      		gtk_editable_select_region(GTK_EDITABLE(w_current->tientry),
					   0, -1);
    		gtk_widget_show(w_current->tientry);
		gtk_widget_grab_focus(w_current->tientry);

		buttonok = gtk_button_new_with_label("Apply");
		GTK_WIDGET_SET_FLAGS (buttonok, GTK_CAN_DEFAULT);
		gtk_box_pack_start(
			GTK_BOX(action_area),
			buttonok, TRUE, TRUE, 0);
      		gtk_signal_connect(GTK_OBJECT (buttonok), "clicked",
				   GTK_SIGNAL_FUNC(text_input_dialog_apply),
				   w_current);
      		gtk_widget_show (buttonok);
		gtk_widget_grab_default (buttonok);

		buttoncancel = gtk_button_new_with_label ("Close");
		GTK_WIDGET_SET_FLAGS (buttoncancel, GTK_CAN_DEFAULT);
		gtk_box_pack_start(
			GTK_BOX(action_area),
			buttoncancel, TRUE, TRUE, 0);
      		gtk_signal_connect (GTK_OBJECT (buttoncancel), "clicked",
			  	    GTK_SIGNAL_FUNC(text_input_dialog_close),
				    w_current);
      		gtk_widget_show (buttoncancel);
	}

  	if (!GTK_WIDGET_VISIBLE (w_current->tiwindow)) {
		gtk_widget_show (w_current->tiwindow);
#if 0
		gtk_grab_add (w_current->tiwindow);
#endif
	} else {
	 	gdk_window_raise(w_current->tiwindow->window);
	}
}
/***************** End of Text Input dialog box *********************/

/***************** Start of Attrib Edit dialog box ******************/

void
attrib_edit_dialog_ok(GtkWidget *w, TOPLEVEL *w_current)
{
	char *value, *label;
	char *newtext;
	GtkEntry *val_entry, *lab_entry;
	GtkWidget *visbutton,*showvalbutton,*showlabbutton,*showboth;
	OBJECT *attribptr,*real_attribptr;
	int vis,show;

        w_current->event_state = SELECT;
        i_update_status(w_current, "Select Mode");

        gtk_grab_remove(w_current->tewindow);

	val_entry = gtk_object_get_data(GTK_OBJECT(w_current->tewindow),"val_entry");
	lab_entry = gtk_object_get_data(GTK_OBJECT(w_current->tewindow),"lab_entry");
	visbutton = gtk_object_get_data(GTK_OBJECT(w_current->tewindow),"visbutton");
	showvalbutton = gtk_object_get_data(GTK_OBJECT(w_current->tewindow),"showvalbutton");
	showlabbutton = gtk_object_get_data(GTK_OBJECT(w_current->tewindow),"showlabbutton");
	showboth = gtk_object_get_data(GTK_OBJECT(w_current->tewindow),"showboth");

	value=gtk_entry_get_text(val_entry);
	label=gtk_entry_get_text(lab_entry);

	newtext = u_basic_strdup_multiple(label,"=",value,NULL);
	if(gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(visbutton)))
        	vis = VISIBLE;
	else
        	vis = INVISIBLE;

	if(gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(showvalbutton)))
        	show = SHOW_VALUE;
	else if(gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(showlabbutton)))
        	show = SHOW_NAME;
	else 
        	show = SHOW_NAME_VALUE;

	attribptr = gtk_object_get_data(GTK_OBJECT(w_current->tewindow),"attrib");
	if(!attribptr)
		o_attrib_add_attrib(w_current, newtext, vis,
                                    show,
                                    w_current->page_current->
                                    selection_head->next);
	else
		o_text_change(w_current,attribptr,newtext,vis,show);
        gtk_widget_destroy(w_current->tewindow);
	w_current->tewindow = NULL;
	free(newtext);
}

void
attrib_edit_dialog_cancel(GtkWidget *w, TOPLEVEL *w_current)
{
	i_update_status(w_current, "Select Mode");
        w_current->event_state = SELECT;
        gtk_grab_remove(w_current->tewindow);
        gtk_widget_destroy(w_current->tewindow);
        w_current->tewindow = NULL;
}

void
attrib_edit_dialog_delete(GtkWidget *w, TOPLEVEL *w_current)
{
	OBJECT *object;

	/* for now unselect everything, but in the future you really ought 
	 * to just unselect a single object */
	o_unselect_all(w_current);

	object = gtk_object_get_data(GTK_OBJECT(w_current->tewindow),"attrib");
	o_delete_text(w_current, object);

	i_update_status(w_current, "Select Mode");
        w_current->event_state = SELECT;
        gtk_grab_remove(w_current->tewindow);
        gtk_widget_destroy(w_current->tewindow);
        w_current->tewindow = NULL;
}

void
attrib_edit_dialog (TOPLEVEL *w_current, OBJECT *list)
{
	int i;
	char *string=NULL;
	char name[1000], val[1000];
	OBJECT *attrib;
	GtkWidget *tewindow;
	GtkWidget *vbox2;
	GtkWidget *table1;
	GtkWidget *hbox7;
	GtkWidget *label12;
	GtkWidget *combo3;
	GList *combo3_items = NULL;
	GtkWidget *lab_entry;
	GtkWidget *hbox8;
	GtkWidget *label13;
	GtkWidget *val_entry;
	GtkWidget *visbutton;
	GtkWidget *hbox10;
	GSList *show_group = NULL;
	GtkWidget *radiobutton8;
	GtkWidget *radiobutton9;
	GtkWidget *radiobutton10;
	GtkWidget *hbuttonbox2;
	GtkWidget *buttondelete;
	GtkWidget *buttonok;
	GtkWidget *buttoncancel;

	if(w_current->tewindow)return;

	tewindow = gtk_window_new (GTK_WINDOW_TOPLEVEL);
	gtk_window_set_title (GTK_WINDOW (tewindow), "Single Attribute Editor");

	vbox2 = gtk_vbox_new (FALSE, 0);
	gtk_widget_show (vbox2);
	gtk_container_add (GTK_CONTAINER (tewindow), vbox2);

	table1 = gtk_table_new (2, 2, FALSE);
	gtk_widget_show (table1);
	gtk_box_pack_start (GTK_BOX (vbox2), table1, TRUE, TRUE, 0);

	hbox7 = gtk_hbox_new (FALSE, 3);
	gtk_widget_show (hbox7);
	gtk_table_attach (GTK_TABLE (table1), hbox7, 0, 1, 0, 1,
                    (GtkAttachOptions) (GTK_EXPAND | GTK_FILL),
                    (GtkAttachOptions) (GTK_EXPAND | GTK_FILL), 0, 0);
	gtk_container_set_border_width (GTK_CONTAINER (hbox7), 3);

	label12 = gtk_label_new ("Attribute");
	gtk_widget_show (label12);
	gtk_box_pack_start (GTK_BOX (hbox7), label12, TRUE, TRUE, 0);

	combo3 = gtk_combo_new ();
	gtk_widget_show (combo3);
	gtk_box_pack_start (GTK_BOX (hbox7), combo3, TRUE, TRUE, 0);

	lab_entry = GTK_COMBO (combo3)->entry;
	gtk_object_set_data (GTK_OBJECT (tewindow), "lab_entry", lab_entry);
	gtk_widget_show (lab_entry);

	hbox8 = gtk_hbox_new (FALSE, 0);
	gtk_widget_show (hbox8);
	gtk_table_attach (GTK_TABLE (table1), hbox8, 0, 1, 1, 2,
                    (GtkAttachOptions) (GTK_FILL),
                    (GtkAttachOptions) (GTK_EXPAND | GTK_FILL), 0, 0);
	gtk_container_set_border_width (GTK_CONTAINER (hbox8), 3);

	label13 = gtk_label_new ("Value");
	gtk_widget_show (label13);
	gtk_box_pack_start (GTK_BOX (hbox8), label13, TRUE, TRUE, 0);

	val_entry = gtk_entry_new ();
	gtk_object_set_data (GTK_OBJECT (tewindow), "val_entry", val_entry);

	gtk_widget_show (val_entry);

	gtk_box_pack_start (GTK_BOX (hbox8), val_entry, TRUE, TRUE, 0);

	visbutton = gtk_check_button_new_with_label ("Visible");
	gtk_object_set_data (GTK_OBJECT (tewindow), "visbutton", visbutton);

	gtk_table_attach (GTK_TABLE (table1), visbutton, 1, 2, 0, 1,
                    (GtkAttachOptions) (GTK_EXPAND | GTK_FILL),
                    (GtkAttachOptions) (GTK_FILL), 0, 0);

	gtk_widget_show (visbutton);

	hbox10 = gtk_hbox_new (FALSE, 0);
	gtk_widget_show (hbox10);
	gtk_table_attach (GTK_TABLE (table1), hbox10, 1, 2, 1, 2,
                    (GtkAttachOptions) (GTK_FILL),
                    (GtkAttachOptions) (GTK_FILL), 0, 0);
	gtk_container_set_border_width (GTK_CONTAINER (hbox10), 3);

	radiobutton8 = gtk_radio_button_new_with_label (show_group, "Name");
	gtk_object_set_data(GTK_OBJECT(tewindow),"showlabbutton",radiobutton8);
	show_group = gtk_radio_button_group (GTK_RADIO_BUTTON (radiobutton8));
	gtk_widget_show (radiobutton8);
	gtk_box_pack_start (GTK_BOX (hbox10), radiobutton8, FALSE, FALSE, 0);

	radiobutton9 = gtk_radio_button_new_with_label (show_group, "Value");
	show_group = gtk_radio_button_group (GTK_RADIO_BUTTON (radiobutton9));
	gtk_object_set_data(GTK_OBJECT(tewindow),"showvalbutton",radiobutton9);
	gtk_widget_show (radiobutton9);
	gtk_box_pack_start (GTK_BOX (hbox10), radiobutton9, FALSE, FALSE, 0);

	radiobutton10 = gtk_radio_button_new_with_label (show_group, "Both");
	show_group = gtk_radio_button_group (GTK_RADIO_BUTTON (radiobutton10));
	gtk_object_set_data(GTK_OBJECT(tewindow),"showboth",radiobutton10);
	gtk_widget_show (radiobutton10);
	gtk_box_pack_start (GTK_BOX (hbox10), radiobutton10, FALSE, FALSE, 0);

	hbuttonbox2 = gtk_hbutton_box_new ();
	gtk_widget_show (hbuttonbox2);
	gtk_box_pack_start (GTK_BOX (vbox2), hbuttonbox2, FALSE, TRUE, 0);

	buttonok = gtk_button_new_with_label ("OK");
	gtk_widget_show (buttonok);
	gtk_container_add (GTK_CONTAINER (hbuttonbox2), buttonok);
	GTK_WIDGET_SET_FLAGS (buttonok, GTK_CAN_DEFAULT);

	if(list)
	{
		buttondelete = gtk_button_new_with_label ("Delete");
		gtk_widget_show (buttondelete);
		gtk_container_add (GTK_CONTAINER (hbuttonbox2), buttondelete);
		GTK_WIDGET_SET_FLAGS (buttondelete, GTK_CAN_DEFAULT);
	}

	buttoncancel = gtk_button_new_with_label ("Cancel");
	gtk_widget_show (buttoncancel);
	gtk_container_add (GTK_CONTAINER (hbuttonbox2), buttoncancel);
	GTK_WIDGET_SET_FLAGS (buttoncancel, GTK_CAN_DEFAULT);


	if(list)
	{
		o_attrib_get_name_value(list->text_string,name,val);
		attrib=o_list_search(w_current->page_current->object_head,list);
		if(attrib->visibility == VISIBLE)
			gtk_toggle_button_set_active 
				(GTK_TOGGLE_BUTTON (visbutton), TRUE);

		if(attrib->show_name_value == SHOW_NAME)
			gtk_toggle_button_set_active 
				(GTK_TOGGLE_BUTTON(radiobutton8),TRUE);
		else if(attrib->show_name_value == SHOW_VALUE)
			gtk_toggle_button_set_active 
				(GTK_TOGGLE_BUTTON(radiobutton9),TRUE);
		else gtk_toggle_button_set_active 
			(GTK_TOGGLE_BUTTON(radiobutton10),TRUE);
	}
	else
	{
		attrib=NULL;
		name[0]=0;
		val[0]=0;
		gtk_toggle_button_set_active 
			(GTK_TOGGLE_BUTTON (visbutton), TRUE);
		gtk_toggle_button_set_active 
			(GTK_TOGGLE_BUTTON(radiobutton9),TRUE);
	}
	i = 0;
	string = (char *) s_attrib_get(i);
	while (string != NULL)
	{
		combo3_items = g_list_append (combo3_items, string);
		i++;
		string = (char *) s_attrib_get(i);
	}
	combo3_items = g_list_prepend (combo3_items, name);
	gtk_combo_set_popdown_strings (GTK_COMBO (combo3), combo3_items);
	g_list_free (combo3_items);

	gtk_object_set_data(GTK_OBJECT(tewindow),"attrib",attrib);

	gtk_entry_set_text (GTK_ENTRY (val_entry), val);
	gtk_entry_set_text (GTK_ENTRY (lab_entry), name);

	w_current->tewindow=tewindow;

	gtk_window_position(GTK_WINDOW (tewindow), GTK_WIN_POS_MOUSE);

	gtk_signal_connect(GTK_OBJECT (tewindow), "destroy",
                                   GTK_SIGNAL_FUNC(destroy_window),
                                   &w_current->tewindow);

	gtk_signal_connect(GTK_OBJECT(buttonok),"clicked",
			GTK_SIGNAL_FUNC(attrib_edit_dialog_ok),w_current);
	gtk_signal_connect(GTK_OBJECT(buttoncancel),"clicked",
			GTK_SIGNAL_FUNC(attrib_edit_dialog_cancel),w_current);

	gtk_signal_connect(GTK_OBJECT(val_entry),"activate",
			GTK_SIGNAL_FUNC(attrib_edit_dialog_ok),w_current);
	if(list)
		gtk_signal_connect(GTK_OBJECT(buttondelete),"clicked",
			GTK_SIGNAL_FUNC(attrib_edit_dialog_delete),w_current);

	gtk_widget_show(tewindow);

	gtk_widget_grab_focus(tewindow);
}


/***************** End of Attrib Edit dialog box *********************/

/***************** Start of Text Edit dialog box *********************/
void
text_edit_dialog_ok(GtkWidget *w, TOPLEVEL *w_current)
{
	int len;
	int text_size;
	char *text_string = NULL;
	char *text_size_string = NULL;

	text_string = gtk_entry_get_text(GTK_ENTRY(w_current->teentry));
	text_size_string = gtk_entry_get_text(GTK_ENTRY(w_current->tsentry));

	if ((text_string[0] != '\0') && (text_string[0] != '\0')) {
		len = strlen(text_string);

#if DEBUG
		printf("text was: _%s_ %d\n", string, len);
#endif

		text_size = atoi(text_size_string);
#if DEBUG
		printf("text size was: _%s_ %d\n",
		       text_size_string, text_size);
#endif

		if (text_size == 0) {
			text_size = default_text_size;
		}

		if (len < 80) {
			o_text_edit_end(w_current, text_string,
					 len, text_size);
		} else {
			/* TODO: you should NOT have limits */
			fprintf(stderr, "String too long... hack!\n");
		}
	}

	w_current->event_state = SELECT;
	i_update_status(w_current, "Select Mode");

	gtk_grab_remove(w_current->tewindow);
	gtk_widget_destroy(w_current->tewindow);
	w_current->tewindow = NULL;
}

void
text_edit_dialog_cancel(GtkWidget *w, TOPLEVEL *w_current)
{
	i_update_status(w_current, "Select Mode");
	w_current->event_state = SELECT;
	gtk_grab_remove(w_current->tewindow);
	gtk_widget_destroy(w_current->tewindow);
	w_current->tewindow = NULL;
}

void
text_edit_dialog (TOPLEVEL *w_current, char *string, int text_size)
{
	GtkWidget *label = NULL;
	GtkWidget *buttonok     = NULL;
	GtkWidget *buttoncancel = NULL;
	GtkWidget *vbox, *action_area;
	char text_size_string[10];
	int len;

	if (!w_current->tewindow) {
		w_current->tewindow = x_create_dialog_box(&vbox, &action_area);

		gtk_window_position(GTK_WINDOW (w_current->tewindow),
				    GTK_WIN_POS_MOUSE);

		gtk_signal_connect(GTK_OBJECT (w_current->tewindow),
				   "destroy",
				   GTK_SIGNAL_FUNC(destroy_window),
				   &w_current->tewindow);

#if 0 /* removed because it was causing the dialog box to not close */
      		gtk_signal_connect(GTK_OBJECT (w_current->tewindow),
				   "delete_event",
				   GTK_SIGNAL_FUNC(gtk_widget_destroyed),
				   &w_current->tewindow);
#endif

		gtk_window_set_title(GTK_WINDOW (w_current->tewindow),
				     "Edit Text");
                gtk_container_border_width(
			GTK_CONTAINER(w_current->tewindow), 0);

		label = gtk_label_new ("Edit Text");
		gtk_box_pack_start(
			GTK_BOX(vbox),
			label, TRUE, TRUE, 0);
      		gtk_widget_show (label);

		w_current->teentry = gtk_entry_new();
      		gtk_editable_select_region(
			GTK_EDITABLE(w_current->teentry), 0, -1);
		gtk_box_pack_start(
			GTK_BOX(vbox),
			w_current->teentry, TRUE, TRUE, 10);

		gtk_signal_connect(GTK_OBJECT(w_current->teentry), "activate",
                       	           GTK_SIGNAL_FUNC(text_edit_dialog_ok),
                       	           w_current);
      		gtk_widget_show (w_current->teentry);
		gtk_widget_grab_focus(w_current->teentry);

		label = gtk_label_new ("Edit Text Size");
		gtk_box_pack_start(
			GTK_BOX(vbox),
			label, TRUE, TRUE, 0);
      		gtk_widget_show (label);

		w_current->tsentry = gtk_entry_new_with_max_length (10);
      		gtk_editable_select_region(
			GTK_EDITABLE (w_current->tsentry), 0, -1);
		gtk_box_pack_start(
			GTK_BOX(vbox),
			w_current->tsentry, FALSE, FALSE, 5);
		gtk_signal_connect(GTK_OBJECT(w_current->tsentry), "activate",
				   GTK_SIGNAL_FUNC(text_edit_dialog_ok),
				   w_current);
      		gtk_widget_show (w_current->tsentry);

		buttonok = gtk_button_new_with_label ("OK");
		GTK_WIDGET_SET_FLAGS (buttonok, GTK_CAN_DEFAULT);
		gtk_box_pack_start(
			GTK_BOX(action_area),
			buttonok, TRUE, TRUE, 0);
      		gtk_signal_connect(GTK_OBJECT (buttonok), "clicked",
				   GTK_SIGNAL_FUNC(text_edit_dialog_ok),
				   w_current);
      		gtk_widget_show(buttonok);
		gtk_widget_grab_default(buttonok);

		buttoncancel = gtk_button_new_with_label ("Cancel");
		GTK_WIDGET_SET_FLAGS(buttoncancel, GTK_CAN_DEFAULT);
		gtk_box_pack_start(
			GTK_BOX(action_area),
			buttoncancel, TRUE, TRUE, 0);
      		gtk_signal_connect(GTK_OBJECT (buttoncancel), "clicked",
			           GTK_SIGNAL_FUNC(text_edit_dialog_cancel),
				   w_current);
      		gtk_widget_show(buttoncancel);

	}

  	if (!GTK_WIDGET_VISIBLE (w_current->tewindow)) {
		gtk_widget_show (w_current->tewindow);
		if (string != NULL) {
			len = strlen(string);
			gtk_entry_set_text(GTK_ENTRY(w_current->teentry),
					   string);
			gtk_entry_select_region(GTK_ENTRY(w_current->teentry),
						0, len);
		}

	        sprintf(text_size_string, "%d", text_size);
                gtk_entry_set_text(GTK_ENTRY(w_current->tsentry),
				   text_size_string);

		gtk_grab_add (w_current->tewindow);
	}
}
/***************** End of Text Edit dialog box *********************/

/***************** Start of Exit dialog box *********************/
void
exit_dialog_ok(GtkWidget *w, TOPLEVEL *w_current)
{
	gtk_grab_remove(w_current->exwindow);
	gtk_widget_destroy(w_current->exwindow);
	w_current->exwindow = NULL;

	/* go through and change ALL changed flags to 0 */
#if 0
	w_current->page_current->CHANGED = 0;
#endif
	s_page_clear_changed(w_current->page_head);
	i_callback_file_close(w_current, 0, NULL);
}

void
exit_dialog_cancel(GtkWidget *w, TOPLEVEL *w_current)
{
	gtk_grab_remove(w_current->exwindow);
	gtk_widget_destroy(w_current->exwindow);
	w_current->exwindow = NULL;

	/* leave this one */
	w_current->event_state = SELECT;
}

void
exit_dialog (TOPLEVEL *w_current)
{
	GtkWidget *label= NULL;
	GtkWidget *button = NULL;
	GtkWidget *vbox, *action_area;

	if (!w_current->exwindow) {
		w_current->exwindow = x_create_dialog_box(&vbox, &action_area);

		gtk_window_position(GTK_WINDOW(w_current->exwindow),
				    GTK_WIN_POS_MOUSE);

		gtk_signal_connect(GTK_OBJECT(w_current->exwindow),
				   "destroy",
				   GTK_SIGNAL_FUNC(destroy_window),
				   &w_current->exwindow);

#if 0 /* removed because it was causing the dialog box to not close */
      		gtk_signal_connect(GTK_OBJECT(w_current->exwindow),
				   "delete_event",
				   GTK_SIGNAL_FUNC(destroy_window),
				   &w_current->exwindow);
#endif

		gtk_window_set_title(GTK_WINDOW(w_current->exwindow),
				     "Discard Changes?");
                gtk_container_border_width(GTK_CONTAINER(w_current->exwindow),
					   0);

		button = gtk_button_new_with_label ("OK");
		GTK_WIDGET_SET_FLAGS (button, GTK_CAN_DEFAULT);
		gtk_box_pack_start(
			GTK_BOX(action_area),
			button, TRUE, TRUE, 0);
      		gtk_signal_connect(GTK_OBJECT (button), "clicked",
				   GTK_SIGNAL_FUNC(exit_dialog_ok), w_current);
      		gtk_widget_show (button);

		button = gtk_button_new_with_label ("Cancel");
		GTK_WIDGET_SET_FLAGS (button, GTK_CAN_DEFAULT);
		gtk_box_pack_start(
			GTK_BOX(action_area),
			button, TRUE, TRUE, 0);
      		gtk_signal_connect(GTK_OBJECT (button), "clicked",
				   GTK_SIGNAL_FUNC(exit_dialog_cancel),
				   w_current);
      		gtk_widget_show (button);
		gtk_widget_grab_default (button);

		label = gtk_label_new(
			"There are schematics which are unsaved!\n"
			"\n"
			"Are you sure?\n"
			"OK will discard ALL changes!");
	        gtk_misc_set_padding (GTK_MISC (label), 10, 10);
		gtk_box_pack_start(
			GTK_BOX(vbox),
			label, TRUE, TRUE, 10);
      		gtk_widget_show (label);

	}

  	if (!GTK_WIDGET_VISIBLE (w_current->exwindow)) {
		gtk_widget_show(w_current->exwindow);
		gtk_grab_add(w_current->exwindow);
	}
}
/***************** End of Exit dialog box *********************/

/***************** Start of Arc dialog box *********************/
void
arc_angles_dialog_ok(GtkWidget *w, TOPLEVEL *w_current)
{
	char *string_start = NULL;
	char *string_sweep = NULL;

	string_start = gtk_entry_get_text(GTK_ENTRY(w_current->aaentry_start));
	string_sweep = gtk_entry_get_text(GTK_ENTRY(w_current->aaentry_sweep));

	if ( (string_start[0] != '\0') && (string_sweep[0] != '\0') ) {
		/* TODO: put error detection */
		o_arc_end2(w_current, atoi(string_start), atoi(string_sweep));
	}

#if 0
	gtk_grab_remove(w_current->aawindow);
#endif
	gtk_widget_destroy(w_current->aawindow);
	w_current->aawindow = NULL;
}

void
arc_angles_dialog_cancel(GtkWidget *w, TOPLEVEL *w_current)
{
#if 0
	gtk_grab_remove(w_current->aawindow);
#endif
	gtk_widget_destroy(w_current->aawindow);
	w_current->aawindow = NULL;

	w_current->event_state = DRAWARC;
}

void
arc_angle_dialog (TOPLEVEL *w_current)
{
	GtkWidget *label = NULL;
	GtkWidget *buttonok     = NULL;
	GtkWidget *buttoncancel = NULL;
	GtkWidget *vbox, *action_area;

	if (!w_current->aawindow) {
		w_current->aawindow = x_create_dialog_box(&vbox, &action_area);

		gtk_window_position(GTK_WINDOW(w_current->aawindow),
				    GTK_WIN_POS_MOUSE);

		gtk_signal_connect(GTK_OBJECT(w_current->aawindow),
				   "destroy",
				   GTK_SIGNAL_FUNC(destroy_window),
				   &w_current->aawindow);

#if 0 /* removed because it was causing the dialog box to not close */
      		gtk_signal_connect(GTK_OBJECT(w_current->aawindow),
				   "delete_event",
				   GTK_SIGNAL_FUNC(destroy_window),
				   &w_current->aawindow);
#endif

		gtk_window_set_title(GTK_WINDOW(w_current->aawindow),
				     "Arc Params");
                gtk_container_border_width(GTK_CONTAINER(w_current->aawindow),
					   0);

		label = gtk_label_new ("Start Angle");
		gtk_box_pack_start(
			GTK_BOX(vbox),
			label, TRUE, TRUE, 0);
      		gtk_widget_show (label);

		w_current->aaentry_start = gtk_entry_new_with_max_length (4);
      		gtk_editable_select_region(
			GTK_EDITABLE(w_current->aaentry_start), 0, -1);
		gtk_box_pack_start(
			GTK_BOX(vbox),
			w_current->aaentry_start, FALSE, FALSE, 5);
      		gtk_widget_show(w_current->aaentry_start);
		gtk_widget_grab_focus(w_current->aaentry_start);

		label = gtk_label_new("Degrees of Sweep");
		gtk_box_pack_start(
			GTK_BOX(vbox),
			label, TRUE, TRUE, 0);
      		gtk_widget_show(label);

		w_current->aaentry_sweep = gtk_entry_new_with_max_length (4);
      		gtk_editable_select_region(
			GTK_EDITABLE(w_current->aaentry_sweep), 0, -1);
		gtk_box_pack_start(
			GTK_BOX(vbox),
			w_current->aaentry_sweep, FALSE, FALSE, 5);
		gtk_signal_connect(GTK_OBJECT(w_current->aaentry_sweep),
				   "activate",
				   GTK_SIGNAL_FUNC(arc_angles_dialog_ok),
				   w_current);
      		gtk_widget_show(w_current->aaentry_sweep);

		buttonok = gtk_button_new_with_label ("OK");
		GTK_WIDGET_SET_FLAGS (buttonok, GTK_CAN_DEFAULT);
		gtk_box_pack_start (
			GTK_BOX(action_area),
			buttonok, TRUE, TRUE, 0);
      		gtk_signal_connect(GTK_OBJECT (buttonok), "clicked",
				   GTK_SIGNAL_FUNC(arc_angles_dialog_ok),
				   w_current);
      		gtk_widget_show (buttonok);
		gtk_widget_grab_default (buttonok);

		buttoncancel = gtk_button_new_with_label ("Cancel");
		GTK_WIDGET_SET_FLAGS (buttoncancel, GTK_CAN_DEFAULT);
		gtk_box_pack_start (GTK_BOX (action_area),
				    buttoncancel, TRUE, TRUE, 0);
      		gtk_signal_connect (GTK_OBJECT (buttoncancel), "clicked",
				    GTK_SIGNAL_FUNC(arc_angles_dialog_cancel),
				    w_current);
      		gtk_widget_show (buttoncancel);

	}

  	if (!GTK_WIDGET_VISIBLE (w_current->aawindow)) {
		gtk_widget_show (w_current->aawindow);
#if 0
		gtk_grab_add (w_current->aawindow);
#endif
	}
}
/***************** End of Arc dialog box *********************/

/***************** Start of Translate dialog box *********************/
void
translate_dialog_ok(GtkWidget *w, TOPLEVEL *w_current)
{
	char *string=NULL;

	string = gtk_entry_get_text(GTK_ENTRY(w_current->trentry));

	if ((string[0] != '\0')) {
		/* TODO: put error detection */
		/* zero offset has a special meaning... */
		o_complex_translate_all(w_current, atoi(string));
	}

#if 0
	gtk_grab_remove(w_current->trwindow);
#endif
	gtk_widget_destroy(w_current->trwindow);
	w_current->trwindow=NULL;
}

void
translate_dialog_cancel(GtkWidget *w, TOPLEVEL *w_current)
{
	i_update_status(w_current, "Select Mode");
	w_current->event_state = SELECT;
#if 0
	gtk_grab_remove(w_current->trwindow);
#endif
	gtk_widget_destroy(w_current->trwindow);
	w_current->trwindow=NULL;
}

void
translate_dialog (TOPLEVEL *w_current)
{
	GtkWidget *label = NULL;
	GtkWidget *buttonok = NULL;
	GtkWidget *buttoncancel = NULL;
	GtkWidget *vbox, *action_area;

	if (!w_current->trwindow) {
		w_current->trwindow = x_create_dialog_box(&vbox, &action_area);

		gtk_window_position(GTK_WINDOW (w_current->trwindow),
				    GTK_WIN_POS_MOUSE);

		gtk_signal_connect(GTK_OBJECT (w_current->trwindow),
				   "destroy",
				   GTK_SIGNAL_FUNC(destroy_window),
				   &w_current->trwindow);

#if 0 /* removed because it was causing the dialog box to not close */
      		gtk_signal_connect(GTK_OBJECT (w_current->trwindow),
				   "delete_event",
				   GTK_SIGNAL_FUNC(destroy_window),
				   &w_current->trwindow);
#endif

		gtk_window_set_title(GTK_WINDOW (w_current->trwindow),
				     "Translate");
                gtk_container_border_width (GTK_CONTAINER (
			w_current->trwindow), 0);

		label = gtk_label_new("Offset to translate?\n(0 for origin)");
	        gtk_misc_set_padding(GTK_MISC (label), 10, 10);
		gtk_box_pack_start(
			GTK_BOX(vbox),
			label, TRUE, TRUE, 0);
      		gtk_widget_show (label);

		w_current->trentry = gtk_entry_new_with_max_length (10);
      		gtk_editable_select_region(GTK_EDITABLE(w_current->trentry),
					   0, -1);
		gtk_box_pack_start(
			GTK_BOX(vbox),
			w_current->trentry, FALSE, FALSE, 5);
		gtk_signal_connect(GTK_OBJECT(w_current->trentry), "activate",
				   GTK_SIGNAL_FUNC(translate_dialog_ok),
				   w_current);
      		gtk_widget_show (w_current->trentry);
		gtk_widget_grab_focus(w_current->trentry);

		buttonok = gtk_button_new_with_label ("OK");
		GTK_WIDGET_SET_FLAGS (buttonok, GTK_CAN_DEFAULT);
		gtk_box_pack_start (GTK_BOX (action_area),
				    buttonok, TRUE, TRUE, 0);
      		gtk_signal_connect(GTK_OBJECT (buttonok), "clicked",
				   GTK_SIGNAL_FUNC(translate_dialog_ok),
				   w_current);
      		gtk_widget_show (buttonok);
		gtk_widget_grab_default (buttonok);

		buttoncancel = gtk_button_new_with_label ("Cancel");
		GTK_WIDGET_SET_FLAGS (buttoncancel, GTK_CAN_DEFAULT);
		gtk_box_pack_start(
			GTK_BOX(action_area),
			buttoncancel, TRUE, TRUE, 0);
      		gtk_signal_connect(GTK_OBJECT (buttoncancel), "clicked",
				   GTK_SIGNAL_FUNC(translate_dialog_cancel),
				   w_current);
      		gtk_widget_show (buttoncancel);

	}

  	if (!GTK_WIDGET_VISIBLE (w_current->trwindow)) {
		gtk_widget_show (w_current->trwindow);
#if 0
		gtk_grab_add (w_current->trwindow);
#endif
	}
}
/***************** End of Translate dialog box *********************/

/***************** Start of Text size dialog box *********************/
void
text_size_dialog_ok(GtkWidget *w, TOPLEVEL *w_current)
{
	char *string = NULL;
	int size;

	string = gtk_entry_get_text(GTK_ENTRY(w_current->tsentry));

	if ((string[0] != '\0')) {
		size = atoi(string);
		if (size) {
			w_current->text_size = size;
		}
	}

	gtk_grab_remove(w_current->tswindow);
	gtk_widget_destroy(w_current->tswindow);
	w_current->tswindow = NULL;
}

void
text_size_dialog_cancel(GtkWidget *w, TOPLEVEL *w_current)
{
	i_update_status(w_current, "Select Mode");
	w_current->event_state = SELECT;
	gtk_grab_remove(w_current->tswindow);
	gtk_widget_destroy(w_current->tswindow);
	w_current->tswindow = NULL;
}

void
text_size_dialog (TOPLEVEL *w_current)
{
	char string[10];
	int len;
	GtkWidget *label = NULL;
	GtkWidget *buttonok = NULL;
	GtkWidget *buttoncancel = NULL;
	GtkWidget *vbox, *action_area;

	if (!w_current->tswindow) {
		w_current->tswindow = x_create_dialog_box(&vbox, &action_area);

		gtk_window_position(GTK_WINDOW(w_current->tswindow),
				    GTK_WIN_POS_MOUSE);

		gtk_signal_connect(GTK_OBJECT(w_current->tswindow),
				   "destroy",
				   GTK_SIGNAL_FUNC(destroy_window),
				   &w_current->tswindow);

#if 0 /* removed because it was causing the dialog box to not close */
      		gtk_signal_connect(GTK_OBJECT (w_current->tswindow),
				   "delete_event",
				   GTK_SIGNAL_FUNC(destroy_window),
				   &w_current->tswindow);
#endif

		gtk_window_set_title(GTK_WINDOW (w_current->tswindow),
				     "Text Size");
                gtk_container_border_width(GTK_CONTAINER(w_current->tswindow),
					   0);

		label = gtk_label_new ("Enter new text size");
	        gtk_misc_set_padding (GTK_MISC (label), 10, 10);
		gtk_box_pack_start(
			GTK_BOX(vbox),
			label, TRUE, TRUE, 0);
      		gtk_widget_show (label);

		w_current->tsentry = gtk_entry_new_with_max_length (10);
      		gtk_editable_select_region(
			GTK_EDITABLE(w_current->tsentry), 0, -1);
		gtk_box_pack_start(
			GTK_BOX(vbox),
			w_current->tsentry, FALSE, FALSE, 5);
		gtk_signal_connect(GTK_OBJECT(w_current->tsentry), "activate",
				   GTK_SIGNAL_FUNC(text_size_dialog_ok),
				   w_current);
      		gtk_widget_show (w_current->tsentry);
		gtk_widget_grab_focus(w_current->tsentry);

		buttonok = gtk_button_new_with_label ("OK");
		GTK_WIDGET_SET_FLAGS (buttonok, GTK_CAN_DEFAULT);
		gtk_box_pack_start(
			GTK_BOX(action_area),
			buttonok, TRUE, TRUE, 0);
      		gtk_signal_connect(GTK_OBJECT (buttonok), "clicked",
				   GTK_SIGNAL_FUNC(text_size_dialog_ok),
				   w_current);
      		gtk_widget_show (buttonok);
		gtk_widget_grab_default (buttonok);

		buttoncancel = gtk_button_new_with_label ("Cancel");
		GTK_WIDGET_SET_FLAGS (buttoncancel, GTK_CAN_DEFAULT);
		gtk_box_pack_start(
			GTK_BOX(action_area),
			buttoncancel, TRUE, TRUE, 0);
      		gtk_signal_connect(GTK_OBJECT (buttoncancel), "clicked",
				   GTK_SIGNAL_FUNC(text_size_dialog_cancel),
				   w_current);
      		gtk_widget_show (buttoncancel);
	}

  	if (!GTK_WIDGET_VISIBLE (w_current->tswindow)) {
		sprintf(string, "%d", w_current->text_size);
		len = strlen(string);
		gtk_entry_set_text(GTK_ENTRY(w_current->tsentry), string);
		gtk_entry_select_region(GTK_ENTRY(w_current->tsentry), 0, len);
		gtk_widget_show (w_current->tswindow);
		gtk_grab_add(w_current->tswindow);
	}
}
/***************** End of Text size dialog box *********************/

/***************** Start of Snap size dialog box *********************/
void
snap_size_dialog_ok(GtkWidget *w, TOPLEVEL *w_current)
{
	char *string = NULL;
	int size;

	string = gtk_entry_get_text(GTK_ENTRY(w_current->tsentry));

	if ((string[0] != '\0')) {
		size = atoi(string);
		if (size) {
			w_current->snap_size = size;
		}
	}

	gtk_grab_remove(w_current->tswindow);
	gtk_widget_destroy(w_current->tswindow);
	w_current->tswindow = NULL;
}

void
snap_size_dialog_cancel(GtkWidget *w, TOPLEVEL *w_current)
{
	i_update_status(w_current, "Select Mode");
	w_current->event_state = SELECT;
	gtk_grab_remove(w_current->tswindow);
	gtk_widget_destroy(w_current->tswindow);
	w_current->tswindow = NULL;
}

void
snap_size_dialog (TOPLEVEL *w_current)
{
	char string[10];
	int len;
	GtkWidget *label = NULL;
	GtkWidget *buttonok     = NULL;
	GtkWidget *buttoncancel = NULL;
	GtkWidget *vbox, *action_area;

	if (!w_current->tswindow) {
		w_current->tswindow = x_create_dialog_box(&vbox, &action_area);

		gtk_window_position(GTK_WINDOW (w_current->tswindow),
				    GTK_WIN_POS_MOUSE);

		gtk_signal_connect(GTK_OBJECT(w_current->tswindow), "destroy",
				   GTK_SIGNAL_FUNC(destroy_window),
				   &w_current->tswindow);

#if 0 /* removed because it was causing the dialog box to not close */
      		gtk_signal_connect(GTK_OBJECT(w_current->tswindow),
				   "delete_event",
				   GTK_SIGNAL_FUNC(destroy_window),
				   &w_current->tswindow);
#endif

		gtk_window_set_title(GTK_WINDOW (w_current->tswindow),
				     "Snap Grid");
                gtk_container_border_width(GTK_CONTAINER(w_current->tswindow),
					   0);

		label = gtk_label_new("Enter new snap grid spacing");
	        gtk_misc_set_padding(GTK_MISC (label), 10, 10);
		gtk_box_pack_start(
			GTK_BOX(vbox),
			label, TRUE, TRUE, 0);
      		gtk_widget_show(label);

		w_current->tsentry = gtk_entry_new_with_max_length (10);
      		gtk_editable_select_region(GTK_EDITABLE(w_current->tsentry),
					   0, -1);
		gtk_box_pack_start(
			GTK_BOX(vbox),
			w_current->tsentry, FALSE, FALSE, 5);
		gtk_signal_connect(GTK_OBJECT(w_current->tsentry), "activate",
				   GTK_SIGNAL_FUNC(snap_size_dialog_ok),
				   w_current);
      		gtk_widget_show(w_current->tsentry);
		gtk_widget_grab_focus(w_current->tsentry);

		buttonok = gtk_button_new_with_label ("OK");
		GTK_WIDGET_SET_FLAGS (buttonok, GTK_CAN_DEFAULT);
		gtk_box_pack_start(
			GTK_BOX(action_area),
			buttonok, TRUE, TRUE, 0);
      		gtk_signal_connect(GTK_OBJECT (buttonok), "clicked",
				   GTK_SIGNAL_FUNC(snap_size_dialog_ok),
				   w_current);
      		gtk_widget_show (buttonok);
		gtk_widget_grab_default (buttonok);

		buttoncancel = gtk_button_new_with_label ("Cancel");
		GTK_WIDGET_SET_FLAGS (buttoncancel, GTK_CAN_DEFAULT);
		gtk_box_pack_start(
			GTK_BOX(action_area),
			buttoncancel, TRUE, TRUE, 0);
      		gtk_signal_connect(GTK_OBJECT (buttoncancel), "clicked",
				   GTK_SIGNAL_FUNC(snap_size_dialog_cancel),
				   w_current);
      		gtk_widget_show(buttoncancel);

	}

  	if (!GTK_WIDGET_VISIBLE (w_current->tswindow)) {
		sprintf(string, "%d", w_current->snap_size);
		len = strlen(string);
		gtk_entry_set_text(GTK_ENTRY(w_current->tsentry), string);
		gtk_entry_select_region(GTK_ENTRY(w_current->tsentry), 0, len);
		gtk_widget_show (w_current->tswindow);
		gtk_grab_add (w_current->tswindow);
	}
}
/***************** End of Snap size dialog box *********************/

/***************** Start of slot edit dialog box *********************/
void
slot_edit_dialog_ok(GtkWidget *w, TOPLEVEL *w_current)
{
	int len;
	char *string = NULL;

	string = gtk_entry_get_text(GTK_ENTRY(w_current->teentry));

	if (string[0] != '\0') {
		len = strlen(string);

#if DEBUG
		printf("text was: _%s_ %d\n", string, len);
#endif

		if (len < 80) {
			o_slot_end(w_current, string, len);
		} else {
			/* TODO: you should NOT have limits */
			fprintf(stderr, "String too long... hack!\n");
		}
	}

	w_current->event_state = SELECT;
	i_update_status(w_current, "Select Mode");

	gtk_grab_remove(w_current->tewindow);
	gtk_widget_destroy(w_current->tewindow);
	w_current->tewindow = NULL;
}

void
slot_edit_dialog_cancel(GtkWidget *w, TOPLEVEL *w_current)
{
	i_update_status(w_current, "Select Mode");
	w_current->event_state = SELECT;
	gtk_grab_remove(w_current->tewindow);
	gtk_widget_destroy(w_current->tewindow);
	w_current->tewindow = NULL;
}

void
slot_edit_dialog (TOPLEVEL *w_current, char *string)
{
	GtkWidget *label = NULL;
	GtkWidget *buttonok = NULL;
	GtkWidget *buttoncancel = NULL;
	GtkWidget *vbox, *action_area;
	int len;

	if (!w_current->tewindow) {
		w_current->tewindow = x_create_dialog_box(&vbox, &action_area);

		gtk_window_position(GTK_WINDOW(w_current->tewindow),
				    GTK_WIN_POS_MOUSE);

		gtk_signal_connect(GTK_OBJECT (w_current->tewindow),
				   "destroy", GTK_SIGNAL_FUNC(destroy_window),
				   &w_current->tewindow);

#if 0 /* removed because it was causing the dialog box to not close */
      		gtk_signal_connect(GTK_OBJECT (w_current->tewindow),
				   "delete_event",
				   GTK_SIGNAL_FUNC(destroy_window),
				   &w_current->tewindow);
#endif

		gtk_window_set_title(GTK_WINDOW (w_current->tewindow),
				     "Edit slot number");
                gtk_container_border_width(
			GTK_CONTAINER(w_current->tewindow), 0);

		label = gtk_label_new ("Edit slot number");
		gtk_box_pack_start(
			GTK_BOX (vbox),
			label, TRUE, TRUE, 0);
      		gtk_widget_show (label);

		w_current->teentry = gtk_entry_new();
      		gtk_editable_select_region(
			GTK_EDITABLE (w_current->teentry), 0, -1);
		gtk_box_pack_start(
			GTK_BOX(vbox),
			w_current->teentry, TRUE, TRUE, 10);

		gtk_signal_connect(GTK_OBJECT(w_current->teentry), "activate",
                       	           GTK_SIGNAL_FUNC(slot_edit_dialog_ok),
                       	           w_current);
      		gtk_widget_show (w_current->teentry);
		gtk_widget_grab_focus(w_current->teentry);

		buttonok = gtk_button_new_with_label ("OK");
		GTK_WIDGET_SET_FLAGS (buttonok, GTK_CAN_DEFAULT);
		gtk_box_pack_start(
			GTK_BOX(action_area),
			buttonok, TRUE, TRUE, 0);
      		gtk_signal_connect (GTK_OBJECT (buttonok), "clicked",
			            GTK_SIGNAL_FUNC(slot_edit_dialog_ok),
				    w_current);
      		gtk_widget_show (buttonok);
		gtk_widget_grab_default (buttonok);

		buttoncancel = gtk_button_new_with_label ("Cancel");
		GTK_WIDGET_SET_FLAGS (buttoncancel, GTK_CAN_DEFAULT);
		gtk_box_pack_start (GTK_BOX (action_area), buttoncancel, 
                                    TRUE, TRUE, 0);
      		gtk_signal_connect (GTK_OBJECT (buttoncancel), "clicked",
			            GTK_SIGNAL_FUNC(slot_edit_dialog_cancel),
				    w_current);
      		gtk_widget_show (buttoncancel);

	}

  	if (!GTK_WIDGET_VISIBLE (w_current->tewindow)) {
		gtk_widget_show (w_current->tewindow);

		if (string != NULL) {
			len = strlen(string);
			gtk_entry_set_text(GTK_ENTRY(w_current->teentry),
					   string);
			gtk_entry_select_region(GTK_ENTRY(w_current->teentry),
						strlen("slot="), len);
		}
		gtk_grab_add (w_current->tewindow);
	}
}
/***************** End of Slot Edit dialog box *********************/

/***************** Start of help/about dialog box *********************/
void
about_dialog_close(GtkWidget *w, TOPLEVEL *w_current)
{
	gtk_widget_destroy(w_current->abwindow);
	w_current->abwindow = NULL;
}

void
about_dialog (TOPLEVEL *w_current)
{
	GtkWidget *label = NULL;
	GtkWidget *buttonclose = NULL;
	GtkWidget *vbox, *action_area;
	char string[100];

	if (!w_current->abwindow) {
		w_current->abwindow = x_create_dialog_box(&vbox, &action_area);

		gtk_window_position (GTK_WINDOW (w_current->abwindow),
				     GTK_WIN_POS_MOUSE);

		gtk_window_set_title (GTK_WINDOW (w_current->abwindow),
				      "About...");
                gtk_container_border_width (GTK_CONTAINER (
			w_current->abwindow), 5);

		gtk_signal_connect (GTK_OBJECT (w_current->abwindow),
				    "destroy", GTK_SIGNAL_FUNC(destroy_window),
                          	    &w_current->abwindow);

#if 0 /* removed because it was causing the dialog box to not close */
      		gtk_signal_connect (GTK_OBJECT (w_current->abwindow),
				    "delete_event",
				    GTK_SIGNAL_FUNC(destroy_window),
                          	    &w_current->abwindow);
#endif

		sprintf(string, "gEDA : GNU Electronic Design Automation");
		label = gtk_label_new (string);
		gtk_box_pack_start(
			GTK_BOX(vbox),
			label, TRUE, TRUE, 5);
      		gtk_widget_show (label);

		sprintf(string, "gschem version %s", VERSION);
		label = gtk_label_new (string);
		gtk_box_pack_start(
			GTK_BOX(vbox),
			label, TRUE, TRUE, 5);
      		gtk_widget_show (label);

		sprintf(string, "Ales V. Hvezda\nahvezda@geda.seul.org");
		label = gtk_label_new (string);
		gtk_box_pack_start(
			GTK_BOX(vbox),
			label, TRUE, TRUE, 5);
      		gtk_widget_show (label);

		buttonclose = gtk_button_new_with_label ("Close");
		GTK_WIDGET_SET_FLAGS (buttonclose, GTK_CAN_DEFAULT);
		gtk_box_pack_start(
			GTK_BOX(action_area),
			buttonclose, TRUE, TRUE, 0);
      		gtk_signal_connect(GTK_OBJECT (buttonclose), "clicked",
				   GTK_SIGNAL_FUNC(about_dialog_close),
				   w_current);
      		gtk_widget_show(buttonclose);

	}

  	if (!GTK_WIDGET_VISIBLE(w_current->abwindow)) {
		gtk_widget_show(w_current->abwindow);
	}
}
/***************** End of help/about dialog box *********************/

/***************** Start of coord dialog box *********************/
void
coord_dialog_close(GtkWidget *w, TOPLEVEL *w_current)
{
	gtk_widget_destroy(w_current->cowindow);
	w_current->cowindow = NULL;
}

void
coord_display_update(TOPLEVEL *w_current, int x, int y)
{
	char string[25]; /* this should be big enough */
	int world_x, world_y;

	sprintf(string, "(%d, %d)", x, y);
 	gtk_label_set_text(GTK_LABEL(w_current->coord_screen), string );

	SCREENtoWORLD(w_current, x, y, &world_x, &world_y);

	sprintf(string, "(%d, %d)", world_x, world_y);
 	gtk_label_set_text(GTK_LABEL(w_current->coord_world), string );
}

void
coord_dialog (TOPLEVEL *w_current, int x, int y)
{
        GtkWidget *buttonclose = NULL;
        GtkWidget *frame;
        GtkWidget *vbox2;

        if (!w_current->cowindow) {
                w_current->cowindow = gtk_window_new (GTK_WINDOW_TOPLEVEL);
                gtk_window_position (GTK_WINDOW (w_current->cowindow),
                                     GTK_WIN_POS_MOUSE);

                gtk_window_set_title (GTK_WINDOW (w_current->cowindow),
                                      "Coords");
                gtk_container_border_width (GTK_CONTAINER (
			w_current->cowindow), 5);

                gtk_signal_connect (GTK_OBJECT (w_current->cowindow),
                                    "destroy", GTK_SIGNAL_FUNC(destroy_window),
                                    &w_current->cowindow);

#if 0 /* removed because it was causing the dialog box to not close */
                gtk_signal_connect (GTK_OBJECT (w_current->cowindow),
                                    "delete_event",
                                    GTK_SIGNAL_FUNC(destroy_window),
                                    &w_current->cowindow);
#endif

                vbox2 = gtk_vbox_new (FALSE, 5);
                gtk_container_add (GTK_CONTAINER (w_current->cowindow), vbox2);
                gtk_widget_show(vbox2);

                frame = gtk_frame_new ("Screen");
                w_current->coord_screen =
			gtk_label_new("(########, ########)");
                gtk_label_set_justify(
			GTK_LABEL(w_current->coord_screen), GTK_JUSTIFY_LEFT);
                gtk_misc_set_padding(GTK_MISC(w_current->coord_screen),
				     10, 10);
                gtk_container_add(GTK_CONTAINER (frame),
				  w_current->coord_screen);
                gtk_box_pack_start(GTK_BOX (vbox2), frame, FALSE, FALSE, 0);
                gtk_widget_show(w_current->coord_screen);
                gtk_widget_show(frame);

                frame = gtk_frame_new ("World");
                w_current->coord_world =
			gtk_label_new ("(########, ########)");
                gtk_misc_set_padding(GTK_MISC(w_current->coord_world), 10, 10);
                gtk_label_set_justify(GTK_LABEL(w_current->coord_world),
				      GTK_JUSTIFY_LEFT);
                gtk_container_add(GTK_CONTAINER (frame),
				  w_current->coord_world);
                gtk_box_pack_start(GTK_BOX (vbox2), frame, FALSE, FALSE, 0);
                gtk_widget_show(w_current->coord_world);
                gtk_widget_show(frame);

		buttonclose = gtk_button_new_with_label ("Close");
		GTK_WIDGET_SET_FLAGS (buttonclose, GTK_CAN_DEFAULT);
		gtk_box_pack_start(GTK_BOX ( vbox2 ),
				   buttonclose, TRUE, TRUE, 0);
      		gtk_signal_connect(GTK_OBJECT (buttonclose), "clicked",
				   GTK_SIGNAL_FUNC(coord_dialog_close),
				   w_current);
      		gtk_widget_show(buttonclose);
		gtk_widget_grab_default (buttonclose);

        }

        if (!GTK_WIDGET_VISIBLE (w_current->cowindow)) {
                gtk_widget_show (w_current->cowindow);
		coord_display_update(w_current, x, y);
        } else {
		gdk_window_raise(w_current->cowindow->window);
	}
}
/***************** End of coord dialog box *********************/

/***************** Start of color edit dialog box *********************/
gint
color_set(GtkWidget *w, gpointer data)
{
	int index;

	index = (int) data;

	/* hate to use this here... but I have to... */
	global_window_current->edit_color = index;
        return(0);
}

/* be sure that the caller frees the returned string */
char *
index2functionstring(int index)
{
	char *string;

	switch(index) {
		case(BACKGROUND_COLOR):
			string = u_basic_strdup("background");
			break;
		case(PIN_COLOR):
			string = u_basic_strdup("pin");
			break;
		case(NET_ENDPOINT_COLOR):
			string = u_basic_strdup("net endpoint");
			break;
		case(GRAPHIC_COLOR):
			string = u_basic_strdup("graphic");
			break;
		case(NET_COLOR):
			string = u_basic_strdup("net");
			break;
		case(ATTRIBUTE_COLOR):
			string = u_basic_strdup("attribute");
			break;
		case(LOGIC_BUBBLE_COLOR):
			string = u_basic_strdup("logic bubble");
			break;
		case(GRID_COLOR):
			string = u_basic_strdup("grid point");
			break;
		case(DETACHED_ATTRIBUTE_COLOR):
			string = u_basic_strdup("detached attribute");
			break;
		case(TEXT_COLOR):
			string = u_basic_strdup("text");
			break;
		case(BUS_COLOR):
			string = u_basic_strdup("bus");
			break;
		case(SELECT_COLOR):
			string = u_basic_strdup("select");
			break;
		case(BOUNDINGBOX_COLOR):
			string = u_basic_strdup("bounding box");
			break;
		case(ZOOM_BOX_COLOR):
			string = u_basic_strdup("zoom box");
			break;
		case(STROKE_COLOR):
			string = u_basic_strdup("stroke");
			break;
		case(LOCK_COLOR):
			string = u_basic_strdup("lock");
			break;
		case(OUTPUT_BACKGROUND_COLOR):
			string = u_basic_strdup("output background");
			break;
		default:
			string = u_basic_strdup("unknown");
			break;
	}
	return(string);
}


/* this is from gtktest.c */
static GtkWidget*
create_color_menu (TOPLEVEL *w_current)
{
	GtkWidget *menu;
	GtkWidget *menuitem;
	GSList *group;
	int index=0;
	char buf[30]; /* should be be big enough hack */
	char menu_string[60]; /* size is a hack */
	char *temp=NULL;
	int found=0;
	int set_first=0;

	menu = gtk_menu_new ();
	group = NULL;

	found = x_color_get_name(index, buf);
	while (found != FALSE) {

		if (found == TRUE) {

			/* set the default to the first entry */
			if (!set_first) {
				global_window_current->edit_color = index;
				set_first = 1;
			}

			temp = index2functionstring(index);
			sprintf(menu_string, "%d | %s | %s", index, 
							 temp,
							 buf);
			free(temp);
			
			menuitem = gtk_radio_menu_item_new_with_label (group, 
								  menu_string);
			group = gtk_radio_menu_item_group(GTK_RADIO_MENU_ITEM(
						          menuitem));

			gtk_menu_append (GTK_MENU (menu), menuitem);

			gtk_signal_connect (GTK_OBJECT (menuitem), 
					       "activate", 
					       (GtkSignalFunc) color_set,
			    		       (int *) index);

			/* I have no idea if doing the above cast is valid, */
			/* since index isn't a pointer, it's just being */
			/* treated as one, it's then cast to an int in */
			/* color_set */

			gtk_widget_show (menuitem);
		}

		index++;
		found = x_color_get_name(index, buf);
	}


	return menu;
}

void
color_edit_dialog_close(GtkWidget *w, TOPLEVEL *w_current)
{
	gtk_widget_destroy(w_current->clwindow);
	w_current->clwindow = NULL;
}

void
color_edit_dialog_apply(GtkWidget *w, TOPLEVEL *w_current)
{
	OBJECT *o_current = NULL;
	OBJECT *found = NULL;

	o_current = w_current->page_current->selection_head->next;

	while(o_current != NULL) {
		found = (OBJECT *) o_list_search(
			w_current->page_current->object_head,
			o_current);

		if (found == NULL) {
			fprintf(stderr,
				"UGGG! you blew it... "
				"tried to delete something that didn't exist");
			exit(-1);
		}

		switch(found->type) {
			case(OBJ_LINE):
			case(OBJ_BOX):
			case(OBJ_CIRCLE):
			case(OBJ_NET):
			case(OBJ_BUS):
			case(OBJ_PIN):
			case(OBJ_ARC):
				found->color = w_current->edit_color;
				w_current->page_current->CHANGED = 1;
				break;

			case(OBJ_TEXT):
				found->color = w_current->edit_color;
				o_complex_set_color(w_current,
					    w_current->edit_color,
					    found->complex);
				w_current->page_current->CHANGED = 1;
				break;
		}

		o_current = o_current->next;
	}
}

void
color_edit_dialog (TOPLEVEL *w_current)
{
        GtkWidget *buttonclose = NULL;
        GtkWidget *buttonapply = NULL;
	GtkWidget *optionmenu;
	GtkWidget *vbox, *action_area;

        if (!w_current->clwindow) {
		w_current->clwindow = x_create_dialog_box(&vbox, &action_area);

                gtk_window_position (GTK_WINDOW (w_current->clwindow),
                                     GTK_WIN_POS_MOUSE);

                gtk_window_set_title (GTK_WINDOW (w_current->clwindow),
                                      "Color Edit");
                gtk_container_border_width(
			GTK_CONTAINER(w_current->clwindow), 5);

                gtk_signal_connect (GTK_OBJECT (w_current->clwindow),
                                    "destroy", GTK_SIGNAL_FUNC(destroy_window),
                                    &w_current->clwindow);

#if 0 /* removed because it was causing the dialog box to not close */
                gtk_signal_connect (GTK_OBJECT (w_current->clwindow),
                                    "delete_event",
                                    GTK_SIGNAL_FUNC(destroy_window),
                                    &w_current->clwindow);
#endif

		optionmenu = gtk_option_menu_new ();
                gtk_option_menu_set_menu(GTK_OPTION_MENU(optionmenu),
					 create_color_menu (w_current));
                gtk_option_menu_set_history(GTK_OPTION_MENU (optionmenu), 0);
                gtk_box_pack_start(
			GTK_BOX(vbox),
			optionmenu, TRUE, TRUE, 0);
                gtk_widget_show (optionmenu);

		buttonapply = gtk_button_new_with_label ("Apply");
		GTK_WIDGET_SET_FLAGS (buttonapply, GTK_CAN_DEFAULT);
		gtk_box_pack_start(
			GTK_BOX(action_area),
			buttonapply, TRUE, TRUE, 0);
      		gtk_signal_connect(GTK_OBJECT (buttonapply), "clicked",
				   GTK_SIGNAL_FUNC(color_edit_dialog_apply),
				   w_current);
      		gtk_widget_show (buttonapply);
		gtk_widget_grab_default(buttonapply);

		buttonclose = gtk_button_new_with_label ("Close");
		gtk_box_pack_start(
			GTK_BOX(action_area),
			buttonclose, TRUE, TRUE, 0);
      		gtk_signal_connect(GTK_OBJECT (buttonclose), "clicked",
				   GTK_SIGNAL_FUNC(color_edit_dialog_close),
				   w_current);
      		gtk_widget_show(buttonclose);

        }

        if (!GTK_WIDGET_VISIBLE(w_current->clwindow)) {
                gtk_widget_show(w_current->clwindow);
        } else {
		gdk_window_raise(w_current->clwindow->window);
	}
}
/***************** End of color edit dialog box *********************/

/***************** Start of help/keymapping dialog box *********************/

/* limit this to 128 hotkeys */
static char *hotkey_strings[128];
static int hotkey_counter=0;

void
x_dialog_hotkeys_close (GtkWidget *w, TOPLEVEL *w_current)
{
	gtk_widget_destroy(w_current->hkwindow);
	w_current->hkwindow = NULL;
}

void
x_dialog_hotkeys_free_all(void)
{
	int i;

	for (i = 0 ; i < hotkey_counter; i++) {
		if (hotkey_strings[i]) {
			free(hotkey_strings[i]);
		}
	}
}

void
x_dialog_hotkeys_fill(char *string) 
{

	if (hotkey_counter > 127) {
		printf("Ran out of space in the hotkey buffer...\n");
		return;
	}	

	hotkey_strings[hotkey_counter] = (char *) malloc(sizeof(char)*(
							strlen(string)+1));
;
	strcpy(hotkey_strings[hotkey_counter], string);
	hotkey_counter++;
}

void
x_dialog_hotkeys (TOPLEVEL *w_current)
{
	GtkWidget *buttonclose = NULL;
	GtkWidget *vbox, *action_area, *scrolled_win, *list;
	GtkWidget *item;
	int i;

	if (!w_current->hkwindow) {


		w_current->hkwindow = x_create_dialog_box(&vbox, &action_area);

		gtk_window_position (GTK_WINDOW (w_current->hkwindow),
				     GTK_WIN_POS_MOUSE);

		gtk_window_set_title (GTK_WINDOW (w_current->hkwindow),
				      "Hotkeys...");
                gtk_container_border_width (GTK_CONTAINER (
			w_current->hkwindow), 5);

		gtk_widget_set_usize(w_current->hkwindow, 300,300);

		gtk_signal_connect (GTK_OBJECT (w_current->hkwindow),
				    "destroy", GTK_SIGNAL_FUNC(destroy_window),
                          	    &w_current->hkwindow);

#if 0 /* removed because it was causing the dialog box to not close */
      		gtk_signal_connect (GTK_OBJECT (w_current->hkwindow),
				    "delete_event",
				    GTK_SIGNAL_FUNC(destroy_window),
                          	    &w_current->hkwindow);
#endif

      		scrolled_win = gtk_scrolled_window_new (NULL, NULL);
      		gtk_container_set_border_width (GTK_CONTAINER (scrolled_win), 5);
      		gtk_widget_set_usize (scrolled_win, -1, 300);
      		gtk_box_pack_start (GTK_BOX (vbox), scrolled_win, TRUE, TRUE, 0);
      		gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (scrolled_win),
				      GTK_POLICY_AUTOMATIC,
				      GTK_POLICY_AUTOMATIC);
      		gtk_widget_show (scrolled_win);

      		list = gtk_list_new ();
      		gtk_list_set_selection_mode (GTK_LIST (list), GTK_SELECTION_SINGLE);
      		gtk_scrolled_window_add_with_viewport
				(GTK_SCROLLED_WINDOW (scrolled_win), list);
      		gtk_container_set_focus_vadjustment
				(GTK_CONTAINER (list),
	 	gtk_scrolled_window_get_vadjustment
	 			(GTK_SCROLLED_WINDOW (scrolled_win)));
      		gtk_container_set_focus_hadjustment
				(GTK_CONTAINER (list),
	 			gtk_scrolled_window_get_hadjustment
	 			(GTK_SCROLLED_WINDOW (scrolled_win)));
		gtk_widget_show(list);

	      	item = gtk_list_item_new_with_label (
						"Function : keystroke(s)");
	       	gtk_container_add (GTK_CONTAINER (list), item);
		gtk_widget_show(item);

	      	item = gtk_list_item_new_with_label (" ");
	       	gtk_container_add (GTK_CONTAINER (list), item);
		gtk_widget_show(item);

		for (i = 0 ; i < hotkey_counter; i++) {

			if (hotkey_strings[i]) {	
	      			item = gtk_list_item_new_with_label (
						hotkey_strings[i]);
	        		gtk_container_add (GTK_CONTAINER (list), item);
				gtk_widget_show(item);
			}
		}

		buttonclose = gtk_button_new_with_label ("Close");
		GTK_WIDGET_SET_FLAGS (buttonclose, GTK_CAN_DEFAULT);
		gtk_box_pack_start(
			GTK_BOX(action_area),
			buttonclose, TRUE, TRUE, 0);
      		gtk_signal_connect(GTK_OBJECT (buttonclose), "clicked",
				   GTK_SIGNAL_FUNC(x_dialog_hotkeys_close),
				   w_current);
      		gtk_widget_show(buttonclose);

	}

  	if (!GTK_WIDGET_VISIBLE(w_current->hkwindow)) {
		gtk_widget_show(w_current->hkwindow);
        } else {
		gdk_window_raise(w_current->hkwindow->window);
	}
}
/***************** End of help/keymapping dialog box *********************/
