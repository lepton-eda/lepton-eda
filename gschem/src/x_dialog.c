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

char*
multi_attrib_edit_parser (GtkWidget *w,
	char **text, int *vis, int *show)
{
	GtkWidget *window;
	GtkWidget *label;
	GtkWidget *value;

	GtkWidget *visbutton;
	GtkWidget *showvalbutton;
	GtkWidget *shownamebutton;

	char *newtext;
	
	text[1]=malloc(4*sizeof(char)); /* this needs to be freed somewhere? */
	
	window = gtk_object_get_data(GTK_OBJECT(w),"window");
	label = gtk_object_get_data(GTK_OBJECT(window),"lab_entry");
	value = gtk_object_get_data(GTK_OBJECT(window),"val_entry");

	visbutton = gtk_object_get_data(GTK_OBJECT(window),"visbutton");
	showvalbutton = gtk_object_get_data(GTK_OBJECT(window),"showvalbutton");
	shownamebutton = gtk_object_get_data(GTK_OBJECT(window),"shownamebutton");

	text[0]=gtk_entry_get_text(GTK_ENTRY(label));
	text[2]=gtk_entry_get_text(GTK_ENTRY(value));

	text[1][1]=' ';
	text[1][3]=0;

	newtext = u_basic_strdup_multiple(text[0],"=",text[2],NULL);

        if(gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(visbutton)))
	{
                *vis = VISIBLE;
		text[1][0]='V';
	}
        else
	{
                *vis = INVISIBLE;
		text[1][0]='I';
	}
        if(gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(showvalbutton)))
	{
                *show = SHOW_VALUE;
		text[1][2]='V';
	}
        else if(gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(shownamebutton)))
	{
                *show = SHOW_NAME;
		text[1][2]='N';
	}
        else
	{
                *show = SHOW_NAME_VALUE;
		text[1][2]='B';
	}
	return newtext;
}

void
multi_attrib_edit_set_values (GtkWindow *window, OBJECT *attrib)
{
	GtkWidget *lab_entry;
	GtkWidget *val_entry;
	GtkWidget *showvalbutton;
	GtkWidget *shownamebutton;
	GtkWidget *showbothbutton;
	GtkWidget *visbutton;

	char name[1000];
	char value[1000];

	lab_entry = gtk_object_get_data(GTK_OBJECT(window),"lab_entry");
	val_entry = gtk_object_get_data(GTK_OBJECT(window),"val_entry");

	visbutton = gtk_object_get_data(GTK_OBJECT(window),"visbutton");

	showbothbutton = gtk_object_get_data(GTK_OBJECT(window),"showbothbutton");
	shownamebutton = gtk_object_get_data(GTK_OBJECT(window),"shownamebutton");
	showvalbutton = gtk_object_get_data(GTK_OBJECT(window),"showvalbutton");

	if(!attrib)
	{
		gtk_entry_set_text(GTK_ENTRY(lab_entry),"");
		gtk_entry_set_text(GTK_ENTRY(val_entry),"");
		gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(visbutton),TRUE);
		gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(showvalbutton),TRUE);
	}
	else
	{
		if(attrib->visibility == VISIBLE)
			gtk_toggle_button_set_active 
				(GTK_TOGGLE_BUTTON (visbutton), TRUE);
		else
			gtk_toggle_button_set_active
                                (GTK_TOGGLE_BUTTON (visbutton), FALSE);
	
		if(attrib->show_name_value == SHOW_NAME)
			gtk_toggle_button_set_active 
				(GTK_TOGGLE_BUTTON(shownamebutton),TRUE);
		else if(attrib->show_name_value == SHOW_VALUE)
			gtk_toggle_button_set_active 
				(GTK_TOGGLE_BUTTON(showvalbutton),TRUE);
		else gtk_toggle_button_set_active 
			(GTK_TOGGLE_BUTTON(showbothbutton),TRUE);

		o_attrib_get_name_value(attrib->text_string,name,value);
		gtk_entry_set_text (GTK_ENTRY (val_entry), value);
		gtk_entry_set_text (GTK_ENTRY (lab_entry), name);
		gtk_widget_grab_focus(val_entry);
	}
}

void
multi_attrib_edit_clear (GtkWidget *w, GtkWindow *window)
{
	multi_attrib_edit_set_values(window,NULL);
}

void
multi_attrib_edit_select_row (GtkCList *clist, gint row, gint col,
				GdkEventButton *event, TOPLEVEL *w_current)
{
	GtkWidget *window;
	OBJECT *attrib;

	window = w_current->mawindow;
	clist = gtk_object_get_data(GTK_OBJECT(window),"clist");

	gtk_object_set_data(GTK_OBJECT(clist),"selected",(gpointer)row);
	attrib = gtk_clist_get_row_data(clist,row);
	multi_attrib_edit_set_values (GTK_WINDOW(window), attrib);
}

void
multi_attrib_edit_add (GtkWidget *w, TOPLEVEL *w_current)
{
	int vis,show;
	char *newtext;
	gint row;
	OBJECT *attrib;
	OBJECT *object;
	char **text;
	GtkWidget *clist;

	clist = gtk_object_get_data(GTK_OBJECT(w_current->mawindow),"clist");

	text=malloc(3*sizeof(char*));

	newtext = multi_attrib_edit_parser (w,text,&vis,&show);

	if(text[0][0] == '\0' || text[0][0] == ' ')
		return;

	row = gtk_clist_append(GTK_CLIST(clist),text);

	object = o_select_return_first_object(w_current); 
	attrib = o_attrib_add_attrib(w_current, newtext, vis, show, object);

	gtk_clist_set_row_data(GTK_CLIST(clist),row,attrib);
	multi_attrib_edit_clear(NULL,GTK_WINDOW(w_current->mawindow));

	free(newtext);
	free(text);
}

void
multi_attrib_edit_change (GtkWidget *w, TOPLEVEL *w_current)
{
	int vis,show;
	char *newtext;
	gint row;
	OBJECT *attrib;
	char **text;
	GtkWidget *clist;

	clist = gtk_object_get_data(GTK_OBJECT(w_current->mawindow),"clist");

	text=malloc(3*sizeof(char*));
	newtext = multi_attrib_edit_parser (w,text,&vis,&show);

	if(text[0][0] != '\0' && text[0][0] != ' ')
	{
		row = (int)gtk_object_get_data(GTK_OBJECT(clist),"selected");
		if(row != -1)
		{
			attrib = gtk_clist_get_row_data(GTK_CLIST(clist),row);
			o_text_change(w_current,attrib,newtext,vis,show);
/*			multi_attrib_edit_clear(NULL,GTK_WINDOW(w_current->mawindow));*/
			gtk_clist_set_text(GTK_CLIST(clist),row,0,text[0]);
			gtk_clist_set_text(GTK_CLIST(clist),row,1,text[1]);
			gtk_clist_set_text(GTK_CLIST(clist),row,2,text[2]);
		}
	}
	free(newtext);
	free(text);

	if (gtk_object_get_data(GTK_OBJECT(w), "close")) {
	  multi_attrib_edit_close (w,w_current);
	}
}


void
multi_attrib_edit_delete (GtkWidget *w, TOPLEVEL *w_current)
{
	OBJECT *attrib;
	GtkCList *clist;
	gint selected;
	
	clist = gtk_object_get_data(GTK_OBJECT(w_current->mawindow),"clist");

	selected = (gint)gtk_object_get_data(GTK_OBJECT(clist),"selected");
	if(selected == -1)
		return;
	attrib = gtk_clist_get_row_data(clist,selected);
	gtk_clist_remove(clist,selected);
	gtk_object_set_data(GTK_OBJECT(clist),"selected",(gpointer)-1);
	
	o_delete_text(w_current,attrib);
	
	multi_attrib_edit_clear (NULL,GTK_WINDOW(w_current->mawindow));
/* Tell System about change! */

}

void
multi_attrib_edit_close (GtkWidget *w, TOPLEVEL *w_current)
{
        i_update_status(w_current, "Select Mode");
        w_current->event_state = SELECT;
        gtk_grab_remove(w_current->mawindow);
        gtk_widget_destroy(w_current->mawindow);
        w_current->mawindow = NULL;
}

void
multi_attrib_edit (TOPLEVEL *w_current, SELECTION *list)
{

	char *text[3];
	char *string;
	int i;
	int row;
	OBJECT **attriblist=NULL;
	OBJECT *object=NULL;

	GtkWidget *window;
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

	/* no longer used?*/
	/* gchar *titles[]= {"Attribute","Visibility","Value"}; */


	/* Do basic checks first */	
	if(!w_current) return;
	if(!w_current->page_current) return;
	if(!w_current->page_current->object_head) return;
	object = list->selected_object;
	if (!object) return;
	
	attriblist=o_attrib_return_attribs(w_current->page_current->object_head,
					   object);	
	text[0] = malloc(sizeof(char)*512);
	text[1] = malloc(sizeof(char)*5);
	text[2] = malloc(sizeof(char)*512);
	text[1][1]=' ';
	text[1][3]='\0';

#if 0 /* this method cannot be used since it prevents this dialog box to be */
      /* opened if nothing is selected; the check needs to happen further */
      /* down */
	/* Make sure attriblist isn't NULL */
	if (attriblist == NULL) {
		free(text[0]);
		free(text[1]);
		free(text[2]);
		return;
	}
#endif
	
	window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
	gtk_window_set_title (GTK_WINDOW (window), "Edit Component");
	gtk_widget_set_usize(window, 545, 200);
  
	gtk_window_position(GTK_WINDOW (window), GTK_WIN_POS_MOUSE);

	vbox = gtk_vbox_new (FALSE, 0);
	gtk_widget_show (vbox);
	gtk_container_add (GTK_CONTAINER (window), vbox);
	
	scrolledwindow1 = gtk_scrolled_window_new (NULL, NULL);
	gtk_widget_show (scrolledwindow1);
	gtk_box_pack_start (GTK_BOX (vbox), scrolledwindow1, TRUE, TRUE, 0);
	gtk_container_set_border_width (GTK_CONTAINER (scrolledwindow1), 3);
	gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (scrolledwindow1),
		GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
	
	clist = gtk_clist_new (3);
	gtk_object_set_data(GTK_OBJECT(window),"clist",clist);
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
	gtk_object_set_data(GTK_OBJECT(window),"lab_entry",lab_entry);
	
	hbox5 = gtk_hbox_new (FALSE, 0);
	gtk_widget_show (hbox5);
	
	visbutton = gtk_check_button_new_with_label ("Visible");
	gtk_object_set_data (GTK_OBJECT (window), "visbutton", visbutton);
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
	gtk_object_set_data(GTK_OBJECT(window),"val_entry",val_entry);
	gtk_object_set_data(GTK_OBJECT(val_entry),"window",window);
	gtk_object_set_data(GTK_OBJECT(val_entry),"close",(void *)1);
	
	hbox6 = gtk_hbox_new (FALSE, 0);
	gtk_widget_show (hbox6);
	
	gtk_table_attach (GTK_TABLE (table), hbox6,1,2,1,2,
	                  (GtkAttachOptions) (GTK_EXPAND | GTK_FILL),
                    (GtkAttachOptions) (GTK_EXPAND | GTK_FILL), 0, 0);

	namebutton = gtk_radio_button_new_with_label (show_group, "Name");
	show_group = gtk_radio_button_group (GTK_RADIO_BUTTON (namebutton));
	gtk_widget_show (namebutton);
	gtk_box_pack_start (GTK_BOX (hbox6), namebutton, FALSE, FALSE, 0);
	gtk_object_set_data(GTK_OBJECT(window),"shownamebutton",namebutton);
	
	valbutton = gtk_radio_button_new_with_label (show_group, "Value");
	show_group = gtk_radio_button_group (GTK_RADIO_BUTTON (valbutton));
	gtk_widget_show (valbutton);
	gtk_box_pack_start (GTK_BOX (hbox6), valbutton, FALSE, FALSE, 0);
	gtk_object_set_data(GTK_OBJECT(window),"showvalbutton",valbutton);
	
	bothbutton = gtk_radio_button_new_with_label (show_group, "Both");
	show_group = gtk_radio_button_group (GTK_RADIO_BUTTON (bothbutton));
	gtk_widget_show (bothbutton);
	gtk_box_pack_start (GTK_BOX (hbox6), bothbutton, FALSE, FALSE, 0);
	gtk_object_set_data(GTK_OBJECT(window),"showbothbutton",bothbutton);
	
	hbuttonbox1 = gtk_hbutton_box_new ();
	gtk_widget_show (hbuttonbox1);
	gtk_box_pack_end (GTK_BOX (vbox), hbuttonbox1, FALSE, FALSE, 0);
	
	addbutton = gtk_button_new_with_label ("Add");
	gtk_widget_show (addbutton);
	gtk_container_add (GTK_CONTAINER (hbuttonbox1), addbutton);
	GTK_WIDGET_SET_FLAGS (addbutton, GTK_CAN_DEFAULT);

	gtk_object_set_data(GTK_OBJECT(addbutton),"window",window);

	changebutton = gtk_button_new_with_label ("Change");
	gtk_widget_show (changebutton);
	gtk_container_add (GTK_CONTAINER (hbuttonbox1), changebutton);
	GTK_WIDGET_SET_FLAGS (changebutton, GTK_CAN_DEFAULT);

	gtk_object_set_data(GTK_OBJECT(changebutton),"window",window);

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

	if (attriblist) {
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
                	row = gtk_clist_append(GTK_CLIST(clist),text);
			gtk_clist_set_row_data (GTK_CLIST(clist),row,attriblist[i]);
                	i++;
        	}
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

	gtk_widget_show (window);
	o_attrib_free_returned(attriblist);
	w_current->mawindow=window;

        gtk_window_position(GTK_WINDOW (window), GTK_WIN_POS_MOUSE);

	gtk_signal_connect(GTK_OBJECT (window), "destroy",
				GTK_SIGNAL_FUNC(destroy_window),
				&w_current->mawindow);

	gtk_signal_connect(GTK_OBJECT(val_entry), "activate",
			GTK_SIGNAL_FUNC(multi_attrib_edit_change),w_current);
	gtk_signal_connect(GTK_OBJECT(addbutton),"clicked",
                        GTK_SIGNAL_FUNC(multi_attrib_edit_add),w_current);
        gtk_signal_connect(GTK_OBJECT(changebutton),"clicked",
                        GTK_SIGNAL_FUNC(multi_attrib_edit_change),w_current);
        gtk_signal_connect(GTK_OBJECT(clearbutton),"clicked",
                        GTK_SIGNAL_FUNC(multi_attrib_edit_clear),window);
        gtk_signal_connect(GTK_OBJECT(delbutton),"clicked",
                        GTK_SIGNAL_FUNC(multi_attrib_edit_delete),w_current);
        gtk_signal_connect(GTK_OBJECT(closebutton),"clicked",
                        GTK_SIGNAL_FUNC(multi_attrib_edit_close),w_current);

	gtk_signal_connect(GTK_OBJECT(clist),"select-row",
			GTK_SIGNAL_FUNC(multi_attrib_edit_select_row),w_current);

	multi_attrib_edit_clear(NULL,GTK_WINDOW(window));
        gtk_grab_add(w_current->mawindow);

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
	OBJECT *attribptr;
	OBJECT *object;
	int vis,show;

        w_current->event_state = SELECT;
        i_update_status(w_current, "Select Mode");

	val_entry = gtk_object_get_data(GTK_OBJECT(w_current->aewindow),"val_entry");
	lab_entry = gtk_object_get_data(GTK_OBJECT(w_current->aewindow),"lab_entry");
	visbutton = gtk_object_get_data(GTK_OBJECT(w_current->aewindow),"visbutton");
	showvalbutton = gtk_object_get_data(GTK_OBJECT(w_current->aewindow),"showvalbutton");
	showlabbutton = gtk_object_get_data(GTK_OBJECT(w_current->aewindow),"showlabbutton");
	showboth = gtk_object_get_data(GTK_OBJECT(w_current->aewindow),"showboth");

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

	attribptr = gtk_object_get_data(GTK_OBJECT(w_current->aewindow),"attrib");
	if(!attribptr) {
	        int world_x, world_y;
		OBJECT *new;

		object = o_select_return_first_object(w_current);
		new=o_attrib_add_attrib(w_current, newtext, vis, show, object);
		SCREENtoWORLD(w_current, mouse_x, mouse_y, &world_x, &world_y);
		new->x=world_x;
		new->y=world_y;
                o_text_erase(w_current, new);
                o_text_recreate(w_current, new);
                o_text_draw(w_current, new);
	} else {
		o_text_change(w_current,attribptr,newtext,vis,show);
	}
        gtk_grab_remove(w_current->aewindow);
        gtk_widget_destroy(w_current->aewindow);
	w_current->aewindow = NULL;
	free(newtext);
}

void
attrib_edit_dialog_cancel(GtkWidget *w, TOPLEVEL *w_current)
{
	i_update_status(w_current, "Select Mode");
        w_current->event_state = SELECT;
        gtk_grab_remove(w_current->aewindow);
        gtk_widget_destroy(w_current->aewindow);
        w_current->aewindow = NULL;
}

void
attrib_edit_dialog_delete(GtkWidget *w, TOPLEVEL *w_current)
{
	OBJECT *object;

	/* for now unselect everything, but in the future you really ought 
	 * to just unselect a single object */
	o_unselect_all(w_current);

	object = gtk_object_get_data(GTK_OBJECT(w_current->aewindow),"attrib");
	o_delete_text(w_current, object);

	i_update_status(w_current, "Select Mode");
        w_current->event_state = SELECT;
        gtk_grab_remove(w_current->aewindow);
        gtk_widget_destroy(w_current->aewindow);
        w_current->aewindow = NULL;
}

void
attrib_edit_dialog (TOPLEVEL *w_current, OBJECT *list)
{
	int i;
	char *string=NULL;
	char name[1000], val[1000];
	OBJECT *attrib;
	GtkWidget *aewindow=NULL;
	GtkWidget *vbox2=NULL;
	GtkWidget *table1=NULL;
	GtkWidget *hbox7=NULL;
	GtkWidget *label12=NULL;
	GtkWidget *combo3=NULL;
	GList *combo3_items = NULL;
	GtkWidget *lab_entry=NULL;
	GtkWidget *hbox8=NULL;
	GtkWidget *label13=NULL;
	GtkWidget *val_entry=NULL;
	GtkWidget *visbutton=NULL;
	GtkWidget *hbox10=NULL;
	GSList *show_group = NULL;
	GtkWidget *namebutton=NULL;
	GtkWidget *valbutton=NULL;
	GtkWidget *showbothbutton=NULL;
	GtkWidget *hbuttonbox2=NULL;
	GtkWidget *buttondelete=NULL; 
	GtkWidget *buttonok=NULL;
	/* GtkWidget *buttonapply;*/
	GtkWidget *buttoncancel;

	if(w_current->aewindow)return;

	aewindow = gtk_window_new (GTK_WINDOW_TOPLEVEL);
	gtk_window_set_title (GTK_WINDOW (aewindow), "Single Attribute Editor");

	vbox2 = gtk_vbox_new (FALSE, 0);
	gtk_widget_show (vbox2);
	gtk_container_add (GTK_CONTAINER (aewindow), vbox2);

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
	gtk_object_set_data (GTK_OBJECT (aewindow), "lab_entry", lab_entry);
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
	gtk_object_set_data (GTK_OBJECT (aewindow), "val_entry", val_entry);

	gtk_widget_show (val_entry);

	gtk_box_pack_start (GTK_BOX (hbox8), val_entry, TRUE, TRUE, 0);

	visbutton = gtk_check_button_new_with_label ("Visible");
	gtk_object_set_data (GTK_OBJECT (aewindow), "visbutton", visbutton);

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

	namebutton = gtk_radio_button_new_with_label (show_group, "Name");
	gtk_object_set_data(GTK_OBJECT(aewindow),"showlabbutton",namebutton);
	show_group = gtk_radio_button_group (GTK_RADIO_BUTTON (namebutton));
	gtk_widget_show (namebutton);
	gtk_box_pack_start (GTK_BOX (hbox10), namebutton, FALSE, FALSE, 0);

	valbutton = gtk_radio_button_new_with_label (show_group, "Value");
	show_group = gtk_radio_button_group (GTK_RADIO_BUTTON (valbutton));
	gtk_object_set_data(GTK_OBJECT(aewindow),"showvalbutton",valbutton);
	gtk_widget_show (valbutton);
	gtk_box_pack_start (GTK_BOX (hbox10), valbutton, FALSE, FALSE, 0);

	showbothbutton = gtk_radio_button_new_with_label (show_group, "Both");
	show_group = gtk_radio_button_group (GTK_RADIO_BUTTON (showbothbutton));
	gtk_object_set_data(GTK_OBJECT(aewindow),"showboth",showbothbutton);
	gtk_widget_show (showbothbutton);
	gtk_box_pack_start (GTK_BOX (hbox10), showbothbutton, FALSE, FALSE, 0);

	hbuttonbox2 = gtk_hbutton_box_new ();
	gtk_widget_show (hbuttonbox2);
	gtk_box_pack_start (GTK_BOX (vbox2), hbuttonbox2, FALSE, TRUE, 0);

	buttonok = gtk_button_new_with_label ("OK");
	gtk_widget_show (buttonok);
	gtk_container_add (GTK_CONTAINER (hbuttonbox2), buttonok);
	GTK_WIDGET_SET_FLAGS (buttonok, GTK_CAN_DEFAULT);

#if 0 /* highly temp change which Ales will eventually finish up to allow */
      /* this dialog to stay open */ /* out for now */
	buttonapply = gtk_button_new_with_label ("Apply");
	gtk_widget_show (buttonapply);
	gtk_container_add (GTK_CONTAINER (hbuttonbox2), buttonapply);
	GTK_WIDGET_SET_FLAGS (buttonapply, GTK_CAN_DEFAULT);
#endif

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
		attrib=list;
		if(attrib->visibility == VISIBLE)
			gtk_toggle_button_set_active 
				(GTK_TOGGLE_BUTTON (visbutton), TRUE);

		if(attrib->show_name_value == SHOW_NAME)
			gtk_toggle_button_set_active 
				(GTK_TOGGLE_BUTTON(namebutton),TRUE);
		else if(attrib->show_name_value == SHOW_VALUE)
			gtk_toggle_button_set_active 
				(GTK_TOGGLE_BUTTON(valbutton),TRUE);
		else gtk_toggle_button_set_active 
			(GTK_TOGGLE_BUTTON(showbothbutton),TRUE);
	}
	else
	{
	        OBJECT *object;

		attrib=NULL;
		name[0]=0;
		val[0]=0;

	        if (object = o_select_return_first_object(w_current)) { 
		  if (object->type==OBJ_NET)
		    strcpy(name,"label");
		}

		gtk_toggle_button_set_active 
			(GTK_TOGGLE_BUTTON (visbutton), TRUE);
		gtk_toggle_button_set_active 
			(GTK_TOGGLE_BUTTON(valbutton),TRUE);
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

	gtk_object_set_data(GTK_OBJECT(aewindow),"attrib",attrib);

	gtk_entry_set_text (GTK_ENTRY (val_entry), val);
	gtk_entry_set_text (GTK_ENTRY (lab_entry), name);

	w_current->aewindow=aewindow;

	gtk_window_position(GTK_WINDOW (aewindow), GTK_WIN_POS_MOUSE);

	gtk_signal_connect(GTK_OBJECT (aewindow), "destroy",
                                   GTK_SIGNAL_FUNC(destroy_window),
                                   &w_current->aewindow);

	gtk_signal_connect(GTK_OBJECT(buttonok),"clicked",
			GTK_SIGNAL_FUNC(attrib_edit_dialog_ok),w_current);
	gtk_signal_connect(GTK_OBJECT(buttoncancel),"clicked",
			GTK_SIGNAL_FUNC(attrib_edit_dialog_cancel),w_current);

	gtk_signal_connect(GTK_OBJECT(val_entry),"activate",
			GTK_SIGNAL_FUNC(attrib_edit_dialog_ok),w_current);
	if(list)
		gtk_signal_connect(GTK_OBJECT(buttondelete),"clicked",
			GTK_SIGNAL_FUNC(attrib_edit_dialog_delete),w_current);

	gtk_widget_show(aewindow);

        gtk_grab_add(w_current->aewindow);
	gtk_widget_grab_focus(val_entry);
}


/***************** End of Attrib Edit dialog box *********************/

/***************** Start of Text Edit dialog box *********************/
gint
change_alignment(GtkWidget *w, TOPLEVEL *w_current)
{
	char *alignment;
	alignment = gtk_object_get_data(GTK_OBJECT(w),"alignment");
	w_current->text_alignment = atoi(alignment);
	
	return(0);
}

static GtkWidget*
create_menu_alignment (TOPLEVEL *w_current)
{
	GtkWidget *menu;
	GtkWidget *menuitem;
	GSList *group;
	char buf[100];

	menu = gtk_menu_new ();
	group = NULL;

	sprintf (buf, "Lower Left");
	menuitem = gtk_radio_menu_item_new_with_label (group, buf);
	group = gtk_radio_menu_item_group (GTK_RADIO_MENU_ITEM (menuitem));
	gtk_menu_append (GTK_MENU (menu), menuitem);
	gtk_object_set_data (GTK_OBJECT(menuitem), "alignment", "0");
	gtk_signal_connect(GTK_OBJECT (menuitem), "activate",
			   (GtkSignalFunc) change_alignment,
			   w_current);
	gtk_widget_show (menuitem);

	sprintf (buf, "Middle Left");
	menuitem = gtk_radio_menu_item_new_with_label (group, buf);
	group = gtk_radio_menu_item_group (GTK_RADIO_MENU_ITEM (menuitem));
	gtk_menu_append (GTK_MENU (menu), menuitem);
	gtk_object_set_data (GTK_OBJECT(menuitem), "alignment", "1");
	gtk_signal_connect(GTK_OBJECT (menuitem), "activate",
			   (GtkSignalFunc) change_alignment,
			   w_current);
	gtk_widget_show (menuitem);

	sprintf (buf, "Upper Left");
	menuitem = gtk_radio_menu_item_new_with_label (group, buf);
	group = gtk_radio_menu_item_group (GTK_RADIO_MENU_ITEM (menuitem));
	gtk_menu_append (GTK_MENU (menu), menuitem);
	gtk_object_set_data (GTK_OBJECT(menuitem), "alignment", "2");
	gtk_signal_connect(GTK_OBJECT (menuitem), "activate",
			   (GtkSignalFunc) change_alignment,
			   w_current);
	gtk_widget_show (menuitem);

	sprintf (buf, "Lower Middle");
	menuitem = gtk_radio_menu_item_new_with_label (group, buf);
	group = gtk_radio_menu_item_group (GTK_RADIO_MENU_ITEM (menuitem));
	gtk_menu_append (GTK_MENU (menu), menuitem);
	gtk_object_set_data (GTK_OBJECT(menuitem), "alignment", "3");
	gtk_signal_connect(GTK_OBJECT (menuitem), "activate",
			   (GtkSignalFunc) change_alignment,
			   w_current);
	gtk_widget_show (menuitem);

	sprintf (buf, "Middle Middle");
	menuitem = gtk_radio_menu_item_new_with_label (group, buf);
	group = gtk_radio_menu_item_group (GTK_RADIO_MENU_ITEM (menuitem));
	gtk_menu_append (GTK_MENU (menu), menuitem);
	gtk_object_set_data (GTK_OBJECT(menuitem), "alignment", "4");
	gtk_signal_connect(GTK_OBJECT (menuitem), "activate",
			   (GtkSignalFunc) change_alignment,
			   w_current);
	gtk_widget_show (menuitem);

	sprintf (buf, "Upper Middle");
	menuitem = gtk_radio_menu_item_new_with_label (group, buf);
	group = gtk_radio_menu_item_group (GTK_RADIO_MENU_ITEM (menuitem));
	gtk_menu_append (GTK_MENU (menu), menuitem);
	gtk_object_set_data (GTK_OBJECT(menuitem), "alignment", "5");
	gtk_signal_connect(GTK_OBJECT (menuitem), "activate",
			   (GtkSignalFunc) change_alignment,
			   w_current);
	gtk_widget_show (menuitem);

	sprintf (buf, "Lower Right");
	menuitem = gtk_radio_menu_item_new_with_label (group, buf);
	group = gtk_radio_menu_item_group (GTK_RADIO_MENU_ITEM (menuitem));
	gtk_menu_append (GTK_MENU (menu), menuitem);
	gtk_object_set_data (GTK_OBJECT(menuitem), "alignment", "6");
	gtk_signal_connect(GTK_OBJECT (menuitem), "activate",
			   (GtkSignalFunc) change_alignment,
			   w_current);
	gtk_widget_show (menuitem);

	sprintf (buf, "Middle Right");
	menuitem = gtk_radio_menu_item_new_with_label (group, buf);
	group = gtk_radio_menu_item_group (GTK_RADIO_MENU_ITEM (menuitem));
	gtk_menu_append (GTK_MENU (menu), menuitem);
	gtk_object_set_data (GTK_OBJECT(menuitem), "alignment", "7");
	gtk_signal_connect(GTK_OBJECT (menuitem), "activate",
			   (GtkSignalFunc) change_alignment,
			   w_current);
	gtk_widget_show (menuitem);

	sprintf (buf, "Upper Right");
	menuitem = gtk_radio_menu_item_new_with_label (group, buf);
	group = gtk_radio_menu_item_group (GTK_RADIO_MENU_ITEM (menuitem));
	gtk_menu_append (GTK_MENU (menu), menuitem);
	gtk_object_set_data (GTK_OBJECT(menuitem), "alignment", "8");
	gtk_signal_connect(GTK_OBJECT (menuitem), "activate",
			   (GtkSignalFunc) change_alignment,
			   w_current);
	gtk_widget_show (menuitem);

	return(menu);
}


void
text_edit_dialog_ok(GtkWidget *w, TOPLEVEL *w_current)
{
	int len;
	int text_size;
	char *text_string = NULL;
	char *text_size_string = NULL;
	int new_text_alignment;	

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

		new_text_alignment = w_current->text_alignment;

		if (len < 80) {
			o_text_edit_end(w_current, text_string,
					 len, text_size, new_text_alignment);
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
text_edit_dialog (TOPLEVEL *w_current, char *string, int text_size,
		  int text_alignment)
{
	GtkWidget *label = NULL;
	GtkWidget *buttonok     = NULL;
	GtkWidget *buttoncancel = NULL;
	GtkWidget *vbox, *action_area;
	GtkWidget *optionmenu = NULL;
	GtkWidget *align_menu = NULL;
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

		
		label = gtk_label_new ("Edit Text Alignment");
		gtk_box_pack_start(
			GTK_BOX(vbox),
			label, TRUE, TRUE, 5);
      		gtk_widget_show (label);

		optionmenu = gtk_option_menu_new ();
		align_menu = create_menu_alignment (w_current);
		gtk_option_menu_set_menu(GTK_OPTION_MENU(optionmenu),
					 align_menu);
		gtk_option_menu_set_history(GTK_OPTION_MENU (optionmenu), 
					    text_alignment);
		w_current->text_alignment = text_alignment;
		gtk_box_pack_start(GTK_BOX(vbox), optionmenu, TRUE, TRUE, 0);
										                gtk_widget_show(optionmenu);


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

	string = gtk_entry_get_text(GTK_ENTRY(w_current->seentry));

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

	gtk_grab_remove(w_current->sewindow);
	gtk_widget_destroy(w_current->sewindow);
	w_current->sewindow = NULL;
}

void
slot_edit_dialog_cancel(GtkWidget *w, TOPLEVEL *w_current)
{
	i_update_status(w_current, "Select Mode");
	w_current->event_state = SELECT;
	gtk_grab_remove(w_current->sewindow);
	gtk_widget_destroy(w_current->sewindow);
	w_current->sewindow = NULL;
}

void
slot_edit_dialog (TOPLEVEL *w_current, char *string)
{
	GtkWidget *label = NULL;
	GtkWidget *buttonok = NULL;
	GtkWidget *buttoncancel = NULL;
	GtkWidget *vbox, *action_area;
	int len;

	if (!w_current->sewindow) {
		w_current->sewindow = x_create_dialog_box(&vbox, &action_area);

		gtk_window_position(GTK_WINDOW(w_current->sewindow),
				    GTK_WIN_POS_MOUSE);

		gtk_signal_connect(GTK_OBJECT (w_current->sewindow),
				   "destroy", GTK_SIGNAL_FUNC(destroy_window),
				   &w_current->sewindow);

#if 0 /* removed because it was causing the dialog box to not close */
      		gtk_signal_connect(GTK_OBJECT (w_current->sewindow),
				   "delete_event",
				   GTK_SIGNAL_FUNC(destroy_window),
				   &w_current->sewindow);
#endif

		gtk_window_set_title(GTK_WINDOW (w_current->sewindow),
				     "Edit slot number");
                gtk_container_border_width(
			GTK_CONTAINER(w_current->sewindow), 0);

		label = gtk_label_new ("Edit slot number");
		gtk_box_pack_start(
			GTK_BOX (vbox),
			label, TRUE, TRUE, 0);
      		gtk_widget_show (label);

		w_current->seentry = gtk_entry_new();
      		gtk_editable_select_region(
			GTK_EDITABLE (w_current->seentry), 0, -1);
		gtk_box_pack_start(
			GTK_BOX(vbox),
			w_current->seentry, TRUE, TRUE, 10);

		gtk_signal_connect(GTK_OBJECT(w_current->seentry), "activate",
                       	           GTK_SIGNAL_FUNC(slot_edit_dialog_ok),
                       	           w_current);
      		gtk_widget_show (w_current->seentry);
		gtk_widget_grab_focus(w_current->seentry);

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

  	if (!GTK_WIDGET_VISIBLE (w_current->sewindow)) {
		gtk_widget_show (w_current->sewindow);

		if (string != NULL) {
			len = strlen(string);
			gtk_entry_set_text(GTK_ENTRY(w_current->seentry),
					   string);
			gtk_entry_select_region(GTK_ENTRY(w_current->seentry),
						strlen("slot="), len);
		}
		gtk_grab_add (w_current->sewindow);
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
	SELECTION *s_current = NULL;
	OBJECT *object = NULL;

	/* skip over head */
	s_current = w_current->page_current->selection2_head->next;

	while(s_current != NULL) {

		object = s_current->selected_object;
		if (object == NULL) {
			fprintf(stderr, "ERROR: NULL object in color_edit_dialog_apply!\n");
			exit(-1);
		}

		switch(object->type) {
			case(OBJ_LINE):
			case(OBJ_BOX):
			case(OBJ_CIRCLE):
			case(OBJ_NET):
			case(OBJ_BUS):
			case(OBJ_PIN):
			case(OBJ_ARC):
				object->saved_color = w_current->edit_color;
				w_current->page_current->CHANGED = 1;
				break;

			case(OBJ_TEXT):
				object->saved_color = w_current->edit_color;	
				o_complex_set_saved_color_only(object->complex,
					            w_current->edit_color);
				w_current->page_current->CHANGED = 1;
				break;
		}

		s_current = s_current->next;
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

/*********** Start of misc support functions for dialog boxes ************/

extern GtkWidget *stwindow;

x_dialog_raise_all(TOPLEVEL *w_current)
{
	if (stwindow) {
		gdk_window_raise(stwindow->window);
	}
        if(w_current->fowindow) {
		gdk_window_raise(w_current->fowindow->window);
	}
        if(w_current->fswindow) {
		gdk_window_raise(w_current->fswindow->window);
	}
        if(w_current->sowindow) {
		gdk_window_raise(w_current->sowindow->window);
	}
        if(w_current->aswindow) {
		gdk_window_raise(w_current->aswindow->window);
	}
        if(w_current->cswindow) {
		gdk_window_raise(w_current->cswindow->window);
	}
        if(w_current->fileselect[FILESELECT].xfwindow) {
		gdk_window_raise(w_current->fileselect[FILESELECT].xfwindow->window);
	}
        if(w_current->fileselect[COMPSELECT].xfwindow) {
		gdk_window_raise(w_current->fileselect[COMPSELECT].xfwindow->window);
	}
        if(w_current->pwindow) {
		gdk_window_raise(w_current->pwindow->window);
	}
        if(w_current->iwindow) {
		gdk_window_raise(w_current->iwindow->window);
	}
        if(w_current->pswindow) {
		gdk_window_raise(w_current->pswindow->window);
	}
        if(w_current->tiwindow) {
		gdk_window_raise(w_current->tiwindow->window);
	}
        if(w_current->tewindow) {
		gdk_window_raise(w_current->tewindow->window);
	}
        if(w_current->sewindow) {
		gdk_window_raise(w_current->sewindow->window);
	}
        if(w_current->exwindow) {
		gdk_window_raise(w_current->exwindow->window);
	}
        if(w_current->aawindow) {
		gdk_window_raise(w_current->aawindow->window);
	}
        if(w_current->mawindow) {
		gdk_window_raise(w_current->mawindow->window);
	}
        if(w_current->aewindow) {
		gdk_window_raise(w_current->aewindow->window);
	}
        if(w_current->trwindow) {
		gdk_window_raise(w_current->trwindow->window);
	}
        if(w_current->tswindow) {
		gdk_window_raise(w_current->tswindow->window);
	}
        if(w_current->abwindow) {
		gdk_window_raise(w_current->abwindow->window);
	}
        if(w_current->hkwindow) {
		gdk_window_raise(w_current->hkwindow->window);
	}
        if(w_current->cowindow) {
		gdk_window_raise(w_current->cowindow->window);
	}
        if(w_current->clwindow) {
		gdk_window_raise(w_current->clwindow->window);
	}

}
/*********** End of misc support functions for dialog boxes ************/

