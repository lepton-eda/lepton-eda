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
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#include <config.h>

#include <stdio.h>
#include <stdlib.h>
#ifdef HAVE_STRINGS_H
#include <strings.h>
#endif
#include <gtk/gtk.h>
#include <gdk/gdk.h>
#include <gdk/gdkx.h>

#include <guile/gh.h>

#ifdef HAS_LIBGD
#include <gd/gd.h>
#endif


#include <libgeda/struct.h>   
#include <libgeda/defines.h>
#include <libgeda/colors.h>   
#include <libgeda/globals.h>
#include <libgeda/o_types.h>
#include <libgeda/prototype.h>

#include "../include/i_vars.h"
#include "../include/x_states.h"
#include "../include/prototype.h"


/***************** Start of Text Input dialog box *********************/
void
text_input_dialog_apply(GtkWidget *w, TOPLEVEL *w_current)
{
	int len;
	char *string=NULL;

	string = gtk_entry_get_text(GTK_ENTRY(w_current->tientry));	

	if (string[0] != '\0' ) {
		len = strlen(string);
#if DEBUG
		printf("text was: _%s_ %d\n", string, len);
#endif
		/* length is hack */
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
			gtk_entry_select_region(GTK_ENTRY(w_current->tientry), 0, len);
			gtk_widget_grab_focus(w_current->tientry);

		} else {
			/* hack you should have limits */
			fprintf(stderr, "String too long... hack!\n");
		}

		w_current->event_state = DRAWTEXT;
		w_current->inside_action = 1;

	} 


	/*gtk_grab_remove(w_current->tiwindow);*/
/*   	gtk_widget_destroy(w_current->tiwindow);
	w_current->tiwindow=NULL;*/
}

void
text_input_dialog_close(GtkWidget *w, TOPLEVEL *w_current) 
{
	i_update_status(w_current, "Select Mode");
	w_current->event_state = SELECT;
	/*gtk_grab_remove(w_current->tiwindow);*/
	gtk_widget_destroy(w_current->tiwindow);
	w_current->tiwindow=NULL;
}

void
text_input_dialog (TOPLEVEL *w_current)
{
	GtkWidget *label=NULL;
	GtkWidget *buttonok=NULL;
	GtkWidget *buttoncancel=NULL;

	if (!w_current->tiwindow)
	{
		w_current->tiwindow = gtk_dialog_new ();
/*		gtk_window_position (GTK_WINDOW (w_current->tiwindow), 
				     GTK_WIN_POS_MOUSE);*/
		
		gtk_widget_set_usize (w_current->tiwindow, 400,130);

		 gtk_window_position (GTK_WINDOW (w_current->tiwindow), GTK_WIN_POS_NONE);


		gtk_signal_connect (GTK_OBJECT (w_current->tiwindow), 
				    "destroy", GTK_SIGNAL_FUNC(destroy_window),
                          	    &w_current->tiwindow);

      		gtk_signal_connect (GTK_OBJECT (w_current->tiwindow), 
				    "delete_event",
                          	    GTK_SIGNAL_FUNC(destroy_window),
                          	    &w_current->tiwindow);    

		gtk_window_set_title (GTK_WINDOW (w_current->tiwindow), 
				      "Enter Text");
                gtk_container_border_width (GTK_CONTAINER (w_current->
				            tiwindow), 0);       

		label = gtk_label_new ("Enter Text");
		gtk_box_pack_start (GTK_BOX (GTK_DIALOG (w_current->
				    tiwindow)->vbox), label, TRUE, TRUE, 0);
      		gtk_widget_show (label);

		w_current->tientry = gtk_entry_new_with_max_length (79);
		gtk_signal_connect(GTK_OBJECT(w_current->tientry), "activate",
                       		   GTK_SIGNAL_FUNC(text_input_dialog_apply),
                                   w_current);
    		gtk_box_pack_start (GTK_BOX (GTK_DIALOG (w_current->
				    tiwindow)->vbox), w_current->tientry, 
				    TRUE, TRUE, 10);
      		gtk_editable_select_region (GTK_EDITABLE (w_current->tientry),
					    0, -1);
    		gtk_widget_show (w_current->tientry);                
		gtk_widget_grab_focus(w_current->tientry);

		buttonok = gtk_button_new_with_label ("Apply");   
		GTK_WIDGET_SET_FLAGS (buttonok, GTK_CAN_DEFAULT);
		gtk_box_pack_start (GTK_BOX (GTK_DIALOG(w_current->
				    tiwindow)->action_area),
                          	    buttonok, TRUE, TRUE, 0);
      		gtk_signal_connect (GTK_OBJECT (buttonok), "clicked",
			            GTK_SIGNAL_FUNC(text_input_dialog_apply), 
				    w_current);    
      		gtk_widget_show (buttonok); 
		gtk_widget_grab_default (buttonok);

		buttoncancel = gtk_button_new_with_label ("Close");   
		GTK_WIDGET_SET_FLAGS (buttoncancel, GTK_CAN_DEFAULT);
		gtk_box_pack_start (GTK_BOX (GTK_DIALOG(w_current->
				    tiwindow)->action_area),
                          	    buttoncancel, TRUE, TRUE, 0);
      		gtk_signal_connect (GTK_OBJECT (buttoncancel), "clicked",
			  	    GTK_SIGNAL_FUNC(text_input_dialog_close), 
				    w_current);    
      		gtk_widget_show (buttoncancel); 
	}

  	if (!GTK_WIDGET_VISIBLE (w_current->tiwindow)) {
		gtk_widget_show (w_current->tiwindow);
		/*gtk_grab_add (w_current->tiwindow);*/
	} else {
	 	gdk_window_raise(w_current->tiwindow->window);
	}
}
/***************** End of Text Input dialog box *********************/


/***************** Start of Text Edit dialog box *********************/
void
text_edit_dialog_ok(GtkWidget *w, TOPLEVEL *w_current)
{
	int len;
	int text_size;
	char *text_string=NULL;
	char *text_size_string=NULL;

	text_string = gtk_entry_get_text(GTK_ENTRY(w_current->teentry));	
	text_size_string = gtk_entry_get_text(GTK_ENTRY(w_current->tsentry));	

	if ((text_string[0] != '\0') && (text_string[0] != '\0')) {
		len = strlen(text_string);

#if DEBUG
		printf("text was: _%s_ %d\n", string, len);
#endif
	
		text_size = atoi(text_size_string);
#if DEBUG 
		printf("text size was: _%s_ %d\n", text_size_string, text_size);
#endif

		if (text_size == 0) {
			text_size = default_text_size;
		}

		if (len < 80) {
			o_ntext_edit_end(w_current, text_string, len, text_size);
		} else {
			/* hack you should NOT have limits */
			fprintf(stderr, "String too long... hack!\n");
		}
	} 

	w_current->event_state = SELECT;
	i_update_status(w_current, "Select Mode");

	gtk_grab_remove(w_current->tewindow);
	gtk_widget_destroy(w_current->tewindow);
	w_current->tewindow=NULL;

}

void
text_edit_dialog_cancel(GtkWidget *w, TOPLEVEL *w_current)
{
	i_update_status(w_current, "Select Mode");
	w_current->event_state = SELECT;
	gtk_grab_remove(w_current->tewindow);
	gtk_widget_destroy(w_current->tewindow);
	w_current->tewindow=NULL;

}

void
text_edit_dialog (TOPLEVEL *w_current, char *string, int text_size)
{
	GtkWidget *label=NULL;
	GtkWidget *buttonok=NULL;
	GtkWidget *buttoncancel=NULL;
	char text_size_string[10];
	int len;

	if (!w_current->tewindow)
	{
		w_current->tewindow = gtk_dialog_new ();
		gtk_window_position (GTK_WINDOW (w_current->tewindow), 
				     GTK_WIN_POS_MOUSE);

		gtk_signal_connect (GTK_OBJECT (w_current->tewindow), 
				    "destroy", GTK_SIGNAL_FUNC(destroy_window),
                          	    &w_current->tewindow);

      		gtk_signal_connect (GTK_OBJECT (w_current->tewindow), 
				    "delete_event", 
				    GTK_SIGNAL_FUNC(destroy_window),
                          	    &w_current->tewindow);    

		gtk_window_set_title (GTK_WINDOW (w_current->tewindow), 
				      "Edit Text");
                gtk_container_border_width (GTK_CONTAINER (
				      w_current->tewindow), 0);       

		label = gtk_label_new ("Edit Text");
		gtk_box_pack_start (GTK_BOX (GTK_DIALOG (w_current->
				    tewindow)->vbox), label, TRUE, TRUE, 0);
      		gtk_widget_show (label);

		w_current->teentry = gtk_entry_new();
      		gtk_editable_select_region (GTK_EDITABLE (w_current->teentry), 0, -1);
		gtk_box_pack_start (GTK_BOX (GTK_DIALOG (
				    w_current->tewindow)->vbox),
                          	    w_current->teentry, TRUE, TRUE, 10);

		gtk_signal_connect(GTK_OBJECT(w_current->teentry), "activate",
                       	           GTK_SIGNAL_FUNC(text_edit_dialog_ok),
                       	           w_current); 
      		gtk_widget_show (w_current->teentry);
		gtk_widget_grab_focus(w_current->teentry);

		label = gtk_label_new ("Edit Text Size");
		gtk_box_pack_start (GTK_BOX (GTK_DIALOG (w_current->
				    tewindow)->vbox), label, TRUE, TRUE, 0);
      		gtk_widget_show (label);

		w_current->tsentry = gtk_entry_new_with_max_length (10);
      		gtk_editable_select_region (GTK_EDITABLE (w_current->tsentry), 0, -1);
		gtk_box_pack_start (GTK_BOX (GTK_DIALOG (w_current->tewindow)->vbox),
                          w_current->tsentry, FALSE, FALSE, 5);
		gtk_signal_connect(GTK_OBJECT(w_current->tsentry), "activate",
                       GTK_SIGNAL_FUNC(text_edit_dialog_ok),
                       w_current); 
      		gtk_widget_show (w_current->tsentry);

		buttonok = gtk_button_new_with_label ("OK");   
		GTK_WIDGET_SET_FLAGS (buttonok, GTK_CAN_DEFAULT);
		gtk_box_pack_start (GTK_BOX ( GTK_DIALOG(w_current->
				    tewindow)->action_area),
                          	    buttonok, TRUE, TRUE, 0);
      		gtk_signal_connect (GTK_OBJECT (buttonok), "clicked",
			            GTK_SIGNAL_FUNC(text_edit_dialog_ok), 
				    w_current);    
      		gtk_widget_show (buttonok); 
		gtk_widget_grab_default (buttonok);

		buttoncancel = gtk_button_new_with_label ("Cancel");   
		GTK_WIDGET_SET_FLAGS (buttoncancel, GTK_CAN_DEFAULT);
		gtk_box_pack_start (GTK_BOX (
				    GTK_DIALOG(w_current->tewindow)->
				    action_area), buttoncancel, TRUE, TRUE, 0);
      		gtk_signal_connect (GTK_OBJECT (buttoncancel), "clicked",
			            GTK_SIGNAL_FUNC(text_edit_dialog_cancel), 
				    w_current);    
      		gtk_widget_show (buttoncancel); 

	}

  	if (!GTK_WIDGET_VISIBLE (w_current->tewindow)) {
		gtk_widget_show (w_current->tewindow);
		if (string != NULL) {
			len = strlen(string);
			gtk_entry_set_text(GTK_ENTRY(w_current->teentry), string);
			gtk_entry_select_region(GTK_ENTRY(w_current->teentry), 0, len);
		}

	        sprintf(text_size_string, "%d", text_size);
                gtk_entry_set_text(GTK_ENTRY(w_current->tsentry), text_size_string);

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
	w_current->exwindow=NULL;

	/* go through and change ALL changed flags to 0 */	
	/* w_current->page_current->CHANGED=0;*/
	s_page_clear_changed(w_current->page_head);
#if GTK_DEVEL
	i_callback_file_close(w_current, 0, NULL);
#else
	i_callback_file_close(NULL,w_current);
#endif
}

void
exit_dialog_cancel(GtkWidget *w, TOPLEVEL *w_current)
{
	gtk_grab_remove(w_current->exwindow);
	gtk_widget_destroy(w_current->exwindow);
	w_current->exwindow=NULL;

	/* leave this one */
	w_current->event_state = SELECT;
}

void
exit_dialog (TOPLEVEL *w_current)
{
	GtkWidget *label=NULL;
	GtkWidget *button=NULL;

	if (!w_current->exwindow)
	{
		w_current->exwindow = gtk_dialog_new ();
		gtk_window_position (GTK_WINDOW (w_current->exwindow), GTK_WIN_POS_MOUSE);

		gtk_signal_connect (GTK_OBJECT (w_current->exwindow), "destroy",
                          GTK_SIGNAL_FUNC(destroy_window),
                          &w_current->exwindow);

      		gtk_signal_connect (GTK_OBJECT (w_current->exwindow), "delete_event",
                          GTK_SIGNAL_FUNC(destroy_window),
                          &w_current->exwindow);    

		gtk_window_set_title (GTK_WINDOW (w_current->exwindow), "Discard Changes?");
                gtk_container_border_width (GTK_CONTAINER (w_current->exwindow), 0);       

		button = gtk_button_new_with_label ("OK");   
		GTK_WIDGET_SET_FLAGS (button, GTK_CAN_DEFAULT);
		gtk_box_pack_start (GTK_BOX (
				GTK_DIALOG(w_current->exwindow)->action_area),
                          	button, TRUE, TRUE, 0);
      		gtk_signal_connect (GTK_OBJECT (button), "clicked",
			  GTK_SIGNAL_FUNC(exit_dialog_ok), w_current);    
      		gtk_widget_show (button); 

		button = gtk_button_new_with_label ("Cancel");   
		GTK_WIDGET_SET_FLAGS (button, GTK_CAN_DEFAULT);
		gtk_box_pack_start (GTK_BOX (
				GTK_DIALOG(w_current->exwindow)->action_area),
                          	button, TRUE, TRUE, 0);
      		gtk_signal_connect (GTK_OBJECT (button), "clicked",
			  GTK_SIGNAL_FUNC(exit_dialog_cancel), w_current);    
      		gtk_widget_show (button); 
		gtk_widget_grab_default (button);

		label = gtk_label_new ("There are schematics which are unsaved!\n\nAre you sure?\nOK will discard ALL changes!");
	        gtk_misc_set_padding (GTK_MISC (label), 10, 10);
		gtk_box_pack_start (GTK_BOX (GTK_DIALOG (w_current->exwindow)->vbox),
                          label, TRUE, TRUE, 10);
      		gtk_widget_show (label);

	}

  	if (!GTK_WIDGET_VISIBLE (w_current->exwindow)) {
		gtk_widget_show (w_current->exwindow);
		gtk_grab_add (w_current->exwindow);
	}
}
/***************** End of Exit dialog box *********************/


/***************** Start of Arc dialog box *********************/
void
arc_angles_dialog_ok(GtkWidget *w, TOPLEVEL *w_current)
{
	char *string_start=NULL;
	char *string_sweep=NULL;

	string_start = gtk_entry_get_text(GTK_ENTRY(w_current->aaentry_start));	

	string_sweep = gtk_entry_get_text(GTK_ENTRY(w_current->aaentry_sweep));	

	if ( (string_start[0] != '\0') && (string_sweep[0] != '\0') ) {

		/* no error detection hack */
		o_arc_end2(w_current, atoi(string_start), atoi(string_sweep));
	} 

	/*gtk_grab_remove(w_current->aawindow);*/
	gtk_widget_destroy(w_current->aawindow);
	w_current->aawindow=NULL;
}

void
arc_angles_dialog_cancel(GtkWidget *w, TOPLEVEL *w_current)
{
	/*gtk_grab_remove(w_current->aawindow);*/
	gtk_widget_destroy(w_current->aawindow);
	w_current->aawindow=NULL;

	w_current->event_state = DRAWARC;
}

void
arc_angle_dialog (TOPLEVEL *w_current)
{
	GtkWidget *label=NULL;
	GtkWidget *buttonok=NULL;
	GtkWidget *buttoncancel=NULL;

	if (!w_current->aawindow)
	{
		w_current->aawindow = gtk_dialog_new ();
		gtk_window_position (GTK_WINDOW (w_current->aawindow), GTK_WIN_POS_MOUSE);

		gtk_signal_connect (GTK_OBJECT (w_current->aawindow), "destroy",
                          GTK_SIGNAL_FUNC(destroy_window),
                          &w_current->aawindow);

      		gtk_signal_connect (GTK_OBJECT (w_current->aawindow), "delete_event",
                          GTK_SIGNAL_FUNC(destroy_window),
                          &w_current->aawindow);    

		gtk_window_set_title (GTK_WINDOW (w_current->aawindow), "Arc Params");
                gtk_container_border_width (GTK_CONTAINER (w_current->aawindow), 0);       

		label = gtk_label_new ("Start Angle");
		gtk_box_pack_start (GTK_BOX (GTK_DIALOG (w_current->aawindow)->vbox),
                          label, TRUE, TRUE, 0);
      		gtk_widget_show (label);

		w_current->aaentry_start = gtk_entry_new_with_max_length (4);
      		gtk_editable_select_region (GTK_EDITABLE (w_current->aaentry_start), 0, -1);
		gtk_box_pack_start (GTK_BOX (GTK_DIALOG (w_current->aawindow)->vbox),
                          w_current->aaentry_start, FALSE, FALSE, 5);
      		gtk_widget_show (w_current->aaentry_start);
		gtk_widget_grab_focus(w_current->aaentry_start);

		label = gtk_label_new ("Degrees of Sweep");
		gtk_box_pack_start (GTK_BOX (GTK_DIALOG (w_current->aawindow)->vbox),
                          label, TRUE, TRUE, 0);
      		gtk_widget_show (label);

		w_current->aaentry_sweep = gtk_entry_new_with_max_length (4);
      		gtk_editable_select_region (GTK_EDITABLE (w_current->aaentry_sweep), 0, -1);
		gtk_box_pack_start (GTK_BOX (GTK_DIALOG (w_current->aawindow)->vbox),
                          w_current->aaentry_sweep, FALSE, FALSE, 5);
		gtk_signal_connect(GTK_OBJECT(w_current->aaentry_sweep), "activate",
                       GTK_SIGNAL_FUNC(arc_angles_dialog_ok),
                       w_current); 
      		gtk_widget_show (w_current->aaentry_sweep);


		buttonok = gtk_button_new_with_label ("OK");   
		GTK_WIDGET_SET_FLAGS (buttonok, GTK_CAN_DEFAULT);
		gtk_box_pack_start (GTK_BOX (
				GTK_DIALOG(w_current->aawindow)->action_area),
                          	buttonok, TRUE, TRUE, 0);
      		gtk_signal_connect (GTK_OBJECT (buttonok), "clicked",
			  GTK_SIGNAL_FUNC(arc_angles_dialog_ok), w_current);    
      		gtk_widget_show (buttonok); 
		gtk_widget_grab_default (buttonok);

		buttoncancel = gtk_button_new_with_label ("Cancel");   
		GTK_WIDGET_SET_FLAGS (buttoncancel, GTK_CAN_DEFAULT);
		gtk_box_pack_start (GTK_BOX (
				GTK_DIALOG(w_current->aawindow)->action_area),
                          	buttoncancel, TRUE, TRUE, 0);
      		gtk_signal_connect (GTK_OBJECT (buttoncancel), "clicked",
			  GTK_SIGNAL_FUNC(arc_angles_dialog_cancel), w_current);    
      		gtk_widget_show (buttoncancel); 

	}

  	if (!GTK_WIDGET_VISIBLE (w_current->aawindow)) {
		gtk_widget_show (w_current->aawindow);
		/*gtk_grab_add (w_current->aawindow);*/
	}
}
/***************** End of Arc dialog box *********************/


/***************** Start of Translate dialog box *********************/
void
translate_dialog_ok(GtkWidget *w, TOPLEVEL *w_current)
{
	char *string=NULL;

	string = gtk_entry_get_text(GTK_ENTRY(w_current->trentry));	

	if ( (string[0] != '\0') ) {

		/* no error detection hack */
		/* zero offset has a special meaning... */
		o_complex_translate_all(w_current, atoi(string));
	} 

/*	gtk_grab_remove(w_current->trwindow);*/
	gtk_widget_destroy(w_current->trwindow);
	w_current->trwindow=NULL;
}

void
translate_dialog_cancel(GtkWidget *w, TOPLEVEL *w_current)
{
	i_update_status(w_current, "Select Mode");
	w_current->event_state = SELECT;
	/*gtk_grab_remove(w_current->trwindow);*/
	gtk_widget_destroy(w_current->trwindow);
	w_current->trwindow=NULL;
}

void
translate_dialog (TOPLEVEL *w_current)
{
	GtkWidget *label=NULL;
	GtkWidget *buttonok=NULL;
	GtkWidget *buttoncancel=NULL;

	if (!w_current->trwindow) {
		w_current->trwindow = gtk_dialog_new ();
		gtk_window_position (GTK_WINDOW (w_current->trwindow), 
				     GTK_WIN_POS_MOUSE);

		gtk_signal_connect (GTK_OBJECT (w_current->trwindow), 
			            "destroy",
                                    GTK_SIGNAL_FUNC(destroy_window),
                                    &w_current->trwindow);

      		gtk_signal_connect (GTK_OBJECT (w_current->trwindow), 
				    "delete_event",
                          	    GTK_SIGNAL_FUNC(destroy_window),
                                    &w_current->trwindow);    

		gtk_window_set_title (GTK_WINDOW (w_current->trwindow), 
				      "Translate");
                gtk_container_border_width (GTK_CONTAINER (
					w_current->trwindow), 0);       

		label = gtk_label_new ("Offset to translate?\n(0 for origin)");
	        gtk_misc_set_padding (GTK_MISC (label), 10, 10);
		gtk_box_pack_start (GTK_BOX (GTK_DIALOG (w_current->
				    trwindow)->vbox),
                          	    label, TRUE, TRUE, 0);
      		gtk_widget_show (label);

		w_current->trentry = gtk_entry_new_with_max_length (10);
      		gtk_editable_select_region (GTK_EDITABLE (w_current->trentry), 0, -1);
		gtk_box_pack_start (GTK_BOX (GTK_DIALOG (w_current->
				    trwindow)->vbox),
                                    w_current->trentry, FALSE, FALSE, 5);
		gtk_signal_connect(GTK_OBJECT(w_current->trentry), "activate",
                       GTK_SIGNAL_FUNC(translate_dialog_ok),
                       w_current); 
      		gtk_widget_show (w_current->trentry);
		gtk_widget_grab_focus(w_current->trentry);

		buttonok = gtk_button_new_with_label ("OK");   
		GTK_WIDGET_SET_FLAGS (buttonok, GTK_CAN_DEFAULT);
		gtk_box_pack_start (GTK_BOX (
				GTK_DIALOG(w_current->trwindow)->
				action_area),
                          	buttonok, TRUE, TRUE, 0);
      		gtk_signal_connect (GTK_OBJECT (buttonok), "clicked",
			  GTK_SIGNAL_FUNC(translate_dialog_ok), w_current);    
      		gtk_widget_show (buttonok); 
		gtk_widget_grab_default (buttonok);

		buttoncancel = gtk_button_new_with_label ("Cancel");   
		GTK_WIDGET_SET_FLAGS (buttoncancel, GTK_CAN_DEFAULT);
		gtk_box_pack_start (GTK_BOX (
				GTK_DIALOG(w_current->trwindow)->
				action_area),
                          	buttoncancel, TRUE, TRUE, 0);
      		gtk_signal_connect (GTK_OBJECT (buttoncancel), "clicked",
			  GTK_SIGNAL_FUNC(translate_dialog_cancel), 
			  w_current);    
      		gtk_widget_show (buttoncancel); 

	}

  	if (!GTK_WIDGET_VISIBLE (w_current->trwindow)) {
		gtk_widget_show (w_current->trwindow);
		/*gtk_grab_add (w_current->trwindow);*/
	}
}
/***************** End of Translate dialog box *********************/


/***************** Start of Text size dialog box *********************/
void
text_size_dialog_ok(GtkWidget *w, TOPLEVEL *w_current)
{
	char *string=NULL;
	int size;

	string = gtk_entry_get_text(GTK_ENTRY(w_current->tsentry));	

	if ( (string[0] != '\0') ) {
		size = atoi(string);
		if (size) {
			w_current->text_size = size;
		} 
	} 

	gtk_grab_remove(w_current->tswindow);
	gtk_widget_destroy(w_current->tswindow);
	w_current->tswindow=NULL;
}

void
text_size_dialog_cancel(GtkWidget *w, TOPLEVEL *w_current)
{
	i_update_status(w_current, "Select Mode");
	w_current->event_state = SELECT;
	gtk_grab_remove(w_current->tswindow);
	gtk_widget_destroy(w_current->tswindow);
	w_current->tswindow=NULL;
}

void
text_size_dialog (TOPLEVEL *w_current)
{
	char string[10];
	int len;
	GtkWidget *label=NULL;
	GtkWidget *buttonok=NULL;
	GtkWidget *buttoncancel=NULL;

	if (!w_current->tswindow) {
		w_current->tswindow = gtk_dialog_new ();
		gtk_window_position (GTK_WINDOW (w_current->tswindow), GTK_WIN_POS_MOUSE);

		gtk_signal_connect (GTK_OBJECT (w_current->tswindow), "destroy",
                          GTK_SIGNAL_FUNC(destroy_window),
                          &w_current->tswindow);

      		gtk_signal_connect (GTK_OBJECT (w_current->tswindow), "delete_event",
                          GTK_SIGNAL_FUNC(destroy_window),
                          &w_current->tswindow);    

		gtk_window_set_title (GTK_WINDOW (w_current->tswindow), "Text Size");
                gtk_container_border_width (GTK_CONTAINER (w_current->tswindow), 0);       

		label = gtk_label_new ("Enter new text size");
	        gtk_misc_set_padding (GTK_MISC (label), 10, 10);
		gtk_box_pack_start (GTK_BOX (GTK_DIALOG (w_current->tswindow)->vbox),
                          label, TRUE, TRUE, 0);
      		gtk_widget_show (label);

		w_current->tsentry = gtk_entry_new_with_max_length (10);
      		gtk_editable_select_region (GTK_EDITABLE (w_current->tsentry), 0, -1);
		gtk_box_pack_start (GTK_BOX (GTK_DIALOG (w_current->tswindow)->vbox),
                          w_current->tsentry, FALSE, FALSE, 5);
		gtk_signal_connect(GTK_OBJECT(w_current->tsentry), "activate",
                       GTK_SIGNAL_FUNC(text_size_dialog_ok),
                       w_current); 
      		gtk_widget_show (w_current->tsentry);
		gtk_widget_grab_focus(w_current->tsentry);

		buttonok = gtk_button_new_with_label ("OK");   
		GTK_WIDGET_SET_FLAGS (buttonok, GTK_CAN_DEFAULT);
		gtk_box_pack_start (GTK_BOX (
				GTK_DIALOG(w_current->tswindow)->action_area),
                          	buttonok, TRUE, TRUE, 0);
      		gtk_signal_connect (GTK_OBJECT (buttonok), "clicked",
			  GTK_SIGNAL_FUNC(text_size_dialog_ok), w_current);    
      		gtk_widget_show (buttonok); 
		gtk_widget_grab_default (buttonok);

		buttoncancel = gtk_button_new_with_label ("Cancel");   
		GTK_WIDGET_SET_FLAGS (buttoncancel, GTK_CAN_DEFAULT);
		gtk_box_pack_start (GTK_BOX (
				GTK_DIALOG(w_current->tswindow)->action_area),
                          	buttoncancel, TRUE, TRUE, 0);
      		gtk_signal_connect (GTK_OBJECT (buttoncancel), "clicked",
			  GTK_SIGNAL_FUNC(text_size_dialog_cancel), w_current);    
      		gtk_widget_show (buttoncancel); 

	}

  	if (!GTK_WIDGET_VISIBLE (w_current->tswindow)) {
		sprintf(string, "%d", w_current->text_size);
		len = strlen(string);
		gtk_entry_set_text(GTK_ENTRY(w_current->tsentry), string);
		gtk_entry_select_region(GTK_ENTRY(w_current->tsentry), 0, len);
		gtk_widget_show (w_current->tswindow);
		gtk_grab_add (w_current->tswindow);
	}
}
/***************** End of Text size dialog box *********************/


/***************** Start of Snap size dialog box *********************/
void
snap_size_dialog_ok(GtkWidget *w, TOPLEVEL *w_current)
{
	char *string=NULL;
	int size;

	string = gtk_entry_get_text(GTK_ENTRY(w_current->tsentry));	

	if ( (string[0] != '\0') ) {
		size = atoi(string);
		if (size) {
			w_current->snap_size = size;
		} 
	} 

	gtk_grab_remove(w_current->tswindow);
	gtk_widget_destroy(w_current->tswindow);
	w_current->tswindow=NULL;
}

void
snap_size_dialog_cancel(GtkWidget *w, TOPLEVEL *w_current)
{
	i_update_status(w_current, "Select Mode");
	w_current->event_state = SELECT;
	gtk_grab_remove(w_current->tswindow);
	gtk_widget_destroy(w_current->tswindow);
	w_current->tswindow=NULL;
}

void
snap_size_dialog (TOPLEVEL *w_current)
{
	char string[10];
	int len;
	GtkWidget *label=NULL;
	GtkWidget *buttonok=NULL;
	GtkWidget *buttoncancel=NULL;

	if (!w_current->tswindow) {
		w_current->tswindow = gtk_dialog_new ();
		gtk_window_position (GTK_WINDOW (w_current->tswindow), GTK_WIN_POS_MOUSE);

		gtk_signal_connect (GTK_OBJECT (w_current->tswindow), "destroy",
                          GTK_SIGNAL_FUNC(destroy_window),
                          &w_current->tswindow);

      		gtk_signal_connect (GTK_OBJECT (w_current->tswindow), "delete_event",
                          GTK_SIGNAL_FUNC(destroy_window),
                          &w_current->tswindow);    

		gtk_window_set_title (GTK_WINDOW (w_current->tswindow), "Snap Grid");
                gtk_container_border_width (GTK_CONTAINER (w_current->tswindow), 0);       

		label = gtk_label_new ("Enter new snap grid spacing");
	        gtk_misc_set_padding (GTK_MISC (label), 10, 10);
		gtk_box_pack_start (GTK_BOX (GTK_DIALOG (w_current->tswindow)->vbox),
                          label, TRUE, TRUE, 0);
      		gtk_widget_show (label);

		w_current->tsentry = gtk_entry_new_with_max_length (10);
      		gtk_editable_select_region (GTK_EDITABLE (w_current->tsentry), 0, -1);
		gtk_box_pack_start (GTK_BOX (GTK_DIALOG (w_current->tswindow)->vbox),
                          w_current->tsentry, FALSE, FALSE, 5);
		gtk_signal_connect(GTK_OBJECT(w_current->tsentry), "activate",
                       GTK_SIGNAL_FUNC(snap_size_dialog_ok),
                       w_current); 
      		gtk_widget_show (w_current->tsentry);
		gtk_widget_grab_focus(w_current->tsentry);

		buttonok = gtk_button_new_with_label ("OK");   
		GTK_WIDGET_SET_FLAGS (buttonok, GTK_CAN_DEFAULT);
		gtk_box_pack_start (GTK_BOX (
				GTK_DIALOG(w_current->tswindow)->action_area),
                          	buttonok, TRUE, TRUE, 0);
      		gtk_signal_connect (GTK_OBJECT (buttonok), "clicked",
			  GTK_SIGNAL_FUNC(snap_size_dialog_ok), w_current);    
      		gtk_widget_show (buttonok); 
		gtk_widget_grab_default (buttonok);

		buttoncancel = gtk_button_new_with_label ("Cancel");   
		GTK_WIDGET_SET_FLAGS (buttoncancel, GTK_CAN_DEFAULT);
		gtk_box_pack_start (GTK_BOX (
				GTK_DIALOG(w_current->tswindow)->action_area),
                          	buttoncancel, TRUE, TRUE, 0);
      		gtk_signal_connect (GTK_OBJECT (buttoncancel), "clicked",
			  GTK_SIGNAL_FUNC(snap_size_dialog_cancel), w_current);    
      		gtk_widget_show (buttoncancel); 

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
	char *string=NULL;

	string = gtk_entry_get_text(GTK_ENTRY(w_current->teentry));	

	if (string[0] != '\0') {
		len = strlen(string);

#if DEBUG
		printf("text was: _%s_ %d\n", string, len);
#endif
	
		if (len < 80) {
			o_slot_end(w_current, string, len);
		} else {
			/* hack you should NOT have limits */
			fprintf(stderr, "String too long... hack!\n");
		}
	} 

	w_current->event_state = SELECT;
	i_update_status(w_current, "Select Mode");

	gtk_grab_remove(w_current->tewindow);
	gtk_widget_destroy(w_current->tewindow);
	w_current->tewindow=NULL;

}

void
slot_edit_dialog_cancel(GtkWidget *w, TOPLEVEL *w_current)
{
	i_update_status(w_current, "Select Mode");
	w_current->event_state = SELECT;
	gtk_grab_remove(w_current->tewindow);
	gtk_widget_destroy(w_current->tewindow);
	w_current->tewindow=NULL;

}

void
slot_edit_dialog (TOPLEVEL *w_current, char *string)
{
	GtkWidget *label=NULL;
	GtkWidget *buttonok=NULL;
	GtkWidget *buttoncancel=NULL;
	int len;

	if (!w_current->tewindow)
	{
		w_current->tewindow = gtk_dialog_new ();
		gtk_window_position (GTK_WINDOW (w_current->tewindow), 
				     GTK_WIN_POS_MOUSE);

		gtk_signal_connect (GTK_OBJECT (w_current->tewindow), 
				    "destroy", GTK_SIGNAL_FUNC(destroy_window),
                          	    &w_current->tewindow);

      		gtk_signal_connect (GTK_OBJECT (w_current->tewindow), 
				    "delete_event", 
				    GTK_SIGNAL_FUNC(destroy_window),
                          	    &w_current->tewindow);    

		gtk_window_set_title (GTK_WINDOW (w_current->tewindow), 
				      "Edit slot number");
                gtk_container_border_width (GTK_CONTAINER (
				      w_current->tewindow), 0);       

		label = gtk_label_new ("Edit slot number");
		gtk_box_pack_start (GTK_BOX (GTK_DIALOG (w_current->
				    tewindow)->vbox), label, TRUE, TRUE, 0);
      		gtk_widget_show (label);

		w_current->teentry = gtk_entry_new();
      		gtk_editable_select_region (GTK_EDITABLE (w_current->teentry), 0, -1);
		gtk_box_pack_start (GTK_BOX (GTK_DIALOG (
				    w_current->tewindow)->vbox),
                          	    w_current->teentry, TRUE, TRUE, 10);

		gtk_signal_connect(GTK_OBJECT(w_current->teentry), "activate",
                       	           GTK_SIGNAL_FUNC(slot_edit_dialog_ok),
                       	           w_current); 
      		gtk_widget_show (w_current->teentry);
		gtk_widget_grab_focus(w_current->teentry);

		buttonok = gtk_button_new_with_label ("OK");   
		GTK_WIDGET_SET_FLAGS (buttonok, GTK_CAN_DEFAULT);
		gtk_box_pack_start (GTK_BOX ( GTK_DIALOG(w_current->
				    tewindow)->action_area),
                          	    buttonok, TRUE, TRUE, 0);
      		gtk_signal_connect (GTK_OBJECT (buttonok), "clicked",
			            GTK_SIGNAL_FUNC(slot_edit_dialog_ok), 
				    w_current);    
      		gtk_widget_show (buttonok); 
		gtk_widget_grab_default (buttonok);

		buttoncancel = gtk_button_new_with_label ("Cancel");   
		GTK_WIDGET_SET_FLAGS (buttoncancel, GTK_CAN_DEFAULT);
		gtk_box_pack_start (GTK_BOX (
				    GTK_DIALOG(w_current->tewindow)->
				    action_area), buttoncancel, TRUE, TRUE, 0);
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
			gtk_entry_select_region(GTK_ENTRY(w_current->teentry), strlen("slot="), len);
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
	w_current->abwindow=NULL;
}

void
about_dialog (TOPLEVEL *w_current)
{
	GtkWidget *label=NULL;
	GtkWidget *buttonclose=NULL;
	char string[100];

	if (!w_current->abwindow)
	{
		w_current->abwindow = gtk_dialog_new ();
		gtk_window_position (GTK_WINDOW (w_current->abwindow), 
				     GTK_WIN_POS_MOUSE);

		gtk_window_set_title (GTK_WINDOW (w_current->abwindow), 
				      "About...");
                gtk_container_border_width (GTK_CONTAINER (
				      w_current->abwindow), 5);       


		gtk_signal_connect (GTK_OBJECT (w_current->abwindow), 
				    "destroy", GTK_SIGNAL_FUNC(destroy_window),
                          	    &w_current->abwindow);

      		gtk_signal_connect (GTK_OBJECT (w_current->abwindow), 
				    "delete_event", 
				    GTK_SIGNAL_FUNC(destroy_window),
                          	    &w_current->abwindow);    

		sprintf(string, "gEDA : GNU Electronic Design Automation");
		label = gtk_label_new (string);
		gtk_box_pack_start (GTK_BOX (GTK_DIALOG (w_current->
				    abwindow)->vbox), label, TRUE, TRUE, 5);
      		gtk_widget_show (label); 

		sprintf(string, "gschem version %s", VERSION);
		label = gtk_label_new (string);
		gtk_box_pack_start (GTK_BOX (GTK_DIALOG (w_current->
				    abwindow)->vbox), label, TRUE, TRUE, 5);
      		gtk_widget_show (label); 

		sprintf(string, "Ales V. Hvezda\nahvezda@geda.seul.org");
		label = gtk_label_new (string);
		gtk_box_pack_start (GTK_BOX (GTK_DIALOG (w_current->
				    abwindow)->vbox), label, TRUE, TRUE, 5);
      		gtk_widget_show (label); 

		buttonclose = gtk_button_new_with_label ("Close");   
		GTK_WIDGET_SET_FLAGS (buttonclose, GTK_CAN_DEFAULT);
		gtk_box_pack_start (GTK_BOX (
				    GTK_DIALOG(w_current->abwindow)->
				    action_area), buttonclose, TRUE, TRUE, 0);
      		gtk_signal_connect (GTK_OBJECT (buttonclose), "clicked",
			            GTK_SIGNAL_FUNC(about_dialog_close), 
				    w_current);    
      		gtk_widget_show (buttonclose); 

	}

  	if (!GTK_WIDGET_VISIBLE (w_current->abwindow)) {
		gtk_widget_show (w_current->abwindow);
	}
}
/***************** End of help/about dialog box *********************/

/***************** Start of coord dialog box *********************/
void
coord_dialog_close(GtkWidget *w, TOPLEVEL *w_current)
{
	gtk_widget_destroy(w_current->cowindow);
	w_current->cowindow=NULL;

}

void
coord_display_update(TOPLEVEL *w_current, int x, int y)
{
	char string[25]; /* this should be big enough */
	int world_x, world_y;

	sprintf(string, "(%d, %d)", x, y);
#ifdef GTK_DEVEL
 	gtk_label_set_text(GTK_LABEL(w_current->coord_screen), string );
#else
 	gtk_label_set(GTK_LABEL(w_current->coord_screen), string );
#endif

	SCREENtoWORLD(w_current, x, y, &world_x, &world_y);

	sprintf(string, "(%d, %d)", world_x, world_y);
#ifdef GTK_DEVEL
 	gtk_label_set_text(GTK_LABEL(w_current->coord_world), string );
#else
 	gtk_label_set(GTK_LABEL(w_current->coord_world), string );
#endif

}

void
coord_dialog (TOPLEVEL *w_current, int x, int y)
{
	GtkWidget *label=NULL;
        GtkWidget *buttonclose=NULL;
        GtkWidget *frame;
        GtkWidget *vbox;

        if (!w_current->cowindow)
        {
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

                gtk_signal_connect (GTK_OBJECT (w_current->cowindow), 
                                    "delete_event", 
                                    GTK_SIGNAL_FUNC(destroy_window),
                                    &w_current->cowindow);    

                vbox = gtk_vbox_new (FALSE, 5);
                gtk_container_add (GTK_CONTAINER (w_current->cowindow), vbox);
                gtk_widget_show(vbox);

                frame = gtk_frame_new ("Screen");      
                w_current->coord_screen = gtk_label_new ("(########, ########)");
                gtk_label_set_justify (GTK_LABEL (w_current->coord_screen), GTK_JUSTIFY_LEFT);
                gtk_misc_set_padding (GTK_MISC (w_current->coord_screen), 10, 10);
                gtk_container_add (GTK_CONTAINER (frame), w_current->coord_screen);
                gtk_box_pack_start (GTK_BOX (vbox), frame, FALSE, FALSE, 0);
                gtk_widget_show(w_current->coord_screen);
                gtk_widget_show(frame);

                frame = gtk_frame_new ("World");      
                w_current->coord_world = gtk_label_new ("(########, ########)");
                gtk_misc_set_padding (GTK_MISC (w_current->coord_world), 10, 10);
                gtk_label_set_justify (GTK_LABEL (w_current->coord_world), GTK_JUSTIFY_LEFT);
                gtk_container_add (GTK_CONTAINER (frame), w_current->coord_world);
                gtk_box_pack_start (GTK_BOX (vbox), frame, FALSE, FALSE, 0);
                gtk_widget_show(w_current->coord_world);
                gtk_widget_show(frame);

		buttonclose = gtk_button_new_with_label ("Close");   
		GTK_WIDGET_SET_FLAGS (buttonclose, GTK_CAN_DEFAULT);
		gtk_box_pack_start (GTK_BOX ( vbox ),
                          	    buttonclose, TRUE, TRUE, 0);
      		gtk_signal_connect (GTK_OBJECT (buttonclose), "clicked",
			            GTK_SIGNAL_FUNC(coord_dialog_close), 
				    w_current);    
      		gtk_widget_show (buttonclose); 
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
/* this is a kludge and will be totally replaced once the new color scheme */
/* is in place */

gint
color_black(GtkWidget *w, TOPLEVEL *w_current)
{
	w_current->edit_color = BLACK;
        return(0);
}

gint
color_white(GtkWidget *w, TOPLEVEL *w_current)
{
	w_current->edit_color = WHITE;
        return(0);
}

gint
color_red(GtkWidget *w, TOPLEVEL *w_current)
{
	w_current->edit_color = RED;
        return(0);
}

gint
color_green(GtkWidget *w, TOPLEVEL *w_current)
{
	w_current->edit_color = GREEN;
        return(0);
}

gint
color_blue(GtkWidget *w, TOPLEVEL *w_current)
{
	w_current->edit_color = BLUE;
        return(0);
}

gint
color_yellow(GtkWidget *w, TOPLEVEL *w_current)
{
	w_current->edit_color = YELLOW;
        return(0);
}

gint
color_cyan(GtkWidget *w, TOPLEVEL *w_current)
{
	w_current->edit_color = CYAN;
        return(0);
}

gint
color_grey(GtkWidget *w, TOPLEVEL *w_current)
{
	w_current->edit_color = GREY;
        return(0);
}



/* this is from gtktest.c */
static GtkWidget*
create_color_menu (TOPLEVEL *w_current)
{
	GtkWidget *menu;
	GtkWidget *menuitem;
	GSList *group;
	char buf[20];

	menu = gtk_menu_new ();
	group = NULL;


	sprintf (buf, "Black");
	menuitem = gtk_radio_menu_item_new_with_label (group, buf);
	group = gtk_radio_menu_item_group (GTK_RADIO_MENU_ITEM (menuitem));
	gtk_menu_append (GTK_MENU (menu), menuitem);
	gtk_signal_connect (GTK_OBJECT (menuitem), "activate",
                              (GtkSignalFunc) color_black,
                              w_current);

	gtk_widget_show (menuitem);

	sprintf (buf, "White"); 
	menuitem = gtk_radio_menu_item_new_with_label (group, buf);
	group = gtk_radio_menu_item_group (GTK_RADIO_MENU_ITEM (menuitem));
	gtk_menu_append (GTK_MENU (menu), menuitem);
	gtk_signal_connect (GTK_OBJECT (menuitem), "activate",
                              (GtkSignalFunc) color_white,
                              w_current);
	gtk_widget_show (menuitem);

	sprintf (buf, "Red"); 
	menuitem = gtk_radio_menu_item_new_with_label (group, buf);
	group = gtk_radio_menu_item_group (GTK_RADIO_MENU_ITEM (menuitem));
	gtk_menu_append (GTK_MENU (menu), menuitem);
	gtk_signal_connect (GTK_OBJECT (menuitem), "activate",
                              (GtkSignalFunc) color_red,
                              w_current);
	gtk_widget_show (menuitem);

	sprintf (buf, "Green"); 
	menuitem = gtk_radio_menu_item_new_with_label (group, buf);
	group = gtk_radio_menu_item_group (GTK_RADIO_MENU_ITEM (menuitem));
	gtk_menu_append (GTK_MENU (menu), menuitem);
	gtk_signal_connect (GTK_OBJECT (menuitem), "activate",
                              (GtkSignalFunc) color_green,
                              w_current);
	gtk_widget_show (menuitem);

	sprintf (buf, "Blue"); 
	menuitem = gtk_radio_menu_item_new_with_label (group, buf);
	group = gtk_radio_menu_item_group (GTK_RADIO_MENU_ITEM (menuitem));
	gtk_menu_append (GTK_MENU (menu), menuitem);
	gtk_signal_connect (GTK_OBJECT (menuitem), "activate",
                              (GtkSignalFunc) color_blue,
                              w_current);
	gtk_widget_show (menuitem);

	sprintf (buf, "Yellow"); 
	menuitem = gtk_radio_menu_item_new_with_label (group, buf);
	group = gtk_radio_menu_item_group (GTK_RADIO_MENU_ITEM (menuitem));
	gtk_menu_append (GTK_MENU (menu), menuitem);
	gtk_signal_connect (GTK_OBJECT (menuitem), "activate",
                              (GtkSignalFunc) color_yellow,
                              w_current);
	gtk_widget_show (menuitem);

	sprintf (buf, "Cyan"); 
	menuitem = gtk_radio_menu_item_new_with_label (group, buf);
	group = gtk_radio_menu_item_group (GTK_RADIO_MENU_ITEM (menuitem));
	gtk_menu_append (GTK_MENU (menu), menuitem);
	gtk_signal_connect (GTK_OBJECT (menuitem), "activate",
                              (GtkSignalFunc) color_cyan,
                              w_current);
	gtk_widget_show (menuitem);

	sprintf (buf, "Grey"); 
	menuitem = gtk_radio_menu_item_new_with_label (group, buf);
	group = gtk_radio_menu_item_group (GTK_RADIO_MENU_ITEM (menuitem));
	gtk_menu_append (GTK_MENU (menu), menuitem);
	gtk_signal_connect (GTK_OBJECT (menuitem), "activate",
                              (GtkSignalFunc) color_grey,
                              w_current);
	gtk_widget_show (menuitem);

	w_current->edit_color = GREEN;

	return menu;
}

void
color_edit_dialog_close(GtkWidget *w, TOPLEVEL *w_current)
{
	gtk_widget_destroy(w_current->clwindow);
	w_current->clwindow=NULL;
}

void
color_edit_dialog_apply(GtkWidget *w, TOPLEVEL *w_current)
{
	OBJECT *o_current=NULL;
	OBJECT *found=NULL;

	o_current = w_current->page_current->selection_head->next;

	while(o_current != NULL) {

		found = (OBJECT *) o_list_search(
					w_current->page_current->object_head, 
					o_current);

		if (found == NULL) {
			fprintf(stderr, "UGGG! you blew it... tried to delete something that didn't exist");
			exit(-1);
		}

		switch(found->type) {
			case(OBJ_LINE):
			case(OBJ_NET):
			case(OBJ_BOX):
			case(OBJ_ARC):
			case(OBJ_CIRCLE):
				found->color = w_current->edit_color;
				w_current->page_current->CHANGED = 1;
			break;

			case(OBJ_NTEXT):
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
	GtkWidget *label=NULL;
        GtkWidget *buttonclose=NULL;
        GtkWidget *buttonapply=NULL;
        GtkWidget *frame;
        GtkWidget *vbox;
	GtkWidget *optionmenu;

        if (!w_current->clwindow)
        {
		w_current->clwindow = gtk_dialog_new ();
                gtk_window_position (GTK_WINDOW (w_current->clwindow), 
                                     GTK_WIN_POS_MOUSE);

                gtk_window_set_title (GTK_WINDOW (w_current->clwindow), 
                                      "Color Edit");
                gtk_container_border_width (GTK_CONTAINER (
                                      w_current->clwindow), 5);       

                gtk_signal_connect (GTK_OBJECT (w_current->clwindow), 
                                    "destroy", GTK_SIGNAL_FUNC(destroy_window),
                                    &w_current->clwindow);

                gtk_signal_connect (GTK_OBJECT (w_current->clwindow), 
                                    "delete_event", 
                                    GTK_SIGNAL_FUNC(destroy_window),
                                    &w_current->clwindow);    


		optionmenu = gtk_option_menu_new ();
                gtk_option_menu_set_menu (GTK_OPTION_MENU (optionmenu), create_color_menu (w_current));
                gtk_option_menu_set_history (GTK_OPTION_MENU (optionmenu), 3);
                gtk_box_pack_start (GTK_BOX (GTK_DIALOG (w_current->clwindow)->vbox), optionmenu, TRUE, TRUE, 0);
                gtk_widget_show (optionmenu);


		buttonapply = gtk_button_new_with_label ("Apply");   
		GTK_WIDGET_SET_FLAGS (buttonapply, GTK_CAN_DEFAULT);
		gtk_box_pack_start (GTK_BOX ( GTK_DIALOG(w_current->
				    clwindow)->action_area),
                          	    buttonapply, TRUE, TRUE, 0);
      		gtk_signal_connect (GTK_OBJECT (buttonapply), "clicked",
			            GTK_SIGNAL_FUNC(color_edit_dialog_apply), 
				    w_current);    
      		gtk_widget_show (buttonapply); 
		gtk_widget_grab_default (buttonapply);

		buttonclose = gtk_button_new_with_label ("Close");   
		gtk_box_pack_start (GTK_BOX ( GTK_DIALOG(w_current->
				    clwindow)->action_area),
                          	    buttonclose, TRUE, TRUE, 0);
      		gtk_signal_connect (GTK_OBJECT (buttonclose), "clicked",
			            GTK_SIGNAL_FUNC(color_edit_dialog_close), 
				    w_current);    
      		gtk_widget_show (buttonclose); 

        }

        if (!GTK_WIDGET_VISIBLE (w_current->clwindow)) {
                gtk_widget_show (w_current->clwindow);
        } else {
		gdk_window_raise(w_current->clwindow->window);
	}
}
/***************** End of color edit dialog box *********************/
