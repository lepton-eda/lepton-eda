#include <config.h>
#include <stdio.h>

#include <libgeda/libgeda.h>

#include "../include/x_states.h"
#include "../include/prototype.h"


#ifdef HAS_LIBGTKEXTRA

#include <gtkextra/gtksheet.h>

void
//multi_multi_edit_close(GtkWidget *w,TOPLEVEL *w_current)
multi_multi_edit_close(GtkWidget *w,GtkWidget *window)
{
//	i_update_status(w_current, "Select Mode");
//      w_current->event_state = SELECT;
//      gtk_grab_remove(w_current->mawindow);
        gtk_widget_destroy(window);
//      w_current->mawindow = NULL;
	
}

void
multi_multi_edit(TOPLEVEL *w_current, OBJECT *list)
{
	gint type;
	OBJECT *current;
	GPtrArray *components;

	GtkSheet *sheet;
	GtkWidget *window;
	GtkWidget *vbox;
	GtkWidget *scrolledwindow;
	GtkWidget *buttonbox;
	GtkWidget *okbutton;
	GtkWidget *cancelbutton;

	int rowcount = 1;
	components = g_ptr_array_new();
	type = list->type;
	
	g_ptr_array_add(components,list);

	current=list->next;
	while(current)
	{
		if(current->type == type)
		{
			g_ptr_array_add(components,current);
			rowcount++;
		}
		current=current->next;
	}

//	attriblist=o_attrib_return_attribs(w_current->page_current->object_head,
//		w_current->page_current->selection_head->next);
//	o_attrib_get_name_value(attriblist[i]->text_string,
//		text[0],text[2]);
                        
	
	window = gtk_window_new(GTK_WINDOW_TOPLEVEL);

	vbox = gtk_vbox_new(FALSE,3);
	gtk_container_add(GTK_CONTAINER(window),vbox);
	gtk_widget_show(vbox);
	scrolledwindow = gtk_scrolled_window_new(NULL,NULL);
	gtk_box_pack_start(GTK_BOX(vbox),scrolledwindow,TRUE,TRUE,0);
	gtk_widget_show(scrolledwindow);
	
	buttonbox = gtk_hbutton_box_new();
	gtk_box_pack_end(GTK_BOX(vbox),buttonbox,FALSE,FALSE,3);
	gtk_widget_show(buttonbox);

	okbutton = gtk_button_new_with_label("Ok");
	gtk_container_add(GTK_CONTAINER(buttonbox),okbutton);
	gtk_widget_show(okbutton);
	cancelbutton = gtk_button_new_with_label("Cancel");
	gtk_container_add(GTK_CONTAINER(buttonbox),cancelbutton);
	gtk_widget_show(cancelbutton);

	sheet=GTK_SHEET(gtk_sheet_new(rowcount,5,"Test Sheet"));
	gtk_container_add(GTK_CONTAINER(scrolledwindow),GTK_WIDGET(sheet));
	gtk_widget_show(GTK_WIDGET(sheet));

        gtk_window_set_title (GTK_WINDOW (window), "Edit Component");
        gtk_widget_set_usize(window, 645, 400);

        gtk_window_position(GTK_WINDOW (window), GTK_WIN_POS_MOUSE);

	gtk_widget_show(window);

        gtk_signal_connect(GTK_OBJECT (window), "destroy",
                                GTK_SIGNAL_FUNC(destroy_window),
                                window);

	gtk_signal_connect(GTK_OBJECT(okbutton), "clicked",
				GTK_SIGNAL_FUNC(multi_multi_edit_close),
				window);

	gtk_signal_connect(GTK_OBJECT(cancelbutton), "clicked",
				GTK_SIGNAL_FUNC(multi_multi_edit_close),
				window);
/*
 gtk_signal_connect(GTK_OBJECT(sheet),
                    "key_press_event",
                    (GtkSignalFunc) clipboard_handler,
                    NULL);

 gtk_signal_connect(GTK_OBJECT(sheet),
                    "resize_range",
                    (GtkSignalFunc) resize_handler,
                    NULL);

 gtk_signal_connect(GTK_OBJECT(sheet),
                    "move_range",
                    (GtkSignalFunc) move_handler,
                    NULL);

 gtk_signal_connect(GTK_OBJECT(sheet),
                    "changed",
                    (GtkSignalFunc) alarm_change,
                    NULL);

 gtk_signal_connect(GTK_OBJECT(sheet),
                    "activate",
                    (GtkSignalFunc) alarm_activate,
                    NULL);
 gtk_signal_connect(GTK_OBJECT(sheet),
                    "deactivate",
                    (GtkSignalFunc) alarm_deactivate,
                    NULL);

 gtk_signal_connect(GTK_OBJECT(sheet),
                    "traverse",
                    (GtkSignalFunc) alarm_traverse,
                    NULL);
*/


}

#endif
