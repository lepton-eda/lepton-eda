#include <config.h>
#include <stdio.h>

#include <libgeda/libgeda.h>

#include "../include/x_states.h"
#include "../include/prototype.h"


#ifdef HAS_LIBGTKEXTRA

#include <gtkextra/gtksheet.h>
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

	printf("Multi-Multi\n");

	components = g_ptr_array_new();
	type = list->type;
	
	g_ptr_array_add(components,list);

	current=list->next;
	while(current)
	{
		if(current->type == type)
			g_ptr_array_add(components,current);
		current=current->next;
	}

	window = gtk_window_new(GTK_WINDOW_TOPLEVEL);

	vbox = gtk_vbox_new(FALSE,3);
	gtk_container_add(GTK_CONTAINER(window),vbox);
	gtk_widget_show(vbox);

	scrolledwindow = gtk_scrolled_window_new(NULL,NULL);
	gtk_box_pack_start(vbox,scrolledwindow,TRUE,FALSE,0);
	gtk_widget_show(scrolledwindow);
	
	buttonbox = gtk_hbutton_box_new();
	gtk_box_pack_end(vbox,buttonbox,TRUE,FALSE,0);
	gtk_widget_show(buttonbox);

	gtk_button_new_with_label("Ok");
	gtk_box_pack_start(buttonbox,okbutton,TRUE,FALSE,0);
	gtk_widget_show(okbutton);
	gtk_button_new_with_label("Cancel");
	gtk_box_pack_end(buttonbox,cancelbutton,TRUE,FALSE,0);
	gtk_widget_show(cancelbutton);

	sheet=gtk_sheet_new(10,5,"Test Sheet");
	gtk_container_add(GTK_CONTAINER(scrolledwindow),sheet);
	gtk_widget_show(sheet);

	gtk_widget_show(window);

        gtk_signal_connect(GTK_OBJECT (window), "destroy",
                                GTK_SIGNAL_FUNC(destroy_window),
                                window);

}

void
multi_multi_edit_close(GtkWidget *w,TOPLEVEL *w_current)
{
        i_update_status(w_current, "Select Mode");
        w_current->event_state = SELECT;
//        gtk_grab_remove(w_current->mawindow);
        gtk_widget_destroy(w);
//        w_current->mawindow = NULL;
	
}
#endif
