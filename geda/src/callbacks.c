#ifdef HAVE_CONFIG_H
#include <config.h>
#endif
#include <gtk/gtk.h>
#include "callbacks.h"
#include "filesel.h"
#include "interface.h"
#include "support.h"
#include "tool.h"



gboolean
on_MainWindow_delete_event             (GtkWidget       *pWidget,
                                        GdkEvent        *event,
                                        gpointer         pUserData)
{
	MenuProjectExit_Activation(pWidget, pUserData);
}

gboolean
FlyingVPressed                         (GtkWidget       *widget,
                                        GdkEventButton  *event,
                                        gpointer         user_data)
{
	GdkModifierType ModType;
	int x, y;
	
	gdk_window_get_pointer(pWindowMain, &x, &y, &ModType);
	printf("FlyingVPressed() > x=%d y=%d\n", x, y);
	return FALSE;
}


gboolean
FlyingVReleased                        (GtkWidget       *widget,
                                        GdkEventButton  *event,
                                        gpointer         user_data)
{
	FlyingVPressed(widget, event, user_data);
	
  return FALSE;
}


gboolean
FlyingHPressed                         (GtkWidget       *widget,
                                        GdkEventButton  *event,
                                        gpointer         user_data)
{
	FlyingVPressed(widget, event, user_data);

  return FALSE;
}


gboolean
FlyingHReleased                        (GtkWidget       *widget,
                                        GdkEventButton  *event,
                                        gpointer         user_data)
{
	FlyingVPressed(widget, event, user_data);

  return FALSE;
}



/*
	Project Window pseudo-rulers handling
*/

static gint xStart, yStart;



void on_button1_pressed(GtkButton *button, gpointer user_data)
{
	GdkWindow *pGdkWindow = NULL;
	GdkModifierType GdkMask;
	
	gdk_window_get_pointer(pGdkWindow, &xStart, &yStart, &GdkMask);
}



void on_button1_released(GtkButton *button, gpointer user_data)
{
	GdkWindow *pGdkWindow = NULL;
	GdkModifierType GdkMask;
	GtkWidget *pWidget;
	gint x, y;
	
	gdk_window_get_pointer(pGdkWindow, &x, &y, &GdkMask);
	
	pWidget = lookup_widget(GTK_WIDGET(button), "ProjectArea");
	if (pWidget == NULL)
		return;
	gtk_widget_set_usize(pWidget, pWidget->allocation.width + (x - xStart), pWidget->allocation.height);
}



void on_button2_pressed(GtkButton *button, gpointer user_data)
{
	GdkWindow *pGdkWindow = NULL;
	GdkModifierType GdkMask;
	
	gdk_window_get_pointer(pGdkWindow, &xStart, &yStart, &GdkMask);
}



void on_button2_released(GtkButton *button, gpointer user_data)
{
	GdkWindow *pGdkWindow = NULL;
	GdkModifierType GdkMask;
	GtkWidget *pWidget;
	gint x, y;
	
	gdk_window_get_pointer(pGdkWindow, &x, &y, &GdkMask);
	
	pWidget = lookup_widget(GTK_WIDGET(button), "StatusArea");
	if (pWidget == NULL)
		return;
	gtk_widget_set_usize(pWidget, pWidget->allocation.width, pWidget->allocation.height - (y - yStart));
}
