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
	MenuProjectExit_Activation(GTK_MENU_ITEM(pWidget), pUserData);
}
