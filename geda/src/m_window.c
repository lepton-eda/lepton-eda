/*******************************************************************************/
/*                                                                             */
/* gEDA Suite Project Manager                                                  */
/*                                                                             */
/* Copyright (C) 2002 Piotr Miarecki, sp9rve@eter.ariadna.pl                   */
/*                                                                             */
/* This program is free software; you can redistribute it and/or               */
/* modify it under the terms of the GNU General Public License                 */
/* as published by the Free Software Foundation version 2.                     */
/*                                                                             */
/* This program is distributed in the hope that it will be useful,             */
/* but WITHOUT ANY WARRANTY; without even the implied warranty of              */
/* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the               */
/* GNU General Public License for more details.                                */
/*                                                                             */
/* You should have received a copy of the GNU General Public License           */
/* along with this program; if not, write to the Free Software                 */
/* Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA. */
/*                                                                             */
/*******************************************************************************/

#ifdef HAVE_CONFIG_H
#include "../config.h"
#endif
#include <gtk/gtk.h>
#include "global.h"
#include "m_window.h"
#include "support.h"
#include "window.h"



static GtkMenu *pMenu;
static void MenuWindowActivation(GtkMenuItem *pMenuItem, gpointer pUserData);



void MenuWindowInitialize(void)
{
	GtkWidget *pWidget;
	
	/* look for menu Window */
	pWidget = lookup_widget(GTK_WIDGET(pWindowMain), "MenuWindow");
	if (pWidget == NULL)
	{
		/* TODO: error handling */
		return;
	}
	/* create Window menu */
	pMenu = GTK_MENU(gtk_menu_new());
	gtk_widget_ref(GTK_WIDGET(pMenu));
	gtk_object_set_data_full(GTK_OBJECT(pWindowMain), "MenuWindow_menu", pMenu, (GtkDestroyNotify) gtk_widget_unref);
	gtk_menu_item_set_submenu(GTK_MENU_ITEM (pWidget), GTK_WIDGET(pMenu));
}



GtkMenuItem *MenuWindowNew(const char *szName)
{
	GtkMenuItem *pMenuItem;

	pMenuItem = GTK_MENU_ITEM(gtk_menu_item_new_with_label(szName));
	gtk_widget_ref(GTK_WIDGET(pMenuItem));
	gtk_widget_show(GTK_WIDGET(pMenuItem));
	gtk_container_add(GTK_CONTAINER(pMenu), GTK_WIDGET(pMenuItem));
	gtk_signal_connect(
		GTK_OBJECT(pMenuItem),
		"activate",
		GTK_SIGNAL_FUNC(MenuWindowActivation),
		(char *) szName
		);
	gtk_widget_set_sensitive(GTK_WIDGET(pMenuItem), TRUE);

	return pMenuItem;
}



void MenuWindowDelete(GtkMenuItem *pMenuItem)
{
	gtk_container_remove(GTK_CONTAINER(pMenu), GTK_WIDGET(pMenuItem));
	gtk_widget_destroy(GTK_WIDGET(pMenuItem));
}



static void MenuWindowActivation(GtkMenuItem *pMenuItem, gpointer pUserData)
{
	gmanager_window_select((char *) pUserData);

#if 0
	DocGetProperty(DOC_SELECTED, NULL, szFileName);
	szName = FileGetName(szFileName);
	szExt = FileGetExt(szFileName);
	szPath = FileGetDir(szFileName);
	szRel = FileGetRel(szFileName);

	/* look for an action */
	for (pAction = pActionList; pAction != NULL; pAction = pAction->pNext)
		if (pAction->pMenuItem == pMenuItem)
			break;
	if (pAction == NULL)
	{
		/* TODO*/
		return;
	}

	/* interprete command line */
	strcpy(szCommand, pAction->szCommand);

	/* run the tool */
	pParams[0] = (void *) pAction;
	pParams[1] = (void *) &szFileName;
	iResult = TaskNew(TASK_ACTION, (void **) pParams);
	if (iResult != SUCCESS)
	{
		/**/
		return;
	}

	return;
#endif
}
