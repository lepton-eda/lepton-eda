/* $Id$ */

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
#include "config.h"
#endif

#include <gtk/gtk.h>
#include <stdlib.h>

#ifdef HAVE_STRING_H  
#include <string.h>
#endif

#include "global.h"
#include "m_window.h"
#include "support.h"
#include "window.h"



struct Window_s
{
	char *szPath;
	GtkWidget *pWidget;
	GtkLabel *pLabel;
	GtkMenuItem *pMenuItem;
	struct Window_s *pNext;
};
static struct Window_s *pWindowList = NULL;
static GtkNotebook *pNotebook;



void WindowInitialize(void)
{
	GtkHPaned *pHPaned;
	
	pHPaned = GTK_HPANED(lookup_widget(GTK_WIDGET(pWindowMain), "HPaned"));
	
	pNotebook = GTK_NOTEBOOK(gtk_notebook_new());
	gtk_notebook_set_show_tabs(GTK_NOTEBOOK(pNotebook), FALSE);
	gtk_widget_show(GTK_WIDGET(pNotebook));
	gtk_paned_pack2(GTK_PANED(pHPaned), GTK_WIDGET(pNotebook), TRUE, TRUE);
	gtk_container_set_border_width(GTK_CONTAINER(pNotebook), 4);
	gtk_notebook_set_scrollable(GTK_NOTEBOOK(pNotebook), TRUE);
}



GtkWidget *gmanager_window_new(const char *szPath)
{
	struct Window_s *pWindow, *pPtr;

	int i;
	
	pWindow = (struct Window_s *) malloc(sizeof(struct Window_s));
	if (pWindow == NULL)
		return NULL;
	pWindow->pNext = NULL;

	pWindow->szPath = (char *) malloc(strlen(szPath) + 1);
	if (pWindow->szPath == NULL)
		return NULL;
	strcpy(pWindow->szPath, szPath);
	
	pWindow->pWidget = gtk_scrolled_window_new(NULL, NULL);
	gtk_widget_show(GTK_WIDGET(pWindow->pWidget));
	gtk_container_add(GTK_CONTAINER (pNotebook), pWindow->pWidget);
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(pWindow->pWidget), GTK_POLICY_NEVER, GTK_POLICY_NEVER);

	pWindow->pLabel = GTK_LABEL(gtk_label_new(szPath));
	gtk_widget_show(GTK_WIDGET(pWindow->pLabel));
	
	for (i = 0; TRUE; i ++)
		if (gtk_notebook_get_nth_page(pNotebook, i) == pWindow->pWidget)
			break;
	gtk_notebook_set_tab_label(pNotebook, pWindow->pWidget, GTK_WIDGET(pWindow->pLabel));
	gtk_notebook_set_page(pNotebook, i);
		
	if (pWindowList == NULL)
	{
		pWindowList = pWindow;
	}
	else
	{
		for (pPtr = pWindowList; pPtr->pNext != NULL; pPtr = pPtr->pNext)
			;
		pPtr->pNext = pWindow;
	}

	pWindow->pMenuItem = MenuWindowNew(pWindow->szPath);
	
	return pWindow->pWidget;
}



void gmanager_window_remove(GtkWidget *pWidget)
{
	struct Window_s *pWindow, *pPtr;

	for (pWindow = pWindowList; pWindow != NULL; pWindow = pWindow->pNext)
	{
		if (pWindow->pWidget == pWidget)
			break;
	}
	if (pWindow == NULL)
		return;
	
	MenuWindowDelete(pWindow->pMenuItem);
	
	free(pWindow->szPath);

	if (pWindow == pWindowList)
	{
		pWindowList = (pWindow->pNext == NULL)
			? NULL
			: pWindow->pNext;
	}
	else
	{
		for (pPtr = pWindowList; pPtr != NULL; pPtr = pPtr->pNext)
			if (pPtr->pNext == pWindow)
				break;
		pPtr->pNext = pWindow->pNext;
	}

	gtk_container_remove(GTK_CONTAINER (pNotebook), pWindow->pWidget);
	free((void *) pWindow);
}



void gmanager_window_select(const char *szPath)
{
	struct Window_s *pWindow;

	for (pWindow = pWindowList; pWindow != NULL; pWindow = pWindow->pNext)
	{
		if (strcmp(pWindow->szPath, szPath) == 0)
			break;
	}
	if (pWindow == NULL)
		return;

	gtk_notebook_set_page(pNotebook, gtk_notebook_page_num(pNotebook, pWindow->pWidget));
}



char *WindowTop(void)
{
	struct Window_s *pWindow;
	GtkWidget *pWidget;
	static char *szPath = NULL;

	pWidget = gtk_notebook_get_nth_page(pNotebook, gtk_notebook_get_current_page(pNotebook));
	for (pWindow = pWindowList; pWindow != NULL; pWindow = pWindow->pNext)
		if (pWindow->pWidget == pWidget)
			break;
	if (pWindow == NULL)
		return NULL;
	
	if (szPath != NULL)
		free((void *) szPath);
	szPath = (char *) malloc(strlen(pWindow->szPath) + 1);
	if (szPath == NULL)
		return NULL;
	strcpy(szPath, pWindow->szPath);

	return szPath;
}



void WindowSwitch(GtkNotebook *pNotebook, GtkNotebookPage *pPage, gint iPage, gpointer pUserData)
{
	struct Window_s *pWindow;
	GtkWidget *pWidget;
	
	pWidget = gtk_notebook_get_nth_page(pNotebook, iPage);
	
	for (pWindow = pWindowList; pWindow != NULL; pWindow = pWindow->pNext)
	{
		if (pWindow->pWidget == pWidget)
			break;
	}
	if (pWindow == NULL)
		return;

	WindowSelected(pWindow->szPath);
}



void WindowSelected(const char *szPath)
{
	struct Window_s *pWindow;

	for (pWindow = pWindowList; pWindow != NULL; pWindow = pWindow->pNext)
	{
		if (strcmp(pWindow->szPath, szPath) == 0)
			break;
	}
	if (pWindow == NULL)
		return;
}
