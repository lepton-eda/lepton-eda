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

#include <ctype.h>
#include <gtk/gtk.h>
#include <stdio.h>
#include <string.h>
#include "doc.h"
#include "filetool.h"
#include "global.h"
#include "msgbox.h"
#include "m_action.h"
#include "support.h"
#include "task.h"
#include "tool.h"



GtkMenu *pMenu;
char szOldExt[TEXTLEN] = "";
extern struct Action_s *pActionList;
	


static void MenuActionActivation(GtkMenuItem *pMenuItem, gpointer pUserData);



void MenuActionInitialize(void)
{
	GtkWidget *pWidget;
	struct Action_s *pAction;
	char szMessage[TEXTLEN];
	
	/* look for menu Action */
	pWidget = lookup_widget(pWindowMain, "MenuAction");
	if (pWidget == NULL)
	{
		sprintf(szMessage, "Fatal error in file %s in line %d", __FILE__, __LINE__);
		MsgBox(szMessage, MSGBOX_OK);
		return;
	}
	
	/* create Action menu */
	pMenu = GTK_MENU(gtk_menu_new());
	gtk_widget_ref(GTK_WIDGET(pMenu));
	gtk_object_set_data_full(GTK_OBJECT(pWindowMain), "MenuAction_menu", pMenu, (GtkDestroyNotify) gtk_widget_unref);
	gtk_menu_item_set_submenu(GTK_MENU_ITEM (pWidget), GTK_WIDGET(pMenu));

	/* create Action menu entries */
	for (pAction = pActionList; pAction != NULL; pAction = pAction->pNext)
	{
		pAction->pMenuItem = GTK_MENU_ITEM(gtk_menu_item_new_with_label(pAction->szName));
		gtk_widget_ref(GTK_WIDGET(pAction->pMenuItem));
		pAction->bMenuEntryUsed = FALSE;
		gtk_signal_connect(GTK_OBJECT(pAction->pMenuItem), "activate", GTK_SIGNAL_FUNC(MenuActionActivation), NULL);
	}
}


void MenuActionRefresh(const char *szExt)
{
	struct Action_s *pAction;

	/* delete items from menu */
	for (pAction = pActionList; pAction != NULL; pAction = pAction->pNext)
	{
		if (!pAction->bMenuEntryUsed)
			continue;   
		
		gtk_container_remove(GTK_CONTAINER(pMenu), GTK_WIDGET(pAction->pMenuItem));
		pAction->bMenuEntryUsed = FALSE;
	}
	
	/* add items to the menu */
	for (pAction = pActionList; pAction != NULL; pAction = pAction->pNext)
	{
		if (strcmp(pAction->szExt, szExt))
			continue;   
		
		gtk_widget_show(GTK_WIDGET(pAction->pMenuItem));
		gtk_container_add(GTK_CONTAINER(pMenu), GTK_WIDGET(pAction->pMenuItem));
		gtk_widget_set_sensitive(GTK_WIDGET(pAction->pMenuItem), TRUE);
		pAction->bMenuEntryUsed = TRUE;
	}
}


static void MenuActionActivation(GtkMenuItem *pMenuItem, gpointer pUserData)
{
	struct Action_s *pAction;
	int iResult, i, j;
	char szCommand[TEXTLEN], szFileName[TEXTLEN], *szName, *szExt, *szPath, *szRel, szImport[TEXTLEN], szMessage[TEXTLEN];
	char *pParams[2];
	
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
	
} 


