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

#include <gtk/gtk.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include "doc.h"
#include "filesel.h"
#include "filetool.h"
#include "global.h"
#include "msgbox.h"
#include "project.h"
#include "support.h"



/* private functions */
void ProjectWidgetsHide(void);
void ProjectWidgetsShow(void);



/*
	Project initialization
*/

void ProjectInitialize(void)
{
	char szDir[TEXTLEN];
	
	/* change current directory to $HOME */
	strcpy(szDir, getenv("HOME"));
	chdir(szDir);
	
	/* initialize project properties */
	strcpy(Project.szName, "");
	strcpy(Project.szExt, "");
	strcpy(Project.szDir, "");
	Project.bChanged = FALSE;
	ProjectWidgetsHide();
}



/*
	Menu PROJECT handlers
	(New, Open, Save, Close, Exit)
*/

void MenuProjectNew_Activation(GtkMenuItem *pMenuItem, gpointer pUserData)
{
	int iResult;
	char szProjectFileName[TEXTLEN];
	
	/* firstly ask for name of a new project */
	strcpy(szProjectFileName, "unnamed.prj");
	iResult = FileSelection("prj", szProjectFileName);
	if (iResult != SUCCESS)
	{
		strcpy(szProjectFileName, "");
		return;
	}
	
	ProjectNew(szProjectFileName);
}


void MenuProjectOpen_Activation(GtkMenuItem *pMenuItem, gpointer pUserData)
{
	int iResult;
	char szProjectFileName[TEXTLEN];
	
	/* ask for project name */
	strcpy(szProjectFileName, "unnamed.prj");
	iResult = FileSelection("prj", szProjectFileName);
	if (iResult != SUCCESS)
	{
		strcpy(szProjectFileName, "");
		return;
	}
	
	ProjectOpen(szProjectFileName);
}


void MenuProjectSave_Activation(GtkMenuItem *pMenuItem, gpointer pUserData)
{
	int iResult;
	char szFileName[TEXTLEN];
	
	if (strlen(Project.szName) == 0)
	{
		MsgBox("FATAL ERROR ! Project has no name.", MSGBOX_OK);
		return;
	}
	
	/* save project data */
	strcpy(szFileName, Project.szDir);
	strcat(szFileName, "/");
	strcat(szFileName, Project.szName);
	strcat(szFileName, ".");
	strcat(szFileName, Project.szExt);
	iResult = DocSave(szFileName);
	if (iResult != SUCCESS)
	{
		MsgBox("Cannot save project !", MSGBOX_OK);
		return;
	}
}


void MenuProjectClose_Activation(GtkMenuItem *pMenuItem, gpointer pUserData)
{
	int iResult;
	char szFileName[TEXTLEN], szValue[TEXTLEN];
	
	/* check if project changed */
	if (Project.bChanged)
	{
		iResult = MsgBox("Project changed. Save it now ?", MSGBOX_YESNOCANCEL);
		if (iResult == 2)
			return;
		if (iResult == 0)
			MenuProjectSave_Activation(pMenuItem, pUserData);
	}
	
	/* unload files */
	iResult = SUCCESS;
	strcpy(szFileName, "");
	while (iResult == SUCCESS)
	{
		iResult = DocGetProperty(DOC_NEXT, szFileName, (void *) szValue);
		if (iResult != SUCCESS)
			break;
		DocDestroy(szValue);
	}

	/* unload project */
	strcpy(Project.szName, "");
	ProjectWidgetsHide();
	ProjectTitle();
	Project.bChanged = FALSE;
}


void MenuProjectExit_Activation(GtkMenuItem *pMenuItem, gpointer pUserData)
{
	MenuProjectClose_Activation(pMenuItem, pUserData);
	/* no gtk_main_loop(), so not gtk_main_quit(); */
	bRunning = FALSE;
}



/*
	Private functions
*/

void ProjectWidgetsHide(void)
{
	GtkWidget *pWidget;
	
	/* do menu positions not sensitive */
	pWidget = lookup_widget(pWindowMain, "MenuProjectNew");
	if (pWidget == NULL)
		/**/;
	gtk_widget_set_sensitive(pWidget, TRUE);
	pWidget = lookup_widget(pWindowMain, "MenuProjectOpen");
	if (pWidget == NULL)
		/**/;
	gtk_widget_set_sensitive(pWidget, TRUE);
	pWidget = lookup_widget(pWindowMain, "MenuProjectSave");
	if (pWidget == NULL)
		/**/;
	gtk_widget_set_sensitive(pWidget, FALSE);
	pWidget = lookup_widget(pWindowMain, "MenuProjectClose");
	if (pWidget == NULL)
		/**/;
	gtk_widget_set_sensitive(pWidget, FALSE);
	pWidget = lookup_widget(pWindowMain, "MenuFile");
	if (pWidget == NULL)
		/**/;
	gtk_widget_set_sensitive(pWidget, FALSE);
	pWidget = lookup_widget(pWindowMain, "MenuAction");
	if (pWidget == NULL)
		/**/;
	gtk_widget_set_sensitive(pWidget, FALSE);
	pWidget = lookup_widget(pWindowMain, "MenuTool");
	if (pWidget == NULL)
		/**/;
	gtk_widget_set_sensitive(pWidget, FALSE);
	pWidget = lookup_widget(pWindowMain, "MenuWindow");
	if (pWidget == NULL)
		/**/;
	gtk_widget_set_sensitive(pWidget, FALSE);
	
	/* hide window fields */
	pWidget = lookup_widget(pWindowMain, "ProjectArea");
	if (pWidget == NULL)
		/**/;
	gtk_widget_set_sensitive(pWidget, FALSE);
//	pWidget = lookup_widget(pWindowMain, "DocArea");
//	if (pWidget == NULL)
//		/**/;
//	gtk_widget_set_sensitive(pWidget, FALSE);
	pWidget = lookup_widget(pWindowMain, "StatusArea");
	if (pWidget == NULL)
		/**/;
	gtk_widget_set_sensitive(pWidget, FALSE);
}


void ProjectWidgetsShow(void)
{
	GtkWidget *pWidget;
	
	/* do menu positions sensitive */
	pWidget = lookup_widget(pWindowMain, "MenuProjectNew");
	if (pWidget == NULL)
		/**/;
	gtk_widget_set_sensitive(pWidget, FALSE);
	pWidget = lookup_widget(pWindowMain, "MenuProjectOpen");
	if (pWidget == NULL)
		/**/;
	gtk_widget_set_sensitive(pWidget, FALSE);
	pWidget = lookup_widget(pWindowMain, "MenuProjectSave");
	if (pWidget == NULL)
		/**/;
	gtk_widget_set_sensitive(pWidget, TRUE);
	pWidget = lookup_widget(pWindowMain, "MenuProjectClose");
	if (pWidget == NULL)
		/**/;
	gtk_widget_set_sensitive(pWidget, TRUE);
	pWidget = lookup_widget(pWindowMain, "MenuFile");
	if (pWidget == NULL)
		/**/;
	gtk_widget_set_sensitive(pWidget, TRUE);
	pWidget = lookup_widget(pWindowMain, "MenuAction");
	if (pWidget == NULL)
		/**/;
	gtk_widget_set_sensitive(pWidget, TRUE);
	MenuActionRefresh("");
	pWidget = lookup_widget(pWindowMain, "MenuTool");
	if (pWidget == NULL)
		/**/;
	gtk_widget_set_sensitive(pWidget, TRUE);
	pWidget = lookup_widget(pWindowMain, "MenuWindow");
	if (pWidget == NULL)
		/**/;
	gtk_widget_set_sensitive(pWidget, TRUE);
	
	/* show window fields */
	pWidget = lookup_widget(pWindowMain, "ProjectArea");
	if (pWidget == NULL)
		/**/;
	gtk_widget_set_sensitive(pWidget, TRUE);
//	pWidget = lookup_widget(pWindowMain, "DocArea");
//	if (pWidget == NULL)
//		/**/;
//	gtk_widget_set_sensitive(pWidget, TRUE);
	pWidget = lookup_widget(pWindowMain, "StatusArea");
	if (pWidget == NULL)
		/**/;
	gtk_widget_set_sensitive(pWidget, TRUE);
}



void ProjectTitle(void)
{
	int iResult, bChanged;
	char szTitle[TEXTLEN], szFileName[TEXTLEN];

	/* add software name to title bar */
	strcpy(szTitle, GEDA_TITLE);

	/* add project name to title bar */
	if (strlen(Project.szName) > 0)
	{
		strcat(szTitle, " - ");
		strcat(szTitle, Project.szName);
		strcat(szTitle, ".");
		strcat(szTitle, Project.szExt);
		if (Project.bChanged)
			strcat(szTitle, " *");
	}
	
	/* add current file name to title bar */
	iResult = DocGetProperty(DOC_SELECTED, NULL, (void *) szFileName);
	if (iResult == SUCCESS && strlen(szFileName) > 0)
	{
		strcat(szTitle, " (");
		strcat(szTitle, szFileName);
		DocGetProperty(DOC_CHANGED, szFileName, (void *) &bChanged);
		if (bChanged)
			strcat(szTitle, " *");
		strcat(szTitle, ")");
	}
	
	gtk_window_set_title(GTK_WINDOW(pWindowMain), szTitle);
}



void ProjectChanged(int bValue)
{
	if (bValue != TRUE && bValue !=FALSE)
		return;
	
	Project.bChanged = bValue;
	ProjectTitle();
}
