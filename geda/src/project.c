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

#include <stdlib.h>

#ifdef HAVE_STRING_H  
#include <string.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "doc.h"
#include "filetool.h"
#include "global.h"
#include "m_action.h"
#include "msgbox.h"
#include "project.h"
#include "support.h"



/*******************************************************************************

	Private functions and variables

*******************************************************************************/

#define PROJECT_EXT          "prj"

struct Project_s Project;



/*******************************************************************************

	Project initialize

*******************************************************************************/

void ProjectInitialize(void)
{
	strcpy(Project.szName, "");
	strcpy(Project.szExt, "");
	strcpy(Project.szDir, "");
	ProjectWidgetsHide();
}



/*******************************************************************************

	Project new

*******************************************************************************/

void ProjectNew(const char *szPath)
{
	int iResult;
	char *pPath;
	
	chdir(pDefaultProjectDir);
	
	pPath = (char *) malloc(strlen(szPath) + strlen(PROJECT_EXT) + strlen(FileGetName(szPath) + 3));
	if (pPath == NULL)
		return;
	strcpy(pPath, szPath);

	if (strcmp(FileGetExt(pPath), PROJECT_EXT) != 0)
	{
		strcat(pPath, ".");
		strcat(pPath, PROJECT_EXT);
	}

	iResult = FileIsExisting(pPath);
	if (iResult == SUCCESS)
	{
		iResult = MsgBox(
			pWindowMain,
			_("Question..."),
			_("Project already exists. Overwrite?"),
			MSGBOX_QUESTION | MSGBOX_YES | MSGBOX_NOD | MSGBOX_CANCEL
			);
		if (iResult != MSGBOX_YES)
			goto PROJECT_MENU_EXIT;
	}

	strcpy(Project.szName, FileGetName(pPath));
	strcpy(Project.szExt, FileGetExt(pPath));
	strcpy(Project.szDir, FileGetDir(pPath));
	chdir(Project.szDir);

	/* make project active */
	ProjectWidgetsShow();
	ProjectSave();
	ProjectTitle();

PROJECT_MENU_EXIT:
	
	free(pPath);
}



/*******************************************************************************

	Project open

*******************************************************************************/

void ProjectOpen(const char *szPath)
{
	int iResult;
	
	chdir(pDefaultProjectDir);

	/* this functions fail if the szPath is a directory */
	/* lets do an extra file test */
	if (g_file_test(szPath,G_FILE_TEST_IS_REGULAR)) {
	  strcpy(Project.szName, FileGetName(szPath));
	  strcpy(Project.szExt, FileGetExt(szPath));
	  strcpy(Project.szDir, FileGetDir(szPath));
	  
	  chdir(Project.szDir);
	  
	  iResult = DocLoad(szPath);
	}
	else {
	  iResult = FAILURE;
	}

	if (iResult != SUCCESS)
	{
		MsgBox(
			pWindowMain,
			_("Error!"),
			_("Cannot load project!"),
			MSGBOX_ERROR | MSGBOX_OKD
			);
		return;
	}
	
	/* make project active */
	ProjectWidgetsShow();
	ProjectTitle();
}



/*******************************************************************************

	Project save

*******************************************************************************/

int ProjectSave(void)
{
	int iResult;
	char szFileName[TEXTLEN];
	
	if (strlen(Project.szName) == 0)
		FatalError(__FILE__, __LINE__, __DATE__);


	strcpy(szFileName, Project.szDir);
	strcat(szFileName, G_DIR_SEPARATOR_S);
	strcat(szFileName, Project.szName);
	strcat(szFileName, ".");
	strcat(szFileName, Project.szExt);

	iResult = DocSave(szFileName);
	if (iResult != SUCCESS)
	{
		MsgBox(
			pWindowMain,
			_("Error!"),
			_("Cannot save project!"),
			MSGBOX_ERROR | MSGBOX_OKD
			);
		return FAILURE;
	}


	return SUCCESS;
}



/*******************************************************************************

	Show / Hide widgets used only when a project is opened / closed

*******************************************************************************/

void ProjectWidgetsHide(void)
{
	GtkWidget *pWidget;
	
	/* do menu positions not sensitive */
	pWidget = lookup_widget(GTK_WIDGET(pWindowMain), "MenuProjectNew");
	if (pWidget == NULL)
		/**/;
	gtk_widget_set_sensitive(pWidget, TRUE);
	pWidget = lookup_widget(GTK_WIDGET(pWindowMain), "MenuProjectOpen");
	if (pWidget == NULL)
		/**/;
	gtk_widget_set_sensitive(pWidget, TRUE);
	pWidget = lookup_widget(GTK_WIDGET(pWindowMain), "MenuProjectProperties");
	if (pWidget == NULL)
		/**/;
	gtk_widget_set_sensitive(pWidget, FALSE);
	pWidget = lookup_widget(GTK_WIDGET(pWindowMain), "MenuProjectClose");
	if (pWidget == NULL)
		/**/;
	gtk_widget_set_sensitive(pWidget, FALSE);
	pWidget = lookup_widget(GTK_WIDGET(pWindowMain), "MenuFile");
	if (pWidget == NULL)
		/**/;
	gtk_widget_set_sensitive(pWidget, FALSE);
	pWidget = lookup_widget(GTK_WIDGET(pWindowMain), "MenuAction");
	if (pWidget == NULL)
		/**/;
	gtk_widget_set_sensitive(pWidget, FALSE);
	pWidget = lookup_widget(GTK_WIDGET(pWindowMain), "MenuTool");
	if (pWidget == NULL)
		/**/;
	gtk_widget_set_sensitive(pWidget, FALSE);
	pWidget = lookup_widget(GTK_WIDGET(pWindowMain), "MenuWindow");
	if (pWidget == NULL)
		/**/;
	gtk_widget_set_sensitive(pWidget, FALSE);

	/* hide window fields */
	pWidget = lookup_widget(GTK_WIDGET(pWindowMain), "ProjectArea");
	if (pWidget == NULL)
		/**/;
	gtk_widget_set_sensitive(pWidget, FALSE);
//	pWidget = lookup_widget(GTK_WIDGET(pWindowMain), "DocArea");
//	if (pWidget == NULL)
//		/**/;
//	gtk_widget_set_sensitive(pWidget, FALSE);
	pWidget = lookup_widget(GTK_WIDGET(pWindowMain), "StatusArea");
	if (pWidget == NULL)
		/**/;
	gtk_widget_set_sensitive(pWidget, FALSE);
}


void ProjectWidgetsShow(void)
{
	GtkWidget *pWidget;

	/* do menu positions sensitive */
	pWidget = lookup_widget(GTK_WIDGET(pWindowMain), "MenuProjectNew");
	if (pWidget == NULL)
		/**/;
	gtk_widget_set_sensitive(pWidget, FALSE);
	pWidget = lookup_widget(GTK_WIDGET(pWindowMain), "MenuProjectOpen");
	if (pWidget == NULL)
		/**/;
	gtk_widget_set_sensitive(pWidget, FALSE);
	pWidget = lookup_widget(GTK_WIDGET(pWindowMain), "MenuProjectProperties");
	if (pWidget == NULL)
		/**/;
	gtk_widget_set_sensitive(pWidget, TRUE);
	pWidget = lookup_widget(GTK_WIDGET(pWindowMain), "MenuProjectClose");
	if (pWidget == NULL)
		/**/;
	gtk_widget_set_sensitive(pWidget, TRUE);
	pWidget = lookup_widget(GTK_WIDGET(pWindowMain), "MenuFile");
	if (pWidget == NULL)
		/**/;
	gtk_widget_set_sensitive(pWidget, TRUE);
	pWidget = lookup_widget(GTK_WIDGET(pWindowMain), "MenuAction");
	if (pWidget == NULL)
		/**/;
	gtk_widget_set_sensitive(pWidget, TRUE);
	MenuActionRefresh("");
	pWidget = lookup_widget(GTK_WIDGET(pWindowMain), "MenuTool");
	if (pWidget == NULL)
		/**/;
	gtk_widget_set_sensitive(pWidget, TRUE);
	pWidget = lookup_widget(GTK_WIDGET(pWindowMain), "MenuWindow");
	if (pWidget == NULL)
		/**/;
	gtk_widget_set_sensitive(pWidget, TRUE);

	/* show window fields */
	pWidget = lookup_widget(GTK_WIDGET(pWindowMain), "ProjectArea");
	if (pWidget == NULL)
		/**/;
	gtk_widget_set_sensitive(pWidget, TRUE);
//	pWidget = lookup_widget(GTK_WIDGET(pWindowMain), "DocArea");
//	if (pWidget == NULL)
//		/**/;
//	gtk_widget_set_sensitive(pWidget, TRUE);
	pWidget = lookup_widget(GTK_WIDGET(pWindowMain), "StatusArea");
	if (pWidget == NULL)
		/**/;
	gtk_widget_set_sensitive(pWidget, TRUE);
}



/*******************************************************************************

	Show window title bar

*******************************************************************************/

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

	gtk_window_set_title(pWindowMain, szTitle);
}



/*******************************************************************************

	Actions taken if project is changed
	(a file is added or removed, etc.)

*******************************************************************************/

void ProjectChanged(BOOL bValue)
{
	if (bValue != TRUE && bValue !=FALSE)
		return;
	
	Project.bChanged = bValue;
	ProjectTitle();
}
