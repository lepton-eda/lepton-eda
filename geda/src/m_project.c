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

#define GTK_ENABLE_BROKEN
#include <gtk/gtk.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include "doc.h"
#include "filesel.h"
#include "filetool.h"
#include "global.h"
#include "interface.h"
#include "m_action.h"
#include "m_project.h"
#include "msgbox.h"
#include "project.h"
#include "support.h"



void ProjectWidgetsHide(void);
void ProjectWidgetsShow(void);



/*******************************************************************************

	Static variables and functions, definitions

*******************************************************************************/

static void ProjectPropertiesCreate(void);
static void ProjectPropertiesDestroy(void);
static BOOL ProjectPropertiesValid(void);



/*******************************************************************************

	MENU : Project -> New

*******************************************************************************/

/* callback from menu */
void ProjectNew_MenuActivation(GtkMenuItem *pMenuItem, gpointer pUserData)
{
	int iResult;
	char szProjectFileName[TEXTLEN];

//	strcpy(szProjectFileName, "unnamed.prj");

	chdir(pDefaultProjectDir);

	iResult = ProjectProperties(TRUE);
	if (iResult == SUCCESS)
	{
		strcpy(szProjectFileName, Project.szDir);
		strcat(szProjectFileName, "/");
		strcat(szProjectFileName, Project.szName);
		strcat(szProjectFileName, ".");
		strcat(szProjectFileName, Project.szExt);

		ProjectNew(szProjectFileName);
	}
}



/*******************************************************************************

	MENU : Project -> Open

*******************************************************************************/

/* callback from menu */
void ProjectOpen_MenuActivation(GtkMenuItem *pMenuItem, gpointer pUserData)
{
	int iResult;
	char szProjectFileName[TEXTLEN];
	
	chdir(pDefaultProjectDir);

	/* ask for project name */
	strcpy(szProjectFileName, DEF_PRJNAME);
	iResult = FileSelection(DEF_PRJEXT, szProjectFileName);
	if (iResult != SUCCESS)
	{
		strcpy(szProjectFileName, "");
		return;
	}
	
	ProjectOpen(szProjectFileName);
}



/*******************************************************************************

	MENU : Project -> Properties

*******************************************************************************/

static GtkWindow *pWindow = NULL;
static BOOL bUpdateDir = FALSE;
static BOOL bUpdateBlock = FALSE;
static BOOL bCanceled = FALSE;
static BOOL bCompleted = FALSE;


/* callback from menu */
void ProjectProperties_MenuActivation(GtkMenuItem *pMenuItem, gpointer pUserData)
{
	ProjectProperties(FALSE);
}


/* openning project properties window */
int ProjectProperties(BOOL bIsNew)
{
	GtkEntry *pEntryName = NULL, *pEntryDir = NULL, *pEntryAuthor = NULL;
	GtkText *pTextDescription;
	gint x;
	
	bUpdateDir = FALSE;
	bUpdateBlock = FALSE;

	/* default values for new project */
	if (bIsNew)
	{
		strcpy(Project.szName, pDefaultProjectName);
		strcpy(Project.szExt, pDefaultProjectExt);
		
		strcpy(Project.szDir, pDefaultProjectDir);
		strcat(Project.szDir, G_DIR_SEPARATOR_S);
		strcat(Project.szDir, Project.szName);

		strcpy(Project.szAuthor, getenv("USER"));
		strcpy(Project.szDesc, "");
	}

	/* project editing loop */
	bCanceled = FALSE;
	bCompleted = FALSE;
	while (!bCanceled && !bCompleted)
	{
		ProjectPropertiesCreate();
	
		pEntryName = GTK_ENTRY(lookup_widget(GTK_WIDGET(pWindow), "pProjectPropertiesEntryName"));
		if (pEntryName == NULL)
			FatalError(__FILE__, __LINE__, __DATE__);
	
		pEntryDir = GTK_ENTRY(lookup_widget(GTK_WIDGET(pWindow), "pProjectPropertiesEntryDir"));
		if (pEntryDir == NULL)
			FatalError(__FILE__, __LINE__, __DATE__);
	
		pEntryAuthor = GTK_ENTRY(lookup_widget(GTK_WIDGET(pWindow), "pProjectPropertiesEntryAuthor"));
		if (pEntryAuthor == NULL)
			FatalError(__FILE__, __LINE__, __DATE__);

		pTextDescription = GTK_TEXT(lookup_widget(GTK_WIDGET(pWindow), "pProjectPropertiesTextDescription"));
		if (pTextDescription == NULL)
			FatalError(__FILE__, __LINE__, __DATE__);

		if (!bIsNew)
		{
			gtk_entry_set_editable(pEntryName, FALSE);
			gtk_widget_set_sensitive(GTK_WIDGET(pEntryName), FALSE);
			
			gtk_entry_set_editable(pEntryDir, FALSE);
			gtk_widget_set_sensitive(GTK_WIDGET(pEntryDir), FALSE);
			
			bUpdateDir = TRUE;
		}

		gtk_entry_set_text(pEntryName, Project.szName);
		bUpdateBlock = TRUE, gtk_entry_set_text(pEntryDir, Project.szDir), bUpdateBlock = FALSE;
		gtk_entry_set_text(pEntryAuthor, Project.szAuthor);
		gtk_editable_delete_text(GTK_EDITABLE(pTextDescription), 0, -1), x = 0, 
			gtk_editable_insert_text(GTK_EDITABLE(pTextDescription), Project.szDesc, strlen(Project.szDesc), &x);

		while (!bCanceled && !bCompleted)
			g_main_iteration(FALSE);

		if (bCompleted)
		{
			strcpy(Project.szName, FileGetName(gtk_entry_get_text(pEntryName)));
			strcpy(Project.szExt, 
				strlen(FileGetExt(gtk_entry_get_text(pEntryName))) > 0
					? FileGetExt(gtk_entry_get_text(pEntryName))
					: pDefaultProjectExt
				);
			strcpy(Project.szDir, gtk_entry_get_text(pEntryDir));
			if (strcmp(FileGetName(gtk_entry_get_text(pEntryDir)), Project.szName))
			{
				strcat(Project.szDir, G_DIR_SEPARATOR_S);
				strcat(Project.szDir, Project.szName);
			};
			strcpy(Project.szAuthor, gtk_entry_get_text(pEntryAuthor));
			strcpy(Project.szDesc, gtk_editable_get_chars(GTK_EDITABLE(pTextDescription), 0, -1));
			
			ProjectSave();
		}

		ProjectPropertiesDestroy();

		if (bCompleted && !ProjectPropertiesValid())
			bCompleted = FALSE;
	}

	return bCompleted
		? SUCCESS
		: FAILURE;
}


static void ProjectPropertiesCreate(void)
{
	pWindow = GTK_WINDOW(create_ProjectProperties());
	if (pWindow == NULL)
		FatalError(__FILE__, __LINE__, __DATE__);
	
	gtk_window_set_transient_for(GTK_WINDOW(pWindow), GTK_WINDOW(pWindowMain));
	gtk_window_set_policy(GTK_WINDOW(pWindow), FALSE, FALSE, FALSE);
	gtk_widget_show(GTK_WIDGET(pWindow));
	
	while (g_main_iteration(FALSE)) ;
}


static void ProjectPropertiesDestroy(void)
{
	gtk_widget_destroy(GTK_WIDGET(pWindow));
}


static BOOL ProjectPropertiesValid(void)
{
	/* TODO: validation of project parameters */
	return TRUE;
}


void ProjectProperties_ButtonClicked(GtkButton *pButtonClicked, gpointer pUserData)
{
	GtkButton *pOk = NULL, *pCancel = NULL;
	
	pOk = GTK_BUTTON(lookup_widget(GTK_WIDGET(pWindow), "pProjectPropertiesButtonOk"));
	if (pOk == NULL)
		FatalError(__FILE__, __LINE__, __DATE__);
	
	pCancel = GTK_BUTTON(lookup_widget(GTK_WIDGET(pWindow), "pProjectPropertiesButtonCancel"));
	if (pCancel == NULL)
		FatalError(__FILE__, __LINE__, __DATE__);

	if (pButtonClicked == pOk)
		bCompleted = TRUE;

	else if (pButtonClicked == pCancel)
		bCanceled = TRUE;
}


void ProjectProperties_NameChanged(GtkEditable *pEditable, GdkEventKey *pEvent, gpointer pUserData)
{
	GtkEntry *pEntryName = NULL, *pEntryDir = NULL;

	pEntryName = GTK_ENTRY(lookup_widget(GTK_WIDGET(pWindow), "pProjectPropertiesEntryName"));
	if (pEntryName == NULL)
		FatalError(__FILE__, __LINE__, __DATE__);
	
	pEntryDir = GTK_ENTRY(lookup_widget(GTK_WIDGET(pWindow), "pProjectPropertiesEntryDir"));
	if (pEntryDir == NULL)
		FatalError(__FILE__, __LINE__, __DATE__);
	
	if (!bUpdateDir)
	{
		strcpy(Project.szDir, pDefaultProjectDir);
		strcat(Project.szDir, "/");
		strcat(Project.szDir, gtk_entry_get_text(pEntryName));

		bUpdateBlock = TRUE;
		gtk_entry_set_text(pEntryDir, Project.szDir);
		bUpdateBlock = FALSE;
	}
}


void ProjectProperties_DirChanged(GtkEditable *pEditable, gint pStartPos, gint pEndPos, gpointer pUserData)
{
	if (bUpdateBlock)
		return;
	
	bUpdateDir = TRUE;
}



/*******************************************************************************

	MENU : Project -> Close

*******************************************************************************/

/* callback from menu */
void ProjectClose_MenuActivation(GtkMenuItem *pMenuItem, gpointer pUserData)
{
	int iResult;
	char szFileName[TEXTLEN], szValue[TEXTLEN];
	
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
}



/*******************************************************************************

	MENU : Project -> Exit

*******************************************************************************/

/* callback from menu */
void ProjectExit_MenuActivation(GtkMenuItem *pMenuItem, gpointer pUserData)
{
	ProjectClose_MenuActivation(pMenuItem, pUserData);
	/* no gtk_main_loop(), so not gtk_main_quit(); */
	bRunning = FALSE;
}
