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
#include <stdio.h>

#ifdef HAVE_STRING_H  
#include <string.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "doc.h"
#include "file.h"
#include "filesel.h"
#include "filetool.h"
#include "interface.h"
#include "msgbox.h"
#include "m_file.h"
#include "project.h"
#include "support.h"
#include "tool.h"
#include "txtedit.h"
#include "window.h"



/*******************************************************************************

	Static variables and functions, definitions

*******************************************************************************/

static GtkWindow *pFileNewWindow = NULL;
static BOOL bFileNewCompleted;
static BOOL bFileNewCanceled;
static char szFileNewName[TEXTLEN];

static void FileNewCreate(void);
static void FileNewDestroy(void);
static void FileNewComplete(void);
static BOOL FileNewValid(void);



/*******************************************************************************

	MENU : File -> Open

*******************************************************************************/

/* callback from menu */
void FileEdit_MenuActivation(GtkMenuItem *pMenuItem, gpointer pUserData)
{
	int iResult;
	char szName[TEXTLEN];
	
	/* get the file name */
	iResult = DocGetProperty(DOC_SELECTED, NULL, (void *) szName);
	if (iResult != SUCCESS)
		return;
	if (strlen(szName) == 0)
		return;
	
	FileEdit(szName);
}



/*******************************************************************************

	MENU : File -> Save

*******************************************************************************/

/* callback from menu */
void FileSave_MenuActivation(GtkMenuItem *pMenuItem, gpointer pUserData)
{
	int iResult;
	char szName[TEXTLEN], *pFileName;

	/* get the file name */
	iResult = DocGetProperty(DOC_SELECTED, NULL, (void *) szName);
	if (iResult != SUCCESS)
		return;
	if (strlen(szName) == 0)
		return;
	pFileName = WindowTop();
	FileSave(pFileName);
}



/*******************************************************************************

	MENU : File -> Print

*******************************************************************************/

/* callback from menu */
void FilePrint_MenuActivation(GtkMenuItem *pMenuItem, gpointer pUserData)
{
	int iResult;
	char szName[TEXTLEN];
	
	/* get the file name */
	iResult = DocGetProperty(DOC_SELECTED, NULL, (void *) szName);
	if (iResult != SUCCESS)
		return;
	if (strlen(szName) == 0)
		return;
	
	FilePrint(WindowTop());
}



/*******************************************************************************

	MENU : File -> Close

*******************************************************************************/

/* callback from menu */
void FileClose_MenuActivation(GtkMenuItem *pMenuItem, gpointer pUserData)
{
	int iResult;
	char szName[TEXTLEN], *pFileName;

	/* get the file name */
	iResult = DocGetProperty(DOC_SELECTED, NULL, (void *) szName);
	if (iResult != SUCCESS)
		return;
	if (strlen(szName) == 0)
		return;

	pFileName = WindowTop();
	DocClose(pFileName);
}



/*******************************************************************************

	MENU : File -> New

*******************************************************************************/

/* callback from menu */
void FileNew_MenuActivation(GtkMenuItem *pMenuItem, gpointer pUserData)
{
	char *pResult;
	
	
	/*
		open a dialog to enter a filename and to choose a type of the file
	*/
	
	bFileNewCanceled = FALSE;
	bFileNewCompleted = FALSE;
	while (!bFileNewCanceled && !bFileNewCompleted)
	{
		FileNewCreate();
	
		while (!bFileNewCanceled && !bFileNewCompleted)
			g_main_iteration(FALSE);

		if (bFileNewCompleted)
			FileNewComplete();

		FileNewDestroy();

		if (bFileNewCompleted && !FileNewValid())
			bFileNewCompleted = FALSE;
	}


	/*
		try to create and attach the file
	*/
	
	if (bFileNewCompleted)
	{
		pResult = FileNew(szFileNewName, "");
		if (pResult != NULL)
		{
			MsgBox(
				pWindowMain,
				"Error !",
				pResult,
				MSGBOX_ERROR | MSGBOX_OKD
				);
		}
	}
		
	return;
}


static void FileNewCreate(void)
{
	GList *pFileNewTypeCombo_items = NULL;
	GtkCombo *pFileNewTypeCombo = NULL;
	BOOL bInFileNew;
	int iTabLen = 0, iId, iResult;
	char *szTabName[TEXTLEN], szName[TEXTLEN];
	
	/* create window */

	pFileNewWindow = GTK_WINDOW(create_FileNew());
	if (pFileNewWindow == NULL)
		FatalError(__FILE__, __LINE__, __DATE__);
	
	gtk_window_set_transient_for(GTK_WINDOW(pFileNewWindow), GTK_WINDOW(pWindowMain));
	gtk_window_set_policy(GTK_WINDOW(pFileNewWindow), FALSE, FALSE, FALSE);
	gtk_widget_show(GTK_WIDGET(pFileNewWindow));

	/* fill list of file types in combo */

	pFileNewTypeCombo = GTK_COMBO(lookup_widget(GTK_WIDGET(pFileNewWindow), "pFileNewTypeCombo"));
	if (pFileNewTypeCombo == NULL)
		FatalError(__FILE__, __LINE__, __DATE__);

	for (iId = 0, iResult = SUCCESS, iTabLen = 0; iResult == SUCCESS; iId ++)
	{
		iResult = ToolValueGet(EXT_LIST, EXT_INFILENEW, iId, &bInFileNew);
		if (iResult != SUCCESS)
			break;
		if (!bInFileNew)
			continue;

		iResult = ToolValueGet(EXT_LIST, EXT_NAME, iId, szName);
		if (iResult != SUCCESS)
			FatalError(__FILE__, __LINE__, __DATE__);

		szTabName[iTabLen] = (char *) malloc(strlen(szName) + 1);
		if (szTabName[iTabLen] == NULL)
			FatalError(__FILE__, __LINE__, __DATE__);
		strcpy(szTabName[iTabLen], szName);
		
		pFileNewTypeCombo_items = g_list_append(pFileNewTypeCombo_items, (gpointer) _(szTabName[iTabLen]));
		iTabLen ++;
	}
	gtk_combo_set_popdown_strings(GTK_COMBO(pFileNewTypeCombo), pFileNewTypeCombo_items);
	g_list_free(pFileNewTypeCombo_items);
	for (iId = 0; iId < iTabLen; iId ++)
		free(szTabName[iId]);
}


static void FileNewDestroy(void)
{
	if (pFileNewWindow != NULL)
	{
		gtk_widget_destroy(GTK_WIDGET(pFileNewWindow));
		pFileNewWindow = NULL;
	}
}


static void FileNewComplete(void)
{
	GtkEntry *pFileNewTypeComboEntry, *pFileNewNameEntry;
	BOOL bInFileNew;
	int iId, iResult;
	char szNameFromTable[TEXTLEN], szExt[TEXTLEN];
	const char *pNameFromEntry;
	
	/* read extension from combo */
	
	pFileNewTypeComboEntry = GTK_ENTRY(lookup_widget(GTK_WIDGET(pFileNewWindow), "pFileNewTypeComboEntry"));
	if (pFileNewTypeComboEntry == NULL)
		FatalError(__FILE__, __LINE__, __DATE__);

	pNameFromEntry = gtk_entry_get_text(pFileNewTypeComboEntry);
	if (pNameFromEntry == NULL)
		FatalError(__FILE__, __LINE__, __DATE__);
	
	for (iId = 0, iResult = SUCCESS; iResult == SUCCESS; iId ++)
	{
		iResult = ToolValueGet(EXT_LIST, EXT_INFILENEW, iId, &bInFileNew);
		if (iResult != SUCCESS)
			FatalError(__FILE__, __LINE__, __DATE__);
		if (!bInFileNew)
			continue;
		
		iResult = ToolValueGet(EXT_LIST, EXT_NAME, iId, szNameFromTable);
		if (!strcmp(szNameFromTable, pNameFromEntry))
			break;
	}
	
	iResult = ToolValueGet(EXT_LIST, EXT_EXT, iId, szExt);
	if (iResult != SUCCESS)
		FatalError(__FILE__, __LINE__, __DATE__);
	
	/* read name from entry, add extension if needed */
	
	pFileNewNameEntry = GTK_ENTRY(lookup_widget(GTK_WIDGET(pFileNewWindow), "pFileNewNameEntry"));
	if (pFileNewNameEntry == NULL)
		FatalError(__FILE__, __LINE__, __DATE__);

	pNameFromEntry = gtk_entry_get_text(pFileNewNameEntry);
	if (pNameFromEntry == NULL)
		FatalError(__FILE__, __LINE__, __DATE__);
	
	strcpy(szFileNewName, pNameFromEntry);
	if (strcmp(FileGetExt(szFileNewName), szExt))
	{
		strcat(szFileNewName, ".");
		strcat(szFileNewName, szExt);
	}
}
		
		
static BOOL FileNewValid(void)
{
	if (strlen(FileGetName(szFileNewName)) == 0)
	{
		MsgBox(
			pWindowMain,
			"Error !",
			"Enter a name of file !",
			MSGBOX_ERROR | MSGBOX_OKD
			);
		
		return FALSE;
	}
	
	return TRUE;
}


void FileNew_ButtonOk(GtkButton *pButton, gpointer pUserData)
{
	bFileNewCompleted = TRUE;
}


void FileNew_ButtonCancel(GtkButton *pButton, gpointer pUserData)
{
	bFileNewCanceled = TRUE;
}


gboolean FileNew_Destroy(GtkWidget *pWidget, GdkEvent *pEvent, gpointer pUserData)
{
	bFileNewCanceled = TRUE;
	pFileNewWindow = NULL;
	return FALSE;
}



/*******************************************************************************

	MENU : File -> Import

*******************************************************************************/

/* callback from menu */
void FileImport_MenuActivation(GtkMenuItem *pMenuItem, gpointer pUserData)
{
	int iResult, iExtId;
	char szName[TEXTLEN], szExtension[TEXTLEN], *szRelName, pValue[TEXTLEN], szDocSelected[TEXTLEN], *szExt, szMessage[TEXTLEN], szParent[TEXTLEN];

	/* enter file name and location */
	strcpy(szName, "");
	strcpy(szExtension, "*");
	iResult = FileSelection(szExtension, szName);
	if (iResult != SUCCESS)
	{
		/* not an error, simply action cancelled */
		return;
	}

	/* check extension in database */
	szExt = FileGetExt(szName);
	iResult = ToolGetExtensionId(szExt, &iExtId);
	if (iResult != SUCCESS)
	{
		sprintf(szMessage, "Cannot import files of type '%s'", szExt);
		MsgBox(
			pWindowMain,
			"Error !",
			szMessage,
			MSGBOX_ERROR | MSGBOX_OKD
			);
		return;
	}
	iResult = ToolValueGet(EXT_LIST, EXT_PARENT, iExtId, szParent);
	if (iResult != SUCCESS)
	{
		sprintf(szMessage, "Fatal error in file '%s' at line %d", __FILE__, __LINE__);
		MsgBox(
			pWindowMain,
			"Fatal error !",
			szMessage,
			MSGBOX_FATAL | MSGBOX_OKD
			);
		return;
	}
	iResult = DocGetProperty(DOC_SELECTED, "", szDocSelected);
	if (iResult != SUCCESS)
	{
		sprintf(szMessage, "Fatal error in file '%s' at line %d", __FILE__, __LINE__);
		MsgBox(
			pWindowMain,
			"Fatal error !",
			szMessage,
			MSGBOX_FATAL | MSGBOX_OKD
			);
		return;
	}
	if (strcmp(szParent, FileGetExt(szDocSelected)) && strlen(szParent) > 0)
	{
		sprintf(szMessage, "File '%s' can be bind only to files of type '%s'", szName, szParent);
		MsgBox(
			pWindowMain,
			"Error !",
			szMessage,
			MSGBOX_ERROR | MSGBOX_OKD
			);
		return;
	}
	
	/* check if the file is existing */
	iResult = FileIsExisting(szName);
	if (iResult != SUCCESS)
	{
		/* error, file does exist */
		MsgBox(
			pWindowMain,
			"File does not exist.",
			szMessage,
			MSGBOX_ERROR | MSGBOX_OKD
			);
		return;
	}
	
	/* check if the file has been already added in project */
	iResult = DocGetProperty(DOC_FILENAME, szName, pValue);
	if (iResult == SUCCESS)
	{
		/* error, file in project */
		MsgBox(
			pWindowMain,
			"File already exists.",
			szMessage,
			MSGBOX_ERROR | MSGBOX_OKD
			);
		return;
	}
	
	/* add file to the project */
	szRelName = FileGetRel(szName);
	iResult = DocCreate(szRelName, strlen(szParent) > 0 ? szDocSelected : "");
	if (iResult != SUCCESS)
	{
		MsgBox(
			pWindowMain,
			"Error !",
			"Cannot import file.",
			MSGBOX_ERROR | MSGBOX_OKD
			);
		return;
	}
	ProjectSave();
}



/*******************************************************************************

	MENU : File -> Unlink

*******************************************************************************/

/* callback from menu */
void FileUnlink_MenuActivation(GtkMenuItem *pMenuItem, gpointer pUserData)
{
	int iResult;
	char szMessage[TEXTLEN], szName[TEXTLEN];
	
	/* get the file name */
	iResult = DocGetProperty(DOC_SELECTED, NULL, (void *) szName);
	if (iResult != SUCCESS)
		return;
	if (strlen(szName) == 0)
		return;
	
	/* accept unlink */
	sprintf(szMessage, "Do you want to remove file '%s' (and all dependend files) from project ?", szName);
	iResult = MsgBox(
		pWindowMain,
		"Question ...",
		szMessage,
		MSGBOX_QUESTION | MSGBOX_OKD | MSGBOX_CANCEL
		);
	if (iResult == MSGBOX_CANCEL)
		return;

	FileUnlink(szName);
}



/*******************************************************************************

	MENU : File -> Delete

*******************************************************************************/

/* callback from menu */
void FileDelete_MenuActivation(GtkMenuItem *pMenuItem, gpointer pUserData)
{
	int iResult;
	char szMessage[TEXTLEN], szName[TEXTLEN];

	/* get the file name */
	iResult = DocGetProperty(DOC_SELECTED, NULL, (void *) szName);
	if (iResult != SUCCESS)
		return;
	if (strlen(szName) == 0)
		return;

	/* accept delete */
	sprintf(szMessage, "Do you want to delete file '%s' (and all dependend files) from the disk ?", szName);
	iResult = MsgBox(
		pWindowMain,
		"Question ...",
		szMessage,
		MSGBOX_QUESTION | MSGBOX_OKD | MSGBOX_CANCEL
		);
	if (iResult == MSGBOX_CANCEL)
		return;

	FileDelete(szName);
	
	return;
}
