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
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include "file.h"
#include "filesel.h"
#include "filetool.h"
#include "doc.h"
#include "msgbox.h"
#include "tool.h"
#include "txtedit.h"
#include "window.h"

/*
	Menu FILE handlers
	(Edit, Save, Print, Close, New, Import, Unlink, Delete)
*/
static void FileDelete(const char *szFilename);
static void FileUnlink(const char *szFilename);

void FileEdit(const char *szPath)
{
	EditOpen(szPath);
}



void FileSave(const char *szPath)
{
	EditSave(szPath);
}


void FilePrint(const char *szPath)
{
	EditPrint(szPath);
}



void FileClose(const char *szPath)
{
	EditClose(szPath);
}



void MenuFileEdit_Activation(GtkMenuItem *pMenuItem, gpointer pUserData)
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


void MenuFileSave_Activation(GtkMenuItem *pMenuItem, gpointer pUserData)
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


void MenuFilePrint_Activation(GtkMenuItem *pMenuItem, gpointer pUserData)
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


void MenuFileClose_Activation(GtkMenuItem *pMenuItem, gpointer pUserData)
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


/* TODO: missing MenuFileNew_Activation() */


void MenuFileImport_Activation(GtkMenuItem *pMenuItem, gpointer pUserData)
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
}


void MenuFileUnlink_Activation(GtkMenuItem *pMenuItem, gpointer pUserData)
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


void MenuFileDelete_Activation(GtkMenuItem *pMenuItem, gpointer pUserData)
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
	if (iResult == 1)
		return;

	return;
}


static void FileDelete(const char *szFilename)
{
	char szMessage[TEXTLEN], szName[TEXTLEN], szParent[TEXTLEN];
	int iResult;

	/* delete all children of the file */
	iResult = SUCCESS;
	strcpy(szName, "");
	while (iResult == SUCCESS)
	{
		iResult = DocGetProperty(DOC_NEXT, szName, (void *) szName);
		if (iResult != SUCCESS)
			break;

		iResult = DocGetProperty(DOC_PARENT, szName, (void *) szParent);
		if (iResult != SUCCESS)
			break;

		if (!strcmp(szParent, szFilename))
			FileDelete(szName);
	}

	/* close the file */
	iResult = DocClose(szFilename);
	if (iResult != SUCCESS)
	{
		sprintf(szMessage, "Cannot close file '%s'", szFilename);
		MsgBox(
			pWindowMain,
			"Error !",
			szMessage,
			MSGBOX_ERROR | MSGBOX_OKD
			);
		return;
	}

	/* remove file from document list */
	iResult = DocDestroy(szFilename);
	if (iResult != SUCCESS)
	{
		sprintf(szMessage, "Cannot remove file '%s' from project", szFilename);
		MsgBox(
			pWindowMain,
			"Error !",
			szMessage,
			MSGBOX_ERROR | MSGBOX_OKD
			);
		return;
	}

	/* delete file from disk */
	iResult = unlink(szFilename);
	if (iResult != SUCCESS)
	{	sprintf(szMessage, "Cannot delete file '%s'", szFilename);
		MsgBox(
			pWindowMain,
			"Error !",
			szMessage,
			MSGBOX_ERROR | MSGBOX_OKD
			);
		return;
	}
}


static void FileUnlink(const char *szFilename)
{
	char szMessage[TEXTLEN], szName[TEXTLEN], szParent[TEXTLEN];
	int iResult;

	/* delete all children of the file */
	iResult = SUCCESS;
	strcpy(szName, "");
	while (iResult == SUCCESS)
	{
		iResult = DocGetProperty(DOC_NEXT, szName, (void *) szName);
		if (iResult != SUCCESS)
			break;

		iResult = DocGetProperty(DOC_PARENT, szName, (void *) szParent);
		if (iResult != SUCCESS)
			break;

		if (!strcmp(szParent, szFilename))
			FileUnlink(szName);
	}

	/* close the file */
	iResult = DocClose(szFilename);
	if (iResult != SUCCESS)
	{
		sprintf(szMessage, "Cannot close file '%s'", szFilename);
		MsgBox(
			pWindowMain,
			"Error !",
			szMessage,
			MSGBOX_ERROR | MSGBOX_OKD
			);
		return;
	}

	/* remove file from document list */
	iResult = DocDestroy(szFilename);
	if (iResult != SUCCESS)
	{
		sprintf(szMessage, "Cannot remove file '%s' from project", szFilename);
		MsgBox(
			pWindowMain,
			"Error !",
			szMessage,
			MSGBOX_ERROR | MSGBOX_OKD
			);
		return;
	}
}

















int FileNew(int iExtId)
{
	FILE *hFile;
	int iResult, iActionId;
	char szTemplate[TEXTLEN], szName[TEXTLEN], *szExt, szExtension[TEXTLEN], *szRelName, pValue[TEXTLEN], szDocSelected[TEXTLEN], szParent[TEXTLEN];

	/* check if the file has been already added in project */
	iResult = DocGetProperty(DOC_FILENAME, szName, pValue);
	if (iResult == SUCCESS)
	{
		/* error, file in project */
		MsgBox(
			pWindowMain,
			"Error !",
			"File already exists.",
			MSGBOX_ERROR | MSGBOX_OKD
			);
		return FAILURE;
	}

	/* read extension */
	iResult = ToolValueGet(EXT_LIST, EXT_EXT, iExtId, (void *) szExtension);
	if (iResult != SUCCESS)
	{
		MsgBox(
			pWindowMain,
			"Error !",
			"Cannot read file extension",
			MSGBOX_ERROR | MSGBOX_OKD
			);
		return FAILURE;
	}

	/* read extension of parent */
	iResult = ToolValueGet(EXT_LIST, EXT_PARENT, iExtId, (void *) szParent);
	if (iResult != SUCCESS)
	{
		MsgBox(
			pWindowMain,
			"Error !",
			"Cannot read parent extension",
			MSGBOX_ERROR | MSGBOX_OKD
			);
		return FAILURE;
	}

	/* read template */
	iResult = ToolValueGet(EXT_LIST, EXT_TEMPLATE, iExtId, (void *) szTemplate);
	if (iResult != SUCCESS)
	{
		MsgBox(
			pWindowMain,
			"Error !",
			"Cannot read file template",
			MSGBOX_ERROR | MSGBOX_OKD
			);
		return FAILURE;
	}

	/* read default action */
	iResult = ToolValueGet(EXT_LIST, EXT_ACTIONID, iExtId, (void *) &iActionId);
	if (iResult != SUCCESS)
	{
		MsgBox(
			pWindowMain,
			"Error !",
			"Cannot determine file extension",
			MSGBOX_ERROR | MSGBOX_OKD
			);
		return FAILURE;
	}

	/* enter file name and location */
	strcpy(szName, "unnamed");
	strcat(szName, ".");
	strcat(szName, szExtension);
	iResult = FileSelection(szExtension, szName);
	if (iResult != SUCCESS)
	{
		/* not an error, simply action cancelled */
		return FAILURE;
	}
	szExt = FileGetExt(szName);
	if (strcmp(szExt, szExtension))
	{
		strcat(szName, ".");
		strcat(szName, szExtension);
	}

	/* check if the file is existing */
	iResult = FileIsExisting(szName);
	if (iResult == SUCCESS)
	{
		/* error, file does exist */
		MsgBox(
			pWindowMain,
			"Error !",
			"File already exists",
			MSGBOX_ERROR | MSGBOX_OKD
			);
		return FAILURE;
	}

	/* create file from template */
	hFile = fopen(szName, "w");
	if (hFile == NULL)
	{
		MsgBox(
			pWindowMain,
			"Error !",
			"Cannot copy template",
			MSGBOX_ERROR | MSGBOX_OKD
			);
		return FAILURE;
	}
	fprintf(hFile, "%s", szTemplate);
	fclose(hFile);

	/* check if the file is existing */
	iResult = FileIsExisting(szName);
	if (iResult != SUCCESS)
	{
		/* error, file does exist */
		MsgBox(
			pWindowMain,
			"Error !",
			"File does not exist",
			MSGBOX_ERROR | MSGBOX_OKD
			);
		return FAILURE;
	}

	DocGetProperty(DOC_SELECTED, "", szDocSelected);

	/* create new file and add it to the project */
	szRelName = FileGetRel(szName);
	iResult = DocCreate(szRelName, strlen(szParent) ? szDocSelected : "");
	if (iResult != SUCCESS)
	{
		MsgBox(
			pWindowMain,
			"Error !",
			"Cannot create document",
			MSGBOX_ERROR | MSGBOX_OKD
			);
		return FAILURE;
	}

	return SUCCESS;
}
