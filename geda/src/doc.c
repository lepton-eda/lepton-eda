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
#else
#define VERSION "devel"
#endif

#include <stdlib.h>
#include <stdio.h>

#ifdef HAVE_STRING_H  
#include <string.h>
#endif

#include <sys/stat.h>
#include <sys/types.h>

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "configfile.h"
#include "doc.h"
#include "file.h"
#include "filetool.h"
#include "global.h"
#include "m_action.h"
#include "m_project.h"
#include "msgbox.h"
#include "project.h"
#include "support.h"
#include "tool.h"
#include "txtedit.h"
#include "window.h"



/* document list and current document name */
struct Doc_s *pDocList = NULL;
static char szDocCurrent[TEXTLEN] = "";



/* private functions */
static struct Doc_s *DocSearch(const char *szFileName);

static void DocViewAdd(struct Doc_s *pDoc);
static void DocViewDelete(struct Doc_s *pDoc);
static void DocViewSort(void);
static void DocViewModulesAdd(struct Doc_s *pDoc);
static void DocViewModulesDelete(struct Doc_s *pDoc);
static void DocViewModulesSort(void);
static void DocViewFilesAdd(struct Doc_s *pDoc);
static void DocViewFilesDelete(struct Doc_s *pDoc);
static void DocViewFilesSort(void);
	


/*
	Creating and destroying documents
*/

int DocCreate(const char *szFileName, const char *szParentFileName)
{
	struct Doc_s *pDoc = NULL, *pParent = NULL;
	int iResult;

	pDoc = DocSearch(szFileName);
	if (pDoc != NULL)
		return FAILURE;
	iResult = FileIsExisting(szFileName);
	if (iResult != SUCCESS)
		return FAILURE;
	if (strlen(szParentFileName) > 0)
	{
		pParent = DocSearch(szParentFileName);
		if (pParent == NULL)
			return SUCCESS;
	}

	pDoc = (struct Doc_s *) malloc(sizeof(struct Doc_s));
	if (pDoc == NULL)
		return FAILURE;
	strcpy(pDoc->szFileName, szFileName);
	pDoc->pParent = (strlen(szParentFileName) > 0)
		? pParent
		: NULL;
		
	if (pDocList == NULL)
	{
		pDoc->pNext = NULL;
		pDocList = pDoc;
	}
	else
	{
		pDoc->pNext = pDocList;
		pDocList = pDoc;
	}
	pDoc->bChanged = FALSE;
	DocViewAdd(pDoc);

	return SUCCESS;
}


int DocDestroy(const char *szFileName)
{
	struct Doc_s *pDoc = NULL, *pPrevious;

	/* look for the document on the list, close it and remove from viewes */
	pDoc = DocSearch(szFileName);
	if (pDoc == NULL)
		return FAILURE;
	DocClose(pDoc->szFileName);
	DocViewDelete(pDoc);

	/* remove the document from the list */
	if (pDoc == pDocList)
	{
		pDocList = pDoc->pNext;
	}
	else
	{
		for (pPrevious = pDocList; pPrevious != NULL; pPrevious = pPrevious->pNext)
		{
			if (pPrevious->pNext == pDoc)
				break;
		}

		if (pPrevious == NULL)
		{
			/* impossible condition */
		}

		pPrevious->pNext = pDoc->pNext;
	}
	if (strcmp(szDocCurrent, pDoc->szFileName) == 0)
	{
		strcpy(szDocCurrent, "");
		ProjectTitle();
	}
	free(pDoc);

	return SUCCESS;
}



/*
	Openning and closing documents
*/

int DocOpen(const char *szFileName, const int iAction)
{
	struct Action_s *pAction;
	char *Params[2];
	
	for (pAction = pActionList; pAction != NULL; pAction = pAction->pNext)
	{
		if (strcmp(pAction->szExt, FileGetExt(szFileName)))
			continue;
		
		if (pAction->fFlags & ACTION_DEFAULT)
			break;
	}
	if (pAction == NULL)
		return FAILURE;

	if (pAction->fFlags & TASK_INTERNAL)
		FileEdit(szFileName);
	else
	{
		Params[0] = (char *) pAction;
		Params[1] = (char *) szFileName;
		TaskNew(TASK_ACTION, (const void **) Params);
	}

	return SUCCESS;
}


int DocClose(const char *szFileName)
{
	struct Doc_s *pDoc = NULL;
	int iResult;
	char szMessage[TEXTLEN];
	
	/* look for the document, close it */
	pDoc = DocSearch(szFileName);
	if (pDoc == NULL)
		return FAILURE;
	if (pDoc->bChanged)
	{
		sprintf(szMessage, "File '%s' not saved.\n\nSave it now ?", szFileName);
		iResult = MsgBox(
			pWindowMain,
			"Question ...",
			szMessage,
			MSGBOX_QUESTION | MSGBOX_YESD | MSGBOX_NO
			);
		if (iResult == MSGBOX_YES)
			FileSave(szFileName);
	}
	FileClose(szFileName);
	
	return SUCCESS;
}



/*******************************************************************************

	Saving and loading projects

*******************************************************************************/

int DocSave(const char *szFileName)
{
	FILE *hProject;
	struct Doc_s *pDoc;
	char szMessage[TEXTLEN], szPwd[TEXTLEN], szDirName[TEXTLEN], szFullDest[TEXTLEN], *pResult;
	int iResult, i, j;

	/* read current working directory */
	pResult = getcwd(szPwd, TEXTLEN);
	if (pResult == NULL)
	{
		sprintf(szMessage, "Cannot determine current working directory (Package::FileCopy())");
		return FAILURE;
	}

strcpy(szFullDest, szFileName);
	
	/* creating directories */
	for (i = 0, j = 0; i < strlen(szFullDest); )
	{
		/* get destination directory name */
		for (; i < strlen(szFullDest) && szFullDest[i] != G_DIR_SEPARATOR; i ++)
			szDirName[j ++] = szFullDest[i];
		for (; i < strlen(szFullDest) && szFullDest[i] == G_DIR_SEPARATOR; i ++)
			szDirName[j ++] = szFullDest[i];
		szDirName[j] = 0;
		
		/* if end of string - it is not a directory */
		if (i == strlen(szFullDest))
			break;
		
		/* check if it exists */
		iResult = chdir(szDirName);
		if (iResult != 0)
		{
			/* if not - create it */
			iResult = mkdir(szDirName, 0777);
			if (iResult != 0)
			{
				sprintf(szMessage, "Cannot make directory '%s'",
					szDirName
					);
//				Log(LOG_ERROR, MODULE_PACKAGE, szMessage);
				return FAILURE;
			}
		}

		/* return to the working directory */
		iResult = chdir(szPwd);
		if (iResult != 0)
		{
			sprintf(szMessage,
				"Cannot return to the working directory '%s'",
				szPwd
				);
//			Log(LOG_ERROR, MODULE_PACKAGE, szMessage);
			return FAILURE;
		}
	}
	

	/* 
		open project file for writing
	*/
	
	hProject = fopen(szFileName, "w");
	if (hProject == NULL)
		return FAILURE;
	
	
	/* 
		save GManager section
	*/
	
	fprintf(hProject, "[GMANAGER]\n");
	fprintf(hProject, "VERSION = \"%s\"\n", VERSION);
	fprintf(hProject, "\n");
	
	
	/* 
		save project properties
	*/

	fprintf(hProject, "[PROJECT]\n");
	fprintf(hProject, "AUTHOR = \"%s\"\n", Project.szAuthor);
	fprintf(hProject, "DESC = \"%s\"\n", Project.szDesc);
	fprintf(hProject, "\n");
	
	
	/* 
		save file list 
	*/
	
	fprintf(hProject, "[FILES]\n");
	for (pDoc = pDocList; pDoc != NULL; pDoc = pDoc->pNext)
	{
		fprintf(hProject, "FILE = \"%s\", \"%s\"\n",
			pDoc->szFileName,
			(pDoc->pParent != NULL) ? pDoc->pParent->szFileName : ""
			);
	}
	fprintf(hProject, "\n");
	
	
	/* 
		close project file 
	*/
	
	fclose(hProject);
	
	
	return SUCCESS;
}


int DocLoad(const char *szFileName)
{
	int iResult, i, j, iNumber = 0;
	int bNoParent, bCreated;
	char szName[TEXTLEN], szValue[TEXTLEN], szDoc[TEXTLEN], szParent[TEXTLEN], szMessage[TEXTLEN];
	char *FileTab[TEXTLEN], *ParentTab[TEXTLEN];
	BOOL bOldVersion = FALSE; /* marker that loaded file is an old one */
	
	
	/* 
		open a project file for reading
	*/
	
	iResult = ConfigOpen(szFileName);
	if (iResult != SUCCESS)
	{
		sprintf(szMessage, "Cannot open the project file '%s' !", szFileName);
		MsgBox(
			pWindowMain,
			"Error !",
			szMessage,
			MSGBOX_ERROR | MSGBOX_OKD
			);
		return FAILURE;
	}
	
	
	/* 
		read GMANAGER section 
	*/
	
	iResult = ConfigSection("GMANAGER");
	if (iResult != SUCCESS)
	{
		bOldVersion = TRUE;
		/* TODO: set deafults as appropriate */
	}
	for (iResult = ConfigGetNext(szName, szValue); iResult == SUCCESS; iResult = ConfigGetNext(szName, szValue))
	{
		if (!strcmp(szName, "VERSION"))
		{
			/* read file name */
			for (i = 0; i < strlen(szValue) && szValue[i] != '"'; i ++)
				;
			i ++;
			for (j = 0; i < strlen(szValue) && szValue[i] != '"'; i ++)
				szDoc[j ++] = szValue[i];
			szDoc[j] = 0;
			i ++;
			
			/* TODO: what todo with version ??? */
		}
	}


	/* 
		read PROJECT section 
	*/
	
	iResult = ConfigSection("PROJECT");
	if (iResult != SUCCESS)
	{
		bOldVersion = TRUE;

		strcpy(Project.szAuthor, "");
		strcpy(Project.szDesc, "");
	}
	for (iResult = ConfigGetNext(szName, szValue); iResult == SUCCESS; iResult = ConfigGetNext(szName, szValue))
	{
		if (!strcmp(szName, "AUTHOR"))
		{
			/* read file name */
			for (i = 0; i < strlen(szValue) && szValue[i] != '"'; i ++)
				;
			i ++;
			for (j = 0; i < strlen(szValue) && szValue[i] != '"'; i ++)
				szDoc[j ++] = szValue[i];
			szDoc[j] = 0;
			i ++;
			
			strcpy(Project.szAuthor, szDoc);
		}

		else if (!strcmp(szName, "DESC"))
		{
			/* read file name */
			for (i = 0; i < strlen(szValue) && szValue[i] != '"'; i ++)
				;
			i ++;
			for (j = 0; i < strlen(szValue) && szValue[i] != '"'; i ++)
				szDoc[j ++] = szValue[i];
			szDoc[j] = 0;
			i ++;
			
			strcpy(Project.szDesc, szDoc);
		}
	}


	/* 
		read file list 
	*/
	
	iResult = ConfigSection("FILES");
	if (iResult != SUCCESS)
	{
		sprintf(szMessage, "'%s' is not a valid project file !", szFileName);
		MsgBox(
			pWindowMain,
			"Error !",
			szMessage,
			MSGBOX_ERROR | MSGBOX_OKD
			);
		ConfigClose();
		return FAILURE;
	}
	for (iResult = ConfigGetNext(szName, szValue); iResult == SUCCESS; iResult = ConfigGetNext(szName, szValue))
	{
		if (!strcmp(szName, "FILE"))
		{
			/* read file name */
			for (i = 0; i < strlen(szValue) && szValue[i] != '"'; i ++)
				;
			i ++;
			for (j = 0; i < strlen(szValue) && szValue[i] != '"'; i ++)
				szDoc[j ++] = szValue[i];
			szDoc[j] = 0;
			i ++;
			
			/* read parent file name */
			for (; i < strlen(szValue) && szValue[i] != '"'; i ++)
				;
			i ++;
			for (j = 0; i < strlen(szValue) && szValue[i] != '"'; i ++)
				szParent[j ++] = szValue[i];
			szParent[j] = 0;
			i ++;

			FileTab[iNumber] = malloc(strlen(szDoc) + 1);
			strcpy(FileTab[iNumber], szDoc);
			ParentTab[iNumber] = malloc(strlen(szParent) + 1);
			strcpy(ParentTab[iNumber], szParent);
			iNumber ++;
		}
	}

	bNoParent = TRUE, bCreated = TRUE;
	while (bNoParent && bCreated)
	{
		bNoParent = FALSE;
		bCreated = FALSE;
		
		for (i = 0; i < iNumber; i ++)
		{
			if (FileTab[i] != NULL || (ParentTab[i] != NULL ? (strlen(ParentTab[i]) == 0) : FALSE))
			{
				if (DocSearch(ParentTab[i]) != NULL || strlen(ParentTab[i]) == 0)
				{
					DocCreate(FileTab[i], ParentTab[i]);
//					ProjectSave();
					
					free(FileTab[i]);
					free(ParentTab[i]);

					FileTab[i] = NULL;
					ParentTab[i] = NULL;
					
					bCreated = TRUE;
				}
				
				else
					bNoParent = TRUE;
			}
		}
	}
	
	
	/* 
		close project file 
	*/
	
	ConfigClose();
	

	/*
		convert old files to a new version
	*/	
	
	if (bOldVersion)
		ProjectProperties(FALSE);
	

	return SUCCESS;
}


int DocGetProperty(const int iProperty, const char *szDoc, void *pValue)
{
	struct Doc_s *pDoc;

	/* get the value */
	switch (iProperty)
	{
		case DOC_FILENAME:   

			pDoc = DocSearch((char *) szDoc/*pValue*/);
			strcpy((char *) pValue, (pDoc != NULL) ? pDoc->szFileName : "");
			if (pDoc == NULL)
				return FAILURE;
			break;
		
		case DOC_NEXT:
			
			/* look for the first document */
			if (strlen(szDoc) == 0)
			{
				strcpy((char *) pValue, (pDocList != NULL) ? pDocList->szFileName : "");
				if (pDocList == NULL)
					return FAILURE;
			}
			
			/* look for one of the next documents */
			else
			{
				pDoc = DocSearch(szDoc);
				if (pDoc == NULL)
				{
					/* no such document */
					strcpy((char *) pValue, "");
					return FAILURE;
				}
				
				pDoc = pDoc->pNext;
				if (pDoc == NULL)
				{
					/* no next document */
					strcpy((char *) pValue, "");
					return FAILURE;
				}
				
				strcpy((char *) pValue, pDoc->szFileName);
			}

			break;
		
		case DOC_SELECTED:
			
			strcpy((char *) pValue, szDocCurrent);
			break;
		
		case DOC_PARENT:   

			pDoc = DocSearch(szDoc);
			strcpy((char *) pValue, (pDoc != NULL) ? (pDoc->pParent != NULL ? pDoc->pParent->szFileName : "") : "");
			if (pDoc == NULL)
				return FAILURE;
			break;
		
		case DOC_CHANGED:   

			pDoc = DocSearch(szDoc);
			*((int *) pValue) = (pDoc != NULL) ? pDoc->bChanged : 0;
			if (pDoc == NULL)
				return FAILURE;
			break;
		
		default:             
		
			return FALSE;
	}
	
	return SUCCESS;
}


/*
	Private functions
*/

/* look for document */
static struct Doc_s *DocSearch(const char *szFileName)
{
	struct Doc_s *pPtr;
	
	for (pPtr = pDocList; pPtr != NULL; pPtr = pPtr->pNext)
	{
		if (!strcmp(szFileName, pPtr->szFileName))
			break;
	}

	return pPtr;
}



/*
	Document Views
*/

static GtkCTree *pTreeModules, *pTreeFiles;



void DocViewInitialize(void)
{
	GtkCTreeNode *pNode;
	char *pNameNode[2], pName[TEXTLEN];
	int iId;

	pTreeModules = GTK_CTREE(lookup_widget(GTK_WIDGET(pWindowMain), "DocModulesTree"));
	pTreeFiles = GTK_CTREE(lookup_widget(GTK_WIDGET(pWindowMain), "DocFilesTree"));

	/* create and show file groups */
	for (
		ToolValueGet(GROUP_LIST, GROUP_NEXT, GROUP_NONE, &iId);
		iId != GROUP_NONE;
		ToolValueGet(GROUP_LIST, GROUP_NEXT, iId, &iId)
		)
	{
		ToolValueGet(GROUP_LIST, GROUP_NAME, iId, pName);
		pNameNode[0] = pName;
		pNameNode[1] = NULL;

		pNode = gtk_ctree_insert_node(
			pTreeModules,
			NULL,
			NULL,
			pNameNode,
			1,
			NULL,
			NULL,
			NULL,
			NULL,
			FALSE,
			TRUE
			);

		ToolValueSet(GROUP_LIST, GROUP_NODE, iId, &pNode);
	}
}



void DocViewRelease(void)
{
	GtkCTreeNode *pNode = NULL;
	int iId;

	for (
		ToolValueGet(GROUP_LIST, GROUP_NEXT, GROUP_NONE, &iId);
		iId != GROUP_NONE;
		ToolValueGet(GROUP_LIST, GROUP_NEXT, iId, &iId)
		)
	{
		ToolValueGet(GROUP_LIST, GROUP_NODE, iId, pNode);
		gtk_ctree_remove_node(pTreeModules, pNode);
	}
}



static void DocViewAdd(struct Doc_s *pDoc)
{
	DocViewModulesAdd(pDoc);
	DocViewFilesAdd(pDoc);

	DocViewSort();
}



static void DocViewDelete(struct Doc_s *pDoc)
{
	DocViewModulesDelete(pDoc);
	DocViewFilesDelete(pDoc);
}



static void DocViewSort(void)
{
	DocViewModulesSort();
	DocViewFilesSort();
}



static void DocViewModulesAdd(struct Doc_s *pDoc)
{
	char *pNameNode[2];

	/* insert node into modules tree */
	pNameNode[0] = pDoc->szFileName;
	pNameNode[1] = NULL;
	pDoc->pNodeModules = gtk_ctree_insert_node(
		pTreeModules,
		NULL,
		NULL,
		pNameNode,
		1,
		NULL,
		NULL,
		NULL,
		NULL,
		FALSE,
		TRUE
		);
}



static void DocViewModulesDelete(struct Doc_s *pDoc)
{
	gtk_ctree_remove_node(pTreeModules, pDoc->pNodeModules);
	pDoc->pNodeModules = NULL;
}



static void DocViewModulesSort(void)
{
	struct Doc_s *pDoc, *pSibling, *pPtr, *pParent;
	GtkCTreeNode *pNodeParent, *pNode;
	int iGroupId, iResult, i;
	char szExt[TEXTLEN];

	for (pDoc = pDocList; pDoc != NULL; pDoc = pDoc->pNext)
	{
		/* looking for parent doc */
		for (pParent = pDocList; pParent != NULL; pParent = pParent->pNext)
		{
			if (pDoc->pParent == pParent)
				break;
		}

		/* determine parent node */
		if (pParent != NULL)
			pNodeParent = pParent->pNodeModules;
		else
		{
			for (i = 0, iResult = SUCCESS; iResult == SUCCESS; i ++)
			{
				iResult = ToolValueGet(EXT_LIST, EXT_EXT, i, szExt);
				if (iResult != SUCCESS)
					break;
		
				if (!strcmp(szExt, FileGetExt(pDoc->szFileName)))
					break;
			}
			if (iResult != SUCCESS)
				break;
			
			iResult = ToolValueGet(EXT_LIST, EXT_GROUPID, i, &iGroupId);
			if (iResult != SUCCESS)
				FatalError(__FILE__, __LINE__, __DATE__);
			
			pNodeParent = NULL;
			iResult = ToolValueGet(GROUP_LIST, GROUP_NODE, iGroupId, &pNodeParent);
			if (iResult == FAILURE)
				pNodeParent = NULL;
		}

		/* determine sibling node */
		for (pSibling = NULL, pPtr = pDocList; pPtr != NULL /*&& pParent != NULL*/; pPtr = pPtr->pNext)
		{
			if (pPtr == pDoc)
				continue;
			if (pPtr->pParent != pDoc->pParent)
				continue;

			/* check groups for main files */
			if (pParent == NULL)
			{
				for (i = 0, iResult = SUCCESS; iResult == SUCCESS; i ++)
				{
					iResult = ToolValueGet(EXT_LIST, EXT_EXT, i, szExt);
					if (iResult != SUCCESS)
						break;
		
					if (!strcmp(szExt, FileGetExt(pPtr->szFileName)))
						break;
				}
				if (iResult != SUCCESS)
					break;
			
				iResult = ToolValueGet(EXT_LIST, EXT_GROUPID, i, &iGroupId);
				if (iResult != SUCCESS)
					FatalError(__FILE__, __LINE__, __DATE__);

				iResult = ToolValueGet(GROUP_LIST, GROUP_NODE, iGroupId, &pNode);
				if (iResult == FAILURE)
					pNode = NULL;
			}
			if (pNode != pNodeParent)
				continue;

			if (strcmp(pPtr->szFileName, pDoc->szFileName) < 0)
				continue;

			if (pSibling != NULL)
			{
				if (strcmp(pPtr->szFileName, pSibling->szFileName) < 0)
					pSibling = pPtr;
			}

			else
			{
				pSibling = pPtr;
			}
		}

		gtk_ctree_move(
			pTreeModules,
			pDoc->pNodeModules,
			pNodeParent,
			(pSibling != NULL) ? pSibling->pNodeModules : NULL
			);
	}
}



static void DocViewFilesAdd(struct Doc_s *pDoc)
{
	char *pNode[2];

	/* inserting node */
	pNode[0] = pDoc->szFileName;
	pNode[1] = NULL;
	pDoc->pNodeFiles = gtk_ctree_insert_node(
		pTreeFiles,
		NULL,
		NULL,
		pNode,
		1,
		NULL,
		NULL,
		NULL,
		NULL,
		FALSE,
		FALSE
		);
	gtk_ctree_sort_node(pTreeFiles, pDoc->pNodeFiles);
}



static void DocViewFilesDelete(struct Doc_s *pDoc)
{
	gtk_ctree_remove_node(pTreeFiles, pDoc->pNodeFiles);
}



static void DocViewFilesSort(void)
{
	struct Doc_s *pDoc, *pSibling, *pPtr;

	for (pDoc = pDocList; pDoc != NULL; pDoc = pDoc->pNext)
	{
		for (pSibling = NULL, pPtr = pDocList; pPtr != NULL; pPtr = pPtr->pNext)
		{
			if (pPtr == pDoc)
				continue;
			if (strcmp(pPtr->szFileName, pDoc->szFileName) < 0)
				continue;
			
			if (pSibling != NULL)
			{
				if (strcmp(pPtr->szFileName, pSibling->szFileName) < 0)
					pSibling = pPtr;
			}
			
			else
			{
				pSibling = pPtr;
			}
		}
		
		gtk_ctree_move(
			pTreeFiles, 
			pDoc->pNodeFiles, 
			NULL, 
			(pSibling != NULL) ? pSibling->pNodeFiles : NULL
			);
	}
}



void Doc_Selection(GtkCTree *pTree, GList *node, gint column, gpointer user_data)
{
	static int bBlocked = FALSE;
	
	struct Doc_s *pDoc;
	GtkCTree *pTreeModules, *pTreeFiles;
	int iResult;
	char *szExt;

	if (bBlocked)
		return;
	
	/* look for project tree */
	pTreeModules = GTK_CTREE(lookup_widget(GTK_WIDGET(pWindowMain), "DocModulesTree"));
	if (pTreeModules == NULL)
		return;
	pTreeFiles = GTK_CTREE(lookup_widget(GTK_WIDGET(pWindowMain), "DocFilesTree"));
	if (pTreeFiles == NULL)
		return;

	/* look for the document */
	for (pDoc = pDocList; pDoc != NULL; pDoc = pDoc->pNext)
	{
		if ((GtkCTreeNode *) node == ((pTree == pTreeModules) ? pDoc->pNodeModules : pDoc->pNodeFiles))
			break;
	}
	if (pDoc == NULL)
	{
		/* unselect current file */
		strcpy(szDocCurrent, "");

		/* create appropiate action menu */
		MenuActionRefresh("");
	}
	else
	{
		/* select the same document on different views */
		bBlocked = TRUE;
		gtk_ctree_select(pTreeModules, pDoc->pNodeModules);
		gtk_ctree_select(pTreeFiles, pDoc->pNodeFiles);
		while (g_main_iteration(FALSE));
		bBlocked = FALSE;
	
		/* make the selected document current */
		strcpy(szDocCurrent, pDoc->szFileName);

		/* create appropiate action menu */
		szExt = FileGetExt(pDoc->szFileName);
		MenuActionRefresh(szExt);
	}
	
	ProjectTitle();
	
	iResult = (pDoc != NULL)
		? SUCCESS
		: FAILURE;
}



gboolean
on_DocModulesTree_button_press_event   (GtkWidget       *widget,
                                        GdkEventButton  *event,
                                        gpointer         user_data)
{
	
	if (event->type != GDK_2BUTTON_PRESS)
		return FALSE;

	gmanager_window_select(szDocCurrent);
	DocOpen(szDocCurrent, 0);

	return FALSE;
}
