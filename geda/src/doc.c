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

#include <malloc.h>
#include <stdio.h>
#include <string.h>
#include "config.h"
#include "doc.h"
#include "filetool.h"
#include "global.h"
#include "m_action.h"
#include "msgbox.h"
#include "project.h"
#include "support.h"
#include "tool.h"



/* document list and current document name */
struct Doc_s *pDocList = NULL;
static char szDocCurrent[TEXTLEN] = "";



/* private functions */
static struct Doc_s *DocSearch(char *szFileName);

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
	struct Doc_s *pDoc = NULL, *pParent;
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
	ProjectChanged(TRUE);
	DocViewAdd(pDoc);
	
	return SUCCESS;
}


int DocDestroy(char *szFileName)
{
	struct Doc_s *pDoc = NULL, *pPrevious;
	int iResult;
	
	/* look for document */
	pDoc = DocSearch(szFileName);
	if (pDoc == NULL)
	{
		/* no such document */
		MsgBox("No such document", MSGBOX_OK);
		return FAILURE;
	}
	
	DocClose(pDoc);
	
	/* remove from the view tree */
	DocViewDelete(pDoc);
	
	/* remove from the list */
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
			MsgBox("Missing document", MSGBOX_OK);
		}
		
		pPrevious->pNext = pDoc->pNext;
	}
	if (strcmp(szDocCurrent, pDoc->szFileName) == 0)
	{
		strcpy(szDocCurrent, "");
		ProjectTitle();
	}
	free(pDoc);
	ProjectChanged(TRUE);
	
	return SUCCESS;
}



/*
	Openning and closing documents
*/

int DocOpen(char *szFileName, int iAction)
{
	struct Action_s *pAction;
	int iResult, i;
	char *szName, *szExt, *Params[2];
	
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
		Params[0] = pAction;
		Params[1] = szFileName;
		TaskNew(TASK_ACTION, Params);
	}
#if 0	
	/* extract name and extension */
	szName = FileGetName(szFileName);
	szExt = FileGetExt(szFileName);
	
	/* make the selected document current */
	ProjectTitle();
	
	/* open the file to edit */
	iResult = ToolGetExtensionId(szExt, &i);
	if (iResult != SUCCESS)
	{
		MsgBox("Cannot determine tool for this extension", MSGBOX_OK);
		return FAILURE;
	}
	iResult = ToolOpenFile(i, szFileName, TOOL_DEFAULT);
	if (iResult != SUCCESS)
	{
		MsgBox("Cannot open file", MSGBOX_OK);
		return FAILURE;
	}
#endif
	return SUCCESS;
}


int DocClose(char *szFileName)
{
	struct Doc_s *pDoc = NULL, *pPrevious;
	int iResult;
	char szMessage[TEXTLEN];
	
	/* look for document */
	pDoc = DocSearch(szFileName);
	if (pDoc == NULL)
	{
		/* no such document */
		MsgBox("No such document", MSGBOX_OK);
		return FAILURE;
	}
	if (pDoc->bChanged)
	{
		sprintf(szMessage, "File '%s' not saved.\n\nSave it now ?", szFileName);
		iResult = MsgBox(szMessage, MSGBOX_YESNOCANCEL);
		if (iResult == 2)
			return SUCCESS;
		else if (iResult == 0)
			FileSave(szFileName);
	}
	
	FileClose(szFileName);
	
	return SUCCESS;
}



/*
	Saving and loading project
*/

int DocSave(char *szFileName)
{
	FILE *hProject;
	struct Doc_s *pDoc;
	
	/* open project file */
	hProject = fopen(szFileName, "w");
	if (hProject == NULL)
	{
		MsgBox("ERROR ! Cannot open file", MSGBOX_OK);
		return FAILURE;
	}
	
	/* save file list */
	fprintf(hProject, "[FILES]\n");
	for (pDoc = pDocList; pDoc != NULL; pDoc = pDoc->pNext)
	{
		fprintf(hProject, "FILE = \"%s\", \"%s\"\n",
			pDoc->szFileName,
			(pDoc->pParent != NULL) ? pDoc->pParent->szFileName : ""
			);
	}
	
	/* close project file */
	fclose(hProject);
	ProjectChanged(FALSE);
	
	return SUCCESS;
}

int DocLoad(char *szFileName)
{
	int iResult, i, j, iNumber = 0;
	int bNoParent, bCreated;
	char szName[TEXTLEN], szValue[TEXTLEN], szDoc[TEXTLEN], szParent[TEXTLEN];
	char *FileTab[TEXTLEN], *ParentTab[TEXTLEN];
	
	/* open project file */
	iResult = ConfigOpen(szFileName);
	if (iResult != SUCCESS)
	{
		MsgBox("Cannot open project !", MSGBOX_OK);
		return FAILURE;
	}
	
	/* read FILE section */
	iResult = ConfigSection("FILES");
	if (iResult != SUCCESS)
	{
		MsgBox("Wrong project file !", MSGBOX_OK);
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
	
	/* close project file */
	ConfigClose();
	ProjectChanged(FALSE);
	
	return SUCCESS;
}


int DocGetProperty(int iProperty, char *szDoc, void *pValue)
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
			*((int *) pValue) = (pDoc != NULL) ? pDoc->bChanged : NULL;
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
static struct Doc_s *DocSearch(char *szFileName)
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



static void DocViewAdd(struct Doc_s *pDoc)
{
	pTreeModules = GTK_CTREE(lookup_widget(pWindowMain, "DocModulesTree"));
	pTreeFiles = GTK_CTREE(lookup_widget(pWindowMain, "DocFilesTree"));

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
	struct Doc_s *pParent;
	char *pNode[2];

	/* looking for parent node */
	for (pParent = pDocList; pParent != NULL; pParent = pParent->pNext)
	{
		if (pDoc->pParent == pParent)
			break;
	}

	/* inserting node */
	pNode[0] = pDoc->szFileName;
	pNode[1] = NULL;
	pDoc->pNodeModules = gtk_ctree_insert_node(
		pTreeModules,
		(pDoc->pParent == pParent && pParent != NULL) ? pParent->pNodeModules : NULL,
		NULL,
		pNode,
		1,
		NULL,
		NULL,
		NULL,
		NULL,
		FALSE,
		TRUE
		);
	gtk_ctree_sort_node(pTreeModules, pDoc->pNodeModules);
}



static void DocViewModulesDelete(struct Doc_s *pDoc)
{
	gtk_ctree_remove_node(pTreeModules, pDoc->pNodeModules);
}



static void DocViewModulesSort(void)
{
	struct Doc_s *pDoc, *pSibling, *pPtr;

	for (pDoc = pDocList; pDoc != NULL; pDoc = pDoc->pNext)
	{
		for (pSibling = NULL, pPtr = pDocList; pPtr != NULL; pPtr = pPtr->pNext)
		{
			if (pPtr == pDoc)
				continue;
			if (pPtr->pParent != pDoc->pParent)
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
			(pDoc->pParent != NULL) ? pDoc->pParent->pNodeModules : NULL, 
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
	pTreeModules = GTK_CTREE(lookup_widget(pWindowMain, "DocModulesTree"));
	if (pTreeModules == NULL)
		return;
	pTreeFiles = GTK_CTREE(lookup_widget(pWindowMain, "DocFilesTree"));
	if (pTreeFiles == NULL)
		return;

	/* look for the document */
	for (pDoc = pDocList; pDoc != NULL; pDoc = pDoc->pNext)
	{
		if ((GtkCTreeNode *) node == ((pTree == pTreeModules) ? pDoc->pNodeModules : pDoc->pNodeFiles))
			break;
	}
	if (pDoc == NULL)
		return;
	
	/* select the same document on different views */
	bBlocked = TRUE;
	gtk_ctree_select(pTreeModules, pDoc->pNodeModules);
	gtk_ctree_select(pTreeFiles, pDoc->pNodeFiles);
	while (g_main_iteration(FALSE));
	bBlocked = FALSE;
	
	/* make the selected document current */
	strcpy(szDocCurrent, pDoc->szFileName);
	ProjectTitle();
	
	/* create appropiate action menu */
	szExt = FileGetExt(pDoc->szFileName);
	MenuActionRefresh(szExt);
	MenuFileNewRefresh(szExt);
	
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
