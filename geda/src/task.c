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
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>
#include "doc.h"
#include "filetool.h"
#include "global.h"
#include "m_action.h"
#include "m_window.h"
#include "support.h"
#include "task.h"
#include "tool.h"



#define TASK_IMPORT          257
#define TASK_EXTCMD          258
#define TASK_INTCMD          259

#define TASK_MSGBOX_WIDTH    224
#define TASK_MSGBOX_HEIGHT   150



static struct Task_s *pTaskList = NULL;
static GtkDialog *pDialog = NULL;
static GtkLabel *pLabel = NULL;
static GdkColormap *pColormap;
static GdkColor Color;
static int NewIntCmd(const char *szFilename, struct Action_s *pAction);
static int NewExtCmd(const char *szFilename, struct Action_s *pAction);
static int NewImport(const char *szFilename, struct Action_s *pAction);
static void StrReplace(char *szString, const char *szFrom, const char *szTo);



int TaskInitialize(void)
{
	pColormap = gdk_colormap_get_system();
	Color.red = 0xffff;
	Color.green = 0;
	Color.blue = 0;
	gdk_color_alloc(pColormap, &Color);

	return SUCCESS;
}



int TaskNew(const int iTaskType, const void **pValue)
{
	struct Action_s *pAction;
	int iResult = SUCCESS;
	char *szFilename;
																							
	/* create task a new entry */
	switch (iTaskType)
	{
		case TASK_ACTION:

			pAction = (struct Action_s *) pValue[0];
			szFilename = (char *) pValue[1];

			if (pAction->fFlags & TASK_INTERNAL)
			{
				iResult = NewIntCmd(szFilename, pAction);
				if (iResult != SUCCESS)
					break;
			}
			
			else
			{
				iResult = NewExtCmd(szFilename, pAction);
				if (iResult != SUCCESS)
					break;
			
				iResult = NewImport(szFilename, pAction);
				if (iResult != SUCCESS)
					break;
			}
			
			break;
	}

	return iResult;
}



int TaskDelete(const struct Task_s *pTask)
{
	struct Task_s *pTaskPrevious = NULL;

	/* hide dialog if action task blocking */
	if (pTask->fFlags & TASK_BLOCKING)
	{
		gtk_widget_hide_all(GTK_WIDGET(pDialog));
		gtk_widget_destroy(GTK_WIDGET(pDialog));
		while (g_main_iteration(FALSE));
	}
	
	/* unlink the entry from the task list */
	if (pTask == pTaskList)
	{
		pTaskList = (pTask->pNext == NULL)
			? NULL
			: pTask->pNext;
	}
	else
	{
		for (pTaskPrevious = pTaskList; pTaskPrevious != NULL; pTaskPrevious = pTaskPrevious->pNext)
			if (pTaskPrevious->pNext == pTask)
				break;
		pTaskPrevious->pNext = pTask->pNext;
	}

	/* remove entry from Window menu */
	MenuWindowDelete(pTask->pMenuItem);
	
	/* free the alocated memory */
	if (pTask->szValue != NULL)
		free((void *) pTask->szValue);
	free((void *) pTask);
	
	return SUCCESS;
}



void TaskProcess(void)
{
	GtkText *pText;
	FILE *hStdErr;
	struct Task_s *pTask, *pPtr;
	pid_t PidResult;
	int iBegin, iEnd, i, j;
	char *szParent, *szChild = NULL, *szTmpFile, szText[2];
	
	for (pTask = pTaskList; pTask != NULL; pTask = pTask->pNext)
	{
		/* if the task is running, check for its death */
		if (pTask->iType == TASK_EXTCMD && pTask->Id > 0)
		{
			PidResult = waitpid(pTask->Id, NULL, WNOHANG);
			if (PidResult == pTask->Id)
			{
				pText = GTK_TEXT(lookup_widget(pWindowMain, "StatusText"));
				if (pText == NULL)
					return;

				szTmpFile = (char *) malloc(strlen(GM_TMPDIR) + strlen(GM_TMPNAME) + 64);
				sprintf(szTmpFile, "/%s/%s-stderr-%d", GM_TMPDIR, GM_TMPNAME, pTask->Id);
				hStdErr = fopen(szTmpFile, "r");
				if (hStdErr == NULL)
				{
					free(szTmpFile);
					return;
				}

				while (!feof(hStdErr)) 
				{
					i = fgetc(hStdErr);
					if (i < 0)
						break;

					szText[0] = (char) i;
					szText[1] = 0;
					gtk_text_insert(pText, NULL, &Color, NULL, szText, strlen(szText));
				}
				gtk_widget_show(GTK_WIDGET(pText));
				while (g_main_iteration(FALSE));

				fclose(hStdErr);
				remove(szTmpFile);
				free(szTmpFile);

				TaskDelete(pTask);
				continue;
			}
			
			else if (pTask->fFlags & TASK_BLOCKING)
				return;
		}

		/* run a new task */
		else switch (pTask->iType)
		{
			case TASK_INTCMD:   
				
				if (pTask->Id > 0)
					break;
				for (i = -1, pPtr = pTaskList; pPtr != NULL; pPtr = pPtr->pNext)
				{
					if (pPtr->iType == TASK_INTCMD && pPtr->Id > i)
						i = pPtr->Id;
				}
				pTask->Id = i + 1;
				EditOpen(pTask->szValue);
				break;
			
			case TASK_EXTCMD:   
				
				pText = GTK_TEXT(lookup_widget(pWindowMain, "StatusText"));
				if (pText == NULL)
					return;
				gtk_text_set_point(pText, 0);
				gtk_text_forward_delete(pText, gtk_text_get_length(pText));
				gtk_text_insert(pText, NULL, NULL, NULL, pTask->szValue, strlen(pTask->szValue));
				gtk_text_insert(pText, NULL, NULL, NULL, "\n", strlen("\n"));

				pTask->Id = fork();
				if (pTask->Id < 0)
				{
					TaskDelete(pTask);
					break;
				}
				else if (pTask->Id == 0)
				{
					szTmpFile = (char *) malloc(strlen(GM_TMPDIR) + strlen(GM_TMPNAME) + 64);
					sprintf(szTmpFile, "/%s/%s-stderr-%d", GM_TMPDIR, GM_TMPNAME, getpid());
					hStdErr = freopen(szTmpFile, "w", stderr);
					free(szTmpFile);
					if (hStdErr == NULL)
						_exit(0);
		
					execl("/bin/sh", "sh", "-c", pTask->szValue, NULL);

					_exit(0);
					/* hStdErr is closed automatically */
				}

				if (pTask->fFlags & TASK_BLOCKING)
					return;
				
				break;
			
			case TASK_IMPORT:   
				
				for (i = 0; i < strlen(pTask->szValue);)
				{
					/* get parent file name */
					iBegin = i;
					for (; i < strlen(pTask->szValue) && pTask->szValue[i] != FILETOOL_NAMESEP; i ++)
						;
					iEnd = i;
					i++;

					/* alloc memory for a parent file name */
					szParent = (char *) malloc(iEnd - iBegin + 1);
					if (szParent == NULL)
					{
						TaskDelete(pTask);
						break;
					}
					for (j = iBegin; j < iEnd; j ++)
						szParent[j - iBegin] = pTask->szValue[j];
					szParent[j - iBegin] = 0;

					/* get child file name */
					iBegin = i;
					for (; i < strlen(pTask->szValue) && pTask->szValue[i] != FILETOOL_NAMESEP; i ++)
						;
					iEnd = i;
					i++;
					
					/* alloc memory for a child file name */
					szChild = (char *) malloc(iEnd - iBegin + 1);
					if (szChild == NULL)
					{
						free(szParent);
						TaskDelete(pTask);
						break;
					}
					for (j = iBegin; j < iEnd; j ++)
						szChild[j - iBegin] = pTask->szValue[j];
					szChild[j - iBegin] = 0;

					DocCreate(szChild, szParent);
					free(szParent);
					free(szChild);
				}

				TaskDelete(pTask);
				
				break;
		}
	}
}



static int NewIntCmd(const char *szFilename, struct Action_s *pAction)
{
	struct Task_s *pTask, *pPtr;
	
	/* allocate memory for a new entry in the task list */
	pTask = (void *) malloc(sizeof(struct Task_s));
	if (pTask == NULL)
		return FAILURE;

	/* create a base of the entry */
	pTask->iType = TASK_INTCMD;
	pTask->fFlags = pAction->fFlags;
	pTask->Id = 0;
		
	/* prepare filename string */
	pTask->szValue = malloc(strlen(szFilename) + 1);
	if (pTask->szValue == NULL)
	{
		free((void *) pTask);
		return FAILURE;
	}
	strcpy(pTask->szValue, szFilename);
		
	/* display message box if task blocking */
	if (pTask->fFlags & TASK_BLOCKING)
	{
		/* create widgets */
		pDialog = GTK_DIALOG(gtk_dialog_new());
		pLabel = GTK_LABEL(gtk_label_new(pAction->szName/*pTask->szValue*/));
		gtk_container_add(GTK_CONTAINER(GTK_DIALOG(pDialog)->vbox), GTK_WIDGET(pLabel));
		gtk_label_set_line_wrap(pLabel, TRUE);
		gtk_widget_set_usize(GTK_WIDGET(pDialog), TASK_MSGBOX_WIDTH, TASK_MSGBOX_HEIGHT);
		gtk_window_set_modal(GTK_WINDOW(pDialog), TRUE);
		gtk_window_set_position(GTK_WINDOW(pDialog), GTK_WIN_POS_CENTER);
		gtk_window_set_transient_for(GTK_WINDOW(pDialog), GTK_WINDOW(pWindowMain));
		gtk_window_set_title(GTK_WINDOW(pDialog), GEDA_TITLE);
		gtk_window_set_policy(GTK_WINDOW(pDialog), FALSE, FALSE, FALSE);
		gtk_widget_show_all(GTK_WIDGET(pDialog));
		while (g_main_iteration(FALSE));
	}
			
	pTask->pMenuItem = MenuWindowNew(pAction->szName);
	pTask->Id = 0;
	pTask->pNext = NULL;

	/* add a new entry to the task list */
	if (pTaskList == NULL)
		pTaskList = pTask;
	else
	{
		for (pPtr = pTaskList; pPtr->pNext != NULL; pPtr = pPtr->pNext)
			;
		pPtr->pNext = pTask;
	}
	
	return SUCCESS;
}



static int NewExtCmd(const char *szFilename, struct Action_s *pAction)
{
	struct Task_s *pTask, *pPtr;
	char *pText;
	
	/* allocate memory for a new entry in the task list */
	pTask = (void *) malloc(sizeof(struct Task_s));
	if (pTask == NULL)
		return FAILURE;

	/* create a base of the entry */
	pTask->iType = TASK_EXTCMD;
	pTask->fFlags = pAction->fFlags;
		
	/* prepare command string */
	pTask->szValue = malloc(strlen((char *) pAction->szCommand) + 1);
	if (pTask->szValue == NULL)
	{
		free((void *) pTask);
		return FAILURE;
	}
	strcpy(pTask->szValue, pAction->szCommand);
	StrReplace((char *) pTask->szValue, "%FILENAME%", (char *) FileGetName(szFilename));
	StrReplace((char *) pTask->szValue, "%FILEEXT%", (char *) FileGetExt(szFilename));
	StrReplace((char *) pTask->szValue, "%FILEDIR%", (char *) FileGetDir(szFilename));
	StrReplace((char *) pTask->szValue, "%FILEREL%", (char *) FileGetRel(szFilename));
		
	/* display message box if task blocking */
	if (pTask->fFlags & TASK_BLOCKING)
	{
		/* create widgets */
		pDialog = GTK_DIALOG(gtk_dialog_new());
		pLabel = GTK_LABEL(gtk_label_new(pAction->szName/*pTask->szValue*/));
		gtk_container_add(GTK_CONTAINER(GTK_DIALOG(pDialog)->vbox), GTK_WIDGET(pLabel));
		gtk_label_set_line_wrap(pLabel, TRUE);
		gtk_widget_set_usize(GTK_WIDGET(pDialog), TASK_MSGBOX_WIDTH, TASK_MSGBOX_HEIGHT);
		gtk_window_set_modal(GTK_WINDOW(pDialog), TRUE);
		gtk_window_set_position(GTK_WINDOW(pDialog), GTK_WIN_POS_CENTER);
		gtk_window_set_transient_for(GTK_WINDOW(pDialog), GTK_WINDOW(pWindowMain));
		gtk_window_set_title(GTK_WINDOW(pDialog), GEDA_TITLE);
		gtk_window_set_policy(GTK_WINDOW(pDialog), FALSE, FALSE, FALSE);
		gtk_widget_show_all(GTK_WIDGET(pDialog));
		while (g_main_iteration(FALSE));
	}
			
	pText = (char *) malloc(strlen(szFilename) + strlen(pAction->szName) + 3);
	strcpy(pText, pAction->szName);
	strcat(pText, ": ");
	strcat(pText, szFilename);
	pTask->pMenuItem = MenuWindowNew(pText);
	pTask->Id = 0;
	pTask->pNext = NULL;
	free(pText);

	/* add a new entry to the task list */
	if (pTaskList == NULL)
		pTaskList = pTask;
	else
	{
		for (pPtr = pTaskList; pPtr->pNext != NULL; pPtr = pPtr->pNext)
			;
		pPtr->pNext = pTask;
	}
	
	return SUCCESS;
}



static int NewImport(const char *szFilename, struct Action_s *pAction)
{
	struct Task_s *pTask, *pPtr;

	/* allocate memory for a new entry in the task list */
	pTask = (void *) malloc(sizeof(struct Task_s));
	if (pTask == NULL)
		return FAILURE;
			
	/* create a base of the entry */
	pTask->iType = TASK_IMPORT;
	pTask->fFlags = TASK_NOFLAG;
		
	/* prepare import string */
	pTask->szValue = malloc(strlen(szFilename) + strlen((char *) pAction->szImport) + 2);
	if (pTask->szValue == NULL)
	{
		free((void *) pTask);
		return FAILURE;
	}
	strcpy(pTask->szValue, pAction->szImport);
	StrReplace((char *) pTask->szValue, "%FILENAME%", (char *) FileGetName(szFilename));
	StrReplace((char *) pTask->szValue, "%FILEEXT%", (char *) FileGetExt(szFilename));
	StrReplace((char *) pTask->szValue, "%FILEDIR%", (char *) FileGetDir(szFilename));
	StrReplace((char *) pTask->szValue, "%FILEREL%", (char *) FileGetRel(szFilename));
			
	pTask->pMenuItem = MenuWindowNew(pAction->szName);
	pTask->Id = 0;
	pTask->pNext = NULL;

	/* add a new entry to the task list */
	if (pTaskList == NULL)
		pTaskList = pTask;
	else
	{
		for (pPtr = pTaskList; pPtr->pNext != NULL; pPtr = pPtr->pNext)
			;
		pPtr->pNext = pTask;
	}
	
	return SUCCESS;
}



static void StrReplace(char *szString, const char *szFrom, const char *szTo)
{
	char *szBegin, *szTemp;
	int i;
	
	szTemp = (void *) malloc(strlen(szString) + 1);
	if (szTemp == NULL)
		return;
	
	for (szBegin = strstr(szString, szFrom); szBegin != NULL; szBegin = strstr(szString, szFrom))
	{
		strcpy(szTemp, szString);
		szBegin = strstr(szTemp, szFrom);
		i = szBegin - szTemp;
		
		strcpy(szString, szTemp);
		szString[i] = 0;
		
		realloc(szString, strlen(szString) + strlen(szTo) + strlen(szBegin + strlen(szFrom)) + 1);
		strcat(szString, szTo);
		strcat(szString, szBegin + strlen(szFrom));
	}
	
	free((void *) szTemp);
}
