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

#ifndef __TOOL_H_INCLUDED
#define __TOOL_H_INCLUDED

#include <gtk/gtk.h>
#include "global.h"
#include "task.h"



/* tool list structure */
struct Tool_s
{
	int iId;                             /* ID number of a tool entry */
	char szName[TEXTLEN];                /* name of a tool, f.ex. "Schematic Editor" */
	char szCommand[TEXTLEN];             /* command to run the tool (without file context), f.ex. "gschem" */
	int iType;                           /* type of the tool (embedded/external) */
	GtkMenuItem *pMenuItem;
	struct Tool_s *pNext;                /* pointer to the next tool, NULL for the last */
};

/* action table */
struct Action_s
{
	int iId;                             /* ID number of action entry */
	char szName[TEXTLEN];                /* action name, f.ex. "SPICE simulation" */
	char szExt[TEXTLEN];                 /* extension on what action is allowed */
	char szCommand[TEXTLEN];             /* command to make action, f.ex. "spice %f.cir" */
	char szImport[TEXTLEN];              /* list of files to be imported after executing szCommand[] */
	DWORD fFlags;
	int iToolId;                         /* related tool */
	GtkMenuItem *pMenuItem;
	int bMenuEntryUsed;
	struct Action_s *pNext;          /* pointer to the next action, NULL for the last */
};

/* file extension structure */
struct Ext_s
{
	int iId;                             /* ID number of extension entry */
	char szName[TEXTLEN];                /* file type, f.ex. "Schematic" */
	char szExt[TEXTLEN];                 /* extension, f.ex. "sch" */
	char szParent[TEXTLEN];              /* possible parent extensions */
	int iGroupId;
	int bInFileNew;
	int bMenuUsed;
	char szTemplate[TEXTLEN];            /* path/filename of a template */
	int iActionId;                       /* default action */
	GtkMenuItem *pMenuItem;
	struct Ext_s *pNext;             /* pointer to the next extension, NULL for the last */
};
extern struct Action_s *pActionList;
/*
	Macros for accessing tool database
*/

/* databases */
#define TOOL_LIST            1
#define ACTION_LIST          2
#define EXT_LIST             3
#define GROUP_LIST           4

/* fields of tool database */
#define TOOL_ID              1
#define TOOL_NAME            2
#define TOOL_COMMAND         3
#define TOOL_TYPE            4

/* fields of group database */
#define GROUP_NONE           -1
#define GROUP_ID             1
#define GROUP_NAME           2
#define GROUP_NODE           3
#define GROUP_NEXT           4

/* fields of action database */
#define ACTION_ID            1
#define ACTION_NAME          2
#define ACTION_EXT           3
#define ACTION_COMMAND       4
#define ACTION_IMPORT        5
#define ACTION_TOOLID        6

/* fields of extension database */
#define EXT_ID               1
#define EXT_NAME             2
#define EXT_EXT              3
#define EXT_TEMPLATE         4
#define EXT_INFILENEW        5
#define EXT_ACTIONID         6
#define EXT_PARENT           7
#define EXT_GROUPID          8

/* TODO */
#define TOOL_DEFAULT         -1

/* public functions */
int ToolGetExtensionId(char *szExtension, int *iExtId);
int ToolInitialize(void);
int ToolRelease(void);
int ToolProcess(void);
int ToolValueGet(int iDataBase, int iValue, int iId, void *pValue);
int ToolValueSet(int iDataBase, int iValue, int iId, void *pValue);
int ToolOpenFile(int iExtId, char *szFilename, int iActionId);
int ToolRun(char *szCommand, char *szFilename, DWORD fFlags);
void ActionSelect(char *szExt);
void MenuFileNewRefresh(const char *szExt);



#endif
