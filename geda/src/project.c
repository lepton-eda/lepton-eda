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



/* local variables */
struct Project_s Project;



void ProjectNew(const char *szPath)
{
	int iResult;
	char *pPath;
	
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
			"Question ...",
			"Project already exists. Overwrite ?",
			MSGBOX_QUESTION | MSGBOX_YES | MSGBOX_NOD | MSGBOX_CANCEL
			);
		if (iResult != MSGBOX_YES)
			goto PROJECT_MENU_EXIT;
	}

	iResult = DocSave(pPath);
	if (iResult != SUCCESS)
	{
		MsgBox(
			pWindowMain,
			"Error !",
			"Cannot save project !",
			MSGBOX_ERROR | MSGBOX_OKD
			);
		goto PROJECT_MENU_EXIT;
	}
	
	strcpy(Project.szName, FileGetName(pPath));
	strcpy(Project.szExt, FileGetExt(pPath));
	strcpy(Project.szDir, FileGetDir(pPath));
	chdir(Project.szDir);
	
	/* make project active */
	ProjectWidgetsShow();
	ProjectTitle();

PROJECT_MENU_EXIT:
	
	free(pPath);
}



void ProjectOpen(const char *szPath)
{
	int iResult;
	
	strcpy(Project.szName, FileGetName(szPath));
	strcpy(Project.szExt, FileGetExt(szPath));
	strcpy(Project.szDir, FileGetDir(szPath));
	chdir(Project.szDir);

	iResult = DocLoad(szPath);
	if (iResult != SUCCESS)
	{
		MsgBox(
			pWindowMain,
			"Error !",
			"Cannot load project !",
			MSGBOX_ERROR | MSGBOX_OKD
			);
		return;
	}
	
	/* make project active */
	ProjectWidgetsShow();
	ProjectTitle();
}
