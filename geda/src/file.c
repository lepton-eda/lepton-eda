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

#include "file.h"
#include "filetool.h"
#include "doc.h"
#include "project.h"
#include "tool.h"
#include "txtedit.h"



/*******************************************************************************

	Private functions and variables

*******************************************************************************/

/* error string returned to a calling function */
static char *pErrorString = NULL;

/* error strings */
#define MSG_FILATTACHED    "File already exists in project"
#define MSG_FILEXIST       "File already exists on disk"
#define MSG_FILTYPERR      "File type not supported"
#define MSG_FILWRIERR      "Cannot write to a file"
#define MSG_FILDELERR      "Cannot delete a file"
#define MSG_INTERROR       "Internal error"
#define MSG_PROMPT         ":\n\n"

/* string functions */
/* TODO: move to a separate module */
void StringSet(char *pVar, const char *pString);
void StringAdd(char *pVar, const char *pString);



/*******************************************************************************

	File edit

*******************************************************************************/

char *FileEdit(const char *szPath)
{
	EditOpen(szPath);

	return NULL;
}



/*******************************************************************************

	File save

*******************************************************************************/

char *FileSave(const char *szPath)
{
	EditSave(szPath);
	
	return NULL;
}



/*******************************************************************************

	File print

*******************************************************************************/

char *FilePrint(const char *szPath)
{
	EditPrint(szPath);
	
	return NULL;
}



/*******************************************************************************

	File close

*******************************************************************************/

char *FileClose(const char *szPath)
{
	EditClose(szPath);
	
	return NULL;
}



/*******************************************************************************

	File new

*******************************************************************************/

char *FileNew(const char *pFile, const char *pParent)
{
	FILE *hFile;
	int iId, iResult;
	char *pResult, szTemplate[TEXTLEN], szExt[TEXTLEN], pValue[TEXTLEN];


	/* 
		check existence of file (in project, on disk) 
	*/
	
	iResult = DocGetProperty(DOC_FILENAME, pFile, (void *) pValue);
	if (iResult == SUCCESS)
	{
		StringSet(pErrorString, MSG_FILATTACHED);
		StringAdd(pErrorString, MSG_PROMPT);
		StringAdd(pErrorString, pFile);
		return pErrorString;
	}

	iResult = FileIsExisting(pFile);
	if (iResult == SUCCESS)
	{
		StringSet(pErrorString, MSG_FILEXIST);
		StringAdd(pErrorString, MSG_PROMPT);
		StringAdd(pErrorString, pFile);
		return pErrorString;
	}


	/* 
		create  file 
	*/

	for (
		iId = 0, iResult = SUCCESS; iResult == SUCCESS; iId ++)
	{
		iResult = ToolValueGet(EXT_LIST, EXT_EXT, iId, szExt);
		if (iResult != SUCCESS)
		{
			StringSet(pErrorString, MSG_FILTYPERR);
			return pErrorString;
		}
		
		if (!strcmp(szExt, FileGetExt(pFile)))
			break;
	}

	iResult = ToolValueGet(EXT_LIST, EXT_TEMPLATE, iId, (void *) szTemplate);
	if (iResult != SUCCESS)
		FatalError(__FILE__, __LINE__, __DATE__);

	hFile = fopen(pFile, "w");
	if (hFile == NULL)
	{
		StringSet(pErrorString, MSG_FILWRIERR);
		StringAdd(pErrorString, MSG_PROMPT);
		StringAdd(pErrorString, pFile);
		return pErrorString;
	}
	fprintf(hFile, "%s", szTemplate);
	fclose(hFile);


	/* 
		add file to project 
	*/
	
	iResult = DocCreate(FileGetRel(pFile), pParent);
	if (iResult != SUCCESS)
	{
		StringSet(pErrorString, MSG_INTERROR);
		return pErrorString;
	}
	
	iResult = ProjectSave();
	if (iResult != SUCCESS)
	{
		StringSet(pErrorString, MSG_INTERROR);
		return pErrorString;
	}
	
	
	/* 
		open file 
	*/
	
	pResult = FileEdit(pFile);
	if (pResult != NULL)
		return pResult;


	return NULL;
}



/*******************************************************************************

	File import

*******************************************************************************/



/*******************************************************************************

	File unlink

*******************************************************************************/

char *FileUnlink(const char *szFilename)
{
	char *pResult, szName[TEXTLEN], szParent[TEXTLEN];
	int iResult;

	/* 
		delete all children of the file 
	*/
	
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
		{
			pResult = FileUnlink(szName);
			if (pResult != NULL)
				return pResult;
		}
	}


	/* 
		close the file 
	*/
	
	iResult = DocClose(szFilename);
	if (iResult != SUCCESS)
	{
		StringSet(pErrorString, MSG_INTERROR);
		return pErrorString;
	}


	/* 
		remove file from document list 
	*/
	
	iResult = DocDestroy(szFilename);
	if (iResult != SUCCESS)
	{
		StringSet(pErrorString, MSG_INTERROR);
		return pErrorString;
	}
	
	iResult = ProjectSave();
	if (iResult != SUCCESS)
	{
		StringSet(pErrorString, MSG_INTERROR);
		return pErrorString;
	}
	
	return NULL;
}



/*******************************************************************************

	File delete

*******************************************************************************/

char *FileDelete(const char *szFilename)
{
	char *pResult;
	int iResult;

	/*
		unlink the file
	*/
	
	pResult = FileUnlink(szFilename);
	if (pResult != NULL)
		return pResult;

	
	/* 
		delete the file from disk 
	*/
	
	iResult = unlink(szFilename);
	if (iResult != 0)
	{	
		StringSet(pErrorString, MSG_FILDELERR);
		StringAdd(pErrorString, MSG_PROMPT);
		StringAdd(pErrorString, szFilename);
		return pErrorString;
	}
	
	return NULL;
}



/*******************************************************************************

	Private functions and variables

*******************************************************************************/

/* copy pString to pVar */
void StringSet(char *pVar, const char *pString)
{
	char *pPtr;
	
	if (pString == NULL)
		return;
		
	if (pVar == NULL)
	{
		pPtr = (char *) malloc(strlen(pString) + 1);
		if (pPtr == NULL)
			return;
	} else {
		pPtr = (char *) realloc(pVar, strlen(pString) + 1);
		if (pPtr == NULL)
			return;
	}
	
	strcpy(pVar, pString);
	pVar = pPtr;
}


/* add pString to pVar */
void StringAdd(char *pVar, const char *pString)
{
	char *pPtr;
	
	if (pString == NULL)
		return;
		
	if (pVar == NULL)
	{
		pPtr = (char *) malloc(strlen(pString) + 1);
		if (pPtr == NULL)
			return;
	} else {
		pPtr = (char *) realloc(pVar, strlen(pVar) + strlen(pString) + 1);
		if (pPtr == NULL)
			return;
	}
	
	strcpy(pPtr, pVar);
	strcat(pPtr, pString);
	pVar = pPtr;
}
