/*******************************************************************************/
/*                                                                             */
/* Setup - version 0.2.1                                                       */
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

#include <ctype.h>
#include <gtk/gtk.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/wait.h>
#define __USE_GNU
#include <unistd.h>
#undef __USE_GNU
#include "config.h"
#include "global.h"
#include "msgbox.h"
#include "package.h"



/* software properties and list of packages */
struct Software_s Software;
struct CompsTable_s *pCompsTable = NULL, *pPackage = NULL;
char szInstallDirectory[TEXTLEN];
int iSoftwareInstalled = FALSE;



/* static functions and variables used in this module */
static int ReadSection(char *szpackage, struct CompsTable_s *pComp);
char szWorkingDirectory[TEXTLEN];



/*
	Initializing and releasing package resources
*/

int PackageInitialize(void)
{
	int iCount, iPrevious, iResult, i, j;
	char szName[TEXTLEN], szValue[TEXTLEN], szPkgList[TEXTLEN], szMessage[TEXTLEN];
	struct CompsTable_s *pPkgPrevious, *pComp;

	/* open file */
	iResult = ConfigOpen(PACKAGE_CFGFILE);
	if (iResult == FAILURE)
	{
		sprintf(szValue, "Cannot find a configuration file '%s' in %s", PACKAGE_CFGFILE, get_current_dir_name());
		MsgBox(
			GTK_WINDOW(pWindowMain), 
			"Error !",
			szValue, 
			MSGBOX_ERROR | MSGBOX_OKD
			);
		return FAILURE;
	}
	
	/* prepare variables */
	szPkgList[0] = 0;
	
	/* read component list */
	iResult = ConfigSection("SOFTWARE");
	if (iResult == FAILURE)
		return FAILURE;
	for (iResult = SUCCESS; iResult == SUCCESS; )
	{
		iResult = ConfigGetNext(szName, szValue);
		if (iResult == FAILURE)
			break;
		if (strcmp(szName, "NAME") == 0)
		{
			strcpy(Software.szName, szValue);
		}
		else if (strcmp(szName, "VERSION") == 0)
		{
			strcpy(Software.szVersion, szValue);
		}
		else if (strcmp(szName, "RELEASE") == 0)
		{
			strcpy(Software.szRelease, szValue);
		}
		else if (strcmp(szName, "DESC") == 0)
		{
			strcpy(Software.szDesc, szValue);
		}
		else if (strcmp(szName, "LICENSE") == 0)
		{
			strcpy(Software.szLicense, szValue);
		}
		else if (strcmp(szName, "DIRNAME") == 0)
		{
			strcpy(Software.szDirname, szValue);
		}
		else if (strcmp(szName, "PICTNAME") == 0)
		{
			strcpy(Software.szPictName, szValue);
		}
		else if (strcmp(szName, "PACKAGE") == 0)
		{
			/* TODO: remove it */
			strcat(szPkgList, szValue);
			strcat(szPkgList, " ");
			
			strcat(Software.szPackage, szValue);
			strcat(Software.szPackage, " ");
		}
		else
		{
			sprintf(szMessage, "Not known entry in a configuration file:\n%s = %s\nin section [SOFTWARE]", szName, szValue);
			MsgBox(
				GTK_WINDOW(pWindowMain),
				"Warning !",
				szMessage, 
				MSGBOX_WARNING | MSGBOX_OKD
				);
		}
	}
	
	/* reading package descriptions */
	for (iResult = ConfigSectionFirst(szName), iCount = 0, i = 0, j = 0; iResult == SUCCESS; iResult = ConfigSectionNext(szName))
	{
		/* add necessary memory size */
		j = ReadSection(szName, NULL);
		if (j <= 0)
		{
			sprintf(szMessage, "Fatal error in file %s at line %d\n\nPlease send a bug to %s", __FILE__, __LINE__, SETUP_EMAIL);
			MsgBox(
				GTK_WINDOW(pWindowMain),
				"Fatal error !",
				szMessage, 
				MSGBOX_FATAL | MSGBOX_OKD
				);
			return FAILURE;
		}
		iCount += j + sizeof(struct CompsTable_s);
		j = 0;
	}
	pCompsTable = malloc(iCount);
	if (pCompsTable == NULL)
	{
		sprintf(szMessage, "Fatal error in file %s at line %d\n\nPlease send a bug to %s", __FILE__, __LINE__, SETUP_EMAIL);
		MsgBox(
				GTK_WINDOW(pWindowMain),
				"Fatal error !",
				szMessage, 
				MSGBOX_FATAL | MSGBOX_OKD
				);
		return FAILURE;
	}
	
	/* read components properties */
	for (iResult = ConfigSectionFirst(szName), iCount = 0, iPrevious = -1, i = 0; iResult == SUCCESS; iResult = ConfigSectionNext(szName))
	{
		
		/* save package properties */
		if (iPrevious != -1)
		{
			pPkgPrevious = (struct CompsTable_s *)((char *)pCompsTable + iPrevious);
			pPkgPrevious->pNextComp = (struct CompsTable_s *)((char *)pCompsTable + iCount);
		}
		j = ReadSection(szName, (void *)((char *)pCompsTable + iCount));
		if (j <= 0)
		{
			sprintf(szMessage, "Fatal error in file %s at line %d\n\nPlease send a bug to %s", __FILE__, __LINE__, SETUP_EMAIL);
			MsgBox(
				GTK_WINDOW(pWindowMain),
				"Fatal error !",
				szMessage, 
				MSGBOX_FATAL | MSGBOX_OKD
				);
			return FAILURE;
		}
		iPrevious = iCount;
		iCount += j + sizeof(struct CompsTable_s);
		j = 0;
	}
	
	/* close file */
	iResult = ConfigClose();
	if (iResult == FAILURE)
		return FAILURE;
	
	/* TODO: checking correctness of the configuration */
	for (pComp = pCompsTable; pComp != NULL; pComp = pComp->pNextComp)
	{
		strcpy(pComp->szInstall, pComp->bToBeInstalled ? "+" : "-");

		pComp->szNode[0] = pComp->szName;
		pComp->szNode[1] = pComp->szLicense;
		
		pComp->bInstalled = FALSE;
		pComp->pCompParent = NULL;
	}

	return SUCCESS;
}


int PackageRelease(void)
{
	return SUCCESS;
}


static int ReadSection(char *szPackage, struct CompsTable_s *pComp)
{
	int iSize = 0, iResult;
	char szName[TEXTLEN], szValue[TEXTLEN], szMessage[TEXTLEN];

	/* look for the section */
	iResult = ConfigSection(szPackage);
	if (iResult == FAILURE)
		return FAILURE;
	
	/* reset properties */
	if (pComp != NULL)
	{
		strcpy(pComp->szName, "");
		strcpy(pComp->szCodeName, szPackage);
		strcpy(pComp->szVersion, "");
		strcpy(pComp->szRelease, "");
		strcpy(pComp->szDesc, "");
		strcpy(pComp->szLicense, "");
		pComp->bToBeDisplayed = FALSE;
		pComp->bToBeInstalled = FALSE;
		strcpy(pComp->szRequiresList, "");
		strcpy(pComp->szTopLevel, "");
		strcpy(pComp->szFileName, "");
		strcpy(pComp->szPatch, "");
		strcpy(pComp->szDirName, "");
		strcpy(pComp->szBldTools, "");
		strcpy(pComp->szCommand, "");
		pComp->szVariables = (char *) pComp + sizeof(struct CompsTable_s);
		pComp->iFileList = 0;
		pComp->pNextComp= NULL;
	}

	for (iResult = SUCCESS; iResult == SUCCESS; )
	{
		iResult = ConfigGetNext(szName, szValue);
		if (iResult == FAILURE)
			break;

		if (strcmp(szName, "NAME") == 0)
		{
			if (pComp != NULL && strlen(pComp->szName) == 0)
			{
				strcpy(NAME(pComp), szValue);
			}
			iSize += strlen(szValue) + 1;
		}
		else if (strcmp(szName, "VERSION") == 0)
		{
			if (pComp != NULL && strlen(pComp->szVersion) == 0)
			{
				strcpy(VERSION(pComp), szValue);
			}
			iSize += strlen(szValue) + 1;
		}
		else if (strcmp(szName, "RELEASE") == 0)
		{
			if (pComp != NULL && strlen(pComp->szRelease) == 0)
			{
				strcpy(RELEASE(pComp), szValue);
			}
			iSize += strlen(szValue + 1);
		}
		else if (strcmp(szName, "DESC") == 0)
		{
			if (pComp != NULL && strlen(pComp->szDesc) == 0)
			{
				strcpy(DESC(pComp), szValue);
			}
			iSize += strlen(szValue) + 1;
		}
		else if (strcmp(szName, "LICENSE") == 0)
		{
			if (pComp != NULL && strlen(pComp->szLicense) == 0)
			{
				strcpy(LICENSE(pComp), szValue);
			}
			iSize += strlen(szValue) + 1;
		}
		else if (strcmp(szName, "FLAGDISP") == 0)
		{
			if (pComp != NULL && pComp->bToBeDisplayed == FALSE)
			{
				pComp->bToBeDisplayed = (szValue[0] == 'y' || szValue[0] == 'Y')
					? TRUE
					: FALSE;
			}
		}
		else if (strcmp(szName, "FLAGINST") == 0)
		{
			if (pComp != NULL && pComp->bToBeInstalled == FALSE)
			{
				pComp->bToBeInstalled = (szValue[0] == 'y' || szValue[0] == 'Y')
					? TRUE
					: FALSE;
			}
		}
		else if (strcmp(szName, "REQLIST") == 0)
		{
			if (pComp != NULL && strlen(pComp->szRequiresList) == 0)
			{
				strcpy(REQLIST(pComp), szValue);
			}
			iSize += strlen(szValue) + 1;
		}
		else if (strcmp(szName, "PACKAGE") == 0)
		{
			if (pComp != NULL && strlen(pComp->szTopLevel) == 0)
			{
				strcpy(pComp->szTopLevel, szValue);
			}
			iSize += strlen(szValue) + 1;
		}
		else if (strcmp(szName, "FILENAME") == 0)
		{
			if (pComp != NULL && strlen(pComp->szFileName) == 0)
			{
				strcpy(FILENAME(pComp), szValue);
			}
			iSize += strlen(szValue) + 1;
		}
		else if (strcmp(szName, "PATCH") == 0)
		{
			if (pComp != NULL && strlen(pComp->szPatch) == 0)
			{
				strcpy(PATCH(pComp), szValue);
			}
			iSize += strlen(szValue) + 1;
		}
		else if (strcmp(szName, "DIRNAME") == 0)
		{
			if (pComp != NULL && strlen(pComp->szDirName) == 0)
			{
				strcpy(DIRNAME(pComp), szValue);
			}
			iSize += strlen(szValue) + 1;
		}
		else if (strcmp(szName, "BLDTOOLS") == 0)
		{
			if (pComp != NULL && strlen(pComp->szBldTools) == 0)
			{
				strcpy(BLDTOOLS(pComp), szValue);
			}
			iSize += strlen(szValue) + 1;
		}
		else if (strcmp(szName, "BLDCMD") == 0)
		{
			if (pComp != NULL && strlen(pComp->szCommand) == 0)
			{
				strcpy(BLDCMD(pComp), szValue);
			}
			iSize += strlen(szValue) + 1;
		}
		else if (strcmp(szName, "PREINST") == 0)
		{
			if (pComp != NULL && strlen(pComp->szPreInst) == 0)
			{
				strcpy(PREINST(pComp), szValue);
			}
			iSize += strlen(szValue) + 1;
		}
		else if (strcmp(szName, "POSTINST") == 0)
		{
			if (pComp != NULL && strlen(pComp->szPostInst) == 0)
			{
				strcpy(POSTINST(pComp), szValue);
			}
			iSize += strlen(szValue) + 1;
		}
		else if (strcmp(szName, "PREUNIN") == 0)
		{
			if (pComp != NULL && strlen(pComp->szPreUnin) == 0)
			{
				strcpy(PREUNIN(pComp), szValue);
			}
			iSize += strlen(szValue) + 1;
		}
		else if (strcmp(szName, "POSTUNIN") == 0)
		{
			if (pComp != NULL && strlen(pComp->szPostUnin) == 0)
			{
				strcpy(POSTUNIN(pComp), szValue);
			}
			iSize += strlen(szValue) + 1;
		}
		else if (strcmp(szName, "FILE") == 0)
		{
			if (pComp != NULL)
			{
				strcat(FILES(pComp), " ");
				strcat(FILES(pComp), szValue);
			}
			iSize += strlen(szValue) + 6;
		}
		else if (strcmp(szName, "PICTNAME") == 0)
		{
			iSize += strlen(szValue) + 1;
		}
		else
		{
			sprintf(szMessage, "Not known entry in a configuration file:\n%s = %s\nin section [%s]", szName, szValue, szPackage);
			MsgBox(
				GTK_WINDOW(pWindowMain),
				"Warning !",
				szMessage, 
				MSGBOX_WARNING | MSGBOX_OKD
				);
		}
	}
	
	/* return to the section (to be possible to find the next one) */
	iResult = ConfigSection(szPackage);
	if (iResult == FAILURE)
		return FAILURE;
	
	return iSize;
}



/*
	Accessing to package variables
*/

/* set package pointer to the first one */
int PackageFirst(void)
{
	pPackage = pCompsTable;
	if (pPackage == NULL)
		return FAILURE;
	
	return SUCCESS;
}


/* set package pointer to the next one */
int PackageNext(void)
{
	if (pPackage->pNextComp == NULL)
		return FAILURE;
	pPackage = pPackage->pNextComp;
	
	return SUCCESS;
}



/* get value of a property */
int PackageGetValue(int iName, void *pValue)
{
	if (pPackage == NULL)
		return FAILURE;
	
	switch (iName)
	{
		case PACKAGE_NAME:       strcpy((char *)pValue, pPackage->szName);           break;
		case PACKAGE_VERSION:    strcpy((char *)pValue, pPackage->szVersion);        break;
		case PACKAGE_RELEASE:    strcpy((char *)pValue, pPackage->szRelease);        break;
		case PACKAGE_DESC:       strcpy((char *)pValue, pPackage->szDesc);           break;
		case PACKAGE_LICENSE:    strcpy((char *)pValue, pPackage->szLicense);        break;
		case PACKAGE_FLAGDISP:   *((int *)pValue) = pPackage->bToBeDisplayed;        break;
		case PACKAGE_FLAGINST:   *((int *)pValue) = pPackage->bToBeInstalled;        break;
		case PACKAGE_REQLIST:    strcpy((char *)pValue, pPackage->szRequiresList);   break;
		case PACKAGE_PACKAGES:   strcpy((char *)pValue, pPackage->szTopLevel);       break;
		case PACKAGE_FILENAME:   strcpy((char *)pValue, pPackage->szFileName);       break;
		case PACKAGE_PATCH:      strcpy((char *)pValue, pPackage->szPatch);          break;
		case PACKAGE_DIRNAME:    strcpy((char *)pValue, pPackage->szDirName);        break;
		case PACKAGE_BLDTOOLS:   strcpy((char *)pValue, pPackage->szBldTools);       break;
		case PACKAGE_BLDCMD:     strcpy((char *)pValue, pPackage->szCommand);        break;
		case PACKAGE_FILES:      strcpy((char *)pValue, FILES(pPackage));            break;
		case PACKAGE_PREINST:    strcpy((char *)pValue, pPackage->szPreInst);        break;
		case PACKAGE_POSTINST:   strcpy((char *)pValue, pPackage->szPostInst);       break;
		case PACKAGE_PREUNIN:    strcpy((char *)pValue, pPackage->szPreUnin);        break;
		case PACKAGE_POSTUNIN:   strcpy((char *)pValue, pPackage->szPostUnin);       break;
		default:                 return FAILURE;
	}
	
	return SUCCESS;
}



