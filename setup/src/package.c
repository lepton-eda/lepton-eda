/*******************************************************************************/
/*                                                                             */
/* Setup                                                                       */
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
#include "file.h"
#include "global.h"
#include "log.h"
#include "msgbox.h"
#include "package.h"



/* software properties and list of packages */
struct Software_s Software;
struct CompsTable_s *pCompsTable = NULL, *pPackage = NULL;
char szInstallDirectory[TEXTLEN];
int iSoftwareInstalled = FALSE;



/* static functions and variables used in this module */
static int ReadSection(char *szpackage, struct CompsTable_s *pComp);
char *PackageNext(const char *szPackage);
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
		sprintf(szValue, "Cannot find a configuration file '%s'", PACKAGE_CFGFILE);
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
		strcpy(pComp->szInstall, (pComp->iToBeInstalled == PACKAGE_SELECTED) ? "+" : "-");

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
		pComp->iToBeInstalled = PACKAGE_IGNORED;
		strcpy(pComp->szRequiresList, "");
		strcpy(pComp->szTopLevel, "");
		strcpy(pComp->szFileName, "");
		strcpy(pComp->szPatch, "");
		strcpy(pComp->szFtpFile, "");
		strcpy(pComp->szFtpPatch, "");
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
			if (pComp != NULL && pComp->iToBeInstalled == PACKAGE_IGNORED)
			{
				pComp->iToBeInstalled = (szValue[0] == 'y' || szValue[0] == 'Y')
					? PACKAGE_SELECTED
					: PACKAGE_IGNORED;
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
		else if (strcmp(szName, "INSTCMD") == 0)
		{
			if (pComp != NULL && strlen(pComp->szInstCmd) == 0)
			{
				strcpy(INSTCMD(pComp), szValue);
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


char *PackageNext(const char *szPackage)
{
	struct CompsTable_s *pPkg;

	if (szPackage == NULL)
	{
		if (pCompsTable == NULL)
			return NULL;
		else
			return pCompsTable->szCodeName;
	}

	pPkg = get_component_by_name(szPackage);
	if (pPkg == NULL)
		return NULL;

	if (pPkg->pNextComp == NULL)
		return NULL;

	return pPkg->pNextComp->szCodeName;
}



/* get value of a property */
void *PackageValueGet(const int iName, const int iNumber, const char *szPackage)
{
	static void *pResult = NULL;
	struct CompsTable_s *pPkg;

	if (pResult != NULL)
	{
		free(pResult);
		pResult = NULL;
	}

	if (iName == PACKAGE_NEXT)
	{
		if (PackageNext(szPackage) == NULL)
			return NULL;
		
		pResult = malloc(strlen(PackageNext(szPackage)) + 1);
		strcpy((char *)pResult, PackageNext(szPackage)); 
		return pResult;
	}

	pPkg = get_component_by_name(szPackage);
	if (pPkg == NULL)
		return NULL;

	switch (iName)
	{
		case PACKAGE_NAME:       pResult = malloc(strlen(pPkg->szName) + 1);       break;
		case PACKAGE_VERSION:    pResult = malloc(strlen(pPkg->szVersion) + 1);    break;
		case PACKAGE_RELEASE:    pResult = malloc(strlen(pPkg->szRelease) + 1);    break;
		case PACKAGE_DESC:       pResult = malloc(strlen(pPkg->szDesc) + 1);       break;
		case PACKAGE_LICENSE:    pResult = malloc(strlen(pPkg->szLicense) + 1);    break;
		case PACKAGE_FLAGDISP:   pResult = malloc(sizeof(int));                    break;
		case PACKAGE_FLAGINST:   pResult = malloc(sizeof(int));                    break;
		case PACKAGE_REQLIST:    pResult = malloc(strlen(pPkg->szRequiresList) + 1);    break;
		case PACKAGE_PACKAGES:   pResult = malloc(strlen(pPkg->szTopLevel) + 1);   break;
		case PACKAGE_FILENAME:   pResult = malloc(strlen(pPkg->szFileName) + 1);   break;
		case PACKAGE_PATCH:      pResult = malloc(strlen(pPkg->szPatch) + 1);      break;
		case PACKAGE_DIRNAME:    pResult = malloc(strlen(pPkg->szDirName) + 1);    break;
		case PACKAGE_BLDTOOLS:   pResult = malloc(strlen(pPkg->szBldTools) + 1);   break;
		case PACKAGE_BLDCMD:     pResult = malloc(strlen(pPkg->szCommand) + 1);    break;
		case PACKAGE_INSTCMD:    pResult = malloc(strlen(pPkg->szInstCmd) + 1);    break;
		case PACKAGE_PREINST:    pResult = malloc(strlen(pPkg->szPreInst) + 1);    break;
		case PACKAGE_POSTINST:   pResult = malloc(strlen(pPkg->szPostInst) + 1);   break;
		case PACKAGE_PREUNIN:    pResult = malloc(strlen(pPkg->szPreUnin) + 1);    break;
		case PACKAGE_POSTUNIN:   pResult = malloc(strlen(pPkg->szPostUnin) + 1);   break;
		case PACKAGE_INSTALLED:  pResult = malloc(sizeof(BOOL));                   break;
	}
	if (pResult == NULL)
		return NULL;

	switch (iName)
	{
		case PACKAGE_NAME:       strcpy((char *)pResult, pPkg->szName);           break;
		case PACKAGE_VERSION:    strcpy((char *)pResult, pPkg->szVersion);        break;
		case PACKAGE_RELEASE:    strcpy((char *)pResult, pPkg->szRelease);        break;
		case PACKAGE_DESC:       strcpy((char *)pResult, pPkg->szDesc);           break;
		case PACKAGE_LICENSE:    strcpy((char *)pResult, pPkg->szLicense);        break;
		case PACKAGE_FLAGDISP:   *((int *)pResult) = pPkg->bToBeDisplayed;        break;
		case PACKAGE_FLAGINST:   *((int *)pResult) = pPkg->iToBeInstalled;        break;
		case PACKAGE_REQLIST:    strcpy((char *)pResult, pPkg->szRequiresList);   break;
		case PACKAGE_PACKAGES:   strcpy((char *)pResult, pPkg->szTopLevel);       break;
		case PACKAGE_FILENAME:   strcpy((char *)pResult, pPkg->szFileName);       break;
		case PACKAGE_PATCH:      strcpy((char *)pResult, pPkg->szPatch);          break;
		case PACKAGE_DIRNAME:    strcpy((char *)pResult, pPkg->szDirName);        break;
		case PACKAGE_BLDTOOLS:   strcpy((char *)pResult, pPkg->szBldTools);       break;
		case PACKAGE_BLDCMD:     strcpy((char *)pResult, pPkg->szCommand);        break;
		case PACKAGE_INSTCMD:    strcpy((char *)pResult, pPkg->szInstCmd);        break;
		case PACKAGE_PREINST:    strcpy((char *)pResult, pPkg->szPreInst);        break;
		case PACKAGE_POSTINST:   strcpy((char *)pResult, pPkg->szPostInst);       break;
		case PACKAGE_PREUNIN:    strcpy((char *)pResult, pPkg->szPreUnin);        break;
		case PACKAGE_POSTUNIN:   strcpy((char *)pResult, pPkg->szPostUnin);       break;
		case PACKAGE_INSTALLED:  *((BOOL *)pResult) = pPkg->bInstalled;           break;

		default:                 MsgBox(
						GTK_WINDOW(pWindowMain),
						NULL,
						"Missing case statement in PackageGetValue()",
						MSGBOX_FATAL | MSGBOX_OKD
						);
					_exit(0);
	}

	return (void *) pResult;
}



/* set value of a property */
int PackageValueSet(const int iName, const int iNumber, const char *szPackage, void *pValue)
{
	struct CompsTable_s *pPkg;

	if (pValue == NULL)
		return FAILURE;

	pPkg = get_component_by_name(szPackage);
	if (pPkg == NULL)
		return FAILURE;

	switch (iName)
	{
		case PACKAGE_FLAGINST:   pPkg->iToBeInstalled = * (int *) pValue;          break;
		case PACKAGE_INSTALLED:  pPkg->bInstalled = * (BOOL *) pValue;             break;

		default:     
					MsgBox(
						GTK_WINDOW(pWindowMain),
						NULL,
						"Missing case statement in PackageSetValue()",
						MSGBOX_FATAL | MSGBOX_OKD
						);
					_exit(0);
	}

	return SUCCESS;
}



#define EXPECTED_IN_SYSTEM   "??? (package %s should exist in system)"

char *PackageWhatIsMissing(const char *szTestedCodeName)
{
	static char *szMissingFile = NULL;

	struct CompsTable_s *pTestedComp, *pComp;
	char szCodeName[TEXTLEN];
	int i, j;

	if (szMissingFile != NULL)
	{
		free(szMissingFile);
		szMissingFile = NULL;
	}

	/* look for a package */
	pTestedComp = get_component_by_name(szTestedCodeName);
	if (pTestedComp == NULL)
	{
		szMissingFile = (char *) malloc(1);
		strcpy(szMissingFile, "");
		return szMissingFile;
	}

	/* test a package */
	if (strlen(pTestedComp->szFileName) > 0)
	{
		/* check existence of package and patch files */
		if (strlen(FileGetFilename(pTestedComp->szFileName)) > 0 && FileTest(FileGetFilename(pTestedComp->szFileName)) != SUCCESS)
		{
			szMissingFile = (char *) malloc(strlen(pTestedComp->szFileName) + 1);
			strcpy(szMissingFile, pTestedComp->szFileName);
			return szMissingFile;
		}
		if (strlen(pTestedComp->szPatch) > 0 && FileTest(FileGetFilename(pTestedComp->szPatch)) != SUCCESS)
		{
			szMissingFile = (char *) malloc(strlen(pTestedComp->szPatch) + 1);
			strcpy(szMissingFile, pTestedComp->szPatch);
			return szMissingFile;
		}

		/* test all dependencies */
		for (i = 0; i < strlen(pTestedComp->szRequiresList); i ++)
		{
			i = get_next_component(pTestedComp, i, szCodeName);
			if (strlen(szCodeName) == 0)
				break;

			pComp = get_component_by_name(szCodeName);
			if (pComp == NULL)
			{
				/* TODO: error handling */
				continue;
			}

			szMissingFile = PackageWhatIsMissing(szCodeName);
			if (pComp->bInstalled == FALSE && strlen(szMissingFile) != 0)
				return szMissingFile;
		}
	}

	/* test o group of packages */
	else if (strlen(pTestedComp->szTopLevel) > 0)
	{
		/* test all dependencies */
		for (i = 0; i < strlen(pTestedComp->szTopLevel); i ++)
		{
			for (; i < strlen(pTestedComp->szTopLevel); i++)
			{
				if (!isspace(pTestedComp->szTopLevel[i]))
					break;
			}
			for (j = 0; i < strlen(pTestedComp->szTopLevel); i ++)
			{
				if (!isspace(pTestedComp->szTopLevel[i]))
					szCodeName[j ++] = pTestedComp->szTopLevel[i];
				else
					break;
			}
			szCodeName[j] = 0;
			if (strlen(szCodeName) == 0)
				break;

			pComp = get_component_by_name(szCodeName);
			if (pComp == NULL)
			{
				/* TODO: error handling */
				continue;
			}

			szMissingFile = PackageWhatIsMissing(szCodeName);
			if (pComp->bInstalled == FALSE && strlen(szMissingFile) != 0)
				return szMissingFile;
		}
	}

	/* packages only to be checked */
	else if (PackageTestIfInstalled(pTestedComp) != SUCCESS)
	{
		szMissingFile = (char *) malloc(strlen(EXPECTED_IN_SYSTEM) + strlen(szTestedCodeName) + 1);
		sprintf(szMissingFile, EXPECTED_IN_SYSTEM, szTestedCodeName);
		return szMissingFile;
	}

	szMissingFile = (char *) malloc(1);
	strcpy(szMissingFile, "");
	return szMissingFile;
}



int PackageTestIfInstalled(struct CompsTable_s *pPkg)
{
	int iFileType = PACKAGE_FILE_UNKNOWN, iTotalResult = SUCCESS, iResult, i, j;
	char szName[TEXTLEN], szPreDir[TEXTLEN], szValue[TEXTLEN], szFileDest[TEXTLEN], szMessage[TEXTLEN], *pPath, *pValue, szLdLibraryPath[TEXTLEN];

	/* get PATH variable */
	pPath = getenv("PATH");
	if (pPath == NULL)
	{
		sprintf(szMessage, "Cannot get PATH variable");
		MsgBox(
			GTK_WINDOW(pWindowMain),
			"Error !",
			szMessage,
			MSGBOX_ERROR | MSGBOX_OKD
			);
		_exit(0);
	}

	/* get LD_LIBRARY_PATH */
	strcpy(szLdLibraryPath, "");
	pValue = getenv("LD_AOUT_LIBRARY_PATH");
	if (pValue != NULL)
		strcat(szLdLibraryPath, pValue);
	pValue = getenv("LD_LIBRARY_PATH");
	if (pValue != NULL)
		strcat(szLdLibraryPath, pValue);
	strcat(szLdLibraryPath, ":/usr/lib:/lib");

	/* scan file list */
	for (i = 0; i < strlen(FILES(pPkg)); )
	{
		/* get a type of the next file */
		for (; i < strlen(FILES(pPkg)) && isspace(FILES(pPkg)[i]); i ++)
			;
		for (j = 0; i < strlen(FILES(pPkg)) && isalnum(FILES(pPkg)[i]); i ++, j ++)
			szValue[j] = toupper(FILES(pPkg)[i]);
		szValue[j] = 0;
		if (strcmp(szValue, PACKAGE_TAG_BINARY) == 0)
			iFileType = PACKAGE_FILE_BINARY;
		else if (strcmp(szValue, PACKAGE_TAG_LIBRARY) == 0)
			iFileType = PACKAGE_FILE_LIBRARY;
		else if (strcmp(szValue, PACKAGE_TAG_DATA) == 0)
			iFileType = PACKAGE_FILE_DATA;
		else if (strcmp(szValue, PACKAGE_TAG_LINK) == 0)
			iFileType = PACKAGE_FILE_LINK;

		/* omit file attribute */
		for (; i < strlen(FILES(pPkg)) && FILES(pPkg)[i] != '"'; i ++)
			;
		i ++;
		for (; i < strlen(FILES(pPkg)) && FILES(pPkg)[i] != '"'; i ++)
			;
		i ++;

		/* omit source file name */
		for (; i < strlen(FILES(pPkg)) && FILES(pPkg)[i] != '"'; i ++)
			;
		i ++;
		for (; i < strlen(FILES(pPkg)) && FILES(pPkg)[i] != '"'; i ++)
			;
		i ++;

		/* get destination file name */
		for (; i < strlen(FILES(pPkg)) && FILES(pPkg)[i] != '"'; i ++)
			;
		i ++;
		for (j = 0; i < strlen(FILES(pPkg)) && FILES(pPkg)[i] != '"'; i ++, j ++)
			szFileDest[j] = FILES(pPkg)[i];
		szFileDest[j] = 0;
		i ++;

		/* test file existence */
		switch (iFileType)
		{
			case PACKAGE_FILE_BINARY:

				iResult = FileSearch(pPath, szFileDest);
				if (iResult != SUCCESS)
				{
					sprintf(szMessage, "File '%s' not found in path '%s'", szFileDest, pPath);
					Log(LOG_MESSAGE, MODULE_PACKAGE, szMessage);
					return FAILURE;
				}
				break;

			case PACKAGE_FILE_LIBRARY:

				FileGetDir(szFileDest, szPreDir);
				strcpy(szName, (strlen(szPreDir) > 0) ? szFileDest + strlen(szPreDir) + 1 : szFileDest);
				iResult = FileSearch(szLdLibraryPath, /*szName*/szFileDest);
				if (iResult != SUCCESS)
				{
					sprintf(szMessage, "File '%s' not found in path '%s'", szFileDest, szLdLibraryPath);
					Log(LOG_MESSAGE, MODULE_PACKAGE, szMessage);
					return FAILURE;
				}
				break;

			case PACKAGE_FILE_DATA:

				sprintf(szName, "%s:%s:%s", szInstallDirectory, "/usr", "/usr/local");
				iResult = FileSearch(szName/*szInstallDirectory*/, szFileDest);
				if (iResult != SUCCESS)
				{
					sprintf(szMessage, "File '%s' not found in path '%s'", szFileDest, szName);
					Log(LOG_MESSAGE, MODULE_PACKAGE, szMessage);
					return FAILURE;
				}
				break;

			case PACKAGE_FILE_LINK:

				iResult = FileSearch(szInstallDirectory, szFileDest);
				if (iResult != SUCCESS)
					return FAILURE;
				break;
		}
	}

	return iTotalResult;
}



int get_next_component(struct CompsTable_s *pComp, int iIndex, char *szName)
{
	int i, j = 0;

	/* cancel spaces */
	for (i = iIndex; i < strlen(pComp->szRequiresList); i ++)
	{
		if (!isspace(pComp->szRequiresList[i]))
			break;
	}
	/* read name */
	for (; i < strlen(pComp->szRequiresList); i ++)
	{
		if (!isspace(pComp->szRequiresList[i]))
			szName[j++] = pComp->szRequiresList[i];

		else
			break;
	}
	szName[j] = 0;

	return i;
}



struct CompsTable_s *get_component_by_name(const char *szName)
{
	struct CompsTable_s *pComp = NULL;
	char szValue1[TEXTLEN], szValue2[TEXTLEN];
	int i;

	if (strlen(szName) == 0)
		return NULL;

	for (i = 0; i < strlen(szName); i ++)
		szValue1[i] = toupper(szName[i]);
	szValue1[i] = 0;

	for (pComp = pCompsTable; pComp != NULL; pComp = pComp->pNextComp)
	{
		for (i = 0; i < strlen(pComp->szCodeName); i ++)
			szValue2[i] = toupper(pComp->szCodeName[i]);
		szValue2[i] = 0;

		if (!strcmp(szValue1, szValue2))
			break;
	}

	return pComp;
}
