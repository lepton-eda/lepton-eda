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
#include <unistd.h>
#include "dirs.h"
#include "file.h"
#include "install.h"
#include "global.h"
#include "log.h"
#include "msgbox.h"
#include "package.h"
#include "setup.h"
#include "support.h"



/* private functions */
static int are_requirements_met(struct CompsTable_s *pComp);
static int get_percentage(void);
static int get_next_component(struct CompsTable_s *pComp, int iIndex, char *szName);
static int InstallComponent(struct CompsTable_s *pComp);
static int TestIfInstalled(struct CompsTable_s *pPkg);
static int VerifyFiles(struct CompsTable_s *pPkg);
static int CopyFiles(struct CompsTable_s *pPkg);
static int AddVariable(const char *szVariable, const char *szValue);	
static int iErrorFlag = FALSE;

#define OVERWRITE_ASK        0
#define OVERWRITE_YES        1
#define OVERWRITE_NO         2
static int iOverWrite = OVERWRITE_ASK;



/*
	This function install all selected or required components specified in config file
*/

int InstallSoftware(void)
{
	struct CompsTable_s *pComp;
	GtkWidget *pWidget;
	GtkLabel *pName, *pAction, *pCompleted;
	char szName[TEXTLEN], szValue[TEXTLEN], *szVariable, szMessage[TEXTLEN], *pResult;
	int iPreviouslyToBeInstalled = -1, iToBeInstalled, iResult, i;
	
	/* clear setup log */
	if (FileTest(SETUP_LOGFILE) == SUCCESS)
	{
		iResult = unlink(SETUP_LOGFILE);
		if (iResult != 0)
		{
			sprintf(szMessage, "Cannot remove old setup log file %s", SETUP_LOGFILE);
			MsgBox(
				GTK_WINDOW(pWindowMain),
				"Warning !",
				szMessage, 
				MSGBOX_WARNING | MSGBOX_OKD
				);
		}
	}
	
	/* update window */
	pWidget = (GtkWidget *) lookup_widget(pWindowMain, "Container");
	if (pWidget == NULL)
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
	gtk_notebook_next_page((GtkNotebook *) pWidget);
	/* TODO: remove it */
	setup_buttons(pWindowMain);
	gtk_widget_show(pWidget);	
	while (g_main_iteration(FALSE));
	
	/* prepare used widgets */
	pName = (GtkLabel *) lookup_widget(pWindowMain, "StatusLabelName");
	if (pName == NULL)
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
	pAction = (GtkLabel *) lookup_widget(pWindowMain, "StatusLabelAction");
	if (pAction == NULL)
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
	pCompleted = (GtkLabel *) lookup_widget(pWindowMain, "StatusLabelCompleted");
	if (pCompleted == NULL)
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
		
	/* set PATH */
	szVariable = getenv("PATH");
	if (szVariable == NULL)
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
	sprintf(szValue, "%s%c%s:%s", szInstallDirectory, '/', "bin", szVariable);
	iResult = setenv("PATH", szValue, 1);
	if (iResult != 0)
	{
		sprintf(szMessage, "Cannot set PATH variable");
		MsgBox(
			GTK_WINDOW(pWindowMain),
			"Error !",
			szMessage, 
			MSGBOX_ERROR | MSGBOX_OKD
			);
		_exit(0);
	}
		
	/* set LD_LIBRARY_PATH */
	szVariable = getenv("LD_LIBRARY_PATH");
	if (szVariable == NULL)
	{
		sprintf(szMessage, "Cannot get LD_LIBRARY_PATH variable");
		MsgBox(
			GTK_WINDOW(pWindowMain),
			"Error !",
			szMessage, 
			MSGBOX_ERROR | MSGBOX_OKD
			);
		_exit(0);
	}
	sprintf(szValue, "%s%c%s:%s", szInstallDirectory, '/', "lib", szVariable);
	iResult = setenv("LD_LIBRARY_PATH", szValue, 1);
	if (iResult != 0)
	{
		sprintf(szMessage, "Cannot set LD_LIBRARY_PATH variable");
		MsgBox(
			GTK_WINDOW(pWindowMain),
			"Error !",
			szMessage, 
			MSGBOX_ERROR | MSGBOX_OKD
			);
		_exit(0);
	}
		
	/* set CFLAGS */
	sprintf(szValue, "-I%s/include -L%s/lib", szInstallDirectory, szInstallDirectory);
	iResult = setenv("CFLAGS", szValue, 1);
	if (iResult != 0)
	{
		sprintf(szMessage, "Cannot set CFLAGS variable");
		MsgBox(
			GTK_WINDOW(pWindowMain),
			"Error !",
			szMessage, 
			MSGBOX_ERROR | MSGBOX_OKD
			);
		_exit(0);
	}
		
	/* set CPPFLAGS */
	sprintf(szValue, "-I%s/include -L%s/lib", szInstallDirectory, szInstallDirectory);
	iResult = setenv("CPPFLAGS", szValue, 1);
	if (iResult != 0)
	{
		sprintf(szMessage, "Cannot set CPPFLAGS variable");
		MsgBox(
			GTK_WINDOW(pWindowMain),
			"Error !",
			szMessage, 
			MSGBOX_ERROR | MSGBOX_OKD
			);
		_exit(0);
	}
		
	/* set PREFIX */
	iResult = setenv("PREFIX", szInstallDirectory, 1);
	if (iResult != 0)
	{
		sprintf(szMessage, "Cannot set PREFIX variable");
		MsgBox(
			GTK_WINDOW(pWindowMain),
			"Error !",
			szMessage, 
			MSGBOX_ERROR | MSGBOX_OKD
			);
		_exit(0);
	}

	/* remember the directory */
	pResult = getcwd(szWorkingDirectory, TEXTLEN);
	if (pResult == NULL)
	{
		sprintf(szMessage, "Fatal error in file %s at line %d\n\nPlease send a bug to %s", __FILE__, __LINE__, SETUP_EMAIL);
		MsgBox(
			GTK_WINDOW(pWindowMain),
			"Fatal error !",
			szMessage, 
			MSGBOX_FATAL | MSGBOX_OKD
			);
		_exit(0);
	}
			
	/* check dependencies and mark components to be installed */
	mark_to_be_installed();
	
	while (iPreviouslyToBeInstalled > 0 || iPreviouslyToBeInstalled == -1)
	{
		iToBeInstalled = 0;

		/* try install all packages with fulfilled requirements */
		for (pComp = pCompsTable; pComp != NULL; pComp = pComp->pNextComp)
		{
			/* check if install the package */
			if (strlen(pComp->szFileName) == 0)
				continue;
			if (pComp->iToBeInstalled != PACKAGE_SELECTED
				&& pComp->iToBeInstalled != PACKAGE_REQUIRED
				)
				continue;
			if (pComp->bInstalled)
				continue;
			if (!are_requirements_met(pComp))
			{
				iToBeInstalled ++;
				continue;
			}

			/* install component */
			InstallComponent(pComp);
		}
		
		/* check if something has been installed, if no - exit loop */
		if (iToBeInstalled == iPreviouslyToBeInstalled)
			break;
		iPreviouslyToBeInstalled = iToBeInstalled;
	}

	/* adding variables */
	iResult = MsgBox(
		GTK_WINDOW(pWindowMain),
		"Question ...",
		"Update your environment variables ?\nChanges will be active after you login next time.", 
		MSGBOX_QUESTION | MSGBOX_OK | MSGBOX_CANCELD
		);
	switch (iResult)
	{
		case MSGBOX_OK:
			
			for (i = 0; i < strlen(Software.szDirname); i ++)
				szName[i] = toupper(Software.szDirname[i]);
			szName[i] = 0;
			strcat(szName, "_PATH");
			sprintf(szValue, "%s", szInstallDirectory);
			AddVariable(szName, szValue);
			
			sprintf(szValue, "%s/bin", szInstallDirectory);
			AddVariable("PATH", szValue);
			
			sprintf(szValue, "%s/lib", szInstallDirectory);
			AddVariable("LD_LIBRARY_PATH", szValue);
			
			break;
	}
	
	/* display message and prepare to end */
	if (!iErrorFlag)
		gtk_label_set_text(pCompleted, "Installation completed.");
	else 
	{
		sprintf(szMessage, "Installation completed with errors !\nCheck installation log (file '%s')", SETUP_LOGFILE);
		gtk_label_set_text(pCompleted, szMessage);
	}
	while (g_main_iteration(FALSE));
		
	pWidget = lookup_widget(pWindowMain, "OkButton");
	gtk_widget_set_sensitive(pWidget, TRUE);
	
	pWidget = lookup_widget(pWindowMain, "NextButton");
	gtk_widget_set_sensitive(pWidget, FALSE);
	
	pWidget = lookup_widget(pWindowMain, "PreviousButton");
	gtk_widget_set_sensitive(pWidget, FALSE);

	iSoftwareInstalled = TRUE;
	
	return SUCCESS;
}


static int InstallComponent(struct CompsTable_s *pComp)
{
	struct CompsTable_s *pPkg, *pCount;
	GtkProgressBar *pBar;
	GtkLabel *pName, *pAction, *pCompleted;
	int iResult, i, j;
	char szMessage[TEXTLEN], szCodeName[TEXTLEN], szCommand[TEXTLEN], szDir[TEXTLEN];

	/* prepare used widgets */
	pName = (GtkLabel *) lookup_widget(pWindowMain, "StatusLabelName");
	if (pName == NULL)
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
	pAction = (GtkLabel *) lookup_widget(pWindowMain, "StatusLabelAction");
	if (pAction == NULL)
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
	pCompleted = (GtkLabel *) lookup_widget(pWindowMain, "StatusLabelCompleted");
	if (pCompleted == NULL)
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
		
	/* check dependencies - obsolete, done in InstallSoftware() */
	pPkg = pComp;

	/* display package name */
	sprintf(szMessage, "Installing %s", pPkg->szName);
	Log(LOG_MESSAGE, LOG_MODULE_INSTALL, "");
	Log(LOG_MESSAGE, LOG_MODULE_INSTALL, szMessage);
	gtk_label_set_text(pName, pComp->szName);
	while (g_main_iteration(FALSE));

	/* testing if a package is already installed */
	sprintf(szMessage, "Searching in system");
	Log(LOG_MESSAGE, LOG_MODULE_INSTALL, szMessage);
	gtk_label_set_text(pAction, "Searching in system");
	while (g_main_iteration(FALSE));
	iResult = TestIfInstalled(pPkg);
	if (iResult == SUCCESS)
	{
		Log(LOG_MESSAGE, LOG_MODULE_INSTALL, "Package already installed");
		
		switch (iOverWrite)
		{
			case OVERWRITE_ASK:

				sprintf(szMessage, "%s is already installed.\nInstall it anyway ?", pPkg->szName);
				iResult = MsgBox(
					GTK_WINDOW(pWindowMain),
					"Question ...",
					szMessage, 
					MSGBOX_QUESTION | MSGBOX_ALWAYSYES | MSGBOX_YESD | MSGBOX_ALWAYSNO | MSGBOX_NO
					);
				switch (iResult)
				{
					case MSGBOX_YES:                                                           break;
					case MSGBOX_ALWAYSYES:   iOverWrite = OVERWRITE_YES;                       break;
					case MSGBOX_NO:                                        goto INSTALL_END;   break;
					case MSGBOX_ALWAYSNO:    iOverWrite = OVERWRITE_NO;    goto INSTALL_END;   break;
				}
				break;

			case OVERWRITE_YES:
				
				break;
			
			case OVERWRITE_NO:
				
				goto INSTALL_END;
				break;
		}

		Log(LOG_WARNING, LOG_MODULE_INSTALL, "Package to be installed anyway");
	}
	
	/* unpack the package */
	sprintf(szMessage, "Unpacking %s", pPkg->szFileName);
	Log(LOG_MESSAGE, LOG_MODULE_INSTALL, szMessage);
	gtk_label_set_text(pAction, "Unpacking");
	while (g_main_iteration(FALSE));
	iResult = FileTest(pPkg->szFileName);
	if (iResult != SUCCESS)
	{
		getcwd(szDir, TEXTLEN);
		sprintf(szMessage, "Cannot find a file '%s' in %s", pComp->szFileName, szDir);
		Log(LOG_ERROR, LOG_MODULE_INSTALL, szMessage);
		MsgBox(
			GTK_WINDOW(pWindowMain),
			"Error !",
			szMessage, 
			MSGBOX_ERROR | MSGBOX_OKD
			);
		iErrorFlag = TRUE;
		goto LABEL;
	}
	sprintf(szCommand, "tar -xzf %s", pPkg->szFileName);
	iResult = FileExec(szCommand);
	if (iResult != SUCCESS)
	{
		getcwd(szDir, TEXTLEN);
		sprintf(szMessage, "Cannot unpack a file '%s' in %s", pPkg->szFileName, szDir);
		Log(LOG_ERROR, LOG_MODULE_INSTALL, szMessage);
		MsgBox(
			GTK_WINDOW(pWindowMain),
			"Error !",
			szMessage, 
			MSGBOX_ERROR | MSGBOX_OKD
			);
		iErrorFlag = TRUE;
		goto LABEL;
	}

	/* installing patch */
	if (strlen(pComp->szPatch) > 0)
	{
		sprintf(szMessage, "Patching with %s", pPkg->szPatch);
		Log(LOG_MESSAGE, LOG_MODULE_INSTALL, szMessage);
		gtk_label_set_text(pAction, "Patching");
		while (g_main_iteration(FALSE));
		iResult = FileTest(pPkg->szPatch);
		if (iResult != SUCCESS)
		{
			getcwd(szDir, TEXTLEN);
			sprintf(szMessage, "Cannot find a patch file '%s' in %s", pComp->szPatch, szDir);
			Log(LOG_ERROR, LOG_MODULE_INSTALL, szMessage);
			MsgBox(
				GTK_WINDOW(pWindowMain),
				"Error !",
				szMessage, 
				MSGBOX_ERROR | MSGBOX_OKD
				);
			iErrorFlag = TRUE;
			goto LABEL;
		}
		sprintf(szCommand, "patch -d./ -p0 < %s", pPkg->szPatch);
		iResult = FileExec(szCommand);
		if (iResult != SUCCESS)
		{
			getcwd(szDir, TEXTLEN);
			sprintf(szMessage, "Cannot patch with '%s' in %s", pPkg->szPatch, szDir);
			Log(LOG_ERROR, LOG_MODULE_INSTALL, szMessage);
			MsgBox(
				GTK_WINDOW(pWindowMain),
				"Error !",
				szMessage, 
				MSGBOX_ERROR | MSGBOX_OKD
				);
			iErrorFlag = TRUE;
			goto LABEL;
		}
	}

	/* enter into the package directory */
	sprintf(szMessage, "Entering a directory %s", pPkg->szDirName);
	Log(LOG_MESSAGE, LOG_MODULE_INSTALL, szMessage);
	iResult = chdir(pPkg->szDirName);
	if (iResult != 0)
	{
		sprintf(szMessage, "Cannot enter the directory '%s'", pPkg->szDirName);
		Log(LOG_ERROR, LOG_MODULE_INSTALL, szMessage);
		MsgBox(
			GTK_WINDOW(pWindowMain),
			"Error !",
			szMessage, 
			MSGBOX_ERROR | MSGBOX_OKD
			);
		iErrorFlag = TRUE;
		goto LABEL;
	}

	/* preprocessing */
	if (strlen(pComp->szPreInst) > 0)
	{	
		sprintf(szMessage, "Running preprocessing command: %s", pPkg->szPreInst);
		Log(LOG_MESSAGE, LOG_MODULE_INSTALL, szMessage);
		gtk_label_set_text(pAction, "Preprocessing");
		while (g_main_iteration(FALSE));

		iResult = FileExec(pComp->szPreInst);
		if (iResult != SUCCESS)
		{
			sprintf(szMessage, "Cannot execute preprocessing command:\n%s", pComp->szPreInst);
			Log(LOG_ERROR, LOG_MODULE_INSTALL, szMessage);
			MsgBox(
				GTK_WINDOW(pWindowMain),
				"Error !",
				szMessage, 
				MSGBOX_ERROR | MSGBOX_OKD
				);
			iErrorFlag = TRUE;
			goto LABEL;
		}
	}
			
	/* verify completness of binary and data files */
	sprintf(szMessage, "Verifying binaries in the package file");
	Log(LOG_MESSAGE, LOG_MODULE_INSTALL, szMessage);
	gtk_label_set_text(pAction, "Verifying");
	while (g_main_iteration(FALSE));
	iResult = VerifyFiles(pComp);
	if (iResult != SUCCESS || strlen(FILES(pComp)) == 0)
	{
		sprintf(szMessage, "Preparing to building binaries");
		Log(LOG_MESSAGE, LOG_MODULE_INSTALL, szMessage);

		/* build tools testing */
		for (i = 0; i < strlen(pComp->szBldTools); i ++)
		{
			/* get code name of a tool */
			for (; i <strlen(pComp->szBldTools) && isspace(pComp->szBldTools[i]);)
				i ++;
			for (j = 0; i < strlen(pComp->szBldTools) && !isspace(pComp->szBldTools[i]);)
				szCodeName[j ++] = pComp->szBldTools[i ++];
			szCodeName[j] = 0;

			/* find package with the code name */
			for (pCount = pCompsTable; pCount != NULL; pCount = pCount->pNextComp)
				if (!strcmp(pCount->szCodeName, szCodeName))
					break;
			if (pCount == NULL)
			{
				sprintf(szMessage, "Fatal error in configuration file !\nCannot find section [%s].", szCodeName);
				MsgBox(
					GTK_WINDOW(pWindowMain),
					"Fatal error !",
					szMessage, 
					MSGBOX_FATAL | MSGBOX_OKD
					);
				goto LABEL;
			}
					
			/* test if the tool is installed */
			iResult = TestIfInstalled(pCount);
			if (iResult != SUCCESS)
			{
				/* check if software distribution contains the tool */
				if (strlen(pCount->szFileName) == 0)
				{
					sprintf(szMessage, "Cannot build %s from sources.\n\nPlease install %s before running %s setup.", pPkg->szName, pCount->szName, Software.szName);
					MsgBox(
						GTK_WINDOW(pWindowMain),
						"Error !",
						szMessage, 
						MSGBOX_ERROR | MSGBOX_OKD
						);
					iErrorFlag = TRUE;
					goto LABEL;
				}
						
				/* install the tool */
				iResult = InstallComponent(pCount);
				if (iResult != SUCCESS)
				{
					/* errors signalled by InstallComponent( <BLDTOOL> )*/
					goto LABEL;
				}
			}
		}
				
		/* building */
		sprintf(szMessage, "Running build command: %s", pPkg->szCommand);
		Log(LOG_MESSAGE, LOG_MODULE_INSTALL, szMessage);
		gtk_label_set_text(pAction, "Building");
		while (g_main_iteration(FALSE));
		iResult = FileExec(pPkg->szCommand);
		if (iResult != SUCCESS)
		{
			sprintf(szMessage, "Cannot execute build command\n%s", pPkg->szCommand);
			Log(LOG_ERROR, LOG_MODULE_INSTALL, szMessage);
			MsgBox(
				GTK_WINDOW(pWindowMain),
				"Error !",
				szMessage, 
				MSGBOX_ERROR | MSGBOX_OKD
				);
			iErrorFlag = TRUE;
			goto LABEL;
		}
	}
				
	/* copy binary and data files */
	sprintf(szMessage, "Copying files");
	Log(LOG_MESSAGE, LOG_MODULE_INSTALL, szMessage);
	gtk_label_set_text(pAction, "Copying");
	while (g_main_iteration(FALSE));
	iResult = CopyFiles(pComp);
	if (iResult != SUCCESS)
	{
		iErrorFlag = TRUE;
		goto LABEL;
	}

	/* postprocessing */
	sprintf(szMessage, "Running postprocessing command: %s", pPkg->szPostInst);
	Log(LOG_MESSAGE, LOG_MODULE_INSTALL, szMessage);
	gtk_label_set_text(pAction, "Postprocessing");
	while (g_main_iteration(FALSE));
	if (strlen(pComp->szPostInst) > 0)
	{
		iResult = FileExec(pComp->szPostInst);
		if (iResult != SUCCESS)
		{
			sprintf(szMessage, "Cannot execute postprocessing command:\n%s", pComp->szPostInst);
			Log(LOG_ERROR, LOG_MODULE_INSTALL, szMessage);
			MsgBox(
				GTK_WINDOW(pWindowMain),
				"Error !",
				szMessage, 
				MSGBOX_ERROR | MSGBOX_OKD
				);
			iErrorFlag = TRUE;
			goto LABEL;
		}
	}
			
	/* return to parent directory */
LABEL:	
	sprintf(szMessage, "Entering a directory %s", szWorkingDirectory);
	Log(LOG_MESSAGE, LOG_MODULE_INSTALL, szMessage);
	gtk_label_set_text(pAction, "Removing temporary files");
	while (g_main_iteration(FALSE));
	iResult = chdir(szWorkingDirectory);
	if (iResult != 0)
	{
		sprintf(szMessage, "Cannot change directory to %s", szWorkingDirectory);
		Log(LOG_ERROR, LOG_MODULE_INSTALL, szMessage);
		MsgBox(
			GTK_WINDOW(pWindowMain),
			"Error !",
			szMessage, 
			MSGBOX_ERROR | MSGBOX_OKD
			);
		iErrorFlag = TRUE;
	}

	/* testing if a package is already installed */
	sprintf(szMessage, "Testing installation");
	Log(LOG_MESSAGE, LOG_MODULE_INSTALL, szMessage);
	gtk_label_set_text(pAction, "Testing");
	while (g_main_iteration(FALSE));
	iResult = TestIfInstalled(pPkg);
	if (iResult != SUCCESS)
	{
		sprintf(szMessage, "%s cannot not be installed.", pPkg->szName);
		Log(LOG_ERROR, LOG_MODULE_INSTALL, szMessage);
		MsgBox(
			GTK_WINDOW(pWindowMain),
			"Error !",
			szMessage, 
			MSGBOX_ERROR | MSGBOX_OKD
			);
		iErrorFlag = TRUE;
	}
	
	/* remove temporary package resources */
	else
	{
		sprintf(szMessage, "Removing directory %s", pPkg->szDirName);
		Log(LOG_MESSAGE, LOG_MODULE_INSTALL, szMessage);
		sprintf(szCommand, "rm -Rf %s", pPkg->szDirName);
		iResult = FileExec(szCommand);
		if (iResult != SUCCESS)
		{
			sprintf(szMessage, "Cannot remove directory: %s", pPkg->szDirName);
			Log(LOG_ERROR, LOG_MODULE_INSTALL, szMessage);
			MsgBox(
				GTK_WINDOW(pWindowMain),
				"Error !",
				szMessage, 
				MSGBOX_ERROR | MSGBOX_OKD
				);
			iErrorFlag = TRUE;
		}
	}
			
	/* let's say that the package has been installed (even with errors) */
INSTALL_END:
	pComp->bInstalled = TRUE;
	gtk_label_set_text(pName, "");
	gtk_label_set_text(pAction, "");
	while (g_main_iteration(FALSE));

	/* update progress bar */
	pBar = (GtkProgressBar *) lookup_widget(pWindowMain, "ProgressBar");
	gtk_progress_bar_update(pBar, (gfloat)get_percentage()/100.0);
	gtk_widget_show((GtkWidget *) pBar);
	gtk_widget_show(pWindowMain);
	while (g_main_iteration(FALSE));

	return (iErrorFlag) 
		? FAILURE 
		: SUCCESS;
}


static int TestIfInstalled(struct CompsTable_s *pPkg)
{
	int iFileType = PACKAGE_FILE_UNKNOWN, iTotalResult = SUCCESS, iResult, i, j;
	char szValue[TEXTLEN], szFileDest[TEXTLEN], szMessage[TEXTLEN], *pPath, *pValue, szLdLibraryPath[TEXTLEN];

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
					return FAILURE;
				break;
				
			case PACKAGE_FILE_LIBRARY:
				
				iResult = FileSearch(szLdLibraryPath, szFileDest);
				if (iResult != SUCCESS)
					return FAILURE;
				break;
				
			case PACKAGE_FILE_DATA:
				
				iResult = FileSearch(szInstallDirectory, szFileDest);
				if (iResult != SUCCESS)
					return FAILURE;
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


static int VerifyFiles(struct CompsTable_s *pPkg)
{
	FILE *File;
	int iFileType = PACKAGE_FILE_UNKNOWN, i, j;
	char szFileSource[TEXTLEN], szFileDest[TEXTLEN], szValue[TEXTLEN];

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

		/* get file attributes */
		for (; i < strlen(FILES(pPkg)) && FILES(pPkg)[i] != '"'; i ++)
			;
		i ++;
		for (j = 0; i < strlen(FILES(pPkg)) && FILES(pPkg)[i] != '"'; i ++)
			;
		i ++;

		/* get source file name */
		for (; i < strlen(FILES(pPkg)) && FILES(pPkg)[i] != '"'; i ++)
			;
		i ++;
		for (j = 0; i < strlen(FILES(pPkg)) && FILES(pPkg)[i] != '"'; i ++)
			szFileSource[j ++] = FILES(pPkg)[i];
		szFileSource[j] = 0;
		i ++;

		/* get destination file name */
		for (; i < strlen(FILES(pPkg)) && FILES(pPkg)[i] != '"'; i ++)
			;
		i ++;
		for (j = 0; i < strlen(FILES(pPkg)) && FILES(pPkg)[i] != '"'; i ++)
			szFileDest[j ++] = FILES(pPkg)[i];
		szFileDest[j] = 0;
		i ++;
		
		/* checking if the file is accessible */
		switch (iFileType)
		{
			case PACKAGE_FILE_BINARY:
			case PACKAGE_FILE_LIBRARY:
			case PACKAGE_FILE_DATA:
			
				File = fopen(szFileSource, "r");
				if (File == NULL)
					return FAILURE;
				fclose(File);
				break;
		}
	}
	
	return SUCCESS;
}


static int CopyFiles(struct CompsTable_s *pPkg)
{
	mode_t Mode;
	int iFileType = PACKAGE_FILE_UNKNOWN, iTotalResult = SUCCESS, iResult, i, j;
	char szFileSource[TEXTLEN], szFileDest[TEXTLEN], szMessage[TEXTLEN], szValue[TEXTLEN];
	
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

		/* get file attribute */
		for (; i < strlen(FILES(pPkg)) && FILES(pPkg)[i] != '"'; i ++)
			;
		i ++;
		sscanf(FILES(pPkg) + i, "%o", &Mode);
		for (j = 0; i < strlen(FILES(pPkg)) && FILES(pPkg)[i] != '"'; i ++)
			;
		i ++;
		
		/* get source file name */
		for (; i < strlen(FILES(pPkg)) && FILES(pPkg)[i] != '"'; i ++)
			;
		i ++;
		for (j = 0; i < strlen(FILES(pPkg)) && FILES(pPkg)[i] != '"'; i ++)
			szFileSource[j ++] = FILES(pPkg)[i];
		szFileSource[j] = 0;
		i ++;

		/* get destination file name */
		for (; i < strlen(FILES(pPkg)) && FILES(pPkg)[i] != '"'; i ++)
			;
		i ++;
		for (j = 0; i < strlen(FILES(pPkg)) && FILES(pPkg)[i] != '"'; i ++)
			szFileDest[j ++] = FILES(pPkg)[i];
		szFileDest[j] = 0;
		i ++;
		
		/* copy / link the file */
		sprintf(szMessage, "Installing %s (%o) from %s to %s", szValue, Mode, szFileSource, szFileDest);
		Log(LOG_MESSAGE, LOG_MODULE_INSTALL, szMessage);
		switch (iFileType)
		{
			case PACKAGE_FILE_BINARY:
			case PACKAGE_FILE_LIBRARY:
				
				iResult = FileCopy(szFileSource, szFileDest, Mode);
				if (iResult != SUCCESS)
				{
					sprintf(szMessage, "Cannot install file\n%s/%s", szInstallDirectory, szFileDest);
					Log(LOG_ERROR, LOG_MODULE_INSTALL, szMessage);
					iTotalResult = FAILURE;
				}
				break;
				
			case PACKAGE_FILE_DATA:
				
				iResult = FileTest(szFileDest);
				if (iResult == SUCCESS)
				{
					/* do not overwrite data files */
					sprintf(szMessage, "File exists, no overwriting");
					Log(LOG_WARNING, LOG_MODULE_INSTALL, szMessage);
					break;
				}
				iResult = FileCopy(szFileSource, szFileDest, Mode);
				if (iResult != SUCCESS)
				{
					sprintf(szMessage, "Cannot install file\n%s/%s", szInstallDirectory, szFileDest);
					Log(LOG_ERROR, LOG_MODULE_INSTALL, szMessage);
					iTotalResult = FAILURE;
				}
				break;

			case PACKAGE_FILE_LINK:

				iResult = FileLink(szFileSource, szFileDest);
				if (iResult != SUCCESS)
				{
					sprintf(szMessage, "Cannot create link\n%s/%s", szInstallDirectory, szFileDest);
					Log(LOG_ERROR, LOG_MODULE_INSTALL, szMessage);
					iTotalResult = FAILURE;
				}
				break;
		}
	}
	
	return iTotalResult;
}
















/*
	A set of functions that unpack tgz archive, check if all needed files are present,
	if not then build them, copy files, create links and remove temporary directories
*/



static int are_requirements_met(struct CompsTable_s *pComp)
{
	struct CompsTable_s *pCount;
	int iResult = TRUE, i;
	char szCodeName[TEXTLEN];
	
	for (i = 0; i < strlen(pComp->szRequiresList); i ++)
	{
		i = get_next_component(pComp, i, szCodeName);
		if (strlen(szCodeName) == 0)
			break;
		
		pCount = get_component_by_name(szCodeName);
		if (pCount == NULL)
		{
			/* TODO: error handling */
			continue;
		}
		
		if (pCount->bInstalled == FALSE)
			iResult = FALSE;
	}
	
	
	return iResult;
}



struct CompsTable_s *get_component_by_name(char *szName)
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



void mark_to_be_installed(void)
{
	struct CompsTable_s *pCount, *pComp;
	int i, bChanged;
	char szCodeName[TEXTLEN];
	
	/* cancel all REQUIRED markers */
	for (pComp = pCompsTable; pComp != NULL; pComp = pComp->pNextComp)
	{
		if (pComp->iToBeInstalled == PACKAGE_REQUIRED)
			pComp->iToBeInstalled = PACKAGE_IGNORED;
	}

	for (pComp = pCompsTable; pComp != NULL; pComp = pComp->pNextComp)
	{
		bChanged = FALSE;
		
		if (pComp->iToBeInstalled == PACKAGE_SELECTED
			|| pComp->iToBeInstalled == PACKAGE_REQUIRED
			)
		{
			for (i = 0; i < strlen(pComp->szRequiresList); i ++)
			{
				i = get_next_component(pComp, i, szCodeName);
				if (strlen(szCodeName) == 0)
					break;
		
				pCount = get_component_by_name(szCodeName);
				if (pCount == NULL)
				{
					/* TODO: error handling */
					continue;
				}
		
				if (pCount->iToBeInstalled == PACKAGE_IGNORED)
				{
					pCount->iToBeInstalled = PACKAGE_REQUIRED;
					bChanged = TRUE;
				}
			}
			
			if (bChanged)
				pComp = pCompsTable;
		}
	}
}



static int get_percentage(void)
{
	struct CompsTable_s *pComp;
	float fPercentage;
	int iTotal = 0, iDone = 0;
	
	for (pComp = pCompsTable; pComp != NULL; pComp = pComp->pNextComp)
	{
		if (pComp->iToBeInstalled == PACKAGE_SELECTED || pComp->iToBeInstalled == PACKAGE_REQUIRED)
		{
			iTotal ++;
			
			if (pComp->bInstalled == TRUE)
				iDone ++;
		}
	}
	
	fPercentage = 100.0 * (float)iDone / (float)iTotal;
	return (int)fPercentage;
}


static int AddVariable(const char *szVariable, const char *szValue)
{
	FILE *hFileOrig, *hFileTmp;
	int i, j;
	char szPath[TEXTLEN], szEntry[TEXTLEN], szMessage[TEXTLEN], szFileNameOrig[TEXTLEN], szFileNameTmp[TEXTLEN], *pResult;
	
	sprintf(szMessage, "Updating environment variable %s to %s", szVariable, szValue);
	Log(LOG_MESSAGE, LOG_MODULE_INSTALL, szMessage);
	
	/* check existence of an entry in a path */
	pResult = getenv(szVariable);
	if (pResult != NULL)
	{
		strcpy(szPath, pResult);
		for (i = 0; i < strlen(szPath); i ++)
		{
			/* read path entry */
			for (j = 0; i < strlen(szPath) && szPath[i] != ':'; i ++, j ++)
				szEntry[j] = szPath[i];
			szEntry[j] = 0;

			/* checking entry */
			if (strcmp(szEntry, szValue) == 0)
			{
				Log(LOG_MESSAGE, LOG_MODULE_INSTALL, "Update not needed.");
				return SUCCESS;
			}
		}
	}

	/* determining file names */
	switch (iDirsType)
	{
		case DIRS_LOCAL:    
			
			pResult = getenv("HOME");
			if (pResult == NULL)
			{
				sprintf(szMessage, "Cannot get HOME variable");
				MsgBox(
					GTK_WINDOW(pWindowMain),
					"Error !",
					szMessage, 
					MSGBOX_ERROR | MSGBOX_OKD
					);
				_exit(0);
			}
			strcpy(szFileNameOrig, pResult);
			strcat(szFileNameOrig, "/.bash_profile");
			break;
			
		case DIRS_GLOBAL:
			
			strcpy(szFileNameOrig, "/etc/profile");
			break;
		
		case DIRS_CUSTOM:
			
			Log(LOG_WARNING, LOG_MODULE_INSTALL, "Cannot modify environment for custom installation.");
			return SUCCESS;
	}
	sprintf(szFileNameTmp, "%s.back", szFileNameOrig);
	
	/* copy .bash_profile to a unique file in /tmp, test if  */
	hFileTmp = fopen(szFileNameTmp, "w");
	if (hFileTmp == NULL)
	{
		sprintf(szMessage, "Cannot write a file %s", szFileNameTmp);
		Log(LOG_ERROR, LOG_MODULE_INSTALL, szMessage);
		MsgBox(
			GTK_WINDOW(pWindowMain),
			"Error !",
			szMessage, 
			MSGBOX_ERROR | MSGBOX_OKD
			);
		return FAILURE;
	}
	hFileOrig = fopen(szFileNameOrig, "r");
	if (hFileOrig == NULL)
	{
		fclose(hFileTmp);
		sprintf(szMessage, "Cannot open a file %s", szFileNameOrig);
		Log(LOG_ERROR, LOG_MODULE_INSTALL, szMessage);
		MsgBox(
			GTK_WINDOW(pWindowMain),
			"Error !",
			szMessage, 
			MSGBOX_ERROR | MSGBOX_OKD
			);
		return FAILURE;
	}
	while (!feof(hFileOrig))
	{
		i = fgetc(hFileOrig);
		if (i < 0)
			break;
		
		fputc(i, hFileTmp);
	}
	fclose(hFileTmp);
	fclose(hFileOrig);
	
	/* adding value to .bash_profile */
	hFileOrig = fopen(szFileNameOrig, "w");
	if (hFileOrig == NULL)
	{
		sprintf(szMessage, "Cannot write a file %s", szFileNameOrig);
		Log(LOG_ERROR, LOG_MODULE_INSTALL, szMessage);
		MsgBox(
			GTK_WINDOW(pWindowMain),
			"Error !",
			szMessage, 
			MSGBOX_ERROR | MSGBOX_OKD
			);
		return FAILURE;
	}
	hFileTmp = fopen(szFileNameTmp, "r");
	if (hFileTmp == NULL)
	{
		sprintf(szMessage, "Cannot open a file %s", szFileNameTmp);
		Log(LOG_ERROR, LOG_MODULE_INSTALL, szMessage);
		MsgBox(
			GTK_WINDOW(pWindowMain),
			"Error !",
			szMessage, 
			MSGBOX_ERROR | MSGBOX_OKD
			);
		return FAILURE;
	}
	fprintf(hFileOrig, "# Change made by %s Setup\n", Software.szName);
	fprintf(hFileOrig, "export %s=%s:$%s\n", szVariable, szValue, szVariable);
	fprintf(hFileOrig, "\n");
	while (!feof(hFileTmp))
	{
		i = fgetc(hFileTmp);
		if (i < 0)
			break;
		
		fputc(i, hFileOrig);
	}
	fclose(hFileOrig);
	fclose(hFileTmp);

	sprintf(szMessage, "File %s successfully updated", szFileNameOrig);
	Log(LOG_MESSAGE, LOG_MODULE_INSTALL, szMessage);
	
	return SUCCESS;
}
