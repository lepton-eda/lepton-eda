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
static BOOL RequirementsMet(const char *szPackage);
static int get_percentage(void);
static int InstallComponent(const char *szPackage);
static int TestIfInstall(const char *szPackage);
static int Download(const char *szPackage);
static int InstallPreprocessing(const char *szPackage);
static int VerifyFiles(const char *szPackage);
static int Unpacking(const char *szPackage);
static int Patching(const char *szPackage);
static int TestBuildTools(const char *szPackage);
static int BuildFiles(const char *szPackage);
static int InstallFiles(const char *szPackage);
static int CopyFiles(const char *szPackage);
static int InstallPostprocessing(const char *szPackage);
static int VariableAdd(const char *szVariable, const char *szValue);
static int VariableCheck(const char *szVariable, const char *szValue);
static int iErrorFlag = FALSE;
static GtkLabel *pName, *pAction, *pCompleted;

static GtkLabel *pName, *pAction, *pCompleted;
int bInstallUsingScript = TRUE;


/* messages */
#define MSG_UNPACK_SCRIPT   "tar -xzf "
#define MSG_PATCH_SCRIPT    "patch -d./ -p0 < "
#define MSG_OVERWRITE_Q     " is already installed.\nInstall it anyway ?"
#define MSG_FTPFILE_SCRIPT  "wget -c -t0 "



void FatalError(const char *szFile, const int iLine, const char *szDate);
void FatalError(const char *szFile, const int iLine, const char *szDate)
{
	char szMessage[TEXTLEN];

	sprintf(szMessage, "Fatal error !\nFile: %s\nLine: %d", szFile, iLine);
			MsgBox(
				GTK_WINDOW(pWindowMain),
				"Error !",
				szMessage,
				MSGBOX_ERROR | MSGBOX_OKD
				);
			_exit(0);
}



/*
	This function install all selected or required components specified in config file
*/

int InstallSoftware(void)
{
	GtkWidget *pWidget;
	char szValue[TEXTLEN], *szVariable, szMessage[TEXTLEN], *pResult, *pPackage, szPackage[TEXTLEN];
	int iPreviouslyToBeInstalled = -1, bUpdatePath = FALSE, bUpdateLdLibraryPath = FALSE, iToBeInstalled, iResult;

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
		FatalError(__FILE__, __LINE__, __DATE__);
	gtk_notebook_next_page((GtkNotebook *) pWidget);
	/* TODO: remove it */
	setup_buttons(pWindowMain);
	gtk_widget_show(pWidget);
	while (g_main_iteration(FALSE));

	/* prepare used widgets */
	pName = (GtkLabel *) lookup_widget(pWindowMain, "StatusLabelName");
	if (pName == NULL)
		FatalError(__FILE__, __LINE__, __DATE__);
	pAction = (GtkLabel *) lookup_widget(pWindowMain, "StatusLabelAction");
	if (pAction == NULL)
		FatalError(__FILE__, __LINE__, __DATE__);
	pCompleted = (GtkLabel *) lookup_widget(pWindowMain, "StatusLabelCompleted");
	if (pCompleted == NULL)
		FatalError(__FILE__, __LINE__, __DATE__);

	/* set PATH */
	sprintf(szValue, "%s/bin", szInstallDirectory);
	bUpdatePath = (VariableCheck("PATH", szValue) != SUCCESS)
		? TRUE
		: FALSE;
	if (bUpdatePath)
	{
		szVariable = getenv("PATH");
		sprintf(szValue, "%s%c%s:%s",
			szInstallDirectory, '/', "bin",
			(szVariable != NULL) ? szVariable : ""
			);
		iResult = setenv("PATH", szValue, 1);
		if (iResult != 0)
			FatalError(__FILE__, __LINE__, __DATE__);
	}

	/* set LD_LIBRARY_PATH */
	sprintf(szValue, "%s/lib", szInstallDirectory);
	bUpdateLdLibraryPath = (VariableCheck("LD_LIBRARY_PATH", szValue) != SUCCESS)
		? TRUE
		: FALSE;
	if (bUpdateLdLibraryPath)
	{
		szVariable = getenv("LD_LIBRARY_PATH");
		sprintf(szValue, "%s%c%s:%s",
			szInstallDirectory, '/', "lib",
			(szVariable != NULL) ? szVariable : ""
			);
		iResult = setenv("LD_LIBRARY_PATH", szValue, 1);
		if (iResult != 0)
			FatalError(__FILE__, __LINE__, __DATE__);
	}

	/* set CFLAGS */
	sprintf(szValue, "-I%s/include -L%s/lib", szInstallDirectory, szInstallDirectory);
	iResult = setenv("CFLAGS", szValue, 1);
	if (iResult != 0)
		FatalError(__FILE__, __LINE__, __DATE__);

	/* set CPPFLAGS */
	sprintf(szValue, "-I%s/include -L%s/lib", szInstallDirectory, szInstallDirectory);
	iResult = setenv("CPPFLAGS", szValue, 1);
	if (iResult != 0)
		FatalError(__FILE__, __LINE__, __DATE__);

	/* set PREFIX */
	iResult = setenv("PREFIX", szInstallDirectory, 1);
	if (iResult != 0)
		FatalError(__FILE__, __LINE__, __DATE__);

	/* remember the directory */
	pResult = getcwd(szWorkingDirectory, TEXTLEN);
	if (pResult == NULL)
		FatalError(__FILE__, __LINE__, __DATE__);

	while (iPreviouslyToBeInstalled > 0 || iPreviouslyToBeInstalled == -1)
	{
		iToBeInstalled = 0;
		mark_to_be_installed();

		/* try install required components */
		for (
			pPackage = (char *) PackageValueGet(PACKAGE_NEXT, 0, NULL);
			pPackage != NULL;
			pPackage = (char *) PackageValueGet(PACKAGE_NEXT, 0, szPackage)
			)
		{
			strcpy(szPackage, pPackage);
			
			if (strlen(PackageValueGet(PACKAGE_FILENAME, 0, szPackage)) == 0)
				continue;
			if (*((int *) PackageValueGet(PACKAGE_FLAGINST, 0, szPackage)) != PACKAGE_REQUIRED)
				continue;
			if (*((BOOL *) PackageValueGet(PACKAGE_INSTALLED, 0, szPackage)))
				continue;
			if (!RequirementsMet(szPackage))
			{
				iToBeInstalled ++;
				continue;
			}

			InstallComponent(szPackage);
		}

		/* try install selected components */
		for (
			pPackage = (char *) PackageValueGet(PACKAGE_NEXT, 0, NULL);
			pPackage != NULL;
			pPackage = (char *) PackageValueGet(PACKAGE_NEXT, 0, szPackage)
			)
		{
			strcpy(szPackage, pPackage);
			
			if (strlen(PackageValueGet(PACKAGE_FILENAME, 0, szPackage)) == 0)
				continue;
			if ( *((int *) PackageValueGet(PACKAGE_FLAGINST, 0, szPackage)) != PACKAGE_SELECTED
				&& *((int *) PackageValueGet(PACKAGE_FLAGINST, 0, szPackage)) != PACKAGE_BLDTOOL
				)
				continue;
			if (*((BOOL *) PackageValueGet(PACKAGE_INSTALLED, 0, szPackage)))
				continue;
			if (!RequirementsMet(szPackage))
			{
				iToBeInstalled ++;
				continue;
			}

			InstallComponent(szPackage);
		}

		/* check if something has been installed, if no - exit loop */
		if (iToBeInstalled == iPreviouslyToBeInstalled)
			break;
		iPreviouslyToBeInstalled = iToBeInstalled;
	}

	/* adding variables */
	if (bUpdatePath || bUpdateLdLibraryPath)
	{
		iResult = MsgBox(
			GTK_WINDOW(pWindowMain),
			"Question ...",
			"Update your environment variables ?\nChanges will be active after you login next time.",
			MSGBOX_QUESTION | MSGBOX_OK | MSGBOX_CANCELD
			);
		switch (iResult)
		{
			case MSGBOX_OK:

				if (bUpdateLdLibraryPath)
				{
					sprintf(szValue, "%s/lib", szInstallDirectory);
					VariableAdd("LD_LIBRARY_PATH", szValue);
				}

				if (bUpdatePath)
				{
					sprintf(szValue, "%s/bin", szInstallDirectory);
					VariableAdd("PATH", szValue);
				}

				break;
		}
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
	if (pWidget == NULL)
		FatalError(__FILE__, __LINE__, __DATE__);
	gtk_widget_set_sensitive(pWidget, TRUE);

	pWidget = lookup_widget(pWindowMain, "NextButton");
	if (pWidget == NULL)
		FatalError(__FILE__, __LINE__, __DATE__);
	gtk_widget_set_sensitive(pWidget, FALSE);

	pWidget = lookup_widget(pWindowMain, "PreviousButton");
	if (pWidget == NULL)
		FatalError(__FILE__, __LINE__, __DATE__);
	gtk_widget_set_sensitive(pWidget, FALSE);

	iSoftwareInstalled = TRUE;

	return SUCCESS;
}


static int InstallComponent(const char *szPackage)
{
	GtkProgressBar *pBar;
	BOOL b;
	int iResult;
	char szMessage[TEXTLEN];
	
	Log(LOG_MESSAGE, LOG_MODULE_INSTALL, "");
	Log(LOG_MESSAGE, LOG_MODULE_INSTALL, "Package being installed");
	Log(LOG_MESSAGE, LOG_MODULE_INSTALL, szPackage);
	gtk_label_set_text(pName, PackageValueGet(PACKAGE_NAME, 0, szPackage));
	while (g_main_iteration(FALSE));

	iResult = TestIfInstall(szPackage);
	if (iResult != SUCCESS)
		goto INSTALL_END;
	
	iResult = Download(szPackage);
	if (iResult != SUCCESS)
	{
		iErrorFlag = TRUE;
		goto LABEL;
	}

	iResult = Unpacking(szPackage);
	if (iResult != SUCCESS)
	{
		iErrorFlag = TRUE;
		goto LABEL;
	}

	iResult = InstallPreprocessing(szPackage);
	if (iResult != SUCCESS)
	{
		iErrorFlag = TRUE;
		goto LABEL;
	}

	iResult = VerifyFiles(szPackage);
	if (iResult != SUCCESS || (strlen(PackageValueGet(PACKAGE_INSTCMD, 0, szPackage)) > 0 && bInstallUsingScript))
	{
		iResult = TestBuildTools(szPackage);
		if (iResult != SUCCESS)
		{
			iErrorFlag = TRUE;
			goto LABEL;
		}

		iResult = Patching(szPackage);
		if (iResult != 0)
		{
			iErrorFlag = TRUE;
			goto LABEL;
		}

		iResult = BuildFiles(szPackage);
		if (iResult != 0)
		{
			iErrorFlag = TRUE;
			goto LABEL;
		}

		Log(LOG_MESSAGE, LOG_MODULE_INSTALL, "");
		Log(LOG_MESSAGE, LOG_MODULE_INSTALL, "Package being installed");
		Log(LOG_MESSAGE, LOG_MODULE_INSTALL, szPackage);
		gtk_label_set_text(pName, PackageValueGet(PACKAGE_NAME, 0, szPackage));
		while (g_main_iteration(FALSE));
	}

	if (strlen(PackageValueGet(PACKAGE_INSTCMD, 0, szPackage)) > 0 && bInstallUsingScript)
	{
		iResult = InstallFiles(szPackage);
		if (iResult != SUCCESS)
		{
			iErrorFlag = TRUE;
			goto LABEL;
		}
	}
	else
	{
		iResult = CopyFiles(szPackage);
		if (iResult != SUCCESS)
		{
			iErrorFlag = TRUE;
			goto LABEL;
		}
	}
	
	iResult = InstallPostprocessing(szPackage);
	if (iResult != SUCCESS)
	{
		iErrorFlag = TRUE;
		goto LABEL;
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
	iResult = PackageTestIfInstalled(get_component_by_name(szPackage));
	if (iResult != SUCCESS)
	{
		sprintf(szMessage, "%s cannot not be installed.", (char *) PackageValueGet(PACKAGE_NAME, 0, szPackage));
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
		char szCommand[TEXTLEN];
		
		sprintf(szMessage, "Removing directory %s", PackageValueGet(PACKAGE_DIRNAME, 0, szPackage));
		Log(LOG_MESSAGE, LOG_MODULE_INSTALL, szMessage);
		sprintf(szCommand, "rm -Rf %s", PackageValueGet(PACKAGE_DIRNAME, 0, szPackage));
		iResult = FileExec(szCommand);
		if (iResult != SUCCESS)
		{
			sprintf(szMessage, "Cannot remove directory: %s", PackageValueGet(PACKAGE_DIRNAME, 0, szPackage));
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
	b = TRUE;
	iResult = PackageValueSet(PACKAGE_INSTALLED, 0, szPackage, &b);
	if (iResult != SUCCESS)
		iErrorFlag = FAILURE;
	
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



/*
	Test if package should be installed. It returns SUCCESS if:
	- package is not installed at all
	- all packages should be overwritten
	- user selects overwriting
*/

#define OVERWRITE_ASK        0
#define OVERWRITE_YES        1
#define OVERWRITE_NO         2

static int TestIfInstall(const char *szPackage)
{
	static int iOverWrite = OVERWRITE_ASK;
	int iResult;
	char *pQuestion;

	Log(LOG_MESSAGE, LOG_MODULE_INSTALL, "Searching in system");
	if (PackageValueGet(PACKAGE_NAME, 0, szPackage) == NULL)
	{
		Log(LOG_ERROR, LOG_MODULE_INSTALL, "Cannot find package configuration");
		Log(LOG_MESSAGE, LOG_MODULE_INSTALL, szPackage);
		FatalError(__FILE__, __LINE__, __DATE__);
	}
	gtk_label_set_text(pName, PackageValueGet(PACKAGE_NAME, 0, szPackage));
	gtk_label_set_text(pAction, "Searching in system");
	while (g_main_iteration(FALSE));

	iResult = PackageTestIfInstalled(get_component_by_name(szPackage));
	if (iResult != SUCCESS)
		return SUCCESS;
	
	Log(LOG_MESSAGE, LOG_MODULE_INSTALL, "Package already installed");

	if (iOverWrite == OVERWRITE_NO)
		return FAILURE;

	if (iOverWrite == OVERWRITE_ASK)
	{
		pQuestion = (char *) malloc(strlen(MSG_OVERWRITE_Q) + strlen(FileGetFilename(PackageValueGet(PACKAGE_FILENAME, 0, szPackage))) + 1);
		strcpy(pQuestion, FileGetFilename(PackageValueGet(PACKAGE_NAME, 0, szPackage)));
		strcat(pQuestion, MSG_OVERWRITE_Q);
			
		iResult = MsgBox(
			GTK_WINDOW(pWindowMain),
			"Question ...",
			pQuestion,
			MSGBOX_QUESTION | MSGBOX_ALWAYSYES | MSGBOX_YESD | MSGBOX_ALWAYSNO | MSGBOX_NO
			);
			
		free(pQuestion);
			
		switch (iResult)
		{
			case MSGBOX_YES:                                       break;
			case MSGBOX_ALWAYSYES:   iOverWrite = OVERWRITE_YES;   break;
			case MSGBOX_NO:                                        return FAILURE;
			case MSGBOX_ALWAYSNO:    iOverWrite = OVERWRITE_NO;    return FAILURE;
		}
	}
	
	Log(LOG_WARNING, LOG_MODULE_INSTALL, "Previous installation is to be overwritten");
	
	return SUCCESS;
}



static int Download(const char *szPackage)
{
	int iResult;
	char *pCommand;

	Log(LOG_MESSAGE, LOG_MODULE_INSTALL, "Downloading file");
	if (PackageValueGet(PACKAGE_FILENAME, 0, szPackage) == NULL)
	{
		Log(LOG_ERROR, LOG_MODULE_INSTALL, "Cannot find package configuration");
		Log(LOG_MESSAGE, LOG_MODULE_INSTALL, szPackage);
		FatalError(__FILE__, __LINE__, __DATE__);
	}
	if (strlen(FileGetFilename(PackageValueGet(PACKAGE_FILENAME, 0, szPackage))) == 0)
	{
		Log(LOG_ERROR, LOG_MODULE_INSTALL, "Cannot find package configuration");
		Log(LOG_MESSAGE, LOG_MODULE_INSTALL, szPackage);
		return FAILURE;
	}
	if (FileTest(FileGetFilename(PackageValueGet(PACKAGE_FILENAME, 0, szPackage))) == SUCCESS)
	{
		Log(LOG_MESSAGE, LOG_MODULE_INSTALL, "File exists");
		return SUCCESS;
	}
	gtk_label_set_text(pName, PackageValueGet(PACKAGE_NAME, 0, szPackage));
	gtk_label_set_text(pAction, "Downloading file");
	while (g_main_iteration(FALSE));

	pCommand = (char *) malloc(strlen(MSG_FTPFILE_SCRIPT) + strlen(PackageValueGet(PACKAGE_FILENAME, 0, szPackage)) + 1);
	strcpy(pCommand, MSG_FTPFILE_SCRIPT);
	strcat(pCommand, PackageValueGet(PACKAGE_FILENAME, 0, szPackage));
	iResult = FileExec(pCommand);
	if (iResult != SUCCESS)
	{
		Log(LOG_ERROR, LOG_MODULE_INSTALL, "Cannot execute script");
		Log(LOG_MESSAGE, LOG_MODULE_INSTALL, pCommand);
		free(pCommand);
		return FAILURE;
	}
	free(pCommand);
	
	if (strlen(PackageValueGet(PACKAGE_PATCH, 0, szPackage)) == 0)
		return SUCCESS;
	if (strlen(FileGetFilename(PackageValueGet(PACKAGE_PATCH, 0, szPackage))) == 0)
	{
		Log(LOG_ERROR, LOG_MODULE_INSTALL, "Cannot find package configuration");
		Log(LOG_MESSAGE, LOG_MODULE_INSTALL, szPackage);
		return FAILURE;
	}
	if (FileTest(FileGetFilename(PackageValueGet(PACKAGE_PATCH, 0, szPackage))) == SUCCESS)
	{
		Log(LOG_MESSAGE, LOG_MODULE_INSTALL, "Patch exists");
		return SUCCESS;
	}
	gtk_label_set_text(pName, PackageValueGet(PACKAGE_NAME, 0, szPackage));
	gtk_label_set_text(pAction, "Downloading patch");
	while (g_main_iteration(FALSE));

	pCommand = (char *) malloc(strlen(MSG_FTPFILE_SCRIPT) + strlen(PackageValueGet(PACKAGE_PATCH, 0, szPackage)) + 1);
	strcpy(pCommand, MSG_FTPFILE_SCRIPT);
	strcat(pCommand, PackageValueGet(PACKAGE_PATCH, 0, szPackage));
	iResult = FileExec(pCommand);
	if (iResult != SUCCESS)
	{
		Log(LOG_ERROR, LOG_MODULE_INSTALL, "Cannot execute script");
		Log(LOG_MESSAGE, LOG_MODULE_INSTALL, pCommand);
		free(pCommand);
		return FAILURE;
	}
	free(pCommand);
	
	gtk_label_set_text(pAction, "");
	while (g_main_iteration(FALSE));

	return SUCCESS;
}



static int Unpacking(const char *szPackage)
{
	int iResult;
	char *pCommand;

	Log(LOG_MESSAGE, LOG_MODULE_INSTALL, "Unpacking");
	if (PackageValueGet(PACKAGE_FILENAME, 0, szPackage) == NULL)
	{
		Log(LOG_ERROR, LOG_MODULE_INSTALL, "Cannot find package configuration");
		Log(LOG_MESSAGE, LOG_MODULE_INSTALL, szPackage);
		return FAILURE;
	}
	if (strlen(FileGetFilename(PackageValueGet(PACKAGE_FILENAME, 0, szPackage))) == 0)
	{
		Log(LOG_ERROR, LOG_MODULE_INSTALL, "Cannot find package configuration");
		Log(LOG_MESSAGE, LOG_MODULE_INSTALL, szPackage);
		return FAILURE;
	}
	if (FileTest(FileGetFilename(PackageValueGet(PACKAGE_FILENAME, 0, szPackage))) != SUCCESS)
	{
		Log(LOG_ERROR, LOG_MODULE_INSTALL, "File not found");
		Log(LOG_MESSAGE, LOG_MODULE_INSTALL, FileGetFilename(PackageValueGet(PACKAGE_FILENAME, 0, szPackage)));
		return FAILURE;
	}
	gtk_label_set_text(pName, PackageValueGet(PACKAGE_NAME, 0, szPackage));
	gtk_label_set_text(pAction, "Unpacking");
	while (g_main_iteration(FALSE));

	pCommand = (char *) malloc(strlen(MSG_UNPACK_SCRIPT) + strlen(FileGetFilename(PackageValueGet(PACKAGE_FILENAME, 0, szPackage))) + 1);
	strcpy(pCommand, MSG_UNPACK_SCRIPT);
	strcat(pCommand, FileGetFilename(PackageValueGet(PACKAGE_FILENAME, 0, szPackage)));
	iResult = FileExec(pCommand);
	if (iResult != SUCCESS)
	{
		Log(LOG_ERROR, LOG_MODULE_INSTALL, "Cannot execute script");
		Log(LOG_MESSAGE, LOG_MODULE_INSTALL, pCommand);
		free(pCommand);
		return FAILURE;
	}
	free(pCommand);

	return SUCCESS;
}



static int Patching(const char *szPackage)
{
	int iResult;
	char *pCommand;

	Log(LOG_MESSAGE, LOG_MODULE_INSTALL, "Patching");
	if (PackageValueGet(PACKAGE_PATCH, 0, szPackage) == NULL)
	{
		Log(LOG_ERROR, LOG_MODULE_INSTALL, "Cannot find package configuration");
		Log(LOG_MESSAGE, LOG_MODULE_INSTALL, szPackage);
		return FAILURE;
	}
	if (strlen(PackageValueGet(PACKAGE_PATCH, 0, szPackage)) == 0)
		return SUCCESS;
	if (FileTest(FileGetFilename(PackageValueGet(PACKAGE_PATCH, 0, szPackage))) != SUCCESS)
	{
		Log(LOG_ERROR, LOG_MODULE_INSTALL, "File not found");
		Log(LOG_MESSAGE, LOG_MODULE_INSTALL, FileGetFilename(PackageValueGet(PACKAGE_PATCH, 0, szPackage)));
		return FAILURE;
	}
	gtk_label_set_text(pName, PackageValueGet(PACKAGE_NAME, 0, szPackage));
	gtk_label_set_text(pAction, "Patching");
	while (g_main_iteration(FALSE));

	/*
	iResult = chdir(PackageValueGet(PACKAGE_DIRNAME, 0, szPackage));
	if (iResult != 0)
	{
		Log(LOG_ERROR, LOG_MODULE_INSTALL, "Cannot change directory");
		Log(LOG_MESSAGE, LOG_MODULE_INSTALL, PackageValueGet(PACKAGE_DIRNAME, 0, szPackage));
		return FAILURE;
	}
	*/
	
	pCommand = (char *) malloc(strlen(MSG_PATCH_SCRIPT) + strlen(PackageValueGet(PACKAGE_PATCH, 0, szPackage)) + 1);
	strcpy(pCommand, MSG_PATCH_SCRIPT);
	strcat(pCommand, PackageValueGet(PACKAGE_PATCH, 0, szPackage));
	iResult = FileExec(pCommand);
	if (iResult != SUCCESS)
	{
		Log(LOG_ERROR, LOG_MODULE_INSTALL, "Cannot execute script");
		Log(LOG_MESSAGE, LOG_MODULE_INSTALL, pCommand);
		free(pCommand);
		return FAILURE;
	}
	free(pCommand);

	/*
	iResult = chdir(szWorkingDirectory);
	if (iResult != 0)
	{
		Log(LOG_ERROR, LOG_MODULE_INSTALL, "Cannot change directory");
		Log(LOG_MESSAGE, LOG_MODULE_INSTALL, szWorkingDirectory);
		return FAILURE;
	}
	*/

	return SUCCESS;
}



static int InstallPreprocessing(const char *szPackage)
{
	int iResult;

	Log(LOG_MESSAGE, LOG_MODULE_INSTALL, "Executing initial script");
	if (PackageValueGet(PACKAGE_PREINST, 0, szPackage) == NULL)
	{
		Log(LOG_ERROR, LOG_MODULE_INSTALL, "Cannot find package configuration");
		Log(LOG_MESSAGE, LOG_MODULE_INSTALL, szPackage);
		return FAILURE;
	}
	if (strlen(PackageValueGet(PACKAGE_PREINST, 0, szPackage)) == 0)
		return SUCCESS;
	gtk_label_set_text(pName, PackageValueGet(PACKAGE_NAME, 0, szPackage));
	gtk_label_set_text(pAction, "Executing initial script");
	while (g_main_iteration(FALSE));

	iResult = chdir(PackageValueGet(PACKAGE_DIRNAME, 0, szPackage));
	if (iResult != 0)
	{
		Log(LOG_ERROR, LOG_MODULE_INSTALL, "Cannot change directory");
		Log(LOG_MESSAGE, LOG_MODULE_INSTALL, PackageValueGet(PACKAGE_DIRNAME, 0, szPackage));
		return FAILURE;
	}

	iResult = FileExec(PackageValueGet(PACKAGE_PREINST, 0, szPackage));
	if (iResult != SUCCESS)
	{
		Log(LOG_ERROR, LOG_MODULE_INSTALL, "Cannot execute script");
		Log(LOG_MESSAGE, LOG_MODULE_INSTALL, PackageValueGet(PACKAGE_PREINST, 0, szPackage));
		return FAILURE;
	}

	iResult = chdir(szWorkingDirectory);
	if (iResult != 0)
	{
		Log(LOG_ERROR, LOG_MODULE_INSTALL, "Cannot change directory");
		Log(LOG_MESSAGE, LOG_MODULE_INSTALL, szWorkingDirectory);
		return FAILURE;
	}

	return SUCCESS;
}



static int VerifyFiles(const char *szPackage)
{
	struct CompsTable_s *pPkg;
	FILE *File;
	int iResult, iFileType = PACKAGE_FILE_UNKNOWN, i, j;
	char szFileSource[TEXTLEN], szFileDest[TEXTLEN], szValue[TEXTLEN], szMessage[TEXTLEN];

	pPkg = get_component_by_name(szPackage);
	
	Log(LOG_MESSAGE, LOG_MODULE_INSTALL, "Verifying files");
	if (PackageValueGet(PACKAGE_FILENAME, 0, szPackage) == NULL)
	{
		Log(LOG_ERROR, LOG_MODULE_INSTALL, "Cannot find package configuration");
		Log(LOG_MESSAGE, LOG_MODULE_INSTALL, szPackage);
		FatalError(__FILE__, __LINE__, __DATE__);
	}
	gtk_label_set_text(pName, PackageValueGet(PACKAGE_NAME, 0, szPackage));
	gtk_label_set_text(pAction, "Verifying files");
	while (g_main_iteration(FALSE));

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
		return FAILURE;
	}

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
				{
	iResult = chdir(szWorkingDirectory);

					return FAILURE;
	}
				fclose(File);
				break;
		}
	}
	iResult = chdir(szWorkingDirectory);

	return SUCCESS;
}



static int TestBuildTools(const char *szPackage)
{
	int iResult, i, j;
	char szBldTools[TEXTLEN], szCodeName[TEXTLEN], szMessage[TEXTLEN];

	Log(LOG_MESSAGE, LOG_MODULE_INSTALL, "Testing build tools");
	if (PackageValueGet(PACKAGE_BLDTOOLS, 0, szPackage) == NULL)
	{
		Log(LOG_ERROR, LOG_MODULE_INSTALL, "Cannot find package configuration");
		Log(LOG_MESSAGE, LOG_MODULE_INSTALL, szPackage);
		return FAILURE;
	}
	if (strlen(PackageValueGet(PACKAGE_BLDTOOLS, 0, szPackage)) == 0)
		return SUCCESS;
	gtk_label_set_text(pName, PackageValueGet(PACKAGE_NAME, 0, szPackage));
	gtk_label_set_text(pAction, "Testing build tools");
	while (g_main_iteration(FALSE));

	strcpy(szBldTools, PackageValueGet(PACKAGE_BLDTOOLS, 0, szPackage));
	for (i = 0; i < strlen(szBldTools); i ++)
	{
		/* get code name of a tool */
		for (; i <strlen(szBldTools) && isspace(szBldTools[i]);)
			i ++;
		for (j = 0; i < strlen(szBldTools) && !isspace(szBldTools[i]);)
			szCodeName[j ++] = szBldTools[i ++];
		szCodeName[j] = 0;

		iResult = PackageTestIfInstalled(get_component_by_name(szCodeName));
		if (iResult != SUCCESS)
		{
			if (strlen(FileGetFilename(PackageValueGet(PACKAGE_FILENAME, 0, szCodeName))) == 0)
			{
				Log(LOG_ERROR, LOG_MODULE_INSTALL, "Build tool not found");
				Log(LOG_MESSAGE, LOG_MODULE_INSTALL, PackageValueGet(PACKAGE_NAME, 0, szCodeName));

				sprintf(szMessage, "%s not found.\nPlease install it and restart setup.", (char *) PackageValueGet(PACKAGE_NAME, 0, szCodeName));
				MsgBox(GTK_WINDOW(pWindowMain), "Error !", szMessage, MSGBOX_ERROR | MSGBOX_OKD);
				return FAILURE;
			}

			iResult = InstallComponent(szCodeName);
			if (iResult != SUCCESS)
				return FAILURE;
		}
	}

	return SUCCESS;
}



static int BuildFiles(const char *szPackage)
{
	int iResult;

	Log(LOG_MESSAGE, LOG_MODULE_INSTALL, "Building files");
	if (PackageValueGet(PACKAGE_BLDCMD, 0, szPackage) == NULL)
	{
		Log(LOG_ERROR, LOG_MODULE_INSTALL, "Cannot find package configuration");
		return FAILURE;
	}
	gtk_label_set_text(pName, PackageValueGet(PACKAGE_NAME, 0, szPackage));
	gtk_label_set_text(pAction, "Building files");
	while (g_main_iteration(FALSE));

	iResult = chdir(PackageValueGet(PACKAGE_DIRNAME, 0, szPackage));
	if (iResult != 0)
	{
		Log(LOG_ERROR, LOG_MODULE_INSTALL, "Cannot change directory");
		Log(LOG_MESSAGE, LOG_MODULE_INSTALL, PackageValueGet(PACKAGE_DIRNAME, 0, szPackage));
		return FAILURE;
	}

	iResult = FileExec(PackageValueGet(PACKAGE_BLDCMD, 0, szPackage));
	if (iResult != SUCCESS)
	{
		Log(LOG_ERROR, LOG_MODULE_INSTALL, "Cannot execute script");
		Log(LOG_MESSAGE, LOG_MODULE_INSTALL, PackageValueGet(PACKAGE_BLDCMD, 0, szPackage));
		return FAILURE;
	}

	iResult = chdir(szWorkingDirectory);
	if (iResult != 0)
	{
		Log(LOG_ERROR, LOG_MODULE_INSTALL, "Cannot change directory");
		Log(LOG_MESSAGE, LOG_MODULE_INSTALL, szWorkingDirectory);
		return FAILURE;
	}

	return SUCCESS;
}



static int InstallFiles(const char *szPackage)
{
	int iResult;

	Log(LOG_MESSAGE, LOG_MODULE_INSTALL, "Installing files");
	if (PackageValueGet(PACKAGE_INSTCMD, 0, szPackage) == NULL)
	{
		Log(LOG_ERROR, LOG_MODULE_INSTALL, "Cannot find package configuration");
		return FAILURE;
	}
	gtk_label_set_text(pName, PackageValueGet(PACKAGE_NAME, 0, szPackage));
	gtk_label_set_text(pAction, "Installing files");
	while (g_main_iteration(FALSE));

	iResult = chdir(PackageValueGet(PACKAGE_DIRNAME, 0, szPackage));
	if (iResult != 0)
	{
		Log(LOG_ERROR, LOG_MODULE_INSTALL, "Cannot change directory");
		Log(LOG_MESSAGE, LOG_MODULE_INSTALL, PackageValueGet(PACKAGE_DIRNAME, 0, szPackage));
		return FAILURE;
	}

	iResult = FileExec(PackageValueGet(PACKAGE_INSTCMD, 0, szPackage));
	if (iResult != SUCCESS)
	{
		Log(LOG_ERROR, LOG_MODULE_INSTALL, "Cannot execute script");
		Log(LOG_MESSAGE, LOG_MODULE_INSTALL, PackageValueGet(PACKAGE_BLDCMD, 0, szPackage));
		return FAILURE;
	}

	iResult = chdir(szWorkingDirectory);
	if (iResult != 0)
	{
		Log(LOG_ERROR, LOG_MODULE_INSTALL, "Cannot change directory");
		Log(LOG_MESSAGE, LOG_MODULE_INSTALL, szWorkingDirectory);
		return FAILURE;
	}

	return SUCCESS;
}



static int CopyFiles(const char *szPackage)
{
	struct CompsTable_s *pPkg;
	mode_t Mode;
	int iFileType = PACKAGE_FILE_UNKNOWN, iTotalResult = SUCCESS, iResult, i, j;
	char szFileSource[TEXTLEN], szFileDest[TEXTLEN], szMessage[TEXTLEN], szValue[TEXTLEN];

	Log(LOG_MESSAGE, LOG_MODULE_INSTALL, "Copying files");
	if (PackageValueGet(PACKAGE_NAME, 0, szPackage) == NULL)
	{
		Log(LOG_ERROR, LOG_MODULE_INSTALL, "Cannot find package configuration");
		return FAILURE;
	}
	gtk_label_set_text(pName, PackageValueGet(PACKAGE_NAME, 0, szPackage));
	gtk_label_set_text(pAction, "Copying files");
	while (g_main_iteration(FALSE));

	iResult = chdir(PackageValueGet(PACKAGE_DIRNAME, 0, szPackage));
	if (iResult != 0)
	{
		Log(LOG_ERROR, LOG_MODULE_INSTALL, "Cannot change directory");
		Log(LOG_MESSAGE, LOG_MODULE_INSTALL, PackageValueGet(PACKAGE_DIRNAME, 0, szPackage));
		return FAILURE;
	}

	pPkg = get_component_by_name(szPackage);
	
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

		sprintf(szMessage, "Copying %s", szFileDest);
		gtk_label_set_text(pAction, szMessage);
		while (g_main_iteration(FALSE));

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
					sprintf(szMessage, "Cannot install file\n'%s'/'%s'", szInstallDirectory, szFileDest);
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

	iResult = chdir(szWorkingDirectory);
	if (iResult != 0)
	{
		Log(LOG_ERROR, LOG_MODULE_INSTALL, "Cannot change directory");
		Log(LOG_MESSAGE, LOG_MODULE_INSTALL, szWorkingDirectory);
		return FAILURE;
	}

	return iTotalResult;
}



static int InstallPostprocessing(const char *szPackage)
{
	int iResult;

	Log(LOG_MESSAGE, LOG_MODULE_INSTALL, "Executing final script");
	if (PackageValueGet(PACKAGE_POSTINST, 0, szPackage) == NULL)
	{
		Log(LOG_ERROR, LOG_MODULE_INSTALL, "Cannot find package configuration");
		return FAILURE;
	}
	if (strlen(PackageValueGet(PACKAGE_POSTINST, 0, szPackage)) == 0)
		return SUCCESS;
	gtk_label_set_text(pName, PackageValueGet(PACKAGE_NAME, 0, szPackage));
	gtk_label_set_text(pAction, "Executing final script");
	while (g_main_iteration(FALSE));

	iResult = chdir(PackageValueGet(PACKAGE_DIRNAME, 0, szPackage));
	if (iResult != 0)
	{
		Log(LOG_ERROR, LOG_MODULE_INSTALL, "Cannot change directory");
		Log(LOG_MESSAGE, LOG_MODULE_INSTALL, PackageValueGet(PACKAGE_DIRNAME, 0, szPackage));
		return FAILURE;
	}

	iResult = FileExec(PackageValueGet(PACKAGE_POSTINST, 0, szPackage));
	if (iResult != SUCCESS)
	{
		Log(LOG_ERROR, LOG_MODULE_INSTALL, "Cannot execute script");
		Log(LOG_MESSAGE, LOG_MODULE_INSTALL, PackageValueGet(PACKAGE_POSTINST, 0, szPackage));
		return FAILURE;
	}

	iResult = chdir(szWorkingDirectory);
	if (iResult != 0)
	{
		Log(LOG_ERROR, LOG_MODULE_INSTALL, "Cannot change directory");
		Log(LOG_MESSAGE, LOG_MODULE_INSTALL, szWorkingDirectory);
		return FAILURE;
	}

	return SUCCESS;
}



















/*
	A set of functions that unpack tgz archive, check if all needed files are present,
	if not then build them, copy files, create links and remove temporary directories
*/



static BOOL RequirementsMet(const char *szPackage)
{
	int iResult = TRUE, i;
	char szCodeName[TEXTLEN];
	
	for (i = 0; i < strlen(PackageValueGet(PACKAGE_REQLIST, 0, szPackage)); i ++)
	{
		i = get_next_component(get_component_by_name(szPackage), i, szCodeName);
		if (strlen(szCodeName) == 0)
			break;
		
		if (*((BOOL *) PackageValueGet(PACKAGE_INSTALLED, 0, szCodeName)))
			continue;
		
		if (PackageTestIfInstalled(get_component_by_name(szCodeName)) == SUCCESS)
			continue;

		iResult = FALSE;
		break;
	}

	return iResult;
}



void mark_to_be_installed(void)
{
	int iValue, i, bChanged;
	char szCodeName[TEXTLEN], szPackage[TEXTLEN], *pPackage;
	
	/* cancel all REQUIRED markers */
	for (
		pPackage = (char *) PackageValueGet(PACKAGE_NEXT, 0, NULL);
		pPackage != NULL;
		pPackage = (char *) PackageValueGet(PACKAGE_NEXT, 0, szPackage)
		)
	{
		strcpy(szPackage, pPackage);
		
		if (*((int *) PackageValueGet(PACKAGE_FLAGINST, 0, szPackage)) == PACKAGE_REQUIRED)
		{
			iValue = PACKAGE_IGNORED;
			PackageValueSet(PACKAGE_FLAGINST, 0, szPackage, (void *) &iValue);
		}
	}
	
	for (
		pPackage = (char *) PackageValueGet(PACKAGE_NEXT, 0, NULL);
		pPackage != NULL;
		pPackage = (char *) PackageValueGet(PACKAGE_NEXT, 0, szPackage)
		)
	{
		strcpy(szPackage, pPackage);
		bChanged = FALSE;
		
		if (*((int *) PackageValueGet(PACKAGE_FLAGINST, 0, szPackage)) == PACKAGE_SELECTED
			|| *((int *) PackageValueGet(PACKAGE_FLAGINST, 0, szPackage)) == PACKAGE_REQUIRED
			|| *((int *) PackageValueGet(PACKAGE_FLAGINST, 0, szPackage)) == PACKAGE_BLDTOOL
			)
		{
			for (i = 0; i < strlen(PackageValueGet(PACKAGE_REQLIST, 0, szPackage)); i ++)
			{
				i = get_next_component(get_component_by_name(szPackage), i, szCodeName);
				if (strlen(szCodeName) == 0)
					break;

				if (*((int *) PackageValueGet(PACKAGE_FLAGINST, 0, szCodeName)) == PACKAGE_IGNORED)
				{
					iValue = PACKAGE_REQUIRED;
					PackageValueSet(PACKAGE_FLAGINST, 0, szCodeName, (void *) &iValue);
					bChanged = TRUE;
				}
			}
			
			if (bChanged)
				pPackage = (char *) PackageValueGet(PACKAGE_NEXT, 0, NULL);
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
			if (pComp == get_component_by_name("SOFTWARE"))
				continue;
			if (strlen(pComp->szFileName) == 0)
				continue;

			iTotal ++;
			if (pComp->bInstalled == TRUE)
				iDone ++;
		}
	}

	fPercentage = 100.0 * (float)iDone / (float)iTotal;

	return (int)fPercentage;
}


static int VariableAdd(const char *szVariable, const char *szValue)
{
	FILE *hFileOrig, *hFileTmp;
	int i;
	char szMessage[TEXTLEN], szFileNameOrig[TEXTLEN], szFileNameTmp[TEXTLEN], *pResult;
	
	sprintf(szMessage, "Updating environment variable %s to %s", szVariable, szValue);
	Log(LOG_MESSAGE, LOG_MODULE_INSTALL, szMessage);

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



static int VariableCheck(const char *szVariable, const char *szValue)
{
	char *pResult, szEntry[TEXTLEN], szPath[TEXTLEN];
	int i, j;

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
				return SUCCESS;
		}
	}

	return FAILURE;
}
