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
#include <unistd.h>
#include "file.h"
#include "log.h"

/* TODO: remove it */
#include "package.h"



/* PID of last thread created by FileExec() */
static pid_t Pid = -1;



int FileCopy(char *szSource, char *szDest, mode_t Mode)
{
	FILE *FileSource, *FileDest;
	int iResult, i, j;
	char szMessage[TEXTLEN], szPwd[TEXTLEN], szDirName[TEXTLEN], szFullDest[TEXTLEN], *pResult;
	
	/* read current working directory */
	pResult = getcwd(szPwd, TEXTLEN);
	if (pResult == NULL)
	{
		sprintf(szMessage, "Cannot determine current working directory (Package::FileCopy())");
		Log(LOG_ERROR, MODULE_PACKAGE, szMessage);
		return FAILURE;
	}
	
	/* add installation directory (if only the read one is not an absolute path) */
	if (szDest[0] != '/')
	{
		strcpy(szFullDest, szInstallDirectory);
		strcat(szFullDest, "/");
		strcat(szFullDest, szDest);
	}
		
	/* creating directories */
	for (i = 0, j = 0; i < strlen(szFullDest); )
	{
		/* get destination directory name */
		for (; i < strlen(szFullDest) && szFullDest[i] != '/'; i ++)
			szDirName[j ++] = szFullDest[i];
		for (; i < strlen(szFullDest) && szFullDest[i] == '/'; i ++)
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
				Log(LOG_ERROR, MODULE_PACKAGE, szMessage);
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
			Log(LOG_ERROR, MODULE_PACKAGE, szMessage);
			return FAILURE;
		}
	}
	
	/* copying the file */
	FileSource = fopen(szSource, "r");
	if (FileSource == NULL)
	{
		sprintf(szMessage, "Cannot open file '%s' to copy", szSource);
		Log(LOG_ERROR, MODULE_PACKAGE, szMessage);
		return FAILURE;
	}
	FileDest = fopen(szFullDest, "w");
	if (FileDest == NULL)
	{
		fclose(FileSource);
		sprintf(szMessage, "Cannot create file '%s'", szFullDest);
		Log(LOG_ERROR, MODULE_PACKAGE, szMessage);
		return FAILURE;
	}
	while (!feof(FileSource))
	{
		i = fgetc(FileSource);
		if (i < 0)
			break;
		
		fputc(i, FileDest);
	}
	fclose(FileDest);
	fclose(FileSource);
	
	/* change file attributes */
	iResult = chmod(szFullDest, Mode);
	if (iResult != 0)
	{
		sprintf(szMessage, 
			"Cannot chmod data file '%s'",
			szFullDest
			);
		Log(LOG_ERROR, LOG_FILE, szMessage);
		return FAILURE;
	}

	return SUCCESS;
}


int FileLink(char *szSource, char *szDest)
{
	int iResult;
	char szFullSource[TEXTLEN], szFullDest[TEXTLEN];
	
	strcpy(szFullSource, szInstallDirectory);
	strcat(szFullSource, "/");
	strcat(szFullSource, szSource);
	
	strcpy(szFullDest, szInstallDirectory);
	strcat(szFullDest, "/");
	strcat(szFullDest, szDest);

	/* remove link if it exists */
	iResult = FileTest(szFullDest);
	if (iResult)
	{
		iResult = unlink(szFullDest);
		if (iResult != 0)
			return FAILURE;
	}
	
	iResult = symlink(szFullSource, szFullDest);
	if (iResult != 0)
		return FAILURE;
	
	return SUCCESS;
}


int FileTest(char *szFileName)
{
	FILE *File;
	
	File = fopen(szFileName, "r");
	if (File == NULL)
		return FAILURE;
	
	fclose(File);
	
	return SUCCESS;
}


int FileExec(char *szCommand)
{
	FILE *hLog, *hStdOut, *hStdErr;
	char szValue[TEXTLEN], szMessage[TEXTLEN];
	int iResult, i;

	/* create a new process to execute external shell commands) */
	Pid = fork();
	if (Pid < 0)
	{
		sprintf(szMessage, "Cannot create a new process to execute command '%s'", szCommand);
		Log(LOG_ERROR, MODULE_PACKAGE, szMessage);
		return FAILURE;
	}
	
	/* run the command in the child process */
	if (Pid == 0)
	{
		Pid = getpid();
		
		/* redirecting stdout */
		sprintf(szValue, "/%s/%s-stdout-%d", SETUP_TMPDIR, Software.szDirname, Pid);
		hStdOut = freopen(szValue, "w", stdout);
		if (hStdOut == NULL)
		{
			_exit(0);
		}

		/* redirecting stderr */
		sprintf(szValue, "/%s/%s-stderr-%d", SETUP_TMPDIR, Software.szDirname, Pid);
		hStdErr = freopen(szValue, "w", stderr);
		if (hStdErr == NULL)
		{
			_exit(0);
		}
		
		/* execute command */
		execl("/bin/sh", "sh", "-c", szCommand, NULL);
		sprintf(szMessage, "Cannot execute command '%s'", szCommand);
		Log(LOG_MESSAGE, 0, szMessage);

		/* end child process */
		_exit(0);
	}

	/* wait for death of child in the parent process */
	while (waitpid(Pid, NULL, WNOHANG) == 0)
	{
		sleep(1);
		while (g_main_iteration(FALSE));
	}

	/* copy stderr to log file */
	hLog = fopen(SETUP_LOGFILE, "a");
	if (hLog != NULL)
	{
		/* print header */
		fprintf(hLog, "%s\n", szCommand);
		
		/* print stderr */
		sprintf(szValue, "/%s/%s-stderr-%d", SETUP_TMPDIR, Software.szDirname, Pid);
		hStdErr = fopen(szValue, "r");
		if (hStdErr == NULL)
			fprintf(hLog, "Cannot read logged stderr from file %s\n", szValue);
		else 
		{
			while (!feof(hStdErr)) 
			{
				i = fgetc(hStdErr);
				if (i < 0)
					break;
				fputc(i, hLog);
			}
			
			fclose(hStdErr);
		}
		
		fclose(hLog);
	}
	
	/* remove last stdout file */
	sprintf(szValue, "/%s/%s-stdout-%d", SETUP_TMPDIR, Software.szDirname, Pid);
	iResult = remove(szValue);
	if (iResult != 0)
	{
		/* TODO: error handling */
	}

	/* remove last stderr file */
	sprintf(szValue, "/%s/%s-stderr-%d", SETUP_TMPDIR, Software.szDirname, Pid);
	iResult = remove(szValue);
	if (iResult != 0)
	{
		/* TODO: error handling */
	}

	/* TODO: in this manner the function never return failure, fix it */
	
	return SUCCESS;
}


int FileSearch(char *szSearchDir, char *szSearchName)
{
	int iResult, i, j;
	char szFullPath[TEXTLEN], szDir[TEXTLEN], szName[TEXTLEN], szPreDir[TEXTLEN];

	/* move eventual directory form search name to search directory */
	FileGetDir(szSearchName, szPreDir);
	if (strlen(szPreDir) > 0)
	{
		sprintf(szDir, "%s/%s:%s", szInstallDirectory, szPreDir, szSearchDir);
		i = strlen(szPreDir) + 1;
	}
	else
	{
		strcpy(szDir, szSearchDir);
		i = 0;
	}
	strcpy(szName, szSearchName + i);

	/* check existence of a file in a path */
	for (i = 0; i < strlen(szDir); i ++)
	{
		/* read path entry */
		for (j = 0; i < strlen(szDir) && szDir[i] != ':'; i ++, j ++)
			szFullPath[j] = szDir[i];
		szFullPath[j] = 0;

		/* adding file name to directory */
		strcpy(szFullPath + strlen(szFullPath), "/");
		strcpy(szFullPath + strlen(szFullPath), szName);

		/* checking if the file exists */
		iResult = FileTest(szFullPath);
		if (iResult == SUCCESS)
		{
			return SUCCESS;
		}
	}

	return FAILURE;
}



/*
	get file name, extension and directory from full path
*/

void FileGetName(char *szFullName, char *szName)
{
	int i, j;

	if (szFullName == NULL)
		return;
	
	for (i = strlen(szFullName) - 1; i >= 0 && szFullName[i] != '/'; i --)
		;
	for (j = strlen(szFullName) - 1; j >= i && szFullName[j] != '.'; j --)
		;

	if (j < i)
		j = strlen(szFullName) - 1;
	strncpy(szName, szFullName + i + 1, j - i - 1);
	szName[j - i - 1] = 0;
}


void FileGetExt(char *szFullName, char *szExt)
{
	int i;
	
	if (szFullName == NULL)
		return;
	
	for (i = strlen(szFullName) - 1; i >= 0 && szFullName[i] != '/' && szFullName[i] != '.'; i --)
		;
	if (szFullName[i] == '/')
		strcpy(szExt, "");
	else if (szFullName[i] == '.')
		strcpy(szExt, szFullName + i + 1);
	else
		strcpy(szExt, "");
}


void FileGetDir(char *szFullName, char *szDir)
{
	int i;

	if (szFullName == NULL)
		return;
	
	strcpy(szDir, szFullName);
	for (i = strlen(szDir) - 1; i >= 0 && szDir[i] != '/'; i --)
		;
	if (i >= 0 /*&& szDir[i] == '/'*/)
		szDir[i] = 0;
	else
		szDir[0] = 0;
}




void FileGetRel(char *szFullName, char *szName)
{
	int i, j, k;
	char szDirectory[TEXTLEN], *pResult;

	if (szFullName == NULL)
		return;
	
	/* get current directory */
	pResult = getcwd(szDirectory, TEXTLEN);
	if (pResult == NULL)
	{
		strcpy(szName, "");
		return;
	}

	/* if file name is a relative one */
	if (szFullName[0] != '/')
	{
		strcpy(szName, szFullName);
	}
		
	/* if the file exists in a subdirectory */
	else if (!strncmp(szFullName, szDirectory, strlen(szDirectory)))
	{
		strcpy(szName, szFullName + strlen(szDirectory) + 1);
	}
	
	/* if the file exists in a different directory tree */
	else
	{
		for (i = 0; i < strlen(szFullName); i ++)
			if (strncmp(szFullName, szDirectory, i))
				break;
		if (i > 0)
			i --;
		
		for (j = i, k = 1; j < strlen(szDirectory); j ++)
			if (szDirectory[j] == '/')
				k ++;
			
		strcpy(szName, "");
		for (j = 0; j < k; j ++)
			strcat(szName, "../");
		strcat(szName, szFullName + i);
	}
}



char *FileGetFilename(char *szFullName)
{
	static char szName[TEXTLEN];
	char szExt[TEXTLEN];
	
	if (szFullName == NULL)
		return NULL;
	
	FileGetName(szFullName, szName);
	FileGetExt(szFullName, szExt);
	if (strlen(szExt) > 0)
	{
		strcat(szName, ".");
		strcat(szName, szExt);
	}
	
	return szName;
}

