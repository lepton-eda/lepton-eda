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
#define GTK_ENABLE_BROKEN
#include <gtk/gtk.h>
#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>
#include "filetool.h"
#include "global.h"
#include "support.h"



/* PID of last thread created by FileExec() */
static pid_t Pid = -1;



/* 
	check if file exists 
*/

int FileIsExisting(const char *szFileName)
{
	FILE *fp;
	
	fp = fopen(szFileName, "r");
	if (fp == NULL)
		return FAILURE;
	
	fclose(fp);

	return SUCCESS;
}



/* 
	copy file 
*/

int FileCopy(const char *szSource, const char *szDest)
{
	FILE *Source, *Dest;

	Source = fopen(szSource, "r");
	if (Source == NULL)
	{
		/* TODO: error handling */
		return FAILURE;
	}
	
	Dest = fopen(szDest, "w");
	if (Dest == NULL)
	{
		/* TODO: error handling */
		return FAILURE;
	}
	
	while (!feof(Source))
		fputc(fgetc(Source), Dest);
	
	fclose(Dest);
	fclose(Source);
	
	return SUCCESS;
}



/* 
	get file name, extension and directory from full path 
*/

char *FileGetName(const char *szFilename)
{
	static char szName[TEXTLEN];
	int i, j;

	/* extract only filename with extension */
	for (i = strlen(szFilename) - 1; i >= 0 && szFilename[i] != G_DIR_SEPARATOR; i --)
		;
	if (i < 0)
		i = 0;
	if (szFilename[i] == G_DIR_SEPARATOR)
		i ++;
	if (i > strlen(szFilename) - 1)
		i = strlen(szFilename) - 1;
	strcpy(szName, szFilename + i);
	
	/* remove extension */
	for (j = strlen(szName) - 1; j >= 0 && szName[j] != '.'; j --)
		;
	if (j >= 0)
		szName[j] = 0;

	return szName;
}


char *FileGetExt(const char *szFilename)
{
	static char szExt[TEXTLEN];
	int i;
	
	szExt[0] = 0;
	for (i = strlen(szFilename) - 1; i >= 0 && szFilename[i] != '/' && szFilename[i] != '.'; i --)
		;
	if (szFilename[i] == '.')
	{
		if (strlen(szFilename + i + 1) < TEXTLEN)
			strcpy(szExt, szFilename + i + 1);
	}
	
	return szExt;
}


char *FileGetDir(const char *szFilename)
{
	static char szDir[TEXTLEN];
	int i;
	
	strncpy(szDir, szFilename, TEXTLEN - 1);
	for (i = strlen(szDir) - 1; i >= 0 && szDir[i] != '/'; i --)
		;
	if (szDir[i] == '/' && i < TEXTLEN)
		szDir[i] = 0;
	else
		szDir[0] = 0;
	
	return szDir;
}




char *FileGetRel(const char *szFilename)
{
	int i, j, k;
	static char szRel[TEXTLEN];
	char szDirectory[TEXTLEN], *pResult;
	
	/* get current directory */
	pResult = getcwd(szDirectory, TEXTLEN - 1);
	if (pResult == NULL)
	{
		strcpy(szRel, "");
		return szRel;
	}
	

	/* if file name is a relative one */
	if (szFilename[0] != '/')
	{
		strcpy(szRel, szFilename);
	}
		
	/* if the file exists in a subdirectory */
	else if (!strncmp(szFilename, szDirectory, strlen(szDirectory)))
	{
		strcpy(szRel, szFilename + strlen(szDirectory) + 1);
	}
	
	/* if the file exists in a different directory tree */
	else
	{
		for (i = 0; i < strlen(szFilename); i ++)
			if (strncmp(szFilename, szDirectory, i))
				break;
		if (i > 0)
			i --;
		
		for (j = i, k = 1; j < strlen(szDirectory); j ++)
			if (szDirectory[j] == '/')
				k ++;
			
		strcpy(szRel, "");
		for (j = 0; j < k; j ++)
			strcat(szRel, "../");
		strcat(szRel, szFilename + i);
	}
	
	return szRel;
}



int FileExec(const char *szCommand)
{
	GtkText *pText;
	FILE *hStdOut, *hStdErr;
	char szValue[TEXTLEN], szText[2000];
	int iResult, i, j;

	/* create a new process to execute external shell commands) */
	Pid = fork();
	if (Pid < 0)
	{
		return FAILURE;
	}
	
	/* run the command in the child process */
	if (Pid == 0)
	{
		Pid = getpid();
		
		/* redirecting stdout */
		sprintf(szValue, "/%s/%s-stdout-%d", GM_TMPDIR, GM_TMPNAME, Pid);
		hStdOut = freopen(szValue, "w", stdout);
		if (hStdOut == NULL)
		{
			_exit(0);
		}

		/* redirecting stderr */
		sprintf(szValue, "/%s/%s-stderr-%d", GM_TMPDIR, GM_TMPNAME, Pid);
		hStdErr = freopen(szValue, "w", stderr);
		if (hStdErr == NULL)
		{
			_exit(0);
		}
		
		/* execute command */
		execl("/bin/sh", "sh", "-c", szCommand, NULL);

		/* end child process */
		_exit(0);
	}

	/* wait for death of child in the parent process */
	while (waitpid(Pid, NULL, WNOHANG) == 0)
	{
		sleep(1);
		while (g_main_iteration(FALSE));
	}

	/* copy stderr to sterr window */
	pText = GTK_TEXT(lookup_widget(GTK_WIDGET(pWindowMain), "StatusText"));
	if (pText == NULL)
	{
		/* TODO: error handling */
	}
	else
	{
		/* print stderr */
		sprintf(szValue, "/%s/%s-stderr-%d", GM_TMPDIR, GM_TMPNAME, Pid);
		hStdErr = fopen(szValue, "r");
		if (hStdErr == NULL)
		{
			/* TODO */
		}
		else 
		{
			j = 0;
			while (!feof(hStdErr)) 
			{
				i = fgetc(hStdErr);
				if (i < 0)
					break;
				
				szText[j++] = (char) '*';
			}
			
			szText[j] = 0;
				gtk_text_set_point(pText, 0);
				gtk_text_forward_delete(pText, gtk_text_get_length(pText));
				gtk_text_insert(pText, NULL, NULL, NULL, szText, strlen(szText));
				gtk_widget_show(GTK_WIDGET(pText));
				while (g_main_iteration(FALSE));
			fclose(hStdErr);
		}
	}
	
	/* remove last stdout file */
	sprintf(szValue, "/%s/%s-stdout-%d", GM_TMPDIR, GM_TMPNAME, Pid);
	iResult = remove(szValue);
	if (iResult != 0)
	{
		/* TODO: error handling */
	}

	/* remove last stderr file */
	sprintf(szValue, "/%s/%s-stderr-%d", GM_TMPDIR, GM_TMPNAME, Pid);
	iResult = remove(szValue);
	if (iResult != 0)
	{
		/* TODO: error handling */
	}

	/* TODO: in this manner the function never return failure, fix it */
	
	return SUCCESS;
}


