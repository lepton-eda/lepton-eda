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

#include <stdio.h>
#include <string.h>
#include "log.h"
#include "global.h"
#include "msgbox.h"



/* general purpose string for message logging */
char szLogMessage[TEXTLEN];
int iLogVerbose = FALSE;



/*
	logging to a file and standard output
*/

void Log(const int iImportance, const int iModule, const char *szValue)
{
	FILE *LogFile;
	char szMessage[TEXTLEN], szValue1[TEXTLEN]/*, szValue2[TEXTLEN]*/;

	/* prepare a message */
	strcpy(szValue1, "### ");
	switch (iImportance)
	{
		case LOG_FATAL:     strcat(szValue1, "FATAL ERROR !");   break;
		case LOG_ERROR:     strcat(szValue1, "ERROR !");         break;
		case LOG_WARNING:   strcat(szValue1, "WARNING !");       break;
		case LOG_MESSAGE:   strcat(szValue1, "");                break;
	}
#if 0
	switch (iModule)
	{
		case LOG_MAIN:              strcpy(szValue2, "main");       break;
		case MODULE_PACKAGE:        strcpy(szValue2, "package");    break;
		case LOG_START:             strcpy(szValue2, "start");      break;
		case LOG_MODULE_COMPS:      strcpy(szValue2, "comps");      break;
		case MODULE_LICENSE:        strcpy(szValue2, "license");    break;
		case MODULE_DIRS:           strcpy(szValue2, "dirs");       break;
		case MODULE_SUMMARY:        strcpy(szValue2, "summary");    break;
		case MODULE_STATUS:         strcpy(szValue2, "status");     break;
		case LOG_MODULE_INSTALL:    strcpy(szValue2, "install");    break;
		case LOG_FILE:              strcpy(szValue2, "file");       break;
		case LOG_MODULE_CALLBACK:   strcpy(szValue2, "callback");   break;
		default:                    strcpy(szValue2, "???");        break;
	}
#endif
	sprintf(szMessage, "%s %s\n", szValue1, /*szValue2,*/ szValue);
	
	/* logging to a standard output */
	if (iLogVerbose)
		printf("%s", szMessage);
	
	/* logging to a log file */
	LogFile = fopen(LOG_FILENAME, "a");
	if (LogFile == NULL)
	{
		printf("ERROR ! Cannot write to a log file %s\n", LOG_FILENAME);
		return;
	}
	fprintf(LogFile, "%s", szMessage);
	fclose(LogFile);

#if 0
	/* message box */
	switch (iImportance)
	{
		case LOG_FATAL:     sprintf(szMessage, "FATAL ERROR ! %s", szValue);   MsgBox(szMessage, MSGBOX_OK);   break;
		case LOG_ERROR:     sprintf(szMessage, "ERROR ! %s", szValue);         MsgBox(szMessage, MSGBOX_OK);   break;
		case LOG_WARNING:                                                                                      break;
		case LOG_MESSAGE:                                                                                      break;
	}
#endif
}
