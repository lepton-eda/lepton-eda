/* $Id$ */

/*******************************************************************************/
/*                                                                             */
/* gEDA Suite Project Manager                                                  */
/*                                                                             */
/* Copyright (C) 2002 Piotr Miarecki, sp9rve@radioam.net                       */
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
/* along with this program; if not, email to the author                        */
/*                                                                             */
/*******************************************************************************/

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include <gtk/gtk.h>
#include <stdio.h>

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "export2gwave.h"
#include "global.h"
#include "graph.h"
#include "msg.h"
#include "interface.h"
#include "libstring.h"
#include "support.h"
#include "value.h"


/*******************************************************************************/
/*	Global variables                                                       */
/*******************************************************************************/

GtkWidget *pMainWindow = NULL;
char *szFilename = NULL, *szExtFilename = NULL, *szFilter = NULL;



/*******************************************************************************/
/*	Main function                                                          */
/*******************************************************************************/

#define GETOPT_PARAMETERS   "f:cogdi:e:m:"

/* possible actions */
#define ACTION_NOT_SELECTED 0
#define ACTION_CREATE       1
#define ACTION_OPEN         2
#define ACTION_GENERATE     3
#define ACTION_DISPLAY      4
#define ACTION_IMPORT       5
#define ACTION_EXPORT       6

int main(int iArgc, char *szArgv[])
{
	int iPid, iAction, i;
	char c, *pString = NULL;
	BOOL bErrorFlag;

	szFilename = StringCreate();
	szExtFilename = StringCreate();
	szFilter = StringCreate();

	iAction = ACTION_NOT_SELECTED;
	bErrorFlag = FALSE;


#ifdef ENABLE_NLS
	bindtextdomain (PACKAGE, PACKAGE_LOCALE_DIR);
	textdomain (PACKAGE);
#endif
	gtk_set_locale();
	gtk_init(&iArgc, &szArgv);
/*
	add_pixmap_directory(PACKAGE_DATA_DIR "/pixmaps");
	add_pixmap_directory(PACKAGE_SOURCE_DIR "/pixmaps");
*/


	/*
		parsing arguments
	*/

	for (
		c = getopt(iArgc, szArgv, GETOPT_PARAMETERS);
		c != -1; 
		c = getopt(iArgc, szArgv, GETOPT_PARAMETERS)
		)
	{
		
		
		switch (c)
		{
			/* 
				graph filename 
			*/

			case 'f':

				StringCopy(&szFilename, optarg);
				break;

			/*
				action type
			*/

			case 'c':

				if (iAction != ACTION_NOT_SELECTED)
				{
					Error(MSG_TOO_MANY_PARAMETERS);
					bErrorFlag = TRUE;
					break;
				}
				iAction = ACTION_CREATE;
				break;

			case 'o':

				if (iAction != ACTION_NOT_SELECTED)
				{
					Error(MSG_TOO_MANY_PARAMETERS);
					bErrorFlag = TRUE;
					break;
				}
				iAction = ACTION_OPEN;
				break;
			
			case 'g':

				if (iAction != ACTION_NOT_SELECTED)
				{
					Error(MSG_TOO_MANY_PARAMETERS);
					bErrorFlag = TRUE;
					break;
				}
				iAction = ACTION_GENERATE;
				break;

			case 'd':

				if (iAction != ACTION_NOT_SELECTED)
				{
					Error(MSG_TOO_MANY_PARAMETERS);
					bErrorFlag = TRUE;
					break;
				}
				iAction = ACTION_DISPLAY;
				break;

			case 'i':

				if (iAction != ACTION_NOT_SELECTED)
				{
					Error(MSG_TOO_MANY_PARAMETERS);
					bErrorFlag = TRUE;
					break;
				}
				iAction = ACTION_IMPORT;
				StringCopy(&szExtFilename, optarg);
				break;

			case 'e':

				if (iAction != ACTION_NOT_SELECTED)
				{
					Error(MSG_TOO_MANY_PARAMETERS);
					bErrorFlag = TRUE;
					break;
				}
				iAction = ACTION_EXPORT;
				StringCopy(&szExtFilename, optarg);
				break;

			/*
				import/export filter
			*/

			case 'm':

				StringCopy(&szFilter, optarg);
				break;

			/*
				GManager PID (for GUI integration)
			*/

			case 'p':

				i = sscanf(optarg, "%i", &iPid);
				if (i != 1)
				{
					Error(MSG_WRONG_PARAMETER);
					bErrorFlag = TRUE;
				}
				break;

			/*
				others
			*/

			case ':':

				Error(MSG_MISSING_PARAMETER);
				bErrorFlag = TRUE;
				break;

			case '?':

				Error(MSG_UNKNOWN_OPTION);
				bErrorFlag = TRUE;
				break;

			case -1:

				break;
		}
	}

	if (StringLength(&szFilename) == 0)
	{
		Error(MSG_FILENAME_NOT_SPECIFIED);
		bErrorFlag = TRUE;
	}

	if (iAction == ACTION_NOT_SELECTED)
	{
		Error(MSG_ACTION_NOT_SELECTED);
		bErrorFlag = TRUE;
	}

	if (
		(iAction == ACTION_IMPORT || iAction == ACTION_EXPORT)
		&& (StringLength(&szFilter) == 0)
		)
	{
		Error(MSG_FILTER_NOT_SPECIFIED);
		bErrorFlag = TRUE;
	}



	/*
		Run appropriate action
	*/

	if (!bErrorFlag) switch (iAction)
	{
		case ACTION_CREATE:
			GraphNew(szFilename);
			GraphSave(szFilename);
			break;

		case ACTION_OPEN:
			pMainWindow = create_MainWindow();
			gtk_widget_show(pMainWindow);
			ValueInitialize();
			GraphLoad(szFilename);
			gtk_main();
			break;

		case ACTION_GENERATE:
			pMainWindow = create_MainWindow();
			gtk_widget_hide(pMainWindow);
			GraphLoad(szFilename);
			if (!strcmp(Graph.szViewer, VIEWER_GWAVE))
			{
				pString = StringCreate();
				StringCopy(&pString, Graph.szFileName);
				StringCat(&pString, ".gw");
				Export2Gwave(pString);
				StringDestroy(&pString);
			}
			break;

		case ACTION_DISPLAY:
			if (!strcmp(Graph.szViewer, VIEWER_GWAVE))
			{
				GraphLoad(szFilename);
				/*...*/
			}
			break;

		case ACTION_IMPORT:
			break;

		case ACTION_EXPORT:
			pMainWindow = create_MainWindow();
			gtk_widget_hide(pMainWindow);
			GraphLoad(szFilename);
			if (!strcmp(szFilter, VIEWER_GWAVE))
			{
				Export2Gwave(szExtFilename);
			}
			break;
	}

	StringDestroy(&szFilename);
	StringDestroy(&szExtFilename);
	StringDestroy(&szFilter);


	return 0;
}



/********************************************************************************

	User error handling

********************************************************************************/

void Error(const char *szErrorString)
{
	fprintf(stderr, "ERROR ! %s\n", szErrorString);
}



/*******************************************************************************

	Fatal error handling

	Function prints a message on a console, displayes a message box
	and exits the program.

*******************************************************************************/

void FatalError(const char *szFile, const int iLine, const char *szDate)
{
	char szMessage[256];
	
	sprintf(szMessage, "FATAL ERROR !\n\tFile: %s\n\tLine: %i\n\tDate: %s", szFile, iLine, szDate);
	fprintf(stderr, "%s\n", szMessage);

	_exit(-1);
}
