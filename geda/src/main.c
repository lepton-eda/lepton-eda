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

#include <gtk/gtk.h>
#include <libintl.h>
#include <stdlib.h>

#ifdef HAVE_STRING_H  
#include <string.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef HAVE_TIME_H
#include <time.h>
#endif

#include "doc.h"
#include "global.h"
#include "interface.h"
#include "msgbox.h"
#include "project.h"
#include "tool.h"
#include "window.h"



/*******************************************************************************

	Static functions and variables

*******************************************************************************/

static void MainLoop(void);



/*******************************************************************************

	Global variables

*******************************************************************************/

/* default values */
char *pDefaultProjectName = NULL;
char *pDefaultProjectExt = NULL;
char *pDefaultProjectDir = NULL; 

/* registered windows */
GtkWindow *pWindowMain = NULL;

/* running flag */
int bRunning = TRUE;



/*******************************************************************************

	Main application function

*******************************************************************************/

int main(int argc, char *argv[])
{
	int iResult;

#if ENABLE_NLS
	bindtextdomain (PACKAGE, PACKAGE_LOCALE_DIR);
	textdomain (PACKAGE);
#endif
	gtk_set_locale ();
	gtk_init (&argc, &argv);
#if 0
	add_pixmap_directory (PACKAGE_DATA_DIR "/pixmaps");
	add_pixmap_directory (PACKAGE_SOURCE_DIR "/pixmaps");
#endif
	
	
	/* 
		create windows 
	*/
	
 	pWindowMain = GTK_WINDOW(create_MainWindow());
	gtk_window_set_title(GTK_WINDOW(pWindowMain), GEDA_TITLE);
	gtk_widget_show(GTK_WIDGET(pWindowMain));


	/* 
		set defaults
	*/
	
	pDefaultProjectName = (char *) malloc(strlen(DEF_PRJNAME) + 1);
	if (pDefaultProjectName == NULL)
		FatalError(__FILE__, __LINE__, __DATE__);
	strcpy(pDefaultProjectName, DEF_PRJNAME);

	pDefaultProjectExt = (char *) malloc(strlen(DEF_PRJEXT) + 1);
	if (pDefaultProjectExt == NULL)
		FatalError(__FILE__, __LINE__, __DATE__);
	strcpy(pDefaultProjectExt, DEF_PRJEXT);

	pDefaultProjectDir = (char *) malloc(strlen(getenv("HOME")) + 1);
	if (pDefaultProjectDir == NULL)
		FatalError(__FILE__, __LINE__, __DATE__);
	strcpy(pDefaultProjectDir, getenv("HOME"));
	

	/* 
		initializing modules 
	*/
	
	WindowInitialize();
	iResult = ToolInitialize();
	if (iResult != SUCCESS)
	{
		MsgBox(
			pWindowMain,
			"Error !",
			"Cannot initialize tool list !",
			MSGBOX_ERROR | MSGBOX_OKD
			);
		gtk_main_quit();
		return -1;
	}
	ProjectInitialize();
	DocViewInitialize();
	TaskInitialize();
   

	/* 
		main loop 
	*/
	
	MainLoop();


	/* releasing */
#if 0	
	iResult = ToolRelease();
	if (iResult != SUCCESS)
	{
		/* TODO: error handling */
	}
#endif
	return 0;
}


#ifdef HAVE_NANOSLEEP
static void ReduceCpuSleep(unsigned long usecs)
{
	struct timespec rqtp;
       	rqtp.tv_sec  = usecs / (unsigned long)  1000000;
       	rqtp.tv_nsec = (usecs % (unsigned long) 1000000) * 1000 ;

       	nanosleep(&rqtp, (struct timespec *) NULL);
}
#endif

static void MainLoop(void)
{
	while (bRunning != FALSE)
	{
		TaskProcess();
#ifdef HAVE_NANOSLEEP
		/* Sleep for one microsecond (maybe more) to reduce CPU load */
		ReduceCpuSleep(1000);
#endif
		while (g_main_iteration(FALSE));
	}
}



/*******************************************************************************

	Fatal error handling

	Function prints a message on a console, displayes a message box
	and exits the program.

*******************************************************************************/

void FatalError(const char *szFile, const int iLine, const char *szDate)
{
	char szMessage[TEXTLEN];
	
	sprintf(szMessage, "FATAL ERROR !\n\tFile: %s\n\tLine: %i\n\tDate: %s", szFile, iLine, szDate);
	
	printf("%s\n", szMessage);
	
	MsgBox(
		pWindowMain,
		"FATAL ERROR !",
		szMessage,
		MSGBOX_FATAL | MSGBOX_OKD
		);
	
	_exit(-1);
}
