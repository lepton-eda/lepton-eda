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

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif

#include <glib.h>
#include <gtk/gtk.h>
#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <unistd.h>

#include "comps.h"
#include "dirs.h"
#include "file.h"
#include "interface.h"
#include "license.h"
#include "log.h"
#include "global.h"
#include "msgbox.h"
#include "package.h"
#include "setup.h"
#include "start.h"
#include "status.h"
#include "summary.h"
#include "support.h"
#include "xpm.h"



GtkWidget *pWindowMain;


/* TODO: remove it */
extern int OkPressed;



int main(int argc, char *argv[])
{
	GdkPixmap *pPixmap;
    GdkBitmap *pMask;
    GtkStyle *pStyle;
	GtkWidget *pWidget;
	int iResult;
	char szName[TEXTLEN], szDir[TEXTLEN], szDirStart[TEXTLEN];
	
	gtk_set_locale();
	gtk_init(&argc, &argv);

	/* create main window */
	pWindowMain = create_MainWindow();
	gtk_widget_show(pWindowMain);
	while (g_main_iteration(FALSE));
	
	/* display default window */
	pStyle = gtk_widget_get_style(pWindowMain);
	pPixmap = gdk_pixmap_create_from_xpm_d(pWindowMain->window, &pMask, &pStyle->bg[GTK_STATE_NORMAL], (gchar **) XpmDefault);
	pWidget = lookup_widget(pWindowMain, "StartPixmap");
	if (pWidget == NULL)
	{
		Log(LOG_FATAL, LOG_START, "Cannot find widget: 'StartPixmap'");
		gtk_main_quit();
		return 0;
	}
	gtk_pixmap_set((GtkPixmap *)pWidget, pPixmap, pMask);
	
	/* determine working directory */
	getcwd(szDirStart, TEXTLEN);
	FileGetDir(argv[0], szDir);
	if (strlen(szDir) == 0)
		strcpy(szDir, szDirStart);
	chdir(szDir);
	
	/* initializing modules */
	iResult = PackageInitialize();
	if (iResult != SUCCESS)
		return 0;
	iResult = StartInitialize();
	if (iResult != SUCCESS)
	{
		Log(LOG_FATAL, LOG_MAIN, "Cannot initialize START module");
		return 0;
	}
	iResult = ComponentsInitialize();
	if (iResult != SUCCESS)
	{
		Log(LOG_FATAL, LOG_MAIN, "Cannot initialize COMPONENTS module");
		return 0;
	}
	iResult = LicenseInitialize();
	if (iResult != SUCCESS)
	{
		Log(LOG_FATAL, LOG_MAIN, "Cannot initialize LICENSE module");
		return 0;
	}
	iResult = DirectoryInitialize();
	if (iResult != SUCCESS)
	{
		Log(LOG_FATAL, LOG_MAIN, "Cannot initialize DIRECTORY module");
		return 0;
	}
	iResult = SummaryInitialize();
	if (iResult != SUCCESS)
	{
		Log(LOG_FATAL, LOG_MAIN, "Cannot initialize SUMMARY module");
		return 0;
	}
	iResult = StatusInitialize();
	if (iResult != SUCCESS)
	{
		Log(LOG_FATAL, LOG_MAIN, "Cannot initialize STATUS module");
		return 0;
	}

	/* TODO: move it to the appropriate modules */
	sprintf(szName, "%s %s", Software.szName, Software.szVersion);
	if (strlen(Software.szRelease) > 0)
	{
		strcat(szName, ".");
		strcat(szName, Software.szRelease);
	}
	strcat(szName, " Setup");
	gtk_window_set_title(GTK_WINDOW(pWindowMain), szName);

	pWidget = lookup_widget(pWindowMain, "InstallDirectoryEntry");
	dirs_local(pWidget);
	pWidget = lookup_widget(pWindowMain, "OkButton");
	gtk_widget_set_sensitive(pWidget, FALSE);
	pWidget = lookup_widget(pWindowMain, "PreviousButton");
	gtk_widget_set_sensitive(pWidget, FALSE);
	
	/* main application loop */
	gtk_main();

	/* releasing modules */
	/* TODO */
	iResult = StartRelease();
	if (iResult != SUCCESS)
	{
		Log(LOG_FATAL, LOG_MAIN, "Cannot release START module");
		return 0;
	}
	iResult = PackageRelease();
	if (iResult != SUCCESS)
	{
		Log(LOG_FATAL, LOG_MAIN, "Cannot release PACKAGE module");
		return 0;
	}
	
	return 0;
}

