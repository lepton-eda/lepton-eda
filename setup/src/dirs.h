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

#ifndef __DIRS_H_INCLUDED
#define __DIRS_H_INCLUDED

#include <gtk/gtk.h>



#define DIRS_LOCAL           1
#define DIRS_GLOBAL          2
#define DIRS_CUSTOM          3
extern int iDirsType;

int DirectoryInitialize(void);
void DirectoryRelease(GtkWidget *pMainWindow);
void dirs_local(GtkWidget *Entry);
void dirs_global(GtkWidget *Entry);
void dirs_custom(GtkWidget *Entry);
void DirectoryShow(GtkWidget *pMainWindow);
void DirectoryHide(GtkWidget *pMainWindow);

/* callback functions */
void on_InstallLocalButton_clicked(GtkButton *pButton, gpointer pUserData);
void on_InstallGlobalButton_clicked(GtkButton *pButton, gpointer pUserData);
void on_InstallCustomButton_clicked(GtkButton *pButton, gpointer pUserData);
void on_InstallDirectoryEntry_changed(GtkEditable *pEditable, gpointer pUserData);



#endif
