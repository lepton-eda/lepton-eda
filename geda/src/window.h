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

#ifndef __WINDOW_H_INCLUDED
#define __WINDOW_H_INCLUDED

#include <gtk/gtk.h>



GtkWidget *gmanager_window_new(const char *szLabel);
void gmanager_window_remove(GtkWidget *pWindow);
void gmanager_window_select(const char *szPath);

void WindowInitialize(void);
char *WindowTop(void);
void WindowSelected(const char *szPath);
void WindowSwitch(GtkNotebook *pNotebook, GtkNotebookPage *pPage, gint iPage, gpointer pUserData);



#endif
