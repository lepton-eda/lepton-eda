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

#ifndef __GLOBAL_H_INCLUDED
#define __GLOBAL_H_INCLUDED

#include <gtk/gtk.h>



#ifndef VERSION
#define VERSION             "19700101"
#endif

#define FAILURE             TRUE
#define SUCCESS             FALSE



/* variable types */
typedef int BOOL;



/* public variables and functions */
extern GtkWidget *pMainWindow;
extern char *szFilename, *szExtFilename, *szFilter;
void Error(const char *szErrorString);
void FatalError(const char *szFile, const int iLine, const char *szDate);



#endif /* __GLOBAL_H_INCLUDED */
