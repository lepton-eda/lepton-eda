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

#ifndef __GLOBAL_H_INCLUDED
#define __GLOBAL_H_INCLUDED

#include <gtk/gtk.h>
#include <sys/types.h>



/*******************************************************************************

	Type definitions

*******************************************************************************/

#ifndef DWORD
#define DWORD                unsigned long
#endif

#ifndef BOOL
#define BOOL                 int
#endif

#define SUCCESS              0
#define FAILURE              1



/*******************************************************************************

	Default values

*******************************************************************************/

#define DEF_PRJNAME          "unnamed"
#define DEF_PRJEXT           "prj"

extern char *pDefaultProjectName;
extern char *pDefaultProjectExt;
extern char *pDefaultProjectDir;



/*******************************************************************************

	Public functions and variables

*******************************************************************************/

extern GtkWindow *pWindowMain;
extern BOOL bRunning;

void FatalError(const char *szFile, const int iLine, const char *szDate);



/*******************************************************************************

	TODO: to be removed

*******************************************************************************/

#define GM_TMPDIR            "tmp"
#define GM_TMPNAME           "gmanager"

/* gEDA version */
#define GEDA_TITLE           "gEDA Suite"

/* configuration file */
#define GEDA_CFGFILE         "geda.cfg"

/* default text length */
#define TEXTLEN              256



#endif /* __GLOBAL_H_INCLUDED */
