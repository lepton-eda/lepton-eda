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

#ifndef __DOC_H_INCLUDED
#define __DOC_H_INCLUDED

#include <gtk/gtk.h>
#include "global.h"



/* document properties */
struct Doc_s
{
	char szFileName[TEXTLEN];            /* name of the file */
	struct Doc_s *pParent;               /* pointer to parent, NULL if top level */

	GtkCTreeNode *pNodeModules;
	GtkCTreeNode *pNodeFiles;
	
	union
	{
		struct Window_s *pWindow;
		struct Task_s *pTask;
	} *pOpenedIn;
	
	BOOL bChanged;

	struct Doc_s *pNext;                 /* pointer to next document */
};
extern struct Doc_s *pDocList;

#define DOC_FILENAME         1           /* get relative filename of the document */
#define DOC_NEXT             2           /* get relative filename of the next document on the list */
#define DOC_SELECTED         3           /* get relative name of the selected file */
#define DOC_PARENT           4           /* get parent filename */
#define DOC_CHANGED          5           /* get file changed boolean flag */



/* public functions and variables */
int DocCreate(const char *szFileName, const char *szParentFileName);
int DocDestroy(const char *szFileName);
int DocOpen(const char *szFileName, const int iAction);
int DocClose(const char *szFileName);
int DocSave(const char *szFileName);
int DocLoad(const char *szFileName);
void DocCurrent(char *szProjectName, char *szFileName);
int DocGetNext(char *szFileName);
int DocGetProperty(const int iProperty, const char *szDoc, void *pValue);
void Doc_Selection(GtkCTree *pTree, GList *node, gint column, gpointer user_data);
gboolean on_DocModulesTree_button_press_event(GtkWidget *widget, GdkEventButton *event, gpointer user_data);
void DocViewInitialize(void);
void DocViewRelease(void);



#endif
