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

#ifndef __COMPS_H_INCLUDED
#define __COMPS_H_INCLUDED

#include <gtk/gtk.h>
#include "config.h"
#include "package.h"



/* installation flags */
#define COMPONENTS_IGNORED   0
#define COMPONENTS_SELECTED  1
#define COMPONENTS_REQUIRED  2



#define MSG_INSTALL          1
#define MSG_NEXT             2
#define MSG_PREVIOUS         3
#define MSG_OK               4

#define MARK_SELECT          1
#define MARK_UNSELECT        2


#if 0
struct Message_s
{
	long lMsgType;
	void *pBuffer;
	int iSize;
};

#define COMPS_NUMBER 8
#endif


/* public functions */
int ComponentsInitialize(void);
int ComponentsRelease(void);
int ComponentsShow(struct CompsTable_s *pPkg);
int ComponentsHide(void);
int CompsShow(char *szParent);
int set_pixmaps(GtkCTree *Tree);
int ComponentsPrepare(struct CompsTable_s *pPkg);
void mark_components(struct CompsTable_s *pMaster, int iFlag);
int is_expanded(GtkCTree *pTree, GtkCTreeNode *pNode);



#endif
