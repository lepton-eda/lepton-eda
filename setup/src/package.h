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

#ifndef __PACKAGE_H_INCLUDED
#define __PACKAGE_H_INCLUDED

#include <gtk/gtk.h>
#include "global.h"



/* file types */
#define PACKAGE_FILE_UNKNOWN 0
#define PACKAGE_FILE_BINARY  1
#define PACKAGE_FILE_LIBRARY 2
#define PACKAGE_FILE_DATA    3
#define PACKAGE_FILE_LINK    4

/* file types tags */
#define PACKAGE_TAG_UNKNOWN  ""
#define PACKAGE_TAG_BINARY   "BIN"
#define PACKAGE_TAG_LIBRARY  "LIB"
#define PACKAGE_TAG_DATA     "DAT"
#define PACKAGE_TAG_LINK     "LNK"



/* TODO: remove it */
extern char szInstallDirectory[];
extern int iSoftwareInstalled;
extern char szWorkingDirectory[];



/* software properties */
struct Software_s
{
	char szName    [TEXTLEN];
	char szVersion [TEXTLEN];
	char szRelease [TEXTLEN];
	char szDesc    [TEXTLEN];
	char szLicense [TEXTLEN];
	char szDirname [TEXTLEN];
	char szPackage [TEXTLEN];
	char szPictName[TEXTLEN];

	struct CompsTable_s *Pkg;
};
extern struct Software_s Software;
	
/* package propeties */
struct CompsTable_s
{
	/* properties of a package */
	char *szVariables;
	char szName[TEXTLEN];
	char szCodeName[TEXTLEN];
	char szVersion[TEXTLEN];
	char szRelease[TEXTLEN];
	char szDesc[TEXTLEN];
	char szLicense[TEXTLEN];
	int bToBeDisplayed;
	int iToBeInstalled;
	char szRequiresList[TEXTLEN];
	char szTopLevel[TEXTLEN];
	char szFileName[TEXTLEN];
	char szPatch[TEXTLEN];
	char szDirName[TEXTLEN];
	char szBldTools[TEXTLEN];
	char szCommand[TEXTLEN];
	char szInstCmd[TEXTLEN];
	char szPreInst[TEXTLEN];
	char szPostInst[TEXTLEN];
	char szPreUnin[TEXTLEN];
	char szPostUnin[TEXTLEN];
	char szFtpFile[TEXTLEN];
	char szFtpPatch[TEXTLEN];

	int iFileList;

	/* variables for an internal use or to be removed */
	int bCanBeInstalled;
	/* TODO: check variable list */
	struct CompsTable_s *pNextComp;
	GtkCTreeNode *pNode;
	char *szNode[3];
	char szInstall[TEXTLEN];
	int bInstalled;
	GdkPixmap *Icon;
	struct CompsTable_s *pCompParent; //, *pCompSibling;
};
extern struct CompsTable_s *pCompsTable;
	


/* name and path of a configuration file */
#define PACKAGE_CFGFILE      "setup.cfg"



/* TODO: remove it, use PackageGetValue() */
#define NAME(Comp)           ( Comp->szName )
#define VERSION(Comp)        ( Comp->szVersion )
#define RELEASE(Comp)        ( Comp->szRelease )
#define DESC(Comp)           ( Comp->szDesc )
#define LICENSE(Comp)        ( Comp->szLicense )
#define FLAGDISP(Comp)       ( Comp->bToBeDisplayed )
#define FLAGINST(Comp)       ( Comp->bToBeInstalled )
#define REQLIST(Comp)        ( Comp->szRequiresList )
#define TOPLEVEL(Comp)       ( Comp->szPackages )
#define FILENAME(Comp)       ( Comp->szFileName )
#define PATCH(Comp)          ( Comp->szPatch )
#define DIRNAME(Comp)        ( Comp->szDirName )
#define BLDTOOLS(Comp)       ( Comp->szBldTools )
#define BLDCMD(Comp)         ( Comp->szCommand )
#define INSTCMD(Comp)        ( Comp->szInstCmd )
#define FILES(Comp)          ( Comp->szVariables + Comp->iFileList )
#define PREINST(Comp)        ( Comp->szPreInst )
#define POSTINST(Comp)       ( Comp->szPostInst )
#define PREUNIN(Comp)        ( Comp->szPreUnin )
#define POSTUNIN(Comp)       ( Comp->szPostUnin )




/* package properties */
#define PACKAGE_NAME         1
#define PACKAGE_VERSION      2
#define PACKAGE_RELEASE      3
#define PACKAGE_DESC         4
#define PACKAGE_LICENSE      5
#define PACKAGE_FLAGDISP     6
#define PACKAGE_FLAGINST     7
#define PACKAGE_REQLIST      8
#define PACKAGE_PACKAGES     9
#define PACKAGE_FILENAME     10
#define PACKAGE_PATCH        11
#define PACKAGE_DIRNAME      12
#define PACKAGE_BLDTOOLS     13
#define PACKAGE_BLDCMD       14
#define PACKAGE_FILES        15
#define PACKAGE_PREINST      16
#define PACKAGE_POSTINST     17
#define PACKAGE_PREUNIN      18
#define PACKAGE_POSTUNIN     19
#define PACKAGE_INSTCMD      20
#define PACKAGE_INSTALLED    21
#define PACKAGE_NEXT         22

/* Installation Flag values */
#define PACKAGE_NOTSET       0           /* value not set */
#define PACKAGE_IGNORED      1           /* package will not be installed */
#define PACKAGE_SELECTED     2           /* package selected by a user */
#define PACKAGE_REQUIRED     3           /* package not selected but required (dependency checking) */
#define PACKAGE_BLDTOOL      4           /* package required only to build other package */

/* Visibility Flag values */
#define PACKAGE_NOTSET       0
#define PACKAGE_INVISIBLE    1
#define PACKAGE_VISIBLE      2



/* 
	Interface for other modules
*/

int PackageInitialize(void);
int PackageRelease(void);
void *PackageValueGet(const int iName, const int iNumber, const char *szPackage);
int PackageValueSet(const int iName, const int iNumber, const char *szPackage, void *pValue);
char *PackageWhatIsMissing(const char *szTestedCodeName);
int PackageTestIfInstalled(struct CompsTable_s *pPkg);
int get_next_component(struct CompsTable_s *pComp, int iIndex, char *szName);
struct CompsTable_s *get_component_by_name(const char *szName);



#endif
