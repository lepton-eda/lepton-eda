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

#ifndef __FILE_H_INCLUDED
#define __FILE_H_INCLUDED

#include <sys/types.h>
#include "global.h"



/* public functions */
int FileCopy(char *szSource, char *szDest, mode_t Mode);
int FileLink(char *szSource, char *szDest);
int FileTest(char *szFileName);
int FileExec(char *szCommand);
int FileExecStdOut(char *pValue, int iMaxLength);
int FileExecStdErr(char *pValue, int iMaxLength);
int FileSearch(char *szSearchDir, char *szSearchName);
int FileWhich(char *szCommand);
void FileGetName(char *szFullName, char *szName);
void FileGetExt(char *szFullName, char *szExt);
void FileGetDir(char *szFullName, char *szDir);
void FileGetRel(char *szFullName, char *szName);



#endif
