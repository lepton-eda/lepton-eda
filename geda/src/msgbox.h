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

#ifndef __MSGBOX_H_INCLUDED
#define __MSGBOX_H_INCLUDED



/* 
	MsgBox predefined types 
*/

/* single buttons */
#define MSGBOX_OK            0x00000001
#define MSGBOX_CANCEL        0x00000002
#define MSGBOX_YES           0x00000004
#define MSGBOX_NO            0x00000008
#define MSGBOX_ALWAYSOK      0x00000010
#define MSGBOX_ALWAYSCANCEL  0x00000020
#define MSGBOX_ALWAYSYES     0x00000040
#define MSGBOX_ALWAYSNO      0x00000080

/* single default buttons */
#define MSGBOX_OKD           0x80000001
#define MSGBOX_CANCELD       0x81000002
#define MSGBOX_YESD          0x82000004
#define MSGBOX_NOD           0x83000008
#define MSGBOX_ALWAYSOKD     0x84000010
#define MSGBOX_ALWAYSCANCELD 0x85000020
#define MSGBOX_ALWAYSYESD    0x86000040
#define MSGBOX_ALWAYSNOD     0x87000080

/* msg box status */
#define MSGBOX_MESSAGE       0x00010000
#define MSGBOX_WARNING       0x00020000
#define MSGBOX_ERROR         0x00040000
#define MSGBOX_FATAL         0x00080000

/* groups of buttons (for compatibility) */
#define MSGBOX_OKCANCEL      (MSGBOX_OK | MSGBOX_CANCEL)
#define MSGBOX_YESNOCANCEL   (MSGBOX_YES | MSGBOX_NO | MSGBOX_CANCEL)
#define MSGBOX_ALWAYSYESNO   (MSGBOX_YES | MSGBOX_ALWAYSYES | MSGBOX_NO | MSGBOX_ALWAYSNO)



/* public functions */
int MsgBox(char *szLabel, DWORD dwButtons);



#endif
