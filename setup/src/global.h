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

#ifndef __MAIN_H_INCLUDED
#define __MAIN_H_INCLUDED

#include <gtk/gtk.h>



/* global defines */
#define SETUP_EMAIL          "sp9rve@eter.ariadna.pl"
#define SETUP_TMPDIR         "tmp"
#define SETUP_LOGFILE        "/tmp/geda-setup.log"

/* results of executing of functions */
#define SUCCESS              0
#define FAILURE              1

/* TODO: remove it in the future */
#define TEXTLEN              256
extern int OkPressed;

/* notebook pages */
#define PAGE_START           0
#define PAGE_COMPS           1
#define PAGE_LICENSE         2
#define PAGE_DIRS            3
#define PAGE_SUMMARY         4
#define PAGE_STATUS          5



/* window handles */
extern GtkWidget *pWindowMain;



#endif
