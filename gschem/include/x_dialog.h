/* $Id$ */

/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 * Copyright (C) 2006 Dan McMahill
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */


#ifndef __X_DIALOG_H__
#define __X_DIALOG_H__

/* 
 * Flags for generic_filesel_dialog()
 */

#define FSB_MAY_EXIST		1
#define FSB_MUST_EXIST		2
#define FSB_SHOULD_NOT_EXIST	4

#define FSB_SAVE		256
#define FSB_LOAD		512

/*
 * define spacings for dialogs
 */

#define DIALOG_BORDER_SPACING 5
#define DIALOG_V_SPACING 8
#define DIALOG_H_SPACING 10
#define DIALOG_INDENTATION 10

#endif /* __X_DIALOG_H__ */
