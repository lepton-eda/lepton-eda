/* Lepton EDA library
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1999-2016 gEDA Contributors
 * Copyright (C) 2017-2020 Lepton EDA Contributors
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 */

/*! \file defines.h
 *  \brief global libgeda definitions
 */

#ifndef _DEFINES_H_INCL
#define _DEFINES_H_INCL

/* Current schematic/symbol file format */
#define FILEFORMAT_VERSION     2

/* release version which had file format changes */
/* New file format changes after 20030921 use the above version */
/* and not these #defines anymore. */
#define VERSION_20000220 20000220
#define VERSION_20000704 20000704
#define VERSION_20020825 20020825
#define VERSION_20030921 20030921
/* 20030921 wasn't a real version, just a MinGW test version, but it is */
/* out there */

/* Set this string to something interesting to create a custom */
/* version of gEDA/gaf.  This string is prepended to all messages that */
/* output the program's version output.  You would set this if you are */
/* creating a specific custom version of gEDA/gaf.  */
/* For example, if you set this string to "FIX-", the resulting output is:  */
/* FIX-1.0.0.20060906. */
#define PREPEND_VERSION_STRING ""

/* for show_name_value in st_objects */
#define SHOW_NAME_VALUE         0
#define SHOW_VALUE              1
#define SHOW_NAME               2
#define LEAVE_NAME_VALUE_ALONE  -1

/* for visibility in st_objects */
#define INVISIBLE               0
#define VISIBLE                 1
#define LEAVE_VISIBILITY_ALONE  -1

/* various thicknesses (in mils) */
#define LINE_WIDTH       10
#define NET_WIDTH        10
#define PIN_WIDTH_NET    (NET_WIDTH)
#define BUS_WIDTH        30
#define PIN_WIDTH_BUS    (BUS_WIDTH)

/* various visual cue sizes (in mils) */
#define CUE_BOX_SIZE             30
#define JUNCTION_CUE_SIZE_NET    (NET_WIDTH + 40)
#define JUNCTION_CUE_SIZE_BUS    (BUS_WIDTH + 40)

/* For text location on component not found graphics */
#define NOT_FOUND_TEXT_X	100
#define NOT_FOUND_TEXT_Y	100

/* for s_clib_getfilename() */
#define OPEN_DIR	0
#define READ_DIR	1
#define CLOSE_DIR	2
#define SET_COUNT	3

/* for text alignment */
/*   2 -- 5 -- 8  */
/*   |    |    |  */
/*   1 -- 4 -- 7  */
/*   |    |    |  */
/*   0 -- 3 -- 6  */
#define LOWER_LEFT	0
#define MIDDLE_LEFT	1
#define UPPER_LEFT	2
#define LOWER_MIDDLE	3
#define MIDDLE_MIDDLE	4
#define UPPER_MIDDLE	5
#define LOWER_RIGHT	6
#define MIDDLE_RIGHT	7
#define UPPER_RIGHT	8

/* one character string used to calculate tab's width */
/* Warning: it MUST be a string. */
#define TAB_CHAR_MODEL "b"

/* The conn modes for type */
#define CONN_NULL               0
#define CONN_ENDPOINT		1
#define CONN_MIDPOINT		2

/* used for undo_savestate flag */
#define UNDO_ALL		0
#define UNDO_VIEWPORT_ONLY	1

/* list copying flags */
#define NORMAL_FLAG		0
#define SELECTION_FLAG		1

/* hierarchy traversing flags */
#define HIERARCHY_NODUPS (1<<0)
#define HIERARCHY_POSTORDER (1<<1)
#define HIERARCHY_INNERLOOP (1<<7)

#define MILS_PER_INCH		1000

/* for print dialog box */
#define EXTENTS			0
#define WINDOW			1
#define EXTENTS_NOMARGINS	2

/* for print dialog box */
#define LANDSCAPE		0
#define PORTRAIT 		1
#define AUTOLAYOUT 		2

/* for pin_type */
#define PIN_TYPE_NET		0
#define PIN_TYPE_BUS		1

/* gnetlist: netlist_mode */
#define gEDA			0
#define SPICE			1
#define TANGO			2

/* gschcheck: Error types */
#define NO_ERR                  0
#define FLOAT_NET               1
#define FLOAT_PIN               2
#define DUP_NET_NAME            4

/* Max level of symlinks */
#define MAX_LINK_LEVEL 256

#if defined(__MINGW32__) && !defined(M_PI)
#define M_PI  3.14159265358979323846
#endif

/* Backup filename creation string */
#define AUTOSAVE_BACKUP_FILENAME_STRING "#%s#"

/* These permission bits are absent on MinGW */
#ifndef S_IWGRP
# define S_IWGRP 0
#endif
#ifndef S_IWOTH
# define S_IWOTH 0
#endif
#ifndef S_IXGRP
# define S_IXGRP 0
#endif
#ifndef S_IXOTH
# define S_IXOTH 0
#endif
#ifndef S_IRWXG
# define S_IRWXG 0
#endif

#endif /* !_DEFINES_H_INCL */
