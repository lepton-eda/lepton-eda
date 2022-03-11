/* Lepton EDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2014 gEDA Contributors
 * Copyright (C) 2017-2022 Lepton EDA Contributors
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */

#ifndef H_GSCHEM_GLOBALS_H
#define H_GSCHEM_GLOBALS_H


/* window list */
extern GList *global_window_list;

extern int do_logging;


/* command line options */
extern int quiet_mode;
extern int verbose_mode;

/* Global buffers */
#define MAX_BUFFERS      (5)
#define CLIPBOARD_BUFFER (0)

#include "gettext.h"
#ifdef ENABLE_NLS
# ifdef gettext_noop
#  define N_(String) gettext_noop (String)
# else
#  define N_(String) (String)
# endif
#else
# define N_(String) (String)
#endif

/*
 * __attribute__((unused)) is a gcc extension so define
 * a portable macro, ATTRIBUTE_UNUSED, to use instead
 */
#ifndef GCC_VERSION
#define GCC_VERSION (__GNUC__ * 1000 + __GNUC_MINOR__)
#endif /* GCC_VERSION */

#if GCC_VERSION > 2007
#define ATTRIBUTE_UNUSED __attribute__((unused))
#else
#define ATTRIBUTE_UNUSED
#endif

/*EK* used by prototype.h */
#include "../include/x_states.h"

#endif
