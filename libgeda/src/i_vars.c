/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's Library
 * Copyright (C) 1998-2000 Ales V. Hvezda
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
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111 USA
 */
#include <config.h>
#include <stdio.h>

#include <gtk/gtk.h>
#include <libguile.h>

#include "defines.h"
#include "struct.h"
#include "globals.h"
#include "o_types.h"
#include "colors.h"
#include "papersizes.h"
#include "i_vars.h"

#include "../include/prototype.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

/*! \def INIT_STR(w, name, str) */
#define INIT_STR(w, name, str) {                                        \
        if ((w)->name) {                                                \
                g_free((w)->name);                                      \
        }                                                               \
        (w)->name = g_strdup(((default_ ## name) != NULL) ?             \
                             (default_ ## name) : (str));               \
}

/* \note 
 * Kazu Hirata <kazu@seul.org> on July 16, 1999 - Added these absolute
 * defaults used when default_... is NULL.
 */
#define DEFAULT_SERIES_NAME      "untitled"
#define DEFAULT_UNTITLED_NAME    "untitled"
#define DEFAULT_SCHEME_DIRECTORY "./"
#define DEFAULT_FONT_DIRECTORY   "../lib/sym/font"
#define DEFAULT_BITMAP_DIRECTORY "../lib/bitmaps"
#define DEFAULT_BUS_RIPPER_SYMNAME "busripper-1.sym"
#define DEFAULT_POSTSCRIPT_PROLOG  "prolog.ps"
#define DEFAULT_ALWAYS_PROMOTE_ATTRIBUTES ""
#define DEFAULT_PRINT_COMMAND "lpr"

int   default_init_right = WIDTH_C;
int   default_init_bottom = HEIGHT_C;
char *default_series_name = NULL;
char *default_untitled_name = NULL;
char *default_font_directory = NULL;
char *default_scheme_directory = NULL;
char *default_bitmap_directory = NULL;
char *default_bus_ripper_symname = NULL;
char *default_postscript_prolog = NULL;
char *default_always_promote_attributes = NULL;
char *default_print_command = NULL;

/*! \brief Initialize variables in TOPLEVEL object
 *  \par Function Description
 *  This function will initialize variables to default values.
 *
 *  \param [out] w_current  The TOPLEVEL object to be updated.
 *
 */
void i_vars_libgeda_set(TOPLEVEL *w_current)
{
  w_current->init_right   = default_init_right;
  w_current->init_bottom  = default_init_bottom;

  /* you cannot free the default* strings here since new windows */
  /* need them */
  INIT_STR(w_current, series_name     , DEFAULT_SERIES_NAME     );
  INIT_STR(w_current, untitled_name   , DEFAULT_UNTITLED_NAME   );
  INIT_STR(w_current, font_directory  , DEFAULT_FONT_DIRECTORY  );
  INIT_STR(w_current, scheme_directory, DEFAULT_SCHEME_DIRECTORY);
  INIT_STR(w_current, bitmap_directory, DEFAULT_BITMAP_DIRECTORY);
  INIT_STR(w_current, bus_ripper_symname, DEFAULT_BUS_RIPPER_SYMNAME);
  INIT_STR(w_current, postscript_prolog,  DEFAULT_POSTSCRIPT_PROLOG);
  INIT_STR(w_current, always_promote_attributes, DEFAULT_ALWAYS_PROMOTE_ATTRIBUTES);
  INIT_STR(w_current, print_command, DEFAULT_PRINT_COMMAND);

}

/*! \brief Set all names in TOPLEVEL object to default.
 *  \par Function Description
 *  This function will set all of the names in the TOPLEVEL w_current variable
 *  to their default.
 *
 *  \param [out] w_current  The TOPLEVEL object to set to defaults.
 *
 */
void i_vars_setnames(TOPLEVEL *w_current)
{
  w_current->series_name        = g_strdup (DEFAULT_SERIES_NAME     );
  w_current->untitled_name      = g_strdup (DEFAULT_UNTITLED_NAME   );
  w_current->font_directory     = g_strdup (DEFAULT_FONT_DIRECTORY  );
  w_current->scheme_directory   = g_strdup (DEFAULT_SCHEME_DIRECTORY);
  w_current->bitmap_directory   = g_strdup (DEFAULT_BITMAP_DIRECTORY);
  w_current->bus_ripper_symname = g_strdup (DEFAULT_BUS_RIPPER_SYMNAME);
  w_current->always_promote_attributes = g_strdup (DEFAULT_ALWAYS_PROMOTE_ATTRIBUTES);
  w_current->print_command = g_strdup (DEFAULT_PRINT_COMMAND);
}

/*! \brief Free default names
 *  \par Function Description
 *  This function will free all of the default variables for libgeda.
 *
 */
void i_vars_freenames()
{
  g_free(default_series_name);
  g_free(default_untitled_name);
  g_free(default_font_directory);
  g_free(default_scheme_directory);
  g_free(default_bitmap_directory);
  g_free(default_bus_ripper_symname);
  g_free(default_postscript_prolog);
  g_free(default_always_promote_attributes);
  g_free(default_print_command);
}
