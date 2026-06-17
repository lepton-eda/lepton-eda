/* Lepton EDA attribute editor
 * Copyright (C) 2003-2010 Stuart D. Brorson.
 * Copyright (C) 2003-2013 gEDA Contributors
 * Copyright (C) 2017-2024 Lepton EDA Contributors
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

/*! \file
 *  \brief Miscellaneous STRING_LIST functions
 */

#include <config.h>

#include <stdio.h>
#include <math.h>
#ifdef HAVE_STRING_H
#include <string.h>
#endif

/*------------------------------------------------------------------
 * Gattrib specific includes
 *------------------------------------------------------------------*/
#include <liblepton/liblepton.h>
#include "../include/struct.h"     /* typdef and struct declarations */
#include "../include/prototype.h"  /* function prototypes */
#include "../include/globals.h"
#include "../include/gettext.h"


/*------------------------------------------------------------------
 * The below fcns identical to those defined in
 * geda-gnetlist/src/s_misc.c
 *------------------------------------------------------------------*/
/*!
 * Running count of number of characters printed on current line.
 */
static int char_index = 0;

void
set_verbose_mode () {
  verbose_mode = TRUE;
}

/*! \brief Print message in verbose mode
 *
 * Print the supplied message in verbose mode. Line wrap if necessary.
 *
 * Identical to that defined in gnetlist/src/s_misc.c
 * \param string String to be printed
 */
void verbose_print (const char *string)
{
    if (verbose_mode) {
        printf("%s", string);
        char_index++;
        if ((char_index + 1) >= 78) {
            printf("\n");
            char_index = 0;
        }
    }
}

/*! \brief Print "DONE" message in verbose mode
 *
 * Prints the "DONE" message in verbose mode, wrapping before printing
 * if near the end of line.
 *
 * Identical to function defined in gnetlist/src/s_misc.c
 */
void verbose_done(void)
{
    if (verbose_mode) {
        if (char_index >= 70) {
            printf(_("\nDONE\n"));
        } else {
            printf(_(" DONE\n"));
        }

        char_index = 0;
    }
}
