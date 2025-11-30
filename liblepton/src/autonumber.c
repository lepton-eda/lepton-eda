/* Lepton EDA library
 * Copyright (C) 2025 Lepton EDA Contributors
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

/*! \file autonumber.c
 *  \brief Helpers for autonumbering.
 */

#include <config.h>

#include <ctype.h>

#include "liblepton_priv.h"


/*! \brief Drop wildcard string suffix
 *  \par Function Description
 *  Given that \a searchtext is a prefix for \a str, the function
 *  drops all the suffix characters of \a str consisting of only
 *  digits and question marks until its length is not less than
 *  the length of \a searchtext.
 *
 *  \param [in] str The string to drop suffix of.
 *  \param [in] searchtext The prefix string.
 *  \return The copy of the resulting string without suffix.
 *
 *  \note Caller must g_free returned character string.
 */
char*
lepton_autonumber_drop_string_suffix (const char *str,
                                      char *searchtext)
{
  size_t i;
  if (g_str_has_prefix (str, searchtext))
  {
    for (i = strlen (str) - 1;
         (i >= strlen (searchtext))
           && (str[i] == '?'
               || isdigit ((int) (str[i])));
         i--)
      ; /* void */

    return g_strndup (str, i + 1);
  }
  else
  {
    return NULL;
  }
}
