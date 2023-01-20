/* Lepton EDA Schematic Capture
 * Copyright (C) 2023 Lepton EDA Contributors
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

/*!
 * \file gtk_helper.c
 *
 * \brief GTK helper functions.
 *
 */

#include <config.h>

#include "gschem.h"

/*! \brief Transform a GTK response id value to string.
 * \par Function Description
 * Given a GTK response type id \a response, returns the string
 * corresponding to it.  This is mainly intended to be used for
 * value conversion in Scheme FFI functions.
 *
 * \param [in] response The response id.
 * \return The string corresponding to the id.
 */
const char*
gtk_response_to_string (int response)
{
  const char *result = "unknown";

  switch (response)
  {
  case GTK_RESPONSE_NONE: result = "none"; break;
  case GTK_RESPONSE_REJECT: result = "reject"; break;
  case GTK_RESPONSE_ACCEPT: result = "accept"; break;
  case GTK_RESPONSE_DELETE_EVENT: result = "delete-event"; break;
  case GTK_RESPONSE_OK: result = "ok"; break;
  case GTK_RESPONSE_CANCEL: result = "cancel"; break;
  case GTK_RESPONSE_CLOSE: result = "close"; break;
  case GTK_RESPONSE_YES: result = "yes"; break;
  case GTK_RESPONSE_NO: result = "no"; break;
  case GTK_RESPONSE_APPLY: result = "apply"; break;
  case GTK_RESPONSE_HELP: result = "help"; break;
  default: break;
  }

  return result;
}
