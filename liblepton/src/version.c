/* Lepton EDA library
 * Copyright (C) 1998-2016 gEDA Contributors
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 */

/*!
 * \file version.c
 * \brief Functions for working with Lepton EDA version data.
 */

#include <config.h>
#include <version.h>

#include "liblepton_priv.h"

/*! \brief Return the prepend part of the version string
 *
 *  \return The string to prepend to the version output.
 */
const char*
lepton_version_prepend ()
{
  return PREPEND_VERSION_STRING;
}


/*! \brief Return the dotted part of the version string
 *
 *  \return The string containing the last released version of the
 *          package.
 */
const char*
lepton_version_dotted ()
{
  return PACKAGE_DOTTED_VERSION;
}


/*! \brief Return the date part of the version string
 *
 *  \return The string in the form "YYYYMMDD" containing the date
 *          of the last release of the package.
 */
const char*
lepton_version_date ()
{
  return PACKAGE_DATE_VERSION;
}


/*! \brief Return the git commit part of the version string
 *
 *  \return The string containing the commit hash of the package
 *          build.
 */
const char*
lepton_version_git_commit ()
{
  return PACKAGE_GIT_COMMIT;
}


/*! \brief Return the bug report part of the version string
 *
 *  \return The string with the URI of the bug tracker.
 */
const char*
lepton_version_bugreport ()
{
  return PACKAGE_BUGREPORT;
}


/*! \brief Return the URL part of the version string
 *
 *  \return The string with the URI of the package project site.
 */
const char*
lepton_version_url ()
{
  return PACKAGE_URL;
}


/*! \brief Return the copyright of the package
 *
 *  \return The translated copyright string of the package.
 */
const char*
lepton_version_copyright ()
{
  const char* msg =
    _("Copyright (C) 1998-2016 gEDA developers\n"
      "Copyright (C) 2017-2024 Lepton EDA developers\n"
      "This is free software, and you are welcome to redistribute it\n"
      "under certain conditions. For details, see the file `COPYING',\n"
      "which is included in the Lepton EDA distribution.\n"
      "There is NO WARRANTY, to the extent permitted by law.\n");
  return msg;
}
