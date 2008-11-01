/* gEDA - GPL Electronic Design Automation
 * gsymcheck - gEDA Symbol Check
 * Copyright (C) 1998-2008 Ales Hvezda
 * Copyright (C) 1998-2008 gEDA Contributors (see ChangeLog for details)
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
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#include <config.h>

#include <stdio.h>

#include <libgeda/libgeda.h>

void s_log_update (const gchar *log_domain, GLogLevelFlags log_level,
                   const gchar *buf)
{
  if (buf == NULL)
    return;

  switch (logging_dest) {
    case STDOUT_TTY:
      fputs (buf, stdout);
      break;

    default:
      break;
  }
}
