/* gEDA - GNU Electronic Design Automation
 * gschem - GNU Schematic Capture
 * Copyright (C) 1998 Ales V. Hvezda
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

#include <libgeda/libgeda.h>

#include "../include/papersizes.h"
#include "../include/prototype.h"

int default_net_naming_priority = NET_ATTRIBUTE;

void
i_vars_set(TOPLEVEL *w_current)
{
	w_current->net_naming_priority = default_net_naming_priority;
}

void
i_vars_setnames(TOPLEVEL *w_current)
{

}
