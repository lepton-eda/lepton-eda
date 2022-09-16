/* Lepton EDA library
 * Copyright (C) 2022 Lepton EDA Contributors
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
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

/*! \file sch2pcb.h
 *
 *  \brief Structures and functions for lepton-sch2pcb.
 */

G_BEGIN_DECLS

gint
sch2pcb_main (gint argc,
              gchar **argv);
char*
sch2pcb_get_default_m4_pcbdir ();

void
sch2pcb_set_default_m4_pcbdir (const gchar *dir);

char*
sch2pcb_get_m4_pcbdir ();

void
sch2pcb_set_m4_pcbdir (const gchar *dir);

G_END_DECLS
