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
              gchar **argv,
              const char *m4_dir);
void
sch2pcb_get_args (gint argc,
                  gchar **argv);
void
sch2pcb_add_default_m4_files (void);

char*
sch2pcb_get_default_m4_pcbdir ();

void
sch2pcb_add_schematic (gchar *sch);

void
sch2pcb_set_default_m4_pcbdir (const gchar *dir);

GList*
sch2pcb_get_element_directory_list ();

void
sch2pcb_set_element_directory_list (GList *list);

char*
sch2pcb_get_m4_pcbdir ();

void
sch2pcb_set_m4_pcbdir (const gchar *dir);

void
sch2pcb_load_extra_project_files (void);

GList*
sch2pcb_get_schematics ();

void
sch2pcb_usage ();

void
sch2pcb_load_extra_project_files (void);

void
sch2pcb_load_project (const gchar * path);

gint
sch2pcb_parse_config (gchar *config,
                      gchar *arg);
gint
sch2pcb_get_verbose_mode ();

void
sch2pcb_version ();

G_END_DECLS
