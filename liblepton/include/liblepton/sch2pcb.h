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

void
sch2pcb_get_args (gint argc,
                  gchar **argv);
void
sch2pcb_add_default_m4_files (void);

gint
sch2pcb_add_elements (gchar *pcb_file);

char*
sch2pcb_get_default_m4_pcbdir ();

void
sch2pcb_add_schematic (gchar *sch);

char*
sch2pcb_get_backend_mkfile_cmd ();

void
sch2pcb_set_backend_mkfile_cmd (char *val);

char*
sch2pcb_get_backend_mkfile_net ();

void
sch2pcb_set_backend_mkfile_net (char *val);

char*
sch2pcb_get_backend_mkfile_pcb ();

void
sch2pcb_set_backend_mkfile_pcb (char *val);

void
sch2pcb_set_default_m4_pcbdir (const gchar *dir);

GList*
sch2pcb_get_element_directory_list ();

void
sch2pcb_set_element_directory_list (GList *list);

void
sch2pcb_element_directory_list_append (char *dir);

void
sch2pcb_element_directory_list_prepend (char *dir);

char*
sch2pcb_get_empty_footprint_name ();

void
sch2pcb_set_empty_footprint_name (char *val);

gboolean
sch2pcb_get_fix_elements ();

void
sch2pcb_set_fix_elements (gboolean val);

char*
sch2pcb_get_m4_pcbdir ();

void
sch2pcb_set_m4_pcbdir (const gchar *dir);

void
sch2pcb_load_extra_project_files (void);

void
sch2pcb_make_pcb_element_list (gchar *pcb_file);

GList*
sch2pcb_get_pcb_element_list ();

gboolean
sch2pcb_get_quiet_mode ();

void
sch2pcb_set_quiet_mode (gboolean mode);

char*
sch2pcb_get_sch_basename ();

void
sch2pcb_set_sch_basename (char *arg);

GList*
sch2pcb_get_schematics ();

void
sch2pcb_usage ();

void
sch2pcb_load_extra_project_files (void);

void
sch2pcb_load_project (const gchar * path);

int
sch2pcb_get_n_added_ef ();

void
sch2pcb_set_n_added_ef (int val);

int
sch2pcb_get_n_added_m4 ();

void
sch2pcb_set_n_added_m4 (int val);

int
sch2pcb_get_n_changed_value ();

void
sch2pcb_set_n_changed_value (int val);

int
sch2pcb_get_n_deleted ();

void
sch2pcb_set_n_deleted (int val);

int
sch2pcb_get_n_empty ();

void
sch2pcb_set_n_empty (int val);

int
sch2pcb_get_n_fixed ();

void
sch2pcb_set_n_fixed (int val);

int
sch2pcb_get_n_none ();

void
sch2pcb_set_n_none (int val);

int
sch2pcb_get_n_not_found ();

void
sch2pcb_set_n_not_found (int val);

int
sch2pcb_get_n_PKG_removed_new ();

void
sch2pcb_set_n_PKG_removed_new (int val);

int
sch2pcb_get_n_PKG_removed_old ();

void
sch2pcb_set_n_PKG_removed_old (int val);

int
sch2pcb_get_n_preserved ();

void
sch2pcb_set_n_preserved (int val);

int
sch2pcb_get_n_unknown ();

void
sch2pcb_set_n_unknown (int val);

gboolean
sch2pcb_get_need_PKG_purge ();

void
sch2pcb_set_need_PKG_purge (gboolean val);

gint
sch2pcb_parse_config (gchar *config,
                      gchar *arg);
void
sch2pcb_prune_elements (gchar *pcb_file,
                        gchar *bak);
gboolean
sch2pcb_run_netlister (gchar *pins_file,
                       gchar *net_file,
                       gchar *pcb_file,
                       gchar *basename,
                       GList *largs);
gint
sch2pcb_get_verbose_mode ();

void
sch2pcb_update_element_descriptions (gchar *pcb_file,
                                     gchar *bak);
void
sch2pcb_version ();

G_END_DECLS
