/* Lepton EDA library
 * Copyright (C) 2022-2023 Lepton EDA Contributors
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

#include <glib.h>
#include <stdio.h>

typedef struct
{
  gchar *refdes;
  gchar *value;
  gchar *description;
  gchar *changed_description;
  gchar *changed_value;
  gchar *flags;
  gchar *tail;
  gchar *x;
  gchar *y;
  gchar *pkg_name_fix;

  gchar res_char;

  gboolean still_exists;
  gboolean new_format;
  gboolean hi_res_format;
  gboolean quoted_flags;
  gboolean omit_PKG;
}
PcbElement;

G_BEGIN_DECLS

/* PcbElement accessors */

gchar*
pcb_element_get_refdes (PcbElement *element);

void
pcb_element_set_refdes (PcbElement *element,
                        gchar *val);

gchar*
pcb_element_get_value (PcbElement *element);

void
pcb_element_set_value (PcbElement *element,
                       gchar *val);

gchar*
pcb_element_get_description (PcbElement *element);

void
pcb_element_set_description (PcbElement *element,
                             gchar *val);

gchar*
pcb_element_get_changed_description (PcbElement *element);

void
pcb_element_set_changed_description (PcbElement *element,
                                     gchar *val);

gchar*
pcb_element_get_changed_value (PcbElement *element);

void
pcb_element_set_changed_value (PcbElement *element,
                               gchar *val);

gchar*
pcb_element_get_flags (PcbElement *element);

void
pcb_element_set_flags (PcbElement *element,
                       gchar *val);

gchar*
pcb_element_get_tail (PcbElement *element);

void
pcb_element_set_tail (PcbElement *element,
                      gchar *val);

gchar*
pcb_element_get_x (PcbElement *element);

void
pcb_element_set_x (PcbElement *element,
                   gchar *val);

gchar*
pcb_element_get_y (PcbElement *element);

void
pcb_element_set_y (PcbElement *element,
                   gchar *val);

gchar*
pcb_element_get_pkg_name_fix (PcbElement *element);

void
pcb_element_set_pkg_name_fix (PcbElement *element,
                              gchar *val);

gchar
pcb_element_get_res_char (PcbElement *element);

void
pcb_element_set_res_char (PcbElement *element,
                          gchar val);

gboolean
pcb_element_get_still_exists (PcbElement *element);

void
pcb_element_set_still_exists (PcbElement *element,
                              gboolean val);

gboolean
pcb_element_get_new_format (PcbElement *element);

void
pcb_element_set_new_format (PcbElement *element,
                            gboolean val);

gboolean
pcb_element_get_hi_res_format (PcbElement *element);

void
pcb_element_set_hi_res_format (PcbElement *element,
                               gboolean val);

gboolean
pcb_element_get_quoted_flags (PcbElement *element);

void
pcb_element_set_quoted_flags (PcbElement *element,
                              gboolean val);

gboolean
pcb_element_get_omit_PKG (PcbElement *element);

void
pcb_element_set_omit_PKG (PcbElement *element,
                          gboolean val);

/* PcbElement functions */

void
pcb_element_free (PcbElement *el);

PcbElement*
pcb_element_exists (PcbElement *el_test,
                    gboolean record);
PcbElement*
pcb_element_line_parse (gchar *line);

PcbElement*
pcb_element_pkg_to_element (gchar *pkg_line);

/* lepton-sch2pcb's toplevel functions */

char*
sch2pcb_get_empty_footprint_name ();

void
sch2pcb_set_empty_footprint_name (char *val);

gchar*
sch2pcb_expand_dir (gchar *dir);

void
sch2pcb_extra_gnetlist_arg_list_append (char *arg);

gchar*
sch2pcb_find_element (gchar *dir_path,
                      gchar *element);
gboolean
sch2pcb_insert_element (FILE *f_out,
                        gchar *element_file,
                        gchar *footprint,
                        gchar *refdes,
                        gchar *value);

void
sch2pcb_increment_verbose_mode ();

void
sch2pcb_load_extra_project_files (void);

void
sch2pcb_make_pcb_element_list (gchar *pcb_file);

GList*
sch2pcb_get_pcb_element_list ();

GList*
sch2pcb_get_schematics ();

int
sch2pcb_get_n_empty ();

void
sch2pcb_set_n_empty (int val);

int
sch2pcb_get_n_none ();

void
sch2pcb_set_n_none (int val);

int
sch2pcb_get_n_unknown ();

void
sch2pcb_set_n_unknown (int val);

FILE*
sch2pcb_open_file_to_read (char *filename);

FILE*
sch2pcb_open_file_to_write (char *filename);

void
sch2pcb_close_file (FILE *file);

void
sch2pcb_buffer_to_file (char *buffer,
                        FILE *file);
GList*
sch2pcb_parse_schematics (char *str);

void
sch2pcb_pcb_element_list_append (PcbElement *element);

gint
sch2pcb_get_verbose_mode ();

G_END_DECLS
