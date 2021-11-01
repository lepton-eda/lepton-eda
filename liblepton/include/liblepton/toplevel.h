/* Lepton EDA library
 * Copyright (C) 1998, 1999, 2000 Kazu Hirata / Ales Hvezda
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2016 gEDA Contributors
 * Copyright (C) 2017-2021 Lepton EDA Contributors
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
/*! \file toplevel.h
 */

G_BEGIN_DECLS

struct st_toplevel
{
  /* List of RC files which have been read in. */
  GList *RC_list;

  /* page system */
  LeptonPage *page_current;
  LeptonPageList *pages;

  /* backup variables */
  int auto_save_interval;
  gint auto_save_timeout;

  /* Callback functions for object change notification */
  GList *change_notify_funcs;

  GList *weak_refs; /* Weak references */
};

void
s_toplevel_add_weak_ptr (LeptonToplevel *toplevel,
                         void *weak_pointer_loc);
void
s_toplevel_delete (LeptonToplevel *toplevel);

LeptonToplevel*
s_toplevel_new (void);

void
s_toplevel_remove_weak_ptr (LeptonToplevel *toplevel,
                            void *weak_pointer_loc);
LeptonPage*
lepton_toplevel_get_page_current (LeptonToplevel *toplevel);

void
lepton_toplevel_set_page_current (LeptonToplevel *toplevel,
                                  LeptonPage *page);
LeptonPageList*
lepton_toplevel_get_pages (LeptonToplevel *toplevel);

void
lepton_toplevel_set_pages (LeptonToplevel *toplevel,
                           LeptonPageList *pages);
void
s_toplevel_weak_ref (LeptonToplevel *toplevel,
                     void (*notify_func)(void *, void *),
                     void *user_data);
void
s_toplevel_weak_unref (LeptonToplevel *toplevel,
                       void (*notify_func)(void *, void *),
                       void *user_data);
void
lepton_toplevel_goto_page (LeptonToplevel *toplevel,
                           LeptonPage *p_new);
LeptonPage*
lepton_toplevel_search_page (LeptonToplevel *toplevel,
                             const gchar *filename);
LeptonPage*
lepton_toplevel_search_page_by_basename (LeptonToplevel *toplevel,
                                         const gchar *filename);
G_END_DECLS
