/* Lepton EDA library
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2016 gEDA Contributors
 * Copyright (C) 2017-2022 Lepton EDA Contributors
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
/*! \file object_list.h
 */

G_BEGIN_DECLS

void
lepton_object_list_delete (GList *list);

void
lepton_object_list_mirror (const GList *objects,
                           int x,
                           int y);
void
lepton_object_list_print (GList *objects);

void
lepton_object_list_rotate (const GList *objects,
                           int x,
                           int y,
                           int angle);
void
lepton_object_list_set_color (const GList *objects,
                              int color);
gchar*
lepton_object_list_to_buffer (const GList *objects);

void
lepton_object_list_translate (const GList *objects,
                              int dx,
                              int dy);
GList*
o_glist_copy_all (const GList *src_list,
                  GList *dest_list);

G_END_DECLS
