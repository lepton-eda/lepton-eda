/* Lepton EDA library
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 2007-2010 Peter Clifton
 * Copyright (C) 2011-2015 gEDA Contributors
 * Copyright (C) 2017-2022 Lepton EDA Contributors
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 */
/*! \file list.h
 */

#ifndef __LIST_H__
#define __LIST_H__

G_BEGIN_DECLS

#define LEPTON_TYPE_LIST            (lepton_list_get_type())
#define LEPTON_LIST(obj)            (G_TYPE_CHECK_INSTANCE_CAST((obj), LEPTON_TYPE_LIST, LeptonList))
#define LEPTON_LIST_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST((klass),  LEPTON_TYPE_LIST, LeptonListClass))
#define LEPTON_IS_LIST(obj)         (G_TYPE_CHECK_INSTANCE_TYPE((obj), LEPTON_TYPE_LIST))
#define LEPTON_IS_LIST_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE((klass),  LEPTON_TYPE_LIST))
#define LEPTON_LIST_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS((obj),  LEPTON_TYPE_LIST, LeptonListClass))


typedef struct _LeptonList      LeptonList;
typedef struct _LeptonListClass LeptonListClass;

struct _LeptonList {
  GObject parent;
  GList *glist;
};

struct _LeptonListClass {
  GObjectClass parent;
};

GType lepton_list_get_type (void);

/* It would be nice to add const qualifiers to some of these, but GLib
 * is buggy in this respect, and doesn't have const where necessary. */
LeptonList *lepton_list_new( void );
void lepton_list_add( LeptonList *list, gpointer item );
void lepton_list_add_glist( LeptonList *list, GList *items );
void lepton_list_remove( LeptonList *list, gpointer item );
/*void lepton_list_remove_glist( LeptonList *list, GList *items ); */ /* Undemanded as yet */
void lepton_list_remove_all( LeptonList *list );
void lepton_list_move_item( LeptonList* list, gpointer item, gint newpos );

GList*
lepton_list_get_glist (LeptonList* list);

G_END_DECLS

#endif /* __LIST_H__ */
