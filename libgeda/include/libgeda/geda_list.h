/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 2007-2010 Peter Clifton
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
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */


#ifndef __GEDA_LIST_H__
#define __GEDA_LIST_H__

#define GEDA_TYPE_LIST            (geda_list_get_type())
#define GEDA_LIST(obj)            (G_TYPE_CHECK_INSTANCE_CAST((obj), GEDA_TYPE_LIST, GedaList))
#define GEDA_LIST_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST((klass),  GEDA_TYPE_LIST, GedaListClass))
#define GEDA_IS_LIST(obj)         (G_TYPE_CHECK_INSTANCE_TYPE((obj), GEDA_TYPE_LIST))
#define GEDA_IS_LIST_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE((klass),  GEDA_TYPE_LIST))
#define GEDA_LIST_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS((obj),  GEDA_TYPE_LIST, GedaListClass))


typedef struct _GedaList      GedaList;
typedef struct _GedaListClass GedaListClass;

struct _GedaList {
  GObject parent;
  GList *glist;
};

struct _GedaListClass {
  GObjectClass parent;
};

GType geda_list_get_type (void);

/* It would be nice to add const qualifiers to some of these, but GLib
 * is buggy in this respect, and doesn't have const where necessary. */
GedaList *geda_list_new( void );
void geda_list_add( GedaList *list, gpointer item );
void geda_list_add_glist( GedaList *list, GList *items );
void geda_list_remove( GedaList *list, gpointer item );
/*void geda_list_remove_glist( GedaList *list, GList *items ); */ /* Undemanded as yet */
void geda_list_remove_all( GedaList *list );

/*const GList *geda_list_get_glist( GedaList *list ); */
#define geda_list_get_glist(list) (list->glist)

#endif /* __GEDA_LIST_H__ */

