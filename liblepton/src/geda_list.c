/* Lepton EDA library
 * Copyright (C) 1998-2000 Ales Hvezda
 * Copyright (C) 2007-2010 Peter Clifton
 * Copyright (C) 2011-2013 gEDA Contributors
 * Copyright (C) 2017-2019 Lepton EDA Contributors
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

/*! \file geda_list.c
 *  \brief list derived from GList with GObject properties
 *
 *  This GedaList with the GObject properties can use the signaling
 *  mechanisms of GObject now. 
 */

#include <config.h>

#include <glib-object.h>

#include "geda_list.h"


enum {
  CHANGED,
  LAST_SIGNAL
};

static guint geda_list_signals[ LAST_SIGNAL ] = { 0 };

G_DEFINE_TYPE (GedaList, geda_list, G_TYPE_OBJECT)


/*! \brief GType instance initialiser for GedaList
 *
 *  \par Function Description
 *  GType instance initialiser for GedaList.
 *
 *  \param [in]  list       The GedaList we are initialising.
 */
static void
geda_list_init (GedaList *list)
{
}


/*! \brief GObject finalise handler
 *
 *  \par Function Description
 *  Just before the GedaList GObject is finalized, free our
 *  allocated data, and then chain up to the parent's finalize handler.
 *
 *  \param [in] object  The GObject being finalized.
 */
static void geda_list_finalize( GObject *object )
{
  GedaList *list = GEDA_LIST( object );
  g_list_free( list->glist );

  G_OBJECT_CLASS( geda_list_parent_class )->finalize( object );
}


/*! \brief GType class initialiser for GedaList
 *
 *  \par Function Description
 *  GType class initialiser for GedaList. We override our parents
 *  virtual class methods as needed and register our GObject signals.
 *
 *  \param [in]  klass       The GedaList we are initialising
 */
static void
geda_list_class_init (GedaListClass *klass)
{
  GObjectClass *gobject_class = G_OBJECT_CLASS( klass );

  gobject_class->finalize = geda_list_finalize;

  geda_list_signals[ CHANGED ] =
    g_signal_new ("changed",
                  G_OBJECT_CLASS_TYPE( gobject_class ),
                  (GSignalFlags) 0     /*signal_flags */,
                  0     /*class_offset */,
                  NULL, /* accumulator */
                  NULL, /* accu_data */
                  g_cclosure_marshal_VOID__VOID,
                  G_TYPE_NONE,
                  0     /* n_params */
                 );
}


/*! \brief Returns a pointer to a new GedaList object.
 *
 *  \par Function Description
 *  Returns a pointer to a new GedaList object.
 *
 *  \return pointer to the new GedaList object.
 */
GedaList *geda_list_new( void ) {
  return GEDA_LIST (g_object_new (GEDA_TYPE_LIST, NULL));
}


/*! \brief Adds the given item to the GedaList
 *
 *  \par Function Description
 *  Adds the given item to the GedaList
 *
 *  \param [in] list Pointer to the GedaList
 *  \param [in] item item to add to the GedaList.
 */
void geda_list_add( GedaList *list, gpointer item )
{
  list->glist = g_list_append(list->glist, item );
  g_signal_emit( list, geda_list_signals[ CHANGED ], 0 );
}


/*! \brief Adds the given glist of items to the GedaList
 *
 *  \par Function Description
 *  Adds the given glist of items to the GedaList
 *  A copy is made, so the original GList is not modified.
 *
 *  \param [in] list Pointer to the GedaList
 *  \param [in] items GList of items to add to the GedaList.
 */
void geda_list_add_glist( GedaList *list, GList *items )
{
  GList *glist_copy = g_list_copy( items );
  list->glist = g_list_concat(list->glist, glist_copy );
  g_signal_emit( list, geda_list_signals[ CHANGED ], 0 );
}


/*! \brief Removes the given item from the GedaList
 *
 *  \par Function Description
 *  Removes the given item from the GedaList.
 *  It's ok to call this function with an item which
 *  is not necessarily in the list.
 *
 *  \param [in] list Pointer to the GedaList
 *  \param [in] item to remove from the GedaList.
 */
void geda_list_remove( GedaList *list, gpointer item )
{
  if (g_list_find(list->glist, item) == NULL)
    return;

  list->glist = g_list_remove(list->glist, item);
  g_signal_emit( list, geda_list_signals[ CHANGED ], 0 );
}


/*! \brief Removes all the items in the given GedaList.
 *
 *  \par Function Description
 *  Removes all items in the given GedaList.
 *
 *  \param [in] list Pointer to the GedaList
 */
void geda_list_remove_all( GedaList *list )
{
  g_list_free(list->glist);
  list->glist = NULL;
  g_signal_emit( list, geda_list_signals[ CHANGED ], 0 );
}


/*! \brief Moves the list data \a item to a new position \a newpos.
 */
void geda_list_move_item( GedaList* list, gpointer item, gint newpos )
{
  GList* gl = list->glist;
  GList* node = g_list_find (gl, item);

  if (node != NULL)
  {
    gl = g_list_remove_link (gl, node);
    gl = g_list_insert (gl, item, newpos);
    g_list_free (node);
    list->glist = gl;

    g_signal_emit( list, geda_list_signals[ CHANGED ], 0 );
  }
}

