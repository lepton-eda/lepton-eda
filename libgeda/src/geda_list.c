/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
 * Copyright (C) 1998-2000 Ales Hvezda
 * Copyright (C) 2007-2010 Peter Clifton
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

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif


enum {
  CHANGED,
  LAST_SIGNAL
};

static guint geda_list_signals[ LAST_SIGNAL ] = { 0 };
static GObjectClass *geda_list_parent_class = NULL;


/*! \brief GType instance initialiser for GedaList
 *
 *  \par Function Description
 *  GType instance initialiser for GedaList.
 *
 *  \param [in]  instance       The GedaList we are initialising.
 *  \param [in]  g_class        The class of the type the instance is created for.
 */
static void geda_list_instance_init( GTypeInstance *instance, gpointer g_class )
{
  GedaList *list = (GedaList *)instance;

  /* Strictly un-necessary, as the memory is zero'd after allocation */
  list->glist = NULL;
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
 *  \param [in]  g_class       The GedaList we are initialising
 *  \param [in]  g_class_data  (unused)
 */
static void geda_list_class_init( gpointer g_class, gpointer g_class_data )
{
  GedaListClass *klass = GEDA_LIST_CLASS( g_class );
  GObjectClass *gobject_class = G_OBJECT_CLASS( klass );
  geda_list_parent_class = g_type_class_peek_parent( klass );

  gobject_class->finalize = geda_list_finalize;

  geda_list_signals[ CHANGED ] =
    g_signal_new ("changed",
                  G_OBJECT_CLASS_TYPE( gobject_class ),
                  0     /*signal_flags */,
                  0     /*class_offset */,
                  NULL, /* accumulator */
                  NULL, /* accu_data */
                  g_cclosure_marshal_VOID__VOID,
                  G_TYPE_NONE,
                  0     /* n_params */
                 );
}


/*! \brief Function to retrieve GedaList's GType identifier.
 *
 *  \par Function Description
 *  Function to retrieve GedaList's GType identifier.
 *  Upon first call, this registers the GedaList in the GType system.
 *  Subsequently it returns the saved value from its first execution.
 *
 *  \return the GType identifier associated with GedaList.
 */
GType geda_list_get_type(void)
{
  static GType type = 0;
  if (type == 0) {
    static const GTypeInfo info = {
      sizeof (GedaListClass),
      NULL,                         /* base_init */
      NULL,                         /* base_finalize */
      geda_list_class_init,         /* class_init */
      NULL,                         /* class_finalize */
      NULL,                         /* class_data */
      sizeof (GedaList),
      0,                            /* n_preallocs */
      geda_list_instance_init       /* instance_init */
    };
    type = g_type_register_static (G_TYPE_OBJECT, "GedaList", &info, 0);
  }
  return type;
}


/*! \brief Returns a pointer to a new GedaList object.
 *
 *  \par Function Description
 *  Returns a pointer to a new GedaList object.
 *
 *  \return pointer to the new GedaList object.
 */
GedaList *geda_list_new( void ) {
  return g_object_new( GEDA_TYPE_LIST, NULL );
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

