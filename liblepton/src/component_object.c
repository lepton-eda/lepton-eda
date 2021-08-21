/* Lepton EDA library
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2017 gEDA Contributors
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

/*! \file component_object.c
 *  \brief Functions for component objects
 *
 *  Component objects contain collections of primary objects.
 */

#include <config.h>

#include <stdio.h>
#include <math.h>
#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "liblepton_priv.h"


/*! \brief Get the basename of a component object.
 *  \par Function Description
 *  Returns the basename string of a component object.
 *
 *  \param [in] object The component object.
 *  \return The basename of the component object.
 */
gchar*
lepton_component_object_get_basename (const LeptonObject *object)
{
  g_return_val_if_fail (lepton_object_is_component (object), NULL);
  g_return_val_if_fail (object->component != NULL, NULL);

  return object->component_basename;
}


/*! \brief Set the basename of a component object.
 *  \par Function Description
 *  Sets the given basename for a component object.
 *
 *  \param [in] object The component object.
 *  \param [in] basename The new basename of the component object.
 */
void
lepton_component_object_set_basename (LeptonObject *object,
                                      const gchar *basename)
{
  g_return_if_fail (lepton_object_is_component (object));
  g_return_if_fail (object->component != NULL);

  g_free (object->component_basename);
  if (basename == NULL)
  {
    object->component_basename = NULL;
  }
  else
  {
    object->component_basename = g_strdup (basename);
  }
}


/*! \brief Get the X coordinate of the component base point.
 *  \par Function Description
 *  Returns the X coordinate of the base point of a component
 *  object.
 *
 *  \param [in] object The component object.
 *  \return The X coordinate of the component object.
 */
gint
lepton_component_object_get_x (const LeptonObject *object)
{
  g_return_val_if_fail (lepton_object_is_component (object), G_MAXINT);
  g_return_val_if_fail (object->component != NULL, G_MAXINT);

  return object->component->x;
}


/*! \brief Set the X coordinate of the component base point.
 *  \par Function Description
 *  Sets the X coordinate of the base point of a component object.
 *
 *  \param [in] object The component object.
 *  \param [in] x The new X coordinate of the component object.
 */
void
lepton_component_object_set_x (LeptonObject *object,
                               gint x)
{
  g_return_if_fail (lepton_object_is_component (object));
  g_return_if_fail (object->component != NULL);

  object->component->x = x;
}


/*! \brief Get the Y coordinate of the component base point.
 *  \par Function Description
 *  Returns the Y coordinate of the base point of a component
 *  object.
 *
 *  \param [in] object The component object.
 *  \return The Y coordinate of the component object.
 */
gint
lepton_component_object_get_y (const LeptonObject *object)
{
  g_return_val_if_fail (lepton_object_is_component (object), G_MAXINT);
  g_return_val_if_fail (object->component != NULL, G_MAXINT);

  return object->component->y;
}


/*! \brief Set the Y coordinate of the component base point.
 *  \par Function Description
 *  Sets the Y coordinate of the base point of a component object.
 *
 *  \param [in] object The component object.
 *  \param [in] y The new Y coordinate of the component object.
 */
void
lepton_component_object_set_y (LeptonObject *object,
                               gint y)
{
  g_return_if_fail (lepton_object_is_component (object));
  g_return_if_fail (object->component != NULL);

  object->component->y = y;
}


/*! \brief Get the component angle.
 *
 *  \param [in] object The component object.
 *  \return The component angle.
 */
gint
lepton_component_object_get_angle (const LeptonObject *object)
{
  g_return_val_if_fail (lepton_object_is_component (object), 0);
  g_return_val_if_fail (object->component != NULL, 0);

  return object->component->angle;
}


/*! \brief Set the component angle.
 *
 * The component angle must be orthogonal to an axis, i.e., the
 * component angle must be a multiple of 90 degrees.  In case of
 * an invalid component angle, the property remains unchanged.
 *
 * If the component angle is not normal [0 .. 360], then the angle
 * will be normalized.
 *
 * \param [in,out] object The component object
 * \param [in] angle The component angle in degrees.
 */
void
lepton_component_object_set_angle (LeptonObject *object,
                                   gint angle)
{
  g_return_if_fail (lepton_object_is_component (object));
  g_return_if_fail (object->component != NULL);
  g_return_if_fail (lepton_angle_is_ortho (angle));

  object->component->angle = lepton_angle_normalize (angle);
}


/*! \brief Test component object's 'mirror' flag.
 *
 *  \param [in] object The component object to test.
 *  \return The value of the 'mirror' flag.
 */
gboolean
lepton_component_object_get_mirror (const LeptonObject *object)
{
  g_return_val_if_fail (lepton_object_is_component (object), TRUE);
  g_return_val_if_fail (object->component != NULL, TRUE);

  return object->component->mirror;
}


/*! \brief Set component object's 'mirror' flag.
 *
 *  \param [in] object The component object to amend.
 *  \param [in] mirror The new value of the 'mirror' flag.
 */
void
lepton_component_object_set_mirror (LeptonObject *object,
                                    gboolean mirror)
{
  g_return_if_fail (lepton_object_is_component (object));
  g_return_if_fail (object->component != NULL);

  object->component->mirror = mirror;
}


/*! \brief Get the primitive objects of a component.
 *  \par Function Description
 *  This function returns the pointer to the primitive objects of
 *  a component object.
 *
 *  \param [in] object  The object to get the primitives.
 *  \return The pointer to GList of the primitives.
 */
GList*
lepton_component_object_get_contents (const LeptonObject *object)
{
  g_return_val_if_fail (lepton_object_is_component (object), NULL);
  g_return_val_if_fail (object->component != NULL, NULL);

  return object->component->prim_objs;
}


/*! \brief Set the GList of primitive objects of a component.
 *  \par Function Description
 *  Sets the pointer to the primitive objects of a component
 *  object to a given value.  The function does not change the
 *  previously set GList in any way.
 *
 *  \param [in] object  The component object to set primitives of.
 *  \param [in] primitives The pointer to GList of the new primitives.
 */
void
lepton_component_object_set_contents (LeptonObject *object,
                                      GList *primitives)
{
  g_return_if_fail (lepton_object_is_component (object));
  g_return_if_fail (object->component != NULL);

  object->component->prim_objs = primitives;
}


/*! \brief Return the array of attributes to always promote. */
static GPtrArray*
always_promote_attributes ()
{
  /* List of attributes to always promote */
  static GPtrArray *attributes = NULL;

  static gboolean initialised = FALSE;

  if (initialised)
    return attributes;

  if (attributes)
  {
    g_ptr_array_unref (attributes);
    attributes = NULL;
  }

  attributes = g_ptr_array_new (); /* => refcnt == 1 */

  gchar*     cwd = g_get_current_dir();
  EdaConfig* cfg = eda_config_get_context_for_path (cwd);
  g_free (cwd);

  GError* err   = NULL;
  gsize   size  = 0;
  gchar** ppstr = eda_config_get_string_list (cfg,
                                              "schematic.attrib",
                                              "always-promote",
                                              &size,
                                              &err);
  if (err == NULL && ppstr != NULL)
  {
    for (gsize i = 0; i < size; ++i)
    {
      gchar* attr = ppstr[i];

      if (attr != NULL && strlen (attr) > 0)
      {
#ifdef DEBUG
        printf( " >> always_promote_attributes += [%s]\n", attr );
#endif
        /* important: use g_intern_string() here, because attr strings are
         * compared like pointers in o_component_is_eligible_attribute():
         */
        g_ptr_array_add (attributes,
                         (gpointer) g_intern_string (attr));
      }
    }

    g_strfreev (ppstr);
  }

  g_clear_error (&err);

  initialised = TRUE;

  return attributes;
}

static gboolean placeholder_rendering = FALSE;


/*! \brief Return the bounds of the given GList of objects.
 *  \par Given a list of objects, calcule the bounds coordinates.
 *
 *  \param [in]  head   The list of objects to look the bounds for.
 *  \param [in] include_hidden If bounds of hidden objects should
 *                             be calculated.
 *  \param [out] left   pointer to the left coordinate of the object.
 *  \param [out] top    pointer to the top coordinate of the object.
 *  \param [out] right  pointer to the right coordinate of the object.
 *  \param [out] bottom pointer to the bottom coordinate of the object.
 *  \return If any bounds were found for the list of objects
 *  \retval 0 No bounds were found
 *  \retval 1 Bound was found
 */
int
world_get_object_glist_bounds (const GList *head,
                               gboolean include_hidden,
                               int *left,
                               int *top,
                               int *right,
                               int *bottom)
{
  const GList *s_current=NULL;
  LeptonObject *o_current=NULL;
  int rleft, rtop, rright, rbottom;
  int found = 0;

  s_current = head;

  /* Find the first object with bounds, and set the bounds variables, then expand as necessary */
  while ( s_current != NULL ) {
    o_current = (LeptonObject *) s_current->data;

    /* Sanity check */
    g_return_val_if_fail ((o_current != NULL), found);

    if (lepton_object_calculate_visible_bounds (o_current,
                                                include_hidden,
                                                &rleft,
                                                &rtop,
                                                &rright,
                                                &rbottom))
    {
      if ( found ) {
        *left = MIN( *left, rleft );
        *top = MIN( *top, rtop );
        *right = MAX( *right, rright );
        *bottom = MAX( *bottom, rbottom );
      } else {
        *left = rleft;
        *top = rtop;
        *right = rright;
        *bottom = rbottom;
        found = 1;
      }
    }
    s_current = g_list_next (s_current);
  }
  return found;
}

/*! \brief Calculate the bounds of a component object
 *
 *  On failure, this function sets the bounds to empty.
 *
 *  \param [in] object The component object.
 *  \param [in] include_hidden If bounds of hidden objects should
 *                             be calculated.
 *  \param [out] bounds The bounds of the component object
 */
void
lepton_component_object_calculate_bounds (const LeptonObject *object,
                                          gboolean include_hidden,
                                          LeptonBounds *bounds)
{
  GList *primitives = NULL;

  lepton_bounds_init (bounds);

  g_return_if_fail (lepton_object_is_component (object));
  g_return_if_fail (object->component != NULL);

  primitives = lepton_component_object_get_contents (object);

  world_get_object_glist_bounds (primitives,
                                 include_hidden,
                                 &(bounds->min_x),
                                 &(bounds->min_y),
                                 &(bounds->max_x),
                                 &(bounds->max_y));
}

/*! \brief get the position of the component base point
 *  \par Function Description
 *  This function gets the position of the base point of a
 *  component object.
 *
 *  \param [in] object   The object to get the position.
 *  \param [out] x       pointer to the x-position
 *  \param [out] y       pointer to the y-position
 *  \return TRUE if successfully determined the position, FALSE otherwise
 */
gboolean
lepton_component_object_get_position (const LeptonObject *object,
                                      gint *x,
                                      gint *y)
{
  g_return_val_if_fail (lepton_object_is_component (object), FALSE);
  g_return_val_if_fail (object->component != NULL, FALSE);

  if (x != NULL) {
    *x = object->component->x;
  }

  if (y != NULL) {
    *y = object->component->y;
  }

  return TRUE;
}

/*! \brief check whether an object is a attributes
 *  \par Function Description
 *  This function checks if an object should be promoted.
 *  An attribute object is promotable if it's promoted by default, or the user
 *  has configered it to promote an attribute.
 *
 *  \param [in] object    The attribute object to check
 *  \return TRUE if the object is a eligible attribute, FALSE otherwise
 */
static int
o_component_is_eligible_attribute (LeptonObject *object)
{
  gboolean promote_invisible;
  g_return_val_if_fail (lepton_object_is_attrib (object), FALSE);

  cfg_read_bool ("schematic.attrib", "promote-invisible",
                 default_promote_invisible, &promote_invisible);

  GPtrArray *attributes = always_promote_attributes ();

  const gchar *name = lepton_text_object_get_name (object);
  if (!name) return FALSE;

  /* always promote symversion= attribute, even if it is invisible */
  if (strncmp (lepton_text_object_get_name (object), "symversion", 10) == 0)
    return TRUE;

  /* check list against attributes which can be promoted */
  if (attributes != NULL) {
    for (guint i = 0; i < attributes->len; ++i) {
      gconstpointer promote =
        g_ptr_array_index (attributes, i);
      if (name == promote)
        return TRUE;
    }
  }

  /* object is invisible and we do not want to promote invisible text */
  if ((!lepton_text_object_is_visible (object)) &&
      (promote_invisible == FALSE))
    return FALSE; /* attribute not eligible for promotion */

  /* yup, attribute can be promoted */
  return TRUE;
}

/*! \brief Get the embedded state of an component object
 *  \par Function Description
 *  Checks and returns the status of the component object.
 *
 *  \param o_current  The object to check
 *  \return TRUE if embedded, FALSE otherwise
 */
gboolean
lepton_component_object_get_embedded (const LeptonObject *o_current)
{
  g_return_val_if_fail (lepton_object_is_component (o_current), FALSE);
  g_return_val_if_fail (o_current->component != NULL, FALSE);

  return o_current->component->embedded;
}


/*! \brief Set the embedded state of an component object
 *  \par Function Description
 *  Amends the status of embedding for the component object.
 *
 *  \param [in] o_current  The object to amend.
 *  \param [in] embedded   TRUE if the object should be embedded,
 *                         FALSE otherwise.
 */
void
lepton_component_object_set_embedded (LeptonObject *o_current,
                                      gboolean embedded)
{
  g_return_if_fail (lepton_object_is_component (o_current));
  g_return_if_fail (o_current->component != NULL);

  o_current->component->embedded = embedded;
}


/*! \brief Get attributes eligible for promotion from inside a component
 *
 *  \par Function Description
 *  Returns a GList of LeptonObjects which are eligible for
 *  promotion from within the passed component LeptonObject.
 *
 *  If detach is TRUE, the function removes these attribute objects
 *  from the prim_objs of the component.  If detach is FALSE, the
 *  LeptonObjects are left in place.
 *
 *  \param [in]  object   The component object being modified.
 *  \param [in]  detach   Should the attributes be detached?
 *  \returns              A linked list of LeptonObjects to promote.
 */
GList*
lepton_component_object_get_promotable (LeptonObject *object,
                                        int detach)
{
  GList *promoted = NULL;
  GList *attribs;
  GList *iter;
  GList *primitives = NULL;
  LeptonObject *tmp;
  gboolean attribute_promotion;

  g_return_val_if_fail (lepton_object_is_component (object), NULL);
  g_return_val_if_fail (object->component != NULL, NULL);

  cfg_read_bool ("schematic.attrib", "promote",
                 default_attribute_promotion, &attribute_promotion);

  if (!attribute_promotion)
    return NULL;

  primitives = lepton_component_object_get_contents (object);
  attribs = o_attrib_find_floating_attribs (primitives);

  for (iter = attribs; iter != NULL; iter = g_list_next (iter)) {
    tmp = (LeptonObject*) iter->data;

    /* Is it an attribute we want to promote? */
    if (!o_component_is_eligible_attribute (tmp))
      continue;

    if (detach) {
      tmp->parent = NULL;
      lepton_component_object_set_contents (object,
                                            g_list_remove (primitives, tmp));
    }

    promoted = g_list_prepend (promoted, tmp);
  }

  g_list_free (attribs);

  promoted = g_list_reverse (promoted);
  return promoted;
}


/*! \brief Promote attributes from a component LeptonObject
 *  \par Function Description
 *  Selects promotable attributes from \a object, and returns a new
 *  #GList containing them (suitable for appending to a #LeptonPage).
 *
 *  \param [in]  object   The component #LeptonObject to promote from.
 *  \return A #GList of promoted attributes.
 */
GList*
o_component_promote_attribs (LeptonObject *object)
{
  GList *promoted = NULL;
  GList *promotable = NULL;
  GList *iter = NULL;
  GList *primitives = NULL;
  gboolean keep_invisible;

  g_return_val_if_fail (lepton_object_is_component (object), NULL);
  g_return_val_if_fail (object->component != NULL, NULL);

  cfg_read_bool ("schematic.attrib", "keep-invisible",
                 default_keep_invisible, &keep_invisible);

  promotable = lepton_component_object_get_promotable (object, FALSE);

  /* Run through the attributes deciding if we want to keep them (in
   * which case we copy them and make them invisible) or if we want to
   * remove them. */
  if (keep_invisible) {
    for (iter = promotable; iter != NULL; iter = g_list_next (iter)) {
      LeptonObject *o_kept = (LeptonObject *) iter->data;
      LeptonObject *o_copy = lepton_object_copy (o_kept);
      lepton_text_object_set_visibility (o_kept, INVISIBLE);
      o_copy->parent = NULL;
      promoted = g_list_prepend (promoted, o_copy);
    }
    promoted = g_list_reverse (promoted);
  } else {
    for (iter = promotable; iter != NULL; iter = g_list_next (iter)) {
      LeptonObject *o_removed = (LeptonObject *) iter->data;
      o_removed->parent = NULL;
      primitives = lepton_component_object_get_contents (object);
      lepton_component_object_set_contents (object,
                                            g_list_remove (primitives, o_removed));
    }
    promoted = promotable;
  }

  /* Attach promoted attributes to the original component
     object. */
  o_attrib_attach_list (promoted, object, TRUE);

  return promoted;
}


/*! \brief Delete or hide promotable from the passed LeptonObject
 *
 *  \par Function Description
 *  Deletes or hides promotable attributes from the passed LeptonObject.
 *  This is used when loading symbols during the load of a schematic from
 *  disk. The schematic will already contain local copies of symbol's
 *  promotable objects, so we delete or hide the symbol's copies.
 *
 *  Deletion / hiding is dependant on the setting of the
 *  "schematic.attrib::keep-invisible" config setting.  If it is
 *  true, attributes eligible for promotion are kept in memory but
 *  flagged as invisible.
 *
 *  \param [in]  object   The component object being altered.
 */
static void
o_component_remove_promotable_attribs (LeptonObject *object)
{
  GList *promotable, *iter;
  GList *primitives = NULL;
  gboolean keep_invisible;

  promotable = lepton_component_object_get_promotable (object, FALSE);

  if (promotable == NULL)
    return;

  cfg_read_bool ("schematic.attrib", "keep-invisible",
                 default_keep_invisible, &keep_invisible);

  for (iter = promotable; iter != NULL; iter = g_list_next (iter)) {
    LeptonObject *a_object = (LeptonObject*) iter->data;
    if (keep_invisible == TRUE) {   /* Hide promotable attributes */
      lepton_text_object_set_visibility (a_object, INVISIBLE);
    } else {                                /* Delete promotable attributes */
      primitives = lepton_component_object_get_contents (object);
      lepton_component_object_set_contents (object,
                                            g_list_remove (primitives, a_object));
      lepton_object_delete (a_object);
    }
  }

  g_list_free (promotable);
}

/*! \brief Enable rendering of placeholders */
void
set_render_placeholders()
{
  placeholder_rendering = TRUE;
}

/*! \brief If placeholders have to be rendered
 *  \return TRUE if placeholders have to be rendered, otherwise
 *          FALSE
 */
static gboolean
render_placeholders()
{
  return placeholder_rendering;
}



/*! \brief Create a placeholder symbol.
 *
 *  \par Function Description
 *  Create a smaller placeholder which looks like this:
 *
 *     |
 *     | missing-symbol.sym
 *     |____________________
 *    /
 *   X
 *
 *  Do not call this function directly,
 *  call create_placeholder() instead.
 *
 *  \param node  Placeholder object.
 *  \param x     Placeholder's origin X.
 *  \param y     Placeholder's origin Y.
 *
 */
static void
create_placeholder_small (LeptonObject* node, int x, int y)
{
  g_return_if_fail (lepton_object_is_component (node));
  g_return_if_fail (node->component != NULL);

  const gint color = DETACHED_ATTRIBUTE_COLOR;
  const gint text_size = 6;

  /* two crossed lines to mark component's origin:
  */
  LeptonObject* line1 = lepton_line_object_new (color,
                                                x - 30, y + 30,
                                                x + 30, y - 30 );
  LeptonObject* line2 = lepton_line_object_new (color,
                                                x - 30, y - 30,
                                                x + 50, y + 50 );

  /* text - symbol file name:
  */
  LeptonObject* txt = lepton_text_object_new (color,
                                              x + 100, y + 100,
                                              LOWER_LEFT,
                                              0,
                                              node->component_basename,
                                              text_size,
                                              VISIBLE,
                                              SHOW_NAME_VALUE);

  LeptonBounds bounds;
  lepton_text_object_calculate_bounds (txt, FALSE, &bounds);

  bounds.max_x = lepton_coord_snap (bounds.max_x, 100);
  bounds.max_y = lepton_coord_snap (bounds.max_y, 100);

  /* two lines at the left and bottom sides of the text:
  */
  LeptonObject* line3 = lepton_line_object_new (color,
                                                x + 50, y + 50,
                                                x + 50, bounds.max_y + 10 );
  LeptonObject* line4 = lepton_line_object_new (color,
                                                x + 50, y + 50,
                                                bounds.max_x + 10, y + 50 );

  LeptonObject* objs[] =
  {
    line1,
    line2,
    txt,
    line3,
    line4
  };

  GList *primitives = NULL;

  for ( size_t i = 0; i < sizeof(objs) / sizeof(objs[0]); ++i )
  {
    primitives = lepton_component_object_get_contents (node);
    lepton_component_object_set_contents (node,
                                          g_list_append (primitives, objs[i]));
  }

} /* create_placeholder_small() */



/*! \brief Create a placeholder symbol.
 *
 *  \par Function Description
 *  Create a placeholder which looks like this:
 *
 *          /\
 *         /  \
 *        / !  \
 *       /______\
 *    Component not found:
 *     missing-symbol.sym
 *   +
 *
 *  Do not call this function directly,
 *  call create_placeholder() instead.
 *
 *  \param new_node  Placeholder object.
 *  \param x         Placeholder's origin X.
 *  \param y         Placeholder's origin Y.
 *
 */
static void
create_placeholder_classic (LeptonObject *new_node, int x, int y)
{
  LeptonBounds bounds;
  LeptonObject *new_prim_obj;
  char *not_found_text = NULL;
  int x_offset, y_offset;
  GList *primitives = NULL;

  g_return_if_fail (lepton_object_is_component (new_node));
  g_return_if_fail (new_node->component != NULL);

  primitives = lepton_component_object_get_contents (new_node);

  /* Mark the origin of the missing component */
  new_prim_obj = lepton_line_object_new (DETACHED_ATTRIBUTE_COLOR,
                                         x - 50, y, x + 50, y);
  primitives = g_list_prepend (primitives, new_prim_obj);
  new_prim_obj = lepton_line_object_new (DETACHED_ATTRIBUTE_COLOR,
                                         x, y + 50, x, y - 50);
  primitives = g_list_prepend (primitives, new_prim_obj);

  /* Add some useful text */
  not_found_text =
    g_strdup_printf (_("Component not found:\n %1$s"),
                     new_node->component_basename);
  new_prim_obj = lepton_text_object_new (DETACHED_ATTRIBUTE_COLOR,
                                         x + NOT_FOUND_TEXT_X,
                                         y + NOT_FOUND_TEXT_Y,
                                         LOWER_LEFT,
                                         0,
                                         not_found_text,
                                         8,
                                         VISIBLE, SHOW_NAME_VALUE);
  primitives = g_list_prepend (primitives, new_prim_obj);

  g_free(not_found_text);

  /* figure out where to put the hazard triangle */
  lepton_text_object_calculate_bounds (new_prim_obj,
                                       FALSE,
                                       &bounds);
  x_offset = (bounds.max_x - bounds.min_x) / 4;
  y_offset = bounds.max_y - bounds.min_y + 100;  /* 100 is just an additional offset */

  /* add hazard triangle */
  new_prim_obj = lepton_line_object_new (DETACHED_ATTRIBUTE_COLOR,
                                         x + NOT_FOUND_TEXT_X + x_offset,
                                         y + NOT_FOUND_TEXT_Y + y_offset,
                                         x + NOT_FOUND_TEXT_X + x_offset + 600,
                                         y + NOT_FOUND_TEXT_Y + y_offset);
  lepton_object_set_line_options (new_prim_obj,
                                  END_ROUND,
                                  TYPE_SOLID,
                                  50,
                                  -1,
                                  -1);
  primitives = g_list_prepend (primitives, new_prim_obj);
  new_prim_obj = lepton_line_object_new (DETACHED_ATTRIBUTE_COLOR,
                                         x + NOT_FOUND_TEXT_X + x_offset,
                                         y + NOT_FOUND_TEXT_Y + y_offset,
                                         x + NOT_FOUND_TEXT_X + x_offset + 300,
                                         y + NOT_FOUND_TEXT_Y + y_offset + 500);
  lepton_object_set_line_options (new_prim_obj,
                                  END_ROUND,
                                  TYPE_SOLID,
                                  50,
                                  -1,
                                  -1);
  primitives = g_list_prepend (primitives, new_prim_obj);
  new_prim_obj = lepton_line_object_new (DETACHED_ATTRIBUTE_COLOR,
                                         x + NOT_FOUND_TEXT_X + x_offset + 300,
                                         y + NOT_FOUND_TEXT_Y + y_offset + 500,
                                         x + NOT_FOUND_TEXT_X + x_offset + 600,
                                         y + NOT_FOUND_TEXT_Y + y_offset);
  lepton_object_set_line_options (new_prim_obj,
                                  END_ROUND,
                                  TYPE_SOLID,
                                  50,
                                  -1,
                                  -1);
  primitives = g_list_prepend (primitives, new_prim_obj);
  new_prim_obj = lepton_text_object_new (DETACHED_ATTRIBUTE_COLOR,
                                         x + NOT_FOUND_TEXT_X + x_offset + 270,
                                         y + NOT_FOUND_TEXT_Y + y_offset + 90,
                                         LOWER_LEFT,
                                         0,
                                         "!",
                                         18,
                                         VISIBLE,
                                         SHOW_NAME_VALUE);
  primitives = g_list_prepend (primitives, new_prim_obj);
  primitives = g_list_reverse (primitives);
  lepton_component_object_set_contents (new_node, primitives);

} /* create_placeholder_classic() */



/*! \brief Create a placeholder symbol.
 *
 *  \par Function Description
 *  Create a placeholder to be used in place of missing symbol.
 *  Depending on value of the schematic.gui::small-placeholders
 *  configuration key, draw either alternative, smaller
 *  placeholder (true) or the original one (false).
 *
 *  \param node  Placeholder object.
 *  \param x     Placeholder's origin X.
 *  \param y     Placeholder's origin Y.
 *
 */
static void
create_placeholder (LeptonObject* node, int x, int y)
{
  lepton_component_object_set_missing (node, TRUE);

  /* Some programs (e.g. netlister) don't need to render
   * anything, so we just return here. */
  if (!render_placeholders())
  {
    return;
  }

  gboolean small_placeholders = TRUE;
  cfg_read_bool ("schematic.gui", "small-placeholders",
                 TRUE, &small_placeholders);

  if (small_placeholders)
  {
    create_placeholder_small (node, x, y);
  }
  else
  {
    create_placeholder_classic (node, x, y);
  }

} /* create_placeholder() */



/* Done */
/*! \brief
 *  \par Function Description
 *
 */
LeptonObject*
lepton_component_new (LeptonPage *page,
                      int color,
                      int x,
                      int y,
                      int angle,
                      int mirror,
                      const CLibSymbol *clib,
                      const gchar *basename,
                      int selectable)
{
  LeptonObject *new_node=NULL;
  GList *iter;
  gchar *buffer = NULL;
  GList *primitives = NULL;

  new_node = lepton_object_new (OBJ_COMPONENT, "complex");

  if (clib != NULL) {
    new_node->component_basename = g_strdup (s_clib_symbol_get_name (clib));
  } else {
    new_node->component_basename = g_strdup (basename);
  }

  new_node->selectable = selectable;

  new_node->component = (LeptonComponent *) g_malloc (sizeof (LeptonComponent));
  lepton_component_object_set_contents (new_node, NULL);
  new_node->component->angle = angle;
  new_node->component->mirror = mirror;
  new_node->component->x = x;
  new_node->component->y = y;
  /* Do setting color after initialization of prim_objs as the
     function sets color of prim_objs as well. */
  lepton_object_set_color (new_node, color);
  /* For now, consider the component exists in the library. If
     something goes wrong, this will be changed in
     create_placeholder(). */
  lepton_component_object_set_missing (new_node, FALSE);
  lepton_component_object_set_embedded (new_node, FALSE);

  /* get the symbol data */
  if (clib != NULL) {
    buffer = s_clib_symbol_get_data (clib);
  }

  if (clib == NULL || buffer == NULL)
    create_placeholder (new_node, x, y);
  else {
    GError * err = NULL;

    /* add connections till translated */
    lepton_component_object_set_contents (new_node,
                                          o_read_buffer (page,
                                                         NULL,
                                                         buffer,
                                                         -1,
                                                         new_node->component_basename,
                                                         &err));
    if (err) {
      g_error_free(err);
      /* If reading fails, replace with placeholder object */
      create_placeholder (new_node, x, y);
    }
    else {
      primitives = lepton_component_object_get_contents (new_node);
      if (mirror) {
        lepton_object_list_mirror (primitives, 0, 0);
      }

      lepton_object_list_rotate (primitives, 0, 0, angle);
      lepton_object_list_translate (primitives, x, y);
    }

    g_free (buffer);

  }

  /* set the parent field now */
  for (iter = lepton_component_object_get_contents (new_node);
       iter != NULL;
       iter = g_list_next (iter))
  {
    LeptonObject *tmp = (LeptonObject*) iter->data;
    tmp->parent = new_node;
  }

  return new_node;
}

/*! \brief create a new embedded object
 *  \par Function Description
 *  This function creates a new embedded object.
 *
 *  \param [in]  color     The color of the object
 *  \param [in]  x         The x location of the component object
 *  \param [in]  y         The y location of the component object
 *  \param [in]  angle     The rotation angle
 *  \param [in]  mirror    The mirror status
 *  \param [in]  basename  The basic name the embedded was created of
 *  \param [in]  selectable whether the object can be selected with the mouse
 *  \return a new component object
 */
LeptonObject*
lepton_component_new_embedded (int color,
                               int x,
                               int y,
                               int angle,
                               int mirror,
                               const gchar *basename,
                               int selectable)
{
  LeptonObject *new_node=NULL;

  new_node = lepton_object_new (OBJ_COMPONENT, "complex");

  new_node->component = (LeptonComponent *) g_malloc (sizeof (LeptonComponent));
  new_node->component->x = x;
  new_node->component->y = y;

  new_node->component->angle = angle;
  new_node->component->mirror = mirror;

  new_node->component_basename = g_strdup(basename);

  new_node->selectable = selectable;

  lepton_component_object_set_contents (new_node, NULL);

  lepton_object_set_color (new_node, color);
  /* Consider embedded components always exist since they are read
     from schematic file. */
  lepton_component_object_set_missing (new_node, FALSE);
  lepton_component_object_set_embedded (new_node, TRUE);

  /* don't have to translate/rotate/mirror here at all since the */
  /* object is in place */
  return new_node;
}

/*! \brief read a component object from a char buffer
 *  \par Function Description
 *  This function reads a component object from the buffer \a buf.
 *  If the component object was read successfully, a new object is
 *  allocated and appended to the \a object_list.
 *
 *  \param [in] page         The LeptonPage object
 *  \param [in] buf          a text buffer (usually a line of a schematic file)
 *  \param [in] release_ver  The release number gEDA
 *  \param [in] fileformat_ver a integer value of the file format
 *  \return The object list, or NULL on error.
 */
LeptonObject *o_component_read (LeptonPage *page,
                                const char buf[],
                                unsigned int release_ver,
                                unsigned int fileformat_ver,
                                GError **err)
{
  LeptonObject *new_obj;
  char type;
  int x1, y1;
  int angle;

  char *basename = (char*) g_malloc (1 + strlen (buf));

  int selectable;
  int mirror;

  if (sscanf(buf, "%c %d %d %d %d %d %s\n",
             &type, &x1, &y1, &selectable, &angle, &mirror, basename) != 7) {
    g_set_error(err, EDA_ERROR, EDA_ERROR_PARSE, _("Failed to parse component object"));
    g_free (basename);
    return NULL;
  }

  switch(angle) {

    case(0):
    case(90):
    case(180):
    case(270):
      break;

    default:
      g_message (_("Found a component with an invalid rotation "
                   "[ %1$c %2$d %3$d %4$d %5$d %6$d %7$s ]"),
                 type, x1, y1, selectable, angle, mirror, basename);
      g_message (_("Setting angle to 0."));
      angle = 0;
  }

  switch(mirror) {

    case(0):
    case(1):

      break;

    default:
      g_message (_("Found a component with an invalid mirror flag "
                   "[ %1$c %2$d %3$d %4$d %5$d %6$d %7$s ]"),
                 type, x1, y1, selectable, angle, mirror, basename);
      g_message (_("Setting mirror to 0."));
      mirror = 0;
  }
  if (strncmp(basename, "EMBEDDED", 8) == 0) {

    new_obj = lepton_component_new_embedded (default_color_id(),
                                             x1,
                                             y1,
                                             angle,
                                             mirror,
                                             basename + 8,
                                             selectable);
  } else {

    const CLibSymbol *clib = s_clib_get_symbol_by_name (basename);

    new_obj = lepton_component_new (page,
                                    default_color_id(),
                                    x1,
                                    y1,
                                    angle,
                                    mirror,
                                    clib,
                                    basename,
                                    selectable);
    /* Delete or hide attributes eligible for promotion inside the
       component. */
    if (new_obj)
      o_component_remove_promotable_attribs (new_obj);
  }

  g_free (basename);

  return new_obj;
}

/*! \brief Create a string representation of the component object
 *  \par Function Description
 *  This function takes a component \a object and return a string
 *  according to the file format definition.
 *
 *  On failure, this function returns NULL.
 *
 *  \param [in] object  a component LeptonObject
 *  \return the string representation of the component LeptonObject
 */
gchar*
lepton_component_object_to_buffer (const LeptonObject *object)
{
  gchar *basename;
  gchar *buffer;

  g_return_val_if_fail (lepton_object_is_component (object), NULL);
  g_return_val_if_fail (object->component != NULL, NULL);

  basename = g_strdup_printf ("%s%s",
                              lepton_component_object_get_embedded (object) ? "EMBEDDED" : "",
                              object->component_basename);

  /* We force the object type to be output as OBJ_COMPONENT for both these object
   * types.
   */
  buffer = g_strdup_printf ("%c %d %d %d %d %d %s",
                            lepton_object_get_type (object),
                            object->component->x,
                            object->component->y,
                            lepton_object_get_selectable (object),
                            object->component->angle,
                            object->component->mirror,
                            basename);

  g_free (basename);

  return buffer;
}

/*! \brief move a component object
 *  \par Function Description
 *  This function changes the position of a component \a object.
 *
 *  \param [ref] object  The component LeptonObject to be moved
 *  \param [in]  dx      The x-distance to move the object
 *  \param [in]  dy      The y-distance to move the object
 */
void
lepton_component_object_translate (LeptonObject *object, int dx, int dy)
{
  GList *primitives = NULL;

  g_return_if_fail (lepton_object_is_component (object));
  g_return_if_fail (object->component != NULL);

  object->component->x = object->component->x + dx;
  object->component->y = object->component->y + dy;

  primitives = lepton_component_object_get_contents (object);
  lepton_object_list_translate (primitives, dx, dy);
}

/*! \brief Create a copy of a component object
 *  \par Function Description
 *  This function creates a copy of the component object \a
 *  o_current.
 *
 *  \param [in] o_current    The object that is copied
 *  \return a new component object
 */
LeptonObject*
o_component_copy (LeptonObject *o_current)
{
  LeptonObject *o_new;
  GList *iter;
  GList *primitives = NULL;

  g_return_val_if_fail (lepton_object_is_component (o_current), NULL);
  g_return_val_if_fail (o_current->component != NULL, NULL);

  o_new = lepton_object_new (lepton_object_get_type (o_current), "complex");
  o_new->selectable = o_current->selectable;
  o_new->component_basename = g_strdup(o_current->component_basename);

  o_new->component = (LeptonComponent*) g_malloc0 (sizeof (LeptonComponent));
  o_new->component->x = o_current->component->x;
  o_new->component->y = o_current->component->y;
  o_new->component->angle = o_current->component->angle;
  o_new->component->mirror = o_current->component->mirror;

  /* Set prim_objs temporarily to NULL to prevent crashes on color
     initialization. */
  lepton_component_object_set_contents (o_new, NULL);
  lepton_object_set_color (o_new, lepton_object_get_color (o_current));
  lepton_component_object_set_missing (o_new,
                                       lepton_component_object_get_missing (o_current));
  lepton_component_object_set_embedded (o_new,
                                        lepton_component_object_get_embedded (o_current));

  /* Copy contents and set the parent pointers on the copied objects. */
  primitives = lepton_component_object_get_contents (o_current);
  lepton_component_object_set_contents (o_new,
                                        o_glist_copy_all (primitives, NULL));

  for (iter = lepton_component_object_get_contents (o_new);
       iter != NULL;
       iter = g_list_next (iter)) {
    ((LeptonObject*) iter->data)->parent = o_new;
  }

  /* Delete or hide attributes eligible for promotion inside the
     component. */
  o_component_remove_promotable_attribs (o_new);

  s_slot_update_object (o_new);

  /* deal with stuff that has changed */

  /* here you need to create a list of attributes which need to be
   * connected to the new list, probably make an attribute list and
   * fill it with sid's of the attributes */

  return o_new;
}


/*! \brief Rotates a component object in world coordinates
 *  \par Function Description
 *  This function rotates a component \a object around the
 *  (\a centerx,\a centery) point by \a angle degrees.
 *  The center of rotation is in world units.
 *
 *  \param [in]      centerx   X coordinate of rotation center (world coords).
 *  \param [in]      centery   Y coordinate of rotation center (world coords).
 *  \param [in]      angle     Rotation angle in degrees.
 *  \param [in,out]  object    Component object to rotate.
 */
void
lepton_component_object_rotate (int centerx,
                                int centery,
                                int angle,
                                LeptonObject *object)
{
  int x, y;
  int newx, newy;
  GList *primitives = NULL;

  g_return_if_fail (lepton_object_is_component (object));
  g_return_if_fail (object->component != NULL);

  x = object->component->x + (-centerx);
  y = object->component->y + (-centery);

  lepton_point_rotate_90 (x, y, angle, &newx, &newy);

  x = newx + (centerx);
  y = newy + (centery);

  lepton_component_object_translate (object, -object->component->x, -object->component->y);

  primitives = lepton_component_object_get_contents (object);
  lepton_object_list_rotate (primitives, 0, 0, angle);

  object->component->x = 0;
  object->component->y = 0;

  lepton_component_object_translate (object, x, y);

  object->component->angle = ( object->component->angle + angle ) % 360;
}


/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void
lepton_component_object_mirror (int world_centerx,
                                int world_centery,
                                LeptonObject *object)
{
  int x, y;
  GList *primitives = NULL;

  g_return_if_fail (lepton_object_is_component (object));
  g_return_if_fail (object->component != NULL);

  x = 2 * world_centerx - object->component->x;
  y = object->component->y;

  lepton_component_object_translate (object, -object->component->x, -object->component->y);

  primitives = lepton_component_object_get_contents (object);
  lepton_object_list_mirror (primitives, 0, 0);

  switch(object->component->angle) {
    case(90):
      object->component->angle = 270;
      break;

    case(270):
      object->component->angle = 90;
      break;

  }

  object->component->mirror = !object->component->mirror;

  lepton_component_object_translate (object, x, y);
}


/*! \brief Find a pin with a particular attribute.
 *  \par Function Description
 *  Search for a pin inside the given component which has an attribute
 *  matching those passed.
 *
 *  \param [in] object        component LeptonObject whos pins to search.
 *  \param [in] name          the attribute name to search for.
 *  \param [in] wanted_value  the attribute value to search for.
 *  \return The pin LeptonObject with the given attribute, NULL otherwise.
 */
LeptonObject*
o_component_find_pin_by_attribute (LeptonObject *object,
                                   const char *name,
                                   char *wanted_value)
{
  GList *iter;
  LeptonObject *o_current;
  char *value;
  int found;

  g_return_val_if_fail (lepton_object_is_component (object), NULL);
  g_return_val_if_fail (object->component != NULL, NULL);

  for (iter = lepton_component_object_get_contents (object);
       iter != NULL;
       iter = g_list_next (iter)) {
    o_current = (LeptonObject*) iter->data;

    if (!lepton_object_is_pin (o_current))
      continue;

    value = o_attrib_search_object_attribs_by_name (o_current, name, 0);
    found = (value != NULL && strcmp (value, wanted_value) == 0);
    g_free (value);

    if (found)
      return o_current;
  }

  return NULL;
}


/*! \brief check the symversion of a component object
 *  \par Function Description
 *  This function compares the symversion of a symbol with it's
 *  earlier saved symversion in a schematic.
 *  Major symversion changes are added to the page object
 *  (page->major_changed_refdes), minor changes are reported
 *  to the messaging system.
 *
 *  \param page      The LeptonPage object
 *  \param object    The component LeptonObject
 */
void
o_component_check_symversion (LeptonPage* page,
                              LeptonObject* object)
{
  char *inside = NULL;
  char *outside = NULL;
  char *refdes = NULL;
  double inside_value = -1.0;
  double outside_value = -1.0;
  char *err_check = NULL;
  int inside_present = FALSE;
  int outside_present = FALSE;
  double inside_major, inside_minor;
  double outside_major, outside_minor;

  g_return_if_fail (lepton_object_is_component (object));
  g_return_if_fail (object->component != NULL);


  /* No need to check symversion if symbol is not found in libraries:
  */
  GList* symlist = s_clib_search (object->component_basename, CLIB_EXACT);
  if (symlist == NULL)
  {
    return;
  }
  g_list_free (symlist);


  /* first look on the inside for the symversion= attribute */
  inside = o_attrib_search_inherited_attribs_by_name (object, "symversion", 0);

  /* now look for the symversion= attached to object */
  outside = o_attrib_search_attached_attribs_by_name (object, "symversion", 0);

  /* get the uref for future use */
  refdes = o_attrib_search_object_attribs_by_name(object, "refdes", 0);
  if (!refdes)
  {
    refdes = g_strdup ("no refdes");
  }

  if (inside)
  {
    inside_value = strtod(inside, &err_check);
    if (inside_value == 0 && inside == err_check)
    {
      if (inside)
      {
        g_message (_("WARNING: %s (%s): could not parse "
                     "symversion (%s) in symbol file"),
                   object->component_basename,
                   refdes,
                   inside);
      } else {
        g_message (_("WARNING: %s (%s): could not parse "
                     "symversion in symbol file"),
                   object->component_basename,
                   refdes);
      }
      goto done;
    }
    inside_present = TRUE;
  } else {
    inside_present = FALSE;  /* attribute not inside */
  }

  if (outside)
  {
    outside_value = strtod(outside, &err_check);
    if (outside_value == 0 && outside == err_check)
    {
      g_message (_("WARNING: %s (%s): could not parse "
                   "attached symversion (%s)"),
                 object->component_basename,
                 refdes,
                 outside);
      goto done;
    }
    outside_present = TRUE;
  } else {
    outside_present = FALSE;  /* attribute not outside */
  }

#if DEBUG
  printf("%s:\n\tinside: %.1f outside: %.1f\n\n", object->name,
         inside_value, outside_value);
#endif

  /* symversion= is not present anywhere */
  if (!inside_present && !outside_present)
  {
    /* symbol is legacy and versioned okay */
    goto done;
  }

  /* No symversion inside, but a version is outside, this is a weird case */
  if (!inside_present && outside_present)
  {
    g_message (_("WARNING: %s (%s): symversion attached, "
                 "but absent inside symbol file"),
               object->component_basename,
               refdes);
    goto done;
  }

  /* inside & not outside is a valid case, means symbol in library is newer */
  /* also if inside_value is greater than outside_value, then symbol in */
  /* library is newer */
  if ((inside_present && !outside_present) ||
      ((inside_present && outside_present) && (inside_value > outside_value)))
  {

    /* break up the version values into major.minor numbers */
    inside_major = floor(inside_value);
    inside_minor = inside_value - inside_major;

    if (outside_present)
    {
      outside_major = floor(outside_value);
      outside_minor = outside_value - outside_major;
    } else {
      /* symversion was not attached to the symbol, set all to zero */
      outside_major = 0.0;
      outside_minor = 0.0;
      outside_value = 0.0;
    }

#if DEBUG
    printf("i: %f %f %f\n", inside_value, inside_major, inside_minor);
    printf("o: %f %f %f\n", outside_value, outside_major, outside_minor);
#endif

    if (inside_major > outside_major)
    {
      char* refdes_copy;

      g_message (_("WARNING: %s (%s): MAJOR symversion change "
                   "(attached: %.3f < library: %.3f)"),
                 object->component_basename,
                 refdes,
                 outside_value,
                 inside_value);

      /* add the refdes and basename to the page's major_changed_refdes GList:
      */
      if (page != NULL)
      {
        /* make sure refdes_copy is freed somewhere:
        */
        refdes_copy = g_strconcat ("refdes: ",
                                   refdes,
                                   " (",
                                   object->component_basename,
                                   ")",
                                   NULL);

        page->major_changed_refdes =
          g_list_append (page->major_changed_refdes,
                         refdes_copy);
      }
      else
      {
        g_warning (" >> o_complex_check_symversion(): !page_current");
      }


      /* don't bother checking minor changes if there are major ones*/
      goto done;
    }

    if (inside_minor > outside_minor)
    {
      g_message (_("WARNING: %s (%s): minor symversion change "
                   "(attached: %.3f < library: %.3f)"),
                 object->component_basename,
                 refdes,
                 outside_value,
                 inside_value);
    }

    goto done;
  }

  /* outside value is greater than inside value, this is weird case */
  if ((inside_present && outside_present) && (outside_value > inside_value))
  {
    g_message (_("WARNING: %s (%s): symbol is newer "
                 "than symbol in library (%.3f > %.3f)"),
               object->component_basename,
               refdes,
               outside_value,
               inside_value);
    goto done;
  }

  /* if inside_value and outside_value match, then symbol versions are okay */

done:
  g_free(inside);
  g_free(outside);
  g_free(refdes);
}

/*! \brief Calculates the distance between the given point and the closest
 * point on an object within the component object.
 *
 *  \note When querying the distance to our child objects, we always
 *        force treating them as solid filled.
 *        We ignore the force_solid argument to this function.
 *
 *  \param [in] object         The component LeptonObject.
 *  \param [in] x              The x coordinate of the given point.
 *  \param [in] y              The y coordinate of the given point.
 *  \param [in] force_solid    If true, force treating the object as solid.
 *  \param [in] include_hidden Take hidden text into account.
 *  \return The shortest distance from the object to the point. If the
 *  distance cannot be calculated, this function returns a really large
 *  number (G_MAXDOUBLE).  With an invalid parameter, this function returns
 *  G_MAXDOUBLE.
 */
double
lepton_component_object_shortest_distance (LeptonObject *object,
                                           int x,
                                           int y,
                                           int force_solid,
                                           gboolean include_hidden)
{
  double shortest_distance = G_MAXDOUBLE;
  double distance;
  int found_line_bounds = 0;
  LeptonBox line_bounds;
  GList *iter;

  g_return_val_if_fail (lepton_object_is_component (object), G_MAXDOUBLE);
  g_return_val_if_fail (object->component != NULL, G_MAXDOUBLE);

  for (iter = lepton_component_object_get_contents (object);
       iter != NULL; iter= g_list_next (iter)) {
    LeptonObject *obj = (LeptonObject*) iter->data;
    int left, top, right, bottom;

    /* Collect the bounds of any lines and arcs in the symbol */
    if ((lepton_object_is_line (obj) || lepton_object_is_arc (obj)) &&
        lepton_object_calculate_visible_bounds (obj,
                                                include_hidden,
                                                &left,
                                                &top,
                                                &right,
                                                &bottom))
    {
      if (found_line_bounds) {
        line_bounds.lower_x = MIN (line_bounds.lower_x, left);
        line_bounds.lower_y = MIN (line_bounds.lower_y, top);
        line_bounds.upper_x = MAX (line_bounds.upper_x, right);
        line_bounds.upper_y = MAX (line_bounds.upper_y, bottom);
      } else {
        line_bounds.lower_x = left;
        line_bounds.lower_y = top;
        line_bounds.upper_x = right;
        line_bounds.upper_y = bottom;
        found_line_bounds = 1;
      }
    } else {
      distance = lepton_object_shortest_distance_full (obj, x, y, TRUE, include_hidden);
      shortest_distance = MIN (shortest_distance, distance);
    }

    if (shortest_distance == 0.0)
      return shortest_distance;
  }

  if (found_line_bounds) {
    distance = lepton_box_shortest_distance (&line_bounds, x, y, TRUE);
    shortest_distance = MIN (shortest_distance, distance);
  }

  return shortest_distance;
}

/*! \brief Test component object's 'missing' flag.
 *
 *  \param [in] object The component object to test.
 *  \return The value of the 'missing' flag.
 */
gboolean
lepton_component_object_get_missing (const LeptonObject *object)
{
  g_return_val_if_fail (lepton_object_is_component (object), TRUE);
  g_return_val_if_fail (object->component != NULL, TRUE);

  return object->component->missing;
}


/*! \brief Set component object's 'missing' flag.
 *
 *  \param [in] object  The component object to amend.
 *  \param [in] missing The new value of the 'missing' flag.
 */
void
lepton_component_object_set_missing (const LeptonObject *object,
                                     gboolean missing)
{
  g_return_if_fail (lepton_object_is_component (object));
  g_return_if_fail (object->component != NULL);

  object->component->missing = missing;
}


/*! \brief Embed a component object into its schematic.
 *  \par Function Description
 *  This function embeds a component object into its schematic.
 *  The object is just marked as embedded.
 *
 *  \param object The #LeptonObject to embed
 */
void
lepton_component_object_embed (LeptonObject *object)
{
  LeptonPage *page;

  g_return_if_fail (lepton_object_is_component (object));

  page = lepton_object_get_page (object);

  /* Check the component is not embedded. */
  if (lepton_component_object_get_embedded (object))
    return;

  /* Set the embedded flag. */
  lepton_component_object_set_embedded (object, TRUE);

  g_message (_("Component [%1$s] has been embedded."),
             object->component_basename);
  /* Page content has been modified. */
  if (page != NULL)
  {
    lepton_page_set_changed (page, 1);
  }
}


/*! \brief Unembed a component object from its schematic.
 *  \par Function Description
 *  This function unembeds a component object from its
 *  schematic. The object is just marked as not embedded.
 *
 *  \param object The #LeptonObject to unembed
 */
void
lepton_component_object_unembed (LeptonObject *object)
{
  LeptonPage *page;
  const CLibSymbol *sym;

  g_return_if_fail (lepton_object_is_component (object));

  page = lepton_object_get_page (object);

  /* Check the component is embedded. */
  if (!lepton_component_object_get_embedded (object))
    return;

  /* Search for the symbol in the component library. */
  sym = s_clib_get_symbol_by_name (object->component_basename);

  if (sym == NULL)
  {
    /* Symbol not found in the symbol library: signal an error. */
    g_message (_("Could not find component [%1$s], while trying to "
                 "unembed. Component is still embedded."),
               object->component_basename);
  }
  else
  {
    /* Clear the embedded flag. */
    lepton_component_object_set_embedded (object, FALSE);

    g_message (_("Component [%1$s] has been successfully unembedded."),
               object->component_basename);

    /* Page content has been modified. */
    if (page != NULL)
    {
      lepton_page_set_changed (page, 1);
    }
  }
}
