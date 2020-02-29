/* Lepton EDA library
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2017 gEDA Contributors
 * Copyright (C) 2017-2020 Lepton EDA Contributors
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

/*! \file geda_component_object.c
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

#include "libgeda_priv.h"

/*! \brief Return the bounds of the given GList of objects.
 *  \par Given a list of objects, calcule the bounds coordinates.
 *  \param [in]  toplevel The TOPLEVEL structure.
 *  \param [in]  head   The list of objects to look the bounds for.
 *  \param [out] left   pointer to the left coordinate of the object.
 *  \param [out] top    pointer to the top coordinate of the object.
 *  \param [out] right  pointer to the right coordinate of the object.
 *  \param [out] bottom pointer to the bottom coordinate of the object.
 *  \return If any bounds were found for the list of objects
 *  \retval 0 No bounds were found
 *  \retval 1 Bound was found
 */
int world_get_object_glist_bounds(TOPLEVEL *toplevel, const GList *head,
                                  int *left, int *top, int *right, int *bottom)
{
  const GList *s_current=NULL;
  OBJECT *o_current=NULL;
  int rleft, rtop, rright, rbottom;
  int found = 0;

  s_current = head;

  /* Find the first object with bounds, and set the bounds variables, then expand as necessary */
  while ( s_current != NULL ) {
    o_current = (OBJECT *) s_current->data;

    /* Sanity check */
    g_return_val_if_fail ((o_current != NULL), found);

    if ( geda_object_calculate_visible_bounds( toplevel, o_current, &rleft, &rtop, &rright, &rbottom) ) {
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
 *  \param [in] toplevel The toplevel object.
 *  \param [in] object The component object.
 *  \param [out] bounds The bounds of the component object
 */
void
geda_component_object_calculate_bounds (TOPLEVEL *toplevel,
                                        const OBJECT *object,
                                        GedaBounds *bounds)
{
  geda_bounds_init (bounds);

  g_return_if_fail (object != NULL);
  g_return_if_fail (((object->type == OBJ_COMPONENT) || (object->type == OBJ_PLACEHOLDER)));
  g_return_if_fail (object->component != NULL);

  world_get_object_glist_bounds (toplevel,
                                 object->component->prim_objs,
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
geda_component_object_get_position (const GedaObject *object, gint *x, gint *y)
{
  g_return_val_if_fail (object != NULL, FALSE);
  g_return_val_if_fail (((object->type == OBJ_COMPONENT) || (object->type == OBJ_PLACEHOLDER)), FALSE);
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
 *  \param [in] toplevel  The TOPLEVEL object
 *  \param [in] object    The attribute object to check
 *  \return TRUE if the object is a eligible attribute, FALSE otherwise
 */
static int
o_component_is_eligible_attribute (TOPLEVEL *toplevel, OBJECT *object)
{
  g_return_val_if_fail (toplevel, FALSE);
  g_return_val_if_fail (object, FALSE);

  const gchar *name = o_attrib_get_name (object);
  if (!name) return FALSE;

  /* always promote symversion= attribute, even if it is invisible */
  if (strncmp(o_attrib_get_name(object), "symversion", 10) == 0)
    return TRUE;

  /* check list against attributes which can be promoted */
  if (toplevel->always_promote_attributes != NULL) {
    for (guint i = 0; i < toplevel->always_promote_attributes->len; ++i) {
      gconstpointer promote =
        g_ptr_array_index(toplevel->always_promote_attributes, i);
      if (name == promote)
        return TRUE;
    }
  }

  /* object is invisible and we do not want to promote invisible text */
  if ((!o_is_visible (toplevel, object)) &&
      (toplevel->promote_invisible == FALSE))
    return FALSE; /* attribute not eligible for promotion */

  /* yup, attribute can be promoted */
  return TRUE;
}

/*! \brief get the embedded state of an component object
 *  \par Function Description
 *  Checks and returns the status of the component object.
 *
 *  \param o_current  The object to check
 *  \return 1 if embedded, 0 otherwise
 */
int
o_component_is_embedded (OBJECT *o_current)
{
  g_return_val_if_fail(o_current != NULL, 0);

  if(o_current->component == NULL)
    return 0;

  if (o_current->component_embedded) {
    return 1;
  } else {
    return 0;
  }
}


/*! \brief Get attributes eligible for promotion from inside a component
 *
 *  \par Function Description
 *  Returns a GList of OBJECTs which are eligible for promotion from
 *  within the passed component OBJECT.
 *
 *  If detach is TRUE, the function removes these attribute objects
 *  from the prim_objs of the component.  If detach is FALSE, the
 *  OBJECTs are left in place.
 *
 *  \param [in]  toplevel The toplevel environment.
 *  \param [in]  object   The component object being modified.
 *  \param [in]  detach   Should the attributes be detached?
 *  \returns              A linked list of OBJECTs to promote.
 */
GList *o_component_get_promotable (TOPLEVEL *toplevel, OBJECT *object, int detach)
{
  GList *promoted = NULL;
  GList *attribs;
  GList *iter;
  OBJECT *tmp;

  if (!toplevel->attribute_promotion) /* controlled through rc file */
    return NULL;

  attribs = o_attrib_find_floating_attribs (object->component->prim_objs);

  for (iter = attribs; iter != NULL; iter = g_list_next (iter)) {
    tmp = (OBJECT*) iter->data;

    /* Is it an attribute we want to promote? */
    if (!o_component_is_eligible_attribute (toplevel, tmp))
      continue;

    if (detach) {
      tmp->parent = NULL;
      object->component->prim_objs =
        g_list_remove (object->component->prim_objs, tmp);
    }

    promoted = g_list_prepend (promoted, tmp);
  }

  g_list_free (attribs);

  promoted = g_list_reverse (promoted);
  return promoted;
}


/*! \brief Promote attributes from a component OBJECT
 *  \par Function Description
 *  Selects promotable attributes from \a object, and returns a new
 *  #GList containing them (suitable for appending to a #PAGE).
 *
 *  \param [in]  toplevel The #TOPLEVEL environment.
 *  \param [in]  object   The component #OBJECT to promote from.
 *  \return A #GList of promoted attributes.
 */
GList *o_component_promote_attribs (TOPLEVEL *toplevel, OBJECT *object)
{
  GList *promoted = NULL;
  GList *promotable = NULL;
  GList *iter = NULL;

  promotable = o_component_get_promotable (toplevel, object, FALSE);

  /* Run through the attributes deciding if we want to keep them (in
   * which case we copy them and make them invisible) or if we want to
   * remove them. */
  if (toplevel->keep_invisible) {
    for (iter = promotable; iter != NULL; iter = g_list_next (iter)) {
      OBJECT *o_kept = (OBJECT *) iter->data;
      OBJECT *o_copy = o_object_copy (toplevel, o_kept);
      o_set_visibility (toplevel, o_kept, INVISIBLE);
      o_copy->parent = NULL;
      promoted = g_list_prepend (promoted, o_copy);
    }
    promoted = g_list_reverse (promoted);
  } else {
    for (iter = promotable; iter != NULL; iter = g_list_next (iter)) {
      OBJECT *o_removed = (OBJECT *) iter->data;
      o_removed->parent = NULL;
      object->component->prim_objs =
        g_list_remove (object->component->prim_objs, o_removed);
    }
    promoted = promotable;
  }

  /* Attach promoted attributes to the original component
     object. */
  o_attrib_attach_list (toplevel, promoted, object, TRUE);

  return promoted;
}


/*! \brief Delete or hide promotable from the passed OBJECT
 *
 *  \par Function Description
 *  Deletes or hides promotable attributes from the passed OBJECT.
 *  This is used when loading symbols during the load of a schematic from
 *  disk. The schematic will already contain local copies of symbol's
 *  promotable objects, so we delete or hide the symbol's copies.
 *
 *  Deletion / hiding is dependant on the setting of
 *  toplevel->keep_invisible. If true, attributes eligible for
 *  promotion are kept in memory but flagged as invisible.
 *
 *  \param [in]  toplevel The toplevel environment.
 *  \param [in]  object   The component object being altered.
 */
static void o_component_remove_promotable_attribs (TOPLEVEL *toplevel, OBJECT *object)
{
  GList *promotable, *iter;

  promotable = o_component_get_promotable (toplevel, object, FALSE);

  if (promotable == NULL)
    return;

  for (iter = promotable; iter != NULL; iter = g_list_next (iter)) {
    OBJECT *a_object = (OBJECT*) iter->data;
    if (toplevel->keep_invisible == TRUE) {   /* Hide promotable attributes */
      o_set_visibility (toplevel, a_object, INVISIBLE);
    } else {                                /* Delete promotable attributes */
      object->component->prim_objs =
        g_list_remove (object->component->prim_objs, a_object);
      s_delete_object (toplevel, a_object);
    }
  }

  g_list_free (promotable);
}

static void create_placeholder(TOPLEVEL * toplevel, OBJECT * new_node, int x, int y)
{
    GedaBounds bounds;
    OBJECT *new_prim_obj;
    char *not_found_text = NULL;
    int x_offset, y_offset;

    /* Put placeholder into object list.  Changed by SDB on
     * 1.19.2005 to fix problem that symbols were silently
     * deleted by gattrib when RC files were messed up.  */
    new_node->type = OBJ_PLACEHOLDER;

    /* Some programs (e.g. netlister) don't need to render
     * anything, so we just return here. */
    if (!toplevel->rendered_text_bounds_func) {
      return;
    }

    /* Mark the origin of the missing component */
    new_prim_obj = geda_line_object_new (toplevel,
                                        DETACHED_ATTRIBUTE_COLOR,
                                        x - 50, y, x + 50, y);
    new_node->component->prim_objs = g_list_prepend (new_node->component->prim_objs, new_prim_obj);
    new_prim_obj = geda_line_object_new (toplevel,
                                         DETACHED_ATTRIBUTE_COLOR,
                                         x, y + 50, x, y - 50);
    new_node->component->prim_objs = g_list_prepend (new_node->component->prim_objs, new_prim_obj);

    /* Add some useful text */
    not_found_text =
      g_strdup_printf (_("Component not found:\n %1$s"),
           new_node->component_basename);
    new_prim_obj = geda_text_object_new (toplevel,
                                         DETACHED_ATTRIBUTE_COLOR,
                                         x + NOT_FOUND_TEXT_X,
                                         y + NOT_FOUND_TEXT_Y,
                                         LOWER_LEFT,
                                         0,
                                         not_found_text,
                                         8,
                                         VISIBLE, SHOW_NAME_VALUE);
    new_node->component->prim_objs = g_list_prepend (new_node->component->prim_objs, new_prim_obj);
    g_free(not_found_text);

    /* figure out where to put the hazard triangle */
    geda_text_object_calculate_bounds (toplevel,
                                       new_prim_obj,
                                       &bounds);
    x_offset = (bounds.max_x - bounds.min_x) / 4;
    y_offset = bounds.max_y - bounds.min_y + 100;  /* 100 is just an additional offset */

    /* add hazard triangle */
    new_prim_obj = geda_line_object_new (toplevel,
                                         DETACHED_ATTRIBUTE_COLOR,
                                         x + NOT_FOUND_TEXT_X + x_offset,
                                         y + NOT_FOUND_TEXT_Y + y_offset,
                                         x + NOT_FOUND_TEXT_X + x_offset + 600,
                                         y + NOT_FOUND_TEXT_Y + y_offset);
    o_set_line_options(toplevel, new_prim_obj, END_ROUND, TYPE_SOLID,
                       50, -1, -1);
    new_node->component->prim_objs = g_list_prepend (new_node->component->prim_objs, new_prim_obj);
    new_prim_obj = geda_line_object_new (toplevel,
                                         DETACHED_ATTRIBUTE_COLOR,
                                         x + NOT_FOUND_TEXT_X + x_offset,
                                         y + NOT_FOUND_TEXT_Y + y_offset,
                                         x + NOT_FOUND_TEXT_X + x_offset + 300,
                                         y + NOT_FOUND_TEXT_Y + y_offset + 500);
    o_set_line_options(toplevel, new_prim_obj, END_ROUND, TYPE_SOLID,
                       50, -1, -1);
    new_node->component->prim_objs = g_list_prepend (new_node->component->prim_objs, new_prim_obj);
    new_prim_obj = geda_line_object_new (toplevel,
                                         DETACHED_ATTRIBUTE_COLOR,
                                         x + NOT_FOUND_TEXT_X + x_offset + 300,
                                         y + NOT_FOUND_TEXT_Y + y_offset + 500,
                                         x + NOT_FOUND_TEXT_X + x_offset + 600,
                                         y + NOT_FOUND_TEXT_Y + y_offset);
    o_set_line_options(toplevel, new_prim_obj, END_ROUND, TYPE_SOLID,
                       50, -1, -1);
    new_node->component->prim_objs = g_list_prepend (new_node->component->prim_objs, new_prim_obj);
    new_prim_obj = geda_text_object_new (toplevel,
                                         DETACHED_ATTRIBUTE_COLOR,
                                         x + NOT_FOUND_TEXT_X + x_offset + 270,
                                         y + NOT_FOUND_TEXT_Y + y_offset + 90,
                                         LOWER_LEFT,
                                         0,
                                         "!",
                                         18,
                                         VISIBLE,
                                         SHOW_NAME_VALUE);
    new_node->component->prim_objs = g_list_prepend (new_node->component->prim_objs, new_prim_obj);
    new_node->component->prim_objs = g_list_reverse(new_node->component->prim_objs);
}

/* Done */
/*! \brief
 *  \par Function Description
 *
 */
OBJECT *o_component_new (TOPLEVEL *toplevel,
                         char type,
                         int color, int x, int y, int angle,
                         int mirror, const CLibSymbol *clib,
                         const gchar *basename,
                         int selectable)
{
  OBJECT *new_node=NULL;
  GList *iter;
  gchar *buffer = NULL;

  new_node = s_basic_new_object(type, "complex");

  if (clib != NULL) {
    new_node->component_basename = g_strdup (s_clib_symbol_get_name (clib));
  } else {
    new_node->component_basename = g_strdup (basename);
  }


  new_node->component_embedded = FALSE;
  new_node->color = color;
  new_node->selectable = selectable;

  new_node->component = (COMPONENT *) g_malloc(sizeof(COMPONENT));
  new_node->component->prim_objs = NULL;
  new_node->component->angle = angle;
  new_node->component->mirror = mirror;
  new_node->component->x = x;
  new_node->component->y = y;

  /* get the symbol data */
  if (clib != NULL) {
    buffer = s_clib_symbol_get_data (clib);
  }

  if (clib == NULL || buffer == NULL)
    create_placeholder(toplevel, new_node, x, y);
  else {
    GError * err = NULL;

    /* add connections till translated */
    new_node->component->prim_objs = o_read_buffer (toplevel, NULL, buffer, -1, new_node->component_basename, &err);
    if (err) {
      g_error_free(err);
      /* If reading fails, replace with placeholder object */
      create_placeholder(toplevel, new_node, x, y);
    }
    else {
      if (mirror) {
        geda_object_list_mirror (new_node->component->prim_objs, 0, 0, toplevel);
      }

      geda_object_list_rotate (new_node->component->prim_objs, 0, 0, angle, toplevel);
      geda_object_list_translate (new_node->component->prim_objs, x, y);
    }

    g_free (buffer);

  }

  /* set the parent field now */
  for (iter = new_node->component->prim_objs; iter != NULL; iter = g_list_next (iter)) {
    OBJECT *tmp = (OBJECT*) iter->data;
    tmp->parent = new_node;
  }

  return new_node;
}

/*! \brief create a new embedded object
 *  \par Function Description
 *  This function creates a new embedded object.
 *
 *  \param [in]  toplevel  The TOPLEVEL object
 *  \param [in]  type      The type of the object (usually OBJ_COMLEX)
 *  \param [in]  color     The color of the object
 *  \param [in]  x         The x location of the component object
 *  \param [in]  y         The y location of the component object
 *  \param [in]  angle     The rotation angle
 *  \param [in]  mirror    The mirror status
 *  \param [in]  basename  The basic name the embedded was created of
 *  \param [in]  selectable whether the object can be selected with the mouse
 *  \return a new component object
 */
OBJECT *o_component_new_embedded (TOPLEVEL *toplevel,
                                  char type, int color, int x, int y, int angle, int mirror,
                                  const gchar *basename, int selectable)
{
  OBJECT *new_node=NULL;

  new_node = s_basic_new_object(type, "complex");

  new_node->component = (COMPONENT *) g_malloc(sizeof(COMPONENT));
  new_node->component->x = x;
  new_node->component->y = y;

  new_node->component->angle = angle;
  new_node->component->mirror = mirror;

  new_node->component_basename = g_strdup(basename);

  new_node->component_embedded = TRUE;

  new_node->color = color;
  new_node->selectable = selectable;

  new_node->component->prim_objs = NULL;

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
 *  \param [in] toplevel     The TOPLEVEL object
 *  \param [in] buf          a text buffer (usually a line of a schematic file)
 *  \param [in] release_ver  The release number gEDA
 *  \param [in] fileformat_ver a integer value of the file format
 *  \return The object list, or NULL on error.
 */
OBJECT *o_component_read (TOPLEVEL *toplevel,
                          const char buf[], unsigned int release_ver,
                          unsigned int fileformat_ver, GError **err)
{
  OBJECT *new_obj;
  char type;
  int x1, y1;
  int angle;

  char *basename = (char*) g_malloc (1 + strlen (buf));

  int selectable;
  int mirror;

  if (sscanf(buf, "%c %d %d %d %d %d %s\n",
	     &type, &x1, &y1, &selectable, &angle, &mirror, basename) != 7) {
    g_set_error(err, EDA_ERROR, EDA_ERROR_PARSE, _("Failed to parse component object"));
    return NULL;
  }

  switch(angle) {

    case(0):
    case(90):
    case(180):
    case(270):
      break;

    default:
      s_log_message(_("Found a component with an invalid rotation "
                      "[ %1$c %2$d %3$d %4$d %5$d %6$d %7$s ]"),
                    type, x1, y1, selectable, angle, mirror, basename);
      s_log_message (_("Setting angle to 0."));
      angle = 0;
  }

  switch(mirror) {

    case(0):
    case(1):

      break;

    default:
      s_log_message(_("Found a component with an invalid mirror flag "
                      "[ %1$c %2$d %3$d %4$d %5$d %6$d %7$s ]"),
                    type, x1, y1, selectable, angle, mirror, basename);
      s_log_message (_("Setting mirror to 0."));
      mirror = 0;
  }
  if (strncmp(basename, "EMBEDDED", 8) == 0) {

    new_obj = o_component_new_embedded (toplevel, type,
                                        DEFAULT_COLOR, x1, y1, angle, mirror,
                                        basename + 8,
                                        selectable);
  } else {

    const CLibSymbol *clib = s_clib_get_symbol_by_name (basename);

    new_obj = o_component_new (toplevel, type,
                               DEFAULT_COLOR,
                               x1, y1,
                               angle, mirror, clib,
                               basename, selectable);
    /* Delete or hide attributes eligible for promotion inside the
       component. */
    if (new_obj)
      o_component_remove_promotable_attribs (toplevel, new_obj);
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
 *  \param [in] object  a component OBJECT
 *  \return the string representation of the component OBJECT
 */
gchar*
geda_component_object_to_buffer (const GedaObject *object)
{
  gchar *basename;
  gchar *buffer;

  g_return_val_if_fail (object != NULL, NULL);
  g_return_val_if_fail (object->component != NULL, NULL);
  g_return_val_if_fail ((object->type == OBJ_COMPONENT) ||
                        (object->type == OBJ_PLACEHOLDER), NULL);

  basename = g_strdup_printf ("%s%s",
                              object->component_embedded ? "EMBEDDED" : "",
                              object->component_basename);

  /* We force the object type to be output as OBJ_COMPONENT for both these object
   * types.
   */
  buffer = g_strdup_printf ("%c %d %d %d %d %d %s",
                            OBJ_COMPONENT,
                            object->component->x,
                            object->component->y,
                            geda_object_get_selectable (object),
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
 *  \param [ref] object  The component GedaObject to be moved
 *  \param [in]  dx      The x-distance to move the object
 *  \param [in]  dy      The y-distance to move the object
 */
void
geda_component_object_translate (GedaObject *object, int dx, int dy)
{
  g_return_if_fail (object != NULL &&
                    (object->type == OBJ_COMPONENT ||
                     object->type == OBJ_PLACEHOLDER));

  object->component->x = object->component->x + dx;
  object->component->y = object->component->y + dy;

  geda_object_list_translate (object->component->prim_objs, dx, dy);
}

/*! \brief Create a copy of a component object
 *  \par Function Description
 *  This function creates a copy of the component object \a
 *  o_current.
 *
 *  \param [in] toplevel     The TOPLEVEL object
 *  \param [in] o_current    The object that is copied
 *  \return a new component object
 */
OBJECT *o_component_copy(TOPLEVEL *toplevel, OBJECT *o_current)
{
  OBJECT *o_new;
  GList *iter;

  g_return_val_if_fail(o_current != NULL, NULL);

  o_new = s_basic_new_object(o_current->type, "complex");
  o_new->color = o_current->color;
  o_new->selectable = o_current->selectable;
  o_new->component_basename = g_strdup(o_current->component_basename);
  o_new->component_embedded = o_current->component_embedded;

  o_new->component = (COMPONENT*) g_malloc0 (sizeof (COMPONENT));
  o_new->component->x = o_current->component->x;
  o_new->component->y = o_current->component->y;
  o_new->component->angle = o_current->component->angle;
  o_new->component->mirror = o_current->component->mirror;

  /* Copy contents and set the parent pointers on the copied objects. */
  o_new->component->prim_objs =
    o_glist_copy_all (toplevel, o_current->component->prim_objs,
                      NULL);

  for (iter = o_new->component->prim_objs;
       iter != NULL;
       iter = g_list_next (iter)) {
    ((OBJECT*) iter->data)->parent = o_new;
  }

  /* Delete or hide attributes eligible for promotion inside the
     component. */
  o_component_remove_promotable_attribs (toplevel, o_new);

  s_slot_update_object (toplevel, o_new);

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
 *  \param [in]      toplevel  The toplevel environment.
 *  \param [in]      centerx   X coordinate of rotation center (world coords).
 *  \param [in]      centery   Y coordinate of rotation center (world coords).
 *  \param [in]      angle     Rotation angle in degrees.
 *  \param [in,out]  object    Component object to rotate.
 */
void
geda_component_object_rotate (TOPLEVEL *toplevel,
                              int centerx, int centery,
                              int angle, OBJECT *object)
{
  int x, y;
  int newx, newy;

  g_return_if_fail (object!=NULL);
  g_return_if_fail ((object->type == OBJ_COMPONENT) ||
                    (object->type == OBJ_PLACEHOLDER));

  x = object->component->x + (-centerx);
  y = object->component->y + (-centery);

  geda_point_rotate_90 (x, y, angle, &newx, &newy);

  x = newx + (centerx);
  y = newy + (centery);

  geda_component_object_translate (object, -object->component->x, -object->component->y);

  geda_object_list_rotate (object->component->prim_objs, 0, 0, angle, toplevel);

  object->component->x = 0;
  object->component->y = 0;

  geda_component_object_translate (object, x, y);

  object->component->angle = ( object->component->angle + angle ) % 360;
}


/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void
geda_component_object_mirror (TOPLEVEL *toplevel,
                              int world_centerx, int world_centery,
                              OBJECT *object)
{
  int x, y;

  g_return_if_fail( object != NULL );
  g_return_if_fail( (object->type == OBJ_COMPONENT ||
                     object->type == OBJ_PLACEHOLDER) );
  g_return_if_fail( object->component != NULL );

  x = 2 * world_centerx - object->component->x;
  y = object->component->y;

  geda_component_object_translate (object, -object->component->x, -object->component->y);

  geda_object_list_mirror (object->component->prim_objs, 0, 0, toplevel);

  switch(object->component->angle) {
    case(90):
      object->component->angle = 270;
      break;

    case(270):
      object->component->angle = 90;
      break;

  }

  object->component->mirror = !object->component->mirror;

  geda_component_object_translate (object, x, y);
}


/*! \brief Find a pin with a particular attribute.
 *  \par Function Description
 *  Search for a pin inside the given component which has an attribute
 *  matching those passed.
 *
 *  \param [in] object        component OBJECT whos pins to search.
 *  \param [in] name          the attribute name to search for.
 *  \param [in] wanted_value  the attribute value to search for.
 *  \return The pin OBJECT with the given attribute, NULL otherwise.
 */
OBJECT*
o_component_find_pin_by_attribute (OBJECT *object,
                                   const char *name,
                                   char *wanted_value)
{
  GList *iter;
  OBJECT *o_current;
  char *value;
  int found;

  g_return_val_if_fail (object != NULL, NULL);
  g_return_val_if_fail (object->type == OBJ_COMPONENT ||
                        object->type == OBJ_PLACEHOLDER, NULL);

  for (iter = object->component->prim_objs; iter != NULL;
       iter = g_list_next (iter)) {
    o_current = (OBJECT*) iter->data;

    if (o_current->type != OBJ_PIN)
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
 *  Major symversion changes are added to the toplevel object
 *  (toplevel->major_changed_refdes), minor changes are reported
 *  to the messaging system.
 *
 *  \param toplevel  The TOPLEVEL object
 *  \param object    The component OBJECT
 */
void
o_component_check_symversion (TOPLEVEL* toplevel, OBJECT* object)
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

  g_return_if_fail (object != NULL);
  g_return_if_fail ((object->type == OBJ_COMPONENT ||
		     object->type == OBJ_PLACEHOLDER));
  g_return_if_fail (object->component != NULL);

  /* first look on the inside for the symversion= attribute */
  inside = o_attrib_search_inherited_attribs_by_name (object, "symversion", 0);

  /* now look for the symversion= attached to object */
  outside = o_attrib_search_attached_attribs_by_name (object, "symversion", 0);

  /* get the uref for future use */
  refdes = o_attrib_search_object_attribs_by_name(object, "refdes", 0);
  if (!refdes)
  {
    refdes = g_strdup ("unknown");
  }

  if (inside)
  {
    inside_value = strtod(inside, &err_check);
    if (inside_value == 0 && inside == err_check)
    {
      if (inside)
      {
        s_log_message(_("WARNING: Symbol version parse error on refdes %1$s:\n"
                        "\tCould not parse symbol file symversion=%2$s"),
                      refdes, inside);
      } else {
        s_log_message(_("WARNING: Symbol version parse error on refdes %1$s:\n"
                        "\tCould not parse symbol file symversion="),
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
      s_log_message(_("WARNING: Symbol version parse error on refdes %1$s:\n"
                      "\tCould not parse attached symversion=%2$s"),
                    refdes, outside);
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
    s_log_message(_("WARNING: Symbol version oddity on refdes %1$s:\n"
                    "\tsymversion=%2$s attached to instantiated symbol, "
                    "but no symversion= inside symbol file."),
                  refdes, outside);
    goto done;
  }

  /* inside & not outside is a valid case, means symbol in library is newer */
  /* also if inside_value is greater than outside_value, then symbol in */
  /* library is newer */
  if ((inside_present && !outside_present) ||
      ((inside_present && outside_present) && (inside_value > outside_value)))
  {

    s_log_message(_("WARNING: Symbol version mismatch on refdes %1$s (%2$s):\n"
                    "\tSymbol in library is newer than "
                    "instantiated symbol."),
                  refdes, object->component_basename);

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
      s_log_message(_("\tMAJOR VERSION CHANGE (file %1$.3f, "
                      "instantiated %2$.3f, %3$s)!"),
                    inside_value, outside_value, refdes);

      /* add the refdes to the major_changed_refdes GList */
      /* make sure refdes_copy is freed somewhere */
      refdes_copy = g_strconcat (refdes, " (",
                                 object->component_basename,
                                 ")", NULL);
      toplevel->major_changed_refdes =
        g_list_append(toplevel->major_changed_refdes, refdes_copy);

      /* don't bother checking minor changes if there are major ones*/
      goto done;
    }

    if (inside_minor > outside_minor)
    {
      s_log_message(_("\tMinor version change (file %1$.3f, "
                      "instantiated %2$.3f)"),
                    inside_value, outside_value);
    }

    goto done;
  }

  /* outside value is greater than inside value, this is weird case */
  if ((inside_present && outside_present) && (outside_value > inside_value))
  {
    s_log_message(_("WARNING: Symbol version oddity on refdes %1$s:\n"
                    "\tInstantiated symbol is newer than "
                    "symbol in library."),
                  refdes);
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
 *  \param [in] toplevel     The TOPLEVEL object.
 *  \param [in] object       The component OBJECT.
 *  \param [in] x            The x coordinate of the given point.
 *  \param [in] y            The y coordinate of the given point.
 *  \param [in] force_solid  If true, force treating the object as solid.
 *  \return The shortest distance from the object to the point. If the
 *  distance cannot be calculated, this function returns a really large
 *  number (G_MAXDOUBLE).  With an invalid parameter, this function returns
 *  G_MAXDOUBLE.
 */
double
geda_component_object_shortest_distance (TOPLEVEL *toplevel, OBJECT *object,
                                         int x, int y, int force_solid)
{
  double shortest_distance = G_MAXDOUBLE;
  double distance;
  int found_line_bounds = 0;
  BOX line_bounds;
  GList *iter;

  g_return_val_if_fail (object->component != NULL, G_MAXDOUBLE);

  for (iter = object->component->prim_objs;
       iter != NULL; iter= g_list_next (iter)) {
    OBJECT *obj = (OBJECT*) iter->data;
    int left, top, right, bottom;

    /* Collect the bounds of any lines and arcs in the symbol */
    if ((obj->type == OBJ_LINE || obj->type == OBJ_ARC) &&
        geda_object_calculate_visible_bounds(toplevel, obj,
                                       &left, &top, &right, &bottom)) {
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
      distance = geda_object_shortest_distance_full (toplevel, obj, x, y, TRUE);
      shortest_distance = MIN (shortest_distance, distance);
    }

    if (shortest_distance == 0.0)
      return shortest_distance;
  }

  if (found_line_bounds) {
    distance = geda_box_shortest_distance (&line_bounds, x, y, TRUE);
    shortest_distance = MIN (shortest_distance, distance);
  }

  return shortest_distance;
}
