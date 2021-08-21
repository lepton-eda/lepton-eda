/* Lepton EDA attribute editor
 * Copyright (C) 2003-2010 Stuart D. Brorson.
 * Copyright (C) 2003-2016 gEDA Contributors
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

/*------------------------------------------------------------------*/
/*! \file
 * \brief Functions for manipulating LeptonObjects.
 *
 * This file holds functions involved in manipulating the
 * LeptonObject data structure.  LeptonObject is defined in
 * libgeda.  An LeptonObject is a graphical primitive normally
 * used in gschem.  Example LeptonObjects: some text, a component, a
 * pin, a line, etc.
 *
 * The functions herein are functions which I wrote as wrappers to the
 * fcns in libgeda.
 */

#include <config.h>

#include <stdio.h>
#ifdef HAVE_STRING_H
#include <string.h>
#endif
#include <math.h>

/*------------------------------------------------------------------
 * Gattrib specific includes
 *------------------------------------------------------------------*/
#include <liblepton/liblepton.h>
#include "../include/struct.h"     /* typdef and struct declarations */
#include "../include/prototype.h"  /* function prototypes */
#include "../include/globals.h"
#include "../include/gettext.h"


/*------------------------------------------------------------------
 * Gattrib specific defines
 *------------------------------------------------------------------*/
#define DEFAULT_TEXT_SIZE 10

static LeptonObject*
s_object_attrib_add_attrib_in_object (LeptonToplevel *toplevel,
                                      char *text_string,
                                      gint visibility,
                                      gint show_name_value,
                                      LeptonObject * object);

/* ===================  Public Functions  ====================== */

/*------------------------------------------------------------------*/
/*! \brief Add an attribute to an LeptonObject
 *
 * This fcn adds a new attrib to o_current, when o_current is a
 * component.  It does it in the following
 * way:
 * -# It creates an object -- "attrib_graphic" -- and fills it in.
 * -# It gets the position info from o_current's refdes attrib and
 *    calls o_text_new() to add position info and name=value string
 *    to attrib_graphic.
 * -# It calls o_attrib_add() to wrap attrib_graphic with (attribute LeptonObject )
 * \param toplevel LeptonToplevel structure
 * \param o_current pointer to object to add attribute to
 * \param new_attrib_name name of the attribute to add
 * \param new_attrib_value value of the attribute to add
 * \param visibility Is the attribute visible?
 * \param show_name_value Control visibility of name and value.
 */
void
s_object_add_comp_attrib_to_object (LeptonToplevel *toplevel,
                                    LeptonObject *o_current,
                                    char *new_attrib_name,
                                    char *new_attrib_value,
                                    gint visibility,
                                    gint show_name_value)
{
  char *name_value_pair;
  g_return_if_fail (o_current != NULL);

  /* One last sanity check, then add attrib */
  if (strlen(new_attrib_value) != 0) {
    name_value_pair = g_strconcat(new_attrib_name, "=", new_attrib_value, NULL);
    s_object_attrib_add_attrib_in_object (toplevel,
                                          name_value_pair,
                                          visibility,
                                          show_name_value,
                                          o_current);
  }

  return;

}


/*------------------------------------------------------------------*/
/*!
 * \todo This needs to be filled in.
 */
void
s_object_add_net_attrib_to_object (LeptonToplevel *toplevel,
                                   LeptonObject *o_current,
                                   char *new_attrib_name,
                                   char *new_attrib_value)
{
  /* TBD */
}


/*------------------------------------------------------------------*/
/*! \brief Add a new attribute to an pin LeptonObject
 *
 * Add a new attribute to o_current, when o_current is a
 * pin.  It does it in the following
 * way:
 * -# It creates an object -- "attrib_graphic" -- and fills it in.
 * -# It gets the position info from o_current's refdes attrib and
 *    calls o_text_new() to add position info and name=value string
 *    to attrib_graphic.
 * -# It calls o_attrib_add() to wrap attrib_graphic with (attribute
      LeptonObject)
 * \param toplevel LeptonToplevel structure
 * \param o_current Pointer to pin object
 * \param new_attrib_name Name of attribute to add
 * \param new_attrib_value Value of attribute to add
 * \todo Do I really need separate fcns for comps, nets, and
 * pins???
 */
void
s_object_add_pin_attrib_to_object (LeptonToplevel *toplevel,
                                   LeptonObject *o_current,
                                   char *new_attrib_name,
                                   char *new_attrib_value)
{
  char *name_value_pair;
  g_return_if_fail (o_current != NULL);

  /* One last sanity check */
  if (strlen(new_attrib_value) != 0) {
    name_value_pair = g_strconcat(new_attrib_name, "=", new_attrib_value, NULL);
    s_object_attrib_add_attrib_in_object (toplevel,
                                          name_value_pair,
                                          INVISIBLE,
                                          SHOW_NAME_VALUE,
                                          o_current);
  }

  return;
}


/*------------------------------------------------------------------*/
/*! \brief Replace attribute value in object
 *
 * Find the instance of attrib_name on o_current, and
 * replace its value with the new_attrib_value.
 *
 * \param o_current object to operate on
 * \param new_attrib_name name of attribute to replace
 * \param new_attrib_value value to set attribute to
 * \param visibility set visibility of attribute
 * \param show_name_value set visibility of attribute name and value
 */
void
s_object_replace_attrib_in_object(LeptonObject *o_current,
                                  char *new_attrib_name,
                                  char *new_attrib_value,
                                  gint visibility,
                                  gint show_name_value)
{
  GList *a_iter;
  LeptonObject *a_current;
  char *old_attrib_text;
  char *old_attrib_name;
  char *new_attrib_text;


  a_iter = lepton_object_get_attribs (o_current);
  while (a_iter != NULL) {
    a_current = (LeptonObject*) a_iter->data;
    if (lepton_object_is_text (a_current)
        && a_current->text != NULL) {  /* found an attribute */

      /* may need to check more thoroughly here. . . . */
      old_attrib_text = g_strdup (lepton_text_object_get_string (a_current));
      old_attrib_name = u_basic_breakup_string(old_attrib_text, '=', 0);

      if (strcmp(old_attrib_name, new_attrib_name) == 0) {
        /* create attrib=value text string */
        new_attrib_text = g_strconcat(new_attrib_name, "=", new_attrib_value, NULL);
        lepton_text_object_set_string (a_current, new_attrib_text);
        if (visibility != LEAVE_VISIBILITY_ALONE)
          lepton_text_object_set_visibility (a_current, visibility);
        if (show_name_value != LEAVE_NAME_VALUE_ALONE)
          lepton_text_object_set_show (a_current, show_name_value);
        g_free(new_attrib_text);
        g_free(old_attrib_text);
        g_free(old_attrib_name);
        return;     /* we are done -- leave. */
      } else {
        g_free(old_attrib_text);
        g_free(old_attrib_name);
      }  /* if (strcmp . . . . */
    } /* if (a_current . . . . */

    a_iter = g_list_next (a_iter);
  }  /* while */

  /* if we get here, it's because we have failed to find the attrib on the component.
   * This is an error condition. */
  fprintf (stderr, "s_object_replace_attrib_in_object: ");
  fprintf (stderr,
          _("Failed to find the attrib %1$s on the component.\n"),
          new_attrib_name);
  exit(-1);
}


/*------------------------------------------------------------------*/
/*!
 * \brief Remove attribute from object
 *
 * Remove an attribute from an object.
 * \param toplevel LeptonToplevel structure
 * \param o_current Object to remove attribute from
 * \param new_attrib_name Name of attribute to remove
 */
void
s_object_remove_attrib_in_object (LeptonToplevel *toplevel,
                                  LeptonObject *o_current,
                                  char *new_attrib_name)
{
  GList *a_iter;
  LeptonObject *a_current;
  LeptonObject *attribute_object;
  char *old_attrib_text;
  char *old_attrib_name;

  a_iter = lepton_object_get_attribs (o_current);
  while (a_iter != NULL) {
    a_current = (LeptonObject*) a_iter->data;
    if (lepton_object_is_text (a_current)
        && a_current->text != NULL) {  /* found an attribute */

      /* may need to check more thoroughly here. . . . */
      old_attrib_text = g_strdup (lepton_text_object_get_string (a_current));
      old_attrib_name = u_basic_breakup_string(old_attrib_text, '=', 0);

      if (strcmp(old_attrib_name, new_attrib_name) == 0) {
        /* We've found the attrib.  Delete it and then return. */

        g_debug ("s_object_remove_attrib_in_object: "
                 "Removing attrib with name = %1$s\n", old_attrib_name);

        attribute_object = a_current;
        s_object_delete_text_object_in_object (toplevel, attribute_object);

        g_free(old_attrib_text);
        g_free(old_attrib_name);
        return;     /* we are done -- leave. */
      }
    g_free(old_attrib_text);
    g_free(old_attrib_name);
    }
    a_iter = g_list_next (a_iter);
  }

  /* if we get here, it's because we have failed to find the attrib on the component.
   * This is an error condition. */
  fprintf (stderr, "s_object_remove_attrib_in_object: ");
  fprintf (stderr,
           _("Failed to find the attrib %1$s on the component.\n"),
           new_attrib_name);
  exit(-1);
}



/*------------------------------------------------------------------*/
/*! \brief Attach attribute to object.
 *
 * Attach the name=value pair to the LeptonObject "object". This function
 * was stolen from gschem/src/o_attrib.c:o_attrib_add_attrib and
 * hacked for gattrib.
 * \param toplevel LeptonToplevel to operate on
 * \param text_string
 * \param visibility
 * \param show_name_value
 * \param o_current
 * \returns pointer to the object
 * \todo Does it need to return LeptonObject?
 */
static LeptonObject *
s_object_attrib_add_attrib_in_object (LeptonToplevel *toplevel,
                                      char *text_string,
                                      int visibility,
                                      int show_name_value,
                                      LeptonObject * o_current)
{
  int world_x = -1, world_y = -1;
  int color;
  LeptonObject *new_obj;

  g_return_val_if_fail ((o_current != NULL), NULL);

  /* creating a toplevel or unattached attribute */
  /* get coordinates of where to place the text object */
  switch (lepton_object_get_type (o_current)) {
  case (OBJ_COMPONENT):
    world_x = lepton_component_object_get_x (o_current);
    world_y = lepton_component_object_get_y (o_current);
    color = ATTRIBUTE_COLOR;
    break;

  case (OBJ_NET):
    world_x = lepton_component_object_get_x (o_current);
    world_y = lepton_component_object_get_y (o_current);
    color = ATTRIBUTE_COLOR;
    break;

  default:
    fprintf (stderr, "s_object_attrib_add_attrib_in_object: ");
    fprintf (stderr, _("Trying to add attrib to non-component or non-net!\n"));
    exit(-1);
  }

  /* first create text item */
  g_debug ("s_object_attrib_add_attrib_in_object: "
           "About to attach new text attrib with properties:\n"
           "     color = %d\n"
           "     text_string = %s\n"
           "     visibility = %d\n"
           "     show_name_value = %d\n",
           color, text_string, visibility, show_name_value);

  new_obj = lepton_text_object_new (color,
                                    world_x,
                                    world_y,
                                    LOWER_LEFT,
                                    0, /* zero is angle */
                                    text_string,
                                    DEFAULT_TEXT_SIZE,
                                    visibility,
                                    show_name_value);
  s_page_append (toplevel->page_current, new_obj);

  /* now toplevel->page_current->object_tail contains new text item */

  /* now attach the attribute to the object */
  /* remember that o_current contains the object to get the attribute */
  o_attrib_attach (new_obj, o_current, FALSE);

  o_selection_add (toplevel->page_current->selection_list, new_obj);


  toplevel->page_current->CHANGED = 1;

  return new_obj;
}




/*------------------------------------------------------------------*/
/*! \brief Delete text object
 *
 * Delete the text object pointed to by text_object.  This function
 * was shamelessly stolen from gschem/src/o_delete.c and hacked
 * for gattrib by SDB.
 * \param toplevel LeptonToplevel to be operated on
 * \param test_object text object to be deleted
 */
void
s_object_delete_text_object_in_object (LeptonToplevel *toplevel,
                                       LeptonObject * text_object)
{
  s_page_remove (toplevel->page_current, text_object);
  lepton_object_delete (text_object);
  toplevel->page_current->CHANGED = 1;
}


/*------------------------------------------------------------------*/
/*! \brief Ensure object has a symbol file
 *
 * This verifies that the object has a non-null symbol file.
 *
 * \returns 0 = valid symbol file, 1 = no symbol file found.
 */
int s_object_has_sym_file(LeptonObject *object)
{
  char *filename;

  filename = object->component_basename;
  if (filename != NULL) {
    g_debug ("s_object_has_sym_file: Object has sym file = %s.\n", filename);
    return 0;
  } else {
    g_debug ("s_object_has_sym_file: Found object with no attached symbol file.\n");
    return 1;
  }
}
