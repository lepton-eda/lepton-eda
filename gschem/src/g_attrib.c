/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 * Copyright (C) 2011 Peter Brett <peter@peter-b.co.uk>
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

/*!
 * \file g_attrib.c
 * \brief Scheme API functions for manipulating attributes in
 * gschem-specific ways.
 */

#include <config.h>

#include "gschem.h"

SCM_SYMBOL (name_sym , "name");
SCM_SYMBOL (value_sym , "value");
SCM_SYMBOL (both_sym , "both");
SCM_SYMBOL (object_state_sym, "object-state");

/*! \brief Add an attribute to an object, or floating.
 * \par Function Description
 * Creates a new attribute, either attached to an object or floating.
 *
 * The \a name_s and \a value_s should be strings.  If \a visible_s is
 * false, the new attribute will be invisible; otherwise it will be
 * visible.  \a show_s determines which parts of an
 * attribute-formatted string should be shown, and should be one of
 * the symbols "name", "value" or "both".
 *
 * If \a target_s is specified and is a gEDA object, the new attribute
 * will be attached to it. If \a target_s is not in gschem's active
 * page, an "object-state" error will be raised.
 *
 * If \a target_s is #f, the new attribute will be floating in
 * gschem's current active page.
 *
 * \note Scheme API: Implements the %add-attrib! procedure in the
 * (gschem core attrib) module.
 *
 * \bug This function does not verify that \a name_s is actually a
 * valid attribute name.
 *
 * \todo It would be nice to support pages other than the current
 * active page.
 *
 * \param target_s  where to attach the new attribute.
 * \param name_s    name for the new attribute.
 * \param value_s   value for the new attribute.
 * \param visible_s visibility of the new attribute (true or false).
 * \param show_s    the attribute part visibility setting.
 *
 * \return the newly created text object.
 */
SCM_DEFINE (add_attrib_x, "%add-attrib!", 5, 0, 0,
            (SCM target_s, SCM name_s, SCM value_s, SCM visible_s, SCM show_s),
            "Add an attribute to an object, or floating")
{
  SCM_ASSERT ((edascm_is_page (target_s) ||
               edascm_is_object (target_s) ||
               scm_is_false (target_s)),
              target_s, SCM_ARG1, s_add_attrib_x);
  SCM_ASSERT (scm_is_string (name_s), name_s, SCM_ARG2, s_add_attrib_x);
  SCM_ASSERT (scm_is_string (value_s), value_s, SCM_ARG3, s_add_attrib_x);
  SCM_ASSERT (scm_is_symbol (show_s), show_s, SCM_ARG5, s_add_attrib_x);

  GschemToplevel *w_current = g_current_window ();
  TOPLEVEL *toplevel = gschem_toplevel_get_toplevel (w_current);

  /* Check target object, if present */
  OBJECT *obj = NULL;
  if (edascm_is_object (target_s)) {
    obj = edascm_to_object (target_s);
    if (o_get_page (toplevel, obj) != toplevel->page_current) {
      scm_error (object_state_sym,
                 s_add_attrib_x,
                 _("Object ~A is not included in the current gschem page."),
                 scm_list_1 (target_s), SCM_EOL);
    }
  }

  /* Visibility */
  int visibility;
  if (scm_is_false (visible_s)) {
    visibility = INVISIBLE;
  } else {
    visibility = VISIBLE;
  }

  /* Name/value visibility */
  int show;
  if      (scm_is_eq (show_s, name_sym))  { show = SHOW_NAME;       }
  else if (scm_is_eq (show_s, value_sym)) { show = SHOW_VALUE;      }
  else if (scm_is_eq (show_s, both_sym))  { show = SHOW_NAME_VALUE; }
  else {
    scm_misc_error (s_add_attrib_x,
                    _("Invalid text name/value visibility ~A."),
                    scm_list_1 (show_s));
  }


  scm_dynwind_begin (0);

  char *name;
  name = scm_to_utf8_string (name_s);
  scm_dynwind_free (name);

  char *value;
  value = scm_to_utf8_string (value_s);
  scm_dynwind_free (value);

  gchar *str = g_strdup_printf ("%s=%s", name, value);
  scm_dynwind_unwind_handler (g_free, str, SCM_F_WIND_EXPLICITLY);

  OBJECT *result = o_attrib_add_attrib (w_current, str, visibility, show, obj);

  scm_dynwind_end ();

  return edascm_from_object (result);
}

/*!
 * \brief Create the (gschem core attrib) Scheme module.
 * \par Function Description
 * Defines procedures in the (gschem core attrib) module. The module can
 * be accessed using (use-modules (gschem core attrib)).
 */
static void
init_module_gschem_core_attrib ()
{
  /* Register the functions and symbols */
  #include "g_attrib.x"

  /* Add them to the module's public definitions. */
  scm_c_export (s_add_attrib_x, NULL);
}

/*!
 * \brief Initialise the gschem attribute procedures.
 * \par Function Description

 * Registers some Scheme procedures for working with
 * attributes. Should only be called by main_prog().
 */
void
g_init_attrib ()
{
  /* Define the (gschem core attrib) module */
  scm_c_define_module ("gschem core attrib",
                       init_module_gschem_core_attrib,
                       NULL);
}
