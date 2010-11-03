/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library - Scheme API
 * Copyright (C) 2010 Peter Brett <peter@peter-b.co.uk>
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
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111 USA
 */

/*!
 * \file scheme_object.c
 * \brief Scheme API object manipulation procedures.
 */

#include <config.h>

#include "libgeda_priv.h"
#include "libgedaguile_priv.h"

SCM_SYMBOL (wrong_type_arg_sym , "wrong-type-arg");
SCM_SYMBOL (line_sym , "line");
SCM_SYMBOL (net_sym , "net");
SCM_SYMBOL (bus_sym , "bus");
SCM_SYMBOL (box_sym , "box");
SCM_SYMBOL (picture_sym , "picture");
SCM_SYMBOL (circle_sym , "circle");
SCM_SYMBOL (complex_sym , "complex");
SCM_SYMBOL (text_sym , "text");
SCM_SYMBOL (path_sym , "path");
SCM_SYMBOL (pin_sym , "pin");
SCM_SYMBOL (arc_sym , "arc");

/*! \brief Convert a Scheme object list to a GList.
 * \par Function Description
 * Takes a Scheme list of #OBJECT smobs, and returns a GList
 * containing the objects. If \a objs is not a list of #OBJECT smobs,
 * throws a Scheme error.
 *
 * \warning If the #OBJECT structures in the GList are to be stored by
 * C code and later free()'d directly, the smobs must be marked as
 * unsafe for garbage collection (by calling edascm_c_set_gc()).
 *
 * \param [in] objs a Scheme list of #OBJECT smobs.
 * \param [in] subr the name of the Scheme subroutine (used for error
 *                  messages).
 * \return a #GList of #OBJECT.
 */
GList *
edascm_to_object_glist (SCM objs, const char *subr)
{
  GList *result = NULL;
  SCM lst;

  SCM_ASSERT (scm_is_true (scm_list_p (objs)), objs, SCM_ARGn, subr);

  scm_dynwind_begin (0);
  scm_dynwind_unwind_handler ((void (*)(void *))g_list_free, result, 0);

  for (lst = objs; lst != SCM_EOL; lst = SCM_CDR (lst)) {
    SCM smob = SCM_CAR (lst);
    EDASCM_ASSERT_SMOB_VALID (smob);
    if (!EDASCM_OBJECTP (smob)) {
      scm_error_scm (wrong_type_arg_sym,
                     scm_from_locale_string (subr),
                     scm_from_locale_string (_("Expected a gEDA object, found ~A")),
                     scm_list_1 (smob), scm_list_1 (smob));
    }
    result = g_list_prepend (result, (gpointer) edascm_to_object (smob));
  }

  scm_remember_upto_here_1 (lst);

  scm_dynwind_end ();

  return g_list_reverse (result);
}

/*! \brief Convert a GList of objects into a Scheme list.
 * \par Function Description
 * Takes a GList of #OBJECT and returns a Scheme list of corresponding
 * object smobs.
 *
 * \warning If the #OBJECT structures are to be subsequently managed
 * only by Scheme, the smobs in the returned list must be marked as
 * safe for garbage collection (by calling edascm_c_set_gc()).
 *
 * \param [in] objs a #GList of #OBJECT instances.
 * \return a Scheme list of smobs corresponding to each #OBJECT.
 */
SCM
edascm_from_object_glist (const GList *objs)
{
  SCM lst = SCM_EOL;
  SCM rlst;
  GList *iter = (GList *) objs;

  while (iter != NULL) {
    lst = scm_cons (edascm_from_object (iter->data), lst);
    iter = g_list_next (iter);
  }

  rlst = scm_reverse (lst);

  scm_remember_upto_here_1 (lst);
  return rlst;
}

/*! \brief Test if an object smob is of a particular type.
 * \par Function Description
 * Checks if \a smob contains an #OBJECT of the given \a type. This is
 * intended to be used by C-based Scheme procedures for working with
 * particular object types.
 *
 * \param [in] smob Scheme value to check type for.
 * \param [in] type Type to check against (e.g. OBJ_LINE).
 * \return non-zero if \a smob is an #OBJECT smob of \a type.
 */
int
edascm_is_object_type (SCM smob, int type)
{
  if (!EDASCM_OBJECTP(smob)) return 0;

  OBJECT *obj = edascm_to_object (smob);
  return (obj->type == type);
}

/*! \brief Copy an object.
 * \par Function Description
 * Returns a copy of the #OBJECT contained in smob \a obj_s as a new
 * smob.
 *
 * \note Scheme API: Implements the %copy-object procedure in the
 * (geda core object) module.
 *
 * \param [in] obj_s an #OBJECT smob.
 * \return a new #OBJECT smob containing a copy of the #OBJECT in \a obj_s.
 */
SCM_DEFINE (copy_object, "%copy-object", 1, 0, 0,
            (SCM obj_s), "Copy an object.")
{
  SCM result;
  SCM_ASSERT (EDASCM_OBJECTP (obj_s), obj_s,
              SCM_ARG1, s_copy_object);

  TOPLEVEL *toplevel = edascm_c_current_toplevel ();
  OBJECT *obj = edascm_to_object (obj_s);

  result = edascm_from_object (o_object_copy (toplevel, obj));

  /* At the moment, the only pointer to the object is owned by the
   * smob. */
  edascm_c_set_gc (result, TRUE);

  return result;
}

/*! \brief Get the type of an object.
 * \par Function Description
 * Returns a symbol describing the type of the #OBJECT smob \a obj_s.
 *
 * \note Scheme API: Implements the %object-type procedure in the
 * (geda core object) module.
 *
 * \param [in] obj_s an #OBJECT smob.
 * \return a Scheme symbol representing the object type.
 */
SCM_DEFINE (object_type, "%object-type", 1, 0, 0,
            (SCM obj_s), "Get an object smob's type")
{
  SCM result;

  SCM_ASSERT (EDASCM_OBJECTP (obj_s), obj_s,
              SCM_ARG1, s_object_type);

  OBJECT *obj = edascm_to_object (obj_s);
  switch (obj->type) {
  case OBJ_LINE:    result = line_sym;       break;
  case OBJ_NET:     result = net_sym;        break;
  case OBJ_BUS:     result = bus_sym;        break;
  case OBJ_BOX:     result = box_sym;        break;
  case OBJ_PICTURE: result = picture_sym;    break;
  case OBJ_CIRCLE:  result = circle_sym;     break;
  case OBJ_PLACEHOLDER:
  case OBJ_COMPLEX: result = complex_sym;    break;
  case OBJ_TEXT:    result = text_sym;       break;
  case OBJ_PATH:    result = path_sym;       break;
  case OBJ_PIN:     result = pin_sym;        break;
  case OBJ_ARC:     result = arc_sym;        break;
  default:
    g_critical ("o_mirror_world: object %p has bad type '%c'\n",
                obj, obj->type);
    result = SCM_BOOL_F;
  }

  return result;
}

/*! \brief Get the color of an object.
 * \par Function Description
 * Returns the colormap index of the color used to draw the #OBJECT
 * smob \a obj_s. Note that the color may not be meaningful for some
 * object types.
 *
 * \note Scheme API: Implements the %object-color procedure in the
 * (geda core object) module.
 *
 * \param [in] obj_s #OBJECT smob to inspect.
 * \return The colormap index used by \a obj_s.
 */
SCM_DEFINE (object_color, "%object-color", 1, 0, 0,
            (SCM obj_s), "Get the color of an object.")
{
  SCM_ASSERT (EDASCM_OBJECTP (obj_s), obj_s,
              SCM_ARG1, s_object_color);

  OBJECT *obj = edascm_to_object (obj_s);
  return scm_from_int (obj->color);
}

/*! \brief Set the color of an object.
 * \par Function Description
 * Set the colormap index of the color used to draw the #OBJECT smob
 * \a obj_s to \a color_s. Note that the color may not be meaningful
 * for some object types.
 *
 * \note Scheme API: Implements the %set-object-color! procedure in
 * the (geda core object) module.
 *
 * \param obj_s   #OBJECT smob to modify.
 * \param color_s new colormap index to use for \a obj_s.
 * \return the modified \a obj_s.
 */
SCM_DEFINE (set_object_color, "%set-object-color!", 2, 0, 0,
            (SCM obj_s, SCM color_s), "Set the color of an object.")
{
  SCM_ASSERT (EDASCM_OBJECTP (obj_s), obj_s,
              SCM_ARG1, s_set_object_color);
  SCM_ASSERT (scm_is_integer (color_s), color_s,
              SCM_ARG2, s_set_object_color);

  o_set_color (edascm_c_current_toplevel (),
               edascm_to_object (obj_s), scm_to_int (color_s));

  return obj_s;
}

/*! \brief Create a new line.
 * \par Function Description
 * Creates a new line object, with all its parameters set to default
 * values.
 *
 * \note Scheme API: Implements the %make-line procedure in the (geda
 * core object) module.
 *
 * \return a newly-created line object.
 */
SCM_DEFINE (make_line, "%make-line", 0, 0, 0,
            (), "Create a new line object.")
{
  OBJECT *obj = o_line_new (edascm_c_current_toplevel (),
                            OBJ_LINE, DEFAULT_COLOR,
                            0, 0, 0, 0);

  SCM result = edascm_from_object (obj);

  /* At the moment, the only pointer to the object is owned by the
   * smob. */
  edascm_c_set_gc (result, TRUE);

  return result;
}

/*! \brief Set line parameters.
 * \par Function Description
 * Modifies a line object by setting its parameters to new values.
 *
 * \note Scheme API: Implements the %set-line! procedure in the (geda
 * core object) module.
 *
 * This function also works on net, bus and pin objects.  For pins,
 * the start is the connectable point on the pin.
 *
 * \param line_s the line object to modify.
 * \param x1_s   the new x-coordinate of the start of the line.
 * \param y1_s   the new y-coordinate of the start of the line.
 * \param x2_s   the new x-coordinate of the end of the line.
 * \param y2_s   the new y-coordinate of the end of the line.
 * \param color  the colormap index of the color to be used for
 *               drawing the line.
 *
 * \return the modified line object.
 */
SCM_DEFINE (set_line, "%set-line!", 6, 0, 0,
            (SCM line_s, SCM x1_s, SCM y1_s, SCM x2_s, SCM y2_s, SCM color_s),
            "Set line parameters.")
{
  SCM_ASSERT ((edascm_is_object_type (line_s, OBJ_LINE)
               || edascm_is_object_type (line_s, OBJ_NET)
               || edascm_is_object_type (line_s, OBJ_BUS)
               || edascm_is_object_type (line_s, OBJ_PIN)),
              line_s, SCM_ARG1, s_set_line);

  SCM_ASSERT (scm_is_integer (x1_s),    x1_s,    SCM_ARG2, s_set_line);
  SCM_ASSERT (scm_is_integer (y1_s),    y1_s,    SCM_ARG3, s_set_line);
  SCM_ASSERT (scm_is_integer (x2_s),    x2_s,    SCM_ARG4, s_set_line);
  SCM_ASSERT (scm_is_integer (y2_s),    y2_s,    SCM_ARG5, s_set_line);
  SCM_ASSERT (scm_is_integer (color_s), color_s, SCM_ARG6, s_set_line);

  TOPLEVEL *toplevel = edascm_c_current_toplevel ();
  OBJECT *obj = edascm_to_object (line_s);
  int x1 = scm_to_int (x1_s);
  int y1 = scm_to_int (y1_s);
  int x2 = scm_to_int (x2_s);
  int y2 = scm_to_int (y2_s);
  switch (obj->type) {
  case OBJ_LINE:
    o_line_modify (toplevel, obj, x1, y1, LINE_END1);
    o_line_modify (toplevel, obj, x2, y2, LINE_END2);
    break;
  case OBJ_NET:
    o_net_modify (toplevel, obj, x1, y1, 0);
    o_net_modify (toplevel, obj, x2, y2, 1);
    break;
  case OBJ_BUS:
    o_bus_modify (toplevel, obj, x1, y1, 0);
    o_bus_modify (toplevel, obj, x2, y2, 1);
    break;
  case OBJ_PIN:
    /* Swap ends according to pin's whichend flag. */
    o_pin_modify (toplevel, obj, x1, y1, obj->whichend ? 1 : 0);
    o_pin_modify (toplevel, obj, x2, y2, obj->whichend ? 0 : 1);
    break;
  default:
    return line_s;
  }
  o_set_color (toplevel, obj, scm_to_int (color_s));

  return line_s;
}

/*! \brief Get line parameters.
 * \par Function Description
 * Retrieves the parameters of a line object. The return value is a
 * list of parameters:
 *
 * -# X-coordinate of start of line
 * -# Y-coordinate of start of line
 * -# X-coordinate of end of line
 * -# Y-coordinate of end of line
 * -# Colormap index of color to be used for drawing the line
 *
 * This function also works on net, bus and pin objects.  For pins,
 * the start is the connectable point on the pin.
 *
 * \param line_s the line object to inspect.
 * \return a list of line parameters.
 */
SCM_DEFINE (line_info, "%line-info", 1, 0, 0,
            (SCM line_s), "Get line parameters.")
{
  SCM_ASSERT ((edascm_is_object_type (line_s, OBJ_LINE)
               || edascm_is_object_type (line_s, OBJ_NET)
               || edascm_is_object_type (line_s, OBJ_BUS)
               || edascm_is_object_type (line_s, OBJ_PIN)),
              line_s, SCM_ARG1, s_line_info);

  OBJECT *obj = edascm_to_object (line_s);
  SCM x1 = scm_from_int (obj->line->x[0]);
  SCM y1 = scm_from_int (obj->line->y[0]);
  SCM x2 = scm_from_int (obj->line->x[1]);
  SCM y2 = scm_from_int (obj->line->y[1]);
  SCM color = scm_from_int (obj->color);

  /* Swap ends according to pin's whichend flag. */
  if ((obj->type == OBJ_PIN) && obj->whichend) {
    SCM s;
    s = x1; x1 = x2; x2 = s;
    s = y1; y1 = y2; y2 = s;
  }

  return scm_list_n (x1, y1, x2, y2, color, SCM_UNDEFINED);
}

/*! \brief Create a new net.
 * \par Function Description
 * Creates a new net object, with all its parameters set to default
 * values.
 *
 * \note Scheme API: Implements the %make-net procedure in the (geda
 * core object) module.
 *
 * \return a newly-created net object.
 */
SCM_DEFINE (make_net, "%make-net", 0, 0, 0,
            (), "Create a new net object.")
{
  OBJECT *obj;
  SCM result;

  obj = o_net_new (edascm_c_current_toplevel (),
                   OBJ_NET, NET_COLOR, 0, 0, 0, 0);


  result = edascm_from_object (obj);

  /* At the moment, the only pointer to the object is owned by the
   * smob. */
  edascm_c_set_gc (result, 1);

  return result;
}

/*! \brief Create a new bus.
 * \par Function Description
 * Creates a new bus object, with all its parameters set to default
 * values.
 *
 * \note Scheme API: Implements the %make-bus procedure in the (geda
 * core object) module.
 *
 * \todo Do we need a way to get/set bus ripper direction?
 *
 * \return a newly-created bus object.
 */
SCM_DEFINE (make_bus, "%make-bus", 0, 0, 0,
            (), "Create a new bus object.")
{
  OBJECT *obj;
  SCM result;

  obj = o_bus_new (edascm_c_current_toplevel (),
                   OBJ_BUS, BUS_COLOR, 0, 0, 0, 0,
                   0); /* Bus ripper direction */

  result = edascm_from_object (obj);

  /* At the moment, the only pointer to the object is owned by the
   * smob. */
  edascm_c_set_gc (result, 1);

  return result;
}

/*! \brief Create a new pin.
 * \par Function description
 * Creates a new pin object, with all parameters set to default
 * values.  type_s is a Scheme symbol indicating whether the pin
 * should be a "net" pin or a "bus" pin.
 *
 * \note Scheme API: Implements the %make-pin procedure in the (geda
 * core object) module.
 *
 * \return a newly-created pin object.
 */
SCM_DEFINE (make_pin, "%make-pin", 1, 0, 0,
            (SCM type_s), "Create a new pin object.")
{
  SCM_ASSERT (scm_is_symbol (type_s),
              type_s, SCM_ARG1, s_make_pin);

  int type;
  if (type_s == net_sym) {
    type = PIN_TYPE_NET;
  } else if (type_s == bus_sym) {
    type = PIN_TYPE_BUS;
  } else {
    scm_misc_error (s_make_pin,
                    _("Invalid pin type ~A, must be 'net or 'bus"),
                    scm_list_1 (type_s));
  }

  OBJECT *obj = o_pin_new (edascm_c_current_toplevel (),
                           OBJ_PIN, PIN_COLOR, 0, 0, 0, 0, type, 0);
  SCM result = edascm_from_object (obj);

  /* At the moment, the only pointer to the object is owned by the
   * smob. */
  edascm_c_set_gc (result, 1);

  return result;
}

/*! \brief Get the type of a pin object.
 * \par Function Description
 * Returns a symbol describing the pin type of the pin object \a
 * pin_s.
 *
 * \note Scheme API: Implements the %make-pin procedure in the (geda
 * core object) module.
 *
 * \return the symbol 'pin or 'bus.
 */
SCM_DEFINE (pin_type, "%pin-type", 1, 0, 0,
            (SCM pin_s), "Get the type of a pin object.")
{
  SCM_ASSERT (edascm_is_object_type (pin_s, OBJ_PIN), pin_s,
              SCM_ARG1, s_pin_type);

  OBJECT *obj = edascm_to_object (pin_s);
  SCM result;

  switch (obj->pin_type) {
  case PIN_TYPE_NET:
    result = net_sym;
    break;
  case PIN_TYPE_BUS:
    result = bus_sym;
    break;
  default:
    scm_misc_error (s_make_pin,
                    _("Object ~A has invalid pin type."),
                    scm_list_1 (pin_s));
  }

  return result;
}

/*! \brief Create a new box.
 * \par Function Description
 * Creates a new box object, with all its parameters set to default
 * values.
 *
 * \note Scheme API: Implements the %make-box procedure in the (geda
 * core object) module.
 *
 * \return a newly-created box object.
 */
SCM_DEFINE (make_box, "%make-box", 0, 0, 0,
            (), "Create a new box object.")
{
  OBJECT *obj = o_box_new (edascm_c_current_toplevel (),
                           OBJ_BOX, DEFAULT_COLOR,
                           0, 0, 0, 0);

  SCM result = edascm_from_object (obj);

  /* At the moment, the only pointer to the object is owned by the
   * smob. */
  edascm_c_set_gc (result, 1);

  return result;
}

/*! \brief Set box parameters.
 * \par Function Description
 * Modifies a box object by setting its parameters to new values.
 *
 * \note Scheme API: Implements the %set-box! procedure in the (geda
 * core object) module.
 *
 * \param box_s  the box object to modify.
 * \param x1_s   the new x-coordinate of the top left of the box.
 * \param y1_s   the new y-coordinate of the top left of the box.
 * \param x2_s   the new x-coordinate of the bottom right of the box.
 * \param y2_s   the new y-coordinate of the bottom right of the box.
 * \param color  the colormap index of the color to be used for
 *               drawing the box.
 *
 * \return the modified box object.
 */
SCM_DEFINE (set_box, "%set-box!", 6, 0, 0,
            (SCM box_s, SCM x1_s, SCM y1_s, SCM x2_s, SCM y2_s, SCM color_s),
            "Set box parameters.")
{
  SCM_ASSERT (edascm_is_object_type (box_s, OBJ_BOX), box_s,
              SCM_ARG1, s_set_box);
  SCM_ASSERT (scm_is_integer (x1_s),    x1_s,    SCM_ARG2, s_set_box);
  SCM_ASSERT (scm_is_integer (y1_s),    y1_s,    SCM_ARG3, s_set_box);
  SCM_ASSERT (scm_is_integer (x2_s),    x2_s,    SCM_ARG4, s_set_box);
  SCM_ASSERT (scm_is_integer (y2_s),    y2_s,    SCM_ARG5, s_set_box);
  SCM_ASSERT (scm_is_integer (color_s), color_s, SCM_ARG6, s_set_box);

  TOPLEVEL *toplevel = edascm_c_current_toplevel ();
  OBJECT *obj = edascm_to_object (box_s);
  o_box_modify_all (toplevel, obj,
                    scm_to_int (x1_s), scm_to_int (y1_s),
                    scm_to_int (x2_s), scm_to_int (y2_s));
  o_set_color (toplevel, obj, scm_to_int (color_s));

  return box_s;
}

/*! \brief Get box parameters.
 * \par Function Description
 * Retrieves the parameters of a box object. The return value is a
 * list of parameters:
 *
 * -# X-coordinate of top left of box
 * -# Y-coordinate of top left of box
 * -# X-coordinate of bottom right of box
 * -# Y-coordinate of bottom right of box
 * -# Colormap index of color to be used for drawing the box
 *
 * \param box_s the box object to inspect.
 * \return a list of box parameters.
 */
SCM_DEFINE (box_info, "%box-info", 1, 0, 0,
            (SCM box_s), "Get box parameters.")
{
  SCM_ASSERT (edascm_is_object_type (box_s, OBJ_BOX), box_s,
              SCM_ARG1, s_box_info);

  OBJECT *obj = edascm_to_object (box_s);

  return scm_list_n (scm_from_int (obj->box->upper_x),
                     scm_from_int (obj->box->upper_y),
                     scm_from_int (obj->box->lower_x),
                     scm_from_int (obj->box->lower_y),
                     scm_from_int (obj->color),
                     SCM_UNDEFINED);
}

/*! \brief Create a new circle.
 * \par Function Description

 * Creates a new circle object, with all its parameters set to default
 * values.
 *
 * \note Scheme API: Implements the %make-circle procedure in the
 * (geda core object) module.
 *
 * \return a newly-created circle object.
 */
SCM_DEFINE (make_circle, "%make-circle", 0, 0, 0,
            (), "Create a new circle object.")
{
  OBJECT *obj = o_circle_new (edascm_c_current_toplevel (),
                              OBJ_CIRCLE, DEFAULT_COLOR,
                              0, 0, 1);

  SCM result = edascm_from_object (obj);

  /* At the moment, the only pointer to the object is owned by the
   * smob. */
  edascm_c_set_gc (result, 1);

  return result;
}

/*! \brief Set circle parameters.
 * \par Function Description
 * Modifies a circle object by setting its parameters to new values.
 *
 * \note Scheme API: Implements the %set-circle! procedure in the
 * (geda core object) module.
 *
 * \param circle_s the circle object to modify.
 * \param x_s    the new x-coordinate of the center of the circle.
 * \param y_s    the new y-coordinate of the center of the circle.
 * \param r_s    the new radius of the circle.
 * \param color  the colormap index of the color to be used for
 *               drawing the circle.
 *
 * \return the modified circle object.
 */
SCM_DEFINE (set_circle, "%set-circle!", 5, 0, 0,
            (SCM circle_s, SCM x_s, SCM y_s, SCM r_s, SCM color_s),
            "Set circle parameters")
{
  SCM_ASSERT (edascm_is_object_type (circle_s, OBJ_CIRCLE), circle_s,
              SCM_ARG1, s_set_circle);
  SCM_ASSERT (scm_is_integer (x_s),     x_s,     SCM_ARG2, s_set_circle);
  SCM_ASSERT (scm_is_integer (y_s),     y_s,     SCM_ARG3, s_set_circle);
  SCM_ASSERT (scm_is_integer (r_s),     r_s,     SCM_ARG4, s_set_circle);
  SCM_ASSERT (scm_is_integer (color_s), color_s, SCM_ARG5, s_set_circle);

  TOPLEVEL *toplevel = edascm_c_current_toplevel ();
  OBJECT *obj = edascm_to_object (circle_s);
  o_circle_modify (toplevel, obj, scm_to_int(x_s), scm_to_int(y_s),
                   CIRCLE_CENTER);
  o_circle_modify (toplevel, obj, scm_to_int(r_s), 0, CIRCLE_RADIUS);
  o_set_color (toplevel, obj, scm_to_int (color_s));

  return circle_s;
}

/*! \brief Get circle parameters.
 * \par Function Description

 * Retrieves the parameters of a circle object. The return value is a
 * list of parameters:
 *
 * -# X-coordinate of center of circle
 * -# Y-coordinate of center of circle
 * -# Radius of circle
 * -# Colormap index of color to be used for drawing the circle
 *
 * \param circle_s the circle object to inspect.
 * \return a list of circle parameters.
 */
SCM_DEFINE (circle_info, "%circle-info", 1, 0, 0,
            (SCM circle_s), "Get circle parameters.")
{
  SCM_ASSERT (edascm_is_object_type (circle_s, OBJ_CIRCLE),
              circle_s, SCM_ARG1, s_circle_info);

  OBJECT *obj = edascm_to_object (circle_s);

  return scm_list_n (scm_from_int (obj->circle->center_x),
                     scm_from_int (obj->circle->center_y),
                     scm_from_int (obj->circle->radius),
                     scm_from_int (obj->color),
                     SCM_UNDEFINED);
}

/*! \brief Create a new arc.
 * \par Function Description
 * Creates a new arc object, with all its parameters set to default
 * values.
 *
 * \note Scheme API: Implements the %make-arc procedure in the
 * (geda core object) module.
 *
 * \return a newly-created arc object.
 */
SCM_DEFINE (make_arc, "%make-arc", 0, 0, 0,
            (), "Create a new arc object.")
{
  OBJECT *obj = o_arc_new (edascm_c_current_toplevel (),
                              OBJ_ARC, DEFAULT_COLOR,
                           0, 0, 1, 0, 0);

  SCM result = edascm_from_object (obj);

  /* At the moment, the only pointer to the object is owned by the
   * smob. */
  edascm_c_set_gc (result, 1);

  return result;
}

/*! \brief Set arc parameters.
 * \par Function Description
 * Modifies a arc object by setting its parameters to new values.
 *
 * \note Scheme API: Implements the %set-arc! procedure in the
 * (geda core object) module.
 *
 * \param arc_s         the arc object to modify.
 * \param x_s           the new x-coordinate of the center of the arc.
 * \param y_s           the new y-coordinate of the center of the arc.
 * \param r_s           the new radius of the arc.
 * \param start_angle_s the start angle of the arc.
 * \param end_angle_s   the start angle of the arc.
 * \param color_s       the colormap index of the color to be used for
 *                      drawing the arc.
 *
 * \return the modified arc object.
 */
SCM_DEFINE (set_arc, "%set-arc!", 7, 0, 0,
            (SCM arc_s, SCM x_s, SCM y_s, SCM r_s, SCM start_angle_s,
             SCM end_angle_s, SCM color_s),
            "Set arc parameters")
{
  SCM_ASSERT (edascm_is_object_type (arc_s, OBJ_ARC), arc_s,
              SCM_ARG1, s_set_arc);
  SCM_ASSERT (scm_is_integer (x_s),     x_s,     SCM_ARG2, s_set_arc);
  SCM_ASSERT (scm_is_integer (y_s),     y_s,     SCM_ARG3, s_set_arc);
  SCM_ASSERT (scm_is_integer (r_s),     r_s,     SCM_ARG4, s_set_arc);
  SCM_ASSERT (scm_is_integer (color_s), color_s, SCM_ARG5, s_set_arc);
  SCM_ASSERT (scm_is_integer (start_angle_s),
                                  start_angle_s, SCM_ARG3, s_set_arc);
  SCM_ASSERT (scm_is_integer (end_angle_s),
                                  end_angle_s, SCM_ARG4, s_set_arc);

  TOPLEVEL *toplevel = edascm_c_current_toplevel ();
  OBJECT *obj = edascm_to_object (arc_s);
  o_arc_modify (toplevel, obj, scm_to_int(x_s), scm_to_int(y_s),
                   ARC_CENTER);
  o_arc_modify (toplevel, obj, scm_to_int(r_s), 0, ARC_RADIUS);
  o_arc_modify (toplevel, obj, scm_to_int(start_angle_s), 0, ARC_START_ANGLE);
  o_arc_modify (toplevel, obj, scm_to_int(end_angle_s), 0, ARC_END_ANGLE);
  o_set_color (toplevel, obj, scm_to_int (color_s));

  return arc_s;
}

/*! \brief Get arc parameters.
 * \par Function Description
 * Retrieves the parameters of a arc object. The return value is a
 * list of parameters:
 *
 * -# X-coordinate of center of arc
 * -# Y-coordinate of center of arc
 * -# Radius of arc
 * -# Start angle of arc
 * -# End angle of arc
 * -# Colormap index of color to be used for drawing the arc
 *
 * \note Scheme API: Implements the %arc-info procedure in the
 * (geda core object) module.
 *
 * \param arc_s the arc object to inspect.
 * \return a list of arc parameters.
 */
SCM_DEFINE (arc_info, "%arc-info", 1, 0, 0,
            (SCM arc_s), "Get arc parameters.")
{
  SCM_ASSERT (edascm_is_object_type (arc_s, OBJ_ARC),
              arc_s, SCM_ARG1, s_arc_info);

  OBJECT *obj = edascm_to_object (arc_s);

  return scm_list_n (scm_from_int (obj->arc->x),
                     scm_from_int (obj->arc->y),
                     scm_from_int (obj->arc->width / 2),
                     scm_from_int (obj->arc->start_angle),
                     scm_from_int (obj->arc->end_angle),
                     scm_from_int (obj->color),
                     SCM_UNDEFINED);
}

/*!
 * \brief Create the (geda core object) Scheme module.
 * \par Function Description
 * Defines procedures in the (geda core object) module. The module can
 * be accessed using (use-modules (geda core object)).
 */
static void
init_module_geda_core_object ()
{
  /* Register the functions and symbols */
  #include "scheme_object.x"

  /* Add them to the module's public definitions. */
  scm_c_export (s_object_type, s_copy_object,
                s_object_color, s_set_object_color,
                s_make_line, s_make_net, s_make_bus,
                s_make_pin, s_pin_type,
                s_set_line, s_line_info,
                s_make_box, s_set_box, s_box_info,
                s_make_circle, s_set_circle, s_circle_info,
                s_make_arc, s_set_arc, s_arc_info,
                NULL);
}

/*!
 * \brief Initialise the basic gEDA object manipulation procedures.
 * \par Function Description
 * Registers some Scheme procedures for working with #OBJECT
 * smobs. Should only be called by scheme_api_init().
 */
void
edascm_init_object ()
{
  /* Define the (geda core object) module */
  scm_c_define_module ("geda core object",
                       init_module_geda_core_object,
                       NULL);
}
