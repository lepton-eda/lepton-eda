/* Lepton EDA library - Scheme API
 * Copyright (C) 2010-2012 Peter Brett <peter@peter-b.co.uk>
 * Copyright (C) 2011-2016 gEDA Contributors
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 */

/*!
 * \file scheme_object.c
 * \brief Scheme API object manipulation procedures.
 */

#include <config.h>

#include "liblepton_priv.h"
#include "libleptonguile_priv.h"

SCM_SYMBOL (wrong_type_arg_sym , "wrong-type-arg");
SCM_SYMBOL (net_sym , "net");
SCM_SYMBOL (bus_sym , "bus");

SCM_SYMBOL (lower_left_sym , "lower-left");
SCM_SYMBOL (middle_left_sym , "middle-left");
SCM_SYMBOL (upper_left_sym , "upper-left");
SCM_SYMBOL (lower_center_sym , "lower-center");
SCM_SYMBOL (middle_center_sym , "middle-center");
SCM_SYMBOL (upper_center_sym , "upper-center");
SCM_SYMBOL (lower_right_sym , "lower-right");
SCM_SYMBOL (middle_right_sym , "middle-right");
SCM_SYMBOL (upper_right_sym , "upper-right");

SCM_SYMBOL (name_sym , "name");
SCM_SYMBOL (value_sym , "value");
SCM_SYMBOL (both_sym , "both");

SCM_SYMBOL (solid_sym , "solid");
SCM_SYMBOL (hollow_sym , "hollow");
SCM_SYMBOL (mesh_sym , "mesh");
SCM_SYMBOL (hatch_sym , "hatch");

SCM_SYMBOL (moveto_sym , "moveto");
SCM_SYMBOL (lineto_sym , "lineto");
SCM_SYMBOL (curveto_sym , "curveto");
SCM_SYMBOL (closepath_sym , "closepath");

/*! \brief Convert a Scheme object list to a GList.
 * \par Function Description
 * Takes a Scheme list of #LeptonObject smobs, and returns a GList
 * containing the objects. If \a objs is not a list of #LeptonObject smobs,
 * throws a Scheme error.
 *
 * \warning If the #LeptonObject structures in the GList are to be stored by
 * C code and later free()'d directly, the smobs must be marked as
 * unsafe for garbage collection (by calling edascm_c_set_gc()).
 *
 * \param [in] objs a Scheme list of #LeptonObject smobs.
 * \param [in] subr the name of the Scheme subroutine (used for error
 *                  messages).
 * \return a #GList of #LeptonObject.
 */
GList *
edascm_to_object_glist (SCM objs, const char *subr)
{
  GList *result = NULL;
  SCM lst;

  SCM_ASSERT (scm_is_true (scm_list_p (objs)), objs, SCM_ARGn, subr);

  scm_dynwind_begin ((scm_t_dynwind_flags) 0);
  scm_dynwind_unwind_handler ((void (*)(void *)) g_list_free,
                              result,
                              (scm_t_wind_flags) 0);

  for (lst = objs; !scm_is_null (lst); lst = SCM_CDR (lst)) {
    SCM smob = SCM_CAR (lst);
    result = g_list_prepend (result, (gpointer) edascm_to_object (smob));
  }

  scm_remember_upto_here_1 (lst);

  scm_dynwind_end ();

  return g_list_reverse (result);
}

/*! \brief Convert a GList of objects into a Scheme list.
 * \par Function Description
 * Takes a GList of #LeptonObject and returns a Scheme list of corresponding
 * object smobs.
 *
 * \warning If the #LeptonObject structures are to be subsequently managed
 * only by Scheme, the smobs in the returned list must be marked as
 * safe for garbage collection (by calling edascm_c_set_gc()).
 *
 * \param [in] objs a #GList of #LeptonObject instances.
 * \return a Scheme list of smobs corresponding to each #LeptonObject.
 */
SCM
edascm_from_object_glist (const GList *objs)
{
  SCM lst = SCM_EOL;
  SCM rlst;
  GList *iter = (GList *) objs;

  while (iter != NULL) {
    lst = scm_cons (edascm_from_object ((LeptonObject*) iter->data), lst);
    iter = g_list_next (iter);
  }

  rlst = scm_reverse (lst);

  scm_remember_upto_here_1 (lst);
  return rlst;
}

/*! \brief Test if an object smob is of a particular type.
 * \par Function Description
 * Checks if \a smob contains an #LeptonObject of the given \a type. This is
 * intended to be used by C-based Scheme procedures for working with
 * particular object types.
 *
 * \param [in] smob Scheme value to check type for.
 * \param [in] type Type to check against (e.g. OBJ_LINE).
 * \return non-zero if \a smob is an #LeptonObject smob of \a type.
 */
int
edascm_is_object_type (SCM smob, int type)
{
  if (!EDASCM_OBJECTP(smob)) return 0;

  LeptonObject *obj = edascm_to_object (smob);
  return (lepton_object_get_type (obj) == type);
}


/*! \brief Create a new line.
 * \par Function Description
 * Creates a new line object, with all its parameters set to default
 * values.
 *
 * \note Scheme API: Implements the %make-line procedure in the
 * (lepton core object) module.
 *
 * \return a newly-created line object.
 */
SCM_DEFINE (make_line, "%make-line", 0, 0, 0,
            (), "Create a new line object.")
{
  LeptonObject *object = lepton_line_object_new (default_color_id(),
                                                 0,
                                                 0,
                                                 0,
                                                 0);

  SCM result = edascm_from_object (object);

  /* At the moment, the only pointer to the object is owned by the
   * smob. */
  edascm_c_set_gc (result, TRUE);

  return result;
}

/*! \brief Set line parameters.
 * \par Function Description
 * Modifies a line object by setting its parameters to new values.
 *
 * \note Scheme API: Implements the %set-line! procedure in the
 * (lepton core object) module.
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
SCM_DEFINE (set_line_x, "%set-line!", 6, 0, 0,
            (SCM line_s, SCM x1_s, SCM y1_s, SCM x2_s, SCM y2_s, SCM color_s),
            "Set line parameters.")
{
  SCM_ASSERT ((edascm_is_object_type (line_s, OBJ_LINE)
               || edascm_is_object_type (line_s, OBJ_NET)
               || edascm_is_object_type (line_s, OBJ_BUS)
               || edascm_is_object_type (line_s, OBJ_PIN)),
              line_s, SCM_ARG1, s_set_line_x);

  SCM_ASSERT (scm_is_integer (x1_s),    x1_s,    SCM_ARG2, s_set_line_x);
  SCM_ASSERT (scm_is_integer (y1_s),    y1_s,    SCM_ARG3, s_set_line_x);
  SCM_ASSERT (scm_is_integer (x2_s),    x2_s,    SCM_ARG4, s_set_line_x);
  SCM_ASSERT (scm_is_integer (y2_s),    y2_s,    SCM_ARG5, s_set_line_x);
  SCM_ASSERT (scm_is_integer (color_s), color_s, SCM_ARG6, s_set_line_x);

  LeptonObject *obj = edascm_to_object (line_s);
  int x1 = scm_to_int (x1_s);
  int y1 = scm_to_int (y1_s);
  int x2 = scm_to_int (x2_s);
  int y2 = scm_to_int (y2_s);

  /* We may need to update connectivity. */
  s_conn_remove_object_connections (obj);

  switch (lepton_object_get_type (obj)) {
  case OBJ_LINE:
    lepton_line_object_modify (obj, x1, y1, LINE_END1);
    lepton_line_object_modify (obj, x2, y2, LINE_END2);
    break;
  case OBJ_NET:
    lepton_net_object_modify (obj, x1, y1, 0);
    lepton_net_object_modify (obj, x2, y2, 1);
    break;
  case OBJ_BUS:
    lepton_bus_object_modify (obj, x1, y1, 0);
    lepton_bus_object_modify (obj, x2, y2, 1);
    break;
  case OBJ_PIN:
    /* Swap ends according to pin's whichend flag. */
    lepton_pin_object_modify (obj, x1, y1, obj->whichend ? 1 : 0);
    lepton_pin_object_modify (obj, x2, y2, obj->whichend ? 0 : 1);
    break;
  default:
    return line_s;
  }
  lepton_object_set_color (obj, scm_to_int (color_s));

  /* We may need to update connectivity. */
  LeptonPage *page = lepton_object_get_page (obj);
  if (page != NULL) {
    s_conn_update_object (page, obj);
  }

  lepton_object_page_set_changed (obj);

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

  LeptonObject *obj = edascm_to_object (line_s);
  SCM x1 = scm_from_int (lepton_line_object_get_x0 (obj));
  SCM y1 = scm_from_int (lepton_line_object_get_y0 (obj));
  SCM x2 = scm_from_int (lepton_line_object_get_x1 (obj));
  SCM y2 = scm_from_int (lepton_line_object_get_y1 (obj));
  SCM color = scm_from_int (lepton_object_get_color (obj));

  /* Swap ends according to pin's whichend flag. */
  if (lepton_object_is_pin (obj) && obj->whichend)
  {
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
 * \note Scheme API: Implements the %make-net procedure in the
 * (lepton core object) module.
 *
 * \return a newly-created net object.
 */
SCM_DEFINE (make_net, "%make-net", 0, 0, 0,
            (), "Create a new net object.")
{
  LeptonObject *obj;
  SCM result;

  obj = lepton_net_object_new (NET_COLOR, 0, 0, 0, 0);


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
 * \note Scheme API: Implements the %make-bus procedure in the
 * (lepton core object) module.
 *
 * \todo Do we need a way to get/set bus ripper direction?
 *
 * \return a newly-created bus object.
 */
SCM_DEFINE (make_bus, "%make-bus", 0, 0, 0,
            (), "Create a new bus object.")
{
  LeptonObject *obj;
  SCM result;

  obj = lepton_bus_object_new (BUS_COLOR,
                             0,
                             0,
                             0,
                             0,
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
 * \note Scheme API: Implements the %make-pin procedure in the
 * (lepton core object) module.
 *
 * \return a newly-created pin object.
 */
SCM_DEFINE (make_pin, "%make-pin", 1, 0, 0,
            (SCM type_s), "Create a new pin object.")
{
  SCM_ASSERT (scm_is_symbol (type_s),
              type_s, SCM_ARG1, s_make_pin);

  int type;
  if (scm_is_eq (type_s, net_sym)) {
    type = PIN_TYPE_NET;
  } else if (scm_is_eq (type_s, bus_sym)) {
    type = PIN_TYPE_BUS;
  } else {
    scm_misc_error (s_make_pin,
                    _("Invalid pin type ~A, must be 'net or 'bus"),
                    scm_list_1 (type_s));
  }

  LeptonObject *obj = lepton_pin_object_new (PIN_COLOR,
                                             0,
                                             0,
                                             0,
                                             0,
                                             type,
                                             0);
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
 * \note Scheme API: Implements the %make-pin procedure in the
 * (lepton core object) module.
 *
 * \return the symbol 'pin or 'bus.
 */
SCM_DEFINE (pin_type, "%pin-type", 1, 0, 0,
            (SCM pin_s), "Get the type of a pin object.")
{
  SCM_ASSERT (edascm_is_object_type (pin_s, OBJ_PIN), pin_s,
              SCM_ARG1, s_pin_type);

  LeptonObject *obj = edascm_to_object (pin_s);
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

/*! \brief Create a new circle.
 * \par Function Description

 * Creates a new circle object, with all its parameters set to default
 * values.
 *
 * \note Scheme API: Implements the %make-circle procedure in the
 * (lepton core object) module.
 *
 * \return a newly-created circle object.
 */
SCM_DEFINE (make_circle, "%make-circle", 0, 0, 0,
            (), "Create a new circle object.")
{
  LeptonObject *object = lepton_circle_object_new (default_color_id(),
                                                   0,
                                                   0,
                                                   1);

  SCM result = edascm_from_object (object);

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
 * (lepton core object) module.
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
SCM_DEFINE (set_circle_x, "%set-circle!", 5, 0, 0,
            (SCM circle_s, SCM x_s, SCM y_s, SCM r_s, SCM color_s),
            "Set circle parameters")
{
  SCM_ASSERT (edascm_is_object_type (circle_s, OBJ_CIRCLE), circle_s,
              SCM_ARG1, s_set_circle_x);
  SCM_ASSERT (scm_is_integer (x_s),     x_s,     SCM_ARG2, s_set_circle_x);
  SCM_ASSERT (scm_is_integer (y_s),     y_s,     SCM_ARG3, s_set_circle_x);
  SCM_ASSERT (scm_is_integer (r_s),     r_s,     SCM_ARG4, s_set_circle_x);
  SCM_ASSERT (scm_is_integer (color_s), color_s, SCM_ARG5, s_set_circle_x);

  LeptonObject *obj = edascm_to_object (circle_s);
  lepton_circle_object_modify (obj, scm_to_int(x_s), scm_to_int(y_s),
                               CIRCLE_CENTER);
  lepton_circle_object_modify (obj, scm_to_int(r_s), 0, CIRCLE_RADIUS);
  lepton_object_set_color (obj, scm_to_int (color_s));

  lepton_object_page_set_changed (obj);

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

  LeptonObject *obj = edascm_to_object (circle_s);

  return scm_list_n (scm_from_int (lepton_circle_object_get_center_x (obj)),
                     scm_from_int (lepton_circle_object_get_center_y (obj)),
                     scm_from_int (lepton_circle_object_get_radius (obj)),
                     scm_from_int (lepton_object_get_color (obj)),
                     SCM_UNDEFINED);
}


/*! \brief Create a new text item.
 * \par Function Description
 * Creates a new text object, with all its parameters set to default
 * values.
 *
 * \note Scheme API: Implements the %make-text procedure in the
 * (lepton core object) module.
 *
 * \return a newly-created text object.
 */
SCM_DEFINE (make_text, "%make-text", 0, 0, 0,
            (), "Create a new text object.")
{
  LeptonObject *obj = lepton_text_object_new (default_color_id(),
                                              0,
                                              0,
                                              LOWER_LEFT,
                                              0,
                                              "",
                                              10,
                                              VISIBLE,
                                              SHOW_NAME_VALUE);

  SCM result = edascm_from_object (obj);

  /* At the moment, the only pointer to the object is owned by the
   * smob. */
  edascm_c_set_gc (result, 1);

  return result;
}

/*! \brief Set text parameters.
 * \par Function Description
 * Modifies a text object by setting its parameters to new values.
 *
 * The alignment \a align_s should be a symbol of the form "x-y" where
 * x can be one of "lower", "middle", or "upper", and y can be one of
 * "left", "center" or "right". \a show_s determines which parts of an
 * attribute-formatted string should be shown, and should be one of
 * the symbols "name", "value" or "both".
 *
 * \note Scheme API: Implements the %set-text! procedure in the
 * (lepton core object) module.
 *
 * \param text_s    the text object to modify.
 * \param x_s       the new x-coordinate of the anchor of the text.
 * \param y_s       the new y-coordinate of the anchor of the text.
 * \param align_s   the new alignment of the text on the anchor.
 * \param angle_s   the angle the text in degrees (0, 90, 180 or 270).
 * \param string_s  the new string to display.
 * \param size_s    the new text size.
 * \param visible_s the new text visibility (SCM_BOOL_T or SCM_BOOL_F).
 * \param show_s    the new attribute part visibility setting.
 * \param color_s   the colormap index of the color to be used for
 *                  drawing the text.
 *
 * \return the modified text object.
 */
SCM_DEFINE (set_text_x, "%set-text!", 10, 0, 0,
            (SCM text_s, SCM x_s, SCM y_s, SCM align_s, SCM angle_s,
             SCM string_s, SCM size_s, SCM visible_s, SCM show_s, SCM color_s),
            "Set text parameters")
{
  SCM_ASSERT (edascm_is_object_type (text_s, OBJ_TEXT), text_s,
              SCM_ARG1, s_set_text_x);
  SCM_ASSERT (scm_is_integer (x_s),     x_s,      SCM_ARG2, s_set_text_x);
  SCM_ASSERT (scm_is_integer (y_s),     y_s,      SCM_ARG3, s_set_text_x);
  SCM_ASSERT (scm_is_symbol (align_s),  align_s,  SCM_ARG4, s_set_text_x);
  SCM_ASSERT (scm_is_integer (angle_s), angle_s,  SCM_ARG5, s_set_text_x);
  SCM_ASSERT (scm_is_string (string_s), string_s, SCM_ARG6, s_set_text_x);
  SCM_ASSERT (scm_is_integer (size_s),  size_s,   SCM_ARG7, s_set_text_x);

  SCM_ASSERT (scm_is_symbol (show_s),    show_s,     9, s_set_text_x);
  SCM_ASSERT (scm_is_integer (color_s),  color_s,   10, s_set_text_x);

  LeptonObject *obj = edascm_to_object (text_s);

  /* Alignment. Sadly we can't switch on pointers. :-( */
  int align;
  if      (scm_is_eq (align_s, lower_left_sym))    { align = LOWER_LEFT;    }
  else if (scm_is_eq (align_s, middle_left_sym))   { align = MIDDLE_LEFT;   }
  else if (scm_is_eq (align_s, upper_left_sym))    { align = UPPER_LEFT;    }
  else if (scm_is_eq (align_s, lower_center_sym))  { align = LOWER_MIDDLE;  }
  else if (scm_is_eq (align_s, middle_center_sym)) { align = MIDDLE_MIDDLE; }
  else if (scm_is_eq (align_s, upper_center_sym))  { align = UPPER_MIDDLE;  }
  else if (scm_is_eq (align_s, lower_right_sym))   { align = LOWER_RIGHT;   }
  else if (scm_is_eq (align_s, middle_right_sym))  { align = MIDDLE_RIGHT;  }
  else if (scm_is_eq (align_s, upper_right_sym))   { align = UPPER_RIGHT;   }
  else {
    scm_misc_error (s_set_text_x,
                    _("Invalid text alignment ~A."),
                    scm_list_1 (align_s));
  }

  /* Angle */
  int angle = scm_to_int (angle_s);
  switch (angle) {
  case 0:
  case 90:
  case 180:
  case 270:
    /* These are all fine. */
    break;
  default:
    /* Otherwise, not fine. */
    scm_misc_error (s_set_text_x,
                    _("Invalid text angle ~A. Must be 0, 90, 180, or 270 degrees"),
                    scm_list_1 (angle_s));
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
    scm_misc_error (s_set_text_x,
                    _("Invalid text name/value visibility ~A."),
                    scm_list_1 (show_s));
  }

  /* Actually make changes */
  lepton_object_emit_pre_change_notify (obj);

  obj->text->x = scm_to_int (x_s);
  obj->text->y = scm_to_int (y_s);
  lepton_text_object_set_alignment (obj, align);
  lepton_text_object_set_angle (obj, angle);

  lepton_text_object_set_size (obj, scm_to_int (size_s));
  lepton_text_object_set_visibility (obj, visibility);
  lepton_text_object_set_show (obj, show);

  lepton_object_emit_change_notify (obj);

  char *tmp = scm_to_utf8_string (string_s);
  lepton_text_object_set_string (obj, tmp);
  free (tmp);

  lepton_text_object_recreate (obj);

  /* Color */
  lepton_object_set_color (obj, scm_to_int (color_s));

  lepton_object_page_set_changed (obj);

  return text_s;
}

/*! \brief Get text parameters.
 * \par Function Description
 * Retrieves the parameters of a text object. The return value is a
 * list of parameters:
 *
 * -# X-coordinate of anchor of text
 * -# Y-coordinate of anchor of text
 * -# Alignment of text
 * -# Angle of text
 * -# The string contained in the text object
 * -# Size of text
 * -# Text visibility
 * -# Which part(s) of an text attribute are shown
 * -# Colormap index of color to be used for drawing the text
 *
 * \note Scheme API: Implements the %text-info procedure in the
 * (lepton core object) module.
 *
 * \param text_s the text object to inspect.
 * \return a list of text parameters.
 */
SCM_DEFINE (text_info, "%text-info", 1, 0, 0,
            (SCM text_s), "Get text parameters.")
{
  SCM_ASSERT (edascm_is_object_type (text_s, OBJ_TEXT),
              text_s, SCM_ARG1, s_text_info);

  LeptonObject *obj = edascm_to_object (text_s);
  SCM align_s, visible_s, show_s;

  switch (lepton_text_object_get_alignment (obj)) {
  case LOWER_LEFT:    align_s = lower_left_sym;    break;
  case MIDDLE_LEFT:   align_s = middle_left_sym;   break;
  case UPPER_LEFT:    align_s = upper_left_sym;    break;
  case LOWER_MIDDLE:  align_s = lower_center_sym;  break;
  case MIDDLE_MIDDLE: align_s = middle_center_sym; break;
  case UPPER_MIDDLE:  align_s = upper_center_sym;  break;
  case LOWER_RIGHT:   align_s = lower_right_sym;   break;
  case MIDDLE_RIGHT:  align_s = middle_right_sym;  break;
  case UPPER_RIGHT:   align_s = upper_right_sym;   break;
  default:
    scm_misc_error (s_text_info,
                    _("Text object ~A has invalid text alignment ~A"),
                    scm_list_2 (text_s, scm_from_int (lepton_text_object_get_alignment (obj))));
  }

  switch (lepton_text_object_get_visibility (obj))
  {
  case VISIBLE:   visible_s = SCM_BOOL_T; break;
  case INVISIBLE: visible_s = SCM_BOOL_F; break;
  default:
    scm_misc_error (s_text_info,
                    _("Text object ~A has invalid visibility ~A"),
                    scm_list_2 (text_s, scm_from_int (lepton_text_object_get_visibility (obj))));
  }

  switch (lepton_text_object_get_show (obj))
  {
  case SHOW_NAME:       show_s = name_sym;  break;
  case SHOW_VALUE:      show_s = value_sym; break;
  case SHOW_NAME_VALUE: show_s = both_sym;  break;
  default:
    scm_misc_error (s_text_info,
                    _("Text object ~A has invalid text attribute visibility ~A"),
                    scm_list_2 (text_s,
                                scm_from_int (lepton_text_object_get_show (obj))));
  }

  return scm_list_n (scm_from_int (obj->text->x),
                     scm_from_int (obj->text->y),
                     align_s,
                     scm_from_int (lepton_text_object_get_angle (obj)),
                     scm_from_utf8_string (lepton_text_object_get_string (obj)),
                     scm_from_int (lepton_text_object_get_size (obj)),
                     visible_s,
                     show_s,
                     scm_from_int (lepton_object_get_color (obj)),
                     SCM_UNDEFINED);
}

/*! \brief Get objects that are connected to an object.
 * \par Function Description
 * Returns a list of all objects directly connected to \a obj_s.  If
 * \a obj_s is not included in a page, throws a Scheme error.  If \a
 * obj_s is not a pin, net, bus, or component object, returns the empty
 * list.
 *
 * \note Scheme API: Implements the %object-connections procedure of
 * the (lepton core object) module.
 *
 * \param obj_s #LeptonObject smob for object to get connections for.
 * \return a list of #LeptonObject smobs.
 */
SCM_DEFINE (object_connections, "%object-connections", 1, 0, 0,
            (SCM obj_s), "Get objects that are connected to an object.")
{
  /* Ensure that the argument is an object smob */
  SCM_ASSERT (edascm_is_object (obj_s), obj_s,
              SCM_ARG1, s_object_connections);

  LeptonObject *obj = edascm_to_object (obj_s);
  if (lepton_object_get_page (obj) == NULL) {
    scm_error (edascm_object_state_sym,
               s_object_connections,
               _("Object ~A is not included in a page."),
               scm_list_1 (obj_s), SCM_EOL);
  }

  GList *lst = s_conn_return_others (NULL, obj);
  SCM result = edascm_from_object_glist (lst);
  g_list_free (lst);
  return result;
}

/*! \brief Make a new, empty path object.
 * \par Function Description
 * Creates a new, empty path object with default color, stroke and
 * fill options.
 *
 * \note Scheme API: Implements the %make-path procedure in the
 * (lepton core object) module.
 *
 * \return a newly-created path object.
 */
SCM_DEFINE (make_path, "%make-path", 0, 0, 0,
            (), "Create a new path object")
{
  LeptonObject *obj = lepton_path_object_new (default_color_id(), "");

  SCM result = edascm_from_object (obj);

  /* At the moment, the only pointer to the object is owned by the
   * smob. */
  edascm_c_set_gc (result, TRUE);

  return result;
}

/*! \brief Get the number of elements in a path.
 * \par Function Description
 * Retrieves the number of path elements in the path object \a obj_s.
 *
 * \note Scheme API: Implements the %path-length procedure in the
 * (lepton core object) module.
 *
 * \param obj_s #LeptonObject smob for path object to inspect.
 * \return The number of path elements in \a obj_s.
 */
SCM_DEFINE (path_length, "%path-length", 1, 0, 0,
            (SCM obj_s), "Get number of elements in a path object.")
{
  /* Ensure that the argument is a path object */
  SCM_ASSERT (edascm_is_object_type (obj_s, OBJ_PATH), obj_s,
              SCM_ARG1, s_path_length);

  LeptonObject *obj = edascm_to_object (obj_s);
  return scm_from_int (obj->path->num_sections);
}

/*! \brief Get one of the elements from a path.
 * \par Function Description
 * Retrieves a path element at index \a index_s from the path object
 * \a obj_s.  If \a index_s is not a valid index, raises a Scheme
 * "out-of-range" error.
 *
 * The return value is a list.  The first element in the list is a
 * symbol indicating the type of path element ("moveto", "lineto",
 * "curveto" or "closepath"), and the remainder of the list contains
 * zero or more control point coordinates, depending on the type of
 * path element.  Each element is evaluated relative to the current
 * path position.
 *
 * - moveto: x and y coordinates of position to step to.
 * - lineto: x and y coordinates of straight line endpoint.
 * - curveto: coordinates of first Bezier control point; coordinates
 *   of second control point; and coordinates of curve endpoint.
 * - closepath: No coordinate parameters.
 *
 * All coordinates are absolute.
 *
 * \note Scheme API: Implements the %path-ref procedure in the
 * (lepton core object) module.
 *
 * \param obj_s   #LeptonObject smob of path object to get element from.
 * \param index_s Index of element to retrieve from \a obj_s
 * \return A list containing the requested path element data.
 */
SCM_DEFINE (path_ref, "%path-ref", 2, 0, 0,
            (SCM obj_s, SCM index_s),
            "Get a path element from a path object.")
{
  /* Ensure that the arguments are a path object and integer */
  SCM_ASSERT (edascm_is_object_type (obj_s, OBJ_PATH), obj_s,
              SCM_ARG1, s_path_ref);
  SCM_ASSERT (scm_is_integer (index_s), index_s, SCM_ARG2, s_path_ref);

  LeptonObject *obj = edascm_to_object (obj_s);
  int idx = scm_to_int (index_s);

  /* Check index is valid for path */
  if ((idx < 0) || (idx >= obj->path->num_sections)) {
    scm_out_of_range (s_path_ref, index_s);
  }

  LeptonPathSection *section = &obj->path->sections[idx];

  switch (section->code) {
  case PATH_MOVETO:
  case PATH_MOVETO_OPEN:
    return scm_list_3 (moveto_sym,
                       scm_from_int (section->x3),
                       scm_from_int (section->y3));
  case PATH_LINETO:
    return scm_list_3 (lineto_sym,
                       scm_from_int (section->x3),
                       scm_from_int (section->y3));
  case PATH_CURVETO:
    return scm_list_n (curveto_sym,
                       scm_from_int (section->x1),
                       scm_from_int (section->y1),
                       scm_from_int (section->x2),
                       scm_from_int (section->y2),
                       scm_from_int (section->x3),
                       scm_from_int (section->y3),
                       SCM_UNDEFINED);
  case PATH_END:
    return scm_list_1 (closepath_sym);
  default:
    scm_misc_error (s_path_ref,
                    _("Path object ~A has invalid element type ~A at index ~A"),
                    scm_list_3 (obj_s, scm_from_int (section->code), index_s));
  }

}

/*! \brief Remove an element from a path.
 * \par Function Description
 * Removes the path element at index \a index_s from the path object
 * \a obj_s. If \a index_s is not a valid index, raises a Scheme
 * "out-of-range" error.
 *
 * \note Scheme API: Implements the %path-remove! procedure in the
 * (lepton core object) module.
 *
 * \param obj_s   #LeptonObject smob of path object to remove element from.
 * \param index_s Index of element to remove from \a obj_s.
 * \return \a obj_s.
 */
SCM_DEFINE (path_remove_x, "%path-remove!", 2, 0, 0,
            (SCM obj_s, SCM index_s),
            "Remove a path element from a path object.")
{
  /* Ensure that the arguments are a path object and integer */
  SCM_ASSERT (edascm_is_object_type (obj_s, OBJ_PATH), obj_s,
              SCM_ARG1, s_path_ref);
  SCM_ASSERT (scm_is_integer (index_s), index_s, SCM_ARG2, s_path_ref);

  LeptonObject *obj = edascm_to_object (obj_s);
  int idx = scm_to_int (index_s);

  if ((idx < 0) || (idx >= obj->path->num_sections)) {
    /* Index is valid for path */
    scm_out_of_range (s_path_ref, index_s);

  }

  lepton_object_emit_pre_change_notify (obj);

  if (idx + 1 == obj->path->num_sections) {
    /* Section is last in path */
    obj->path->num_sections--;

  } else {
    /* Remove section at index by moving all sections above index one
     * location down. */
    memmove (&obj->path->sections[idx],
             &obj->path->sections[idx+1],
             sizeof (LeptonPathSection) * (obj->path->num_sections - idx - 1));
    obj->path->num_sections--;
  }

  lepton_object_emit_change_notify (obj);
  lepton_object_page_set_changed (obj);

  return obj_s;
}

/*! \brief Insert an element into a path.
 * \par Function Description
 * Inserts a path element into the path object \a obj_s at index \a
 * index_s.  The type of element to be inserted is specified by the
 * symbol \a type_s, and the remaining optional integer arguments
 * provide as many absolute coordinate pairs as are required by that
 * element type:
 *
 * - "closepath" elements require no coordinate arguments;
 * - "moveto" and "lineto" elements require one coordinate pair, for
 *   the endpoint;
 * - "curveto" elements require the coordinates of the first control
 *   point, coordinates of the second control point, and coordinates
 *   of the endpoint.
 *
 * If the index is negative, or is greater than or equal to the number
 * of elements currently in the path, the new element will be appended
 * to the path.
 *
 * \note Scheme API: Implements the %path-insert! procedure of the
 * (lepton core object) module.
 *
 * \param obj_s   #LeptonObject smob for the path object to modify.
 * \param index_s Index at which to insert new element.
 * \param type_s  Symbol indicating what type of element to insert.
 * \param x1_s    X-coordinate of first coordinate pair.
 * \param y1_s    Y-coordinate of first coordinate pair.
 * \param x2_s    X-coordinate of second coordinate pair.
 * \param y2_s    Y-coordinate of second coordinate pair.
 * \param x3_s    X-coordinate of third coordinate pair.
 * \param y3_s    Y-coordinate of third coordinate pair.
 * \return \a obj_s.
 */
SCM_DEFINE (path_insert_x, "%path-insert", 3, 6, 0,
            (SCM obj_s, SCM index_s, SCM type_s,
             SCM x1_s, SCM y1_s, SCM x2_s, SCM y2_s, SCM x3_s, SCM y3_s),
            "Insert a path element into a path object.")
{
  SCM_ASSERT (edascm_is_object_type (obj_s, OBJ_PATH), obj_s,
              SCM_ARG1, s_path_insert_x);
  SCM_ASSERT (scm_is_integer (index_s), index_s, SCM_ARG2, s_path_insert_x);
  SCM_ASSERT (scm_is_symbol (type_s), type_s, SCM_ARG3, s_path_insert_x);

  LeptonObject *obj = edascm_to_object (obj_s);
  LeptonPath *path = obj->path;
  LeptonPathSection section = {(PATH_CODE) 0, 0, 0, 0, 0, 0, 0};

  /* Check & extract path element type. */
  if      (scm_is_eq (type_s, closepath_sym)) { section.code = PATH_END;     }
  else if (scm_is_eq (type_s, moveto_sym))    { section.code = PATH_MOVETO;  }
  else if (scm_is_eq (type_s, lineto_sym))    { section.code = PATH_LINETO;  }
  else if (scm_is_eq (type_s, curveto_sym))   { section.code = PATH_CURVETO; }
  else {
    scm_misc_error (s_path_insert_x,
                    _("Invalid path element type ~A."),
                    scm_list_1 (type_s));
  }

  /* Check the right number of coordinates have been provided. */
  switch (section.code) {
  case PATH_CURVETO:
    SCM_ASSERT (scm_is_integer (x1_s), x1_s, SCM_ARG4, s_path_insert_x);
    section.x1 = scm_to_int (x1_s);
    SCM_ASSERT (scm_is_integer (y1_s), y1_s, SCM_ARG5, s_path_insert_x);
    section.y1 = scm_to_int (y1_s);
    SCM_ASSERT (scm_is_integer (x2_s), x2_s, SCM_ARG6, s_path_insert_x);
    section.x2 = scm_to_int (x2_s);
    SCM_ASSERT (scm_is_integer (y2_s), y2_s, SCM_ARG7, s_path_insert_x);
    section.y2 = scm_to_int (y2_s);
    SCM_ASSERT (scm_is_integer (x3_s), x3_s, 8, s_path_insert_x);
    section.x3 = scm_to_int (x3_s);
    SCM_ASSERT (scm_is_integer (y3_s), y3_s, 9, s_path_insert_x);
    section.y3 = scm_to_int (y3_s);
    break;
  case PATH_MOVETO:
  case PATH_MOVETO_OPEN:
  case PATH_LINETO:
    SCM_ASSERT (scm_is_integer (x1_s), x1_s, SCM_ARG4, s_path_insert_x);
    section.x3 = scm_to_int (x1_s);
    SCM_ASSERT (scm_is_integer (y1_s), y1_s, SCM_ARG5, s_path_insert_x);
    section.y3 = scm_to_int (y1_s);
    break;
  case PATH_END:
    break;
  }

  /* Start making changes */
  lepton_object_emit_pre_change_notify (obj);

  /* Make sure there's enough space for the new element */
  if (path->num_sections == path->num_sections_max) {
    path->sections =
      (LeptonPathSection*) g_realloc (path->sections,
                                      (path->num_sections_max <<= 1) *
                                      sizeof (LeptonPathSection));
  }

  /* Move path contents to make a gap in the right place. */
  int idx = scm_to_int (index_s);

  if ((idx < 0) || (idx > path->num_sections)) {
    idx = path->num_sections;
  } else {
    memmove (&path->sections[idx+1], &path->sections[idx],
             sizeof (LeptonPathSection) * (path->num_sections - idx));
  }

  path->num_sections++;
  path->sections[idx] = section;

  lepton_object_emit_change_notify (obj);
  lepton_object_page_set_changed (obj);

  return obj_s;
}

/*! \brief Create a new, empty picture object.
 * \par Function Description
 * Creates a new picture object with no filename, no image data and
 * all other parameters set to default values.  It is initially set to
 * be embedded.
 *
 * \note Scheme API: Implements the %make-picture procedure in the
 * (lepton core object) module.
 *
 * \return a newly-created picture object.
 */
SCM_DEFINE (make_picture, "%make-picture", 0, 0, 0, (),
            "Create a new picture object")
{
  LeptonObject *obj = lepton_picture_object_new (NULL,
                                                 0,
                                                 NULL,
                                                 OBJ_PICTURE,
                                                 0,
                                                 0,
                                                 0,
                                                 0,
                                                 0,
                                                 FALSE,
                                                 TRUE);
  SCM result = edascm_from_object (obj);

  /* At the moment, the only pointer to the object is owned by the
   * smob. */
  edascm_c_set_gc (result, 1);

  return result;
}

/*! \brief Get picture object parameters.
 * \par Function Description
 * Retrieves the parameters of a picture object.  The return value is
 * a list of parameters:
 *
 * -# Filename of picture.
 * -# X-coordinate of top left of picture.
 * -# Y-coordinate of top left of picture.
 * -# X-coordinate of bottom right of picture.
 * -# Y-coordinate of bottom right of picture.
 * -# Rotation angle.
 * -# Whether object is mirrored.
 *
 * \note Scheme API: Implements the %picture-info procedure in the
 * (lepton core object) module.
 *
 * \param obj_s the picture object to inspect.
 * \return a list of picture object parameters.
 */
SCM_DEFINE (picture_info, "%picture-info", 1, 0, 0,
            (SCM obj_s), "Get picture object parameters")
{
  SCM_ASSERT (edascm_is_object_type (obj_s, OBJ_PICTURE), obj_s,
              SCM_ARG1, s_picture_info);

  LeptonObject *obj = edascm_to_object (obj_s);
  const gchar *filename = lepton_picture_object_get_filename (obj);

  SCM filename_s = SCM_BOOL_F;
  if (filename != NULL) {
    filename_s = scm_from_utf8_string (filename);
  }

  return scm_list_n (filename_s,
                     scm_from_int (lepton_picture_object_get_upper_x (obj)),
                     scm_from_int (lepton_picture_object_get_upper_y (obj)),
                     scm_from_int (lepton_picture_object_get_lower_x (obj)),
                     scm_from_int (lepton_picture_object_get_lower_y (obj)),
                     scm_from_int (lepton_picture_object_get_angle (obj)),
                     (lepton_picture_object_get_mirrored (obj) ? SCM_BOOL_T : SCM_BOOL_F),
                     SCM_UNDEFINED);
}

/* \brief Set picture object parameters.
 * \par Function Description
 * Sets the parameters of the picture object \a obj_s.
 *
 * \note Scheme API: Implements the %set-picture! procedure in the
 * (lepton core object) module.
 *
 * \param obj_s       the picture object to modify
 * \param x1_s  the new x-coordinate of the top left of the picture.
 * \param y1_s  the new y-coordinate of the top left of the picture.
 * \param x2_s  the new x-coordinate of the bottom right of the picture.
 * \param y2_s  the new y-coordinate of the bottom right of the picture.
 * \param angle_s     the new rotation angle.
 * \param mirror_s    whether the picture object should be mirrored.
 * \return the modify \a obj_s.
 */
SCM_DEFINE (set_picture_x, "%set-picture!", 7, 0, 0,
            (SCM obj_s, SCM x1_s, SCM y1_s, SCM x2_s, SCM y2_s, SCM angle_s,
             SCM mirror_s), "Set picture object parameters")
{
  SCM_ASSERT (edascm_is_object_type (obj_s, OBJ_PICTURE), obj_s,
              SCM_ARG1, s_set_picture_x);
  SCM_ASSERT (scm_is_integer (x1_s), x1_s, SCM_ARG2, s_set_picture_x);
  SCM_ASSERT (scm_is_integer (y1_s), x1_s, SCM_ARG3, s_set_picture_x);
  SCM_ASSERT (scm_is_integer (x2_s), x1_s, SCM_ARG4, s_set_picture_x);
  SCM_ASSERT (scm_is_integer (y2_s), x1_s, SCM_ARG5, s_set_picture_x);
  SCM_ASSERT (scm_is_integer (angle_s), angle_s, SCM_ARG6, s_set_picture_x);

  LeptonObject *obj = edascm_to_object (obj_s);

  /* Angle */
  int angle = scm_to_int (angle_s);
  switch (angle) {
  case 0:
  case 90:
  case 180:
  case 270:
    /* These are all fine. */
    break;
  default:
    /* Otherwise, not fine. */
    scm_misc_error (s_set_picture_x,
                    _("Invalid picture angle ~A. Must be 0, 90, 180, or 270 degrees"),
                    scm_list_1 (angle_s));
  }

  lepton_object_emit_pre_change_notify (obj);

  lepton_picture_object_set_angle (obj, scm_to_int (angle_s));
  lepton_picture_object_set_mirrored (obj, scm_is_true (mirror_s));
  lepton_picture_object_modify_all (obj,
                                    scm_to_int (x1_s),
                                    scm_to_int (y1_s),
                                    scm_to_int (x2_s),
                                    scm_to_int (y2_s));

  lepton_object_emit_change_notify (obj);
  return obj_s;
}

/*! \brief Set a picture object's data from a vector.
 * \par Function Description
 * Sets the image data for the picture object \a obj_s from the vector
 * \a data_s, and set its \a filename.  If the contents of \a data_s
 * could not be successfully loaded as an image, raises an error.  The
 * contents of \a data_s should be image data encoded in on-disk
 * format.
 *
 * \note Scheme API: Implements the %set-picture-data/vector!
 * procedure in the (lepton core object) module.
 *
 * \param obj_s       The picture object to modify.
 * \param data_s      Vector containing encoded image data.
 * \param filename_s  New filename for \a obj_s.
 * \return \a obj_s.
 */
SCM_DEFINE (set_picture_data_vector_x, "%set-picture-data/vector!",
            3, 0, 0, (SCM obj_s, SCM data_s, SCM filename_s),
            "Set a picture object's data from a vector.")
{
  SCM vec_s = scm_any_to_s8vector (data_s);
  /* Check argument types */
  SCM_ASSERT (edascm_is_object_type (obj_s, OBJ_PICTURE), obj_s,
              SCM_ARG1, s_set_picture_data_vector_x);
  SCM_ASSERT (scm_is_true (scm_s8vector_p (vec_s)), data_s, SCM_ARG2,
              s_set_picture_data_vector_x);
  SCM_ASSERT (scm_is_string (filename_s), filename_s, SCM_ARG3,
              s_set_picture_data_vector_x);

  scm_dynwind_begin ((scm_t_dynwind_flags) 0);

  /* Convert vector to contiguous buffer */
  scm_t_array_handle handle;
  size_t len;
  ssize_t inc;
  const int8_t *elt = scm_s8vector_elements (vec_s, &handle, &len, &inc);
  gchar *buf = (gchar*) g_malloc (len);
  guint i;

  scm_dynwind_unwind_handler (g_free, buf, SCM_F_WIND_EXPLICITLY);

  for (i = 0; i < len; i++, elt += inc) {
    buf[i] = (gchar) *elt;
  }
  scm_array_handle_release (&handle);

  gboolean status;
  GError *error = NULL;
  LeptonObject *obj = edascm_to_object (obj_s);
  gchar *filename = scm_to_utf8_string (filename_s);
  scm_dynwind_unwind_handler (g_free, filename, SCM_F_WIND_EXPLICITLY);

  status = lepton_picture_object_set_from_buffer (obj,
                                                  filename,
                                                  buf,
                                                  len,
                                                  &error);
  if (!status) {
    scm_dynwind_unwind_handler ((void (*)(void *)) g_error_free, error,
                                SCM_F_WIND_EXPLICITLY);
    scm_misc_error (s_set_picture_data_vector_x,
                    "Failed to set picture image data from vector: ~S",
                    scm_list_1 (scm_from_utf8_string (error->message)));
  }

  lepton_object_page_set_changed (obj);
  scm_dynwind_end ();
  return obj_s;
}

/*!
 * \brief Create the (lepton core object) Scheme module.
 * \par Function Description
 * Defines procedures in the (lepton core object) module. The module can
 * be accessed using (use-modules (lepton core object)).
 */
static void
init_module_lepton_core_object (void *unused)
{
  /* Register the functions and symbols */
  #include "scheme_object.x"

  /* Add them to the module's public definitions. */
  scm_c_export (s_make_line,
                s_make_net,
                s_make_bus,
                s_make_pin,
                s_pin_type,
                s_set_line_x,
                s_line_info,
                s_make_circle,
                s_set_circle_x,
                s_circle_info,
                s_make_text,
                s_set_text_x,
                s_text_info,
                s_object_connections,
                s_make_path,
                s_path_length,
                s_path_ref,
                s_path_remove_x,
                s_path_insert_x,
                s_make_picture,
                s_picture_info,
                s_set_picture_x,
                s_set_picture_data_vector_x,
                NULL);
}

/*!
 * \brief Initialise the basic Lepton EDA object manipulation procedures.
 * \par Function Description
 * Registers some Scheme procedures for working with #LeptonObject
 * smobs. Should only be called by edascm_init().
 */
void
edascm_init_object ()
{
  /* Define the (lepton core object) module */
  scm_c_define_module ("lepton core object",
                       (void (*) (void*)) init_module_lepton_core_object,
                       NULL);
}
