/* Lepton EDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2016 gEDA Contributors
 * Copyright (C) 2017-2024 Lepton EDA Contributors
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
#include <config.h>

#include <stdio.h>
#include <math.h>

#include "gschem.h"

#define GET_BOX_WIDTH(w)  abs((w)->second_wx - (w)->first_wx)
#define GET_BOX_HEIGHT(w) abs((w)->second_wy - (w)->first_wy)

#define GET_PICTURE_WIDTH(w)                    \
  abs((w)->second_wx - (w)->first_wx)
#define GET_PICTURE_HEIGHT(w)                                           \
  (w)->pixbuf_wh_ratio == 0 ? 0 : abs((w)->second_wx - (w)->first_wx)/(w)->pixbuf_wh_ratio
#define GET_PICTURE_LEFT(w)                     \
  MIN((w)->first_wx, (w)->second_wx)
#define GET_PICTURE_TOP(w)                                              \
  (w)->first_wy > (w)->second_wy ? (w)->first_wy  :                     \
  (w)->first_wy+abs((w)->second_wx - (w)->first_wx)/(w)->pixbuf_wh_ratio


/*! \brief Check if point is inside grip.
 *  \par Function Description
 *  This function is used to determine if the (<B>x</B>,<B>y</B>) point is
 *  inside a grip of one of the selected object on the current sheet.
 *  The <B>x</B> and <B>y</B> parameters are in world units.
 *  If the point is inside one grip, a pointer on the object it belongs to is
 *  returned and <B>*whichone</B> is set according to the position of the grip
 *  on the object.
 *  Else, <B>*whichone</B> is unchanged and the function returns <B>NULL</B>.
 *
 *  A specific search function is provided for every kind of graphical object.
 *  The list of selected object is covered : each object is tested with the
 *  appropriate function.
 *
 *  \param [in]  w_current  The GschemToplevel object.
 *  \param [in]  x          Current x coordinate of pointer in world units.
 *  \param [in]  y          Current y coordinate of pointer in world units.
 *  \param [out] whichone   Which grip point is selected.
 *  \return Pointer to LeptonObject the grip is on, NULL otherwise.
 */
LeptonObject *o_grips_search_world(GschemToplevel *w_current, int x, int y, int *whichone)
{
  LeptonObject *object=NULL;
  LeptonObject *found=NULL;
  GList *s_current;
  int size;
  int w_size;

  GschemPageView *page_view = gschem_toplevel_get_current_page_view (w_current);
  g_return_val_if_fail (page_view != NULL, NULL);

  LeptonPage *active_page = schematic_window_get_active_page (w_current);

  if (!whichone) {
    return(NULL);
  }

  /* get the size of the grip according to zoom level */
  size = GRIP_SIZE / 2;
  w_size = gschem_page_view_WORLDabs (page_view, size);

  s_current = lepton_list_get_glist (active_page->selection_list );
  while (s_current != NULL) {
    object = (LeptonObject *) s_current->data;
    if (object) {
      switch (lepton_object_get_type (object)) {
        case(OBJ_ARC):
          /* check the grips of the arc object */
          found = o_grips_search_arc_world(w_current, object,
                                           x, y, w_size, whichone);
          if(found != NULL) return found;
          break;

        case(OBJ_BOX):
          /* check the grips of the box object */
          found = o_grips_search_box_world(w_current, object,
                                           x, y, w_size, whichone);
          if(found != NULL) return found;
          break;

        case(OBJ_PATH):
          /* check the grips of the path object */
          found = o_grips_search_path_world(w_current, object,
                                            x, y, w_size, whichone);
          if(found != NULL) return found;
          break;

        case(OBJ_PICTURE):
          /* check the grips of the picture object */
          found = o_grips_search_picture_world(w_current, object,
                                               x, y, w_size, whichone);
          if(found != NULL) return found;
          break;

        case(OBJ_CIRCLE):
          /* check the grips of the circle object */
          found = o_grips_search_circle_world(w_current, object,
                                              x, y, w_size, whichone);
          if(found != NULL) return found;
          break;

        case(OBJ_LINE):
        case(OBJ_PIN):
        case(OBJ_NET):
        case(OBJ_BUS):
          /* check the grips of the line object */
          /* the function is the same for line, pin, net, bus */
          found = o_grips_search_line_world(w_current, object,
                                            x, y, w_size, whichone);
          if(found != NULL) return found;
          break;

        default:
          break;
      }
    }
    s_current = g_list_next(s_current);
  }

  return(NULL);
}


/*! \brief Check if pointer is inside the grip region.
 *
 *  \par Function Description
 *  This function checks if the point (<B>x</B>,<B>y</B>) is
 *  inside the grip centered at (<B>grip_x</B>,<B>grip_y</B>).
 *
 *  \param [in]  x          Current x coordinate of pointer in world units.
 *  \param [in]  y          Current y coordinate of pointer in world units.
 *  \param [in]  grip_x     Current x coordinate of grip center in world units.
 *  \param [in]  grip_y     Current y coordinate of grip center in world units.
 *  \param [in]  size       Half the width of the grip square in world units.
 *  \return True / False whether the mouse pointer is inside the grip.
 */
static gboolean inside_grip( int x, int y, int grip_x, int grip_y, int size )
{
  int xmin, ymin, xmax, ymax;

  xmin = grip_x - size;
  ymin = grip_y - size;
  xmax = xmin + 2 * size;
  ymax = ymin + 2 * size;

  return inside_region(xmin, ymin, xmax, ymax, x, y);
}

/*! \brief Check if pointer is inside arc grip.
 *  \par Function Description
 *  This function checks if the pointer event occuring at (<B>x</B>,<B>y</B>) is
 *  inside one of the grips of an <B>o_current</B> pointed arc object. If so
 *  the <B>whichone</B> pointed integer is set to the number of this grip and
 *  the return pointer is a pointer on this object. If the point is not
 *  inside a grip the function returns a NULL pointer and the <B>whichone</B>
 *  pointed integer is unset.
 *
 *  An arc object has three grips :
 *  <DL>
 *    <DT>*</DT><DD>one at the center of the arc. This grip is used to modify
 *                  the radius of the arc. If this one is selected, the
 *                  <B>whichone</B> pointed integer is set to <B>ARC_RADIUS</B>.
 *    <DT>*</DT><DD>one at one end of the arc. It corresponds to the starting
 *                  angle of the arc. If this one is selected, the
 *                  <B>whichone</B> pointed integer is set to <B>ARC_START_ANGLE</B>.
 *    <DT>*</DT><DD>one at the other end of the arc. It corresponds to the
 *                  ending angle of the arc. If this one is selected, the
 *                  <B>whichone</B> pointed integer is set to <B>ARC_SWEEP_ANGLE</B>.
 *  </DL>
 *
 *  The <B>x</B> and <B>y</B> parameters are in world units.
 *
 *  The <B>size</B> parameter is the width (and height) of the square
 *  representing a grip in world units.
 *
 *  \param [in]  w_current  The GschemToplevel object.
 *  \param [in]  o_current  Arc LeptonObject to check.
 *  \param [in]  x          Current x coordinate of pointer in world units.
 *  \param [in]  y          Current y coordinate of pointer in world units.
 *  \param [in]  size       Half the width of the grip square in world units.
 *  \param [out] whichone   Which grip point is selected.
 *  \return Pointer to LeptonObject the grip is on, NULL otherwise.
 */
LeptonObject *o_grips_search_arc_world(GschemToplevel *w_current, LeptonObject *o_current,
                                 int x, int y, int size, int *whichone)
{
  int centerx, centery, radius, start_angle, sweep_angle;
  double tmp;

  centerx     = lepton_arc_object_get_center_x (o_current);
  centery     = lepton_arc_object_get_center_y (o_current);
  radius      = lepton_arc_object_get_radius (o_current);
  start_angle = lepton_arc_object_get_start_angle (o_current);
  sweep_angle = lepton_arc_object_get_sweep_angle (o_current);

  /* check the grip on the center of the arc */
  if (inside_grip(x, y, centerx, centery, size)) {
    *whichone = ARC_RADIUS;
    return(o_current);
  }

  /* check the grip at the end angle of the arc */
  tmp = ((double) start_angle + sweep_angle) * M_PI / 180;
  if (inside_grip(x, y,
                  centerx + radius * cos(tmp),
                  centery + radius * sin(tmp), size)) {
    *whichone = ARC_SWEEP_ANGLE;
    return(o_current);
  }

  /* check the grip at the start angle of the arc */
  tmp = ((double) start_angle) * M_PI / 180;
  if (inside_grip(x, y,
                  centerx + radius * cos(tmp),
                  centery + radius * sin(tmp), size)) {
    *whichone = ARC_START_ANGLE;
    return(o_current);
  }

  return NULL;
}

/*! \brief Check if pointer is inside box grip.
 *  \par Function Description
 *  This function checks if the pointer event occuring at (<B>x</B>,<B>y</B>) is
 *  inside one of the grips of the <B>o_current</B> pointed box object.
 *  If so, the <B>whichone</B> pointed integer is set to the identifier of
 *  this grip and the returned pointer is a pointer on this object.
 *  If the point is not inside a grip the function returns a NULL pointer
 *  and the <B>whichone</B> pointed integer is unset.
 *
 *  A box object has four grips : one at each corner of the box. The
 *  identifiers of each corner are <B>BOX_UPPER_LEFT</B>,
 *  <B>BOX_UPPER_RIGHT</B>, <B>BOX_LOWER_LEFT</B> and <B>BOX_LOWER_RIGHT</B>.
 *
 *  The <B>x</B> and <B>y</B> parameters are in world units.
 *
 *  The <B>size</B> parameter is half the width (and half the height) of
 *  the square representing a grip in world units.
 *
 *  \param [in]  w_current  The GschemToplevel object.
 *  \param [in]  o_current  Box LeptonObject to check.
 *  \param [in]  x          Current x coordinate of pointer in world units.
 *  \param [in]  y          Current y coordinate of pointer in world units.
 *  \param [in]  size       Half the width of the grip square in world units.
 *  \param [out] whichone   Which grip point is selected.
 *  \return Pointer to LeptonObject the grip is on, NULL otherwise.
 */
LeptonObject *o_grips_search_box_world(GschemToplevel *w_current, LeptonObject *o_current,
                                 int x, int y, int size, int *whichone)
{
  int upper_x, upper_y, lower_x, lower_y;

  upper_x = lepton_box_object_get_upper_x (o_current);
  upper_y = lepton_box_object_get_upper_y (o_current);
  lower_x = lepton_box_object_get_lower_x (o_current);
  lower_y = lepton_box_object_get_lower_y (o_current);

  /* inside upper left grip ? */
  if (inside_grip (x, y, upper_x, upper_y, size))
  {
    *whichone = BOX_UPPER_LEFT;
    return(o_current);
  }

  /* inside lower right grip ? */
  if (inside_grip (x, y, lower_x, lower_y, size))
  {
    *whichone = BOX_LOWER_RIGHT;
    return(o_current);
  }

  /* inside upper right grip ? */
  if (inside_grip (x, y, lower_x, upper_y, size))
  {
    *whichone = BOX_UPPER_RIGHT;
    return(o_current);
  }

  /* inside lower left grip ? */
  if (inside_grip (x, y, upper_x, lower_y, size))
  {
    *whichone = BOX_LOWER_LEFT;
    return(o_current);
  }

  return NULL;
}

/*! \brief Check if pointer is inside path grip.
 *  \par Function Description
 *  This function checks if the pointer event occuring at (<B>x</B>,<B>y</B>)
 *  is inside one of the grips of the <B>o_current</B> pointed path object.
 *  If so, the <B>whichone</B> pointed integer is set to the identifier of
 *  this grip and the returned pointer is a pointer on this object.
 *  If the point is not inside a grip the function returns a NULL pointer
 *  and the <B>whichone</B> pointed integer is unset.
 *
 *  A path object may have several grips depending on the number
 *  of its elements.  If a path has N elements and M elements of
 *  them are curve ones, the number of position (square) grips is
 *  N+1 and the number of curve control (round) grips may achieve
 *  2*M (since not all curve elements may have 2 grips).
 *
 *  The <B>x</B> and <B>y</B> parameters are in world units.
 *
 *  The <B>size</B> parameter is half the width (and half the height) of the
 *  square representing a grip in world units.
 *
 *  \param [in]  w_current  The GschemToplevel object.
 *  \param [in]  o_current  Picture LeptonObject to check.
 *  \param [in]  x          Current x coordinate of pointer in world units.
 *  \param [in]  y          Current y coordinate of pointer in world units.
 *  \param [in]  size       Half the width of the grip square in world units.
 *  \param [out] whichone   Which grip point is selected.
 *  \return Pointer to LeptonObject the grip is on, NULL otherwise.
 */
LeptonObject *o_grips_search_path_world(GschemToplevel *w_current, LeptonObject *o_current,
                                     int x, int y, int size, int *whichone)
{
  LeptonPathSection *section;
  int i;
  int grip_no = 0;

  for (i = 0; i < lepton_path_object_get_num_sections (o_current); i++)
  {
    section = lepton_path_object_get_section (o_current, i);

    switch (section->code) {
    case PATH_CURVETO:
      /* inside first control grip ? */
      if (inside_grip(x, y, section->x1, section->y1, size)) {
        *whichone = grip_no;
        return o_current;
      }
      grip_no ++;
      /* inside second control grip ? */
      if (inside_grip(x, y, section->x2, section->y2, size)) {
        *whichone = grip_no;
        return o_current;
      }
      grip_no ++;
      /* Fall through */
    case PATH_MOVETO:
    case PATH_MOVETO_OPEN:
    case PATH_LINETO:
      /* inside destination control grip ? */
      if (inside_grip(x, y, section->x3, section->y3, size)) {
        *whichone = grip_no;
        return o_current;
      }
      grip_no ++;
      break;
    case PATH_END:
      break;
    }
  }

  return NULL;
}

/*! \brief Check if pointer is inside picture grip.
 *  \par Function Description
 *  This function checks if the pointer event occuring at (<B>x</B>,<B>y</B>)
 *  is inside one of the grips of the <B>o_current</B> pointed picture object.
 *  If so, the <B>whichone</B> pointed integer is set to the identifier of
 *  this grip and the returned pointer is a pointer on this object.
 *  If the point is not inside a grip the function returns a NULL pointer
 *  and the <B>whichone</B> pointed integer is unset.
 *
 *  A picture object has four grips: one at each corner of the
 *  picture.  The identifiers of each corner are \c
 *  PICTURE_UPPER_LEFT, \c PICTURE_UPPER_RIGHT, \c
 *  PICTURE_LOWER_LEFT and \c PICTURE_LOWER_RIGHT.
 *
 *  The <B>x</B> and <B>y</B> parameters are in world units.
 *
 *  The <B>size</B> parameter is half the width (and half the height) of the
 *  square representing a grip in world units.
 *
 *  \param [in]  w_current  The GschemToplevel object.
 *  \param [in]  o_current  Picture LeptonObject to check.
 *  \param [in]  x          Current x coordinate of pointer in world units.
 *  \param [in]  y          Current y coordinate of pointer in world units.
 *  \param [in]  size       Half the width of the grip square in world units.
 *  \param [out] whichone   Which grip point is selected.
 *  \return Pointer to LeptonObject the grip is on, NULL otherwise.
 */
LeptonObject *o_grips_search_picture_world(GschemToplevel *w_current, LeptonObject *o_current,
                                     int x, int y, int size, int *whichone)
{
  int lower_x, lower_y, upper_x, upper_y;

  lower_x = lepton_picture_object_get_lower_x (o_current);
  lower_y = lepton_picture_object_get_lower_y (o_current);
  upper_x = lepton_picture_object_get_upper_x (o_current);
  upper_y = lepton_picture_object_get_upper_y (o_current);

  /* inside upper left grip ? */
  if (inside_grip (x, y, upper_x, upper_y, size))
  {
    *whichone = PICTURE_UPPER_LEFT;
    return(o_current);
  }

  /* inside lower right grip ? */
  if (inside_grip (x, y, lower_x, lower_y, size))
  {
    *whichone = PICTURE_LOWER_RIGHT;
    return(o_current);
  }

  /* inside upper right grip ? */
  if (inside_grip (x, y, lower_x, upper_y, size))
  {
    *whichone = PICTURE_UPPER_RIGHT;
    return(o_current);
  }

  /* inside lower left grip ? */
  if (inside_grip (x, y, upper_x, lower_y, size))
  {
    *whichone = PICTURE_LOWER_LEFT;
    return(o_current);
  }

  return NULL;
}

/*! \brief Check if pointer is inside circle grip.
 *  \par Function Description
 *  This function determines if the (<B>x</B>,<B>y</B>) point is inside one of
 *  the grip of the circle object <B>o_current</B>.
 *  It computes the area covered by each grip and check if (<B>x</B>,<B>y</B>)
 *  is in one of these areas.
 *  If the event occured in one of the grip, a pointer on the object is
 *  returned and <B>*whichone</B> is set to the identifier of the grip.
 *  If not, the function returns a <B>NULL</B> pointer and <B>*whichone</B>
 *  is unchanged.
 *
 *  The parameter <B>size</B> is half the size of the grip in world units.
 *
 *  A circle has only one grip on the lower right corner of the box it
 *  is inscribed in. Moving this grip change the radius of the circle.
 *  The identifier of this grip is <B>CIRCLE_RADIUS</B>.
 *
 *  \param [in]  w_current  The GschemToplevel object.
 *  \param [in]  o_current  Circle LeptonObject to check.
 *  \param [in]  x          Current x coordinate of pointer in world units.
 *  \param [in]  y          Current y coordinate of pointer in world units.
 *  \param [in]  size       Half the width of the grip square in world units.
 *  \param [out] whichone   Which grip point is selected.
 *  \return Pointer to LeptonObject the grip is on, NULL otherwise.
 */
LeptonObject *o_grips_search_circle_world(GschemToplevel *w_current, LeptonObject *o_current,
                                    int x, int y, int size, int *whichone)
{
  gint center_x = lepton_circle_object_get_center_x (o_current);
  gint center_y = lepton_circle_object_get_center_y (o_current);
  gint radius = lepton_circle_object_get_radius (o_current);

  /* check the grip for radius */
  if (inside_grip(x, y,
                  center_x + radius,
                  center_y - radius,
                  size)) {
    *whichone = CIRCLE_RADIUS;
    return(o_current);
  }

  return NULL;
}

/*! \brief Check if pointer is inside line grip.
 *  \par Function Description
 *  This function determines if the (<B>x</B>,<B>y</B>) point is inside one of
 *  the grip of the line object <B>o_current</B>.
 *  It computes the area covered by each grip and check if (<B>x</B>,<B>y</B>)
 *  is in one of these areas.
 *  If the event occured in one of its grip, a pointer on the object is
 *  returned and <B>*whichone</B> is set to the identifier of the grip. If not,
 *  the function returns <B>NULL</B> pointer and <B>*whichone</B> is unchanged.
 *
 *  The parameter <B>size</B> is half the size of the grip in world units.
 *
 *  \param [in]  w_current  The GschemToplevel object.
 *  \param [in]  o_current  Line LeptonObject to check.
 *  \param [in]  x          Current x coordinate of pointer in world units.
 *  \param [in]  y          Current y coordinate of pointer in world units.
 *  \param [in]  size       Half the width of the grip square in world units.
 *  \param [out] whichone   Which grip point is selected.
 *  \return Pointer to LeptonObject the grip is on, NULL otherwise.
 */
LeptonObject *o_grips_search_line_world(GschemToplevel *w_current, LeptonObject *o_current,
                                  int x, int y, int size, int *whichone)
{
  /* check the grip on the end of line 1 */
  if (inside_grip(x, y,
                  o_current->line->x[LINE_END1],
                  o_current->line->y[LINE_END1], size)) {
    *whichone = LINE_END1;
    return(o_current);
  }

  /* check the grip on the end of line 2 */
  if (inside_grip(x, y,
                  o_current->line->x[LINE_END2],
                  o_current->line->y[LINE_END2], size)) {
    *whichone = LINE_END2;
    return(o_current);
  }

  return NULL;
}

/*! \brief Initialize grip motion process for an arc.
 *  \par Function Description
 *  This function initializes the grip motion process for an arc.
 *  From the <B>o_current</B> pointed object, it stores into the
 *  GschemToplevel structure the coordinates of the center, the radius
 *  and the two angle that describes an arc. These variables are used in
 *  the grip process.
 *
 *  The coordinates of the center of the arc on x- and y-axis are stored
 *  into the <B>first_wx</B> and <B>first_wy</B> fields of the GschemToplevel
 *  structure in screen units.
 *
 *  The radius of the center is stored into the <B>distance</B> field of
 *  the GschemToplevel structure in screen units.
 *
 *  The two angles describing the arc on a circle are stored into the
 *  <B>second_wx</B> for the starting angle and <B>second_wy</B> for the ending angle.
 *  These angles are expressed in degrees.
 *
 *  \param [in]  w_current  The GschemToplevel object.
 *  \param [in]  o_current  Arc LeptonObject to check.
 *  \param [in]  x          (unused)
 *  \param [in]  y          (unused)
 *  \param [out] whichone   (unused)
 */
static void o_grips_start_arc(GschemToplevel *w_current, LeptonObject *o_current,
                              int x, int y, int whichone)
{
  /* describe the arc with GschemToplevel variables */
  /* center */
  w_current->first_wx = lepton_arc_object_get_center_x (o_current);
  w_current->first_wy = lepton_arc_object_get_center_y (o_current);
  /* radius */
  w_current->distance = lepton_arc_object_get_radius (o_current);
  /* angles */
  w_current->second_wx = lepton_arc_object_get_start_angle (o_current);
  w_current->second_wy = lepton_arc_object_get_sweep_angle (o_current);

  /* draw the first temporary arc */
  /* o_arc_invalidate_rubber (w_current); */
  schematic_window_set_rubber_visible (w_current, 1);
}

/*! \brief Initialize grip motion process for a box.
 *  \par Function Description
 *  This function initializes the grip motion process for a box. From the
 *  <B>o_current</B> pointed object, it stores into the GschemToplevel
 *  structure the .... These variables are used in the grip process.
 *
 *  The function first erases the grips.
 *
 *  The coordinates of the selected corner are put in
 *  (<B>w_current->second_wx</B>,<B>w_current->second_wx</B>).
 *
 *  The coordinates of the opposite corner go in
 *  (<B>w_current->first_wx</B>,<B>w_current->first_wy</B>). They are not suppose
 *  to change during the action.
 *
 *  \param [in]  w_current  The GschemToplevel object.
 *  \param [in]  o_current  Box LeptonObject to check.
 *  \param [in]  x          (unused)
 *  \param [in]  y          (unused)
 *  \param [out] whichone   Which coordinate to check.
 */
static void o_grips_start_box(GschemToplevel *w_current, LeptonObject *o_current,
                              int x, int y, int whichone)
{
  int upper_x, upper_y, lower_x, lower_y;

  upper_x = lepton_box_object_get_upper_x (o_current);
  upper_y = lepton_box_object_get_upper_y (o_current);
  lower_x = lepton_box_object_get_lower_x (o_current);
  lower_y = lepton_box_object_get_lower_y (o_current);

  /* (second_wx, second_wy) is the selected corner */
  /* (first_wx, first_wy) is the opposite corner */
  switch(whichone) {
    case BOX_UPPER_LEFT:
      w_current->second_wx = upper_x;
      w_current->second_wy = upper_y;
      w_current->first_wx  = lower_x;
      w_current->first_wy  = lower_y;
      break;
    case BOX_LOWER_RIGHT:
      w_current->second_wx = lower_x;
      w_current->second_wy = lower_y;
      w_current->first_wx  = upper_x;
      w_current->first_wy  = upper_y;
      break;
    case BOX_UPPER_RIGHT:
      w_current->second_wx = lower_x;
      w_current->second_wy = upper_y;
      w_current->first_wx  = upper_x;
      w_current->first_wy  = lower_y;
      break;
    case BOX_LOWER_LEFT:
      w_current->second_wx = upper_x;
      w_current->second_wy = lower_y;
      w_current->first_wx  = lower_x;
      w_current->first_wy  = upper_y;
      break;
    default:
      return; /* error */
  }

  /* draw the first temporary box */
  /* o_box_invalidate_rubber (w_current); */
  schematic_window_set_rubber_visible (w_current, 1);
}

/*! \brief Initialize grip motion process for a path.
 *  \par Function Description
 *  This function initializes the grip motion process for a path.
 *  From the <B>o_current</B> pointed object, it stores into the
 *  GschemToplevel structure the ....
 *  These variables are used in the grip process.
 *
 *  The function first erases the grips.
 *
 *  The coordinates of the selected corner are put in
 *  (<B>w_current->second_wx</B>,<B>w_current->second_wy</B>).
 *
 *  The coordinates of the opposite corner go in
 *  (<B>w_current->first_wx</B>,<B>w_current->first_wy</B>). They are not
 *  suppose to change during the action.
 *
 *  \param [in]  w_current  The GschemToplevel object.
 *  \param [in]  o_current  Picture LeptonObject to check.
 *  \param [in]  x          (unused)
 *  \param [in]  y          (unused)
 *  \param [out] whichone   Which coordinate to check.
 */
static void o_grips_start_path(GschemToplevel *w_current, LeptonObject *o_current,
                               int x, int y, int whichone)
{
  LeptonPathSection *section;
  int i;
  int grip_no = 0;
  int gx = -1;
  int gy = -1;

  for (i = 0; i < lepton_path_object_get_num_sections (o_current); i++)
  {
    section = lepton_path_object_get_section (o_current, i);

    switch (section->code) {
    case PATH_CURVETO:
      /* Two control point grips */
      if (whichone == grip_no++) {
        gx = section->x1;
        gy = section->y1;
      }
      if (whichone == grip_no++) {
        gx = section->x2;
        gy = section->y2;
      }
      /* Fall through */
    case PATH_MOVETO:
    case PATH_MOVETO_OPEN:
    case PATH_LINETO:
      /* Destination point grip */
      if (whichone == grip_no++) {
        gx = section->x3;
        gy = section->y3;
      }
      break;
    case PATH_END:
      break;
    }
  }

  w_current->first_wx = w_current->second_wx = gx;
  w_current->first_wy = w_current->second_wy = gy;

  /* draw the first temporary path */
  /* o_path_invalidate_rubber_grips (w_current); */
  schematic_window_set_rubber_visible (w_current, 1);
}

/*! \brief Initialize grip motion process for a picture.
 *  \par Function Description
 *  This function initializes the grip motion process for a picture.
 *  From the <B>o_current</B> pointed object, it stores into the
 *  GschemToplevel structure the ....
 *  These variables are used in the grip process.
 *
 *  The function first erases the grips.
 *
 *  The coordinates of the selected corner are put in
 *  (<B>w_current->second_wx</B>,<B>w_current->second_wy</B>).
 *
 *  The coordinates of the opposite corner go in
 *  (<B>w_current->first_wx</B>,<B>w_current->first_wy</B>). They are not
 *  suppose to change during the action.
 *
 *  \param [in]  w_current  The GschemToplevel object.
 *  \param [in]  o_current  Picture LeptonObject to check.
 *  \param [in]  x          (unused)
 *  \param [in]  y          (unused)
 *  \param [out] whichone   Which coordinate to check.
 */
static void o_grips_start_picture(GschemToplevel *w_current, LeptonObject *o_current,
                                  int x, int y, int whichone)
{
  int lower_x, lower_y, upper_x, upper_y;

  lower_x = lepton_picture_object_get_lower_x (o_current);
  lower_y = lepton_picture_object_get_lower_y (o_current);
  upper_x = lepton_picture_object_get_upper_x (o_current);
  upper_y = lepton_picture_object_get_upper_y (o_current);

  w_current->current_pixbuf = lepton_picture_object_get_pixbuf (o_current);
  w_current->pixbuf_filename =
    g_strdup (lepton_picture_object_get_filename (o_current));
  w_current->pixbuf_wh_ratio = lepton_picture_object_get_real_ratio (o_current);

  /* (second_wx,second_wy) is the selected corner */
  /* (first_wx, first_wy) is the opposite corner */
  switch(whichone) {
    case PICTURE_UPPER_LEFT:
      w_current->second_wx = upper_x;
      w_current->second_wy = upper_y;
      w_current->first_wx  = lower_x;
      w_current->first_wy  = lower_y;
      break;
    case PICTURE_LOWER_RIGHT:
      w_current->second_wx = lower_x;
      w_current->second_wy = lower_y;
      w_current->first_wx  = upper_x;
      w_current->first_wy  = upper_y;
      break;
    case PICTURE_UPPER_RIGHT:
      w_current->second_wx = lower_x;
      w_current->second_wy = upper_y;
      w_current->first_wx  = upper_x;
      w_current->first_wy  = lower_y;
      break;
    case PICTURE_LOWER_LEFT:
      w_current->second_wx = upper_x;
      w_current->second_wy = lower_y;
      w_current->first_wx  = lower_x;
      w_current->first_wy  = upper_y;
      break;
    default:
      return; /* error */
  }

  /* draw the first temporary picture */
  /* o_picture_invalidate_rubber (w_current); */
  schematic_window_set_rubber_visible (w_current, 1);
}

/*! \brief Initialize grip motion process for a circle.
 *  \par Function Description
 *  This function initializes the grip motion process for a circle.
 *  From the <B>o_current</B> pointed object, it stores into the
 *  GschemToplevel structure the coordinate of the center and the radius.
 *  These variables are used in the grip process.
 *
 *  The function first erases the grips.
 *
 *  The coordinates of the center are put in
 *  (<B>w_current->first_wx</B>,<B>w_current->first_wy</B>). They are not suppose
 *  to change during the action.
 *
 *  The radius of the circle is stored in <B>w_current->distance</B>.
 *
 *  \param [in]  w_current  The GschemToplevel object.
 *  \param [in]  o_current  Circle LeptonObject to check.
 *  \param [in]  x          (unused)
 *  \param [in]  y          (unused)
 *  \param [out] whichone   Which coordinate to check.
 */
static void o_grips_start_circle(GschemToplevel *w_current, LeptonObject *o_current,
                                 int x, int y, int whichone)
{

  /* store circle center and radius in GschemToplevel structure */
  w_current->first_wx = lepton_circle_object_get_center_x (o_current);
  w_current->first_wy = lepton_circle_object_get_center_y (o_current);
  w_current->distance = lepton_circle_object_get_radius (o_current);

  /* draw the first temporary circle */
  /* o_circle_invalidate_rubber (w_current); */
  w_current->rubber_visible = 1;
}

/*! \brief Initialize grip motion process for a line.
 *  This function starts the move of one of the two grips of the line
 *  object <B>o_current</B>.
 *
 *  During the move of the grip, the line is described by
 *  (<B>w_current->first_wx</B>,<B>w_current->first_wy</B>) and
 *  (<B>w_current->second_wx</B>,<B>w_current->second_wy</B>).
 *
 *  The line end that corresponds to the moving grip is in
 *  (<B>w_current->second_wx</B>,<B>w_current->second_wy</B>).
 *
 *  \param [in]  w_current  The GschemToplevel object.
 *  \param [in]  o_current  Line LeptonObject to check.
 *  \param [in]  x          (unused)
 *  \param [in]  y          (unused)
 *  \param [out] whichone   Which coordinate to check.
 */
static void o_grips_start_line(GschemToplevel *w_current, LeptonObject *o_current,
                               int x, int y, int whichone)
{
  /* describe the line with GschemToplevel variables */
  w_current->second_wx = o_current->line->x[whichone];
  w_current->second_wy = o_current->line->y[whichone];
  w_current->first_wx = o_current->line->x[!whichone];
  w_current->first_wy = o_current->line->y[!whichone];

  /* draw the first temporary line */
  /* o_line_invalidate_rubber (w_current); */
  w_current->rubber_visible = 1;
}

/*! \brief Start process of modifiying one grip.
 *  \par Function Description
 *  This function starts the process of modifying one grip of an object
 *  on the current sheet. The event occured in (<B>w_x</B>,<B>w_y</B>) in world unit.
 *  If this position is related to a grip of an object, the function
 *  prepares the modification of this grip thanks to the user input.
 *
 *  The function does nothing if an error occured or if no grip
 *  have been found under (<B>w_x</B>,<B>w_y</B>). Otherwise, it
 *  switches the GRIPS mode on if a grip has been found and
 *  modification of the object has been started.
 *
 *  If a grip has been found, this function modifies the GschemToplevel
 *  variables <B>which_grip</B> and <B>which_object</B> with the identifier
 *  of the grip and the object it belongs to respectively.
 *
 *  If the \a draw-grips rc setting is "disabled", no grids are
 *  displayed though the object is modified the same way.
 *
 *  \param [in]  w_current  The GschemToplevel object.
 *  \param [in]  w_x        Current x coordinate of pointer in world units.
 *  \param [in]  w_y        Current y coordinate of pointer in world units.
 */
void o_grips_start(GschemToplevel *w_current, int w_x, int w_y)
{
  LeptonObject *object;
  int whichone;
  void (*func) (GschemToplevel*, LeptonObject*, int, int, int) = NULL;

  /* search if there is a grip on a selected object at (w_x,w_y) */
  object = o_grips_search_world(w_current, w_x, w_y, &whichone);

  if (object != NULL) {
    w_current->which_grip = whichone;
    w_current->which_object = object;

    /* Switch off drawing for the object being modified */
    object->dont_redraw = TRUE;
    o_invalidate (w_current, object);

    /* there is one */
    /* depending on its type, start the modification process */
    switch (lepton_object_get_type (object)) {
    case OBJ_ARC:     func = o_grips_start_arc;     break;
    case OBJ_BOX:     func = o_grips_start_box;     break;
    case OBJ_PATH:    func = o_grips_start_path;    break;
    case OBJ_PICTURE: func = o_grips_start_picture; break;
    case OBJ_CIRCLE:  func = o_grips_start_circle;  break;
    case OBJ_LINE:
    case OBJ_NET:
    case OBJ_PIN:
    case OBJ_BUS:     func = o_grips_start_line;    break;

    default: break;
    }

    /* start the modification of a grip on the object */
    if (func != NULL) {
      (*func) (w_current, object, w_x, w_y, whichone);
      i_set_state (w_current, GRIPS);
      i_action_start (w_current);
    }
  }
}

/*! \brief Modify previously selected object according to mouse position.
 *  \par Function Description
 *  This function modify the previously selected
 *  object according to the mouse position in <B>w_x</B> and <B>w_y</B>.
 *  The grip under modification is updated and the temporary object displayed.
 *
 *  The object under modification is <B>w_current->which_object</B> and
 *  the grip concerned is <B>w_current->which_grip</B>.
 *
 *  Depending on the object type, a specific function is used.
 *  It erases the temporary object, updates its internal representation,
 *  and draws it again.
 *
 *  \param [in] w_current  The GschemToplevel object.
 *  \param [in] w_x        Current x coordinate of pointer in world units.
 *  \param [in] w_y        Current y coordinate of pointer in world units.
 */
void o_grips_motion(GschemToplevel *w_current, int w_x, int w_y)
{
  int grip = w_current->which_grip;

  g_assert (schematic_window_get_inside_action (w_current) != 0);
  g_return_if_fail( w_current->which_object != NULL );

  switch (lepton_object_get_type (w_current->which_object)) {
    case OBJ_ARC:
      o_arc_motion (w_current, w_x, w_y, grip);
      break;

    case OBJ_BOX:
      o_box_motion (w_current, w_x, w_y);
      break;

    case OBJ_PATH:
      o_path_motion_grips (w_current, w_x, w_y);
      break;

    case OBJ_PICTURE:
      o_picture_motion (w_current, w_x, w_y);
      break;

    case OBJ_CIRCLE:
      o_circle_motion (w_current, w_x, w_y);
      break;

    case OBJ_LINE:
    case OBJ_NET:
    case OBJ_PIN:
    case OBJ_BUS:
      o_line_motion (w_current, w_x, w_y);
      break;

    default:
    return; /* error condition */
  }
}


/*! \brief Cancel process of modifying object with grip.
 *
 *  \par Function Description
 *  This function cancels the process of modifying a parameter
 *  of an object with a grip. It's main utility is to reset the
 *  dont_redraw flag on the object which was being modified.
 *
 *  \param [in,out] w_current  The GschemToplevel object.
 */
void o_grips_cancel(GschemToplevel *w_current)
{
  LeptonObject *object = w_current->which_object;

  /* reset global variables */
  w_current->which_grip = -1;
  w_current->which_object = NULL;
  schematic_window_set_rubber_visible (w_current, 0);

  /* Switch drawing of the object back on */
  g_return_if_fail (object != NULL);
  object->dont_redraw = FALSE;
}


/*! \brief End process of modifying arc object with grip.
 *  \par Function Description
 *  This function ends the grips process specific to an arc object. It erases
 *  the old arc and write back to the object the new parameters of the arc.
 *  Depending on the grip selected and moved, the right fields are updated.
 *  The function handles the conversion from screen unit to world unit before
 *  updating and redrawing.
 *
 *  If the grip at the center of the arc has been moved - modifying the radius
 *  of the arc -, the new radius is calculated expressed in world unit
 *  (the center is unchanged). It is updated with the function
 *  lepton_arc_object_modify().
 *
 *  If one of the end of arc grip has been moved - modifying one of the
 *  angles describing the arc -, this angle is updated with the
 *  lepton_arc_object_modify() function.
 *
 *  \param [in] w_current  The GschemToplevel object.
 *  \param [in] o_current  Arc LeptonObject to end modification on.
 *  \param [in] whichone   Which grip is pointed to.
 */
static void o_grips_end_arc(GschemToplevel *w_current, LeptonObject *o_current,
                            int whichone)
{
  int arg1, arg2;

  /* erase the temporary arc */
  /* o_arc_invalidate_rubber (w_current); */

  /* determination of the parameters to give to o_arc_modify() */
  switch(whichone) {
    case ARC_RADIUS:
      /* get the radius from w_current */
      arg1 = w_current->distance;
      /* second parameter is not used */
      arg2 = -1;
      break;

    case ARC_START_ANGLE:
      /* get the start angle from w_current */
      arg1 = w_current->second_wx;
      /* second parameter is not used */
      arg2 = -1;
      break;

    case ARC_SWEEP_ANGLE:
      /* get the end angle from w_current */
      arg1 = w_current->second_wy;
      /* second parameter is not used */
      arg2 = -1;
      break;

    default:
      return;
  }

  /* modify the arc with the parameters determined above */
  lepton_arc_object_modify (o_current, arg1, arg2, whichone);
}

/*! \todo Finish function documentation!!!
 *  \brief End process of modifying box object with grip.
 *  \par Function Description
 *
 *  \param [in] w_current  The GschemToplevel object.
 *  \param [in] o_current  Box LeptonObject to end modification on.
 *  \param [in] whichone   Which grip is pointed to.
 */
static void o_grips_end_box(GschemToplevel *w_current, LeptonObject *o_current,
                            int whichone)
{
  int box_width, box_height;

  box_width  = GET_BOX_WIDTH (w_current);
  box_height = GET_BOX_HEIGHT(w_current);

  /* don't allow zero width/height boxes
   * this ends the box drawing behavior
   * we want this? hack */
  if ((box_width == 0) || (box_height == 0)) {
    o_box_invalidate_rubber (w_current);
    o_invalidate (w_current, o_current);
    return;
  }

  lepton_box_object_modify (o_current, w_current->second_wx, w_current->second_wy, whichone);
}

/*! \todo Finish function documentation!!!
 *  \brief End process of modifying path object with grip.
 *  \par Function Description
 *
 *  \param [in] w_current  The GschemToplevel object.
 *  \param [in] o_current  Picture LeptonObject to end modification on.
 *  \param [in] whichone   Which grip is pointed to.
 */
static void o_grips_end_path(GschemToplevel *w_current, LeptonObject *o_current,
                             int whichone)
{
  lepton_path_object_modify (o_current,
                             w_current->second_wx,
                             w_current->second_wy,
                             whichone);
}

/*! \todo Finish function documentation!!!
 *  \brief End process of modifying picture object with grip.
 *  \par Function Description
 *
 *  \param [in] w_current  The GschemToplevel object.
 *  \param [in] o_current  Picture LeptonObject to end modification on.
 *  \param [in] whichone   Which grip is pointed to.
 */
static void o_grips_end_picture(GschemToplevel *w_current, LeptonObject *o_current,
                                int whichone)
{
  /* don't allow zero width/height pictures
   * this ends the picture drawing behavior
   * we want this? hack */
  if ((GET_PICTURE_WIDTH(w_current) == 0) || (GET_PICTURE_HEIGHT(w_current) == 0)) {
    o_picture_invalidate_rubber (w_current);
    o_invalidate (w_current, o_current);
    return;
  }

  lepton_picture_object_modify (o_current,
                                w_current->second_wx,
                                w_current->second_wy,
                                whichone);

  g_object_unref (w_current->current_pixbuf);
  w_current->current_pixbuf = NULL;
  g_free (w_current->pixbuf_filename);
  w_current->pixbuf_filename = NULL;
  w_current->pixbuf_wh_ratio = 0;
}

/*! \brief End process of modifying circle object with grip.
 *  \par Function Description
 *  This function ends the process of modifying the radius of the circle
 *  object <B>*o_current</B>.
 *  The modified circle is finally normally drawn.
 *
 *  A circle with a null radius is not allowed. In this case, the process
 *  is stopped and the circle is left unchanged.
 *
 *  The last value of the radius is in <B>w_current->distance</B> in screen units.
 *
 *  \param [in] w_current  The GschemToplevel object.
 *  \param [in] o_current  Circle LeptonObject to end modification on.
 *  \param [in] whichone   Which grip is pointed to.
 */
static void o_grips_end_circle(GschemToplevel *w_current, LeptonObject *o_current,
                               int whichone)
{
  /* don't allow zero radius circles
   * this ends the circle drawing behavior
   * we want this? hack */
  if (w_current->distance == 0) {
    o_circle_invalidate_rubber (w_current);
    o_invalidate (w_current, o_current);
    return;
  }

  /* modify the radius of the circle */
  lepton_circle_object_modify (o_current, w_current->distance, -1, CIRCLE_RADIUS);
}

/*! \brief End process of modifying line object with grip.
 *  \par Function Description
 *  This function ends the process of modifying one end of the line
 *  object <B>*o_current</B>.
 *  This end is identified by <B>whichone</B>. The line object is modified
 *  according to the <B>whichone</B> parameter and the last position of the
 *  line end.
 *  The modified line is finally normally drawn.
 *
 *  A line with a null width, i.e. when both ends are identical, is not
 *  allowed. In this case, the process is stopped and the line unchanged.
 *
 *  \param [in] w_current  The GschemToplevel object.
 *  \param [in] o_current  Line LeptonObject to end modification on.
 *  \param [in] whichone   Which grip is pointed to.
 */
static void o_grips_end_line(GschemToplevel *w_current, LeptonObject *o_current,
                             int whichone)
{
  /* don't allow zero length nets / lines / pins
   * this ends the net drawing behavior
   * we want this? hack */
  if ((w_current->first_wx == w_current->second_wx) &&
      (w_current->first_wy == w_current->second_wy)) {
    o_box_invalidate_rubber (w_current);
    o_invalidate (w_current, o_current);
    return;
  }

  /* modify the right line end according to whichone */
  lepton_line_object_modify (o_current,
                             w_current->second_wx, w_current->second_wy, whichone);
}


/*! \brief End process of modifying net object with grip.
 *  \par Function Description
 *  This function ends the process of modifying one end of the net
 *  object <B>*o_current</B>.
 *  This end is identified by <B>whichone</B>. The line object is modified
 *  according to the <B>whichone</B> parameter and the last position of the
 *  line end.
 *  The connections to the modified net are checked and recreated if neccessary.
 *
 *  A net with zero length, i.e. when both ends are identical, is not
 *  allowed. In this case, the process is stopped and the line unchanged.
 *
 *  \param [in] w_current  The GschemToplevel object.
 *  \param [in] o_current  Net LeptonObject to end modification on.
 *  \param [in] whichone   Which grip is pointed to.
 */
static void o_grips_end_net(GschemToplevel *w_current, LeptonObject *o_current,
                            int whichone)
{
  GList *connected_objects;

  /* don't allow zero length net
   * this ends the net drawing behavior
   * we want this? hack */
  if ((w_current->first_wx == w_current->second_wx) &&
      (w_current->first_wy == w_current->second_wy)) {
    o_invalidate (w_current, o_current);
    return;
  }

  s_conn_remove_object_connections (o_current);
  lepton_net_object_modify (o_current, w_current->second_wx,
                            w_current->second_wy, w_current->which_grip);
  s_conn_update_object (o_current->page, o_current);

  /* add bus rippers if necessary */
  connected_objects = s_conn_return_others (NULL, o_current);
  o_net_add_busrippers (w_current, o_current, connected_objects);
  g_list_free (connected_objects);
}

/*! \brief End process of modifying pin object with grip.
 *  \par Function Description
 *  This function ends the process of modifying one end of the pin
 *  object <B>*o_current</B>.
 *  This end is identified by <B>whichone</B>. The pin object is modified
 *  according to the <B>whichone</B> parameter and the last position of the
 *  pin end.
 *  The connections to the modified pin are checked and recreated if neccessary.
 *
 *  A pin with zero length, i.e. when both ends are identical, is not
 *  allowed. In this case, the process is stopped and the line unchanged.
 *
 *  \param [in] w_current  The GschemToplevel object.
 *  \param [in] o_current  Net LeptonObject to end modification on.
 *  \param [in] whichone   Which grip is pointed to.
 */
static void o_grips_end_pin(GschemToplevel *w_current, LeptonObject *o_current,
                            int whichone)
{
  /* don't allow zero length pin
   * this ends the pin changing behavior
   * we want this? hack */
  if ((w_current->first_wx == w_current->second_wx) &&
      (w_current->first_wy == w_current->second_wy)) {
    o_invalidate (w_current, o_current);
    return;
  }

  s_conn_remove_object_connections (o_current);
  lepton_pin_object_modify (o_current,
                            w_current->second_wx,
                            w_current->second_wy,
                            w_current->which_grip);
  s_conn_update_object (o_current->page, o_current);
}

/*! \brief End process of modifying bus object with grip.
 *  \par Function Description
 *  This function ends the process of modifying one end of the bus
 *  object <B>*o_current</B>.
 *  This end is identified by <B>whichone</B>. The line object is modified
 *  according to the <B>whichone</B> parameter and the last position of the
 *  bus end.
 *  The connections to the modified bus are checked and recreated if neccessary.
 *
 *  A bus with zero length, i.e. when both ends are identical, is not
 *  allowed. In this case, the process is stopped and the bus unchanged.
 *
 *  \param [in] w_current  The GschemToplevel object.
 *  \param [in] o_current  bus LeptonObject to end modification on.
 *  \param [in] whichone   Which grip is pointed to.
 */
static void o_grips_end_bus(GschemToplevel *w_current, LeptonObject *o_current,
                            int whichone)
{
  /* don't allow zero length bus
   * this ends the bus changing behavior
   * we want this? hack */
  if ((w_current->first_wx == w_current->second_wx) &&
      (w_current->first_wy == w_current->second_wy)) {
    o_invalidate (w_current, o_current);
    return;
  }

  s_conn_remove_object_connections (o_current);
  lepton_bus_object_modify (o_current, w_current->second_wx,
                            w_current->second_wy, w_current->which_grip);
  s_conn_update_object (o_current->page, o_current);
}


/*! \brief End process of modifying object with grip.
 *  \par Function Description
 *  This function ends the process of modifying a parameter of an object
 *  with a grip.
 *  The temporary representation of the object is erased, the object is
 *  modified and finally drawn.
 *
 *  The object under modification is <B>w_current->which_object</B> and
 *  the grip concerned is <B>w_current->which_grip</B>.
 *
 *  Depending on the object type, a specific function is used. It erases
 *  the temporary object, updates the object and draws the modified object
 *  normally.
 *
 *  \param [in,out] w_current  The GschemToplevel object.
 */
void o_grips_end(GschemToplevel *w_current)
{
  LeptonObject *object;
  int grip;

  g_assert (schematic_window_get_inside_action (w_current) != 0);

  object = w_current->which_object;
  grip = w_current->which_grip;

  if (!object) {
    /* actually this is an error condition hack */
    i_action_stop (w_current);
    i_set_state(w_current, SELECT);
    return;
  }

  switch (lepton_object_get_type (object)) {

    case(OBJ_ARC):
    /* modify an arc object */
    o_grips_end_arc(w_current, object, grip);
    break;

    case(OBJ_BOX):
    /* modify a box object */
    o_grips_end_box(w_current, object, grip);
    break;

    case(OBJ_PATH):
    /* modify a path object */
    o_grips_end_path(w_current, object, grip);
    break;

    case(OBJ_PICTURE):
    /* modify a picture object */
    o_grips_end_picture(w_current, object, grip);
    break;

    case(OBJ_CIRCLE):
    /* modify a circle object */
    o_grips_end_circle(w_current, object, grip);
    break;

    case(OBJ_LINE):
    /* modify a line object */
    o_grips_end_line(w_current, object, grip);
    break;

    case(OBJ_NET):
      /* modify a net object */
      o_grips_end_net(w_current, object, grip);
      break;

    case(OBJ_PIN):
      /* modify a pin object */
      o_grips_end_pin(w_current, object, grip);
      break;

    case(OBJ_BUS):
      /* modify a bus object */
      o_grips_end_bus(w_current, object, grip);
      break;

    default:
    return;
  }

  /* Switch drawing of the object back on */
  object->dont_redraw = FALSE;
  o_invalidate (w_current, object);

  /* reset global variables */
  w_current->which_grip = -1;
  w_current->which_object = NULL;

  schematic_window_set_rubber_visible (w_current, 0);

  schematic_window_active_page_changed (w_current);
  o_undo_savestate_old(w_current, UNDO_ALL);

  i_set_state(w_current, SELECT);
  i_action_stop (w_current);
}


/*! \brief Draw objects being grip maniuplated from GschemToplevel object.
 *
 *  \par Function Description
 *  This function draws the objects being grip manipulated using
 *  \a renderer.
 *
 *  \param [in] w_current  The #GschemToplevel object.
 *  \param [in] renderer   The \c EdaRenderer object.
 */
void o_grips_draw_rubber (GschemToplevel *w_current, EdaRenderer *renderer)
{
  g_return_if_fail (w_current->which_object != NULL);

  switch (lepton_object_get_type (w_current->which_object)) {
    case OBJ_ARC:
      o_arc_draw_rubber (w_current, renderer);
      break;

    case OBJ_BOX:
      o_box_draw_rubber (w_current, renderer);
      break;

    case OBJ_PATH:
      o_path_draw_rubber_grips (w_current, renderer);
      break;

    case OBJ_PICTURE:
      o_picture_draw_rubber (w_current, renderer);
      break;

    case OBJ_CIRCLE:
      o_circle_draw_rubber (w_current, renderer);
      break;

    case OBJ_LINE:
    case OBJ_NET:
    case OBJ_PIN:
    case OBJ_BUS:
      o_line_draw_rubber (w_current, renderer);
    break;

    default:
      g_return_if_reached ();
  }
}
