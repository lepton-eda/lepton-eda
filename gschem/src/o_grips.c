/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 * Copyright (C) 1998-2008 Ales Hvezda
 * Copyright (C) 1998-2008 gEDA Contributors (see ChangeLog for details)
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
#include <config.h>

#include <stdio.h>
#include <math.h>

#include "gschem.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

#define GET_BOX_WIDTH(w)  abs((w)->second_wx - (w)->first_wx)
#define GET_BOX_HEIGHT(w) abs((w)->second_wy - (w)->second_wy)

#define GET_PICTURE_WIDTH(w)			\
  abs((w)->second_wx - (w)->first_wx) 
#define GET_PICTURE_HEIGHT(w)						\
  (w)->pixbuf_wh_ratio == 0 ? 0 : abs((w)->second_wx - (w)->first_wx)/(w)->pixbuf_wh_ratio
#define GET_PICTURE_LEFT(w)			\
  min((w)->first_wx, (w)->second_wx)
#define GET_PICTURE_TOP(w)						\
  (w)->first_wy > (w)->second_wy ? (w)->first_wy  :			\
  (w)->first_wy+abs((w)->second_wx - (w)->first_wx)/(w)->pixbuf_wh_ratio


/*! \brief Check if point is inside grip.
 *  \par Function Description
 *  This function is used to determine if the (<B>x</B>,<B>y</B>) point is
 *  inside a grip of one of the selected object on the current sheet.
 *  The selected object are in a list starting at
 *  <B>w_current->toplevel->page_current->selection2_head</B>.
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
 *  \param [in]  w_current  The GSCHEM_TOPLEVEL object.
 *  \param [in]  x          Current x coordinate of pointer in world units.
 *  \param [in]  y          Current y coordinate of pointer in world units.
 *  \param [out] whichone   Which grip point is selected.
 *  \return Pointer to OBJECT the grip is on, NULL otherwise.
 */
OBJECT *o_grips_search_world(GSCHEM_TOPLEVEL *w_current, int x, int y, int *whichone)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  OBJECT *object=NULL;
  OBJECT *found=NULL;
  GList *s_current;
  int size;
  int w_size;

  if (!whichone) {
    return(NULL);
  }

  /* get the size of the grip according to zoom level */
  size = o_grips_size(w_current);
  w_size = WORLDabs(toplevel, size );

  s_current = geda_list_get_glist( toplevel->page_current->selection_list );
  while (s_current != NULL) {
    object = (OBJECT *) s_current->data;
    if (object) {
      switch(object->type) {
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
 *                  <B>whichone</B> pointed integer is set to <B>ARC_END_ANGLE</B>.
 *  </DL>
 *
 *  The <B>x</B> and <B>y</B> parameters are in world units.
 *
 *  The <B>size</B> parameter is the width (and height) of the square
 *  representing a grip in world units.
 *
 *  \param [in]  w_current  The GSCHEM_TOPLEVEL object.
 *  \param [in]  o_current  Arc OBJECT to check.
 *  \param [in]  x          Current x coordinate of pointer in world units.
 *  \param [in]  y          Current y coordinate of pointer in world units.
 *  \param [in]  size       Half the width of the grip square in world units.
 *  \param [out] whichone   Which grip point is selected.
 *  \return Pointer to OBJECT the grip is on, NULL otherwise.
 */
OBJECT *o_grips_search_arc_world(GSCHEM_TOPLEVEL *w_current, OBJECT *o_current,
                                 int x, int y, int size, int *whichone)
{
  int centerx, centery, radius, start_angle, end_angle;
  double tmp;

  centerx     = o_current->arc->x;
  centery     = o_current->arc->y;
  radius      = o_current->arc->width / 2;
  start_angle = o_current->arc->start_angle;
  end_angle   = o_current->arc->end_angle;

  /* check the grip on the center of the arc */
  if (inside_grip(x, y, centerx, centery, size)) {
    *whichone = ARC_RADIUS;
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

  /* check the grip at the end angle of the arc */
  tmp = ((double) start_angle + end_angle) * M_PI / 180;
  if (inside_grip(x, y,
                  centerx + radius * cos(tmp),
                  centery + radius * sin(tmp), size)) {
    *whichone = ARC_END_ANGLE;
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
 *  \param [in]  w_current  The GSCHEM_TOPLEVEL object.
 *  \param [in]  o_current  Box OBJECT to check.
 *  \param [in]  x          Current x coordinate of pointer in world units.
 *  \param [in]  y          Current y coordinate of pointer in world units.
 *  \param [in]  size       Half the width of the grip square in world units.
 *  \param [out] whichone   Which grip point is selected.
 *  \return Pointer to OBJECT the grip is on, NULL otherwise.
 */
OBJECT *o_grips_search_box_world(GSCHEM_TOPLEVEL *w_current, OBJECT *o_current,
                                 int x, int y, int size, int *whichone)
{
  /* inside upper left grip ? */
  if (inside_grip(x, y,
                  o_current->box->upper_x,
                  o_current->box->upper_y, size)) {
    *whichone = BOX_UPPER_LEFT;
    return(o_current);
  }

  /* inside lower right grip ? */
  if (inside_grip(x, y,
                  o_current->box->lower_x,
                  o_current->box->lower_y, size)) {
    *whichone = BOX_LOWER_RIGHT;
    return(o_current);
  }

  /* inside upper right grip ? */
  if (inside_grip(x, y,
                  o_current->box->lower_x,
                  o_current->box->upper_y, size)) {
    *whichone = BOX_UPPER_RIGHT;
    return(o_current);
  }

  /* inside lower left grip ? */
  if (inside_grip(x, y,
                  o_current->box->upper_x,
                  o_current->box->lower_y, size)) {
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
 *  A path object has four grips : one at each corner of the path.
 *  The identifiers of each corner are #PICTURE_UPPER_LEFT,
 *  #PICTURE_UPPER_RIGHT, #PICTURE_LOWER_LEFT and
 *  #PICTURE_LOWER_RIGHT.
 *
 *  The <B>x</B> and <B>y</B> parameters are in world units.
 *
 *  The <B>size</B> parameter is half the width (and half the height) of the
 *  square representing a grip in world units.
 *
 *  \param [in]  w_current  The GSCHEM_TOPLEVEL object.
 *  \param [in]  o_current  Picture OBJECT to check.
 *  \param [in]  x          Current x coordinate of pointer in world units.
 *  \param [in]  y          Current y coordinate of pointer in world units.
 *  \param [in]  size       Half the width of the grip square in world units.
 *  \param [out] whichone   Which grip point is selected.
 *  \return Pointer to OBJECT the grip is on, NULL otherwise.
 */
OBJECT *o_grips_search_path_world(GSCHEM_TOPLEVEL *w_current, OBJECT *o_current,
                                     int x, int y, int size, int *whichone)
{
  PATH_SECTION *section;
  int i;
  int grip_no = 0;

  for (i = 0; i <  o_current->path->num_sections; i++) {
    section = &o_current->path->sections[i];

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
 *  A picture object has four grips : one at each corner of the picture.
 *  The identifiers of each corner are #PICTURE_UPPER_LEFT,
 *  #PICTURE_UPPER_RIGHT, #PICTURE_LOWER_LEFT and
 *  #PICTURE_LOWER_RIGHT.
 *
 *  The <B>x</B> and <B>y</B> parameters are in world units.
 *
 *  The <B>size</B> parameter is half the width (and half the height) of the
 *  square representing a grip in world units.
 *
 *  \param [in]  w_current  The GSCHEM_TOPLEVEL object.
 *  \param [in]  o_current  Picture OBJECT to check.
 *  \param [in]  x          Current x coordinate of pointer in world units.
 *  \param [in]  y          Current y coordinate of pointer in world units.
 *  \param [in]  size       Half the width of the grip square in world units.
 *  \param [out] whichone   Which grip point is selected.
 *  \return Pointer to OBJECT the grip is on, NULL otherwise.
 */
OBJECT *o_grips_search_picture_world(GSCHEM_TOPLEVEL *w_current, OBJECT *o_current,
                                     int x, int y, int size, int *whichone)
{
  /* inside upper left grip ? */
  if (inside_grip(x, y,
                  o_current->picture->upper_x,
                  o_current->picture->upper_y, size)) {
    *whichone = PICTURE_UPPER_LEFT;
    return(o_current);
  }

  /* inside lower right grip ? */
  if (inside_grip(x, y,
                  o_current->picture->lower_x,
                  o_current->picture->lower_y, size)) {
    *whichone = PICTURE_LOWER_RIGHT;
    return(o_current);
  }

  /* inside upper right grip ? */
  if (inside_grip(x, y,
                  o_current->picture->lower_x,
                  o_current->picture->upper_y, size)) {
    *whichone = PICTURE_UPPER_RIGHT;
    return(o_current);
  }

  /* inside lower left grip ? */
  if (inside_grip(x, y,
                  o_current->picture->upper_x,
                  o_current->picture->lower_y, size)) {
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
 *  \param [in]  w_current  The GSCHEM_TOPLEVEL object.
 *  \param [in]  o_current  Circle OBJECT to check.
 *  \param [in]  x          Current x coordinate of pointer in world units.
 *  \param [in]  y          Current y coordinate of pointer in world units.
 *  \param [in]  size       Half the width of the grip square in world units.
 *  \param [out] whichone   Which grip point is selected.
 *  \return Pointer to OBJECT the grip is on, NULL otherwise.
 */
OBJECT *o_grips_search_circle_world(GSCHEM_TOPLEVEL *w_current, OBJECT *o_current,
                                    int x, int y, int size, int *whichone)
{
  /* check the grip for radius */
  if (inside_grip(x, y,
                  o_current->circle->center_x + o_current->circle->radius,
                  o_current->circle->center_y - o_current->circle->radius,
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
 *  \param [in]  w_current  The GSCHEM_TOPLEVEL object.
 *  \param [in]  o_current  Line OBJECT to check.
 *  \param [in]  x          Current x coordinate of pointer in world units.
 *  \param [in]  y          Current y coordinate of pointer in world units.
 *  \param [in]  size       Half the width of the grip square in world units.
 *  \param [out] whichone   Which grip point is selected.
 *  \return Pointer to OBJECT the grip is on, NULL otherwise.
 */
OBJECT *o_grips_search_line_world(GSCHEM_TOPLEVEL *w_current, OBJECT *o_current,
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

/*! \brief Start process of modifiying one grip.
 *  \par Function Description
 *  This function starts the process of modifying one grip of an object
 *  on the current sheet. The event occured in (<B>w_x</B>,<B>w_y</B>) in world unit.
 *  If this position is related to a grip of an object, the function
 *  prepares the modification of this grip thanks to the user input.
 *
 *  The function returns <B>FALSE</B> if an error occured or if no grip
 *  have been found under (<B>w_x</B>,<B>w_y</B>). It returns <B>TRUE</B> if a grip
 *  has been found and modification of the object has been started.
 *
 *  If a grip has been found, this function modifies the GSCHEM_TOPLEVEL
 *  variables <B>which_grip</B> and <B>which_object</B> with the identifier
 *  of the grip and the object it belongs to respectively.
 *
 *  \param [in]  w_current  The GSCHEM_TOPLEVEL object.
 *  \param [in]  w_x        Current x coordinate of pointer in world units.
 *  \param [in]  w_y        Current y coordinate of pointer in world units.
 *  \return FALSE if an error occurred or no grip was found, TRUE otherwise.
 */
int o_grips_start(GSCHEM_TOPLEVEL *w_current, int w_x, int w_y)
{
  OBJECT *object;
  int whichone;

  if (w_current->draw_grips == FALSE) {
    return(FALSE);
  }

  /* search if there is a grip on a selected object at (w_x,w_y) */
  object = o_grips_search_world(w_current, w_x, w_y, &whichone);

  if (object == NULL)
    return FALSE;

  w_current->which_grip = whichone;
  w_current->which_object = object;

  /* there is one */
  /* depending on its type, start the modification process */
  switch(object->type) {
    case(OBJ_ARC):
      /* start the modification of a grip on an arc */
      o_grips_start_arc(w_current, object, w_x, w_y, whichone);
      return(TRUE);

    case(OBJ_BOX):
      /* start the modification of a grip on a box */
      o_grips_start_box(w_current, object, w_x, w_y, whichone);
      return(TRUE);

    case(OBJ_PATH):
      /* start the modification of a grip on a path */
      o_grips_start_path(w_current, object, w_x, w_y, whichone);
      return(TRUE);

    case(OBJ_PICTURE):
      /* start the modification of a grip on a picture */
      o_grips_start_picture(w_current, object, w_x, w_y, whichone);
      return(TRUE);

    case(OBJ_CIRCLE):
      /* start the modification of a grip on a circle */
      o_grips_start_circle(w_current, object, w_x, w_y, whichone);
      return(TRUE);

    case(OBJ_LINE):
    case(OBJ_NET):
    case(OBJ_PIN):
    case(OBJ_BUS):
      /* identical for line/net/pin/bus */
      /* start the modification of a grip on a line */
      o_grips_start_line(w_current, object, w_x, w_y, whichone);
      return(TRUE);

    default:
      /* object type unknown : error condition */
      return(FALSE);
  }
  return(FALSE);
}

/*! \brief Initialize grip motion process for an arc.
 *  \par Function Description
 *  This function initializes the grip motion process for an arc.
 *  From the <B>o_current</B> pointed object, it stores into the
 *  GSCHEM_TOPLEVEL structure the coordinates of the center, the radius
 *  and the two angle that describes an arc. These variables are used in
 *  the grip process.
 *
 *  The coordinates of the center of the arc on x- and y-axis are stored
 *  into the <B>first_wx</B> and <B>first_wy</B> fields of the GSCHEM_TOPLEVEL
 *  structure in screen units.
 *
 *  The radius of the center is stored into the <B>distance</B> field of
 *  the GSCHEM_TOPLEVEL structure in screen units.
 *
 *  The two angles describing the arc on a circle are stored into the
 *  <B>second_wx</B> for the starting angle and <B>second_wy</B> for the ending angle.
 *  These angles are expressed in degrees.
 *
 *  \param [in]  w_current  The GSCHEM_TOPLEVEL object.
 *  \param [in]  o_current  Arc OBJECT to check.
 *  \param [in]  x          (unused)
 *  \param [in]  y          (unused)
 *  \param [out] whichone   (unused)
 */
void o_grips_start_arc(GSCHEM_TOPLEVEL *w_current, OBJECT *o_current,
                       int x, int y, int whichone)
{
  w_current->last_drawb_mode = LAST_DRAWB_MODE_NONE;

  /* erase the arc before */
  o_invalidate (w_current, o_current);

  /* describe the arc with GSCHEM_TOPLEVEL variables */
  /* center */
  w_current->first_wx = o_current->arc->x;
  w_current->first_wy = o_current->arc->y;
  /* radius */
  w_current->distance = o_current->arc->width / 2;
  /* angles */
  w_current->second_wx = o_current->arc->start_angle;
  w_current->second_wy = o_current->arc->end_angle;

  /* draw the first temporary arc */
  /* o_arc_invalidate_rubber (w_current); */
  w_current->rubber_visible = 1;
}

/*! \brief Initialize grip motion process for a box.
 *  \par Function Description
 *  This function initializes the grip motion process for a box. From the
 *  <B>o_current</B> pointed object, it stores into the GSCHEM_TOPLEVEL
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
 *  \param [in]  w_current  The GSCHEM_TOPLEVEL object.
 *  \param [in]  o_current  Box OBJECT to check.
 *  \param [in]  x          (unused)
 *  \param [in]  y          (unused)
 *  \param [out] whichone   Which coordinate to check.
 */
void o_grips_start_box(GSCHEM_TOPLEVEL *w_current, OBJECT *o_current,
                       int x, int y, int whichone)
{
  w_current->last_drawb_mode = LAST_DRAWB_MODE_NONE;

  /* erase the box before */
  o_invalidate (w_current, o_current);

  /* (second_wx, second_wy) is the selected corner */
  /* (first_wx, first_wy) is the opposite corner */
  switch(whichone) {
    case BOX_UPPER_LEFT:
      w_current->second_wx = o_current->box->upper_x;
      w_current->second_wy = o_current->box->upper_y;
      w_current->first_wx = o_current->box->lower_x;
      w_current->first_wy = o_current->box->lower_y;
      break;
    case BOX_LOWER_RIGHT:
      w_current->second_wx = o_current->box->lower_x;
      w_current->second_wy = o_current->box->lower_y;
      w_current->first_wx = o_current->box->upper_x;
      w_current->first_wy = o_current->box->upper_y;
      break;
    case BOX_UPPER_RIGHT:
      w_current->second_wx = o_current->box->lower_x;
      w_current->second_wy = o_current->box->upper_y;
      w_current->first_wx = o_current->box->upper_x;
      w_current->first_wy = o_current->box->lower_y;
      break;
    case BOX_LOWER_LEFT:
      w_current->second_wx = o_current->box->upper_x;
      w_current->second_wy = o_current->box->lower_y;
      w_current->first_wx = o_current->box->lower_x;
      w_current->first_wy = o_current->box->upper_y;
      break;
    default:
      return; /* error */
  }

  /* draw the first temporary box */
  /* o_box_invalidate_rubber (w_current); */
  w_current->rubber_visible = 1;
}

/*! \brief Initialize grip motion process for a path.
 *  \par Function Description
 *  This function initializes the grip motion process for a path.
 *  From the <B>o_current</B> pointed object, it stores into the
 *  GSCHEM_TOPLEVEL structure the ....
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
 *  \param [in]  w_current  The GSCHEM_TOPLEVEL object.
 *  \param [in]  o_current  Picture OBJECT to check.
 *  \param [in]  x          (unused)
 *  \param [in]  y          (unused)
 *  \param [out] whichone   Which coordinate to check.
 */
void o_grips_start_path(GSCHEM_TOPLEVEL *w_current, OBJECT *o_current,
                           int x, int y, int whichone)
{
  PATH_SECTION *section;
  int i;
  int grip_no = 0;
  int gx = -1;
  int gy = -1;

  w_current->last_drawb_mode = -1;

  /* erase the path before */
  o_invalidate (w_current, o_current);

  for (i = 0; i <  o_current->path->num_sections; i++) {
    section = &o_current->path->sections[i];

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
  /* o_path_invalidate_rubber (w_current); */
  w_current->rubber_visible = 1;
}

/*! \brief Initialize grip motion process for a picture.
 *  \par Function Description
 *  This function initializes the grip motion process for a picture.
 *  From the <B>o_current</B> pointed object, it stores into the
 *  GSCHEM_TOPLEVEL structure the ....
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
 *  \param [in]  w_current  The GSCHEM_TOPLEVEL object.
 *  \param [in]  o_current  Picture OBJECT to check.
 *  \param [in]  x          (unused)
 *  \param [in]  y          (unused)
 *  \param [out] whichone   Which coordinate to check.
 */
void o_grips_start_picture(GSCHEM_TOPLEVEL *w_current, OBJECT *o_current,
                           int x, int y, int whichone)
{
  w_current->last_drawb_mode = LAST_DRAWB_MODE_NONE;

  /* erase the picture before */
  o_invalidate (w_current, o_current);
  w_current->current_pixbuf = o_current->picture->original_picture;
  w_current->pixbuf_filename = o_current->picture->filename;
  w_current->pixbuf_wh_ratio = o_current->picture->ratio;

  /* (second_wx,second_wy) is the selected corner */
  /* (first_wx, first_wy) is the opposite corner */
  switch(whichone) {
    case PICTURE_UPPER_LEFT:
      w_current->second_wx = o_current->picture->upper_x;
      w_current->second_wy = o_current->picture->upper_y;
      w_current->first_wx = o_current->picture->lower_x; 
      w_current->first_wy = o_current->picture->lower_y;
      break;
    case PICTURE_LOWER_RIGHT:
      w_current->second_wx = o_current->picture->lower_x;
      w_current->second_wy = o_current->picture->lower_y;
      w_current->first_wx = o_current->picture->upper_x; 
      w_current->first_wy = o_current->picture->upper_y;
      break;
    case PICTURE_UPPER_RIGHT:
      w_current->second_wx = o_current->picture->lower_x;
      w_current->second_wy = o_current->picture->upper_y;
      w_current->first_wx = o_current->picture->upper_x; 
      w_current->first_wy = o_current->picture->lower_y;
      break;
    case PICTURE_LOWER_LEFT:
      w_current->second_wx = o_current->picture->upper_x;
      w_current->second_wy = o_current->picture->lower_y;
      w_current->first_wx = o_current->picture->lower_x; 
      w_current->first_wy = o_current->picture->upper_y;
      break;
    default:
      return; /* error */
  }

  /* draw the first temporary picture */
  /* o_picture_invalidate_rubber (w_current); */
  w_current->rubber_visible = 1;
}

/*! \brief Initialize grip motion process for a circle.
 *  \par Function Description
 *  This function initializes the grip motion process for a circle.
 *  From the <B>o_current</B> pointed object, it stores into the
 *  GSCHEM_TOPLEVEL structure the coordinate of the center and the radius.
 *  These variables are used in the grip process.
 *
 *  The function first erases the grips.
 *
 *  The coordinates of the center are put in
 *  (<B>w_current->first_wx</B>,<B>w_current->first_wy</B>). They are not suppose
 *  to change during the action.
 *
 *  The radius of the circle is stored in <B>w_current->distance<B>.
 *
 *  \param [in]  w_current  The GSCHEM_TOPLEVEL object.
 *  \param [in]  o_current  Circle OBJECT to check.
 *  \param [in]  x          (unused)
 *  \param [in]  y          (unused)
 *  \param [out] whichone   Which coordinate to check.
 */
void o_grips_start_circle(GSCHEM_TOPLEVEL *w_current, OBJECT *o_current,
                          int x, int y, int whichone)
{

  w_current->last_drawb_mode = LAST_DRAWB_MODE_NONE;

  /* erase the circle before */
  o_invalidate (w_current, o_current);

  /* store circle center and radius in GSCHEM_TOPLEVEL structure */
  w_current->first_wx = o_current->circle->center_x;
  w_current->first_wy = o_current->circle->center_y;
  w_current->distance = o_current->circle->radius;

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
 *  \param [in]  w_current  The GSCHEM_TOPLEVEL object.
 *  \param [in]  o_current  Line OBJECT to check.
 *  \param [in]  x          (unused)
 *  \param [in]  y          (unused)
 *  \param [out] whichone   Which coordinate to check.
 */
void o_grips_start_line(GSCHEM_TOPLEVEL *w_current, OBJECT *o_current,
                        int x, int y, int whichone)
{
  w_current->last_drawb_mode = LAST_DRAWB_MODE_NONE;

  /* erase the line before */
  o_invalidate (w_current, o_current);

  /* describe the line with GSCHEM_TOPLEVEL variables */
  w_current->second_wx = o_current->line->x[whichone];
  w_current->second_wy = o_current->line->y[whichone];
  w_current->first_wx = o_current->line->x[!whichone];
  w_current->first_wy = o_current->line->y[!whichone];

  /* draw the first temporary line */
  /* o_line_invalidate_rubber (w_current); */
  w_current->rubber_visible = 1;
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
 *  \param [in] w_current  The GSCHEM_TOPLEVEL object.
 *  \param [in] w_x        Current x coordinate of pointer in world units.
 *  \param [in] w_y        Current y coordinate of pointer in world units.
 */
void o_grips_motion(GSCHEM_TOPLEVEL *w_current, int w_x, int w_y)
{
  int grip = w_current->which_grip;

  g_assert( w_current->inside_action != 0 );
  g_return_if_fail( w_current->which_object != NULL );

  switch(w_current->which_object->type) {
    case OBJ_ARC:
      o_arc_motion (w_current, w_x, w_y, grip);
      break;

    case OBJ_BOX:
      o_box_motion (w_current, w_x, w_y);
      break;

    case OBJ_PATH:
      o_path_motion (w_current, w_x, w_y);
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
 *  \param [in,out] w_current  The GSCHEM_TOPLEVEL object.
 */
void o_grips_end(GSCHEM_TOPLEVEL *w_current)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  OBJECT *object;
  int grip;

  object = w_current->which_object;
  grip = w_current->which_grip;

  if (!object) {
    /* actually this is an error condition hack */
    w_current->inside_action = 0;
    i_set_state(w_current, SELECT);
    return;
  }

  switch(object->type) {

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

  /* reset global variables */
  w_current->which_grip = -1;
  w_current->which_object = NULL;

  w_current->rubber_visible = 0;

  toplevel->page_current->CHANGED=1;
  o_undo_savestate(w_current, UNDO_ALL);
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
 *  (the center is unchanged). It is updated with the function #o_arc_modify().
 *
 *  If one of the end of arc grip has been moved - modifying one of the
 *  angles describing the arc -, this angle is updated with the
 *  #o_arc_modify() function.
 *
 *  \param [in] w_current  The GSCHEM_TOPLEVEL object.
 *  \param [in] o_current  Arc OBJECT to end modification on.
 *  \param [in] whichone   Which grip is pointed to.
 */
void o_grips_end_arc(GSCHEM_TOPLEVEL *w_current, OBJECT *o_current, int whichone)
{
  TOPLEVEL *toplevel = w_current->toplevel;
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

    case ARC_END_ANGLE:
      /* get the end angle from w_current */
      arg1 = w_current->second_wy;
      /* second parameter is not used */
      arg2 = -1;
      break;

    default:
      return;
  }

  /* modify the arc with the parameters determined above */
  o_arc_modify(toplevel, o_current, arg1, arg2, whichone);

  /* display the new arc */
  o_invalidate (w_current, o_current);

}

/*! \todo Finish function documentation!!!
 *  \brief End process of modifying box object with grip.
 *  \par Function Description
 *
 *  \param [in] w_current  The GSCHEM_TOPLEVEL object.
 *  \param [in] o_current  Box OBJECT to end modification on.
 *  \param [in] whichone   Which grip is pointed to.
 */
void o_grips_end_box(GSCHEM_TOPLEVEL *w_current, OBJECT *o_current, int whichone)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  int box_width, box_height;

  box_width  = GET_BOX_WIDTH (w_current);
  box_height = GET_BOX_HEIGHT(w_current);

  /* don't allow zero width/height boxes
   * this ends the box drawing behavior
   * we want this? hack */
  if ((box_width == 0) && (box_height == 0)) {
    o_invalidate (w_current, o_current);
    return;
  }

  o_box_modify(toplevel, o_current, w_current->second_wx, w_current->second_wy, whichone);

  /* erase the temporary box */
  /* o_box_invalidate_rubber (w_current); */

  /* draw the modified box */
  o_invalidate (w_current, o_current);
}

/*! \todo Finish function documentation!!!
 *  \brief End process of modifying path object with grip.
 *  \par Function Description
 *
 *  \param [in] w_current  The GSCHEM_TOPLEVEL object.
 *  \param [in] o_current  Picture OBJECT to end modification on.
 *  \param [in] whichone   Which grip is pointed to.
 */
void o_grips_end_path(GSCHEM_TOPLEVEL *w_current, OBJECT *o_current, int whichone)
{
  /* erase the temporary path */
  /* if (w_current->rubber_visible) */
  /*  o_path_invalidate_rubber (w_current); */

  o_path_modify (w_current->toplevel, o_current,
                 w_current->second_wx, w_current->second_wy, whichone);

  /* draw the modified path */
  o_invalidate (w_current, o_current);
}

/*! \todo Finish function documentation!!!
 *  \brief End process of modifying picture object with grip.
 *  \par Function Description
 *
 *  \param [in] w_current  The GSCHEM_TOPLEVEL object.
 *  \param [in] o_current  Picture OBJECT to end modification on.
 *  \param [in] whichone   Which grip is pointed to.
 */
void o_grips_end_picture(GSCHEM_TOPLEVEL *w_current, OBJECT *o_current, int whichone)
{
  TOPLEVEL *toplevel = w_current->toplevel;

  /* erase the temporary picture */
  /* o_picture_invalidate_rubber (w_current); */

  /* don't allow zero width/height picturees
   * this ends the picture drawing behavior
   * we want this? hack */
  if ((GET_PICTURE_WIDTH(w_current) == 0) || (GET_PICTURE_HEIGHT(w_current) == 0)) {
    o_invalidate (w_current, o_current);
    return;
  }

  o_picture_modify(toplevel, o_current, 
		   w_current->second_wx, w_current->second_wy, whichone);

  /* draw the modified picture */
  o_invalidate (w_current, o_current);

  w_current->current_pixbuf = NULL;
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
 *  \param [in] w_current  The GSCHEM_TOPLEVEL object.
 *  \param [in] o_current  Circle OBJECT to end modification on.
 *  \param [in] whichone   Which grip is pointed to.
 */
void o_grips_end_circle(GSCHEM_TOPLEVEL *w_current, OBJECT *o_current, int whichone)
{
  TOPLEVEL *toplevel = w_current->toplevel;

  /* erase the temporary circle */
  /* o_circle_invalidate_rubber (w_current); */

  /* don't allow zero radius circles
   * this ends the circle drawing behavior
   * we want this? hack */
  if (w_current->distance == 0) {
    o_invalidate (w_current, o_current);
    return;
  }

  /* modify the radius of the circle */
  o_circle_modify(toplevel, o_current, w_current->distance, -1, CIRCLE_RADIUS);

  /* display the new circle */
  o_invalidate (w_current, o_current);
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
 *  \param [in] w_current  The GSCHEM_TOPLEVEL object.
 *  \param [in] o_current  Line OBJECT to end modification on.
 *  \param [in] whichone   Which grip is pointed to.
 */
void o_grips_end_line(GSCHEM_TOPLEVEL *w_current, OBJECT *o_current, int whichone)
{
  TOPLEVEL *toplevel = w_current->toplevel;

  /* erase the temporary line */
  /* o_line_invalidate_rubber (w_current); */

  /* don't allow zero length nets / lines / pins
   * this ends the net drawing behavior
   * we want this? hack */
  if ((w_current->first_wx == w_current->second_wx) &&
      (w_current->first_wy == w_current->second_wy)) {
    o_invalidate (w_current, o_current);
    return;
  }

  /* modify the right line end according to whichone */
  o_line_modify(toplevel, o_current, 
		w_current->second_wx, w_current->second_wy, whichone);

  /* display the new line */
  o_invalidate (w_current, o_current);
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
 *  \param [in] w_current  The GSCHEM_TOPLEVEL object.
 *  \param [in] o_current  Net OBJECT to end modification on.
 *  \param [in] whichone   Which grip is pointed to.
 */
void o_grips_end_net(GSCHEM_TOPLEVEL *w_current, OBJECT *o_current, int whichone)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  GList *prev_conn_objects = NULL;
  GList *connected_objects = NULL;

  /* erase the temporary line */
  /* o_line_invalidate_rubber (w_current); */

  /* don't allow zero length net
   * this ends the net drawing behavior
   * we want this? hack */
  if ((w_current->first_wx == w_current->second_wx) &&
      (w_current->first_wy == w_current->second_wy)) {
    o_invalidate (w_current, o_current);
    return;
  }

  /* remove the old net */
  o_invalidate (w_current, o_current);

  prev_conn_objects = s_conn_return_others (prev_conn_objects, o_current);

  s_conn_remove_object (toplevel, o_current);
  o_net_modify (toplevel, o_current, w_current->second_wx,
                w_current->second_wy, w_current->which_grip);
  s_conn_update_object (toplevel, o_current);

  /* get the other connected objects and redraw them */
  connected_objects = s_conn_return_others(connected_objects,
					   o_current);
  /* add bus rippers if necessary */
  o_net_add_busrippers(w_current, o_current, connected_objects);

  /* draw the object objects */
  o_invalidate_glist (w_current, prev_conn_objects);
  
  o_invalidate (w_current, o_current);
  
  g_list_free(connected_objects);
  connected_objects = NULL;
  
  /* get the other connected objects and redraw them */
  connected_objects = s_conn_return_others(connected_objects,
					   o_current);
  
  o_invalidate_glist (w_current, connected_objects);

  g_list_free (prev_conn_objects);
  prev_conn_objects = NULL;
  g_list_free(connected_objects);
  connected_objects = NULL;
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
 *  \param [in] w_current  The GSCHEM_TOPLEVEL object.
 *  \param [in] o_current  Net OBJECT to end modification on.
 *  \param [in] whichone   Which grip is pointed to.
 */
void o_grips_end_pin(GSCHEM_TOPLEVEL *w_current, OBJECT *o_current, int whichone)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  GList *prev_conn_objects = NULL;
  GList *connected_objects = NULL;

  /* erase the temporary line */
  /* o_line_invalidate_rubber (w_current); */

  /* don't allow zero length pin
   * this ends the pin changing behavior
   * we want this? hack */
  if ((w_current->first_wx == w_current->second_wx) &&
      (w_current->first_wy == w_current->second_wy)) {
    o_invalidate (w_current, o_current);
    return;
  }

  /* erase old pin object */
  o_invalidate (w_current, o_current);

  prev_conn_objects = s_conn_return_others (prev_conn_objects, o_current);

  s_conn_remove_object (toplevel, o_current);
  o_pin_modify (toplevel, o_current, w_current->second_wx,
                w_current->second_wy, w_current->which_grip);
  s_conn_update_object (toplevel, o_current);
  o_invalidate (w_current, o_current);

  /* redraw the object connections */
  o_invalidate_glist (w_current, prev_conn_objects);

  /* get the other connected objects and redraw them */
  connected_objects = s_conn_return_others(connected_objects,
					   o_current);
  o_invalidate_glist (w_current, connected_objects);

  /* free the two lists */
  g_list_free (prev_conn_objects);
  prev_conn_objects = NULL;
  g_list_free(connected_objects);
  connected_objects = NULL;
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
 *  \param [in] w_current  The GSCHEM_TOPLEVEL object.
 *  \param [in] o_current  bus OBJECT to end modification on.
 *  \param [in] whichone   Which grip is pointed to.
 */
void o_grips_end_bus(GSCHEM_TOPLEVEL *w_current, OBJECT *o_current, int whichone)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  GList *prev_conn_objects = NULL;
  GList *connected_objects = NULL;

  /* erase the temporary line */
  /* o_line_invalidate_rubber (w_current); */

  /* don't allow zero length bus
   * this ends the bus changing behavior
   * we want this? hack */
  if ((w_current->first_wx == w_current->second_wx) &&
      (w_current->first_wy == w_current->second_wy)) {
    o_invalidate (w_current, o_current);
    return;
  }

  /* erase the old bus and it's cues */
  o_invalidate (w_current, o_current);

  prev_conn_objects = s_conn_return_others (prev_conn_objects, o_current);
  s_conn_remove_object (toplevel, o_current);

  o_bus_modify (toplevel, o_current, w_current->second_wx,
                w_current->second_wy, w_current->which_grip);
  s_conn_update_object (toplevel, o_current);
  o_invalidate (w_current, o_current);

  /* redraw the connected objects */
  o_invalidate_glist (w_current, prev_conn_objects);

  /* get the other connected objects and redraw them */
  connected_objects = s_conn_return_others(connected_objects,
					   o_current);
  o_invalidate_glist (w_current, connected_objects);

  /* free the two lists */
  g_list_free (prev_conn_objects);
  prev_conn_objects = NULL;
  g_list_free(connected_objects);
  connected_objects = NULL;
}


/*! \brief Get half the width and height of grip in screen units.
 *  \par Function Description
 *  According to the current zoom level, the function returns half the width
 *  and height of a grip in screen units.
 *
 *  <B>GRIP_SIZE1</B> and <B>GRIP_SIZE2</B> and <B>GRIP_SIZE3</B> are macros defined
 *  in libgeda #defines.h. They are the half width/height of a grip in
 *  world unit for a determined range of zoom factors.
 *
 *  \param [in] w_current  The GSCHEM_TOPLEVEL object.
 *  \return Half grip size in screen units.
 */
int o_grips_size(GSCHEM_TOPLEVEL *w_current)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  int factor, size;
  
  factor = (int) toplevel->page_current->to_world_x_constant;
  if (factor > SMALL_ZOOMFACTOR1) {
    /* big zoom factor : small size converted to screen unit */
    size = SCREENabs(toplevel, GRIP_SIZE1);
  } else if (factor > SMALL_ZOOMFACTOR2) {
    /* medium zoom factor : medium size converted to screen unit */
    size = SCREENabs(toplevel, GRIP_SIZE2);
  } else {
    /* small zoom factor : big size converted to screen unit */
    size = SCREENabs(toplevel, GRIP_SIZE3);
  }
  
  return size;
}

/*! \brief Draw grip centered at <B>x</B>, <B>y</B>
 *  \par Function Description
 *  This function draws a grip centered at (<B>x</B>,<B>y</B>). Its color is
 *  either the selection color or the overriding color from
 *  <B>toplevel->override_color</B>.
 *
 *  The size of the grip depends on the current zoom factor.
 *
 *  <B>x</B> and <B>y</B> are in screen unit.
 *
 *  \param [in] w_current  The GSCHEM_TOPLEVEL object.
 *  \param [in] x          Center x screen coordinate for drawing grip.
 *  \param [in] y          Center y screen coordinate for drawing grip.
 */
void o_grips_draw(GSCHEM_TOPLEVEL *w_current, int x, int y)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  int color;
  int size;

  /*
   * Depending on the current zoom level, the size of the grip is
   * determined. <B>size</B> is half the width and height of the grip.
   */
  /* size is half the width of grip */
  size = o_grips_size(w_current);

  /*
   * The grip can be displayed or erased : if <B>toplevel->override_color</B>
   * is not set the grip is drawn with the selection color ; if
   * <B>toplevel->override_color</B> is set then the color it refers it
   * is used. This way the grip can be erased if this color is the
   * background color.
   */
  if (toplevel->override_color != -1 ) {
    /* override : use the override_color instead */
    color = toplevel->override_color;
  } else {
    /* use the normal selection color */
    color = SELECT_COLOR;
  }

  /* A grip is a hollow square centered at (<B>x</B>,<B>y</B>)
   * with a  width / height of 2 * <B>size</B>.
   */
  if (toplevel->DONT_REDRAW == 0) {
    gschem_cairo_box (w_current->cr, 1, x - size, y - size, x + size, y + size);

    gschem_cairo_set_source_color (w_current->cr, x_color_lookup (color));
    gschem_cairo_stroke (w_current->cr, TYPE_SOLID, END_NONE, 1, -1, -1);
  }
}


/*! \brief Draw objects being grip maniuplated from GSCHEM_TOPLEVEL object.
 *
 *  \par Function Description
 *  This function draws the objects being grip manipulated.
 *
 *  \param [in] w_current  The GSCHEM_TOPLEVEL object.
 */
void o_grips_draw_rubber (GSCHEM_TOPLEVEL *w_current)
{
  g_return_if_fail (w_current->which_object != NULL);

  switch(w_current->which_object->type) {
    case OBJ_ARC:
      o_arc_draw_rubber (w_current);
      break;

    case OBJ_BOX:
      o_box_draw_rubber (w_current);
      break;

    case OBJ_PATH:
      o_path_draw_rubber (w_current);
      break;

    case OBJ_PICTURE:
      o_picture_draw_rubber (w_current);
      break;

    case OBJ_CIRCLE:
      o_circle_draw_rubber (w_current);
      break;

    case OBJ_LINE:
    case OBJ_NET:
    case OBJ_PIN:
    case OBJ_BUS:
      o_line_draw_rubber (w_current);
    break;

    default:
    return; /* error condition */
  }
}
