/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
 * Copyright (C) 1998-2000 Ales V. Hvezda
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

/* instrumentation code */
#if 0
#include <sys/time.h>
#include <unistd.h>
#endif

#include <gtk/gtk.h>
#include <libguile.h>

#include "defines.h"
#include "struct.h"
#include "globals.h"
#include "o_types.h"
#include "colors.h"

#include "../include/prototype.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

/* \todo 
 * Lots of Gross code... needs lots of cleanup
 * mainly readability issues
 */

/*! \brief Check if point is inside a region
 *  \par Function Description
 *  This function takes a rectangular region and a point.  It will check
 *  if the point is located in the region or not.
 *
 *  \param [in] left    Left coordinate of the region.
 *  \param [in] top     Top coordinate of the region.
 *  \param [in] right   Right coordinate of the region.
 *  \param [in] bottom  Bottom coordinate of the region.
 *  \param [in] x       x coordinate of the point to check.
 *  \param [in] y       y coordinate of the point to check.
 *  \return 1 if the point is inside the region, 0 otherwise.
 */
int inside_region(int left, int top, int right, int bottom, int x, int y)
{
  return ((x >= left && x <= right && y >= top && y <= bottom) ? 1 : 0);
}

/*! \brief Redraw an object on the screen.
 *  \par Function Description
 *  This function will redraw a single object on the screen.
 *
 *  \param [in] w_current  The TOPLEVEL object.
 *  \param [in] o_current  The OBJECT to redraw.
 *
 */
void o_redraw_single(TOPLEVEL *w_current, OBJECT *o_current)
{
  if (o_current == NULL)
  return;
	
  if (w_current->DONT_REDRAW) /* highly experimental */
  return;

  if (o_current->draw_func != NULL && o_current->type != OBJ_HEAD) {
    w_current->inside_redraw = 1;
    (*o_current->draw_func)(w_current, o_current);
    w_current->inside_redraw = 0;
  }
}

/*! \brief Recalculate position of the given object.
 *  \par Function Description
 *  This function will take an object and recalculate its
 *  position on the screen.
 *
 *  \param [in]     w_current    The TOPLEVEL object.
 *  \param [in,out] o_current    OBJECT to recalculate.
 *
 */
void o_recalc_single_object(TOPLEVEL *w_current, OBJECT *o_current)
{
  if (o_current != NULL) {
    switch(o_current->type) {

      case(OBJ_LINE):
        o_line_recalc(w_current, o_current);
        break;

      case(OBJ_NET):
        o_net_recalc(w_current, o_current);
        break;

      case(OBJ_BUS):
        o_bus_recalc(w_current, o_current);
        break;

      case(OBJ_BOX):
        o_box_recalc(w_current, o_current);
        break;

      case(OBJ_PICTURE):
        o_picture_recalc(w_current, o_current);
        break;

      case(OBJ_CIRCLE):
        o_circle_recalc(w_current, o_current);
        break;

      case(OBJ_COMPLEX):
      case(OBJ_PLACEHOLDER):
        o_complex_recalc(w_current, o_current);
        break;

      case(OBJ_PIN):
        o_pin_recalc(w_current, o_current);
        break;

      case(OBJ_ARC):
        o_arc_recalc(w_current, o_current);
        break;
    }
  }
}

/*! \brief Recalculate position of a list of objects.
 *  \par Function Description
 *  This function will take a list of objects and recalculate their
 *  positions on the screen.
 *
 *  \param [in]     w_current    The TOPLEVEL object.
 *  \param [in,out] object_list  OBJECT list to recalculate.
 *
 */
void
o_recalc_object_list(TOPLEVEL *w_current, OBJECT *object_list)
{
  OBJECT *o_current;

  o_current = object_list;
  while (o_current != NULL) {
    o_recalc_single_object(w_current, o_current);
    o_current = o_current->next;
  }
}
 
/*! \brief Recalculate position of a list (GList) of objects.
 *  \par Function Description
 *  This function will take a list (GList) of objects and recalculate their
 *  positions on the screen.
 *
 *  \param [in]     w_current    The TOPLEVEL object.
 *  \param [in,out] object_glist  OBJECT list to recalculate.
 *
 */
void
o_recalc_object_glist(TOPLEVEL *w_current, GList *object_glist)
{
  GList *list = object_glist;
  OBJECT *o_current;

  while (list != NULL) {
    o_current = (OBJECT *) list->data;
    o_recalc_single_object(w_current, o_current);
   list = list->next;
  }
}




/*! \brief Set an #OBJECT's line options.
 *  \par Function Description
 *  This function allows a line's end, type, width, length and space to be set.
 *  See #OBJECT_END and #OBJECT_TYPE for information on valid
 *  object end and type values.
 *
 *  \param [in]     w_current  The TOPLEVEL object.
 *  \param [in,out] o_current  OBJECT to set line options on.
 *  \param [in]     end        An OBJECT_END.
 *  \param [in]     type       An OBJECT_TYPE.
 *  \param [in]     width      Line width.
 *  \param [in]     length     Line length.
 *  \param [in]     space      Spacing between dashes/dots. Cannot be negative.
 *
 *  \todo Make space an unsigned int and check for a max value instead.
 *        If a max value is not required, then it would simplify the code.
 */
void o_set_line_options(TOPLEVEL *w_current, OBJECT *o_current,
			OBJECT_END end, OBJECT_TYPE type,
			int width, int length, int space) 
{
  if(o_current == NULL) {
    return;
  }

  /* do some error checking / correcting */
  switch(type) {
    case(TYPE_DOTTED):
    if (space < 1) {
      space = 100;
      s_log_message ("Invalid space specified, setting to 100\n");
    }
    break;
    case(TYPE_DASHED):
    case(TYPE_CENTER):
    case(TYPE_PHANTOM):
    if (length < 1) {
      length = 100;
      s_log_message ("Invalid length specified, setting to 100\n");
    }
    if (space < 1) {
      space = 100;
      s_log_message ("Invalid space specified, setting to 100\n");
    }
    break;
    default:
    
    break;
  }
  
  o_current->line_width = width;
  o_current->line_end   = end;
  o_current->line_type  = type;

  o_current->line_length = length;
  o_current->line_space  = space;
}

/*! \brief Set #OBJECT's fill options.
 *  \par Function Description
 *  This function allows an #OBJECT's fill options to be configured.
 *  See #OBJECT_FILLING for information on valid fill types.
 *
 *  \param [in]      w_current  The TOPLEVEL object.
 *  \param [in,out]  o_current  OBJECT to be updated.
 *  \param [in]      type       OBJECT_FILLING type.
 *  \param [in]      width      fill width.
 *  \param [in]      pitch1     cross hatch???.
 *  \param [in]      angle1     cross hatch???.
 *  \param [in]      pitch2     cross hatch???.
 *  \param [in]      angle2     cross hatch???.
 *
 */
void o_set_fill_options(TOPLEVEL *w_current, OBJECT *o_current,
			OBJECT_FILLING type, int width,
			int pitch1, int angle1,
			int pitch2, int angle2) 
{
  if(o_current == NULL) {
    return;
  }

  o_current->fill_type = type;
  o_current->fill_width = width;

  o_current->fill_pitch1 = pitch1;
  o_current->fill_angle1 = angle1;

  o_current->fill_pitch2 = pitch2;
  o_current->fill_angle2 = angle2;
	
}

/*! \brief Recalculate a single OBJECT in screen coordinates.
 *  \par Function Description
 *  This function takes an OBJECT and converts it to SCREEN coordinates.
 *
 *  \param [in]     w_current  The TOPLEVEL object.
 *  \param [in,out] o_current  OBJECT to recalculate.
 *
 */
void o_object_recalc(TOPLEVEL *w_current, OBJECT *o_current) 
{
  int width, length, space, pitch;
	
  if(o_current == NULL) {
    return;
  }

  width = SCREENabs(w_current, o_current->line_width);
  o_current->screen_line_width = width;

  length = SCREENabs(w_current, o_current->line_length);
  o_current->screen_line_length = length;

  space = SCREENabs(w_current, o_current->line_space);
  o_current->screen_line_space = space;

  width = SCREENabs(w_current, o_current->fill_width);
  o_current->screen_fill_width = width;
  pitch = SCREENabs(w_current, o_current->fill_pitch1);
  o_current->screen_fill_pitch1 = pitch;
  pitch = SCREENabs(w_current, o_current->fill_pitch2);
  o_current->screen_fill_pitch2 = pitch;

}


