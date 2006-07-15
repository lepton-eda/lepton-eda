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
#include <math.h>

#include <gtk/gtk.h>
#include <libguile.h>

#ifdef HAS_LIBGDGEDA
#include <gdgeda/gd.h>
#endif

#include "defines.h"
#include "struct.h"
#include "globals.h"
#include "o_types.h"
#include "colors.h"
#include "funcs.h"

#include "../include/prototype.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

/*! \brief
 *  \par Function Description
 *  The function creates a new object of type arc and links it
 *  with the object pointed by parameter object_list. As the
 *  new object is then the last object of the list, its pointer
 *  is returned to update the end of list pointer in the calling function.
 *  The arc is defined by its center in parameters x and y.
 *  The radius parameter specifies the radius of the arc. The start
 *  angle is given by start_angle and the end angle by end_angle.
 *  The line and fill type of the created arc are set to default.
 *
 *  All dimensions are in world unit, except start_angle and
 *  end_angle in degrees.
 *  
 *  A new object of type OBJECT is allocated. Its type and color
 *  are initilized. The description of the arc characteristics
 *  are stored in a new ARC structure.
 *
 *  Now fixed for world coordinates.
 *
 *  \param [in] w_current    The TOPLEVEL object.
 *  \param [in] object_list  
 *  \param [in] type
 *  \param [in] color
 *  \param [in] x
 *  \param [in] y
 *  \param [in] radius
 *  \param [in] start_angle
 *  \param [in] end_angle
 *  \return
 */
OBJECT *o_arc_add(TOPLEVEL *w_current, OBJECT *object_list,
		  char type, int color,
		  int x, int y, int radius, int start_angle, int end_angle)
{

  OBJECT *new_node;

  new_node = s_basic_init_object("arc");
  new_node->type = type;
  new_node->color = color;

  new_node->arc = (ARC *) g_malloc(sizeof(ARC));

  /*! \note
   *  The ARC structure is initialized with the parameters.
   *  A default initialization is performed for the line and
   *  fill type to avoid misunderstood.
   *
   *  The functions relative to the use of the object are sets.
   */

  /* World coordinates */
  new_node->arc->x      = x; 
  new_node->arc->y      = y; 
  new_node->arc->width  = 2 * radius;
  new_node->arc->height = 2 * radius;

  /* PB : must check the sign of start_angle, end_angle ... */
  if(end_angle < 0) {
    start_angle = start_angle + end_angle;
    end_angle = -end_angle;
  }
  if(start_angle < 0) start_angle = 360 + start_angle;
  
  new_node->arc->start_angle = start_angle;
  new_node->arc->end_angle   = end_angle;

  /* Default init */
  o_set_line_options(w_current, new_node,
                     END_NONE, TYPE_SOLID, 0, -1, -1);
  o_set_fill_options(w_current, new_node,
                     FILLING_HOLLOW, -1, -1, -1, -1, -1);
	
  o_arc_recalc(w_current, new_node);

  /* new_node->graphical = arc; eventually */
	
  /* \todo questionable cast */
  new_node->draw_func = (void *) arc_draw_func;  
  /* \todo questionable cast */
  new_node->sel_func = (void *) select_func;

  /* \note
   * The new object is linked to the object given in parameter object_list
   * and then returned as a pointer to the calling function.
   */

  object_list = (OBJECT *) s_basic_link_object(new_node, object_list);
	
  return(object_list);
}

/*! \brief 
 *  \par Function Description
 *  This function creates a new object representing an arc.
 *
 *  The values of the <B>o_current</B> pointed OBJECT are then copied to the new object.
 *
 *  The arc, the line options are initialized whereas the fill options are
 *  initialized to passive values - as an arc can not be filled.
 *
 *  The new object is added to the end of the object list given by <B>list_tail</B>.
 *  A pointer on it is returned for update purpose in the calling function.
 *
 *  \param [in] w_current  The TOPLEVEL object
 *  \param [in] list_tail
 *  \param [in] o_current
 *  \return
 */
OBJECT *o_arc_copy(TOPLEVEL *w_current, OBJECT *list_tail,
		   OBJECT *o_current)
{
  OBJECT *new_obj;
  ATTRIB *a_current;
  int color;

  if (o_current->saved_color == -1) {
    color = o_current->color;
  } else {
    color = o_current->saved_color;
  }

  new_obj = o_arc_add(w_current, list_tail, OBJ_ARC, color,
                      o_current->arc->x, o_current->arc->y, 
                      o_current->arc->width / 2,
                      o_current->arc->start_angle,
                      o_current->arc->end_angle);
  o_set_line_options(w_current, new_obj,
                     o_current->line_end, o_current->line_type,
                     o_current->line_width,
                     o_current->line_length, o_current->line_space);
  o_set_fill_options(w_current, new_obj,
                     FILLING_HOLLOW, -1, -1, -1, -1, -1);
	
  a_current = o_current->attribs;
  if (a_current) {
    while (a_current) {

      /* head attrib node has prev = NULL */
      if (a_current->prev != NULL) {
        a_current->copied_to = new_obj;
      }
      a_current = a_current->next;
    }
  }

  return(new_obj);
}

/*! \brief
 *  \par Function Description
 *  This function modifies the internal values of the arc object
 *  *object according to the whichone parameter.
 *
 *  The new values are given by <B>x</B> and/or <B>y</B>. Their meaning depends on the value of whichone.
 *
 *  If <B>whichone</B> is equal to #ARC_CENTER, the (<B>x</B>,<B>y</B>) point is taken as the new center
 *  of the arc in world unit.
 *
 *  If <B>whichone</B> is equal to #ARC_RADIUS, the <B>x</B> parameter is taken to be the radius
 *  of the arc in world unit. The <B>y</B> parameter is ignored.
 *
 *  If <B>whichone</B> is equal to #ARC_START_ANGLE, the <B>x</B> parameter is the starting angle of the arc.
 *  <B>x</B> is in degrees. <B>y</B> is ignored.
 *
 *  If <B>whichone</B> is equal to #ARC_END_ANGLE, the <B>x</B> parameter is the ending angle of the arc.
 *  <B>x</B> is in degrees. <B>y</B> is ignored.
 *
 *  The screen coordinates of the arc and its bounding box are computed again
 *  after the change in world coordinates.
 *
 *  \param [in]     w_current  The TOPLEVEL object.
 *  \param [in,out] object     
 *  \param [in]     x
 *  \param [in]     y
 *  \param [in]     whichone
 */
void o_arc_modify(TOPLEVEL *w_current, OBJECT *object, 
		  int x, int y, int whichone)
{

	switch(whichone) {
		case ARC_CENTER:
		/* modify the center of arc object */
		object->arc->x = x;
		object->arc->y = y;
		break;
		
		case ARC_RADIUS:
		/* modify the radius of arc object */
		object->arc->width  = 2 * x;
		object->arc->height = 2 * x;
		break;

		case ARC_START_ANGLE:
		/* modify the start angle of the arc object */
		object->arc->start_angle = x;
		break;

		case ARC_END_ANGLE:
		/* modify the end angle of the arc object */
		object->arc->end_angle = x;
		break;

		default:
		break;
	}

	/* update the screen coords and the bounding box */
	o_arc_recalc(w_current, object);
	
}

/*! \brief
 *  \par Function Description
 *  This function reads a formatted text buffer describing an arc
 *  in the gEDA file format and initializes the corresponding object.
 *  This arc is linked to the end of the <B>object_list</B> pointed list.
 *  Depending on the version of the file format the data extraction is
 *  performed differently : currently pre-20000704 and 20000704 on one
 *  hand and post-20000704 file format version on the other hand are supported.
 *  The version is specified in string pointed by <B>fileformat_ver</B>.
 *
 *  To get information on the various file formats have a
 *  look to the fileformats.html document.
 *  
 *  The object is initialized with the functions #o_set_line_options() and #o_set_fill_options().
 *  The second one is only used to put initialize unused values for an arc as an arc can not be filled.
 * 
 *  The arc is allocated initialized and linked with the function #o_arc_add().
 * 
 *  A negative or null radius is not allowed.
 *
 *  \param [in] w_current    The TOPLEVEL object.
 *  \param [in] object_list  
 *  \param [in] buf
 *  \param [in] release_ver
 *  \param [in] fileformat_ver
 *  \return
 */
OBJECT *o_arc_read(TOPLEVEL *w_current, OBJECT *object_list, char buf[],
		   unsigned int release_ver, unsigned int fileformat_ver)
{
  char type; 
  int x1, y1;
  int radius;
  int start_angle, end_angle;
  int color;
  int arc_width, arc_length, arc_space;
  int arc_type;
  int arc_end;

  /*! \note
   *  Depending on the version of the file format used to describe this arc,
   *  the buffer is parsed differently. The unknown parameters of the less
   *  restrictive - the oldest - file format are set to common values
   */
  if(release_ver <= VERSION_20000704) {
    sscanf(buf, "%c %d %d %d %d %d %d", &type,
           &x1, &y1, &radius, &start_angle, &end_angle, &color);

    arc_width = 0;
    arc_end   = END_NONE;
    arc_type  = TYPE_SOLID;
    arc_space = -1;
    arc_length= -1;
  } else {
    sscanf(buf, "%c %d %d %d %d %d %d %d %d %d %d %d", &type,
           &x1, &y1, &radius, &start_angle, &end_angle, &color,
           &arc_width, &arc_end, &arc_type, &arc_length, &arc_space);

  }

  /* Error check */
  if (radius <= 0) {
    fprintf(stderr,
            "Found a zero radius arc [ %c %d, %d, %d, %d, %d, %d ]\n",
            type, x1, y1, radius, start_angle, end_angle, color);
    s_log_message
      ("Found a zero radius arc [ %c %d, %d, %d, %d, %d, %d ]\n",
       type, x1, y1, radius, start_angle, end_angle, color);
  }
	
  if (color < 0 || color > MAX_COLORS) {
    fprintf(stderr, "Found an invalid color [ %s ]\n", buf);
    s_log_message("Found an invalid color [ %s ]\n", buf);
    s_log_message("Setting color to WHITE\n");
    color = WHITE;
  }

  /* Allocation and initialization */
  object_list = o_arc_add(w_current, object_list, OBJ_ARC, color,
                          x1, y1, radius, start_angle, end_angle);
  o_set_line_options(w_current, object_list,
                     arc_end, arc_type, arc_width, arc_length,
                     arc_space);
  o_set_fill_options(w_current, object_list,
                     FILLING_HOLLOW, -1, -1, -1,
                     -1, -1);

  return(object_list);
}

/*! \brief
 *  \par Function Description
 *  This function formats a string in the buffer <B>*buff</B> to describe
 *  the arc object <B>*object</B>.
 *  It follows the post-20000704 release file format that handle the
 *  line type and filling options.
 *  A pointer to the new allocated and formated string is returned.
 *  The string must be freed at some point.
 *
 *  \param [in] object
 *  \return
 *
 *  \todo EEK! there is a nasty non-snap bug here!
 *        Basically the center isn't being snapped
 *        in complex objects only it seems...
 */
char *o_arc_save(OBJECT *object)
{
  int color;
  int x, y, radius, start_angle, end_angle;
  int arc_width, arc_length, arc_space;
  char *buf;
  OBJECT_END arc_end;
  OBJECT_TYPE arc_type;

  /* radius, center and angles of the arc */
  radius      = object->arc->width / 2;
  x           = object->arc->x;
  y           = object->arc->y;
  start_angle = object->arc->start_angle;
  end_angle   = object->arc->end_angle;

  /* line type parameters */
  arc_width  = object->line_width;
  arc_end    = object->line_end;
  arc_type   = object->line_type;
  arc_length = object->line_length;
  arc_space  = object->line_space;

  /* Save the right color */
  if (object->saved_color == -1) {
    color = object->color;
  } else {
    color = object->saved_color;
  }

  /* Describe a circle with post-20000704 file format */
  buf = g_strdup_printf("%c %d %d %d %d %d %d %d %d %d %d %d", object->type,
			x, y, radius, start_angle, end_angle, color,
			arc_width, arc_end, arc_type, arc_length, arc_space);

  return(buf);
}


/*! \brief
 *  \par Function Description
 *  This function translates the arc described in the <B>object</B>
 *  pointed structure by <B>dx</B> horizontally and <B>dy</B> vertically.
 *  <B>dx</B> and <B>dy</B> are in screen unit.
 *
 *  The translation vector is converted in world unit. The translation
 *  is made with <B>o_arc_translate_world()</B> that also updates the
 *  screen coordinates and the bounding box.
 *
 *  \param [in] w_current  The TOPLEVEL object.
 *  \param [in] dx         
 *  \param [in] dy         
 *  \param [in] object
 */
void o_arc_translate(TOPLEVEL *w_current, int dx, int dy, OBJECT *object)
{
  int world_dx, world_dy;
  
  if (object == NULL) {
    return;
  }

  /* convert the translation vector in world unit */
  world_dx = SCREENabs(w_current, dx);
  world_dy = SCREENabs(w_current, dy);
  
  /* translate the arc */
  o_arc_translate_world(w_current, world_dx, world_dy, object);

  /* screen coords and boundings are updated by _translate_world() */
	
}

/*! \brief
 *  \par Function Description
 *  This function applies a translation of (<B>dx</B>,<B>dy</B>)
 *  to the arc described in <B>*object</B>. <B>dx</B> and <B>dy</B> are in world unit.
 *
 *  \param [in] w_current  The TOPLEVEL object.
 *  \param [in] dx
 *  \param [in] dy
 *  \param [in] object
 */
void o_arc_translate_world(TOPLEVEL *w_current, int dx, int dy,
			   OBJECT *object)
{
  if (object == NULL) {
    return;
  }

  /* Do world coords */
  object->arc->x = object->arc->x + dx;
  object->arc->y = object->arc->y + dy;


  /* Recalculate screen coords from new world coords */
  o_arc_recalc(w_current, object);
}

/*! \brief
 *  \par Function Description
 *  This function applies a rotation of center (<B>centerx</B>,<B>centery</B>)
 *  and angle <B>angle</B> to the arc object <B>*object</B>.
 *  The coordinates of the rotation center are in screen units.
 *  The angle is in degree.
 *
 *  The rotation is made with th <B>o_arc_rotate_world()</B> function
 *  that perform a rotation of <B>angle</B> and center
 *  (<B>world_centerx</B>,<B>world_centery</B>) in world unit.
 *
 *  \param [in] w_current  The TOPLEVEL object.
 *  \param [in] centerx
 *  \param [in] centery
 *  \param [in] angle
 *  \param [in] object
 */
void o_arc_rotate(TOPLEVEL *w_current,
		  int centerx, int centery, int angle,
		  OBJECT *object)
{
  int world_centerx, world_centery;

  /* convert the center of rotation to world unit */
  SCREENtoWORLD(w_current,
				centerx, centery,
                &world_centerx, &world_centery);

  /* rotate the arc */
  o_arc_rotate_world(w_current,
					 world_centerx, world_centery, angle,
					 object);

  /* screen coords and boundings are updated by _rotate_world() */
  
}                                   

/*! \brief
 *  \par Function Description
 *  This function rotates the world coordinates of an arc of an angle
 *  specified by <B>angle</B>. The center of the rotation is given by
 *  (<B>world_centerx</B>,<B>world_centery</B>).
 *
 *  The arc is translated in order to put the center of the rotation
 *  on the origin. The center of the arc is then rotated of the angle
 *  specified by <B>angle</B>. The start angle of the arc is incremented by <B>angle</B>.
 *
 *  The arc is finally back translated to its previous location on the page.
 *
 *  <B>world_centerx</B> and <B>world_centery</B> are in world units, <B>angle</B> is in degrees.
 *
 *  \param [in] w_current      The TOPLEVEL object.
 *  \param [in] world_centerx
 *  \param [in] world_centery
 *  \param [in] angle
 *  \param [in] object
 */
void o_arc_rotate_world(TOPLEVEL *w_current,
			int world_centerx, int world_centery, int angle,
			OBJECT *object)
{
  int x, y, newx, newy;

  /* translate object to origin */
  object->arc->x -= world_centerx;
  object->arc->y -= world_centery;

  /* get center, and rotate center */
  x = object->arc->x;
  y = object->arc->y;
  if(angle % 90 == 0) {
	  rotate_point_90(x, y, angle % 360, &newx, &newy);
  } else {
	  rotate_point(x, y, angle % 360, &newx, &newy);
  }
  object->arc->x = newx;
  object->arc->y = newy;

  /* apply rotation to angles */
  object->arc->start_angle = (object->arc->start_angle + angle) % 360;
  /* end_angle is unchanged as it is the sweep of the arc */
  /* object->arc->end_angle = (object->arc->end_angle); */

  /* translate object to its previous place */
  object->arc->x += world_centerx;
  object->arc->y += world_centery;

  /* update the screen coords and the bounding box */
  o_arc_recalc(w_current, object);
  
}                                   

/*! \brief
 *  \par Function Description
 *  This function mirrors the screen coordinates of an arc.
 *  The symetry axis is given by the vertical line going
 *  through the point (<B>centerx</B>,<B>centery</B>).
 *  
 *  <B>centerx</B> and <B>centery</B> are in screen unit.
 *
 *  The arc is translated in order to put the point (<B>centerx</B>,<B>centery</B>)
 *  on the origin. The center of the arc is then mirrored. The start angle of
 *  the arc and the sweep of the arc are also mirrored.
 * 
 *  The arc is finally back translated to its previous location on the page.
 *
 *  \param [in] w_current  The TOPLEVEL object.
 *  \param [in] centerx
 *  \param [in] centery
 *  \param [in] object
 */
void o_arc_mirror(TOPLEVEL *w_current,
		  int centerx, int centery,
		  OBJECT *object)
{
  int world_centerx, world_centery;

  /* convert mirror origin in world unit */
  SCREENtoWORLD(w_current, centerx, centery,
                &world_centerx, &world_centery);

  /* mirror the arc */
  o_arc_mirror_world(w_current, world_centerx, world_centery, object);

  /* screen coords and boundings are updated by _rotate_world() */
  
}

/*! \brief Mirror the WORLD coordinates of an ARC.
 *  \par Function Description
 *  This function mirrors the world coordinates of an arc.
 *  The symetry axis is given by the vertical line going through the point (<B>world_centerx</B>,<B>world_centery</B>).
 *
 *  The arc is translated in order to put the point (<B>world_centerx</B>,<B>world_centery</B>)
 *  on the origin. The center of the arc is then mirrored. The start angle of the arc
 *  and the sweep of the arc are also mirrored.
 *
 *  The arc is finally back translated to its previous location on the page.
 *
 *  \param [in] w_current      The TOPLEVEL object.
 *  \param [in] world_centerx
 *  \param [in] world_centery
 *  \param [in] object
 */
void o_arc_mirror_world(TOPLEVEL *w_current,
			int world_centerx, int world_centery,
			OBJECT *object)
{
  /* translate object to origin */
  object->arc->x -= world_centerx;
  object->arc->y -= world_centery;

  /* get center, and mirror it (vertical mirror) */
  object->arc->x = -object->arc->x;
  object->arc->y =  object->arc->y;

  /* apply mirror to angles (vertical mirror) */
  object->arc->start_angle = (180 - object->arc->start_angle) % 360;
  /* pb20011125 - start_angle *MUST* be positive */
  if(object->arc->start_angle < 0) object->arc->start_angle += 360;
  object->arc->end_angle = -object->arc->end_angle;
	
  /* translate object back to its previous position */
  object->arc->x += world_centerx;
  object->arc->y += world_centery;

  /* update the screen coords and bounding box */
  o_arc_recalc(w_current, object);
	
}

/*! \brief
 *  \par Function Description
 *  This function recalculates internal parameters in screen units
 *  of an object containing an arc. The object is given as parameters <B>o_current</B>.
 *  The calculation is done according to the zoom factor detailed in the <B>w_current</B>
 *  pointed structure.
 *  It also recalculates the <B>OBJECT</B> specific fields and the bounding box of the arc.
 *  
 *  The <B>OBJECT</B> specific fields are handled by the function <B>o_object_recalc()</B>
 *  whereas bounding box - in screen units - is recalculated with the <B>get_arc_bounds()</B> function.
 *
 *  \param [in] w_current  The TOPLEVEL object.
 *  \param [in] o_current
 */
void o_arc_recalc(TOPLEVEL *w_current, OBJECT *o_current)
{
  int screen_x1, screen_y1, screen_x2, screen_y2;	
  int left, right, top, bottom;
	
  if (o_current->arc == NULL) {
    return;
  }

  /* update the screen_x and screen_y fields of the arc */
  WORLDtoSCREEN(w_current, o_current->arc->x, o_current->arc->y, 
                &screen_x1, &screen_y1);  

  o_current->arc->screen_x = screen_x1; /* x coord */
  o_current->arc->screen_y = screen_y1; /* y coord */

  /* update the screen_width and screen_height fields of the arc */
  WORLDtoSCREEN(w_current,
                o_current->arc->x + o_current->arc->width,
                o_current->arc->y - o_current->arc->height, 
                &screen_x2, &screen_y2);  

  o_current->arc->screen_width  = screen_x2 - screen_x1; /* width */
  o_current->arc->screen_height = screen_y2 - screen_y1; /* height */

  /* recalculates the line type information in o_current */
  o_object_recalc(w_current, o_current);

  /* recalculates the bounding box */
  get_arc_bounds(w_current, o_current, &left, &top, &right, &bottom);
  o_current->left   = left;
  o_current->top    = top;
  o_current->right  = right;
  o_current->bottom = bottom;

}

/*! \brief
 *  \par Function Description
 *  This function calculates the smallest rectangle the arc can be drawn into.
 *  The <B>OBJECT</B> pointed by object is assumed to be an arc.
 *  The <B>left</B>, <B>top</B>, <B>right</B> and <B>bottom</B> pointed integers define
 *  this rectangle at the end of the function. It is expressed in screen units.
 *
 *  The process is divided into two steps : the first step is to calculate the
 *  coordinates of the two ends of the arc and the coordinates of the center.
 *  They form a first rectangle but (depending on the start angle and the sweep
 *  of the arc) not the right.
 *
 *  \param [in]  w_current  The TOPLEVEL object.
 *  \param [in]  object
 *  \param [out] left
 *  \param [out] top
 *  \param [out] right
 *  \param [out] bottom
 */
void get_arc_bounds(TOPLEVEL *w_current, OBJECT *object,
		    int *left, int *top, int *right, int *bottom)
{
  int x1, y1, x2, y2, x3, y3;
  int radius, start_angle, end_angle;
  int i, angle;

  radius      = object->arc->screen_width / 2;
  start_angle = object->arc->start_angle % 360;
  end_angle   = object->arc->end_angle   % 360;

  x1 = object->arc->screen_x;
  y1 = object->arc->screen_y;
  x2 = x1 + radius * cos(start_angle * M_PI / 180);
  y2 = y1 - radius * sin(start_angle * M_PI / 180);
  x3 = x1 + radius * cos((start_angle + end_angle) * M_PI / 180);
  y3 = y1 - radius * sin((start_angle + end_angle) * M_PI / 180);

  *left   = (x1 < x2) ? ((x1 < x3) ? x1 : x3) : ((x2 < x3) ? x2 : x3);
  *right  = (x1 > x2) ? ((x1 > x3) ? x1 : x3) : ((x2 > x3) ? x2 : x3);
  *top    = (y1 < y2) ? ((y1 < y3) ? y1 : y3) : ((y2 < y3) ? y2 : y3);
  *bottom = (y1 > y2) ? ((y1 > y3) ? y1 : y3) : ((y2 > y3) ? y2 : y3);

  /*! \note
   *  The previous rectangle is extended to the final one by checking
   *  whether the arc is over a main axis (vertical or horizontal).
   *  If so, the rectangle is extended in these directions.
   */
  angle = ((int) (start_angle / 90)) * 90;
  for(i = 0; i < 4; i++) {
    angle = angle + 90;
    if(angle < start_angle + end_angle) {
      if(angle % 360 == 0)   *right  = x1 + radius;
      if(angle % 360 == 90)  *top    = y1 - radius;
      if(angle % 360 == 180) *left   = x1 - radius;
      if(angle % 360 == 270) *bottom = y1 + radius;
    } else {
      break;
    }
  }

  /* PB : bounding box has to take into account the width of the line it is
     composed with, ie adding/substracting half the width to this box */
  /* PB : but width is unknown here */	
   
}

/*! \brief
 *  \par Function Description
 *  This function calculates the smallest rectangle the arc can be drawn into.
 *  The <B>OBJECT</B> pointed by object is assumed to be an arc.
 *  The <B>left</B>, <B>top</B>, <B>right</B> and <B>bottom</B> pointed integers define
 *  this rectangle at the end of the function. It is expressed in world units.
 *  The process is divided into two steps : the first step is to calculate the
 *  coordinates of the two ends of the arc and the coordinates of the center.
 *  They forms a first rectangle but (depending on the start angle and the
 *  sweep of the arc) not the right.
 *
 *  \param [in]  w_current  The TOPLEVEL object.
 *  \param [in]  object
 *  \param [out] left
 *  \param [out] top
 *  \param [out] right
 *  \param [out] bottom
 */
void world_get_arc_bounds(TOPLEVEL *w_current, OBJECT *object, int *left,
			  int *top, int *right, int *bottom)
{
  int x1, y1, x2, y2, x3, y3;
  int radius, start_angle, end_angle;
  int i, angle;

  radius      = object->arc->width / 2;
  start_angle = object->arc->start_angle % 360;
  end_angle   = object->arc->end_angle   % 360;

  x1 = object->arc->x;
  y1 = object->arc->y;
  x2 = x1 + radius * cos(start_angle * M_PI / 180);
  y2 = y1 + radius * sin(start_angle * M_PI / 180);
  x3 = x1 + radius * cos((start_angle + end_angle) * M_PI / 180);
  y3 = y1 + radius * sin((start_angle + end_angle) * M_PI / 180);

  *left   = (x1 < x2) ? ((x1 < x3) ? x1 : x3) : ((x2 < x3) ? x2 : x3);
  *right  = (x1 > x2) ? ((x1 > x3) ? x1 : x3) : ((x2 > x3) ? x2 : x3);
  *bottom = (y1 < y2) ? ((y1 < y3) ? y1 : y3) : ((y2 < y3) ? y2 : y3);
  *top    = (y1 > y2) ? ((y1 > y3) ? y1 : y3) : ((y2 > y3) ? y2 : y3);

  /*! \note
   *  The previous rectangle is extended to the final one
   *  by checking whether the arc is over a main axis (vertical or horizontal).
   *  If so, the rectangle is extended in these directions.
   */
  angle = ((int) (start_angle / 90)) * 90;
  for(i = 0; i < 4; i++) {
    angle = angle + 90;
    if(angle < start_angle + end_angle) {
      if(angle % 360 == 0)   *right  = x1 + radius;
      if(angle % 360 == 90)  *top    = y1 + radius;
      if(angle % 360 == 180) *left   = x1 - radius;
      if(angle % 360 == 270) *bottom = y1 - radius;
    } else {
      break;
    }
  }

  /* PB : same problem as above */
}


/*! \brief
 *  \par Function Description
 *  This function writes in a postscript file the arc described by
 *  the <B>o_current</B> pointed object.
 *  The postscript resulting file is described by the <B>fp</B> file pointer.
 *  
 *  Parameters of the arc are extracted from object pointed by <B>o_current</B>
 *  and formatted to suit future calls to specialized arc printing functions.
 *
 *  \param [in] w_current  The TOPLEVEL object.
 *  \param [in] fp         The postscript document to print to.
 *  \param [in] o_current
 *  \param [in] origin_x
 *  \param [in] origin_y
 */
void o_arc_print(TOPLEVEL *w_current, FILE *fp, OBJECT *o_current, 
		 int origin_x, int origin_y)
{
  int x, y, radius, start_angle, end_angle;
  int color;
  int arc_width, space, length;
  void (*outl_func)() = NULL;

  if (o_current == NULL) {
    printf("got null in o_arc_print\n");
    return;
  }
	
  x      = o_current->arc->x;
  y      = o_current->arc->y;
  radius = o_current->arc->width / 2;
  start_angle = o_current->arc->start_angle;
  end_angle   = o_current->arc->end_angle;
  color  = o_current->color;

  /*! \note
   *  Depending on the type of the line for this particular arc, the
   *  appropriate function is chosen among #o_arc_print_solid(),
   *  #o_arc_print_dotted(), #o_arc_print_dashed(), #o_arc_print_center() and #o_arc_print_phantom().
   *
   *  The needed parameters for each of these types are extracted from the <B>o_current</B> object.
   *  Depending on the type, unused parameters are set to -1.
   *  
   *  In the eventuality of a length and/or space null, the arc is printed solid to avoid and
   *  endless loop produced by other functions.
   */

#if 0  /* was causing arcs which are solid to be much thinner compared to */
  /* lines, boxes, also of zero width */
  if (o_current->line_width > 0) {
    arc_width = o_current->line_width;
  } else {
    arc_width = 1;
  }
#endif
  arc_width = o_current->line_width;	/* Added instead of above */
  if (arc_width <= 2) arc_width = 2;
  length = o_current->line_length;
  space  = o_current->line_space;
	
  switch(o_current->line_type) {
    case(TYPE_SOLID):
      length = -1; space = -1;
      outl_func = (void *) o_arc_print_solid;
      break;
			
    case(TYPE_DOTTED):
      length = -1;
      outl_func = (void *) o_arc_print_dotted;
      break;
			
    case(TYPE_DASHED):
      outl_func = (void *) o_arc_print_dashed;
      break;
			
    case(TYPE_CENTER):
      outl_func = (void *) o_arc_print_center;
      break;
			
    case(TYPE_PHANTOM):
      outl_func = (void *) o_arc_print_phantom;
      break;
			
    case(TYPE_ERASE):
      /* Unused for now, print it solid */
      length = -1; space = -1;
      outl_func = (void *) o_arc_print_solid;
      break;
  }

  if((space == 0) || (length == 0)) {
    length = -1; space = -1;
    outl_func = (void *) o_arc_print_solid;
  }

  (*outl_func)(w_current, fp,
               x - origin_x, y - origin_x, radius,
               start_angle, end_angle,
               color, arc_width, length, space, origin_x, origin_y);
}


/*! \brief
 *  \par Function Description
 *  This function prints an arc when a solid line type is required.
 *  The arc is defined by its center in <B>x</B> and <B>y</B>, its radius
 *  in <B>radius</B> and the start and end angles of the arc on the circle.
 *  The postscript file is defined by the file pointer <B>fp</B>.
 *
 *  The parameters <B>length</B> and <B>space</B> are ignored
 *  whereas <B>arc_width</B> specifies the width of the printed line.
 *
 *  All dimensions are in mils, except <B>angle1</B> and <B>angle2</B> in degrees.
 *
 *  \param [in] w_current  The TOPLEVEL object.
 *  \param [in] fp         FILE pointer to postscript document.
 *  \param [in] x
 *  \param [in] y
 *  \param [in] radius
 *  \param [in] angle1
 *  \param [in] angle2
 *  \param [in] color
 *  \param [in] arc_width
 *  \param [in] length
 *  \param [in] space
 *  \param [in] origin_x
 *  \param [in] origin_y
 */
void o_arc_print_solid(TOPLEVEL *w_current, FILE *fp,
		       int x, int y, int radius,
		       int angle1, int angle2,
		       int color,
		       int arc_width, int length, int space,
		       int origin_x, int origin_y)
{
  if (w_current->print_color) {
    f_print_set_color(fp, color);
  }

  /* PB/AVH inverting angle2 if < 0 and changing angle1 accordingly */
  if (angle2 < 0) {
    angle1 = angle1 + angle2;
    angle2 = -angle2;
  }

  fprintf(fp, "%d %d %d %d %d %d darc\n",
	  x,y, radius, angle1, angle1 + angle2,
	  arc_width);

}

/*! \brief
 *  \par Function Description
 *  This function prints an arc when a dotted line type is required.
 *  The arc is defined by its center in <B>x</B> and <B>y</B>, its
 *  radius in <B>radius</B> and the start and end angles of the arc on the circle.
 *  The postscript file is defined by the file pointer <B>fp</B>.
 *  The parameter <B>length</B> is ignored whereas <B>arc_width</B> specifies
 *  the diameter of the dots of the printed line and <B>space</B> the distance
 *  between two dots.
 *  
 *  A negative value for <B>space</B> leads to an endless loop.
 *
 *  All dimensions are in mils, except <B>angle1</B> and <B>angle2</B> in degrees.
 *
 *  The function sets the color the line will be printed with.
 * 
 *  \param [in] w_current  The TOPLEVEL object.
 *  \param [in] fp         FILE pointer to postscript document.
 *  \param [in] x
 *  \param [in] y
 *  \param [in] radius
 *  \param [in] angle1
 *  \param [in] angle2
 *  \param [in] color
 *  \param [in] arc_width
 *  \param [in] length
 *  \param [in] space
 *  \param [in] origin_x
 *  \param [in] origin_y
 */
void o_arc_print_dotted(TOPLEVEL *w_current, FILE *fp,
			int x, int y, int radius,
			int angle1, int angle2,
			int color,				   
			int arc_width, int length, int space,
			int origin_x, int origin_y)
{
  int da, d;


  if (w_current->print_color) {
    f_print_set_color(fp, color);
  }

  /*! \note
   *  Depending on the radius of the arc, the <B>space</B> parameter is
   *  changed into a small angle <B>da</B>.
   *  Starting from <B>angle1</B> - the start angle - the dots are printed
   *  along the arc by increments of this new angle.
   *
   *  As <B>da</B> is rounded as an integer, it can take a null value which
   *  will make the function enter an endless loop. In such a case, the arc
   *  is printed solid. The <B>da</B> variable should never be negative
   *  except if <B>space</B> is negative.
   */

  /* Inverting angle2 if < 0 and changing angle1 accordingly */
  /* the loop test assume that da > 0 */
  if (angle2 < 0) {
    angle1 = angle1 + angle2;
    angle2 = -angle2;
  }
  da = (int) ((space * 180) / (M_PI * ((double) radius)));

	/* If da or db too small for arc to be displayed as dotted,
           draw a solid arc */
  if (da <= 0) {
    o_arc_print_solid(w_current, fp,
                      x, y, radius,
                      angle1, angle2,
                      color,
                      arc_width, length, space, origin_x, origin_y);
    return;
  }

  fprintf(fp,"[");
  d = angle1;
  while (d < (angle2 + angle1)) {
    /*xa = ((double) x) + ((double) radius) * cos(d * M_PI / 180);
    ya = ((double) y) + ((double) radius) * sin(d * M_PI / 180);
    */
    fprintf(fp,"[%d] ",d);


#if 0 
    /*! \note
     *  A dot is represented by a filled circle.
     *  Position of the circle is (<B>xa</B>, <B>ya</B>) and its radius is the <B>arc_width</B> parameter.
     */
    /* PB : problem corrected : diameter of printed dots */
    /* 
      fprintf(fp, "newpath\n");
      fprintf(fp, "%d mils %d mils\n", (int) xa, (int) ya);
      if (arc_width <= 1) {
        fprintf(fp, "2 mils\n");
      } else {
        fprintf(fp, "%d mils\n", (int) arc_width/2);
      }
      fprintf(fp, "0 360 arc\n");
      fprintf(fp, "fill\n");
    */
    /* PB : end */		
#endif
    d = d + da;
  }
  fprintf(fp,"] %d %d %d %d dashedarc %% dotted\n",
	  x,y, radius, arc_width);
}

/*! \brief
 *  \par Function Description
 *  This function prints an arc when a dashed line type is required.
 *  The arc is defined by its center in <B>x</B> and <B>y</B>, its radius
 *  in <B>radius</B> and the start and end angles of the arc on the circle.
 *  The postscript file is defined by the file pointer <B>fp</B>.
 *  The parameter <B>arc_width</B> specifies the diameter of the dots of the printed line.
 *
 *  A negative value for <B>space</B> or <B>length</B> leads to an endless loop.
 *
 *  All dimensions are in mils, except <B>angle1</B> and <B>angle2</B> in degrees.
 *
 *  The function sets the color the line will be printed with.
 *
 *  \param [in] w_current  The TOPLEVEL object.
 *  \param [in] fp         FILE pointer to postscript document.
 *  \param [in] x
 *  \param [in] y
 *  \param [in] radius
 *  \param [in] angle1
 *  \param [in] angle2
 *  \param [in] color
 *  \param [in] arc_width
 *  \param [in] length
 *  \param [in] space
 *  \param [in] origin_x
 *  \param [in] origin_y
 */
void o_arc_print_dashed(TOPLEVEL *w_current, FILE *fp,
			int x, int y, int radius,
			int angle1, int angle2,
			int color,				   
			int arc_width, int length, int space,
			int origin_x, int origin_y)
{
  int da, db, a1, a2, d;

  if (w_current->print_color) {
    f_print_set_color(fp, color);
  }
  
  /*! \note
   *  Depending on the radius of the arc, the <B>space</B> (resp. <B>length</B>)
   *  parameter is changed into a small angle <B>da</B> (resp. <B>db</B>).
   *  Starting from <B>angle1</B> - the start angle - the dashes are printed
   *  along the arc by increments of these new angles.
   *
   *  As <B>da</B> (resp. <B>db</B>) is rounded as an integer, it can take a
   *  null value which will make the function enter an endless loop. In such a case,
   *  the arc is printed solid. The <B>da</B> (resp. <B>db</B>) variable should never
   *  be negative except if <B>space</B> (resp. <B>length</B>) is negative.
   *
   *  It prints as many dashes of length <B>length</B> as possible.
   */

  /* Inverting angle2 if < 0 and changing angle1 accordingly */
  /* the loop test assume that da > 0 */
  if (angle2 < 0) {
    angle1 = angle1 + angle2;
    angle2 = -angle2;
  }
  da = (int) ((length * 180) / (M_PI * ((double) radius)));
  db = (int) ((space  * 180) / (M_PI * ((double) radius)));

  /* If da or db too small for arc to be displayed as dotted,
           draw a solid arc */
  if ((da <= 0) || (db <= 0)) {
    o_arc_print_solid(w_current, fp,
                      x, y, radius, 
                      angle1, angle2,
                      color,
                      arc_width, length, space, origin_x, origin_y);
    return;
  }
  
  fprintf(fp,"[");
  d = angle1;
  while ((d + da + db) < (angle1 + angle2)) {
    a1 = d;
    d = d + da;

    fprintf(fp,"[%d %d] ",
	    a1, a1+da);

    d = d + db;
  }
  /*! \note
   *  When the above condition is no more satisfied, then it is not
   *  possible to print a dash of length <B>length</B> and the following <B>space</B>.
   *  However it may be possible to print the complete dash or a shorter one.
   */

  if ((d + da) < (angle1 + angle2)) {
    a1 = d;
    a2 = da;
  } else {
    a1 = d;
    a2 = (angle1 + angle2) - d;
  }

  fprintf(fp,"[%d %d] ",
	  a1, a1+da);


  fprintf(fp,"] %d %d %d %d dashedarc %% dashed\n",
	  x,y, radius, arc_width);

}

/*! \brief
 *  \par Function Description
 *  This function prints an arc when a centered line type is required.
 *  The arc is defined by its center in <B>x</B> and <B>y</B>, its radius in
 *  <B>radius</B> and the start and end angles of the arc on the circle.
 *  The postscript file is defined by the file pointer <B>fp</B>.
 *  The parameter <B>arc_width</B> specifies the diameter of the dots and the width of the dashes of the printed line.
 *
 *  A negative value for <B>space</B> or <B>length</B> leads to an endless loop.
 *
 *  All dimensions are in mils, except <B>angle1</B> and <B>angle2</B> in degrees.
 *
 *  The function sets the color in which the line will be printed with.
 *
 *  \param [in] w_current  The TOPLEVEL object.
 *  \param [in] fp         FILE pointer to postscript document.
 *  \param [in] x
 *  \param [in] y
 *  \param [in] radius
 *  \param [in] angle1
 *  \param [in] angle2
 *  \param [in] color
 *  \param [in] arc_width
 *  \param [in] length
 *  \param [in] space
 *  \param [in] origin_x
 *  \param [in] origin_y
 */
void o_arc_print_center(TOPLEVEL *w_current, FILE *fp,
			int x, int y, int radius, 
			int angle1, int angle2,
			int color,				   
			int arc_width, int length, int space,
			int origin_x, int origin_y)
{
  int da, db, a1, a2, d;

  if (w_current->print_color) {
    f_print_set_color(fp, color);
  }

  /*! \note
   *  Depending on the radius of the arc, the <B>space</B> (resp. <B>length</B>)
   *  parameter is changed into a small angle <B>da</B> (resp. <B>db</B>).
   *  Starting from <B>angle1</B> - the start angle - the dashes are printed
   *  along the arc by increments of these new angles.
   *
   *  As <B>da</B> (resp. <B>db</B>) is rounded as an integer, it can take a null
   *  value which will make the function enter an endless loop. In such a case,
   *  the arc is printed solid. The <B>da</B> (resp. <B>db</B>) variable should never
   *  be negative except if <B>space</B> (resp. <B>length</B>) is negative.
   *
   *  It prints as many sets of dash-dot as possible.
   */

  /* Inverting angle2 if < 0 and changing angle1 accordingly */
  /* the loop test assume that da > 0 */
  if (angle2 < 0) {
    angle1 = angle1 + angle2;
    angle2 = -angle2;
  }

  da = (int) ((length * 180) / (M_PI * ((double) radius)));
  db = (int) ((space  * 180) / (M_PI * ((double) radius)));

  /* If da or db too small to be displayed, draw an arc */
  if ((da <= 0) || (db <= 0)) {
    o_arc_print_solid(w_current, fp,
		      x, y, radius,
		      angle1, angle2,
		      color,
		      arc_width, length, space, origin_x, origin_y);
    return;
  }
  
  fprintf(fp, "[");
  d = angle1;
  while ((d + da + 2 * db) < (angle1 + angle2)) {
    a1 = d;
    d = d + da;
    fprintf(fp,"[%d %d] ",(int) a1, (int) a1 + da);
    
    d = d + db;
    /*
      xa = ((double) x) + ((double) radius) * cos(d * (M_PI / 180));
      ya = ((double) y) + ((double) radius) * sin(d * (M_PI / 180));
    */
    fprintf(fp,"[%d] ",d);
    d = d + db;
  }
  /*! \note
   *  When the above condition is no more satisfied, then it is not
   *  possible to print a dash of length <B>length</B>. However two cases are possible :
   *  <DL>
   *      <DT>*</DT><DD>it is possible to print the dash and the dot
   *      <DT>*</DT><DD>it is possible to print the dash or a part of the original dash
   *  </DL>
   */
  
  if ((d + da) < (angle1 + angle2)) {
    a1 = d;
    a2 = da;
    
    d = d + da;
  } else {
    a1 = d;
    a2 = (angle1 + angle2) - d;
    
    d = d + da;
  }
  
  fprintf(fp,"[%d %d] ",(int) a1, (int) a1 + da);

	
  if ((d + db) < (angle1 + angle2)) {
    /*
      xa = ((double) x) + ((double) radius) * cos(d * (M_PI / 180));
      ya = ((double) y) + ((double) radius) * sin(d * (M_PI / 180));
    */
    fprintf(fp,"[%d] ",d);
    
  }
  
  fprintf(fp,"] %d %d %d %d dashedarc %% center\n",
	  x,y, radius, arc_width);
}

/*! \note
 *  A dot is represented by a filled circle. Position of the circle is (<B>xa</B>, <B>ya</B>)
 *  and its radius is the <B>arc_width</B> parameter.
 */

/*! \brief
 *  \par Function Description
 *  This function prints an arc when a phantom line type is required.
 *  The arc is defined by its center in <B>x</B> and <B>y</B>, its radius
 *  in <B>radius</B> and the start and end angles of the arc on the circle.
 *  The postscript file is defined by the file pointer <B>fp</B>.
 *  The parameter <B>arc_width</B> specifies the diameter of the dots and the width of the dashes of the printed line.
 *
 *  A negative value for <B>space</B> or <B>length</B> leads to an endless loop.
 *
 *  All dimensions are in mils, except <B>angle1</B> and <B>angle2</B> in degrees.
 *
 * The function sets the color in which the line will be printed with.
 *
 *  \param [in] w_current  The TOPLEVEL object.
 *  \param [in] fp         FILE pointer to postscript document.
 *  \param [in] x
 *  \param [in] y
 *  \param [in] radius
 *  \param [in] angle1
 *  \param [in] angle2
 *  \param [in] color
 *  \param [in] arc_width
 *  \param [in] length
 *  \param [in] space
 *  \param [in] origin_x
 *  \param [in] origin_y
 */
void o_arc_print_phantom(TOPLEVEL *w_current, FILE *fp,
			 int x, int y, int radius,
			 int angle1, int angle2,
			 int color,
			 int arc_width, int length, int space,
			 int origin_x, int origin_y)
{
  int da, db, a1, a2, d;

  if (w_current->print_color) {
    f_print_set_color(fp, color);
  }

  /*! \note
   *  Depending on the radius of the arc, the <B>space</B> (resp. <B>length</B>)
   *  parameter is changed into a small angle <B>da</B> (resp. <B>db</B>).
   *  Starting from <B>angle1</B> - the start angle - the dashes are printed
   *  along the arc by increments of these new angles.
   *
   *  As <B>da</B> (resp. <B>db</B>) is rounded as an integer, it can take a
   *  null value which will make the function enter an endless loop. In such
   *  a case, the arc is printed solid. The <B>da</B> (resp. <B>db</B>) variable
   *  should never be negative except if <B>space</B> (resp. <B>length</B>) is negative.
   *
   *  It prints as many sets of dash-dot-dot as possible.
   */

  /* Inverting angle2 if < 0 and changing angle1 accordingly */
  /* the loop test assume that da > 0 */
  if (angle2 < 0) {
    angle1 = angle1 + angle2;
    angle2 = -angle2;
  }
  da = (int) ((length * 180) / (((double) radius) * M_PI));
  db = (int) ((space  * 180) / (((double) radius) * M_PI));
  
  /* If da or db too small for arc to be displayed as dotted,
     draw a solid arc */
  if ((da <= 0) || (db <= 0)) {
    o_arc_print_solid(w_current, fp,
		      x, y, radius,
		      angle1, angle2,
		      color,						  
		      arc_width, length, space, origin_x, origin_y);
    return;
  }
  
  fprintf(fp,"[");
  
  d = angle1;
  while ((d + da + 3 * db) < (angle1 + angle2)) {
    a1 = d;
    d = d + da;
    
    fprintf(fp,"[%d %d] ",(int) a1, (int) a1 + da);
    
    d = d + db;
    /*
      xa = ((double) x) + ((double) radius) * cos(d * (M_PI / 180));
      ya = ((double) y) + ((double) radius) * sin(d * (M_PI / 180));
    */
    fprintf(fp,"[%d] ",d);
    
    d = d + db;
    
    /*
      xa = ((double) x) + ((double) radius) * cos(d * (M_PI / 180));
      ya = ((double) y) + ((double) radius) * sin(d * (M_PI / 180));
    */
    fprintf(fp,"[%d] ",d);
    
    d = d + db;
  }

  /*! \note
   *  When the above condition is no more satisfied, then it is not
   *  possible to print a dash of length <B>length</B>.
   *  However three cases are possible :
   *  <DL>
   *    <DT>*</DT><DD>it is possible to print a dash and a dot and a dot
   *    <DT>*</DT><DD>it is possible to print a dash and a dot
   *    <DT>*</DT><DD>it is possible to print the dash or a part of the original dash
   *  </DL>
   */

  if ((d + da) < (angle1 + angle2)) {
    a1 = d;
    a2 = da;
    d = d + da;
  } else {
    a1 = d;
    a2 = (angle1 + angle2) - d;
    d = d + da;
  }
  
  fprintf(fp,"[%d %d] ",(int) a1, (int) a1 + da);
  
  if ((d + db) < (angle1 + angle2)) {
    d = d + db;
    
    /*
      xa = ((double) x) + ((double) radius) * cos(d * (M_PI / 180));
      ya = ((double) y) + ((double) radius) * sin(d * (M_PI / 180));
    */
    fprintf(fp,"[%d] ",d);
    
  }
  
  if ((d + db) < (angle1 + angle2)) {
    d = d + db;
    
    /*
      xa = ((double) x) + ((double) radius) * cos(d * (M_PI / 180));
      ya = ((double) y) + ((double) radius) * sin(d * (M_PI / 180));
    */
    
    fprintf(fp,"[%d] ",d);
    
    
  }
  
  fprintf(fp,"] %d %d %d %d dashedarc %% phantom\n",
	  x,y, radius, arc_width);
}


#if 0 /* original way of printing arcs, no longer used */

/*! \deprecated
 *  \brief Print solid arc in a Postscript file.
 *  \par Function Description
 *  This function prints a solid arc in a Postscript file indepedentely of its line type.
 *  It is basically the old way to manage the arc printing.
 *  This function is now replaced by the #o_arc_print() function.
 *
 *  \param [in] w_current  The TOPLEVEL object.
 *  \param [in] fp         FILE pointer to postscript document.
 *  \param [in] o_current
 *  \param [in] origin_x
 *  \param [in] origin_y
 */
void o_arc_print_old(TOPLEVEL *w_current, FILE *fp, OBJECT *o_current, 
		     int origin_x, int origin_y)
{
  int radius;
  int start_angle, end_angle;
  int awidth, aheight;
  int x, y;

  if (o_current == NULL) {
    printf("got null in o_arc_print\n");
    return;
  }

  fprintf(fp, "gsave\n");
  if (w_current->print_color) {
    f_print_set_color(fp, o_current->color);
  }

  f_print_set_line_width(fp, o_current->line_width);

  awidth = o_current->arc->width;
  aheight = o_current->arc->height;

  radius = abs(aheight - o_current->arc->y)/2;

  /* hack hack hack */
  /* the snap_grid here is a safety for arcs inside complex objects */
  /* which are not snapped to the grid */
  /* ALL arcs centers will be snapped to the center */
  /* hack hack hack */
  /* Geee I wish there was a better solution */
  /* well for now, if you print the complex structure that's in memory */
  /* then the arc will be properly snapped */
  /*x = snap_grid(w_current, o_current->x+radius);
    y = snap_grid(w_current, o_current->y-radius);*/

  x = (o_current->arc->x+radius);
  y = (o_current->arc->y-radius);

  start_angle = o_current->arc->start_angle/64;
  end_angle = o_current->arc->end_angle/64;

  if ( end_angle < 0) {
		
    if (end_angle >= 180) {
      start_angle = (start_angle - (end_angle)) % 360;
    } else {
      start_angle = (start_angle + (end_angle)) % 360;
    }

    end_angle = abs(end_angle);
		
  }

  end_angle = start_angle + end_angle;
		

  fprintf(fp, "newpath\n");
  fprintf(fp, "%d %d\n", x-origin_x, y-origin_y);
  fprintf(fp, "%d\n", radius);
  fprintf(fp, "%d %d arc\n", start_angle, end_angle);
  fprintf(fp, "stroke\n");
  fprintf(fp, "grestore\n");
}
#endif

/*! \brief Draw an arc in an image.
 *  \par Function Description
 *  This function draws an arc in an image with the libgdgeda function <B>gdImageArc()</B>.
 *
 *  \param [in] w_current  The TOPLEVEL object.
 *  \param [in] o_current
 *  \param [in] origin_x
 *  \param [in] origin_y
 *  \param [in] color_mode
 */
void
o_arc_image_write(TOPLEVEL *w_current, OBJECT *o_current,
                  int origin_x, int origin_y, int color_mode)
{
  int start_angle, end_angle;
  int width, height;
  int color;
  int x, y;

  if (o_current == NULL) {
    printf("got null in o_arc_image_write\n");
    return;
  }

  if (color_mode == TRUE) {
    color = o_image_geda2gd_color(o_current->color);
  } else {
    color = image_black;
  }

  start_angle = o_current->arc->start_angle;
  end_angle   = o_current->arc->end_angle;

  if ( end_angle < 0) {

    if (end_angle >= 180) {
      start_angle = (start_angle - (end_angle)) % 360;
    } else {
      start_angle = (start_angle + (end_angle)) % 360;
    }

    end_angle = abs(end_angle);

  }

  end_angle = start_angle + end_angle;



#if DEBUG
  printf("%d %d -- %d %d -- %d %d\n", 
         o_current->arc->screen_x, o_current->arc->screen_y,
         o_current->arc->screen_width-o_current->arc->screen_x,
         o_current->arc->screen_height-o_current->arc->screen_y,
         start_angle, end_angle);
#endif

  if (start_angle < end_angle) {

    start_angle = start_angle + 360;
  }

#if DEBUG
  printf("%d %d -- %d %d -- %d %d\n", 
         o_current->arc->screen_x, o_current->arc->screen_y,
         o_current->arc->screen_width-o_current->arc->screen_x,
         o_current->arc->screen_height-o_current->arc->screen_y,
         start_angle, end_angle);
#endif


  width  = o_current->arc->screen_width;
  height = o_current->arc->screen_height;

  x = o_current->arc->screen_x;
  y = o_current->arc->screen_y;
	
#ifdef HAS_LIBGDGEDA

  gdImageSetThickness(current_im_ptr, SCREENabs(w_current,
                                                o_current->line_width));

  gdImageArc(current_im_ptr, 
             x, y, width, height, start_angle, end_angle, color);
#endif
	
}
