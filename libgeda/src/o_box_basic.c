
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


/* DO NOT read or edit this file ! Use ../noweb/o_box_basic.nw instead */

#include <config.h>
#include <math.h>
#include <stdio.h>

#include <gtk/gtk.h>
#include <gdk/gdk.h>
#include <gdk/gdkx.h>

#include <guile/gh.h>

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

/* Kazu on July 16, 1999 - Added these macros to simplify the code */
#define GET_BOX_WIDTH(w)                        \
        abs((w)->last_x - (w)->start_x)
#define GET_BOX_HEIGHT(w)                       \
                abs((w)->last_y - (w)->start_y)
#define GET_BOX_LEFT(w)                         \
                min((w)->start_x, (w)->last_x);
#define GET_BOX_TOP(w)                          \
                min((w)->start_y, (w)->last_y);

#define VERSION_20000704 20000704



void
get_box_bounds (TOPLEVEL * w_current, BOX * box, int *left, int *top, int *right, int
		*bottom)
{
  *left = box->screen_upper_x;
  *top = box->screen_upper_y;
  *right = box->screen_lower_x;
  *bottom = box->screen_lower_y;

/* PB : bounding box has to take into account the width of the line it is
   composed with, ie adding/substracting half the width to this box */
/* PB : but width is unknown here */

  *left = *left - 4;
  *top = *top - 4;

  *right = *right + 4;
  *bottom = *bottom + 4;
}

void
world_get_box_bounds (TOPLEVEL * w_current, BOX * box, int *left, int *top, int *right, int *bottom)
{
  *left = min (box->upper_x, box->lower_x);
  *top = min (box->upper_y, box->lower_y);
  *right = max (box->upper_x, box->lower_x);
  *bottom = max (box->upper_y, box->lower_y);

/* PB : same as above here */


#if DEBUG
  printf ("box: %d %d %d %d\n", *left, *top, *right, *bottom);
#endif


}


OBJECT *
o_box_add (TOPLEVEL * w_current, OBJECT * object_list,
	   char type, int color,
	   int x1, int y1, int x2, int y2)
{
  OBJECT *new_node;
  BOX *box;

  new_node = s_basic_init_object ("box");
  new_node->type = type;
  new_node->color = color;

  box = (BOX *) malloc (sizeof (BOX));

  box->upper_x = x1;
  box->upper_y = y1;
  box->lower_x = x2;
  box->lower_y = y2;

  new_node->box = box;

  /* Init */
  o_set_line_options (w_current, new_node,
		      END_NONE, TYPE_SOLID, 0, -1, -1);
  o_set_fill_options (w_current, new_node,
		      FILLING_HOLLOW, -1, -1, -1, -1, -1);

  o_box_recalc (w_current, new_node);

  /* TODO: questionable cast */
  new_node->draw_func = (void *) box_draw_func;
  /* TODO: questionable cast */
  new_node->sel_func = (void *) select_func;

  object_list = (OBJECT *) s_basic_link_object (new_node, object_list);
  return (object_list);
}


void
o_box_recalc (TOPLEVEL * w_current, OBJECT * o_current)
{
  int left, top, right, bottom;
  int screen_x1, screen_y1;
  int screen_x2, screen_y2;

  if (o_current->box == NULL)
    {
      return;
    }

  WORLDtoSCREEN (w_current, o_current->box->upper_x,
		 o_current->box->upper_y,
		 &screen_x1,
		 &screen_y1);

  o_current->box->screen_upper_x = screen_x1;
  o_current->box->screen_upper_y = screen_y1;

  WORLDtoSCREEN (w_current, o_current->box->lower_x,
		 o_current->box->lower_y,
		 &screen_x2,
		 &screen_y2);

  o_current->box->screen_lower_x = screen_x2;
  o_current->box->screen_lower_y = screen_y2;

  get_box_bounds (w_current, o_current->box, &left, &top, &right, &bottom);

  o_current->left = left;
  o_current->top = top;
  o_current->right = right;
  o_current->bottom = bottom;

  o_object_recalc (w_current, o_current);

}


OBJECT *
o_box_read (TOPLEVEL * w_current, OBJECT * object_list, char buf[], char *version)
{
  char type;
  int x1, y1;
  int width, height;
  int d_x1, d_y1;
  int d_x2, d_y2;
  int color;
  int box_width, box_space, box_length;
  int fill_width, angle1, pitch1, angle2, pitch2;
  OBJECT_END box_end;
  OBJECT_TYPE box_type;
  OBJECT_FILLING box_filling;
  long int ver;

  ver = strtol (version, NULL, 10);
  if (ver <= VERSION_20000704)
    {
      sscanf (buf, "%c %d %d %d %d %d\n",
	      &type, &x1, &y1, &width, &height, &color);

      box_width = 0;
      box_end = END_NONE;
      box_type = TYPE_SOLID;
      box_length = -1;
      box_space = -1;

      box_filling = FILLING_HOLLOW;
      fill_width = 0;
      angle1 = -1;
      pitch1 = -1;
      angle2 = -1;
      pitch2 = -1;
    }
  else
    {
      sscanf (buf, "%c %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d\n",
	      &type, &x1, &y1, &width, &height, &color,
	      &box_width, &box_end, &box_type, &box_length,
	      &box_space, &box_filling,
	      &fill_width, &angle1, &pitch1, &angle2, &pitch2);
    }

  d_x1 = x1;
  d_y1 = y1 + height;		/* move box origin to top left */

  d_x2 = x1 + width;		/* end points of the box */
  d_y2 = y1;

  if (width == 0 || height == 0)
    {
      fprintf (stderr, "Found a zero width/height box [ %c %d %d %d %d %d ]\n",
	       type, x1, y1, width, height, color);
      s_log_message ("Found a zero width/height box [ %c %d %d %d %d %d ]\n",
		     type, x1, y1, width, height, color);
    }

  if (color < 0 || color > MAX_COLORS)
    {
      fprintf (stderr, "Found an invalid color [ %s ]\n", buf);
      s_log_message ("Found an invalid color [ %s ]\n", buf);
      s_log_message ("Setting color to WHITE\n");
      color = WHITE;
    }

  object_list = (OBJECT *) o_box_add (w_current, object_list,
				      type, color,
				      d_x1, d_y1, d_x2, d_y2);
  o_set_line_options (w_current, object_list,
		      box_end, box_type, box_width,
		      box_length, box_space);
  o_set_fill_options (w_current, object_list,
		      box_filling, fill_width,
		      pitch1, angle1, pitch2, angle2);

  return (object_list);
}

char *
o_box_save (char *buf, OBJECT * object)
{
  int x1, y1;
  int width, height;
  int color;
  int box_width, box_space, box_length;
  int fill_width, angle1, pitch1, angle2, pitch2;
  OBJECT_END box_end;
  OBJECT_TYPE box_type;
  OBJECT_FILLING box_fill;

  width = abs (object->box->lower_x - object->box->upper_x);
  height = abs (object->box->upper_y - object->box->lower_y);

  x1 = object->box->upper_x;
  y1 = object->box->upper_y - height;	/* move the origin to 0, 0 */

#if DEBUG
  printf ("box: %d %d %d %d\n", x1, y1, d_x, d_y);
#endif

  /* Use the right color */
  if (object->saved_color == -1)
    {
      color = object->color;
    }
  else
    {
      color = object->saved_color;
    }

  box_end = object->line_end;
  box_width = object->line_width;
  box_type = object->line_type;
  box_length = object->line_length;
  box_space = object->line_space;

  box_fill = object->fill_type;
  fill_width = object->fill_width;
  angle1 = object->fill_angle1;
  pitch1 = object->fill_pitch1;
  angle2 = object->fill_angle2;
  pitch2 = object->fill_pitch2;

  sprintf (buf, "%c %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d",
	   object->type,
	   x1, y1, width, height, color,
	   box_width, box_end, box_type, box_length, box_space,
	   box_fill,
	   fill_width, angle1, pitch1, angle2, pitch2);

  return (buf);
}


void
o_box_translate (TOPLEVEL * w_current, int dx, int dy, OBJECT * object)
{
  int x, y;

  if (object == NULL)
    printf ("bt NO!\n");


  /* Do screen coords */
  object->box->screen_upper_x = object->box->screen_upper_x + dx;
  object->box->screen_upper_y = object->box->screen_upper_y + dy;
  object->box->screen_lower_x = object->box->screen_lower_x + dx;
  object->box->screen_lower_y = object->box->screen_lower_y + dy;

  /* printf("box: trans: %d %d\n", dx, dy); */

  SCREENtoWORLD (w_current, object->box->screen_upper_x,
		 object->box->screen_upper_y,
		 &x,
		 &y);

  object->box->upper_x = snap_grid (w_current, x);
  object->box->upper_y = snap_grid (w_current, y);

  SCREENtoWORLD (w_current, object->box->screen_lower_x,
		 object->box->screen_lower_y,
		 &x,
		 &y);

  object->box->lower_x = snap_grid (w_current, x);
  object->box->lower_y = snap_grid (w_current, y);
}

void
o_box_translate_world (TOPLEVEL * w_current, int x1, int y1, OBJECT * object)
{
  int screen_x1, screen_y1;
  int screen_x2, screen_y2;
  int left, right, top, bottom;

  if (object == NULL)
    printf ("btw NO!\n");


  /* Do world coords */
  object->box->upper_x = object->box->upper_x + x1;
  object->box->upper_y = object->box->upper_y + y1;
  object->box->lower_x = object->box->lower_x + x1;
  object->box->lower_y = object->box->lower_y + y1;

  WORLDtoSCREEN (w_current, object->box->upper_x,
		 object->box->upper_y,
		 &screen_x1,
		 &screen_y1);

  object->box->screen_upper_x = screen_x1;
  object->box->screen_upper_y = screen_y1;

  WORLDtoSCREEN (w_current, object->box->lower_x,
		 object->box->lower_y,
		 &screen_x2,
		 &screen_y2);

  object->box->screen_lower_x = screen_x2;
  object->box->screen_lower_y = screen_y2;

  get_box_bounds (w_current, object->box, &left, &top, &right, &bottom);

  object->left = left;
  object->top = top;
  object->right = right;
  object->bottom = bottom;
}


OBJECT *
o_box_copy (TOPLEVEL * w_current, OBJECT * list_tail, OBJECT * o_current)
{
  OBJECT *new_obj;
  ATTRIB *a_current;
  int color;

  if (o_current->saved_color == -1)
    {
      color = o_current->color;
    }
  else
    {
      color = o_current->saved_color;
    }

  new_obj = o_box_add (w_current, list_tail,
		       OBJ_BOX, color,
		       0, 0, 0, 0);

  new_obj->box->screen_upper_x = o_current->box->screen_upper_x;
  new_obj->box->screen_upper_y = o_current->box->screen_upper_y;
  new_obj->box->screen_lower_x = o_current->box->screen_lower_x;
  new_obj->box->screen_lower_y = o_current->box->screen_lower_y;

  new_obj->box->upper_x = o_current->box->upper_x;
  new_obj->box->upper_y = o_current->box->upper_y;
  new_obj->box->lower_x = o_current->box->lower_x;
  new_obj->box->lower_y = o_current->box->lower_y;


  o_set_line_options (w_current, new_obj, o_current->line_end,
		      o_current->line_type, o_current->line_width,
		      o_current->line_length, o_current->line_space);
  o_set_fill_options (w_current, new_obj,
		      o_current->fill_type, o_current->fill_width,
		      o_current->fill_pitch1, o_current->fill_angle1,
		      o_current->fill_pitch2, o_current->fill_angle2);

/*      new_obj->attribute = 0; */
  a_current = o_current->attribs;
  if (a_current)
    {
      while (a_current)
	{

	  /* head attrib node has prev = NULL */
	  if (a_current->prev != NULL)
	    {
	      a_current->copied_to = new_obj;
	    }
	  a_current = a_current->next;
	}
    }

  return (new_obj);
}


void
o_box_print (TOPLEVEL * w_current, FILE * fp, OBJECT * o_current,
	     int origin_x, int origin_y)
{
  int x, y, width, height;
  int color;
  int line_width, length, space;
  int fill_width, angle1, pitch1, angle2, pitch2;
  void (*outl_func) () = NULL;
  void (*fill_func) () = NULL;

  if (o_current == NULL)
    {
      printf ("got null in o_box_print\n");
      return;
    }

  x = o_current->box->upper_x;
  y = o_current->box->upper_y;
  width = abs (o_current->box->lower_x - o_current->box->upper_x);
  height = abs (o_current->box->lower_y - o_current->box->upper_y);
  color = o_current->color;

  line_width = o_current->line_width;
  length = o_current->line_length;
  space = o_current->line_space;

  switch (o_current->line_type)
    {
    case (TYPE_SOLID):
      length = -1;
      space = -1;
      outl_func = (void *) o_box_print_solid;
      break;

    case (TYPE_DOTTED):
      length = -1;
      outl_func = (void *) o_box_print_dotted;
      break;

    case (TYPE_DASHED):
      outl_func = (void *) o_box_print_dashed;
      break;

    case (TYPE_CENTER):
      outl_func = (void *) o_box_print_center;
      break;

    case (TYPE_PHANTOM):
      outl_func = (void *) o_box_print_phantom;
      break;

    case (TYPE_ERASE):
      /* Unused for now, print it solid */
      length = -1;
      space = -1;
      outl_func = (void *) o_box_print_solid;
      break;
    }

  if ((length == 0) || (space == 0))
    {
      length = -1;
      space = -1;
      outl_func = (void *) o_box_print_solid;
    }

  (*outl_func) (w_current, fp,
		x, y, width, height,
		color,
		line_width,
		length, space,
		origin_x, origin_y);

  if (o_current->fill_type != FILLING_HOLLOW)
    {
      fill_width = o_current->fill_width;
      angle1 = o_current->fill_angle1;
      pitch1 = o_current->fill_pitch1;
      angle2 = o_current->fill_angle2;
      pitch2 = o_current->fill_pitch2;

      switch (o_current->fill_type)
	{
	case (FILLING_FILL):
	  angle1 = -1;
	  pitch1 = 1;
	  angle2 = -1;
	  pitch2 = 1;
	  fill_width = -1;
	  fill_func = (void *) o_box_print_filled;
	  break;

	case (FILLING_MESH):
	  fill_func = (void *) o_box_print_mesh;
	  break;

	case (FILLING_HATCH):
	  angle2 = -1;
	  pitch2 = 1;
	  fill_func = (void *) o_box_print_hatch;
	  break;

	case (FILLING_VOID):
	  /* Unused for now, print it filled */
	  angle1 = -1;
	  pitch1 = 1;
	  angle2 = -1;
	  pitch2 = 1;
	  fill_width = -1;
	  fill_func = (void *) o_box_print_filled;
	  break;
	}

      if ((pitch1 <= 0) || (pitch2 <= 0))
	{
	  angle1 = -1;
	  pitch1 = 1;
	  angle2 = -1;
	  pitch2 = 1;
	  fill_func = (void *) o_box_print_filled;
	}

      (*fill_func) (w_current, fp,
		    x, y, width, height,
		    color,
		    fill_width,
		    angle1, pitch1, angle2, pitch2,
		    origin_x, origin_y);
    }


}				/* done */

/* PB : parameter filled removed */
void
o_box_print_solid (TOPLEVEL * w_current, FILE * fp,
		   int x, int y,
		   int width, int height,
		   int color,
		   int line_width, int length, int space,
		   int origin_x, int origin_y)
{
  int x1, y1;

  fprintf (fp, "gsave\n");
  if (w_current->print_color)
    {
      f_print_set_color (fp, color);
    }

  f_print_set_line_width (fp, line_width);

  x1 = x;
  y1 = y - height;		/* move the origin to 0, 0 */

  o_line_print_solid (w_current, fp,
		      x1, y1, x1 + width, y1,
		      color,
		      line_width, length, space,
		      origin_x, origin_y);
  o_line_print_solid (w_current, fp,
		      x1 + width, y1, x1 + width, y1 + height,
		      color,
		      line_width, length, space,
		      origin_x, origin_y);
  o_line_print_solid (w_current, fp,
		      x1 + width, y1 + height, x1, y1 + height,
		      color,
		      line_width, length, space,
		      origin_x, origin_y);
  o_line_print_solid (w_current, fp,
		      x1, y1 + height, x1, y1,
		      color,
		      line_width, length, space,
		      origin_x, origin_y);
  fprintf (fp, "grestore\n");

}
   /* done */
/* PB : parameter filled removed */
/* PB : parameter o_current removed */
void
o_box_print_dotted (TOPLEVEL * w_current, FILE * fp,
		    int x, int y,
		    int width, int height,
		    int color,
		    int line_width, int length, int space,
		    int origin_x, int origin_y)
{
  int x1, y1;

  fprintf (fp, "gsave\n");
  if (w_current->print_color)
    {
      f_print_set_color (fp, color);
    }

  f_print_set_line_width (fp, line_width);

  x1 = x;
  y1 = y - height;		/* move the origin to 0, 0 */

  o_line_print_dotted (w_current, fp,
		       x1, y1, x1 + width, y1,
		       color,
		       line_width, length, space,
		       origin_x, origin_y);
  o_line_print_dotted (w_current, fp,
		       x1 + width, y1, x1 + width, y1 + height,
		       color,
		       line_width, length, space,
		       origin_x, origin_y);
  o_line_print_dotted (w_current, fp,
		       x1 + width, y1 + height, x1, y1 + height,
		       color,
		       line_width, length, space,
		       origin_x, origin_y);
  o_line_print_dotted (w_current, fp,
		       x1, y1 + height, x1, y1,
		       color,
		       line_width, length, space,
		       origin_x, origin_y);
  fprintf (fp, "grestore\n");

}
  /* done */
/* PB : parameter filled removed */
/* PB : parameter o_current removed */
void
o_box_print_dashed (TOPLEVEL * w_current, FILE * fp,
		    int x, int y,
		    int width, int height,
		    int color,
		    int line_width, int length, int space,
		    int origin_x, int origin_y)
{
  int x1, y1;

  fprintf (fp, "gsave\n");
  if (w_current->print_color)
    {
      f_print_set_color (fp, color);
    }

  f_print_set_line_width (fp, line_width);

  x1 = x;
  y1 = y - height;		/* move the origin to 0, 0 */

  o_line_print_dashed (w_current, fp,
		       x1, y1, x1 + width, y1,
		       color,
		       line_width, length, space,
		       origin_x, origin_y);
  o_line_print_dashed (w_current, fp,
		       x1 + width, y1, x1 + width, y1 + height,
		       color,
		       line_width, length, space,
		       origin_x, origin_y);
  o_line_print_dashed (w_current, fp,
		       x1 + width, y1 + height, x1, y1 + height,
		       color,
		       line_width, length, space,
		       origin_x, origin_y);
  o_line_print_dashed (w_current, fp,
		       x1, y1 + height, x1, y1,
		       color,
		       line_width, length, space,
		       origin_x, origin_y);
  fprintf (fp, "grestore\n");

}
  /* done */
/* PB : parameter filled removed */
/* PB : parameter o_current removed */
void
o_box_print_center (TOPLEVEL * w_current, FILE * fp,
		    int x, int y,
		    int width, int height,
		    int color,
		    int line_width, int length, int space,
		    int origin_x, int origin_y)
{
  int x1, y1;

  fprintf (fp, "gsave\n");
  if (w_current->print_color)
    {
      f_print_set_color (fp, color);
    }

  f_print_set_line_width (fp, line_width);

  x1 = x;
  y1 = y - height;		/* move the origin to 0, 0 */

  o_line_print_center (w_current, fp,
		       x1, y1, x1 + width, y1,
		       color,
		       line_width, length, space,
		       origin_x, origin_y);
  o_line_print_center (w_current, fp,
		       x1 + width, y1, x1 + width, y1 + height,
		       color,
		       line_width, length, space,
		       origin_x, origin_y);
  o_line_print_center (w_current, fp,
		       x1 + width, y1 + height, x1, y1 + height,
		       color,
		       line_width, length, space,
		       origin_x, origin_y);
  o_line_print_center (w_current, fp,
		       x1, y1 + height, x1, y1,
		       color,
		       line_width, length, space,
		       origin_x, origin_y);
  fprintf (fp, "grestore\n");

}
  /* done */
/* PB : parameter filled removed */
/* PB : parameter o_current removed */
void
o_box_print_phantom (TOPLEVEL * w_current, FILE * fp,
		     int x, int y,
		     int width, int height,
		     int color,
		     int line_width, int length, int space,
		     int origin_x, int origin_y)
{
  int x1, y1;

  fprintf (fp, "gsave\n");
  if (w_current->print_color)
    {
      f_print_set_color (fp, color);
    }

  f_print_set_line_width (fp, line_width);

  x1 = x;
  y1 = y - height;		/* move the origin to 0, 0 */

  o_line_print_phantom (w_current, fp,
			x1, y1, x1 + width, y1,
			color,
			line_width, length, space,
			origin_x, origin_y);
  o_line_print_phantom (w_current, fp,
			x1 + width, y1, x1 + width, y1 + height,
			color,
			line_width, length, space,
			origin_x, origin_y);
  o_line_print_phantom (w_current, fp,
			x1 + width, y1 + height, x1, y1 + height,
			color,
			line_width, length, space,
			origin_x, origin_y);
  o_line_print_phantom (w_current, fp,
			x1, y1 + height, x1, y1,
			color,
			line_width, length, space,
			origin_x, origin_y);
  fprintf (fp, "grestore\n");
}
 /* done */

void
o_box_print_filled (TOPLEVEL * w_current, FILE * fp,
		    int x, int y,
		    int width, int height,
		    int color,
		    int fill_width,
		    int angle1, int pitch1,
		    int angle2, int pitch2,
		    int origin_x, int origin_y)
{
  int x1, y1;

  fprintf (fp, "gsave\n");
  if (w_current->print_color)
    {
      f_print_set_color (fp, color);
    }

  f_print_set_line_width (fp, 1);

  x1 = x;
  y1 = y - height;		/* move the origin to 0, 0 */

  fprintf (fp, "newpath\n");
  fprintf (fp, "%d mils %d mils moveto\n", x1 - origin_x, y1 - origin_y);
  fprintf (fp, "%d mils %d mils fbox\n", width, height);
  fprintf (fp, "grestore\n");

}
  /* done */
void
o_box_print_mesh (TOPLEVEL * w_current, FILE * fp,
		  int x, int y,
		  int width, int height,
		  int color,
		  int fill_width,
		  int angle1, int pitch1,
		  int angle2, int pitch2,
		  int origin_x, int origin_y)
{
  o_box_print_hatch (w_current, fp,
		     x, y, width, height,
		     color,
		     fill_width,
		     angle1, pitch1, -1, -1,
		     origin_x, origin_y);
  o_box_print_hatch (w_current, fp,
		     x, y, width, height,
		     color,
		     fill_width,
		     angle2, pitch2, -1, -1,
		     origin_x, origin_y);

}
    /* done */
void
o_box_print_hatch (TOPLEVEL * w_current, FILE * fp,
		   int x, int y,
		   int width, int height,
		   int color,
		   int fill_width,
		   int angle1, int pitch1,
		   int angle2, int pitch2,
		   int origin_x, int origin_y)
{
  int x3, y3, x4, y4;
  double cos_a_, sin_a_;
  double x0, y0, r;
  double x1, y1, x2, y2;
  double amin, amax, a[4], min1, min2, max1, max2;

  fprintf (fp, "gsave\n");
  if (w_current->print_color)
    {
      f_print_set_color (fp, color);
    }

  f_print_set_line_width (fp, fill_width);

  cos_a_ = cos (((double) angle1) * M_PI / 180);
  sin_a_ = sin (((double) angle1) * M_PI / 180);

  r = sqrt ((double) (pow (width, 2) + pow (height, 2))) / 2;

  y0 = 0;
  while (y0 < r)
    {
      x0 = pow (r, 2) - pow (y0, 2);
      x0 = sqrt (x0);

      x1 = (x0 * cos_a_ - y0 * sin_a_);
      y1 = (x0 * sin_a_ + y0 * cos_a_);
      x2 = ((-x0) * cos_a_ - y0 * sin_a_);
      y2 = ((-x0) * sin_a_ + y0 * cos_a_);

      if ((int) (x2 - x1) != 0)
	{
	  a[0] = ((-width / 2) - x1) / (x2 - x1);
	  a[1] = ((width / 2) - x1) / (x2 - x1);
	}
      else
	{
	  a[0] = 0;
	  a[1] = 1;
	}

      if ((int) (y2 - y1) != 0)
	{
	  a[2] = ((-height / 2) - y1) / (y2 - y1);
	  a[3] = ((height / 2) - y1) / (y2 - y1);
	}
      else
	{
	  a[2] = 0;
	  a[3] = 1;
	}

      if (a[0] < a[1])
	{
	  min1 = a[0];
	  max1 = a[1];
	}
      else
	{
	  min1 = a[1];
	  max1 = a[0];
	}

      if (a[2] < a[3])
	{
	  min2 = a[2];
	  max2 = a[3];
	}
      else
	{
	  min2 = a[3];
	  max2 = a[2];
	}

      amin = (min1 < min2) ? min2 : min1;
      amin = (amin < 0) ? 0 : amin;

      amax = (max1 < max2) ? max1 : max2;
      amax = (amax < 1) ? amax : 1;

      if ((amax > amin) && (amax != 1) && (amin != 0))
	{
	  /* There is intersection between the line and the box edges */
	  x3 = (int) (x1 + amin * (x2 - x1));
	  y3 = (int) (y1 + amin * (y2 - y1));

	  x4 = (int) (x1 + amax * (x2 - x1));
	  y4 = (int) (y1 + amax * (y2 - y1));

	  fprintf (fp, "newpath\n");
	  fprintf (fp, "%d mils %d mils moveto\n",
		   x3 + (x + width / 2), y3 + (y - height / 2));
	  fprintf (fp, "%d mils %d mils lineto\n",
		   x4 + (x + width / 2), y4 + (y - height / 2));

	  fprintf (fp, "stroke\n");

	  fprintf (fp, "newpath\n");
	  fprintf (fp, "%d mils %d mils moveto\n",
		   -x3 + (x + width / 2), -y3 + (y - height / 2));
	  fprintf (fp, "%d mils %d mils lineto\n",
		   -x4 + (x + width / 2), -y4 + (y - height / 2));
	  fprintf (fp, "stroke\n");

	}
      else
	{
	  break;
	}

      y0 = y0 + pitch1;
    }


  fprintf (fp, "grestore\n");

}				/* done */

#if 0				/* original way of printing box, no longer used */
void
o_box_print_old (TOPLEVEL * w_current, FILE * fp,
		 int origin_x, int origin_y)
{
  int width, height;
  int x1, y1;
  if (o_current == NULL)
    {
      printf ("got null in o_box_print\n");
      return;
    }

  if (w_current->print_color)
    {
      f_print_set_color (fp, o_current->color);
    }


  width = abs (o_current->line_points->x2 - o_current->line_points->x1);
  height = abs (o_current->line_points->y1 - o_current->line_points->y2);

  x1 = o_current->line_points->x1;
  y1 = o_current->line_points->y1 - height;	/* move the origin to 0, 0 */

  fprintf (fp, "newpath\n");
  fprintf (fp, "%d mils %d mils moveto\n", x1 - origin_x, y1 - origin_y);
  fprintf (fp, "%d mils %d mils box\n", width, height);

}

#endif

void
o_box_image_write (TOPLEVEL * w_current, OBJECT * o_current,
		   int origin_x, int origin_y, int color_mode)
{
  int color;


  if (o_current == NULL)
    {
      printf ("got null in o_box_image_write\n");
      return;
    }


  if (color_mode == TRUE)
    {
      color = o_image_geda2gd_color (o_current->color);
    }
  else
    {
      color = image_black;
    }

  /* assumes screen coords are already calculated correctly */
#ifdef HAS_LIBGDGEDA
  gdImageRectangle (current_im_ptr,
		    o_current->box->screen_upper_x,
		    o_current->box->screen_upper_y,
		    o_current->box->screen_lower_x,
		    o_current->box->screen_lower_y,
		    color);
#endif

}


/* takes in screen coordinates for the centerx,y, and then does the rotate 
 * in world space */
/* also ignores angle argument... for now, rotate only in 90 degree 
 * increments */
/* fixed to 90 degrees... it's *not* general now */
void
o_box_rotate (TOPLEVEL * w_current, int centerx, int centery, int angle,
	      OBJECT * object)
{
  int world_centerx, world_centery;
  int newx1, newy1;
  int newx2, newy2;
  int width, height;

  SCREENtoWORLD (w_current, centerx, centery,
		 &world_centerx,
		 &world_centery);

  width = abs (object->box->upper_x - object->box->lower_x);
  height = abs (object->box->upper_y - object->box->lower_y);

  /* translate object to origin */
  o_box_translate_world (w_current, -world_centerx, -world_centery, object);
  rotate_point_90 (object->box->upper_x, object->box->upper_y, angle,
		   &newx1, &newy1);

  rotate_point_90 (object->box->lower_x, object->box->lower_y, angle,
		   &newx2, &newy2);



  object->box->upper_x = min (newx1, newx2);
  object->box->upper_y = max (newy1, newy2);
  object->box->lower_x = max (newx1, newx2);
  object->box->lower_y = min (newy1, newy2);

  o_box_translate_world (w_current, world_centerx, world_centery, object);
}

void
o_box_rotate_world (TOPLEVEL * w_current,
		    int world_centerx, int world_centery, int angle,
		    OBJECT * object)
{
  int newx1, newy1;
  int newx2, newy2;
  int width, height;

  if (angle == 0)
    return;


  width = abs (object->box->upper_x - object->box->lower_x);
  height = abs (object->box->upper_y - object->box->lower_y);

  /* translate object to origin */
  o_box_translate_world (w_current, -world_centerx, -world_centery, object);
  rotate_point_90 (object->box->upper_x, object->box->upper_y, angle,
		   &newx1, &newy1);

  rotate_point_90 (object->box->lower_x, object->box->lower_y, angle,
		   &newx2, &newy2);

  object->box->upper_x = min (newx1, newx2);
  object->box->upper_y = max (newy1, newy2);
  object->box->lower_x = max (newx1, newx2);
  object->box->lower_y = min (newy1, newy2);

  o_box_translate_world (w_current, world_centerx, world_centery, object);
}


void
o_box_mirror (TOPLEVEL * w_current, int centerx, int centery, OBJECT * object)
{
  int world_centerx, world_centery;
  int newx1, newy1;
  int newx2, newy2;
  int width, height;

  SCREENtoWORLD (w_current, centerx, centery,
		 &world_centerx,
		 &world_centery);

  width = abs (object->box->upper_x - object->box->lower_x);
  height = abs (object->box->upper_y - object->box->lower_y);

  /* translate object to origin */
  o_box_translate_world (w_current, -world_centerx, -world_centery, object);

  newx1 = -object->box->upper_x;
  newy1 = object->box->upper_y;
  newx2 = -object->box->lower_x;
  newy2 = object->box->lower_y;

  object->box->upper_x = min (newx1, newx2);
  object->box->upper_y = max (newy1, newy2);
  object->box->lower_x = max (newx1, newx2);
  object->box->lower_y = min (newy1, newy2);

  o_box_translate_world (w_current, world_centerx, world_centery, object);
}

void
o_box_mirror_world (TOPLEVEL * w_current, int world_centerx, int world_centery, OBJECT * object)
{
  int newx1, newy1;
  int newx2, newy2;
  int width, height;

  width = abs (object->box->upper_x - object->box->lower_x);
  height = abs (object->box->upper_y - object->box->lower_y);

  /* translate object to origin */
  o_box_translate_world (w_current, -world_centerx, -world_centery, object);

  newx1 = -object->box->upper_x;
  newy1 = object->box->upper_y;
  newx2 = -object->box->lower_x;
  newy2 = object->box->lower_y;

  object->box->upper_x = min (newx1, newx2);
  object->box->upper_y = max (newy1, newy2);
  object->box->lower_x = max (newx1, newx2);
  object->box->lower_y = min (newy1, newy2);

  o_box_translate_world (w_current, world_centerx, world_centery, object);
}


void
o_box_modify (TOPLEVEL * w_current, OBJECT * object,
	      int x, int y, int whichone)
{
  int x1, y1, x2, y2;
  int left, right, top, bottom;
  int box_width, box_height, box_left, box_top;

  box_width = GET_BOX_WIDTH (w_current);
  box_height = GET_BOX_HEIGHT (w_current);
  box_left = GET_BOX_LEFT (w_current);
  box_top = GET_BOX_TOP (w_current);

  SCREENtoWORLD (w_current,
		 box_left,
		 box_top,
		 &x1,
		 &y1);
  SCREENtoWORLD (w_current,
		 box_left + box_width,
		 box_top + box_height,
		 &x2,
		 &y2);

  x1 = snap_grid (w_current, x1);
  y1 = snap_grid (w_current, y1);
  x2 = snap_grid (w_current, x2);
  y2 = snap_grid (w_current, y2);

  object->box->upper_x = x1;
  object->box->upper_y = y1;
  object->box->lower_x = x2;
  object->box->lower_y = y2;

  object->box->screen_upper_x = box_left;
  object->box->screen_upper_y = box_top;
  object->box->screen_lower_x = box_left + box_width;
  object->box->screen_lower_y = box_top + box_height;

  get_box_bounds (w_current, object->box, &left, &top, &right, &bottom);

  object->left = left;
  object->top = top;
  object->right = right;
  object->bottom = bottom;
}
