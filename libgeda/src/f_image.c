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

#include "../include/prototype.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

#ifdef HAS_LIBGDGEDA
/*! \todo Finish function description!!!
 *  \brief
 *  \par Function Description
 *
 *  \param [in,out] w_current  The TOPLEVEL object to write to.
 *  \param [in,out]      head  The object to read image from.
 *  \param [in]       start_x  X Offset to start from.
 *  \param [in]       start_y  Y Offset to start from.
 *  \param [in]    color_mode
 *
 *  \todo what happens if snap is off? hack deal with this !!!!!!!!
 */
void f_image_write_objects(TOPLEVEL *w_current, OBJECT *head, 
			   int start_x, int start_y,
			   float scale, int color_mode)
{
  OBJECT *o_current=NULL;
  int origin_x, origin_y;
	
  if (head == NULL) {
    return;
  }

  origin_x = 0;
  origin_y = 0;

  o_current = head;


  while ( o_current != NULL ) {

    if (o_current->type != OBJ_HEAD) {

      switch (o_current->type) {
        case(OBJ_LINE):
          o_line_image_write(w_current, o_current,
                             origin_x, origin_y, color_mode);
          break;

        case(OBJ_PIN):
          o_pin_image_write(w_current, o_current,
                            origin_x, origin_y, color_mode);
          break;

        case(OBJ_COMPLEX):
        case(OBJ_PLACEHOLDER):

          f_image_write_objects(w_current, 
                                o_current->complex->prim_objs,
                                origin_x, origin_y, scale, color_mode);
          break;

        case(OBJ_TEXT):
          if (o_current->visibility == VISIBLE) {
			
            /*if (w_current->text_output == VECTOR_FONTS) {	*/
            f_image_write_objects(w_current, 
                                  o_current->text->
                                  prim_objs,
                                  origin_x, origin_y, scale, color_mode);
            /*} else {*/
#if 0
            o_text_image_write(w_current, fp, 
                               o_current,
                               origin_x, origin_y);

            /*}*/
#endif

          }
          break;

        case(OBJ_NET):
          o_net_image_write(w_current, o_current,
                            origin_x, origin_y, color_mode);

          break;

        case(OBJ_BUS):
          o_bus_image_write(w_current, o_current,
                            origin_x, origin_y, color_mode);
          break;

        case(OBJ_CIRCLE):
          o_circle_image_write(w_current, 
                               o_current,
                               origin_x, origin_y, color_mode);
          break;

        case(OBJ_ARC):
          o_arc_image_write(w_current, o_current,
                            origin_x, origin_y, color_mode);
          break;

        case(OBJ_BOX):
          o_box_image_write(w_current, o_current,
                            origin_x, origin_y, color_mode);
          break;
			
	case(OBJ_PICTURE):
          /*! \todo FIXME: Implement this */
	  fprintf(stderr, "f_image_write_objects: o_picture_image_write not implemented yet\n");
	  /* out = NULL; */
	  /* out = (char *) o_picture_image_write(w_current, o_current,
	     origin_x, origin_y, color_mode); */
	  break;

        default:
          fprintf(stderr, "Error type!\n");
          exit(-1);
          break;
      }

    } 
    o_current = o_current->next;
  }

  s_cue_output_all(w_current, head, NULL, PNG);

  return;
}
#endif

/*! \todo Finish function description!!!
 *  \brief Write image to file from w_current
 *  \par Function Description
 *
 *  \param [in,out] w_current  The TOPLEVEL object to read image from.
 *  \param [in]      filename  The name of the file to write image to.
 *  \param [in]         width  The width of the image to write
 *  \param [in]        height  The height of the image to write
 *  \param [in]    color_mode  
 */
void f_image_write(TOPLEVEL *w_current, const char *filename,
		   int width, int height, 
		   int color_mode)
{

#ifdef HAS_LIBGDGEDA
  int origin_x, origin_y, bottom, right;
  float scale=0.0;

  /* dots are breaking my filename selection hack hack !!!! */
	

  /*	printf("%d %d\n", w_current->paper_width, w_current->paper_height);*/

  world_get_complex_bounds(w_current, 
                           w_current->page_current->object_head, 
                           &origin_x, &origin_y, 
                           &right, &bottom);

  o_image_create(width, height, color_mode);

  f_image_write_objects(w_current,
			w_current->page_current->object_head,
			w_current->page_current->left,
			w_current->page_current->top, scale, color_mode);
	
  o_image_write(filename);
  o_image_close();
#else
  fprintf(stderr, "f_image_write: Called this function without libgdgeda support.\n");
  s_log_message("f_image_write: Called this function without libgdgeda support.\n");
#endif

}

/*! \brief Sets the image output type
 *  \par Function Description
 *  This function will set the image output type in the TOPLEVEL object.
 *
 *  \param [in,out] w_current  The TOPLEVEL object to set image type in.
 *  \param [in]          type  The image type to set
 */
void f_image_set_type(TOPLEVEL *w_current, int type)
{
  w_current->image_output_type = type;
}
