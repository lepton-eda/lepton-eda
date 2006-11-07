/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
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
#include <sys/stat.h>
#include <math.h>
#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <libgeda/libgeda.h>

#include "../include/globals.h"
#include "../include/prototype.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

#define WINONLY	1
#define BACKING 2

/* font storage and friends are staying global so that all can access */
#define NUM_CHARS 255

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_text_draw_lowlevel(TOPLEVEL *w_current, OBJECT *o_current)
{
  int left, right, top, bottom;

  g_return_if_fail (o_current != NULL);
  g_return_if_fail (o_current->text != NULL);

  if (o_current->visibility == INVISIBLE && w_current->show_hidden_text &&
      o_current->text->prim_objs == NULL) {
    o_text_recreate(w_current, o_current);
  }
  
  o_redraw(w_current, o_current->text->prim_objs, TRUE);

  get_complex_bounds(w_current, o_current->text->prim_objs,
                     &left, &top, &right, &bottom);
  o_current->left   = left;
  o_current->top    = top;
  o_current->right  = right;
  o_current->bottom = bottom;

  WORLDtoSCREEN(w_current,
                o_current->text->x,
                o_current->text->y,
                &o_current->text->screen_x,
                &o_current->text->screen_y);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_text_draw_rectangle(TOPLEVEL *w_current, OBJECT *o_current)
{
  int left=0, right=0, top=0, bottom=0;
  int screen_x1, screen_y1;
  int width, height, dx=0, dy=0;
  GdkColor *color;

  if (o_current->visibility == INVISIBLE && w_current->show_hidden_text &&
      o_current->text->prim_objs == NULL) {
    o_text_recreate(w_current, o_current);
  }
  
  o_text_recalc(w_current, o_current);

  /* text is too small so go through and draw a rectangle in
     it's place */
	
  screen_x1 = o_current->text->screen_x;
  screen_y1 = o_current->text->screen_y;

  if (w_current->override_color != -1 ) {  /* Override */
    color = x_get_color(w_current->override_color);
  } else {
    color = x_get_color(o_current->color);
  }
  gdk_gc_set_foreground(w_current->gc, color);


  width = SCREENabs(w_current, o_current->text->displayed_width);
  height = SCREENabs(w_current, o_current->text->displayed_height);
      
  switch(o_current->text->angle) {
    case(0):
      left = screen_x1+dx;
      top = screen_y1+dy-height;
      right = width;
      bottom = height;
      break;
	
    case(90):
      left = screen_x1+dx-height;
      top = screen_y1+dy-width;
      right = height;
      bottom = width;
      break;
	  
    case(180):
      left = screen_x1+dx-width;
      top = screen_y1+dy;
      right = width;
      bottom = height;
      break;
	
    case(270):
      left = screen_x1+dx;
      top = screen_y1+dy;
      right = height;
      bottom = width;
      break;

    default:
      s_log_message(_("Tried to render text with an invalid angle: %d\n"),
                    o_current->text->angle); 
      return;
      break;
  }

  /* The right, bottom variables are really just the width and height and */
  /* not the "right" or "bottom". */
  if (w_current->DONT_REDRAW == 0) {
    gdk_draw_rectangle(w_current->window,
		       w_current->gc,
		       FALSE,
		       left, 
		       top,
		       right,
		       bottom);
    gdk_draw_rectangle(w_current->backingstore,
		       w_current->gc,
		       FALSE,
		       left, 
		       top,
		       right,
		       bottom);
  }

#if 0 /* in prep for future performance enhancement */
  right = right+left;
  bottom = bottom+top;
  o_current->left   = min(left, right);
  o_current->top    = min(top, bottom);
  o_current->right  = max(left, right);
  o_current->bottom = max(top, bottom);
  
  printf("%d %d %d %d\n", left, top, right, bottom);
  
  WORLDtoSCREEN(w_current,
                o_current->text->x,
                o_current->text->y,
                &o_current->text->screen_x,
                &o_current->text->screen_y);
#endif

}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_text_draw(TOPLEVEL *w_current, OBJECT *o_current)
{
  int screen_x1, screen_y1;
  int small_dist, offset;

  g_return_if_fail (o_current != NULL);
  g_return_if_fail (o_current->type == OBJ_TEXT);
  g_return_if_fail (o_current->text != NULL);

  if (o_current->visibility == INVISIBLE && !w_current->show_hidden_text) {
    return;
  }

  if (!w_current->fast_mousepan || !w_current->doing_pan) {

#if 0 /* in prep for future performance enhancement... */
    int pixel_height;
    pixel_height = SCREENabs(w_current, 26*o_current->text->size/2);
    if (pixel_height < 4 /* && w_current->text_rectangle */)
    {
      o_text_draw_rectangle(w_current, o_current);
    } else {
      o_text_draw_lowlevel(w_current, o_current);
    }
#endif
    
    o_text_draw_lowlevel(w_current, o_current);

    /* Indicate on the schematic that the text is invisible by */
    /* drawing a little I on the screen at the origin */
    if (o_current->visibility == INVISIBLE && w_current->show_hidden_text) {
      if (w_current->override_color != -1 ) {
        gdk_gc_set_foreground(w_current->gc, 
                              x_get_color(w_current->override_color));
      } else {

        gdk_gc_set_foreground(w_current->gc, 
                              x_get_color(w_current->lock_color));
      }

      offset = SCREENabs(w_current, 10);
      small_dist = SCREENabs(w_current, 20);
      screen_x1 = o_current->text->screen_x + offset;
      screen_y1 = o_current->text->screen_y + offset;
      if (w_current->DONT_REDRAW == 0) {
	/* Top part of the I */
	gdk_draw_line(w_current->window, w_current->gc,
		      screen_x1,
		      screen_y1,
		      screen_x1+small_dist,
		      screen_y1);
	gdk_draw_line(w_current->backingstore, w_current->gc, 
		      screen_x1,
		      screen_y1,
		      screen_x1+small_dist,
		      screen_y1);
	/* Middle part of the I */
	gdk_draw_line(w_current->window, w_current->gc,
		      screen_x1+small_dist/2,
		      screen_y1,
		      screen_x1+small_dist/2,
		      screen_y1+small_dist);
	gdk_draw_line(w_current->backingstore, w_current->gc, 
		      screen_x1+small_dist/2,
		      screen_y1,
		      screen_x1+small_dist/2,
		      screen_y1+small_dist);
	/* Bottom part of the I */
	gdk_draw_line(w_current->window, w_current->gc,
		      screen_x1,
		      screen_y1+small_dist,
		      screen_x1+small_dist,
		      screen_y1+small_dist);
	gdk_draw_line(w_current->backingstore, w_current->gc, 
		      screen_x1,
		      screen_y1+small_dist,
		      screen_x1+small_dist,
		      screen_y1+small_dist);
      }	
    }
    
  } else {
    if (w_current->doing_pan) {
      o_text_draw_rectangle(w_current, o_current);
      return;
    }
  }
	
  /* return if text origin marker displaying is disabled */ 
  if (w_current->text_origin_marker == FALSE) {
    return;
  }

  small_dist = SCREENabs(w_current, 10);

  screen_x1 = o_current->text->screen_x;
  screen_y1 = o_current->text->screen_y;

  /* this is not really a fix, but a lame patch */
  /* not having this will cause a bad draw of things when coords */
  /* get close to the 2^15 limit of X */
  if (screen_x1+small_dist > 32767 || screen_y1+small_dist > 32767) {
    return;
  }

  if (w_current->override_color != -1 ) {
    gdk_gc_set_foreground(w_current->gc, 
                          x_get_color(w_current->override_color));
  } else {

    gdk_gc_set_foreground(w_current->gc, 
                          x_get_color(w_current->lock_color));
  }

  if (w_current->DONT_REDRAW == 0) {
    gdk_draw_line(w_current->window, w_current->gc, 
		  screen_x1-small_dist, 
		  screen_y1+small_dist, 
		  screen_x1+small_dist, 
		  screen_y1-small_dist);
    gdk_draw_line(w_current->backingstore, w_current->gc, 
		  screen_x1-small_dist, 
		  screen_y1+small_dist, 
		  screen_x1+small_dist, 
		  screen_y1-small_dist);
    
    gdk_draw_line(w_current->window, w_current->gc, 
		  screen_x1+small_dist, 
		  screen_y1+small_dist, 
		  screen_x1-small_dist, 
		  screen_y1-small_dist);
    gdk_draw_line(w_current->backingstore, w_current->gc, 
		  screen_x1+small_dist, 
		  screen_y1+small_dist, 
		  screen_x1-small_dist, 
		  screen_y1-small_dist);
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_text_erase(TOPLEVEL *w_current, OBJECT *o_current)
{
  w_current->override_color = w_current->background_color;
  o_text_draw(w_current, o_current);
  w_current->override_color = -1;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_text_draw_xor(TOPLEVEL *w_current, int dx, int dy, OBJECT *o_current)
{
  int screen_x1, screen_y1;
  int width, height;
  int color, factor;

  if (o_current->visibility == INVISIBLE && !w_current->show_hidden_text) {
    return;
  }

  /* always display text which is 12 or larger */
  factor = (int) w_current->page_current->to_world_x_constant;
  if ((factor < w_current->text_display_zoomfactor) ||
      o_current->text->size >= 12 ||
      w_current->text_feedback == ALWAYS) {
    o_complex_draw_xor(w_current, dx, dy, o_current->text->prim_objs);
  } else {
    /* text is too small so go through and draw a line in
       it's place */

    screen_x1 = o_current->text->screen_x;
    screen_y1 = o_current->text->screen_y;

    if (o_current->saved_color != -1) {
      color = o_current->saved_color;
    } else {
      color = o_current->color;
    }

    gdk_gc_set_foreground(w_current->outline_xor_gc,
                          x_get_darkcolor(color));

#if 0
    width = SCREENabs(w_current, o_text_width(w_current, 
                                               o_current->text->string,
                                               o_current->text->size/2)); 
    height = SCREENabs(w_current, o_text_height(o_current->text->string,
						o_current->text->size));
#endif

    width = SCREENabs(w_current, o_current->text->displayed_width);
    height = SCREENabs(w_current, o_current->text->displayed_height);

    switch(o_current->text->angle) {
      case(0):
	gdk_draw_rectangle(w_current->window,
			   w_current->outline_xor_gc,
			   FALSE,
			   screen_x1+dx,
			   screen_y1+dy-height,
			   width,
			   height);
        break;

      case(90):
	gdk_draw_rectangle(w_current->window,
			   w_current->outline_xor_gc,
			   FALSE,
			   screen_x1+dx-height,
			   screen_y1+dy-width,
			   height,
			   width);
        break;

      case(180):
	gdk_draw_rectangle(w_current->window,
			   w_current->outline_xor_gc,
			   FALSE,
			   screen_x1+dx-width,
			   screen_y1+dy,
			   width,
			   height);

        break;

      case(270):
	gdk_draw_rectangle(w_current->window,
			   w_current->outline_xor_gc,
			   FALSE,
			   screen_x1+dx,
			   screen_y1+dy,
			   height,
			   width);
        break;
    }
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_text_input(TOPLEVEL *w_current)
{
  text_input_dialog(w_current);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_text_start(TOPLEVEL *w_current, int screen_x, int screen_y)
{
  int x, y;
  int temp, i;
  char *value;

  w_current->last_x = w_current->start_x = fix_x(w_current, screen_x);
  w_current->last_y = w_current->start_y = fix_y(w_current, screen_y);

  w_current->last_drawb_mode = -1;

  /* make sure list is null first, so that you don't have a mem leak */
  SCREENtoWORLD(w_current,
                w_current->start_x,
                w_current->start_y,
                &x,
                &y);

  /* remove the old attrib list if it exists without killing the
     head structure */
  o_list_delete_rest(w_current,
                     w_current->page_current->attrib_place_head);

  value = w_current->current_attribute;

  switch(w_current->text_caps) {
    case(LOWER):
      string_tolower(value, value);
      break;

    case(UPPER):
      string_toupper(value, value);
      break;

    case(BOTH):
    default:
      /* do nothing */
      break;
  }

  /* here you need to add OBJ_TEXT when it's done */
  w_current->page_current->attrib_place_tail =
  (OBJECT *) o_text_add(
			w_current,
			w_current->page_current->attrib_place_head,
				/* type changed from TEXT to TEXT */
			OBJ_TEXT, w_current->text_color,
			x, y, LOWER_LEFT, 0, /* zero is angle */
			w_current->current_attribute,
			w_current->text_size,
			/* has to be visible so you can place it */
			/* visibility is set when you create the object */
			VISIBLE, SHOW_NAME_VALUE);

  if (w_current->complex_rotate) {
    temp = w_current->complex_rotate / 90;
    for (i = 0; i < temp; i++) {
      o_text_place_rotate(w_current);
    }
  }

  o_drawbounding(w_current,
                 w_current->page_current->attrib_place_head->next,
                 NULL,
                 x_get_darkcolor(w_current->bb_color), TRUE);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_text_end(TOPLEVEL *w_current)
{
  /*! \todo get consistant names */
  int world_x, world_y;

  SCREENtoWORLD(w_current,
                w_current->last_x,
                w_current->last_y,
                &world_x,
                &world_y);

  world_x = snap_grid(w_current, world_x);
  world_y = snap_grid(w_current, world_y);

  /* here you need to add OBJ_TEXT when it's done */
  /*! \todo make this VIS and SHOW default configurable */
  w_current->page_current->object_tail =
  o_text_add(w_current, w_current->page_current->object_tail,
				/* type changed from TEXT to TEXT */
             OBJ_TEXT,
             w_current->text_color,
             world_x, world_y, LOWER_LEFT, 
             w_current->complex_rotate,
             w_current->current_attribute,
             w_current->text_size,
             VISIBLE, SHOW_NAME_VALUE);

  /* if the text is invisible then you need to erase the outline
     left by the place */
  if (w_current->current_visible == INVISIBLE) {
    o_drawbounding(
                   w_current,
                   w_current->page_current->attrib_place_head->next,
                   NULL,
                   x_get_darkcolor(w_current->bb_color), FALSE);
  }
  /*! \todo you need to erase the bounding box if have that mode
     set!!! */

  /* erase the old bounding box / outline */
  if (w_current->actionfeedback_mode == OUTLINE) {
    o_drawbounding(
                   w_current,
                   w_current->page_current->attrib_place_head->next,
                   NULL,
                   x_get_color(w_current->text_color), FALSE);
  } else {
    o_drawbounding(
                   w_current,
                   w_current->page_current->attrib_place_head->next,
                   NULL,
                   x_get_darkcolor(w_current->select_color), FALSE);
  }

  w_current->override_color = -1;

  w_current->page_current->CHANGED=1;
  o_select_run_hooks(w_current, NULL, 2); 
  o_selection_remove_most(w_current,
                          w_current->page_current->selection2_head);
  o_selection_add(w_current->page_current->selection2_head, 
                  w_current->page_current->object_tail);
	

  /* object_tail is the object that was just added */
  if (w_current->page_current->object_tail->draw_func != NULL &&
      w_current->page_current->object_tail->type != OBJ_HEAD) {
    (*w_current->page_current->object_tail->draw_func)(
                                                       w_current,
                                                       w_current->page_current->object_tail);
  }

  w_current->override_color = -1;
  o_undo_savestate(w_current, UNDO_ALL);
  i_update_menus(w_current);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_text_rubberattrib(TOPLEVEL *w_current)
{
  o_drawbounding(w_current,
                 w_current->page_current->attrib_place_head->next,
                 NULL,
                 x_get_darkcolor(w_current->bb_color), FALSE);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_text_edit(TOPLEVEL *w_current, OBJECT *o_current)
{
  /* you need to check to make sure only one object is selected */
  /* no actually this is okay... not here in o_edit */
  text_edit_dialog(w_current,
                   o_current->text->string, o_current->text->size,
                   o_current->text->alignment);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_text_edit_end(TOPLEVEL *w_current, char *string, int len, int text_size,
		     int text_alignment)
{
  OBJECT *object;
  SELECTION *s_current;
  int numselect;

  /* skip over head */
  s_current = w_current->page_current->selection2_head->next;
  numselect = o_selection_return_num(w_current->page_current->selection2_head);
  
  while(s_current != NULL) {
    object = s_current->selected_object;

    if (object) {
      if (object->type == OBJ_TEXT) {

        /* only change text string if there is only ONE text object selected */
        if (numselect == 1 && string) {
          if (object->text->string) {
            g_free(object->text->string);
          }
          object->text->string = g_strdup (string);
	  /* handle slot= attribute, it's a special case */
	  if (g_ascii_strncasecmp (string, "slot=", 5) == 0) {
	    o_slot_end (w_current, string, strlen (string));
	  }
        }

        object->text->size = text_size;
        object->text->alignment = text_alignment;
		
        /* probably the text object should be extended to carry a color */
        /* and we should pass it here with a function parameter (?) */
        object->saved_color = w_current->edit_color;

        o_text_erase(w_current, object);
        o_text_recreate(w_current, object);
        o_text_draw(w_current, object);

      } 
    }
    
    s_current = s_current->next;
  }
  
  w_current->page_current->CHANGED = 1;
  o_undo_savestate(w_current, UNDO_ALL);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \note
 *  The object passed in should be the REAL object, NOT any copy in any
 *  selection list
 */
void o_text_change(TOPLEVEL *w_current, OBJECT *object, char *string, 
		   int visibility, int show)
{
  if (object == NULL) {
    return;
  }

  if (object->type != OBJ_TEXT) {
    return;
  }

  /* erase old object */
  o_text_erase(w_current, object);

  /* second change the real object */
  if (object->text->string) {
    g_free(object->text->string);
  }

  object->text->string = g_strdup (string);
  object->visibility = visibility;
  object->show_name_value = show;
  o_text_recreate(w_current, object);
  o_text_draw(w_current, object);

  /* handle slot= attribute, it's a special case */
  if (g_ascii_strncasecmp (string, "slot=", 5) == 0) {
    o_slot_end (w_current, string, strlen (string));
  }

  w_current->page_current->CHANGED = 1;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_text_place_rotate(TOPLEVEL *w_current)
{
  OBJECT *o_current;
  int screen_x_local = -1;
  int screen_y_local = -1;
  int new_angle;

  o_current = w_current->page_current->attrib_place_head->next;
  while(o_current) {
    switch(o_current->type) {	
      case(OBJ_TEXT):
        screen_x_local = o_current->text->screen_x; 
        screen_y_local = o_current->text->screen_y;
        break;
    }
    o_current = o_current->next;
  }

  if (screen_x_local == -1) {
    printf("Could not find text obj in new text placement!\n");
    return;
  }

  o_current = w_current->page_current->attrib_place_head->next;
  while(o_current) {
    switch(o_current->type) {	

      case(OBJ_TEXT):
        new_angle = (o_current->text->angle + 90) % 360;
        o_text_rotate(w_current, screen_x_local, screen_y_local,
                      new_angle, 90, o_current);
        break;
    }
    o_current = o_current->next;
  }
}
