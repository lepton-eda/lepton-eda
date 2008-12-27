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
#include <sys/stat.h>
#include <math.h>
#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "gschem.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

#define MINIMUM_MARK_SMALL_DIST 1


/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_text_draw_lowlevel(GSCHEM_TOPLEVEL *w_current, OBJECT *o_current)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  int left, right, top, bottom;

  g_return_if_fail (o_current != NULL);
  g_return_if_fail (o_current->text != NULL);

  if (o_current->visibility == INVISIBLE && toplevel->show_hidden_text &&
      o_current->text->prim_objs == NULL) {
    o_text_recreate(toplevel, o_current);
  }
  
  o_redraw(w_current, o_current->text->prim_objs, TRUE);

  world_get_object_glist_bounds (toplevel, o_current->text->prim_objs,
                                 &left, &top, &right, &bottom);
  o_current->w_left   = left;
  o_current->w_top    = top;
  o_current->w_right  = right;
  o_current->w_bottom = bottom;

}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_text_draw_rectangle(GSCHEM_TOPLEVEL *w_current, OBJECT *o_current)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  int left=0, right=0, top=0, bottom=0;
  GdkColor *color;

  if (o_current->visibility == INVISIBLE && toplevel->show_hidden_text &&
      o_current->text->prim_objs == NULL) {
    o_text_recreate(toplevel, o_current);
  }

  /* text is too small so go through and draw a rectangle in
     it's place */

  /* NOTE THAT THE TOP AND BOTTOM ARE REVERSED THROUGHT THE WHOLE OF GEDA FOR WORLD COORDS */
  WORLDtoSCREEN( toplevel, o_current->w_left, o_current->w_bottom, &left, &top );
  WORLDtoSCREEN( toplevel, o_current->w_right, o_current->w_top, &right, &bottom );

  if (toplevel->override_color != -1 ) {  /* Override */
    color = x_get_color (toplevel->override_color);
  } else {
    color = x_get_color (o_current->color);
  }
  gdk_gc_set_foreground(w_current->gc, color);

  if (toplevel->DONT_REDRAW == 0) {
    gdk_draw_rectangle (w_current->drawable,
                        w_current->gc,
                        FALSE,
                        left,
                        top,
                        right - left,
                        bottom - top);
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_text_draw(GSCHEM_TOPLEVEL *w_current, OBJECT *o_current)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  int screen_x1, screen_y1;
  int small_dist, offset;

  g_return_if_fail (o_current != NULL);
  g_return_if_fail (o_current->type == OBJ_TEXT);
  g_return_if_fail (o_current->text != NULL);

  if (toplevel->DONT_REDRAW == 1 ||
      (o_current->visibility == INVISIBLE && !toplevel->show_hidden_text)) {
    return;
  }

  if (!w_current->fast_mousepan || !w_current->doing_pan) {
    o_text_draw_lowlevel(w_current, o_current);

    /* Indicate on the schematic that the text is invisible by */
    /* drawing a little I on the screen at the origin */
    if (o_current->visibility == INVISIBLE && toplevel->show_hidden_text) {
      if (toplevel->override_color != -1 ) {
        gdk_gc_set_foreground(w_current->gc, 
                              x_get_color(toplevel->override_color));
      } else {

        gdk_gc_set_foreground (w_current->gc, x_get_color (LOCK_COLOR));
      }

      offset = SCREENabs(toplevel, 10);
      small_dist = SCREENabs(toplevel, 20);
      WORLDtoSCREEN( toplevel, o_current->text->x, o_current->text->y, &screen_x1, &screen_y1 );
      screen_x1 += offset;
      screen_y1 += offset;
      if (toplevel->DONT_REDRAW == 0) {
        /* Top part of the I */
        gdk_draw_line (w_current->drawable, w_current->gc,
                       screen_x1,
                       screen_y1,
                       screen_x1+small_dist,
                       screen_y1);
        /* Middle part of the I */
        gdk_draw_line (w_current->drawable, w_current->gc,
                       screen_x1+small_dist/2,
                       screen_y1,
                       screen_x1+small_dist/2,
                       screen_y1+small_dist);
        /* Bottom part of the I */
        gdk_draw_line (w_current->drawable, w_current->gc,
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

  small_dist = SCREENabs(toplevel, 10);

  /* Switch of mark drawing for non-selected text, and at small sizes */
  if (!o_current->selected || small_dist < MINIMUM_MARK_SMALL_DIST)
    return;

  WORLDtoSCREEN( toplevel, o_current->text->x, o_current->text->y, &screen_x1, &screen_y1 );

  /* this is not really a fix, but a lame patch */
  /* not having this will cause a bad draw of things when coords */
  /* get close to the 2^15 limit of X */
  if (screen_x1+small_dist > 32767 || screen_y1+small_dist > 32767) {
    return;
  }

  if (toplevel->override_color != -1 ) {
    gdk_gc_set_foreground(w_current->gc, 
                          x_get_color(toplevel->override_color));
  } else {

    gdk_gc_set_foreground (w_current->gc, x_get_color (LOCK_COLOR));
  }

  if (toplevel->DONT_REDRAW == 0) {
    gdk_draw_line (w_current->drawable, w_current->gc,
                   screen_x1-small_dist,
                   screen_y1+small_dist,
                   screen_x1+small_dist,
                   screen_y1-small_dist);

    gdk_draw_line (w_current->drawable, w_current->gc,
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
void o_text_draw_xor(GSCHEM_TOPLEVEL *w_current, int dx, int dy, OBJECT *o_current)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  int top, bottom, left, right;
  int color, factor;

  if (o_current->visibility == INVISIBLE && !toplevel->show_hidden_text) {
    return;
  }

  /* always display text which is 12 or larger */
  factor = (int) toplevel->page_current->to_world_x_constant;
  if ((factor < w_current->text_display_zoomfactor) ||
      o_current->text->size >= 12 ||
      w_current->text_feedback == ALWAYS) {
    o_glist_draw_xor (w_current, dx, dy, o_current->text->prim_objs);
  } else {
    /* text is too small so go through and draw a line in
       it's place */
    WORLDtoSCREEN(toplevel, o_current->w_left + dx, o_current->w_bottom + dy, &left, &top);
    WORLDtoSCREEN(toplevel, o_current->w_right + dx, o_current->w_top + dy, &right, &bottom);

    if (o_current->saved_color != -1) {
      color = o_current->saved_color;
    } else {
      color = o_current->color;
    }

    gdk_gc_set_foreground(w_current->outline_xor_gc,
                          x_get_darkcolor(color));

    gdk_draw_rectangle (w_current->drawable,
                        w_current->outline_xor_gc,
                        FALSE,
                        left,
                        top,
                        right - left,
                        bottom - top);
  }
}


/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_text_prepare_place(GSCHEM_TOPLEVEL *w_current, char *text)
{
  TOPLEVEL *toplevel = w_current->toplevel;

  /* Insert the new object into the buffer at world coordinates (0,0).
   * It will be translated to the mouse coordinates during placement. */

  w_current->first_wx = 0;
  w_current->first_wy = 0;

  w_current->last_drawb_mode = LAST_DRAWB_MODE_NONE;

  /* remove the old place list if it exists */
  s_delete_object_glist(toplevel, toplevel->page_current->place_list);
  toplevel->page_current->place_list = NULL;

  /* here you need to add OBJ_TEXT when it's done */
  toplevel->page_current->place_list =
    g_list_append(toplevel->page_current->place_list,
                  o_text_new (toplevel, OBJ_TEXT, TEXT_COLOR,
                              0, 0, LOWER_LEFT, 0, /* zero is angle */
                              text,
                              w_current->text_size,
                              /* has to be visible so you can place it */
                              /* visibility is set when you create the object */
                              VISIBLE, SHOW_NAME_VALUE));

  w_current->inside_action = 1;
  i_set_state (w_current, ENDTEXT);
}


/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_text_edit(GSCHEM_TOPLEVEL *w_current, OBJECT *o_current)
{
  /* you need to check to make sure only one object is selected */
  /* no actually this is okay... not here in o_edit */
  text_edit_dialog(w_current,
                   o_text_get_string (w_current->toplevel, o_current),
                   o_current->text->size, o_current->text->alignment);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_text_edit_end(GSCHEM_TOPLEVEL *w_current, char *string, int len, int text_size,
		     int text_alignment)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  OBJECT *object;
  GList *s_current;
  int numselect;

  /* skip over head */
  s_current = geda_list_get_glist( toplevel->page_current->selection_list );
  numselect = g_list_length( geda_list_get_glist( toplevel->page_current->selection_list ));
  
  while(s_current != NULL) {
    object = (OBJECT *) s_current->data;

    if (object) {
      if (object->type == OBJ_TEXT) {
        o_invalidate (w_current, object);

        object->text->size = text_size;
        object->text->alignment = text_alignment;

        /* probably the text object should be extended to carry a color */
        /* and we should pass it here with a function parameter (?) */
        object->saved_color = w_current->edit_color;

        /* only change text string if there is only ONE text object selected */
        if (numselect == 1 && string) {
          o_text_set_string (w_current->toplevel, object, string);
	  /* handle slot= attribute, it's a special case */
	  if (g_ascii_strncasecmp (string, "slot=", 5) == 0) {
	    o_slot_end (w_current, string, strlen (string));
	  }
        }
        o_text_recreate(toplevel, object);
        o_invalidate (w_current, object);
      } 
    }
    
    s_current = g_list_next(s_current);
  }
  
  toplevel->page_current->CHANGED = 1;
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
void o_text_change(GSCHEM_TOPLEVEL *w_current, OBJECT *object, char *string,
		   int visibility, int show)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  if (object == NULL) {
    return;
  }

  if (object->type != OBJ_TEXT) {
    return;
  }

  /* erase old object */
  o_invalidate (w_current, object);

  /* second change the real object */
  o_text_set_string (toplevel, object, string);

  object->visibility = visibility;
  object->show_name_value = show;
  o_text_recreate(toplevel, object);
  o_invalidate (w_current, object);

  /* handle slot= attribute, it's a special case */
  if (g_ascii_strncasecmp (string, "slot=", 5) == 0) {
    o_slot_end (w_current, string, strlen (string));
  }

  toplevel->page_current->CHANGED = 1;
}
