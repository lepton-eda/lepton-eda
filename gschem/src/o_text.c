/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2010 gEDA Contributors (see ChangeLog for details)
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

#if 1
# define FONT_NAME "Arial"
#else
# define FONT_NAME "Helvetica"
#endif

#undef DEBUG_TEXT


char *unescape_text_and_overbars (char *text, PangoAttrList *attrs)
{
  char *p, *sp, *strip_text;
  char *overbar_start = NULL;
  int escape = FALSE;

  /* The unescaped text is alwasys shorter than the original
     string, so just allocate the same ammount of memory. */
  sp = strip_text = g_malloc (strlen (text) + 1);

  for (p = text; p != NULL; p++) {
    int finish_overbar = FALSE;

    /* If we find an escape character "\", we note it and continue looping */
    if (!escape && *p == '\\') {
      escape = TRUE;
      continue;
    }

    if (escape && *p == '_') {
      /* Overbar start or end sequence */
      if (overbar_start != NULL) {
        finish_overbar = TRUE;
      } else {
        overbar_start = sp;
      }
    } else {
      /* just append the character, which may have been escaped */
      *sp++ = *p;
    }
    escape = FALSE;

    if (overbar_start != NULL &&
        (finish_overbar || *p == '\0')) {
      PangoAttribute *attr;

      attr = gschem_pango_attr_overbar_new (TRUE);
      attr->start_index = overbar_start - strip_text;
      attr->end_index = sp - strip_text;
      pango_attr_list_insert (attrs, attr);
      overbar_start = NULL;
    }

    /* end of the string, stop iterating */
    if (*p == '\0')
      break;
  }

  return strip_text;
}


static void calculate_position (OBJECT *object,
                                PangoFontMetrics *font_metrics,
                                PangoRectangle logical_rect,
                                PangoRectangle inked_rect,
                                double *x, double *y)
{
  double temp;
  double y_lower, y_middle, y_upper;
  double x_left,  x_middle, x_right;
  double descent = pango_font_metrics_get_descent (font_metrics) / PANGO_SCALE;

  x_left   = 0;
  x_middle = -logical_rect.width / 2.;
  x_right  = -logical_rect.width;

  /*! \note Ideally, we would be using just font / logical metrics for vertical
   *        alignment, however this way seems to be more backward compatible
   *        with the old gschem rendering.
   *
   *        Lower alignment is at the baseline of the bottom text line, whereas
   *        middle and upper alignment is based upon the inked extents of the
   *        entire text block.
   */
  y_upper  = -inked_rect.y;                     /* Top of inked extents */
  y_middle = y_upper - inked_rect.height / 2.;  /* Middle of inked extents */
  y_lower  = descent - logical_rect.height;     /* Baseline of bottom line */

  /* Special case flips attachment point to opposite corner when
   * the text is rotated to 180 degrees, since the drawing code
   * does not rotate the text to be shown upside down.
   */
  if (object->text->angle == 180) {
    temp = y_lower; y_lower = y_upper; y_upper = temp;
    temp = x_left;  x_left  = x_right; x_right = temp;
  }

  switch (object->text->alignment) {
    default:
      /* Fall through to LOWER_left case */
    case LOWER_LEFT:    *y = y_lower;  *x = x_left;   break;
    case MIDDLE_LEFT:   *y = y_middle; *x = x_left;   break;
    case UPPER_LEFT:    *y = y_upper;  *x = x_left;   break;
    case LOWER_MIDDLE:  *y = y_lower;  *x = x_middle; break;
    case MIDDLE_MIDDLE: *y = y_middle; *x = x_middle; break;
    case UPPER_MIDDLE:  *y = y_upper;  *x = x_middle; break;
    case LOWER_RIGHT:   *y = y_lower;  *x = x_right;  break;
    case MIDDLE_RIGHT:  *y = y_middle; *x = x_right;  break;
    case UPPER_RIGHT:   *y = y_upper;  *x = x_right;  break;
  }
}


static PangoFontMetrics *setup_pango_return_metrics (GSCHEM_TOPLEVEL *w_current, PangoLayout *layout,
                                                     double scale_factor, OBJECT *o_current)
{
  PangoContext *context;
  PangoFontDescription *desc;
  PangoFontMetrics *font_metrics;
  PangoAttrList *attrs;
  cairo_font_options_t *options;
  double font_size_pt;
  char *unescaped;

  context = pango_layout_get_context (layout);

  /* Switch off metric hinting, set medium outline hinting */
  options = cairo_font_options_create ();
  cairo_font_options_set_hint_metrics (options, CAIRO_HINT_METRICS_OFF);
  cairo_font_options_set_hint_style (options, CAIRO_HINT_STYLE_MEDIUM);
  pango_cairo_context_set_font_options (context, options);
  cairo_font_options_destroy (options);

  pango_cairo_context_set_resolution (context, 1000. * scale_factor);
  font_size_pt = o_text_get_font_size_in_points (w_current->toplevel,
                                                 o_current);

  desc = pango_font_description_from_string (FONT_NAME);
  pango_font_description_set_size (desc, (double)PANGO_SCALE * font_size_pt);

  pango_layout_set_font_description (layout, desc);
  font_metrics = pango_context_get_metrics (context, desc, NULL);
  pango_font_description_free (desc);

  attrs = pango_attr_list_new ();
  unescaped = unescape_text_and_overbars (o_current->text->disp_string, attrs);
  pango_layout_set_text (layout, unescaped, -1);
  g_free (unescaped);
  pango_layout_set_attributes (layout, attrs);
  pango_attr_list_unref (attrs);

  return font_metrics;
}


static void rotate_vector (double x, double y, double angle,
                           double *rx, double *ry)
{
  double costheta = cos (angle * M_PI / 180.);
  double sintheta = sin (angle * M_PI / 180.);

  *rx = costheta * x - sintheta * y;
  *ry = sintheta * x + costheta * y;
}


static void expand_bounds (int *left, int *top, int *right, int *bottom,
                           int new_x, int new_y)
{
  *left =   MIN (*left,   new_x);
  *right =  MAX (*right,  new_x);
  *top =    MIN (*top,    new_y);
  *bottom = MAX (*bottom, new_y);
}


/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
int o_text_get_rendered_bounds (void *user_data, OBJECT *o_current,
                                int *min_x, int *min_y,
                                int *max_x, int *max_y)
{
  GSCHEM_TOPLEVEL *w_current = user_data;
  TOPLEVEL *toplevel = w_current->toplevel;
  PangoLayout *layout;
  cairo_t *cr;
  double x, y;
  PangoFontMetrics *font_metrics;
  PangoRectangle logical_rect;
  PangoRectangle inked_rect;
  int angle;
  double rx, ry;
  double tleft, ttop, tright, tbottom;
  int left, right, top, bottom;

  g_return_val_if_fail (o_current != NULL, FALSE);
  g_return_val_if_fail (o_current->text != NULL, FALSE);

  if (o_current->visibility == INVISIBLE &&
      !toplevel->show_hidden_text)
    return FALSE;

  if (o_current->text->disp_string == NULL)
    return FALSE;

  cr = gdk_cairo_create (w_current->drawable);
  layout = pango_cairo_create_layout (cr);

  font_metrics = setup_pango_return_metrics (w_current, layout, 1., o_current);

  pango_layout_get_pixel_extents (layout, &inked_rect, &logical_rect);
  calculate_position (o_current, font_metrics, logical_rect, inked_rect, &x, &y);
  pango_font_metrics_unref (font_metrics);

  tleft = x + inked_rect.x;
  tright = x + inked_rect.x + inked_rect.width;
  /* Deliberately include bounds up to the height of the logical rect,
   * since we draw overbars in that space. In the unlikely event that
   * the inked rect extends above the logical (inked_rect.y is -ve),
   * do take that into account.
   */
  ttop = -y - (inked_rect.y < 0 ? inked_rect.y : 0.);
  tbottom = -y - inked_rect.y - inked_rect.height;

  angle = o_current->text->angle;
  /* Special case turns upside down text back upright */
  if (angle == 180)
    angle = 0;

  rotate_vector (tleft, ttop, angle, &rx, &ry);
  left = right = rx;
  top = bottom = ry;
  rotate_vector (tright, ttop, angle, &rx, &ry);
  expand_bounds (&left, &top, &right, &bottom, rx, ry);
  rotate_vector (tleft, tbottom, angle, &rx, &ry);
  expand_bounds (&left, &top, &right, &bottom, rx, ry);
  rotate_vector (tright, tbottom, angle, &rx, &ry);
  expand_bounds (&left, &top, &right, &bottom, rx, ry);

  *min_x = o_current->text->x + left;
  *max_x = o_current->text->x + right;
  *min_y = o_current->text->y + top;
  *max_y = o_current->text->y + bottom;

  g_object_unref (layout);
  cairo_destroy (cr);

  return TRUE;
}


#ifdef DEBUG_TEXT
static void draw_construction_lines (GSCHEM_TOPLEVEL *w_current,
                                     double x, double y,
                                     PangoFontMetrics *font_metrics,
                                     PangoRectangle logical_rect)
{
  double px = 1.;
  double dashlength;
  double ascent = pango_font_metrics_get_ascent (font_metrics) / PANGO_SCALE;
  double descent = pango_font_metrics_get_descent (font_metrics) / PANGO_SCALE;
  cairo_t *cr = w_current->cr;

  /* Pick an arbitrary size constant for the construction lines */
  /* Includes * 10 factor for precision */
  px = SCREENabs (w_current, 4 * 10);

  /* Threshold the drawing to be above a certain size */
  if (px < 2.)
    return;

  px = px / 10.;

  /* baseline, descent, ascent, height */
  cairo_set_line_width (cr, 2 * px);
  dashlength = 9 * px;
  cairo_set_dash (cr, &dashlength, 1, 0);

  /* Underline logical text rect in green, y coord is as gschem text origin */
  cairo_set_source_rgba (cr, 0, 0.6, 0, 0.5);
  cairo_move_to (cr, x + logical_rect.x, y + ascent);
  cairo_rel_line_to (cr, logical_rect.width, 0);
  cairo_stroke (cr);

  /* Underline descent height in red */
  cairo_set_source_rgba (cr, 1, 0, 0, 1);
  cairo_move_to (cr, x + logical_rect.x, y + ascent + descent);
  cairo_rel_line_to (cr, logical_rect.width, 0);
  cairo_stroke (cr);

  /* Overbar ascent height in yellow */
  cairo_set_source_rgba (cr, 1, 1, 0, 1);
  cairo_move_to (cr, x + logical_rect.x, y);
  cairo_rel_line_to (cr, logical_rect.width, 0);
  cairo_stroke (cr);

  /* extents: width & height in blue */
  cairo_set_source_rgba (cr, 0, 0, 0.75, 0.5);
  cairo_set_line_width (cr, px);
  dashlength = 3 * px;
  cairo_set_dash (cr, &dashlength, 1, 0);
  cairo_rectangle (cr, x + logical_rect.x, y + logical_rect.y,
                   logical_rect.width, logical_rect.height);
  cairo_stroke (cr);

  /* show layout origin point in black */
  cairo_arc (cr, x, y, 3 * px, 0, 2 * M_PI);
  cairo_set_source_rgba (cr, 0.0, 0, 0, 0.5);
  cairo_fill (cr);

  /* text's advance in blue */
  cairo_set_source_rgba (cr, 0, 0, 0.75, 0.5);
  cairo_arc (cr, x + logical_rect.x + logical_rect.width, y + logical_rect.height - descent,
             3 * px, 0, 2 * M_PI);
  cairo_fill (cr);

  /* reference point in red */
  cairo_arc (cr, x, y + ascent, 3 * px, 0, 2 * M_PI);
  cairo_set_source_rgba (cr, 0.75, 0, 0, 0.5);
  cairo_fill (cr);
}
#endif


/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
static void o_text_draw_lowlevel(GSCHEM_TOPLEVEL *w_current, OBJECT *o_current,
                                 int dx, int dy, COLOR *color)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  cairo_t *cr = w_current->cr;
  int sx, sy;
  double x, y;
  PangoFontMetrics *font_metrics;
  PangoRectangle logical_rect;
  PangoRectangle inked_rect;

  g_return_if_fail (o_current != NULL);
  g_return_if_fail (o_current->text != NULL);

  if (o_current->visibility == INVISIBLE &&
      !toplevel->show_hidden_text)
    return;

  if (o_current->text->disp_string == NULL)
    return;

  font_metrics =
    setup_pango_return_metrics (w_current, w_current->pl,
                                toplevel->page_current->to_screen_x_constant,
                                o_current);

  pango_layout_get_pixel_extents (w_current->pl, &inked_rect, &logical_rect);
  calculate_position (o_current, font_metrics, logical_rect, inked_rect, &x, &y);

  cairo_save (cr);

  WORLDtoSCREEN (w_current, o_current->text->x + dx,
                            o_current->text->y + dy, &sx, &sy);
  cairo_translate (cr, sx, sy);

  /* Special case turns upside-down text back upright */
  if (o_current->text->angle != 180) {
    cairo_rotate (cr, - M_PI * o_current->text->angle / 180.);
  }

  gschem_cairo_set_source_color (w_current, color);

  /* NB: Shift the position by 0.5px to match the hinting applied to single
   *     pixel wide lines. This means the text will sit correctly on top of
   *     the grid lines, and ensures consistency with other lines when the
   *     page view is zoomed out. */
  cairo_move_to (cr, x + 0.5, y + 0.5);
  gschem_pango_show_layout (cr, w_current->pl);

#ifdef DEBUG_TEXT
  draw_construction_lines (w_current, x, y, font_metrics, logical_rect);
#endif

  pango_font_metrics_unref (font_metrics);
  cairo_restore (cr);
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

  if (o_current->visibility == INVISIBLE && !toplevel->show_hidden_text) {
    return;
  }

  if (!w_current->fast_mousepan || !w_current->doing_pan) {

    o_text_draw_lowlevel (w_current, o_current, 0, 0,
                          o_drawing_color (w_current, o_current));

    /* Indicate on the schematic that the text is invisible by */
    /* drawing a little I on the screen at the origin */
    if (o_current->visibility == INVISIBLE && toplevel->show_hidden_text) {
      if (toplevel->override_color != -1 ) {
        gdk_gc_set_foreground(w_current->gc, 
                              x_get_color(toplevel->override_color));
      } else {

        gdk_gc_set_foreground (w_current->gc, x_get_color (LOCK_COLOR));
      }

      offset = SCREENabs (w_current, 10);
      small_dist = SCREENabs (w_current, 20);
      WORLDtoSCREEN (w_current, o_current->text->x, o_current->text->y, &screen_x1, &screen_y1);
      screen_x1 += offset;
      screen_y1 += offset;
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
  } else {
    /* draw a box in it's place */
    gschem_cairo_box (w_current, 0,
                      o_current->w_left,  o_current->w_bottom,
                      o_current->w_right, o_current->w_top);

    gschem_cairo_set_source_color (w_current,
                                   o_drawing_color (w_current, o_current));
    gschem_cairo_stroke (w_current, TYPE_SOLID, END_NONE, 0, -1, -1);

    return;
  }

  /* return if text origin marker displaying is disabled */ 
  if (w_current->text_origin_marker == FALSE) {
    return;
  }

  small_dist = SCREENabs (w_current, 10);

  /* Switch of mark drawing for non-selected text, and at small sizes */
  if (!o_current->selected || small_dist < MINIMUM_MARK_SMALL_DIST)
    return;

  WORLDtoSCREEN (w_current, o_current->text->x, o_current->text->y, &screen_x1, &screen_y1);

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


/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_text_draw_place (GSCHEM_TOPLEVEL *w_current, int dx, int dy, OBJECT *o_current)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  int factor;

  if (o_current->visibility == INVISIBLE && !toplevel->show_hidden_text) {
    return;
  }

  /* always display text which is 12 or larger */
  factor = (int) toplevel->page_current->to_world_x_constant;
  if ((factor < w_current->text_display_zoomfactor) ||
      o_current->text->size >= 12 ||
      w_current->text_feedback == ALWAYS) {

    o_text_draw_lowlevel (w_current, o_current, dx, dy,
                          x_color_lookup_dark (o_current->color));

  } else {
    /* text is too small so draw a box in it's place */

    gschem_cairo_box (w_current, 0,
                      o_current->w_left  + dx, o_current->w_bottom + dy,
                      o_current->w_right + dx, o_current->w_top    + dy);

    gschem_cairo_set_source_color (w_current,
                                   x_color_lookup_dark (o_current->color));
    gschem_cairo_stroke (w_current, TYPE_SOLID, END_NONE, 0, -1, -1);
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

        object->text->size = text_size;
        object->text->alignment = text_alignment;

        /* probably the text object should be extended to carry a color */
        /* and we should pass it here with a function parameter (?) */
        object->color = w_current->edit_color;

        /* only change text string if there is only ONE text object selected */
        if (numselect == 1 && string) {
          o_text_set_string (w_current->toplevel, object, string);
	  /* handle slot= attribute, it's a special case */
	  if (object->attached_to != NULL &&
	      g_ascii_strncasecmp (string, "slot=", 5) == 0) {
	    o_slot_end (w_current, object->attached_to, string);
	  }
        }
        o_text_recreate(toplevel, object);
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

  o_text_set_string (toplevel, object, string);

  object->visibility = visibility;
  object->show_name_value = show;
  o_text_recreate(toplevel, object);

  /* handle slot= attribute, it's a special case */
  if (object->attached_to != NULL &&
      g_ascii_strncasecmp (string, "slot=", 5) == 0) {
    o_slot_end (w_current, object->attached_to, string);
  }

  toplevel->page_current->CHANGED = 1;
}
