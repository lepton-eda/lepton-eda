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

#define INVALIDATE_MARGIN 1

extern LeptonColorMap display_colors;
extern LeptonColorMap display_outline_colors;

/*! \todo Lots of Gross code... needs lots of cleanup - mainly
 * readability issues
 */

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
#ifdef ENABLE_GTK3
void
o_redraw_rect (GschemToplevel *w_current,
               GtkWidget *widget,
               LeptonPage *page,
               GschemPageGeometry *geometry,
               cairo_t *cr)
#else
void o_redraw_rect (GschemToplevel *w_current,
                    GdkDrawable *drawable,
                    LeptonPage *page,
                    GschemPageGeometry *geometry,
                    GdkRectangle *rectangle)
#endif
{
  gboolean draw_selected;
  int grip_half_size;
  double cue_half_size;
  int bloat;
  double dummy = 0.0;
  GList *obj_list;
  GList *iter;
  LeptonBox *world_rect;
  EdaRenderer *renderer;
  int render_flags;
  GArray *render_color_map = NULL;
  GArray *render_outline_color_map = NULL;
#ifndef ENABLE_GTK3
  cairo_t *cr;
#endif

  g_return_if_fail (w_current != NULL);
  g_return_if_fail (page != NULL);
  g_return_if_fail (geometry != NULL);
#ifndef ENABLE_GTK3
  cr = gdk_cairo_create (drawable);

  gdk_cairo_rectangle (cr, rectangle);
  cairo_clip (cr);

  cairo_save (cr);
#endif
  cairo_set_matrix (cr, gschem_page_geometry_get_world_to_screen_matrix (geometry));

  grip_half_size = GRIP_SIZE / 2;
  cue_half_size = CUE_BOX_SIZE;
  cairo_user_to_device (cr, &cue_half_size, &dummy);
  bloat = MAX (grip_half_size, (int)cue_half_size);


  world_rect = g_new (LeptonBox, 1);

#ifdef ENABLE_GTK3
  gint wx, wy;
  gtk_widget_translate_coordinates (w_current->drawing_area,
                                    gtk_widget_get_toplevel (w_current->drawing_area),
                                    0, 0, &wx, &wy);

  gint x = 0;
  gint y = 0;
  gint width = gtk_widget_get_allocated_width (GTK_WIDGET (widget));
  gint height = gtk_widget_get_allocated_height (GTK_WIDGET (widget));

  double lower_x = x - bloat;
  double lower_y = y + height + bloat;
  double upper_x = x + width + bloat;
  double upper_y = y - bloat;
#else
  double lower_x = rectangle->x - bloat;
  double lower_y = rectangle->y + rectangle->height + bloat;
  double upper_x = rectangle->x + rectangle->width + bloat;
  double upper_y = rectangle->y - bloat;
#endif

  cairo_device_to_user (cr, &lower_x, &lower_y);
  cairo_device_to_user (cr, &upper_x, &upper_y);

  world_rect->lower_x = floor (lower_x);
  world_rect->lower_y = floor (lower_y);
  world_rect->upper_x = ceil (upper_x);
  world_rect->upper_y = ceil (upper_y);

  gboolean show_hidden_text =
    gschem_toplevel_get_show_hidden_text (w_current);

  obj_list = lepton_page_objects_in_regions (page,
                                             world_rect,
                                             1,
                                             show_hidden_text);

  g_free (world_rect);

  /* Set up renderer based on configuration in w_current */
  render_flags = EDA_RENDERER_FLAG_HINTING;
  if (show_hidden_text)
    render_flags |= EDA_RENDERER_FLAG_TEXT_HIDDEN;
  if (w_current->fast_mousepan &&
      gschem_toplevel_get_current_page_view(w_current)->doing_pan)
    render_flags |= (EDA_RENDERER_FLAG_TEXT_OUTLINE
                     | EDA_RENDERER_FLAG_PICTURE_OUTLINE);

  /* This color map is used for "normal" rendering. */
  render_color_map =
    g_array_sized_new (FALSE, FALSE, sizeof(LeptonColor), colors_count());
  render_color_map =
    g_array_append_vals (render_color_map, display_colors, colors_count());

  /* This color map is used for rendering rubberbanding nets and
     buses, and objects which are in the process of being placed. */
  render_outline_color_map =
    g_array_sized_new (FALSE, FALSE, sizeof(LeptonColor), colors_count());
  render_outline_color_map =
    g_array_append_vals (render_outline_color_map, display_outline_colors,
                         colors_count());

  /* Set up renderer */
  renderer = EDA_RENDERER (g_object_ref (w_current->renderer));
  g_object_set (G_OBJECT (renderer),
                "cairo-context", cr,
                "grip-size", ((double) grip_half_size * geometry->to_world_x_constant),
                "render-flags", render_flags,
                "color-map", render_color_map,
                NULL);

  /* Paint background */
  LeptonColor *color = x_color_lookup (BACKGROUND_COLOR);

  cairo_set_source_rgba (cr,
                         lepton_color_get_red_double (color),
                         lepton_color_get_green_double (color),
                         lepton_color_get_blue_double (color),
                         lepton_color_get_alpha_double (color));

#ifdef ENABLE_GTK3
  double cx=wx, cy=wy;
  cairo_device_to_user_distance (cr, &cx, &cy);
  cairo_translate (cr, cx, cy);
#endif
  cairo_paint (cr);

  /* Draw grid lines */
#ifdef ENABLE_GTK3
  x_grid_draw_region (w_current, cr,
                      wx, wy,
                      width, height);
#else
  x_grid_draw_region (w_current, cr,
                      rectangle->x, rectangle->y,
                      rectangle->width, rectangle->height);
#endif

  SchematicActionMode action_mode =
    schematic_window_get_action_mode (w_current);

  /* Determine whether we should draw the selection at all */
  draw_selected = !(schematic_window_get_inside_action (w_current) &&
                    (action_mode == MOVEMODE));

  /* First pass -- render non-selected objects */
  for (iter = obj_list; iter != NULL; iter = g_list_next (iter)) {
    LeptonObject *o_current = (LeptonObject*) iter->data;

    if (!(o_current->dont_redraw
          || lepton_object_get_selected (o_current)))
    {
      eda_renderer_draw (renderer, o_current);
    }
  }

  /* Second pass -- render cues */
  for (iter = obj_list; iter != NULL; iter = g_list_next (iter)) {
    LeptonObject *o_current = (LeptonObject*) iter->data;

    if (!(o_current->dont_redraw
          || lepton_object_get_selected (o_current)))
    {
      eda_renderer_draw_cues (renderer, o_current);
    }
  }

  /* Second pass -- render selected objects, cues & grips. This is
   * done in a separate pass to non-selected items to make sure that
   * the selection and grips are never obscured by other objects. */
  if (draw_selected) {
    g_object_set (G_OBJECT (renderer),
                  "override-color", SELECT_COLOR,
                  NULL);
    for (iter = lepton_list_get_glist (page->selection_list);
         iter != NULL; iter = g_list_next (iter)) {
      LeptonObject *o_current = (LeptonObject*) iter->data;
      if (!o_current->dont_redraw) {
        eda_renderer_draw (renderer, o_current);
        eda_renderer_draw_cues (renderer, o_current);
        if (w_current->draw_grips) {
          eda_renderer_draw_grips (renderer, o_current);
        }
      }
    }
    g_object_set (G_OBJECT (renderer),
                  "override-color", -1,
                  NULL);
  }

  if (schematic_window_get_inside_action (w_current))
  {
    /* Redraw the rubberband objects (if they were previously visible) */
    if (page->place_list != NULL) {
      switch (action_mode)
      {
        case COMPMODE:
        case TEXTMODE:
        case COPYMODE:
        case MCOPYMODE:
        case PASTEMODE:
          if (schematic_window_get_rubber_visible (w_current))
          {
            /* FIXME shouldn't need to save/restore colormap here */
            cairo_save (cr);
            eda_renderer_set_color_map (renderer, render_outline_color_map);

            o_place_draw_rubber (w_current, renderer);

            eda_renderer_set_color_map (renderer, render_color_map);
            cairo_restore (cr);
          }
        break;
        case MOVEMODE:
          /* FIXME shouldn't need to save/restore colormap here */
          cairo_save (cr);
          eda_renderer_set_color_map (renderer, render_outline_color_map);

          o_move_draw_rubber (w_current, renderer);

          eda_renderer_set_color_map (renderer, render_color_map);
          cairo_restore (cr);
          break;
        default: break;
      }
    }

    if (schematic_window_get_rubber_visible (w_current))
    {
      switch (action_mode)
      {
        case ARCMODE    : o_arc_draw_rubber (w_current, renderer); break;
        case BOXMODE    : o_box_draw_rubber (w_current, renderer); break;
        case CIRCLEMODE : o_circle_draw_rubber (w_current, renderer); break;
        case LINEMODE   : o_line_draw_rubber (w_current, renderer); break;
        case PATHMODE   : o_path_draw_rubber (w_current, renderer); break;
        case PICTUREMODE: o_picture_draw_rubber (w_current, renderer); break;
        case PINMODE    : o_pin_draw_rubber (w_current, renderer); break;
        case BUSMODE:
          /* FIXME shouldn't need to save/restore colormap here */
          cairo_save (cr);
          eda_renderer_set_color_map (renderer, render_outline_color_map);

          o_bus_draw_rubber(w_current, renderer);

          eda_renderer_set_color_map (renderer, render_color_map);
          cairo_restore (cr);
          break;
        case NETMODE:
          /* FIXME shouldn't need to save/restore colormap here */
          cairo_save (cr);
          eda_renderer_set_color_map (renderer, render_outline_color_map);

          o_net_draw_rubber (w_current, renderer);

          eda_renderer_set_color_map (renderer, render_color_map);
          cairo_restore (cr);
          break;
        case GRIPS      : o_grips_draw_rubber (w_current, renderer); break;
        case SBOX       : o_select_box_draw_rubber (w_current, renderer); break;
        case ZOOMBOX    : a_zoom_box_draw_rubber (w_current, renderer); break;
        default: break;
      }
    }
  }

  g_list_free (obj_list);
  g_object_unref (G_OBJECT (renderer));
  g_array_free (render_color_map, TRUE);
  g_array_free (render_outline_color_map, TRUE);

#ifndef ENABLE_GTK3
  cairo_destroy (cr);
#endif
}


/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
int o_invalidate_rubber (GschemToplevel *w_current)
{
  /* return FALSE if it did not erase anything */

  if (!schematic_window_get_inside_action (w_current))
    return(FALSE);

  switch (schematic_window_get_action_mode (w_current))
  {
    case (ARCMODE)    : o_arc_invalidate_rubber (w_current); break;
    case (BOXMODE)    : o_box_invalidate_rubber (w_current); break;
    case (BUSMODE)    : o_bus_invalidate_rubber (w_current); break;
    case (CIRCLEMODE) : o_circle_invalidate_rubber (w_current); break;
    case (LINEMODE)   : o_line_invalidate_rubber (w_current); break;
    case (NETMODE)    : o_net_invalidate_rubber (w_current); break;
    case (PATHMODE)   : o_path_invalidate_rubber (w_current); break;
    case (PICTUREMODE): o_picture_invalidate_rubber (w_current); break;
    case (PINMODE)    : o_pin_invalidate_rubber (w_current); break;

    default:
      return(FALSE);
    break;
  }

  return(TRUE);
}


/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *  This function is neccesary to make jumps between action modes.
 *  If we are inside an drawing action that created something on the dc,
 *  e.g. if we are drawing a box and then jump to line drawing without
 *  leaving the box drawing mode, there will remain some rubberbands on the
 *  screen.
 *  Usually a intermediate select state would clean (redraw) the screen.
 */
int o_redraw_cleanstates(GschemToplevel *w_current)
{
  LeptonPage *active_page = schematic_window_get_active_page (w_current);
  /* returns FALSE if the function was'nt nessecary */
  if (schematic_window_get_inside_action (w_current) == 0)
  {
    return FALSE;
  }

  SchematicActionMode action_mode =
    schematic_window_get_action_mode (w_current);

  switch (action_mode)
  {
    /* all states with something on the dc */
    case(COMPMODE):
      /* De-select the lists in the component selector */
      x_compselect_deselect (w_current);

      /* Fall through */
    case(ARCMODE):
    case(BOXMODE):
    case(BUSMODE):
    case(CIRCLEMODE):
    case(LINEMODE):
    case(NETMODE):
    case(PATHMODE):
    case(PICTUREMODE):
    case(PINMODE):
    case(COPYMODE):
    case(MCOPYMODE):
    case(MOVEMODE):
    case(PASTEMODE):
    case(TEXTMODE):
    case(GRIPS):
    case(ZOOMBOX):
      /* it is possible to cancel in the middle of a place,
       * so lets be sure to clean up the place_list structure */

      /* If we're cancelling from a move action, re-wind the
       * page contents back to their state before we started. */
      if (action_mode == MOVEMODE)
      {
        o_move_cancel (w_current);
      }

      /* If we're cancelling from a grip action, call the specific cancel
       * routine to reset the visibility of the object being modified */
      if (action_mode == GRIPS)
      {
        o_grips_cancel (w_current);
      }

      /* Free the place list and its contents. If we were in a move
       * action, the list (refering to objects on the page) would
       * already have been cleared in o_move_cancel(), so this is OK. */
      lepton_object_list_delete (active_page->place_list);
      active_page->place_list = NULL;

      i_action_stop (w_current);

      /* touch the select state */
      i_set_state(w_current, SELECT);

      /* from i_callback_cancel() */
      gschem_page_view_invalidate_all (gschem_toplevel_get_current_page_view (w_current));
      return TRUE;

    /* all remaining states without dc changes */
    case(SELECT):
    case(PAN):
    case(MIRRORMODE):
    case(ROTATEMODE):
    case(SBOX):
      return FALSE;
  }

  return FALSE;
}

/*! \brief Invalidates a rectangular region of the on screen drawing area
 *  \par Function Description
 *
 *  Given a pair of (x,y) coordinates in SCREEN units, invalidate the
 *  rectangular on-screen drawing area which has those two coordinate
 *  pairs as opposite corners of its region. This will cause that region
 *  to be blitted from the back-buffer once the mainloop reaches idle.
 *
 *  A margin, INVALIDATE_MARGIN is added to the invalidated region as
 *  a hacky workaround for rounding errors which may occur in the
 *  WORLD -> SCREEN coordinate transform. This margin may also be used
 *  to expand the invalidated region if anti-aliased drawing is ever
 *  used.
 *
 *  A further, larger margin is added to account for invalidating the
 *  size occupied by an object's grips.
 *
 *  If the GschemToplevel in question is not rendering to a GDK_WINDOW,
 *  (e.g. image export), this function call is a no-op. A test is used:
 *  GDK_IS_WINDOW(), which should be safe since in either case,
 *  w_current->window is a GObject. This is really a _HACK_,
 *  and should be fixed with a re-worked drawing model.
 *
 *  \param [in] w_current  The GschemToplevel who's drawing area is being invalidated.
 *  \param [in] x1         X coord for corner 1 (SCREEN units)
 *  \param [in] y1         Y coord for corner 1 (SCREEN units)
 *  \param [in] x2         X coord for corner 2 (SCREEN units)
 *  \param [in] y2         Y coord for corner 2 (SCREEN units)
 */
void o_invalidate_rect (GschemToplevel *w_current,
                        int x1, int y1, int x2, int y2)
{
  GschemPageView *page_view = gschem_toplevel_get_current_page_view (w_current);

  gschem_page_view_invalidate_screen_rect (page_view,
                                           x1,
                                           y1,
                                           x2,
                                           y2);
}


/*! \brief Invalidate on-screen area for an object
 *
 *  \par Function Description
 *  This function calls o_invalidate_rect() with the bounds of the
 *  passed LeptonObject, converted to screen coordinates.
 *
 *  \param [in] w_current  The GschemToplevel object.
 *  \param [in] object     The LeptonObject invalidated on screen.
 */
void o_invalidate (GschemToplevel *w_current, LeptonObject *object)
{
  if (w_current == NULL
      || schematic_window_get_dont_invalidate (w_current))
  {
    return;
  }

  int left, top, bottom, right;

  GschemPageView *page_view = gschem_toplevel_get_current_page_view (w_current);
  LeptonPage *page = gschem_page_view_get_page (page_view);
  gboolean show_hidden_text =
    gschem_toplevel_get_show_hidden_text (w_current);

  /* this function may be called before a page is created */
  if (page == NULL) {
    return;
  }

  if (lepton_object_calculate_visible_bounds (object,
                                              show_hidden_text,
                                              &left,
                                              &top,
                                              &right,
                                              &bottom))
  {
    gschem_page_view_invalidate_world_rect (page_view,
                                            left,
                                            top,
                                            right,
                                            bottom);
  }
}


/*! \brief Invalidate on-screen area for a GList of objects
 *
 *  \par Function Description
 *  This function calls o_invalidate_rect() with the bounds of the
 *  passed GList, converted to screen coordinates.
 *
 *  \param [in] w_current  The GschemToplevel object.
 *  \param [in] list       The glist objects invalidated on screen.
 */
void o_invalidate_glist (GschemToplevel *w_current, GList *list)
{
  int left, top, bottom, right;

  GschemPageView *page_view = gschem_toplevel_get_current_page_view (w_current);
  g_return_if_fail (page_view != NULL);

  LeptonPage *page = gschem_page_view_get_page (page_view);
  g_return_if_fail (page != NULL);

  gboolean show_hidden_text =
    gschem_toplevel_get_show_hidden_text (w_current);

  if (world_get_object_glist_bounds (list,
                                     show_hidden_text,
                                     &left,
                                     &top,
                                     &right,
                                     &bottom)) {
    gschem_page_view_invalidate_world_rect (page_view,
                                            left,
                                            top,
                                            right,
                                            bottom);
  }
}
