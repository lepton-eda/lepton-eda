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
/*!
 * \file gschem_page_view.h
 *
 * \brief A widget for viewing a schematic page
 */

#define SCHEMATIC_TYPE_CANVAS           (schematic_canvas_get_type())
#define SCHEMATIC_CANVAS(obj)           (G_TYPE_CHECK_INSTANCE_CAST ((obj), SCHEMATIC_TYPE_CANVAS, SchematicCanvas))
#define SCHEMATIC_CANVAS_CLASS(klass)   (G_TYPE_CHECK_CLASS_CAST ((klass),  SCHEMATIC_TYPE_CANVAS, SchematicCanvasClass))
#define SCHEMATIC_IS_CANVAS(obj)        (G_TYPE_CHECK_INSTANCE_TYPE ((obj), SCHEMATIC_TYPE_CANVAS))
#define SCHEMATIC_CANVAS_GET_CLASS(obj) (G_TYPE_INSTANCE_GET_CLASS ((obj), SCHEMATIC_TYPE_CANVAS, SchematicCanvasClass))

typedef struct _SchematicCanvasClass SchematicCanvasClass;
typedef struct _SchematicCanvas SchematicCanvas;

struct _SchematicCanvasClass
{
  GtkWindowClass parent_class;
};

struct _SchematicCanvas
{
  GtkWindow parent;

  GtkAllocation previous_allocation;

  GtkAdjustment *hadjustment;
  GtkAdjustment *vadjustment;

  #ifdef ENABLE_GTK3
  GtkScrollablePolicy hscroll_policy;
  GtkScrollablePolicy vscroll_policy;
  #endif

  gboolean configured;

  gboolean doing_pan;  /* mouse pan status flag */
  int pan_x;
  int pan_y;
  int throttle;

  gboolean show_hidden_text;

  /*< private >*/
  LeptonPage *_page;

  GHashTable *_geometry_cache;
};


G_BEGIN_DECLS

GtkAdjustment*
schematic_canvas_get_hadjustment (SchematicCanvas *view);

LeptonPage*
schematic_canvas_get_page (SchematicCanvas *view);

GschemPageGeometry*
schematic_canvas_get_page_geometry (SchematicCanvas *view);

GType
schematic_canvas_get_type ();

GtkAdjustment*
schematic_canvas_get_vadjustment (SchematicCanvas *view);

gboolean
schematic_canvas_get_show_hidden_text (SchematicCanvas *view);

void
schematic_canvas_invalidate_all (SchematicCanvas *view);

void
schematic_canvas_invalidate_screen_rect (SchematicCanvas *view,
                                         int left,
                                         int top,
                                         int right,
                                         int bottom);
void
schematic_canvas_invalidate_world_rect (SchematicCanvas *view,
                                        int left,
                                        int top,
                                        int right,
                                        int bottom);
SchematicCanvas*
schematic_canvas_new_with_page (LeptonPage *page);

void
schematic_canvas_pan_general (SchematicCanvas *page_view,
                              int x,
                              int y,
                              double relativ_zoom_factor);
void
schematic_canvas_pan (SchematicCanvas *page_view,
                      int x,
                      int y);
void
schematic_canvas_pan_mouse (SchematicCanvas *page_view,
                            int diff_x,
                            int diff_y);

void
schematic_canvas_pan_start (SchematicCanvas *page_view,
                            int x,
                            int y);
void
schematic_canvas_pan_motion (SchematicCanvas *view,
                             int mousepan_gain,
                             int x,
                             int y);
gboolean
schematic_canvas_pan_end (SchematicCanvas *page_view);

void
#ifdef ENABLE_GTK3
schematic_canvas_redraw (SchematicCanvas *view,
                         cairo_t *cr,
                         SchematicWindow *w_current);
#else
schematic_canvas_redraw (SchematicCanvas *view,
                         GdkEventExpose *event,
                         SchematicWindow *w_current);
#endif

int
schematic_canvas_SCREENabs (SchematicCanvas *view,
                            int val);
void
schematic_canvas_SCREENtoWORLD (SchematicCanvas *view,
                                int mx,
                                int my,
                                int *x,
                                int *y);
void
schematic_canvas_set_hadjustment (SchematicCanvas *view,
                                  GtkAdjustment *hadjustment);
void
schematic_canvas_set_page (SchematicCanvas *view,
                           LeptonPage *page);
void
gschem_page_view_set_vadjustment (SchematicCanvas *view, GtkAdjustment *vadjustment);

void
gschem_page_view_set_show_hidden_text (SchematicCanvas *view,
                                       gboolean show_hidden_text);

int
gschem_page_view_WORLDabs(SchematicCanvas *view, int val);

void
gschem_page_view_WORLDtoSCREEN (SchematicCanvas *view, int x, int y, int *px, int *py);

void
gschem_page_view_zoom_extents (SchematicCanvas *view, const GList *list);

void
gschem_page_view_zoom_object (SchematicCanvas *view, LeptonObject *object);

#ifdef ENABLE_GTK3
GtkScrollablePolicy
gschem_page_view_get_hscroll_policy (SchematicCanvas *view);

GtkScrollablePolicy
gschem_page_view_get_vscroll_policy (SchematicCanvas *view);

void
gschem_page_view_set_hscroll_policy (SchematicCanvas *view, GtkScrollablePolicy scroll);

void
gschem_page_view_set_vscroll_policy (SchematicCanvas *view, GtkScrollablePolicy scroll);

void
schematic_page_view_grab_focus (SchematicCanvas *page_view);

#endif

G_END_DECLS
