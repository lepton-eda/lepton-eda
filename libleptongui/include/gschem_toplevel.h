/* Lepton EDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2016 gEDA Contributors
 * Copyright (C) 2017-2022 Lepton EDA Contributors
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

G_BEGIN_DECLS

/* Temporary cast until GschemToplevel becomes a class */
#define GSCHEM_TOPLEVEL(ptr) ((GschemToplevel*)(ptr))


struct st_gschem_toplevel {

  LeptonToplevel *toplevel;

  /* ------------------- */
  /* main window widgets */
  /* ------------------- */
  GtkWidget *main_window;

  GtkWidget *toolbar;
  GtkWidget *drawing_area; /* page view */
  GtkWidget *menubar;
  GtkWidget *popup_menu;

  /* Widgets at inside the bottom of the main view */
  GtkWidget *find_text_widget;
  GtkWidget *hide_text_widget;
  GtkWidget *show_text_widget;
  GtkWidget *macro_widget;
  GtkWidget *translate_widget;

  GtkWidget *bottom_widget;


  /* tabbed GUI: notebook: */
  GtkNotebook* xtabs_nbook;

  /* tabbed GUI: data structures: */
  GList* xtabs_info_list;


  GtkWidget *bottom_notebook;

  /* Widgets inside notebook at bottom of window */
  GtkWidget *find_text_state;
  GtkWidget *log_widget;

  GtkWidget *right_notebook;

  /* Widgets inside the notebook at the right of the window */
  GtkWidget *object_properties;
  GtkWidget *options_widget;
  GtkWidget *text_properties;

  /* color scheme editor widget: */
  GtkWidget *color_edit_widget;

  /* font selection widget: */
  GtkWidget *font_select_widget;

  /* page selection widget: */
  GtkWidget *page_select_widget;

  /* dialogs for widgets */
  GtkWidget *options_widget_dialog;
  GtkWidget *text_properties_dialog;
  GtkWidget *object_properties_dialog;
  GtkWidget *log_widget_dialog;
  GtkWidget *find_text_state_dialog;
  GtkWidget *color_edit_dialog;
  GtkWidget *font_select_dialog;
  GtkWidget *page_select_dialog;


  gchar *keyaccel_string;               /* visual feedback when pressing
                                           keyboard accelerators */
  guint keyaccel_string_source_id;      /* event source ID used by above */

  /* ------------ */
  /* Dialog boxes */
  /* ------------ */
  GtkWidget *cswindow;                  /* component select */
  GtkWidget *tiwindow;                  /* text input */
  GtkWidget *sewindow;                  /* slot edit */
  GtkWidget *aawindow;                  /* arc attribs */
  GtkWidget *mawindow;                  /* multi attribute */
  GtkWidget *aewindow;                  /* attribute edit */
  GtkWidget *hkwindow;                  /* Help/Hotkeys... dialog*/
  GtkWidget *cowindow;                  /* Coordinate window */
  GtkWidget *coord_world;                 /* World coordinate label */
  GtkWidget *coord_screen;                /* Screen coordinate window */

  /* -------------------------------------- */
  /* Models for widgets inside dialog boxes */
  /* -------------------------------------- */
  GtkListStore *dash_length_list_store;
  GtkListStore *dash_space_list_store;
  GtkListStore *fill_angle_list_store;
  GtkListStore *fill_pitch_list_store;
  GtkListStore *fill_width_list_store;
  GtkListStore *line_width_list_store;
  GtkListStore *text_size_list_store;

  /* ----------------------------------------- */
  /* An adapter for manipulating the selection */
  /* ----------------------------------------- */
  GschemSelectionAdapter *selection_adapter;

  /* --------------------------------- */
  /* Manager for "Open Recent..." menu */
  /* --------------------------------- */
  GtkRecentManager *recent_manager;

  /* ----------------- */
  /* Picture placement */
  /* ----------------- */
  GdkPixbuf *current_pixbuf;            /* used by add picture dialog */
  double pixbuf_wh_ratio;               /* width/height ratio of the pixbuf */
  char *pixbuf_filename;

  /* ------------- */
  /* Drawing state */
  /* ------------- */
  EdaRenderer *renderer;
  int first_wx;
  int first_wy;
  int second_wx;
  int second_wy;
  int third_wx;
  int third_wy;
  int magnetic_wx, magnetic_wy;         /* Position of the magnetic marker*/
  int distance;
  int inside_action;                    /* Are we doing an action? */
  int rubber_visible;                   /* Are there any rubber lines on
                                           the screen? */
  int net_direction;                    /* bit field to guess the best net direction */
  int which_grip;                       /* Which grip is being manipulated.
                                           Its range of values depends on the
                                           type of object being manipulated. */
  LeptonObject *which_object;           /* Object being manipulated */
  LeptonPath *temp_path;                /* Path being created */
  gboolean pathcontrol;                 /* Set path control point while path creating */ /* FIXME: can we do without it? */

  /* ------------------ */
  /* Rubberbanding nets */
  /* ------------------ */
  GList *stretch_list;

  /* --------------------- */
  /* Gschem internal state */
  /* --------------------- */
  int num_untitled;                     /* keep track of untitled wins */
  int action_mode;                      /* Current action mode */
  int min_zoom;                         /* minimum zoom factor */
  int max_zoom;                         /* maximum zoom factor */
  int drawbounding_action_mode;         /* outline vs bounding box */
  int last_drawb_mode;                  /* last above mode */
  int CONTROLKEY;                       /* control key pressed? */
  int SHIFTKEY;                         /* shift key pressed? */
  int ALTKEY;                           /* alt key pressed? */
  int buffer_number;                    /* current paste buffer in use */

  GList *clipboard_buffer;              /* buffer for system clipboard integration */

  /* ------------------ */
  /* rc/user parameters */
  /* ------------------ */
  GschemOptions *options;

  int text_caps;
  int text_size;

  int zoom_with_pan;

  int actionfeedback_mode;      /* can be either OUTLINE or BOUNDINGBOX */
  int net_direction_mode; /* controls if the net direction mode is used */
  int net_selection_mode;  /* controls the behaviour when selecting a net */
  int net_selection_state;  /* current status of the net selecting mode */
  int embed_component;    /* controls if component objects are embedded */
  int include_component;  /* controls if component objects are included */
  int scrollbars_flag;    /* controls if scrollbars are displayed */
  int third_button;       /* controls what the third mouse button does */
  int third_button_cancel;/* controls if the third mouse button cancels actions */
  int middle_button;      /* controls what the third mouse button does */
  int scroll_wheel;       /* controls what the mouse scroll wheel does */
  int file_preview;       /* controls if the preview area is enabled or not */
  int enforce_hierarchy;  /* controls how much freedom user has when traversing the hierarchy */
  int fast_mousepan;      /* controls if text is completely drawn during mouse pan */

  /* controls if after doing a place the same component can be placed again */
  int continue_component_place;

  int undo_levels;        /* number of undo levels stored on disk */
  int undo_control;       /* sets if undo is enabled or not */
  int undo_type;          /* type of undo (disk/memory) */
  int undo_panzoom;       /* sets if pan / zoom info is saved in undo */
  gboolean draw_grips;    /* sets if grips are enabled or not */

  int warp_cursor;        /* warp the cursor when zooming */
  int toolbars;           /* sets if the toolbar(s) are enabled or disabled */
  int handleboxes;        /* sets if the handleboxes are enabled or disabled */
  int bus_ripper_size;    /* sets size of the bus rippers */
  int bus_ripper_type;    /* sets type of the bus ripper (component or net) */
  int bus_ripper_rotation;  /* sets if the the bus ripper is symmetric or not */

  int grid_mode;          /* sets the mode of the grid (no grid, dots or mesh) */
  /* sets the mininum number of pixels necessary for the grid to be */
  /* displayed */
  int dots_grid_fixed_threshold;
  int dots_grid_dot_size; /* sets the grid dot size */
  int dots_grid_mode;     /* sets the mode of the dotted grid (either variable or fixed) */

  /* Minimum grid line pitch to display. Applies to major and minor lines. */
  int mesh_grid_display_threshold;

  int mousepan_gain;      /* Controls the gain of the mouse pan */
  int keyboardpan_gain;   /* Controls the gain of the keyboard pan */
  int select_slack_pixels; /* Number of pixels around an object we can still select it with */
  int zoom_gain;          /* Percentage increase in size for a zoom-in operation */
  int scrollpan_steps;    /* Number of scroll pan events required to traverse the viewed area */

  gchar* bus_ripper_symname; /* default bus ripper symbol file name */

  gboolean dont_invalidate;
};


void
gschem_toplevel_free (GschemToplevel *w_current);

GschemPageView*
gschem_toplevel_get_current_page_view (GschemToplevel *w_current);

GtkListStore*
gschem_toplevel_get_dash_length_list_store (GschemToplevel *w_current);

GtkListStore*
gschem_toplevel_get_dash_space_list_store (GschemToplevel *w_current);

GtkListStore*
gschem_toplevel_get_fill_angle_list_store (GschemToplevel *w_current);

GtkListStore*
gschem_toplevel_get_fill_pitch_list_store (GschemToplevel *w_current);

GtkListStore*
gschem_toplevel_get_fill_width_list_store (GschemToplevel *w_current);

GtkListStore*
gschem_toplevel_get_line_width_list_store (GschemToplevel *w_current);

GschemSelectionAdapter*
gschem_toplevel_get_selection_adapter (GschemToplevel *w_current);

GtkListStore*
gschem_toplevel_get_text_size_list_store (GschemToplevel *w_current);

LeptonToplevel*
gschem_toplevel_get_toplevel (GschemToplevel *w_current);

GschemToplevel*
gschem_toplevel_new ();

void
gschem_toplevel_notify_page_callback (GschemPageView *page_view, GParamSpec *pspec, GschemToplevel *w_current);

void
gschem_toplevel_page_changed (GschemToplevel *w_current);

void
gschem_toplevel_set_toplevel (GschemToplevel *w_current,
                              LeptonToplevel *toplevel);
void
gschem_toplevel_page_content_changed (GschemToplevel *w_current,
                                      LeptonPage *page);
void
schematic_window_active_page_changed (GschemToplevel *w_current);

gboolean
gschem_toplevel_get_show_hidden_text (GschemToplevel *w_current);

LeptonPage*
schematic_window_get_active_page (GschemToplevel *w_current);

GdkDisplay*
schematic_window_get_gdk_display (GschemToplevel *w_current);

LeptonPageList*
schematic_window_get_pages (GschemToplevel *w_current);

GschemOptions*
schematic_window_get_options (GschemToplevel *w_current);

guint
schematic_window_add_timer (guint interval,
                            gpointer callback,
                            gpointer data);
void
schematic_window_destroy_timer (guint source_id);

SchematicActionMode
schematic_window_get_action_mode (GschemToplevel *w_current);

void
schematic_window_set_action_mode (GschemToplevel *w_current,
                                  SchematicActionMode mode);
void
schematic_window_set_toolbar (GschemToplevel *w_current,
                              GtkWidget *toolbar);
int
schematic_window_get_inside_action (GschemToplevel *w_current);

gboolean
schematic_window_get_draw_grips (GschemToplevel *w_current);

void
schematic_window_set_draw_grips (GschemToplevel *w_current,
                                 gboolean draw_grips);
int
schematic_window_get_actionfeedback_mode (GschemToplevel *w_current);

void
schematic_window_set_actionfeedback_mode (GschemToplevel *w_current,
                                          int actionfeedback_mode);
GList*
schematic_window_get_place_list (GschemToplevel *w_current);

GtkWidget*
schematic_window_get_page_select_widget (GschemToplevel *w_current);

void
schematic_window_set_page_select_widget (GschemToplevel *w_current,
                                         GtkWidget* widget);
LeptonSelection*
schematic_window_get_selection_list (GschemToplevel *w_current);

void
schematic_window_set_selection_list (GschemToplevel *w_current,
                                     LeptonSelection *place_list);
GtkWidget*
schematic_window_get_macro_widget (GschemToplevel *w_current);

int
schematic_window_get_shift_key_pressed (GschemToplevel *w_current);

void
schematic_window_set_shift_key_pressed (GschemToplevel *w_current,
                                        int state);
GtkWidget*
schematic_window_get_right_notebook (GschemToplevel *w_current);

void
schematic_window_set_right_notebook (GschemToplevel *w_current,
                                     GtkWidget *widget);
GtkWidget*
schematic_window_get_bottom_notebook (GschemToplevel *w_current);

void
schematic_window_set_bottom_notebook (GschemToplevel *w_current,
                                      GtkWidget *widget);
int
schematic_window_get_undo_panzoom (GschemToplevel *w_current);

void
schematic_window_set_undo_panzoom (GschemToplevel *w_current,
                                   int undo_panzoom);
int
schematic_window_get_keyboardpan_gain (GschemToplevel *w_current);

void
schematic_window_set_keyboardpan_gain (GschemToplevel *w_current,
                                       int keyboardpan_gain);
gboolean
schematic_window_get_dont_invalidate (GschemToplevel *w_current);

void
schematic_window_set_dont_invalidate (GschemToplevel *w_current,
                                      gboolean val);
int
schematic_window_get_enforce_hierarchy (GschemToplevel *w_current);

void
schematic_window_set_enforce_hierarchy (GschemToplevel *w_current,
                                        int enforce);
guint
schematic_window_get_keyaccel_string_source_id (GschemToplevel *w_current);

void
schematic_window_set_keyaccel_string_source_id (GschemToplevel *w_current,
                                                guint source_id);
const char*
schematic_window_get_keyaccel_string (GschemToplevel *w_current);

void
schematic_window_set_keyaccel_string (GschemToplevel *w_current,
                                      char *str);
GtkWidget*
schematic_window_get_compselect (GschemToplevel *w_current);

void
schematic_window_set_compselect (GschemToplevel *w_current,
                                 GtkWidget *widget);
G_END_DECLS
