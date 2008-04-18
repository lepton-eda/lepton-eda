typedef struct st_gschem_toplevel GSCHEM_TOPLEVEL;

struct st_gschem_toplevel {

  TOPLEVEL *toplevel;

  /* ------------------- */
  /* main window widgets */
  /* ------------------- */
  GtkWidget *main_window;

  GtkWidget *drawing_area;
  GtkWidget *menubar;
  GtkWidget *popup_menu;

  GtkWidget *h_scrollbar;
  GtkWidget *v_scrollbar;
  GtkObject *h_adjustment;
  GtkObject *v_adjustment;

  GtkWidget *left_label;
  GtkWidget *middle_label;
  GtkWidget *right_label;
  GtkWidget *grid_label;
  GtkWidget *status_label;

  GtkWidget *toolbar_select;
  GtkWidget *toolbar_net;
  GtkWidget *toolbar_bus;

  gchar *keyaccel_string;               /* visual feedback when pressing
                                           keyboard accelerators */

  /* ------------ */
  /* Dialog boxes */
  /* ------------ */
  GtkWidget *sowindow;                  /* Script open */
  GtkWidget *pfswindow;                 /* Picture File Selection window */
  GtkWidget *cswindow;                  /* component select */
  GtkWidget *iwindow;                   /* image write dialog box */
  GtkWidget *pswindow;                  /* page select */
  GtkWidget *tiwindow;                  /* text input */
  GtkWidget *tewindow;                  /* text edit */
  GtkWidget *sewindow;                  /* slot edit */
  GtkWidget *aawindow;                  /* arc attribs */
  GtkWidget *mawindow;                  /* multi attribute */
  GtkWidget *aewindow;                  /* attribute edit */
  GtkWidget *trwindow;                  /* translate */
  GtkWidget *tswindow;                  /* text size */
  GtkWidget *tshowwindow;               /* text show window */
  GtkWidget *thidewindow;               /* text hide window */
  GtkWidget *tfindwindow;               /* text find window */
  GtkWidget *abwindow;                  /* Help/About... dialog*/
  GtkWidget *hkwindow;                  /* Help/Hotkeys... dialog*/
  GtkWidget *clwindow;                  /* Color edit dialog */
  int edit_color;                         /* Used by the color edit dialog */
  GtkWidget *cowindow;                  /* Coordinate window */
  GtkWidget *coord_world;                 /* World coordinate label */
  GtkWidget *coord_screen;                /* Screen coordinate window */

  /* ----------------- */
  /* Picture placement */
  /* ----------------- */
  GdkPixbuf *current_pixbuf;            /* used by add picture dialog */
  double pixbuf_wh_ratio;               /* width/height ratio of the pixbuf */
  char *pixbuf_filename;


  /* ---------------- */
  /* graphics context */
  /* ---------------- */
  GdkGC *gc;
  GdkGC *xor_gc;
  GdkGC *outline_xor_gc;
  GdkGC *bounding_xor_gc;
  GdkGC *bus_gc;

  /* ---------------- */
  /* Drawing surfaces */
  /* ---------------- */
  GdkWindow *window;                    /* drawing_area's X drawable */
  GdkPixmap *backingstore;              /* backingstore pixmap */
  int win_width, win_height;            /* Actual size of window (?) */

  /* ------------- */
  /* Drawing state */
  /* ------------- */
  int first_wx;
  int first_wy;
  int second_wx;
  int second_wy;
  int third_wx;
  int third_wy;
  int magnetic_wx, magnetic_wy;         /* Position of the magnetic marker*/
  int distance;
  int inside_action;                    /* Are we doing an action? */
  int rotated_inside;                   /* Was the selection rotated
                                           inside an action? */
  int rubber_visible;                   /* Are there any rubber lines on
					   the screen? */
  int magnetic_visible;                 /* Is the magnetic marker visible */
  int net_direction;                    /* bit field to guess the best net direction */

  /* --------------------- */
  /* Gschem internal state */
  /* --------------------- */
  int num_untitled;                     /* keep track of untitled wins */
  int event_state;                      /* Current event state */
  int image_width, image_height;        /* h, w of image write */
  int grid;                             /* Grid on/off*/
  int min_zoom;                         /* minimum zoom factor */
  int max_zoom;                         /* maximum zoom factor */
  int text_alignment;                   /* current alignment of text */
  int inside_redraw;                    /* complex vs list redrawing */
  int drawbounding_action_mode;         /* outline vs bounding box */
  int last_drawb_mode;                  /* last above mode */
  int CONTROLKEY;                       /* control key pressed? */
  int SHIFTKEY;                         /* shift key pressed? */
  int ALTKEY;                           /* alt key pressed? */
  int doing_pan;                        /* mouse pan status flag */
  int buffer_number;                    /* current paste buffer in use */
  int complex_rotate;                   /* Rotation of an object being placed */
  void (*last_callback)();              /* Last i_call* cmd executed */

  /* ------------------ */
  /* rc/user parameters */
  /* ------------------ */
  int graphic_color;
  int net_color;
  int bus_color;
  int pin_color;
  int text_color;

  /* not used anywhere yet, but will be */
  int logic_bubble_color;
  int zoom_box_color;
  int grid_color;
  int select_color;
  int bb_color;
  int lock_color;
  int stroke_color;       /* color of the stroke points */

  int text_caps;
  int text_size;

  int zoom_with_pan;

  int actionfeedback_mode;      /* can be either OUTLINE or BOUNDINGBOX */
  int text_feedback;      /* is text is drawn or not in copy/move/place ops */
  int text_display_zoomfactor;  /* zoom factor at which text is displayed completely */
  int net_endpoint_mode;  /* can be either NONE, FILLEDBOX, EMPTYBOX, X */
  int net_midpoint_mode;  /* can be either NONE or FILLED or EMPTY */
  int net_direction_mode; /* controls if the net direction mode is used */
  int embed_complex;      /* controls if complex objects are embedded */
  int include_complex;    /* controls if complex objects are included */
  int scrollbars_flag;    /* controls if scrollbars are displayed */
  int log_window;         /* controls if the log windows mapped on startup */
  int log_window_type;    /* controls if the log window is decorated or not */
  int third_button;       /* controls what the third mouse button does */
  int middle_button;      /* controls what the third mouse button does */
  int scroll_wheel;       /* controls what the mouse scroll wheel does */
  int file_preview;       /* controls if the preview area is enabled or not */
  int enforce_hierarchy;  /* controls how much freedom user has when traversing the hierarchy */
  int text_origin_marker; /* controls if text origin marker is displayed or not */
  int fast_mousepan;      /* controls if text is completely drawn during mouse pan */
  int raise_dialog_boxes; /*controls if expose events raise dialog boxes*/

  /* controls if after doing a place the same component can be placed again */
  int continue_component_place;

  int undo_levels;        /* number of undo levels stored on disk */
  int undo_control;       /* sets if undo is enabled or not */
  int undo_type;          /* type of undo (disk/memory) */
  int undo_panzoom;       /* sets if pan / zoom info is saved in undo */
  int draw_grips;         /* sets if grips are enabled or not */

  /* sets whether nets rubberband as you move them (or connecting comps) */
  int netconn_rubberband;

  int sort_component_library; /* sort the component library */
  int warp_cursor;        /* warp the cursor when zooming */
  int toolbars;           /* sets if the toolbar(s) are enabled or disabled */
  int handleboxes;        /* sets if the handleboxes are enabled or disabled */
  int bus_ripper_size;    /* sets size of the bus rippers */
  int bus_ripper_type;    /* sets type of the bus ripper (component or net) */
  int bus_ripper_rotation;  /* sets if the the bus ripper is symmetric or not */
  int grid_dot_size;      /* sets the grid dot size */
  int grid_mode;      /* sets the mode of the grid (either variable or fixed) */
  int magneticnet_mode; /* enables/disables the magnetic net mode ON/OFF */

  /* sets the mininum number of pixels necessary for the grid to be */
  /* displayed */
  int grid_fixed_threshold;

  /* sets the offset (in world coordinates) that are added to netname */
  /* attributes when they are attached to vertical or horizontal nets */
  int add_attribute_offset;

  int drag_can_move;      /* Controls if drag can move objects or not */
  int mousepan_gain;      /* Controls the gain of the mouse pan */
  int keyboardpan_gain;   /* Controls the gain of the keyboard pan */
  int select_slack_pixels; /* Number of pixels around an object we can still select it with */

};

