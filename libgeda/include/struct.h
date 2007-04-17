/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's Library
 * Copyright (C) 1998-2004 Ales V. Hvezda
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#ifndef STRUCT_H
#define STRUCT_H

#include <glib.h>  /* Include needed to make GList work. */

/* gschem structures (gschem) */
typedef struct st_complex COMPLEX;
typedef struct st_line LINE;
typedef struct st_circle CIRCLE;
typedef struct st_arc ARC;
typedef struct st_box BOX;
typedef struct st_picture PICTURE;
typedef struct st_text TEXT;

typedef struct st_attrib ATTRIB;
typedef struct st_object OBJECT;
typedef struct st_page PAGE;
typedef struct st_toplevel TOPLEVEL;
typedef struct st_color COLOR;
typedef struct st_undo UNDO;
typedef struct st_tile TILE;
typedef struct st_tile_loc TILE_LOC;

typedef struct st_conn CONN;
typedef struct st_bus_ripper BUS_RIPPER;

/* Used when you move objects and you want the nets/pins to stretch */
typedef struct st_stretch STRETCH;

/* netlist structures (gnetlist) */
typedef struct st_netlist NETLIST;
typedef struct st_cpinlist CPINLIST;
typedef struct st_net NET;

/* sch check structures (gschcheck) */
typedef struct st_schcheck SCHCHECK;
typedef struct st_chkerrs CHKERRS;


/* PB : change begin */
/* PB : these enum are constant to define :
   - the end of open line of an object ;
   - the type of the line of an object ;
   - the filling of a closed object. */
/* PB : used in struct st_object (predefined type OBJECT)*/
typedef enum {END_NONE, END_SQUARE, END_ROUND} OBJECT_END;
typedef enum {TYPE_SOLID, TYPE_DOTTED, TYPE_DASHED, TYPE_CENTER, TYPE_PHANTOM, TYPE_ERASE} OBJECT_TYPE;
typedef enum {FILLING_HOLLOW, FILLING_FILL, FILLING_MESH, FILLING_HATCH, FILLING_VOID} OBJECT_FILLING;
/* PB : change end */


struct st_line {
  int x[2];
  int y[2];

};

/* pb20011014 - name the grips */
#define LINE_END1 0
#define LINE_END2 1

struct st_arc {
  int x, y; /* world */

  int width;
  int height;

  int start_angle;
  int end_angle;
};
/* pb20011014 - name the grips */
#define ARC_CENTER 0
#define ARC_RADIUS 1
#define ARC_START_ANGLE 2
#define ARC_END_ANGLE 3

struct st_box {
  /* upper is considered the origin */
  int upper_x, upper_y; /* world */	
  int lower_x, lower_y;

};
/* pb20011014 - name the grips */
#define BOX_UPPER_LEFT 0
#define BOX_LOWER_RIGHT 1
#define BOX_UPPER_RIGHT 2
#define BOX_LOWER_LEFT 3

struct st_picture {
  GdkPixbuf *original_picture;
  GdkPixbuf *displayed_picture;

  double ratio;
  char *filename;
  int angle;
  char mirrored;
  char embedded;

  /* upper is considered the origin */
  int upper_x, upper_y; /* world */	
  int lower_x, lower_y;

};

#define PICTURE_UPPER_LEFT 0
#define PICTURE_LOWER_RIGHT 1
#define PICTURE_UPPER_RIGHT 2
#define PICTURE_LOWER_LEFT 3


struct st_text {
  int x, y;		/* world origin */

  char *string;			/* text stuff */
  int length;
  int size;
  int alignment;	
  int displayed_width;
  int displayed_height;
  int angle;

  OBJECT *prim_objs;
};

struct st_complex {
  int x, y;		/* world origin */

  int angle;				/* orientation, only multiples
                                         * of 90 degrees allowed */   
  /* in degrees */
  int mirror;

  OBJECT *prim_objs;			/* Primitive objects */
  /* objects which make up the */
  /* complex */
};

struct st_circle {
  int center_x, center_y; /* world */
  int radius;

/* pb20011010 - removed : used only in o_circle_draw_xor() and
   meaning unclear */
};
/* pb20011014 - name the grips */
#define CIRCLE_CENTER 0
#define CIRCLE_RADIUS 1

struct st_object {
  int type;				/* Basic information */
  int sid;
  char *name;

  int w_top;				/* Bounding box information */
  int w_left;				/* in world coords */
  int w_right;
  int w_bottom;

  COMPLEX *complex;
  LINE *line; 
  CIRCLE *circle; 
  ARC *arc;
  BOX *box;
  TEXT *text;
  PICTURE *picture;

  GList *tile_locs;			/* tile locations */

  GList *conn_list;			/* List of connections */
  /* to and from this object */

  /* PB : change begin */
  /* PB : every graphical primitive have more or less the same options. */
  /* PB : depending on its nature a primitive is concerned with one or more */
  /* PB : of these fields. If not, value must be ignored. */
  OBJECT_END line_end;
  OBJECT_TYPE line_type;
  int line_width;
  int line_space;
  int line_length;

  OBJECT_FILLING fill_type;
  int fill_width;
  int fill_angle1, fill_pitch1;
  int fill_angle2, fill_pitch2;
  /* PB : change end */	
	
  int visited;		/* used in gnetlist for travesal purposes */

  char *complex_basename;			/* Complex basename */
  char *complex_clib;			/* Complex Component Library */
  OBJECT *complex_parent;		/* Complex parent object pointer */
  /* used only in complex head nodes */

  /* unused for now */
  void (*action_func)();			/* Execute function */

  void (*sel_func)();			/* Selected function */
  void (*draw_func)();			/* Draw function */

  int color; 				/* Which color */
  int saved_color; 			/* Saved color */
  int selected;				/* object selected flag */
  int locked_color; 			/* Locked color (used to save */
  /* the object's real color */
  /* when the object is locked) */

  int draw_grips;				/* if selected, enables 
						   drawing of grips */

  /* controls which direction bus rippers go */
  /* it is either 0 for un-inited, */
  /* 1 for right, -1 for left (horizontal bus) */
  /* 1 for up, -1 for down (vertial bus) */
  int bus_ripper_direction;             /* only valid on buses */
  

  int font_text_size;			/* used only with fonts defs */
  OBJECT *font_prim_objs;			/* used only with fonts defs */

  ATTRIB *attribs;		/* attribute stuff */
  ATTRIB *attached_to;	  /* when object is an attribute */
  int attribute;
  int show_name_value;
  int visibility; 

  int whichend;    /* for pins only, either 0 or 1 */
  int pin_type;    /* for pins only, either NET or BUS */  

  OBJECT *prev;
  OBJECT *next;
}; 


struct st_attrib {
  OBJECT *object;	/* object attribute is connected to */
	
  OBJECT *copied_to; /* used when copying attributes */

  ATTRIB *prev;
  ATTRIB *next;
};

struct st_conn {
  OBJECT *other_object;	/* The "other" object connected to this one */
  int type;		/* Always in reference to how the "other" */
				/* object is connected to the current one */
  int x, y;		/* x, y coord of the connection */
  int whichone;		/* which endpoint of the current object */
				/* caused this connection */
  int other_whichone;	/* which endpoint of the "other" object */
				/* caused this connection */
};

/* this structure is used in gschem to add rippers when drawing nets */
/* it is never stored in any object, it is only temporary */
struct st_bus_ripper
{
  int x[2];
  int y[2];
};

struct st_stretch
{
  OBJECT *object;
  CONN *connection;

  int whichone;

  STRETCH *prev;
  STRETCH *next;
};

struct st_undo {

  /* one of these is used, depending on if you are doing in-memory */
  /* or file based undo state saving */	
  char *filename;
  OBJECT *object_head;

  /* either UNDO_ALL or UNDO_VIEWPORT_ONLY */
  int type;

  /* viewport information */
  int left, top, right, bottom;

  /* up and down the hierarchy */
  int up;
  /* used to control which pages are viewable when moving around */
  int page_control;

  UNDO *prev;
  UNDO *next;
};

struct st_tile {
  GList *objects;

  int top, left, right, bottom;
};

struct st_tile_loc {
  int i, j;	/* these are the indices into the tile structure */
};

struct st_page {

  int pid;

  OBJECT *object_head;
  OBJECT *object_tail;
  OBJECT *object_parent;
  GList *selection_list; /* new selection mechanism */
  GList *complex_place_list;
  OBJECT *attrib_place_head;
  OBJECT *attrib_place_tail; 
  OBJECT *object_lastplace;
  OBJECT *object_selected;
  STRETCH *stretch_head; 
  STRETCH *stretch_tail; 

  char *page_filename; 
  int CHANGED;			/* changed flag */
  /*int zoom_factor; no longer used*/
  int left, right, top, bottom;		/* World coord limits */
  double coord_aspectratio;		/* Real worldcoords ratio (?) */

  float to_screen_x_constant;
  float to_screen_y_constant;

  float to_world_x_constant;
  float to_world_y_constant;

  TILE world_tiles[MAX_TILES_X][MAX_TILES_Y];

  /* Undo/Redo Stacks and pointers */	
  /* needs to go into page mechanism actually */
  UNDO *undo_bottom;	
  UNDO *undo_current;
  UNDO *undo_tos; 	/* Top Of Stack */

  /* up and down the hierarchy */
  /* this holds the pid of the parent page */
  int up;
  /* int down; not needed */

  /* used to control which pages are viewable when moving around */
  int page_control;

  /* backup variables */
  GTimeVal last_load_or_save_time;
  char saved_since_first_loaded;
  gint ops_since_last_backup;
  gchar do_autosave_backup;

  /* Function which asks the user wether to load a newer backup file */
  int (*load_newer_backup_func)();

  /* left to right movement */
  PAGE *prev;
  PAGE *next;
};

struct st_toplevel {

  int wid;			/* Window id, always unique */

  int num_untitled;		/* keep track of untitled wins */
	
  int start_x;
  int start_y;
  int save_x;
  int save_y;
  int last_x;
  int last_y;
  int second_x;
  int second_y;
  int loc_x, loc_y;
  int distance;
  
  /* used by attribute dialog */
  /* also used by text add dialog */
  char *current_attribute;		


  /* used by add picture dialog */
  GdkPixbuf *current_pixbuf;

  double pixbuf_wh_ratio;                  /* width/height ratio of the pixbuf */
  char *pixbuf_filename;

  int current_visible;			/* in o_attrib.c */
  int current_show;
  /* have to decided on attribute list stuff */
  /* if it should go in here or not */
  /* leave outside for now */

  char *internal_basename;		
  char *internal_clib;     
  /* have to decided on component list stuff */
  /* if it should go in here or not */
  /* leave outside for now */

  GList *RC_list;                       /* List of RC files which have been read in. */

  char *series_name;			/* Current series basename */
  char *untitled_name;			/* untitled sch basename */
  char *font_directory; 		/* path of the vector fonts */
  char *scheme_directory; 		/* path of the scheme scripts */
  char *bitmap_directory; 		/* path of the bitmaps */
	
  int event_state;			/* Current event state */

  int inside_action;			/* Are we doing an action? */
  int rotated_inside;                   /* Was the selection rotated 
                                           inside an action? */

  int init_left, init_right; 		/* Starting values for above */
  int init_top, init_bottom; 

  int win_width, win_height;		/* Actual size of window (?) */
  int width, height;			/* height, width of window */
  int image_width, image_height;		/* h, w of image write */
  int snap;				/* Snap on/off*/
  int grid;				/* Grid on/off*/
  int min_zoom;				/* minimum zoom factor */
  int max_zoom;				/* maximum zoom factor */

  /* starting window width used to control text */
  int starting_width;			

  /* location to hold current alignment of text */
  int text_alignment;

  /* location to hold current line type selection */
  int line_type;			

  /* location to hold current fill type selection (PB) */
  int fill_type;	

  int override_color;			/* used in doing selections */
  int inside_redraw;			/* complex vs list redrawing */
  double window_aspectratio;		/* Window ratio (?) */
  int display_height;			/* display params */
  int display_width;			/* could me made global (?) */

  int DONT_DRAW_CONN;			/* misc flags */
  int DONT_RESIZE;
  int DONT_EXPOSE;
  int DONT_REDRAW;
  int DONT_RECALC;
  int FORCE_CONN_UPDATE;
  int ADDING_SEL;
  int REMOVING_SEL;

  int drawbounding_action_mode; 	/* outline vs bounding box */
  int last_drawb_mode;			/* last above mode */

  int CONTROLKEY;			/* control key pressed? */
  int SHIFTKEY;				/* shift key pressed? */
  int ALTKEY;				/* alt key pressed? */
	
  int doing_pan;			/* mouse pan status flag */

  /* page system */
  PAGE *page_head;	
  PAGE *page_tail;	
  PAGE *page_current;

  /* buffer_number is used by the buffer copy/cut/paste mechanism */
  /* in gschem to keep track of the current buffer number */
  int buffer_number;

  /* show_hidden_text is used to control which text is hidden in gschem */
  int show_hidden_text;

  /* Variable to keep track of what value the complex is at */
  int complex_rotate;	

  void (*last_callback)();	  	/* Last i_call* cmd executed */
  char *cwd; /* current working directory */

  GList* major_changed_refdes;          /* A list of all refdes's that have */
                                        /* major symbol version changes */

  /* main window widgets */
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
  GtkWidget *filename_label;
  GtkWidget *grid_label;
  GtkWidget *status_label;

  GtkWidget *toolbar_select;
  GtkWidget *toolbar_net;
  GtkWidget *toolbar_bus;
  GtkWidget *toolbar_edit;
  GtkWidget *toolbar_move;
  GtkWidget *toolbar_copy;
  GtkWidget *toolbar_delete;
  GtkWidget *toolbar_rotate;
  GtkWidget *toolbar_mirror;

  /* Dialog boxes */
  GtkWidget *fowindow;			/* File open */
  GtkWidget *fswindow;			/* File save */
  GtkWidget *sowindow;			/* Script open */
  GtkWidget *pfswindow;                 /* Picture File Selection window */
  GtkWidget *pcfswindow;                /* Picture Change File Selection window */
  int saveas_flag;     			/* what action after save? */

  GtkWidget *aswindow;			/* Attribute select */
  GtkWidget *attr_list;
  GtkWidget *asentry_name;
  GtkWidget *asentry_value; 

  GtkWidget *cswindow;			/* component select */
  GtkWidget *clib_list;
  GtkWidget *basename_list;
  char *current_clib;
  char current_basename[256]; 	


  GtkWidget *iwindow;			/* image write dialog box */
  GtkWidget *ifilename_entry; 

  GtkWidget *pswindow;			/* page select */

  /* misc dialogs */
  GtkWidget *tiwindow;			/* text input */
  GtkWidget *tewindow;			/* text edit */
  GtkWidget *teentry;
  GtkWidget *ltwindow;			/* line type / width edit */
  GtkWidget *ftwindow;			/* fill type edit (PB) */
  GtkWidget *sewindow;			/* slot edit */
  GtkWidget *seentry;
  GtkWidget *exwindow;			/* exit confirm */
  GtkWidget *aawindow;			/* arc attribs */
  GtkWidget *mawindow;			/* multi attribute */
  GtkWidget *aewindow;			/* attribute edit */
  GtkWidget *aaentry_start;
  GtkWidget *aaentry_sweep;  
  GtkWidget *trwindow;			/* translate */
  GtkWidget *trentry;
  GtkWidget *tswindow;			/* text size */
  GtkWidget *tshowwindow;               /* text show window */
  GtkWidget *thidewindow;               /* text hide window */    
  GtkWidget *tfindwindow;               /* text find window */
  GtkWidget *tsentry;			/* used in edit/edit and */
  					/* Text size and the snap */
  					/* size dialog boxes */
	
  GtkWidget *abwindow;			/* Help/About... dialog*/
  GtkWidget *hkwindow;			/* Help/Hotkeys... dialog*/
  GtkWidget *cowindow;
  GtkWidget *coord_world;
  GtkWidget *coord_screen;

  GtkWidget *clwindow;
  int edit_color;

  /* this is the drawing_area's X drawable */
  GdkWindow *window; 
	
  /* graphics context stuff */
  GdkGC *gc;
  GdkGC *xor_gc;
  GdkGC *outline_xor_gc;
  GdkGC *bounding_xor_gc;
  GdkGC *bus_gc;

  /* backingstore pixmap */
  GdkPixmap *backingstore; 

  /* rc/user parameters */
  int graphic_color;
  int pin_color;
  int text_color;

  /* backup variables */
  int auto_save_interval;
  gint auto_save_timeout;


  /* not used anywhere yet, but will be */
  int logic_bubble_color; 
  int zoom_box_color; 
  int text_caps;
  int attribute_color;
  int detachedattr_color;
  int text_size;

  /* used by math funcs for the snapping */
  int snap_size;		

  int grid_color;
  int background_color;
  int select_color;
  int bb_color;
  int lock_color;
  int net_endpoint_color;
  int net_color;
  int bus_color;
  int override_net_color;
  int override_bus_color;
  int override_pin_color;
  int pin_style;
  int net_style;
  int bus_style;
  int line_style;
  int zoom_with_pan; 

  /* can be either OUTLINE or BOUNDINGBOX */
  int actionfeedback_mode; 

  /* controls if text is drawn or not in copy/move/place ops */
  int text_feedback; 

  /* zoom factor at which text is displayed completely */
  int text_display_zoomfactor; 

  /* can be either NONE, FILLEDBOX, EMPTYBOX, X */
  int net_endpoint_mode; 

  /* can be either NONE or FILLED or EMPTY */
  int net_midpoint_mode; 

  /* controls whether objects are clipped */
  int object_clipping; 

  /* controls if complex objects are embedded */
  int embed_complex; 

  /* controls if complex objects are included */
  int include_complex; 

  /* controls how text is printed (vector / PS font) */ 
  int text_output; 

  /* controls if scrollbars are displayed */ 
  int scrollbars_flag; 

  /* either landscape or portrait */
  int print_orientation; 

  /* either TRUE or FALSE (color or no color) */
  int image_color; 

  /* either TRUE or FALSE (color or no color) */
  int print_color; 

  /* color used color ouput for background */ 
  int print_color_background;

  /* setpagedevice orientation option enable (TRUE or FALSE) */
  int setpagedevice_orientation;

  /* setpagedevice pagesize option enable (TRUE or FALSE) */
  int setpagedevice_pagesize;

  /* The name of the prolog file to paste into the Postscript output */
  char *postscript_prolog;

  /* Use this as a scaling factor for the output font */
  float postscript_font_scale;

  /* color of the stroke points */
  int stroke_color; 

  /* controls if the log windows mapped on startup */
  int log_window; 

  /* controls if the log window is decorated or not */
  int log_window_type; 

  /* controls what the third mouse button does */
  int third_button; 

  /* controls what the third mouse button does */
  int middle_button; 

  /* controls if the net consolidation code is used */ 
  int net_consolidate; 

  /* controls if the preview area is enabled or not */ 
  int file_preview; 

  /* controls how much freedom user has when traversing the hierarchy */
  int enforce_hierarchy; 

  /* controls if text origin marker is displayed or not */
  int text_origin_marker; 

  /* controls if text is completely drawn during mouse pan */
  int fast_mousepan;	

  /*controls if expose events raise dialog boxes*/
  int raise_dialog_boxes; 

  /*controls if attribute promotion happens */
  int attribute_promotion; 

  /* controls if invisible attribs are promoted */
  int promote_invisible; 

  /* controls if invisible attribs are kept and not deleted */
  int keep_invisible;   

  /* controls if after doing a place the */
  /* same component can be placed again */
  int continue_component_place; 

  /* Number of undo levels stored on disk */
  int undo_levels;	

  /* Controls if undo is enabled or not */
  int undo_control;	

  /* Type of undo (disk/memory) */
  int undo_type;	        

  /* Controls if grips are enabled or not */
  int draw_grips;	        

  /* controls if nets are rubberbanded as you move */
  /* them (or connecting comps) */
  int netconn_rubberband;	

  /* sort the component library */
  int sort_component_library;                    

  /* warp the cursor when zooming */
  int warp_cursor;                    

  /* controls if the toolbar(s) are enabled or disabled */
  int toolbars;                    

  /* controls if the handleboxes are enabled or disabled */
  int handleboxes;                    

  /* either window or limits */
  int print_output_type;

  /* BUTT, ROUND, SQUARE caps */
  int print_output_capstyle;		

  /* either window or limits */
  int image_output_type;			

  /* landscape printing only */
  int paper_width, paper_height;

  /* controls the size of the bus rippers */
  int bus_ripper_size;

  /* controls the type of the bus ripper (either component or net) */
  int bus_ripper_type;

  /* filename of the bus ripper component if set above */
  char *bus_ripper_symname;

  /* controls whether or not the the bus ripper is symmetric or not */
  int bus_ripper_rotation;

  /* controls if the whole bounding box is used in the auto whichend code */
  int force_boundingbox;

  /* controls the grid dot size */
  int grid_dot_size;

  /* controls the mode of the grid (either variable or fixed) */
  int grid_mode;

  /* controls the mininum number of pixels necessary for the grid to be */
  /* displayed */
  int grid_fixed_threshold;

  /* controls the threshold (in lines of text) when the multi-line text */
  /* output font is forced to vector */
  int print_vector_threshold;
  
  /* controls the offset (in world coordinates) that are added to netname */
  /* attributes when they are attached to vertical or horizontal nets */
  int add_attribute_offset;

  /* Controls if drag can move objects or not */
  int drag_can_move;	

  /* List of attributes to always promote */
  char *always_promote_attributes;

  /* Controls the gain of the mouse pan */
  int mousepan_gain;

  /* Controls the gain of the keyboard pan */
  int keyboardpan_gain;

  /* The command to send postscript to when printing */
  char *print_command;

  /* Number of pixels around an object we can still select it with */
  int select_slack_pixels;

  /* gnetlist specific */
  int net_naming_priority;
  int hierarchy_traversal;
  int hierarchy_uref_mangle;
  int hierarchy_netname_mangle;
  int hierarchy_netattrib_mangle;
  char *hierarchy_uref_separator;
  char *hierarchy_netname_separator;
  char *hierarchy_netattrib_separator;
  int hierarchy_netattrib_order;
  int hierarchy_netname_order;
  int hierarchy_uref_order;
  char *unnamed_netname;

  TOPLEVEL *next;
  TOPLEVEL *prev; 
};

/* structures below are for gnetlist */

/* for every component in the object database */
struct st_netlist {

  int nlid;

  char *component_uref;
	
  OBJECT *object_ptr;
	
  CPINLIST *cpins;		

  char *hierarchy_tag;
  int composite_component;

  NETLIST *prev;
  NETLIST *next;
};


/* for every pin on a component */
struct st_cpinlist {
  int plid;

  char *pin_number;
  char *net_name;			/* this is resolved at very end */
  char *pin_label;

  NET *nets;

  CPINLIST *prev;
  CPINLIST *next;
};

/* the net run connected to a pin */
struct st_net {

  int nid;

  int net_name_has_priority;
  char *net_name;
  char *pin_label;

  char *connected_to; /* new to replace above */

  NET *prev;
  NET *next;
};

/* By Jamil Khatib */
/* typedef struct st_chkerrs CHKERRS; */

/* Schem check struct */
struct st_schcheck {
  int no_errors;                /* No of Errors */
  int no_warnings;              /* No of Warinings */

  CHKERRS * sheet_errs;
 
  CHKERRS *float_nets;           /* Header of the list of floating nets */
  int net_errs;                 /* No of floating nets */

  OBJECT *float_pins;           /* Header of List of floating pins*/
  int pin_errs;                 /* No of floating pins */

  int net_names;                /* No of mismatched net names */
};


struct st_chkerrs{

  OBJECT * err_obj;
  CHKERRS * next;

};



struct st_color {
  char *color_name;
  char *outline_color_name;
  char *ps_color_string;
  int image_red, image_green, image_blue;

  GdkColor *gtk_color;
  GdkColor *gtk_outline_color;
  int image_color;
};


struct st_attrib_smob {
  TOPLEVEL *world;   /* We need this when updating schematic */
  ATTRIB   *attribute;
};

struct st_object_smob {
  TOPLEVEL *world;   /* We need this when updating schematic */
  OBJECT   *object;
};

struct st_page_smob {
  TOPLEVEL *world;   /* We need this when updating schematic */
  PAGE   *page;
};

#endif
