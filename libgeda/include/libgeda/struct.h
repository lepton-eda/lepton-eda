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

/* Wrappers around a new list mechanism */
typedef struct _GedaList SELECTION;
typedef struct _GedaList GedaPageList;

/* gschem structures (gschem) */
typedef struct st_complex COMPLEX;
typedef struct st_line LINE;
typedef struct st_path_section PATH_SECTION;
typedef struct st_path PATH;
typedef struct st_circle CIRCLE;
typedef struct st_arc ARC;
typedef struct st_box BOX;
typedef struct st_picture PICTURE;
typedef struct st_text TEXT;
typedef struct st_point sPOINT;
typedef struct st_transform TRANSFORM;
typedef struct st_bezier BEZIER;

typedef struct st_object OBJECT;
typedef struct st_page PAGE;
typedef struct st_toplevel TOPLEVEL;
typedef struct st_color COLOR;
typedef struct st_undo UNDO;
typedef struct st_tile TILE;
typedef struct st_bounds BOUNDS;

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

/* Managed text buffers */
typedef struct _TextBuffer TextBuffer;

/* Component library objects */
typedef struct _CLibSource CLibSource;
typedef struct _CLibSymbol CLibSymbol;

/* Component library search modes */
typedef enum { CLIB_EXACT=0, CLIB_GLOB } CLibSearchMode;

/* f_open behaviour flags.  See documentation for f_open_flags() in
   f_basic.c. */
typedef enum { F_OPEN_RC           = 1,
               F_OPEN_CHECK_BACKUP = 2,
               F_OPEN_RESTORE_CWD  = 4,
} FOpenFlags;

/*! \brief line end style for an open line of an object */
typedef enum {END_NONE, END_SQUARE, END_ROUND} OBJECT_END;

/*! \brief line style of lines, rect, circles, arcs */
typedef enum {TYPE_SOLID, TYPE_DOTTED, TYPE_DASHED, TYPE_CENTER, TYPE_PHANTOM, TYPE_ERASE} OBJECT_TYPE;

/*! \brief fill style of objects like cirle, rect, path */
typedef enum {FILLING_HOLLOW, FILLING_FILL, FILLING_MESH, FILLING_HATCH, FILLING_VOID} OBJECT_FILLING;

struct st_line {
  int x[2];
  int y[2];
};

struct st_point {
  gint x;
  gint y;
};

#define LINE_END1 0
#define LINE_END2 1

typedef enum {
    PATH_MOVETO,
    PATH_MOVETO_OPEN,
    PATH_CURVETO,
    PATH_LINETO,
    PATH_END
} PATH_CODE;

struct st_path_section {
  PATH_CODE code;
  int x1;
  int y1;
  int x2;
  int y2;
  int x3;
  int y3;
};

struct st_path {
  PATH_SECTION *sections; /* Bezier path segments  */
  int num_sections;       /* Number with data      */
  int num_sections_max;   /* Number allocated      */
};

struct st_arc {
  int x, y; /* world */

  int width;
  int height;

  int start_angle;
  int end_angle;
};

#define ARC_CENTER 0
#define ARC_RADIUS 1
#define ARC_START_ANGLE 2
#define ARC_END_ANGLE 3

struct st_bezier {
  int x[4];
  int y[4];
};

struct st_box {
  /* upper is considered the origin */
  int upper_x, upper_y; /* world */	
  int lower_x, lower_y;

};

#define BOX_UPPER_LEFT 0
#define BOX_LOWER_RIGHT 1
#define BOX_UPPER_RIGHT 2
#define BOX_LOWER_LEFT 3

struct st_picture {
  GdkPixbuf *original_picture;
  GdkPixbuf *displayed_picture;
  gchar *file_content;
  gsize file_length;

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
  char *disp_string;
  int length;
  int size;
  int alignment;	
  int displayed_width;
  int displayed_height;
  int angle;

  GList *prim_objs;
};

struct st_complex {
  int x, y;		/* world origin */

  int angle;				/* orientation, only multiples
                                         * of 90 degrees allowed */   
  /* in degrees */
  int mirror;

  GList *prim_objs;			/* Primitive objects */
  /* objects which make up the */
  /* complex */
};

struct st_circle {
  int center_x, center_y; /* world */
  int radius;
};

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
  gboolean w_bounds_valid;

  COMPLEX *complex;
  LINE *line; 
  CIRCLE *circle; 
  ARC *arc;
  BOX *box;
  TEXT *text;
  PICTURE *picture;
  PATH *path;

  GList *tiles;			/* tiles */

  GList *conn_list;			/* List of connections */
  /* to and from this object */

  /* every graphical primitive have more or less the same options. */
  /* depending on its nature a primitive is concerned with one or more */
  /* of these fields. If not, value must be ignored. */
  OBJECT_END line_end;
  OBJECT_TYPE line_type;
  int line_width;
  int line_space;
  int line_length;

  OBJECT_FILLING fill_type;
  int fill_width;
  int fill_angle1, fill_pitch1;
  int fill_angle2, fill_pitch2;

  gboolean complex_embedded;                    /* is embedded component? */
  gchar *complex_basename;              /* Component Library Symbol name */
  OBJECT *parent;                       /* Parent object pointer */

  /* unused for now */
  void (*action_func)();			/* Execute function */

  void (*sel_func)();			/* Selected function */
  void (*draw_func)();			/* Draw function */

  int color; 				/* Which color */
  int dont_redraw;			/* Flag to skip redrawing */
  int selected;				/* object selected flag */
  int locked_color; 			/* Locked color (used to save */
  /* the object's real color */
  /* when the object is locked) */

  /* controls which direction bus rippers go */
  /* it is either 0 for un-inited, */
  /* 1 for right, -1 for left (horizontal bus) */
  /* 1 for up, -1 for down (vertial bus) */
  int bus_ripper_direction;             /* only valid on buses */


  int font_text_size;			/* used only with fonts defs */
  GList *font_prim_objs;			/* used only with fonts defs */

  int whichend;    /* for pins only, either 0 or 1 */
  int pin_type;    /* for pins only, either NET or BUS */

  GList *attribs;       /* attribute stuff */
  int show_name_value;
  int visibility;
  OBJECT *attached_to;  /* when object is an attribute */
  OBJECT *copied_to;    /* used when copying attributes */

}; 


/*! \brief Structure for connections between OBJECTs
 *
 * The st_conn structure contains a single connection
 * to another object.
 * The connection system in s_conn.c uses this struct
 */
struct st_conn {
  /*! \brief The "other" object connected to this one */
  OBJECT *other_object;
  /*! \brief type of connection. Always in reference to how the "other"
    object is connected to the current one */
  int type;
  /*! \brief x coord of the connection position */
  int x;
  /*! \brief y coord of the connection position */
  int y;		
  /*! \brief which endpoint of the current object caused this connection */
  int whichone;
  /*! \brief which endpoint of the "other" object caused this connection */
  int other_whichone;
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
};

struct st_bounds {
  gint min_x;
  gint min_y;
  gint max_x;
  gint max_y;
};

/** A structure to store a 2D affine transform.
 *
 *  The transforms get stored in a 3x3 matrix. Code assumes the bottom row to
 *  remain constant at [0 0 1].
 */
struct st_transform {
  gdouble m[2][3];    /* m[row][column] */
};

struct st_undo {

  /* one of these is used, depending on if you are doing in-memory */
  /* or file based undo state saving */	
  char *filename;
  GList *object_list;

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


/*! \brief structure to split a page into tiles
 *  
 *  This structure is used to track objects that are inside
 *  a smaller TILE of o a page.
 *  See s_tile.c for further informations.
 */
struct st_tile {
  GList *objects;

  int top, left, right, bottom;
};

struct st_page {

  int pid;

  GList *_object_list;
  SELECTION *selection_list; /* new selection mechanism */
  GList *place_list;
  OBJECT *object_lastplace; /* the last found item */
  GList *stretch_list;

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

};

/*! \brief different kind of snapping mechanisms used in TOPLEVEL */
typedef enum {SNAP_OFF, SNAP_GRID, SNAP_RESNAP, SNAP_STATE_COUNT} SNAP_STATE;

/*! \brief Type of callback function for calculating text bounds */
typedef int(*RenderedBoundsFunc)(void *, OBJECT *, int *, int *, int *, int *);

/*! \brief Type of callback function for querying loading of backups */
typedef gboolean(*LoadBackupQueryFunc)(void *, GString *);

struct st_toplevel {

  /* have to decided on component list stuff */
  /* if it should go in here or not */
  /* leave outside for now */

  GList *RC_list;                       /* List of RC files which have been read in. */

  char *untitled_name;			/* untitled sch basename */
  char *font_directory; 		/* path of the vector fonts */
  char *scheme_directory; 		/* path of the scheme scripts */
  char *bitmap_directory; 		/* path of the bitmaps */

  int init_left, init_right; 		/* Starting values for above */
  int init_top, init_bottom; 

  int width, height;			/* height, width of window */

  /*! \brief whether and how to snap to the current grid */
  SNAP_STATE snap;

  int override_color;			/* used in doing selections */

  int last_ps_color;                    /* used in print code */

  int DONT_REDRAW;			/* misc flags */
  int ADDING_SEL;

  /* page system */
  PAGE *page_current;
  GedaPageList *pages;

  /* show_hidden_text is used to control which text is hidden in gschem */
  int show_hidden_text;

  GList* major_changed_refdes;          /* A list of all refdes's that have */
                                        /* major symbol version changes */

  /* backup variables */
  int auto_save_interval;
  gint auto_save_timeout;

  /* used by math funcs for the snapping */
  int snap_size;		

  /* BLOCK SET IN GSCHEM, BUT USED IN LIBGEDA - NEEDS A RETHINK */
  int background_color;
  int override_net_color;
  int override_bus_color;
  int override_pin_color;
  int pin_style;
  int net_style;
  int bus_style;
  int line_style;
  /* END BLOCK - ALTHOUGH THERE ARE MORE CASES! */

  /* controls whether objects are clipped */
  int object_clipping; 

  /* controls how text is printed (vector / PS font) */ 
  int text_output; 

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

  /* controls if the net consolidation code is used */ 
  int net_consolidate; 

  /*controls if attribute promotion happens */
  int attribute_promotion; 

  /* controls if invisible attribs are promoted */
  int promote_invisible; 

  /* controls if invisible attribs are kept and not deleted */
  int keep_invisible;   

  /* either window or limits */
  int print_output_type;

  /* BUTT, ROUND, SQUARE caps */
  int print_output_capstyle;		

  /* landscape printing only */
  int paper_width, paper_height;

  /* filename of the bus ripper component if set above */
  char *bus_ripper_symname;

  /* controls if the whole bounding box is used in the auto whichend code */
  int force_boundingbox;

  /* controls the threshold (in lines of text) when the multi-line text */
  /* output font is forced to vector */
  int print_vector_threshold;

  /* List of attributes to always promote */
  GList *always_promote_attributes;

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
  char *unnamed_busname;

  /* Callback function for calculating text bounds */
  RenderedBoundsFunc rendered_text_bounds_func;
  void *rendered_text_bounds_data;

  /* Callback function for deciding whether to load a backup file. */
  LoadBackupQueryFunc load_newer_backup_func;
  void *load_newer_backup_data;
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
  int type;                             /* PIN_TYPE_NET or PIN_TYPE_BUS */

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
  guint8 r, g, b, a;
  gboolean enabled;
};


struct st_attrib_smob {
  TOPLEVEL *world;   /* We need this when updating schematic */
  OBJECT   *attribute;
};

struct st_object_smob {
  TOPLEVEL *world;   /* We need this when updating schematic */
  OBJECT   *object;
};

struct st_page_smob {
  TOPLEVEL *world;   /* We need this when updating schematic */
  PAGE   *page;
};

/* used by the rc loading mechanisms */
typedef struct {
  int   m_val;
  char *m_str;
} vstbl_entry;

#endif
