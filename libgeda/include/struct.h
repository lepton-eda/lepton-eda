/* gEDA - GNU Electronic Design Automation
 * libgeda - include files
 * Copyright (C) 1998 Ales V. Hvezda
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

#include "pcb_struct.h"

/* gschem structures (gschem) */
typedef struct st_linepts LINEPTS;
typedef struct st_circle CIRCLE;
typedef struct st_attrib ATTRIB;
typedef struct st_object OBJECT;
typedef struct st_page PAGE;
typedef struct st_toplevel TOPLEVEL;
typedef struct st_color COLOR;
typedef struct st_filedialog FILEDIALOG;

/* rename to the real thing once things work right */
typedef struct st_conn CONN;

/* netlist structures (gnetlist) */
typedef struct st_netlist NETLIST;
typedef struct st_cpinlist CPINLIST;
typedef struct st_net NET;
typedef struct st_nethash NETHASH;

/* sym check structures (gsymcheck) */
typedef struct st_symcheck SYMCHECK;

/* sch check structures (gschcheck) */
typedef struct st_schcheck SCHCHECK;
typedef struct st_chkerrs CHKERRS;
 
/* gschem/gnetlist structure definitions */
struct st_linepts {
	int x1, y1;
	int snap_1;
	int x2, y2;
	int snap_2;
	int screen_x1, screen_y1;
	int screen_x2, screen_y2;
}; 

struct st_circle {
	int center_x, center_y;
	int radius;

	int screen_x, screen_y;
	int screen_left, screen_top;
	int screen_radius;

};


struct st_object {
	int type;				/* Basic information */
	int sid;
	char *name;

	int top;				/* Bounding box information */
	int left;				/* in screen coords */
	int right;
	int bottom;

	LINEPTS *line_points;		/* Describes a line */
	CIRCLE *circle;			/* Describes a circle */

	int visited;		/* used in gnetlist for travesal purposes */

	char *complex_basename;			/* Complex basename */
	char *complex_clib;			/* Complex Component Library */
	OBJECT *complex;		/* Complex pointer */
	OBJECT *complex_parent;		/* Complex parent object pointer */
					/* used only in complex head nodes */
	int x, y;		/* complex/text/arc origin */
				
	int screen_x, screen_y;	/* complex/text/arc screen origin */

	/* unused for now */
	void (*action_func)();			/* Execute function */

	void (*sel_func)();			/* Selected function */
	void (*draw_func)();			/* Draw function */

	int color; 				/* Which color */
	int saved_color; 			/* Saved color */
	
	int angle;				/* orientation, only multiples
					   	 * of 90 degrees allowed */   
						/* in degrees */
	int mirror;

	char *text_string;			/* text stuff */
	int text_size;
	int text_len;
	int displayed_text_len;

        int snap_size;                          /* snap grid size */

	ATTRIB *attribs;		/* attribute stuff */
	ATTRIB *attached_to;	  /* when object is an attribute */
	int attribute;
	int show_name_value;
	int visibility; 

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
	OBJECT *object;	/* object connected to */
	OBJECT *responsible;	/* object which caused this midpoint to be */
				/* created */
				/* Only used with type == CONN_MIDPOINT */

	int type; /* individual object type */
	int whole_type;	/* type of the entire list, either */
			/* HAS_MIDPOINT or NO_MIDPOINT */
	int visual_cue; /* this is only used when type == HEAD */
	int x, y;
	
	CONN *prev;
	CONN *next;
};


struct st_page {

	int pid;

	OBJECT *object_head;
	OBJECT *object_tail;
	OBJECT *object_parent;
	OBJECT *selection_head;
	OBJECT *selection_tail;
	OBJECT *complex_place_head;  /* used to place complex's and text */
	OBJECT *complex_place_tail; 
	OBJECT *attrib_place_head;
	OBJECT *attrib_place_tail; 
	OBJECT *object_lastplace;
	OBJECT *object_selected;

	char *page_filename; 
	int CHANGED;			/* changed flag */
	int zoom_factor;
	int left, right, top, bottom;		/* World coord limits */
	double coord_aspectratio;		/* Real worldcoords ratio (?) */
	int clist_row;				/* used in page manager */
						/* which row is the page in */

	float to_screen_x_constant;
	float to_screen_y_constant;

	float to_world_x_constant;
	float to_world_y_constant;

	GHashTable *conn_table;	/* used to maintain conn information */

	/* used to maintain net/midpoint information */
	/* used only in gnetlist */
	GHashTable *nethash_table;

	/* used to control which pages are viewable when moving around */
	int page_control;

	/* left to right movement */
	PAGE *prev;
	PAGE *next;

	/* up and down the hierarchy */
	/* this holds the pid of the parent page */
	int up;
	/* PAGE *down; not needed */
};

struct st_filedialog {
	GtkWidget *xfwindow;		

	int type;
	int filesel_type;

	GtkWidget *filter;
	int filter_type;

	GtkWidget *search_entry;
	GtkWidget *search_label;
	int last_search_lib;
	int last_search;		

	GtkWidget *filename_entry;

	GtkWidget *dir_list;
	GtkWidget *file_list;

	char *directory;
	char *filename;

	/* need to make this dynamic TODO ?? */
	char *directory_entries[MAX_DIRS];
	char *file_entries[MAX_FILES];

	TOPLEVEL *preview;
	GtkWidget *preview_checkbox;
	int preview_control;

	GtkWidget *component_pulldown;

	/* this points to the owner of this filedialog structure */
	/* should NEVER be freed */
	TOPLEVEL *toplevel;
}; 

struct st_toplevel {

	int wid;				/* Window id, always unique */

	int num_untitled;			/* keep track of untitled wins */
	
	int start_x;
	int start_y;
	int save_x;
	int save_y;
	int last_x;
	int last_y;
	int loc_x, loc_y;
	int distance;

	char *current_attribute;		/* used by attribute dialog */
						/* also used by text add 
						 * dialog 
						 */
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


	char *series_name;			/* Current series basename */
	char *untitled_name;			/* untitled sch basename */
	char *font_directory; 			/* path of the vector fonts */
	char *scheme_directory; 		/* path of the scheme scripts */
	
	int event_state;			/* Current event state */

	int inside_action;			/* Are we doing an action? */

	int init_left, init_right; 		/* Starting values for above */
	int init_top, init_bottom; 

	int win_width, win_height;		/* Actual size of window (?) */
	int width, height;			/* height, width of window */
	int image_width, image_height;		/* h, w of image write */
	int snap;				/* Snap on/off*/
	int grid;				/* Grid on/off*/
	int min_zoom;				/* minimum zoom factor */
	int max_zoom;				/* maximum zoom factor */
	int starting_width;			/* starting window width */
						/* used to control text */


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

	int drawbounding_action_mode; 		/* outline vs bounding box */
	int last_drawb_mode;			/* last above mode */

	int CONTROLKEY;				/* control key pressed? */
	int SHIFTKEY;				/* shift key pressed? */
	int ALTKEY;				/* alt key pressed? */

/* Page system used by gPCB */
	PAGE_T *current_page;

	/* page system */
	PAGE *page_head;	
	PAGE *page_tail;	
	PAGE *page_current;

	/* hierarchy system */

	void (*last_callback)();	  	/* Last i_call* cmd executed */
	char cwd[256]; /* size is hack */ 	/* current working directory */

	/* main window widgets */
	GtkWidget *main_window;
	GtkWidget *drawing_area;
	GtkWidget *popup_menu;
	GtkWidget *h_scrollbar;
	GtkWidget *v_scrollbar;    
	GtkObject *h_adjustment;
	GtkObject *v_adjustment;
	GtkWidget *left_label;
	GtkWidget *middle_label;
	GtkWidget *right_label;
	GtkWidget *filename_label;
	GtkWidget *status_label;

	GtkMenuFactory *factory;
	GtkMenuFactory *subfactory[2];
	GHashTable *entry_ht;

	/* Dialog boxes */
	GtkWidget *fowindow;			/* File open */
	GtkWidget *fswindow;			/* File save */
	GtkWidget *sowindow;			/* Script open */
	int saveas_flag;     			/* what action after save? */

	GtkWidget *aswindow;			/* Attribute select */
	GtkWidget *attr_list;
	GtkWidget *asentry_name;
	GtkWidget *asentry_value; 

	GtkWidget *cswindow;			/* component select */
	GtkWidget *clib_list;
	GtkWidget *basename_list;
	char current_clib[256]; /* hack */
	char current_basename[256]; 	


	FILEDIALOG fileselect[2];
						/* see define.h for what */
						/* each of the different */
						/* members of this array are */

	GtkWidget *pwindow;			/* printing dialog box */
	GtkWidget *plib_list;			/* paper size box */
	GtkWidget *pfilename_entry; 

	GtkWidget *iwindow;			/* image write dialog box */
	GtkWidget *ifilename_entry; 

	GtkWidget *pswindow;			/* page select */
	GtkWidget *page_clist;
	int clist_sig;				/* used only in page manager */

	/* misc dialogs */
	GtkWidget *tiwindow;			/* text input */
	GtkWidget *tientry;
	GtkWidget *tewindow;			/* text edit */
	GtkWidget *teentry;
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
	int logic_bubble_color; /* not used anywhere yet, but will be */
	int zoom_box_color; 
	int text_caps;
	int attribute_color;
	int detachedattr_color;
	int text_size;
	int snap_size;		/* used by math funcs for the snapping */
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
	int zoom_with_pan; 
	int actionfeedback_mode; /* can be either OUTLINE or BOUNDINGBOX */
	int text_feedback; /* controls if text is drawn or not in */
			   /* copy/move/place ops */
	int text_display_zoomfactor; /* zoom factor at which text is
				      * displayed completely */
	int net_endpoint_mode; /* can be either NONE, FILLEDBOX, EMPTYBOX, X */
	int net_midpoint_mode; /* can be either NONE or FILLED or EMPTY */
	int object_clipping; /* controls whether objects are clipped */
	int embed_complex; /* controls if complex objects are embedded */
	int include_complex; /* controls if complex objects are included */
	int text_output; /* controls how text is printed (vector / PS font) */ 
	int scrollbars_flag; /* controls if scrollbars are displayed */ 
	int print_orientation; /* either landscape or portrait */
	int image_color; /* either TRUE or FALSE (color or no color) */
	int print_color; /* either TRUE or FALSE (color or no color) */
	int print_color_background; /* color used color ouput for background */ 
	int stroke_color; /* color of the stroke points */
	int log_window; /* controls if the log windows mapped on startup */
	int log_window_type; /* controls if the log window is decorated or not */
	int third_button; /* controls what the third mouse button does */
	int middle_button; /* controls what the third mouse button does */
	int net_consolidate; /* controls if the net consolidation code is used */ 
	int file_preview; /* controls if the preview area is enabled or not */ 
	int enforce_hierarchy; /* controls how much freedom user has when */ 
                               /* traversing the hierarchy */

	int print_output_type;			/* either window or limits */

	int print_output_capstyle;		/* BUTT, ROUND, SQUARE caps */

	/* fixed init variables */
	int image_output_type;			/* either window or limits */

	/* landscape printing only */
	int paper_width, paper_height;

	/* gnetlist specific */
	int net_naming_priority;

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

	NETLIST *prev;
	NETLIST *next;
};


/* for every pin on a component */
struct st_cpinlist {
        int plid;

	char *pin_number;
        char *net_name;			/* this is resolved at very end */

        NET *nets;

        CPINLIST *prev;
        CPINLIST *next;
};

/* the net run connected to a pin */
struct st_net {

        int nid;

	int net_name_has_priority;
        char *net_name;

        char *connected_to; /* new to replace above */

        NET *prev;
        NET *next;
};

/* used to resolve midpoint connections */
struct st_nethash {
	OBJECT *object;	/* object connected to */

	int type; /* individual object type */

	NETHASH *prev;
	NETHASH *next;
};


/* gsymcheck structure */
struct st_symcheck {
        int graphical_symbol;
        int missing_device_attrib;
        char *device_attribute;
        int device_attribute_incorrect;
        int missing_pin_attrib;
        int missing_numslots_attrib;
        int unattached_attribs;
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


#endif
