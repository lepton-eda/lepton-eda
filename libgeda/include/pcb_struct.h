/* gEDA - GNU Electronic Design Automation
 * pcb_struct - include file
 * Copyright (C) 1998 Ales V. Hvezda
 * Copyright (C) 2000 Patrick Bernaud
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

#ifndef PCB_STRUCT_H
#define PCB_STRUCT_H

typedef struct st_page_ewb PAGE_T;
typedef struct st_outline OUTLINE_T;
typedef struct st_box BOX_T;
typedef struct st_bounding_box BOUNDING_BOX_T;
typedef struct st_board_ewb BOARD_T;
typedef struct st_component_ewb COMPONENT_T;
typedef struct st_footprint_ewb FOOTPRINT_T;
typedef struct st_track_ewb TRACK_T;
typedef struct st_track_seg_ewb TRACK_SEG_T;
typedef struct st_net_ewb NET_T;
typedef struct st_via_ewb VIA_T;
typedef struct st_pad_ewb PAD_T;
typedef struct st_selection_ewb SELECTION_T;

typedef struct st_viadef_ewb VIADEF_T;
typedef struct st_paddef_ewb PADDEF_T;
typedef struct st_netdef_ewb NETDEF_T;

typedef char * COMP_REF;
typedef char * NET_REF;
typedef char * FOOTPRINT_ID;
typedef char * PAD_NAME;

/* Visiblity of an object */
typedef enum {O_INVISIBLE, O_VISIBLE, O_NULL} o_visibility;

/* Modifiers allowed for drawing objects*/
typedef enum {DRAW_NORMAL, DRAW_SELECTED, DRAW_CLEAR, DRAW_MOVING, DRAW_XOR} drawing_type;

/* Start with 10 layers */
typedef enum {LAYER_ALL=-1, LAYER_NONE,
			  LAYER_1, LAYER_2, LAYER_3, LAYER_4, LAYER_5,
			  LAYER_6, LAYER_7, LAYER_8, LAYER_9, LAYER_10,
			  OUTLINE_LAYER} layer_type;

/* Supported types of vias, should eventually include buried vias and such */
typedef enum {VIA_NULL, VIA_FULL} via_type;

/* Supported shape of vias */
typedef enum {VIA_NONE, VIA_CIRCLE, VIA_RECTANGLE, VIA_HEXAGON, VIA_ANNULUS} via_shape;

/* Supported types of pads, should eventually include surface mount types */
typedef enum {PAD_NULL, PAD_CIRCLE, PAD_RECTANGLE, PAD_HEXAGON, PAD_ANNULUS} pad_type;

/* Supported types of shape used as outline for components */
typedef enum {OUTLINE_NULL, OUTLINE_BOX} outline_type;

/* Supported objects currently selectable on a board */
typedef enum {SELECT_NULL, SELECT_COMP, SELECT_SEG, SELECT_VIA, SELECT_PAD} select_type;


/* Bounding box coordinates, in screen coords */
struct st_bounding_box {
	int top;	
	int left;	
	int right;
	int bottom;
};

struct st_selection_ewb {
	select_type type;

	union st_root_object {
		COMPONENT_T *comp;
		TRACK_T *track;
	} root;

	union st_selected_object {
		COMPONENT_T *comp;
		PAD_T *pad;
		TRACK_SEG_T *seg;
		VIA_T *via;
	} object;

	SELECTION_T *prev;
	SELECTION_T *next;
};

/* All objects that make up a given page are included in this structure */
/* Most members are the start of a linked list */
struct st_page_ewb {
	BOARD_T *board;
	COMPONENT_T *components;
	COMPONENT_T *last_comp;
	NET_T *nets;
	NET_T *last_net;
	TRACK_T *tracks;
	TRACK_T *last_track;
	FOOTPRINT_T *footprints;
	FOOTPRINT_T *last_foot;
	SELECTION_T *selections;
	SELECTION_T *last_selection;
	NET_T *routeNet;
	TRACK_T *routeTrack;
	TRACK_SEG_T *lastSeg;
	TRACK_SEG_T *routeSeg;
};

struct st_board_ewb {
	int x1, y1, x2, y2; /* Board location */
	
	int selected;
	
	BOUNDING_BOX_T bb; /* In screen coordinates */
};

/* Component information is held in this structure. */
struct st_component_ewb {
	int x, y;          /* Components physical location */
	int angle, mirror; /* Components rotation angle and mirror */

	int selected;

	COMP_REF id;

	FOOTPRINT_T *footprint; /* Component footprint */

	BOUNDING_BOX_T bb; /* In screen coordinates */

	COMPONENT_T *next; };

/* Additional drawing objects will need to be added to permit more */
/* flexible footprints */
struct st_footprint_ewb {
	FOOTPRINT_ID id;
	
	OUTLINE_T *outlines;
	OUTLINE_T *last_outline;

	PAD_T *pads;       /* Linked list of all pads for this footprint */
	PAD_T *last_pad;   /* Last pad in list */

	BOUNDING_BOX_T bb; /* In relative coordinates, includes all pads */
	
	FOOTPRINT_T *next;
}; 

/* Outlines of component */
struct st_outline {
	outline_type type;

	BOX_T *shape; /* eventually create a union of possible types that can be selected */

	OUTLINE_T *next;
};

/* Box coordinates */
struct st_box {
	int x1, y1, x2, y2;

	layer_type outline_layer;

	BOUNDING_BOX_T bb; /* In screen coordinates */
};

/* Tracks are the actual routes that make up the PCB */
/* Routing a net creates a track associated with that net */
struct st_track_ewb {
	TRACK_SEG_T *track_segs;   /* Linked list of track segments */
	TRACK_SEG_T *last_seg;     /* Last track segment */
	VIA_T *vias;               /* Linked list of vias in this track */
	VIA_T *last_via;           /* Last via */
	VIA_T *next2last_via;      /* Second to last via */
	NET_T *net;                /* Net that this route corresponds to */

	BOUNDING_BOX_T bb;

	TRACK_T *next;
};

/* A set of track segments constitute a track */
struct st_track_seg_ewb {
	int src_x, src_y;         /* Coords of track segment source */
	int dst_x, dst_y;         /* Coords of track segment destination */
	int width;                /* Track width */
	layer_type layer;         /* Layer that track segment is on */

	int selected;
	
    BOUNDING_BOX_T bb;

	TRACK_SEG_T *next;           /* Next segment in list */
};

/* A net indicates an electrical connection between two or more pads */
/* This structure contains chained list of netdef */
/* and indication whether it is routed or not, */
/* selected or not. */
struct st_net_ewb {
	int selected;
	int routed;             /* Indicates that this net has been routed, so don't draw it */

	NET_REF id;

	NETDEF_T *netdefs;

	BOUNDING_BOX_T bb;
	
	NET_T *next;
};

/* Describe a net corner with pad, component and coord */
struct st_netdef_ewb {
	int x,y;

	COMPONENT_T *comp;
	PAD_T *pad;

    BOUNDING_BOX_T bb;
	
	NETDEF_T *next;
}

;/* Vias connect track segments on different levels */
struct st_via_ewb {
	int x, y;              /* Coords of via position, in screen coords */

	via_type type;
	int selected;

	VIADEF_T *viadefs;
	
	BOUNDING_BOX_T bb;

	VIA_T *next;
};

struct st_viadef_ewb {

	via_shape shape;
	
	int size;
	int size2;

	layer_type src_layer;
	layer_type dst_layer;

	VIADEF_T *viadefs;

	BOUNDING_BOX_T bb;
	
	VIADEF_T *next;
};
	
/* Electrical connections occur between components and tracks at pads */
/* Each component has a number of pads associated with it */
struct st_pad_ewb {
	PAD_NAME name;
	int no;

	int x, y;              /* Pad location, relative to component location */
	
	int selected;
	
	PADDEF_T *paddefs;
	PADDEF_T *last_paddef;

	BOUNDING_BOX_T bb;
	
	PAD_T *next;
};

struct st_paddef_ewb {
	layer_type start_layer;
	layer_type finish_layer;

	pad_type type;

	int size;
	int size2;

	int hole;

	BOUNDING_BOX_T bb;
	
	PADDEF_T *next;
};

#endif
