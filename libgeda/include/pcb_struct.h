/* gEDA - GNU Electronic Design Automation
 * pcb_struct - include file
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
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 */

#ifndef PCB_STRUCT_H
#define PCB_STRUCT_H

typedef struct st_page_ewb PAGE_T;
typedef struct st_box BOX_T;
typedef struct st_bounding_box BOUNDING_BOX_T;
typedef struct st_component_ewb COMPONENT_T;
typedef struct st_footprint_ewb FOOTPRINT_T;
typedef struct st_track_ewb TRACK_T;
typedef struct st_track_seg_ewb TRACK_SEG_T;
typedef struct st_net_ewb NET_T;
typedef struct st_via_ewb VIA_T;
typedef struct st_pad_ewb PAD_T;
typedef struct st_selection_ewb SELECTION_T;
typedef char * COMP_REF;
typedef char * FOOTPRINT_ID;
typedef char * PAD_NAME;

/* Modifiers allowed for drawing objects*/
typedef enum {DRAW_NORMAL, DRAW_SELECTED, DRAW_CLEAR, DRAW_MOVING, DRAW_XOR} drawing_type;

/* Start with 2 layers, but permit additional layers later */
typedef enum {LAYER_ALL=-1, LAYER_NONE, LAYER_1, LAYER_2} layer_type;

/* Supported types of pads, should eventually include surface mount types */
typedef enum {PAD_NULL, PAD_THRU_HOLE} pad_type;

/* Box coordinates */
struct st_box {
        int x1, y1, x2, y2;
};

/* Bounding box coordinates, in screen coords */
struct st_bounding_box {
	int top;	
	int left;	
	int right;
	int bottom;
};

struct st_selection_ewb {
	SELECTION_T *next;
	COMPONENT_T *object; /* Eventually create a union of possible types that can be selected */
};

/* All objects that make up a given page are included in this structure */
/* Each member is the start of a linked list */
struct st_page_ewb {
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
	TRACK_SEG_T *routeSeg;
};

/* Component information is held in this structure. */
struct st_component_ewb {
	int x, y;          /* Components physical location */

	int selected;

	COMP_REF id;

        BOUNDING_BOX_T bb; /* In screen coordinates */

	FOOTPRINT_T *footprint; /* Component footprint */

	COMPONENT_T *next;
};

/* Additional drawing objects will need to be added to permit more */
/* flexible footprints */
struct st_footprint_ewb {
	FOOTPRINT_ID id;

        BOX_T box;         /* Physical shape of component */

	layer_type outline_layer;

        BOUNDING_BOX_T bb; /* In relative coordinates, includes all pads */

        PAD_T *pads;       /* Linked list of all pads for this footprint */
        PAD_T *last_pad;   /* Last pad in list */
        FOOTPRINT_T *next;
}; 

/* Tracks are the actual routes that make up the PCB */
/* Routing a net creates a track associated with that net */
struct st_track_ewb {
	TRACK_SEG_T *track_segs;   /* Linked list of track segments */
	TRACK_SEG_T *last_seg;     /* Last track segment */
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

        BOUNDING_BOX_T bb;

	TRACK_SEG_T *next;           /* Next segment in list */
};

/* A net indicates an electrical connection between two pads */
/* This structure contains screen position information for drawing, */
/* as well as connection information for netlist use and */
/* screen updates */
struct st_net_ewb {
        int src_x, src_y;        /* Coords of net source, in world coords */
        int dst_x, dst_y;        /* Coords of net destination, in world coords */

	int selected;

	BOUNDING_BOX_T bb;

	int routed;             /* Indicates that this net has been routed, so don't draw it */

	COMPONENT_T *src_comp;  /* Link to source component */
	COMPONENT_T *dst_comp;  /* Link to destination component */
	
	PAD_T *src_pad;         /* Link to source pad */
	PAD_T *dst_pad;         /* Link to destination pad */
	
	NET_T *next;
};

/* Vias connect track segments on different levels */
struct st_via_ewb {
	int x, y;              /* Coords of via position, in screen coords */
	int size;              /* Size of via */

	int selected;

	layer_type src_layer;  /* Layer of source track */
	layer_type dst_layer;  /* Layer of destination track */

	BOUNDING_BOX_T bb;
};

/* Electrical connections occur between components and tracks at pads */
/* Each component has a number of pads associated with it */
struct st_pad_ewb {
	pad_type type;
	PAD_NAME name;

	int x, y;              /* Pad location, relative to component location */
	int size;
	
	BOUNDING_BOX_T bb;

	PAD_T *next;
};

#endif
