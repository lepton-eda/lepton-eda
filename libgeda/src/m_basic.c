/* gEDA - GNU Electronic Design Automation
 * libgeda - gEDA's library
 * Copyright (C) 1998 Ales V. Hvezda
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
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#include <config.h>
#include <stdio.h>
#include <math.h>

#include <gtk/gtk.h>
#include <gdk/gdk.h>
#include <gdk/gdkx.h>

#include <guile/gh.h>

#include "struct.h" /* why should I include these hack, just for prototype ? */
#include "globals.h"

#include "../include/prototype.h"

int
mil_x(TOPLEVEL *w_current, int val)
{
	double i;
	double fval;
	int j;

#if 0 /* removed for speed improvements */
	double fw0,fw1,fw,fval;
	fw1 = w_current->page_current->right;
	fw0 = w_current->page_current->left;
	fw  = w_current->width;
#endif

	fval = val;
	i = fval * w_current->page_current->to_world_x_constant + 
		w_current->page_current->left;

	/* i -= mil_x_tw2;
	i = ((i) / 100 ) * 100; I don't think we need this 
	i += mil_x_tw1;*/


#ifdef HAS_RINT
	j = rint(i);
#else
	j = i;
#endif

	return(j);
}


int 
mil_y(TOPLEVEL *w_current, int val)
{
	double i;
	double fval;
	int j;

#if 0 /* removed for speed improvements */
	double fw0,fw1,fw,fval;
	fw1 = w_current->page_current->bottom;
	fw0 = w_current->page_current->top;
	fw  = w_current->height;
#endif

	fval = w_current->height - val; 
	i = fval * w_current->page_current->to_world_y_constant +
		w_current->page_current->top;

	/* i = ((i) / 100 ) * 100; I don't think we need this */
	/* i += mil_y_tw1; or this*/

#ifdef HAS_RINT
	j = rint(i);
#else
	j = i;
#endif

	return(j);
}

int
pix_x(TOPLEVEL *w_current, int val)
{

	double i;
	int j;

#if 0 /* removed for speed */
	double fs,f0,f1,f;
	f0 = w_current->page_current->left;
	f1 = w_current->page_current->right;
	fs = w_current->width;
	f = w_current->width / (f1 - f0);
#endif


	i = w_current->page_current->to_screen_x_constant * 
		(double)(val - w_current->page_current->left);

#ifdef HAS_RINT
	j = rint(i);
#else
	j = i;
#endif

	return(j);
}

int 
pix_y(TOPLEVEL *w_current, int val)
{
	double i;
	int j;

#if 0 /* removed for speed */
	double fs,f0,f1,f;
    	f0 = w_current->page_current->top;
    	f1 = w_current->page_current->bottom;
    	fs = w_current->height;
    	f = fs / (f1 - f0); /* fs was w_current->height */
#endif
    	i = w_current->height - (
	    w_current->page_current->to_screen_y_constant * 
	    (double)(val - w_current->page_current->top)); 

#ifdef HAS_RINT
	j = rint(i);
#else
	j = i;
#endif

	return(j);
}


void
WORLDtoSCREEN(TOPLEVEL *w_current, int x, int y, int *mil_x, int *mil_y)
{
	*mil_x = pix_x(w_current, x);
	*mil_y = pix_y(w_current, y);
}

int
snap_grid(TOPLEVEL *w_current, int input)
{
	int p, m, n;
	int sign, value, snap_grid;
	
	if (!w_current->snap) {
		return(input);
	}

		
	snap_grid = w_current->snap_size;

/* this code was inspired from killustrator, it's much simpler than mine */
	sign = ( input < 0 ? -1 : 1 );
	value = abs(input);

	p = value / snap_grid;
	m = value % snap_grid;
	n = p * snap_grid;
	if (m > snap_grid / 2)
		n += snap_grid;

#if DEBUG 
	printf("p: %d\n", p);
	printf("m: %d\n", m);
	printf("m > snap_grid / 2: %d\n", (m > snap_grid / 2));
	printf("n: %d\n", n);
	printf("n*s: %d\n", n*sign);
#endif

	return(sign*n);

#if 0 /* working snap code, crude, slow */
        int interm; 
        int final;
        int power;
        int itop;
	
        power = snap_grid;
        itop = input / power;
        interm = abs(input % power);

	if (interm > 0 && interm < snap_grid/2) {	
		interm = 0;
        } else if (interm >= snap_grid/2 && interm <= snap_grid) {
		interm = snap_grid;
	}

	if (input >= 0) {	
        	final = itop*snap_grid+interm;
	} else if (input < 0) {
        	final = itop*snap_grid-interm;
	}

        return(final);
#endif

}                               


void
SCREENtoWORLD(TOPLEVEL *w_current, int mx, int my, int *x, int *y)      
{
	if (w_current->snap) { 
		*x = snap_grid(w_current, mil_x(w_current, mx));
		*y = snap_grid(w_current, mil_y(w_current, my));
	} else {
		*x = mil_x(w_current, mx);
		*y = mil_y(w_current, my);
	}

#if 0
	*x = mil_x(w_current, mx);
	*y = mil_y(w_current, my);
#endif

}

/* returns screen coords */
int
SCREENabs(TOPLEVEL *w_current, int val)
{
	double fs,f0,f1,f;

	double i;
	int j;

	f0 = w_current->page_current->left;
	f1 = w_current->page_current->right;
	fs = w_current->width;
	f = w_current->width / (f1 - f0);
	i = f * (double)(val);

#ifdef HAS_RINT
	j = rint(i);
#else
	j = i;
#endif

	return(j);

}

/* returns world coords */
int
WORLDabs(TOPLEVEL *w_current, int val)
{
	double fw0,fw1,fw,fval;

	double i;
	int j;

	fw1 = w_current->page_current->right;
	fw0 = w_current->page_current->left;
	fw  = w_current->width;
	fval = val;
	i = fval * (fw1 - fw0) / fw;

	/* i -= mil_x_tw2;
	i = ((i) / 100 ) * 100; I don't think we need this 
	i += mil_x_tw1;*/


#ifdef HAS_RINT
	j = rint(i);
#else
	j = i;
#endif

	return(j);
}



void set_window(TOPLEVEL *w_current, int xmin, int xmax, int ymin, int ymax)
{
	double fs,f0,f1,f;
	double fw0,fw1,fw,fval;

	w_current->page_current->left=xmin;
	w_current->page_current->right=xmax;
	w_current->page_current->top=ymin; 
	w_current->page_current->bottom=ymax;

	/* now do the constant setups */

	/* pix_x */
	f0 = w_current->page_current->left;
        f1 = w_current->page_current->right;
        fs = w_current->width;
	w_current->page_current->to_screen_x_constant = fs / (f1 - f0);

	/* pix_y */
	f0 = w_current->page_current->left;
	f0 = w_current->page_current->top;
        f1 = w_current->page_current->bottom;
        fs = w_current->height;
	w_current->page_current->to_screen_y_constant = fs / (f1 - f0); 

	/* mil_x */
	fw1 = w_current->page_current->right;
	fw0 = w_current->page_current->left;
	fw  = w_current->width;
	w_current->page_current->to_world_x_constant = (fw1 - fw0) / fw;

	/* mil_y */
	fw1 = w_current->page_current->bottom;
	fw0 = w_current->page_current->top;
	fw  = w_current->height;
	w_current->page_current->to_world_y_constant = (fw1 - fw0) / fw;
}

/* fix_x and fix_y are used for grid snap */

int
fix_x(TOPLEVEL *w_current, int in)
{
	int value;
	int ret;

	if (in > w_current->width) { 
		in = w_current->width;
	}
	
	if (!w_current->snap)
		return(in);

	value = mil_x(w_current, in);	

	ret = pix_x(w_current, snap_grid(w_current, value));
	return(ret);
}

int
fix_y(TOPLEVEL *w_current, int in)
{
	int value;
	int ret;

	if (in > w_current->height) { 
		in = w_current->height;
	}

	if (!w_current->snap)
		return(in);


	value = mil_y(w_current, in);	
	ret = pix_y(w_current, snap_grid(w_current, value));
	return(ret);
}

/* returns 0 if point (x) is snapped */
/* returns something if point (x) is not snapped */
/* unused for now */
int
on_snap(int val)
{
	return( (val / 100)*100 - val);
}

typedef struct st_halfspace HALFSPACE;
typedef struct st_point POINT;

struct st_halfspace {
	int left; /* these are booleans */
	int top;
	int right; 
	int bottom; 
};

struct st_point {
	int x, y;
};



/* encode_halfspace and clip are part of the cohen-sutherland clipping */
/* algorithm.  They are used to determine if an object is visible or not */

/* halfspace must be allocated before this is called */
static void
SCREENencode_halfspace(TOPLEVEL *w_current, POINT *point, HALFSPACE *halfspace)
{
	halfspace->left = point->x < 0;
	halfspace->right = point->x > w_current->width;
	halfspace->bottom = point->y > w_current->height;
	halfspace->top = point->y < 0;
}

/* halfspace must be allocated before this is called */
static void
WORLDencode_halfspace(TOPLEVEL *w_current, POINT *point, HALFSPACE *halfspace)
{
	halfspace->left = point->x < w_current->page_current->left;
	halfspace->right = point->x > w_current->page_current->right;
	halfspace->bottom = point->y > w_current->page_current->bottom;
	halfspace->top = point->y < w_current->page_current->top;
}

int
SCREENclip_change(TOPLEVEL *w_current,int *x1, int *y1, int *x2, int *y2)
{
	HALFSPACE half1, half2; 
	HALFSPACE tmp_half;
	POINT tmp_point;
	POINT point1, point2;
	float slope;
	int in1, in2, done;
	int visible;
	int w_l, w_t, w_r, w_b;

	point1.x = *x1;
	point1.y = *y1;
	point2.x = *x2;
	point2.y = *y2;


	w_l = 0;
	w_t = 0;
	w_r = w_current->width;
	w_b = w_current->height;


	done = FALSE;
	visible = FALSE;

	do {
		SCREENencode_halfspace(w_current, &point1, &half1);
		SCREENencode_halfspace(w_current, &point2, &half2);

#if DEBUG
		printf("starting loop\n");
		printf("1 %d %d %d %d\n", half1.left, half1.top, half1.right, half1.bottom);
		printf("2 %d %d %d %d\n", half2.left, half2.top, half2.right, half2.bottom);
#endif

		in1 = (!half1.left) && 
		      (!half1.top) && 
                      (!half1.right) && 
                      (!half1.bottom);

		in2 = (!half2.left) &&  
                      (!half2.top) && 
	              (!half2.right) && 
		      (!half2.bottom);


		if (in1 && in2) { /* trivally accept */
			done = TRUE;
			visible = TRUE;
		} else if ( ((half1.left && half2.left) || 
			     (half1.right && half2.right)) ||
		            ((half1.top && half2.top) || 
	                     (half1.bottom && half2.bottom)) ) {
			done = TRUE; /* trivially reject */
			visible = FALSE;
		} else { /* at least one point outside */
			if (in1) {
				tmp_half = half1;
				half1 = half2; 
				half2 = tmp_half;

				tmp_point = point1; 
				point1 = point2; 
				point2 = tmp_point;
			}

			if (point2.x == point1.x) { /* vertical line */
				if (half1.top) {
					point1.y = w_t;
				} else if (half1.bottom) {
					point1.y = w_b;
				}
			} else { /* not a vertical line */

				/* possible fix for alpha core dumping */
				/* assume the object is visible */
				if ((point2.x - point1.x) == 0) {
					return(TRUE);
				}

				slope = (float) (point2.y - point1.y) / 
					(float) (point2.x - point1.x); 

				/* possible fix for alpha core dumping */
				/* assume the object is visible */
				if (slope == 0.0) {
					return(TRUE);
				}

				if (half1.left) {
					point1.y = point1.y + 
						   (w_l - point1.x) * slope;
					point1.x = w_l;
				} else if (half1.right) {
					point1.y = point1.y + 
						   (w_r - point1.x) * slope;
					point1.x = w_r;
				} else if (half1.bottom) {
					point1.x = point1.x +
						   (w_b - point1.y) / slope;
					point1.y = w_b;
				} else if (half1.top) {
					point1.x = point1.x + 
						   (w_t - point1.y) / slope;
					point1.y = w_t;
				}
			} /* end of not a vertical line */
		} /* end of at least one outside */
	} while (!done);

	/*printf("after: %d %d %d %d\n", point1.x, point1.y, point2.x, point2.y);*/
	*x1 = point1.x;
	*y1 = point1.y;
	*x2 = point2.x;
	*y2 = point2.y;
	return(visible);
}

int
clip_nochange(TOPLEVEL *w_current,int x1, int y1, int x2, int y2)
{
	HALFSPACE half1, half2; 
	HALFSPACE tmp_half;
	POINT tmp_point;
	POINT point1, point2;
	float slope;
	int in1, in2, done;
	int visible;
	int w_l, w_t, w_r, w_b;

	point1.x = x1;
	point1.y = y1;
	point2.x = x2;
	point2.y = y2;

	/*printf("before: %d %d %d %d\n", x1, y1, x2, y2);*/

	w_l = w_current->page_current->left;
	w_t = w_current->page_current->top;
	w_r = w_current->page_current->right;
	w_b = w_current->page_current->bottom;

	done = FALSE;
	visible = FALSE;

	do {
		WORLDencode_halfspace(w_current, &point1, &half1);
		WORLDencode_halfspace(w_current, &point2, &half2);

#if DEBUG
		printf("starting loop\n");
		printf("1 %d %d %d %d\n", half1.left, half1.top, half1.right, half1.bottom);
		printf("2 %d %d %d %d\n", half2.left, half2.top, half2.right, half2.bottom);
#endif

		in1 = (!half1.left) && 
		      (!half1.top) && 
                      (!half1.right) && 
                      (!half1.bottom);

		in2 = (!half2.left) &&  
                      (!half2.top) && 
	              (!half2.right) && 
		      (!half2.bottom);


		if (in1 && in2) { /* trivally accept */
			done = TRUE;
			visible = TRUE;
		} else if ( ((half1.left && half2.left) || 
			     (half1.right && half2.right)) ||
		            ((half1.top && half2.top) || 
	                     (half1.bottom && half2.bottom)) ) {
			done = TRUE; /* trivially reject */
			visible = FALSE;
		} else { /* at least one point outside */
			if (in1) {
				tmp_half = half1;
				half1 = half2; 
				half2 = tmp_half;

				tmp_point = point1; 
				point1 = point2; 
				point2 = tmp_point;
			}

			if (point2.x == point1.x) { /* vertical line */
				if (half1.top) {
					point1.y = w_t;
				} else if (half1.bottom) {
					point1.y = w_b;
				}
			} else { /* not a vertical line */

				/* possible fix for alpha core dumping */
				/* assume the object is visible */
				if ((point2.x - point1.x) == 0) {
					return(TRUE);
				}

				slope = (float) (point2.y - point1.y) / 
					(float) (point2.x - point1.x); 

				/* possible fix for alpha core dumping */
				/* assume the object is visible */
				if (slope == 0.0) {
					return(TRUE);
				}

				if (half1.left) {
					point1.y = point1.y + 
						   (w_l - point1.x) * slope;
					point1.x = w_l;
				} else if (half1.right) {
					point1.y = point1.y + 
						   (w_r - point1.x) * slope;
					point1.x = w_r;
				} else if (half1.bottom) {
					point1.x = point1.x +
						   (w_b - point1.y) / slope;
					point1.y = w_b;
				} else if (half1.top) {
					point1.x = point1.x + 
						   (w_t - point1.y) / slope;
					point1.y = w_t;
				}
			} /* end of not a vertical line */
		} /* end of at least one outside */
	} while (!done);

	return(visible);
}


/* This is a generic routine which checks to see if the bounding box is */
/* visible on the screen */
/* o_circle and o_box are the only one that uses this routine */
int 
visible(TOPLEVEL *w_current, int wleft, int wtop, int wright, int wbottom)
{
	int visible=FALSE;

	/* don't do object clipping if this is false */
	if (!w_current->object_clipping) {
		return(TRUE);
	}

	visible = clip_nochange(w_current, wleft, wtop, wright, wtop);

#if DEBUG 
	printf("vis1 %d\n", visible);
#endif

        if (!visible) {
                visible = clip_nochange(w_current, wleft, wbottom, wright, wbottom);
        } else {
		return(visible);
	} 

#if DEBUG
	printf("vis2 %d\n", visible);
#endif

        if (!visible) {
                visible = clip_nochange(w_current, wleft, wtop, wleft, wbottom);
        } else {
		return(visible);
	} 

#if DEBUG 
	printf("vis3 %d\n", visible);
#endif

        if (!visible) {
                visible = clip_nochange(w_current, wright, wtop, wright, wbottom);
        } else {
		return(visible);
	} 

#if DEBUG 
	printf("vis4 %d\n", visible);
#endif

#if DEBUG
	printf("%d %d %d\n", wleft, w_current->page_current->top, wright);
	printf("%d %d %d\n", wtop, w_current->page_current->top, wbottom);
	printf("%d %d %d\n", wleft, w_current->page_current->right, wright);
	printf("%d %d %d\n", wtop, w_current->page_current->bottom, wbottom);
#endif

	/* now check to see if bounding box encompasses the entire viewport */
	if (w_current->page_current->left >= wleft && 
	    w_current->page_current->left <= wright  &&
	    w_current->page_current->top <= wtop &&
	    w_current->page_current->top >= wbottom ) {
	    /*w_current->page_current->right >= wleft &&
	    w_current->page_current->right <= wright &&
	    w_current->page_current->bottom <= wtop &&
	    w_current->page_current->bottom >= wbottom ) {*/
		visible = 1;
	} 

#if DEBUG 
	printf("vis5 %d\n", visible);
#endif

	return(visible);
}

void
rotate_point(int x, int y, int angle, int *newx, int *newy)
{
        double costheta, sintheta;
        double rad;

        rad = angle*M_PI/180;

        costheta = cos(rad);
        sintheta = sin(rad);

        *newx = x * costheta - y * sintheta;
        *newy = x * sintheta + y * costheta;
}

/* allows for rotations of increments 90 degrees only */
void
rotate_point_90(int x, int y, int angle, int *newx, int *newy)
{
        double costheta=1; 
	double sintheta=0;

	/* I could have used sine/cosine for this, but I want absolute 
	 * accuracy */
	switch(angle) {

		case(0):
        		*newx = x;
        		*newy = y; 
			return;
		break;
		
		case(90):
        		costheta = 0;
        		sintheta = 1;
		break;
		
		case(180):
        		costheta = -1;
        		sintheta = 0;
		break;
		
		case(270):
        		costheta = 0;
        		sintheta = -1;
		break;
	}

        *newx = x * costheta - y * sintheta;
        *newy = x * sintheta + y * costheta;

#if 0 /* fixed rotation */

        *newx = -y ; 
        *newy = x ;
#endif

}

/* support landscape only for now */
/* fixed aspect ratio */
void
PAPERSIZEtoWORLD(int width, int height, int border, int *right, int *bottom)
{
	float aspect;

	aspect = (float) width / (float) height;

#if DEBUG	
	printf("%f\n", aspect);
#endif

	if (aspect < 1.333333333) {
		/* is this rint really needed? */
#ifdef HAS_RINT
		*right = (int) rint(width+border + 
			((height+border)*1.33333333 - (width+border)));
#else 
		*right = (int) width+border + 
			((height+border)*1.33333333 - (width+border));
#endif
		*bottom = height+border;
	} else {
		*right = (int) width+border;	
 		*bottom = (int) height+border + ((width+border)/1.33333333 - (height+border));
	}
	
#if DEBUG
	aspect = (float) *right / (float) *bottom;
	printf("%f\n", aspect);
#endif

}

/* this function returns the # of zoom's we have done, since zoom_factor */
/* is actually the magnification level */
int
return_zoom_number(int zoom_factor)
{
	double check=0;
	double factor;
	int i=0;
	
	if (zoom_factor != 0) {
	factor = zoom_factor;
	check = pow(2, i);
	while( check != factor && check < factor) {
		i++;
		check = pow(2, i);
	}
	} else {
		return(0);
	}
	
	return(i);

#if 0 /* delete eventually */
	if (zoom_factor > 0) {
		return(log(zoom_factor)+2);
		/* return(log(zoom_factor+1));*/
	} else {
		return(1);
	}
#endif
}
