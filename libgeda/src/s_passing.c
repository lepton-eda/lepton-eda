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
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the Free
 * Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#include <config.h>

#include <stdio.h>

#include <gtk/gtk.h>
#include <gdk/gdk.h>
#include <gdk/gdkx.h>

#include <guile/gh.h>

#include "struct.h"
#include "globals.h"

/* Start of passing arguments for st_object */
/* should this be an int or an unsigned char ??? */ 
/* It can't be a char since I assign to it -1 which kills SGI stability */
/* Now assigned OBJ_HEAD throught the code */
int p_type=-1; 
char p_name[20];

int p_top=-1;
int p_left=-1;
int p_right=-1;
int p_bottom=-1;

int p_x=0;
int p_y=0;

int p_screen_x;
int p_screen_y;

/* FUTURE CHANGE!
int p_x1=-1;
int p_y1=-1;
int p_screen_x1=-1;
int p_screen_y1=-1;

int p_x2=-1;
int p_y2=-1;
int p_screen_x2=-1;
int p_screen_y2=-1;
*/

LINEPTS *p_line_points=NULL;   
CIRCLE *p_circle=NULL;   

OBJECT *p_complex=NULL;
char p_complex_basename[256]; /* hack needs to be dynamic */
char p_complex_clib[256];     /* hack needs to be dynamic */

int p_color;
int p_angle;
int p_mirror;

/* for now hack */
char p_text_string[1025]; /* ugggg hack */
int p_text_size; 
int p_text_len;

ATTRIB *p_attribs = NULL;
int p_attribute;
int p_show_name_value;
int p_visibility;  
ATTRIB *p_attached_to = NULL;

void (*p_action_func)() = NULL;
void (*p_sel_func)() = NULL;  
void (*p_draw_func)() = NULL; 
/* End of passing arguments for st_object */
                          
