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

/* Start of passing arguments for st_object */
extern int p_type; /* see note in s_passing.c */
extern char p_name[20];

extern int p_top;
extern int p_left;
extern int p_right;
extern int p_bottom;

extern LINEPTS *p_line_points;         
extern CIRCLE *p_circle;         


extern OBJECT *p_complex;
extern char p_complex_basename[256]; 
extern char p_complex_clib[256];     

extern int p_x;
extern int p_y;

extern int p_screen_x;
extern int p_screen_y;

extern int p_color;
extern int p_angle;
extern int p_mirror;

/* for now hack */ 
extern char p_text_string[1025];
extern int p_text_size;   
extern int p_text_len;

extern ATTRIB *p_attribs;
extern int p_attribute;
extern int p_show_name_value;
extern int p_visibility;   
extern ATTRIB *p_attached_to;


extern void (*p_action_func)();
extern void (*p_sel_func)();  
extern void (*p_draw_func)(); 
/* End of passing arguments for st_object */
                          
