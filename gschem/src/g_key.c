/* gEDA - GNU Electronic Design Automation
 * gschem - GNU Schematic Capture
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
#include <string.h>
#include <ctype.h>
#include <stdlib.h>
#include <sys/stat.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h> 
#endif

#include <gtk/gtk.h>
#include <gdk/gdk.h>
#include <gdk/gdkx.h>

#include <guile/gh.h>


#include <libgeda/struct.h>
#include <libgeda/defines.h>
#include <libgeda/globals.h>  
#include <libgeda/prototype.h>

#include "../include/prototype.h"

/* this is needed so that these routines know which window they are changing */
static TOPLEVEL *window_current;

void
set_window_current_key(TOPLEVEL *w_current)
{
	window_current = w_current;
}

/* for now this only supports single chars, not shift/alt/ctrl etc... */
void
g_key_execute(int state, int keyval)
{
	char guile_string[50]; /* size hack */
	char modifier[10];

	if (keyval == 0) {
		return;
	}
	
	if (state & GDK_SHIFT_MASK) {
		strcpy(modifier, "\"Shift ");
	} else if (state & GDK_CONTROL_MASK) {
		strcpy(modifier, "\"Control ");
	} else if (state & GDK_MOD1_MASK) {
		strcpy(modifier, "\"Alt ");
	} else {
		modifier[0] = '\"';
		modifier[1] = '\0';
	}

	/* don't pass the raw modifier key presses to the guile code */	
	if ( strstr(XKeysymToString(keyval), "Alt")) {
		return;
	}

	if ( strstr(XKeysymToString(keyval), "Shift")) {
		return;
	}

	if ( strstr(XKeysymToString(keyval), "Control")) {
		return;
	}

	sprintf(guile_string, "(press-key %s%s\")", modifier, XKeysymToString (keyval));
	
#if DEBUG 
	printf("_%s_\n", guile_string);
#endif

	gh_eval_str(guile_string);

#if 0 /* playing with thi's guile stuff */
	gh_eval_str("(display (reverse last-command-sequence))");
	printf("\n");
#endif
		
}

SCM g_key_file_new (void) 
{
	i_callback_file_new(window_current, 0, NULL);
	return(gh_int2scm(0));
}


SCM
g_key_file_new_window (void) 
{

	i_callback_file_new_window(window_current, 0, NULL);
	return(gh_int2scm(0));
}


/* don't use the widget parameter on this function, or do some checking... */
/* since there is a call: widget = NULL, data = 0 (will be w_current) */ 
/* This should be renamed to page_open perhaps... */
SCM 
g_key_file_open (void) 
{
	i_callback_file_open(window_current, 0, NULL);
	return(gh_int2scm(0));
}

/* don't use the widget parameter on this function, or do some checking... */
/* since there is a call: widget = NULL, data = 0 (will be w_current) */ 
SCM 
g_key_file_script (void) 
{
	i_callback_file_script(window_current, 0, NULL);
	return(gh_int2scm(0));
}


/* don't use the widget parameter on this function, or do some checking... */
/* since there is a call: widget = NULL, data = 0 (will be w_current) */ 
SCM 
g_key_file_save (void) 
{
	i_callback_file_save(window_current, 0, NULL);
	return(gh_int2scm(0));
}


SCM 
g_key_file_save_as (void) 
{
	i_callback_file_save_as(window_current, 0, NULL);
	return(gh_int2scm(0));
}

SCM 
g_key_file_save_all (void) 
{
	i_callback_file_save_all(window_current, 0, NULL);
	return(gh_int2scm(0));
}

SCM 
g_key_file_print (void) 
{
	i_callback_file_print(window_current, 0, NULL);
	return(gh_int2scm(0));
}

SCM 
g_key_file_image_write (void) 
{
	i_callback_file_image_write(window_current, 0, NULL);
	return(gh_int2scm(0));
}


/* don't use the widget parameter on this function, or do some checking... */
/* since there is a call: widget = NULL, data = 0 (will be w_current) */ 
/* this function closes a window */
SCM
g_key_file_close_window (void)
{
	/* close the current window */
	i_callback_file_close(window_current, 0, NULL);
	return(gh_int2scm(0));
}


SCM
g_key_file_quit (void)
{
	i_callback_file_quit(window_current, 0, NULL);
	return(gh_int2scm(0));
}


/* Edit menu */

/* Select also does not update the middle button shortcut */
SCM 
g_key_edit_select (void)
{
	i_callback_edit_select(window_current, 0, NULL);
	return(gh_int2scm(0));
}


SCM 
g_key_edit_copy (void)
{
	i_callback_edit_copy(window_current, 0, NULL);
	return(gh_int2scm(0));
}


SCM 
g_key_edit_copy_hotkey (void)
{
	i_callback_edit_copy_hotkey(window_current, 0, NULL);
	return(gh_int2scm(0));
}


SCM 
g_key_edit_move (void)
{
	i_callback_edit_move(window_current, 0, NULL);
	return(gh_int2scm(0));
}


SCM 
g_key_edit_move_hotkey (void)
{
	i_callback_edit_move_hotkey(window_current, 0, NULL);
	return(gh_int2scm(0));
}


SCM 
g_key_edit_delete (void)
{
	i_callback_edit_delete(window_current, 0, NULL);
	return(gh_int2scm(0));
}


SCM 
g_key_edit_rotate_90 (void)
{
	i_callback_edit_rotate_90(window_current, 0, NULL);
	return(gh_int2scm(0));
}


SCM 
g_key_edit_rotate_90_hotkey (void)
{
	i_callback_edit_rotate_90_hotkey(window_current, 0, NULL);
	return(gh_int2scm(0));
}


SCM 
g_key_edit_mirror (void)
{
	i_callback_edit_mirror(window_current, 0, NULL);
	return(gh_int2scm(0));
}


SCM 
g_key_edit_mirror_hotkey (void)
{
	i_callback_edit_mirror_hotkey(window_current, 0, NULL);
	return(gh_int2scm(0));
}

SCM 
g_key_edit_slot (void)
{
	i_callback_edit_slot(window_current, 0, NULL);
	return(gh_int2scm(0));
}

SCM 
g_key_edit_color (void)
{
	i_callback_edit_color(window_current, 0, NULL);
	return(gh_int2scm(0));
}


SCM 
g_key_edit_edit (void)
{
	i_callback_edit_edit(window_current, 0, NULL);
	return(gh_int2scm(0));
}


/* locks all objects in selection list */
SCM 
g_key_edit_lock (void)
{
	i_callback_edit_lock(window_current, 0, NULL);
	return(gh_int2scm(0));
}


/* unlocks all objects in selection list */
SCM 
g_key_edit_unlock (void)
{
	i_callback_edit_unlock(window_current, 0, NULL);
	return(gh_int2scm(0));
}


SCM 
g_key_edit_translate (void)
{
	i_callback_edit_translate(window_current, 0, NULL);
	return(gh_int2scm(0));
}

/* embedds all objects in selection list */
SCM 
g_key_edit_embed (void)
{
	i_callback_edit_embed(window_current, 0, NULL);
	return(gh_int2scm(0));
}

/* unembedds all objects in selection list */
SCM 
g_key_edit_unembed (void)
{
	i_callback_edit_unembed(window_current, 0, NULL);
	return(gh_int2scm(0));
}

SCM 
g_key_edit_show_hidden (void)
{
	i_callback_edit_show_hidden(window_current, 0, NULL);
	return(gh_int2scm(0));
}

/* View menu */

/* repeat middle shortcut doesn't make sense on redraw, just hit right button */
SCM 
g_key_view_redraw (void)
{
	i_callback_view_redraw(window_current, 0, NULL);
	return(gh_int2scm(0));
}


/* repeat middle shortcut would get into the way of what user is try to do */
SCM 
g_key_view_zoom_full (void)
{
	i_callback_view_zoom_full(window_current, 0, NULL);
	return(gh_int2scm(0));
}

/* repeat middle shortcut would get into the way of what user is try to do */
SCM 
g_key_view_zoom_limits (void)
{
	i_callback_view_zoom_limits(window_current, 0, NULL);
	return(gh_int2scm(0));
}

/* repeat middle shortcut would get into the way of what user is try to do */
SCM 
g_key_view_zoom_in (void)
{
	i_callback_view_zoom_in(window_current, 0, NULL);
	return(gh_int2scm(0));
}

/* repeat middle shortcut would get into the way of what user is try to do */
SCM 
g_key_view_zoom_out (void)
{
	i_callback_view_zoom_out(window_current, 0, NULL);
	return(gh_int2scm(0));
}

SCM 
g_key_view_zoom_box (void)
{
	i_callback_view_zoom_box(window_current, 0, NULL);
	return(gh_int2scm(0));
}

SCM 
g_key_view_zoom_box_hotkey (void)
{
	i_callback_view_zoom_box_hotkey(window_current, 0, NULL);
	return(gh_int2scm(0));
}

SCM 
g_key_view_pan (void)
{
	i_callback_view_pan(window_current, 0, NULL);
	return(gh_int2scm(0));
}

SCM 
g_key_view_pan_hotkey (void)
{
	i_callback_view_pan_hotkey(window_current, 0, NULL);
	return(gh_int2scm(0));
}

SCM  
g_key_view_updatenets (void)
{
	i_callback_view_updatenets(window_current, 0, NULL);
	return(gh_int2scm(0));
}


SCM  
g_key_page_manager (void)
{
	i_callback_page_manager(window_current, 0, NULL);
	return(gh_int2scm(0));
}

SCM  
g_key_page_next (void)
{
	i_callback_page_next(window_current, 0, NULL);
	return(gh_int2scm(0));
}


SCM  
g_key_page_prev (void)
{
	i_callback_page_prev(window_current, 0, NULL);
	return(gh_int2scm(0));
}


SCM  
g_key_page_new (void)
{
	i_callback_page_new(window_current, 0, NULL);
	return(gh_int2scm(0));
}


SCM  
g_key_page_close (void)
{
	i_callback_page_close(window_current, 0, NULL);
	return(gh_int2scm(0));
}


SCM  
g_key_page_discard (void)
{
	i_callback_page_discard(window_current, 0, NULL);
	return(gh_int2scm(0));
}


SCM  
g_key_page_print (void)
{
	i_callback_page_print(window_current, 0, NULL);
	return(gh_int2scm(0));
}

/* Add menu */ 
SCM 
g_key_add_component (void)
{
	i_callback_add_component(window_current, 0, NULL);
	return(gh_int2scm(0));
}


SCM 
g_key_add_attribute (void)
{
	i_callback_add_attribute(window_current, 0, NULL);
	return(gh_int2scm(0));
}


SCM 
g_key_add_net (void)
{
	i_callback_add_net(window_current, 0, NULL);
	return(gh_int2scm(0));
}

SCM 
g_key_add_net_hotkey (void)
{
	i_callback_add_net_hotkey(window_current, 0, NULL);
	return(gh_int2scm(0));
}

SCM 
g_key_add_text (void)
{
	i_callback_add_text(window_current, 0, NULL);
	return(gh_int2scm(0));
}


SCM 
g_key_add_line (void)
{
	i_callback_add_line(window_current, 0, NULL);
	return(gh_int2scm(0));
}

SCM 
g_key_add_line_hotkey (void)
{
	i_callback_add_line_hotkey(window_current, 0, NULL);
	return(gh_int2scm(0));
}

SCM 
g_key_add_box (void)
{
	i_callback_add_box(window_current, 0, NULL);
	return(gh_int2scm(0));
}

SCM 
g_key_add_box_hotkey (void)
{
	i_callback_add_box_hotkey(window_current, 0, NULL);
	return(gh_int2scm(0));
}

SCM 
g_key_add_circle (void)
{
	i_callback_add_circle(window_current, 0, NULL);
	return(gh_int2scm(0));
}

SCM 
g_key_add_circle_hotkey (void)
{
	i_callback_add_circle_hotkey(window_current, 0, NULL);
	return(gh_int2scm(0));
}

SCM 
g_key_add_arc (void)
{
	i_callback_add_arc(window_current, 0, NULL);
	return(gh_int2scm(0));
}

SCM 
g_key_add_arc_hotkey (void)
{
	i_callback_add_arc_hotkey(window_current, 0, NULL);
	return(gh_int2scm(0));
}

SCM 
g_key_add_pin (void)
{
	i_callback_add_pin(window_current, 0, NULL);
	return(gh_int2scm(0));
}

SCM 
g_key_add_pin_hotkey (void)
{
	i_callback_add_pin_hotkey(window_current, 0, NULL);
	return(gh_int2scm(0));
}


/* Hierarchy menu */
SCM 
g_key_hierarchy_open_symbol (void)
{
	i_callback_hierarchy_open_symbol(window_current, 0, NULL);
	return(gh_int2scm(0));
}

/* Attributes menu */
SCM  
g_key_attributes_attach (void)
{
	i_callback_attributes_attach(window_current, 0, NULL);
	return(gh_int2scm(0));
}

SCM 
g_key_attributes_detach (void)
{
	i_callback_attributes_detach(window_current, 0, NULL);
	return(gh_int2scm(0));
}

SCM
g_key_attributes_show_name (void)
{
	i_callback_attributes_show_name(window_current, 0, NULL);
	return(gh_int2scm(0));
}

SCM 
g_key_attributes_show_value (void)
{
	i_callback_attributes_show_value(window_current, 0, NULL);
	return(gh_int2scm(0));
}

SCM 
g_key_attributes_show_both (void)
{
	i_callback_attributes_show_both(window_current, 0, NULL);
	return(gh_int2scm(0));
}

SCM 
g_key_attributes_visibility_toggle (void)
{
	i_callback_attributes_visibility_toggle(window_current, 0, NULL);
	return(gh_int2scm(0));
}


/* Script menu */
/* not currently implemented */
SCM 
g_key_script_console (void)
{

	return(gh_int2scm(0));
}

/* Layers menu */

/* Options menu */

/* repeat last command doesn't make sense on options either??? (does it?) */
SCM
g_key_options_text_size (void)
{
	i_callback_options_text_size(window_current, 0, NULL);
	return(gh_int2scm(0));
}     

/* repeat last command doesn't make sense on options either??? (does it?) */
SCM
g_key_options_afeedback (void) 
{
	i_callback_options_afeedback(window_current, 0, NULL);
	return(gh_int2scm(0));
}

SCM
g_key_options_grid (void)
{
	i_callback_options_grid(window_current, 0, NULL);
	return(gh_int2scm(0));
}

SCM
g_key_options_snap (void) 
{
	i_callback_options_snap(window_current, 0, NULL);
	return(gh_int2scm(0));
}

SCM
g_key_options_snap_size (void)
{
	i_callback_options_snap_size(window_current, 0, NULL);
	return(gh_int2scm(0));
}   

SCM g_key_options_show_log_window (void)
{
	i_callback_options_show_status(window_current, 0, NULL);
	return(gh_int2scm(0));
}

SCM g_key_options_show_coord_window (void)
{
	i_callback_options_show_coord(window_current, 0, NULL);
	return(gh_int2scm(0));
}

SCM  
g_key_misc (void)
{
	i_callback_misc(window_current, 0, NULL);
	return(gh_int2scm(0));
}

/* be sure that you don't use the widget parameter in this one, since it is 
being called with a null, I suppose we should call it with the right param.
hack */
SCM
g_key_cancel(void)
{
	i_callback_cancel(window_current, 0, NULL);
	return(gh_int2scm(0));
}

