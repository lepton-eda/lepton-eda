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
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111 USA
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

/* this is needed so that these routines know which window they are
 * changing */
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
	/* TODO: the size is hack */
	char guile_string[50];
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

#if 0 /* old non-portable way */
	/* don't pass the raw modifier key presses to the guile code */
	if (strstr(XKeysymToString(keyval), "Alt")) {
		return;
	}

	if (strstr(XKeysymToString(keyval), "Shift")) {
		return;
	}

	if (strstr(XKeysymToString(keyval), "Control")) {
		return;
	}

	sprintf(guile_string, "(press-key %s%s\")",
		modifier, XKeysymToString (keyval));
#endif

	/* don't pass the raw modifier key presses to the guile code */
	if (strstr(gdk_keyval_name(keyval), "Alt")) {
		return;
	}

	if (strstr(gdk_keyval_name(keyval), "Shift")) {
		return;
	}

	if (strstr(gdk_keyval_name(keyval), "Control")) {
		return;
	}

	sprintf(guile_string, "(press-key %s%s\")",
		modifier, gdk_keyval_name (keyval));

#if DEBUG
	printf("_%s_\n", guile_string);
#endif
	gh_eval_str(guile_string);

#if 0 /* playing with thi's guile stuff */
	gh_eval_str("(display (reverse last-command-sequence))");
	printf("\n");
#endif
}

#define DEFINE_G_KEY(name)				\
SCM g_key_ ## name(void)				\
{							\
	i_callback_ ## name(window_current, 0, NULL);	\
	return SCM_BOOL_T;				\
}

DEFINE_G_KEY(file_new)

DEFINE_G_KEY(file_new_window)

/* don't use the widget parameter on this function, or do some checking... */
/* since there is a call: widget = NULL, data = 0 (will be w_current) */
/* This should be renamed to page_open perhaps... */
DEFINE_G_KEY(file_open)

/* don't use the widget parameter on this function, or do some checking... */
/* since there is a call: widget = NULL, data = 0 (will be w_current) */
DEFINE_G_KEY(file_script)

/* don't use the widget parameter on this function, or do some checking... */
/* since there is a call: widget = NULL, data = 0 (will be w_current) */
DEFINE_G_KEY(file_save)
DEFINE_G_KEY(file_save_as)
DEFINE_G_KEY(file_save_all)
DEFINE_G_KEY(file_print)
DEFINE_G_KEY(file_write_png)

/* don't use the widget parameter on this function, or do some checking... */
/* since there is a call: widget = NULL, data = 0 (will be w_current) */
/* this function closes a window */
DEFINE_G_KEY(file_close)
DEFINE_G_KEY(file_quit)

/* Select also does not update the middle button shortcut */
DEFINE_G_KEY(edit_select)
DEFINE_G_KEY(edit_copy)
DEFINE_G_KEY(edit_copy_hotkey)
DEFINE_G_KEY(edit_move)
DEFINE_G_KEY(edit_move_hotkey)
DEFINE_G_KEY(edit_delete)
DEFINE_G_KEY(edit_rotate_90)
DEFINE_G_KEY(edit_rotate_90_hotkey)
DEFINE_G_KEY(edit_mirror)
DEFINE_G_KEY(edit_mirror_hotkey)
DEFINE_G_KEY(edit_stretch)
DEFINE_G_KEY(edit_stretch_hotkey)
DEFINE_G_KEY(edit_slot)
DEFINE_G_KEY(edit_color)
DEFINE_G_KEY(edit_edit)
DEFINE_G_KEY(edit_lock)
DEFINE_G_KEY(edit_unlock)
DEFINE_G_KEY(edit_translate)
DEFINE_G_KEY(edit_embed)
DEFINE_G_KEY(edit_unembed)
DEFINE_G_KEY(edit_show_hidden)

/* repeat middle shortcut doesn't make sense on redraw, just hit right
 * button */
DEFINE_G_KEY(view_redraw)

/* for these functions, repeat middle shortcut would get into the way
 * of what user is try to do */
DEFINE_G_KEY(view_zoom_full)
DEFINE_G_KEY(view_zoom_limits)
DEFINE_G_KEY(view_zoom_in)
DEFINE_G_KEY(view_zoom_out)
DEFINE_G_KEY(view_zoom_in_hotkey)
DEFINE_G_KEY(view_zoom_out_hotkey)

DEFINE_G_KEY(view_zoom_box)
DEFINE_G_KEY(view_zoom_box_hotkey)
DEFINE_G_KEY(view_pan)
DEFINE_G_KEY(view_pan_hotkey)
DEFINE_G_KEY(view_update_cues)
DEFINE_G_KEY(page_manager)
DEFINE_G_KEY(page_next)
DEFINE_G_KEY(page_prev)
DEFINE_G_KEY(page_new)
DEFINE_G_KEY(page_close)
DEFINE_G_KEY(page_discard)
DEFINE_G_KEY(page_print)
DEFINE_G_KEY(add_component)
DEFINE_G_KEY(add_attribute)
DEFINE_G_KEY(add_net)
DEFINE_G_KEY(add_net_hotkey)
DEFINE_G_KEY(add_bus)
DEFINE_G_KEY(add_bus_hotkey)
DEFINE_G_KEY(add_text)
DEFINE_G_KEY(add_line)
DEFINE_G_KEY(add_line_hotkey)
DEFINE_G_KEY(add_box)
DEFINE_G_KEY(add_box_hotkey)
DEFINE_G_KEY(add_circle)
DEFINE_G_KEY(add_circle_hotkey)
DEFINE_G_KEY(add_arc)
DEFINE_G_KEY(add_arc_hotkey)
DEFINE_G_KEY(add_pin)
DEFINE_G_KEY(add_pin_hotkey)
DEFINE_G_KEY(hierarchy_open_symbol)
DEFINE_G_KEY(attributes_attach)
DEFINE_G_KEY(attributes_detach)
DEFINE_G_KEY(attributes_show_name)
DEFINE_G_KEY(attributes_show_value)
DEFINE_G_KEY(attributes_show_both)
DEFINE_G_KEY(attributes_visibility_toggle)

/* i_callback_script_console is not currently implemented */
DEFINE_G_KEY(script_console)

/* repeat last command doesn't make sense on options either??? (does
 * it?) */
DEFINE_G_KEY(options_text_size)

/* repeat last command doesn't make sense on options either??? (does
 * it?) */
DEFINE_G_KEY(options_afeedback)
DEFINE_G_KEY(options_grid)
DEFINE_G_KEY(options_snap)
DEFINE_G_KEY(options_snap_size)
DEFINE_G_KEY(options_show_log_window)
DEFINE_G_KEY(options_show_coord_window)
DEFINE_G_KEY(misc)

DEFINE_G_KEY(help_about)
DEFINE_G_KEY(help_hotkeys)

/* be sure that you don't use the widget parameter in this one, since it is
being called with a null, I suppose we should call it with the right param.
hack */
DEFINE_G_KEY(cancel)
