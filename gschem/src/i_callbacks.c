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
#include <strings.h>
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
#include <libgeda/colors.h>
#include <libgeda/o_types.h>
#include <libgeda/prototype.h>

#include "../include/globals.h"
#include "../include/x_states.h"
#include "../include/prototype.h"

/* TODO: Kazu Hirata <kazu@seul.org> on July 13, 1999 - Probably we
 * need to use the following set of functions to deal with file
 * extention: adding and removing. Embedding this kind of code
 * directly into i_callback_ functions is mess. Something like the
 * following shuold help. The idea about the string manipulation is
 * based on Java in that it never changes the contents of strings and
 * just keeps creating new strings. */

#if 1
/* Kazu Hirata <kazu@seul.org> on July 14, 1999 - My own version of
 * strdup(). */
static char *
my_strdup(const char* p)
{
	char *q = (char *) malloc(strlen(p) + 1);
	if (q != NULL) {
		strcpy(q, p);
	}
	return q;
}

/* Kazu Hirata <kazu@seul.org> on July 14, 1999 - Returns a pointer to
 * the last '.' in the given string. If there is none, the function
 * returns NULL. If you want to change the extention using the return
 * value of the function, you need to do pointer arithmetic, assuming
 * your fname is defined as a constant. :-) */
const char *
fnameext_get(const char* fname)
{
	const char *p = strrchr(fname, '.');

	/* if '.' is the only '.' and is the first character, ignore
         * it */
	if(p == fname) {
		p = NULL;
	}
	return p;
}

/* Kazu Hirata <kazu@seul.org> on July 14, 1999 - The function removes
 * an extention including a '.' if any and returns the new string in a
 * newly allocated memory. If there is no '.' after the first
 * character, then the function simply returns a copy of fname. If
 * memory allocation fails, the function returns NULL. */
char *
fnameext_remove(const char *fname)
{
	const char *p = fnameext_get(fname);
	char *fname_new;
	int len;

	if(p == NULL) {
		fname_new = my_strdup(p);
	} else {
		len = (p - fname) + 1;
		fname_new = (char *) malloc(sizeof(char) * len);
		if(fname_new == NULL) {
			return NULL;
		}
		strncpy(fname_new, fname, len -1);
		fname_new[len] = '\0';
	}
	return fname_new;
}

/* Kazu Hirata <kazu@seul.org> on July 13, 1999 - The function adds an
 * extention and returns the new string in a newly allocated
 * memory. ext must not have '.'  as the first character unless you
 * wish an extention like '..c'. If memory allocation fails, the
 * function returns NULL. */
char *fnameext_add(const char *fname, const char* ext)
{
	char *p;
	
	/* TODO: use my_strdups */
	p = (char *) malloc(sizeof(char) *
			    strlen(fname) +
			    1 + /* the length of "." */
			    strlen(ext) +
			    1);
	if(p == NULL) {
		return NULL;
	}
	strcpy(p, fname);
	strcat(p, ".");
	strcat(p, ext);
	return p;
}
#endif

/* evey i_callback functions have the same footprint */
#define DEFINE_I_CALLBACK(name)				\
	void						\
	i_callback_ ## name(gpointer data,		\
			    guint callback_action,	\
			    GtkWidget *widget)

/* right now, all callbacks execpt for the ones on the File menu have
 * the middle button shortcut.  Let me (Ales) know if we should also
 * shortcut the File button */

/* File menu */

/* don't use the widget parameter on this function, or do some checking... */
/* since there is a call: widget = NULL, data = 0 (will be w_current hack) */
/* This should be renamed to page_new perhaps... */
DEFINE_I_CALLBACK(file_new)
{
	TOPLEVEL *w_current = (TOPLEVEL *) data;
	char *temp_filename;

	exit_if_null(w_current);

	/* TODO: probably 256 is enough, but shuold be something like
         * getcwd(NULL, 0); */
	if (getcwd(w_current->cwd, 256)) {
		/* TODO: the #'s are for 10 digits hack */
		/* TODO: use strdups */
		/* TODO: perform a NULL check */
		temp_filename = (char *) malloc(
			sizeof(char) * (strlen(w_current->cwd)+
					strlen(w_current->series_name) +
					strlen("/_##########.sch") +
					1));

		w_current->num_untitled++;
		sprintf(temp_filename, "%s/%s_%d.sch", w_current->cwd,
			w_current->series_name,
			w_current->num_untitled);
	} else {
		/* TODO: use strdups */
		/* TODO: perform a NULL check */
       		temp_filename = malloc(sizeof(char) * (
			strlen(w_current->series_name)+
			strlen("_##########.sch") +
			1));

		w_current->num_untitled++;
		sprintf(temp_filename, "%s_%d.sch",
			w_current->series_name,
			w_current->num_untitled);
	}

	/* in this function the filename is allocated and copied */
	s_page_new(w_current, temp_filename);
	s_log_message("New page created [%s]\n", temp_filename);
	free(temp_filename);

	i_set_filename(w_current, w_current->page_current->page_filename);

	update_page_manager(NULL, w_current);
	x_window_setup_world(w_current);
	x_manual_resize(w_current);
	x_hscrollbar_update(w_current);
	x_vscrollbar_update(w_current);
	x_repaint_background(w_current);
}

DEFINE_I_CALLBACK(file_new_window)
{
	TOPLEVEL *w_current = x_window_create_new();

	exit_if_null(w_current);

	if (getcwd(w_current->cwd, 256)) {
		if (w_current->page_current->page_filename) {
               	 	free(w_current->page_current->page_filename);
        	}

		/* TODO: use strdupts */
		/* TODO: perform a NULL check */
        	w_current->page_current->page_filename = malloc(
                       	         sizeof(char) * (
				 strlen(w_current->cwd)+
				 strlen(w_current->series_name)+
				 strlen("/_##########.sch")+1));

		w_current->num_untitled++;
		sprintf(w_current->page_current->page_filename, "%s/%s_%d.sch",
				w_current->cwd,
				w_current->series_name,
				w_current->num_untitled);
	} else {
		/* TODO: shouldn't this part do the same thing as
                 * what's equivalent in i_callback_file_new()? This is
                 * not nice to user. */
		fprintf(stderr, "Cannot obtain the current directory!\n");
	}

	s_log_message("New Window created\n");
	i_set_filename(w_current, w_current->page_current->page_filename);
}

/* don't use the widget parameter on this function, or do some checking... */
/* since there is a call: widget = NULL, data = 0 (will be w_current) */
/* This should be renamed to page_open perhaps... */
DEFINE_I_CALLBACK(file_open)
{
	TOPLEVEL *w_current = (TOPLEVEL *) data;

	exit_if_null(w_current);
	setup_open_file_selector(w_current);
}

DEFINE_I_CALLBACK(file_script)
{
	TOPLEVEL *w_current = (TOPLEVEL *) data;

	exit_if_null(w_current);
	setup_script_selector(w_current);
}

/* don't use the widget parameter on this function, or do some checking... */
/* since there is a call: widget = NULL, data = 0 (will be w_current) */
DEFINE_I_CALLBACK(file_save)
{
	TOPLEVEL *w_current = (TOPLEVEL *) data;

	exit_if_null(w_current);

	if (strstr(w_current->page_current->page_filename,
		   w_current->untitled_name)) {
		setup_saveas_file_selector(
			w_current,
			SAVEAS,
			w_current->page_current->page_filename);
	} else {
		s_log_message("Saved [%s]\n",
			      w_current->page_current->page_filename);

       		f_save(w_current, w_current->page_current->page_filename);

		/* don't know if should be kept going to select mode... */
		i_update_status(w_current, "Saved - Select Mode");
		w_current->event_state = SELECT;
       		w_current->page_current->CHANGED = 0;
		update_page_manager(NULL, w_current);
	}
}

/* don't use the widget parameter on this function, or do some checking... */
/* since there is a call: widget = NULL, data = 0 (will be w_current) */
DEFINE_I_CALLBACK(file_save_all)
{
	TOPLEVEL *w_current = (TOPLEVEL *) data;

	exit_if_null(w_current);

       	s_page_save_all(w_current);

	i_update_status(w_current, "Saved All - Select");
	w_current->event_state = SELECT;
	update_page_manager(NULL, w_current);
}

DEFINE_I_CALLBACK(file_save_as)
{
	TOPLEVEL *w_current = (TOPLEVEL *) data;

	exit_if_null(w_current);
	setup_saveas_file_selector(w_current,
				   SAVEAS,
				   w_current->page_current->page_filename);
}

DEFINE_I_CALLBACK(file_print)
{
	TOPLEVEL *w_current = (TOPLEVEL *) data;
	/* don't fix the size hack */
        char *ps_filename = NULL;
	char *c_ptr;
	int len;

	exit_if_null(w_current);

	/* TODO: use strdups */
	/* TODO: perform a NULL check */
	/* + 1 is for null character */
	/* this will be plenty of space since .sch is being replaced by .ps */
	len = strlen(w_current->page_current->page_filename) +
		strlen(".ps") + 1;
	ps_filename = (char *) malloc(sizeof(char) * len);

	strcpy(ps_filename, w_current->page_current->page_filename);
	/* HACK: .sch may not be at the end like .school.sch :-) */
	c_ptr = strstr(ps_filename, ".sch");
	if (c_ptr == NULL) {
		/* filename didn't have .sch extension */
		/* so append a .ps, string should be big enough */
		len = strlen(w_current->page_current->page_filename);
		strcpy(&ps_filename[len], ".ps");
	} else {
		/* found .sch extension, replace it with .ps */
		strcpy(c_ptr, ".ps");
	}

	if (output_filename) {
		x_print_setup(w_current, output_filename);
	} else {
		x_print_setup(w_current, ps_filename);
	}

#if 0
        f_print(w_current, ps_filename);
	s_log_message("Printed current schematic to [%s]\n", ps_filename);
#endif

	if (ps_filename) {
		free(ps_filename);
	}
}

DEFINE_I_CALLBACK(file_write_png)
{
	TOPLEVEL *w_current = (TOPLEVEL *) data;
	/* don't fix the size hack */
        char *img_filename = NULL;
	char *c_ptr;
	int len;

	exit_if_null(w_current);

#ifndef HAS_LIBGDGEDA
	/* TODO: integrate these to messages */
	fprintf(stderr,
		"libgdgeda not installed or disabled, "
		"so this feature is disabled\n");
	s_log_message("libgdgeda not installed or disabled, "
		      "so this feature is disabled\n");
	return;
#endif

	/* TODO: use strdupts */
	/* TODO: perform a NULL check */
	/* + 1 is for null character */
	/* this will be plenty of space since .sch is being replaced by .png */ 
	len = strlen(w_current->page_current->page_filename) + 
              strlen(".png") + 
	      1;

	img_filename = (char *) malloc(sizeof(char) * len);

	strcpy(img_filename, w_current->page_current->page_filename);

	/* HACK: .sch may not be at the end like .school.sch :-) */
	c_ptr = strstr(img_filename, ".sch");
	if (c_ptr == NULL) {
		/* filename didn't have .sch extension, so append a
		 * .ps, string should be big enough */
		len = strlen(w_current->page_current->page_filename);
		strcpy(&img_filename[len], ".png");
	} else {
		/* found .sch extension, replace it with .ps */
		strcpy(c_ptr, ".png");
	}

	if (output_filename) {
		x_image_setup(w_current, output_filename);
	} else {
		x_image_setup(w_current, img_filename);
	}

#if 0
        f_print(w_current, img_filename);
	s_log_message("Outputed current schematic to [%s]\n", img_filename);
#endif

	if (img_filename) {
		free(img_filename);
	}
}

/* don't use the widget parameter on this function, or do some checking... */
/* since there is a call: widget = NULL, data = 0 (will be w_current) */
/* this function closes a window */
DEFINE_I_CALLBACK(file_close)
{
	TOPLEVEL *w_current = (TOPLEVEL *) data;

	exit_if_null(w_current);

	s_log_message("Closing Window\n");
	x_window_close(w_current);
}

/* this function is called when you send a delete event to gschem */
/* Also DON'T ref the widget parameter since they can be null */
/* HACK: need a cleaner way of doing this hack */
/* this routine is used by the delete event signals */
int
i_callback_close(gpointer data, guint callback_action, GtkWidget *widget)
{
	TOPLEVEL *w_current = (TOPLEVEL *) data;

	exit_if_null(w_current);
	i_callback_file_close(w_current, 0, widget);
	return(FALSE);
}

DEFINE_I_CALLBACK(file_quit)
{
	TOPLEVEL *w_current = (TOPLEVEL *) data;

	exit_if_null(w_current);
	x_window_close_all();
}

/* Edit menu */

/* Select also does not update the middle button shortcut */
DEFINE_I_CALLBACK(edit_select)
{
	TOPLEVEL *w_current = (TOPLEVEL *) data;

	/* this is probably the only place this should be */
	w_current->event_state = SELECT;
        i_update_status(w_current, "Select Mode");
        w_current->inside_action = 0;
}

DEFINE_I_CALLBACK(edit_copy)
{
	TOPLEVEL *w_current = (TOPLEVEL *) data;

	exit_if_null(w_current);

	i_update_middle_button(w_current, i_callback_edit_copy, "Copy");
	if (w_current->page_current->selection_head->next != NULL) {
		i_update_status(w_current, "Copy Mode");
		w_current->event_state = STARTCOPY;
	} else {
		i_update_status(w_current, "Select objs first");
	}
}

DEFINE_I_CALLBACK(edit_copy_hotkey)
{
	TOPLEVEL *w_current = (TOPLEVEL *) data;

	exit_if_null(w_current);

	i_update_middle_button(w_current, i_callback_edit_copy_hotkey, "Copy");
	if (w_current->page_current->selection_head->next != NULL) {
		o_copy_start(w_current, mouse_x, mouse_y);
		w_current->event_state = ENDCOPY;
		w_current->inside_action = 1;
	}
}

DEFINE_I_CALLBACK(edit_move)
{
	TOPLEVEL *w_current = (TOPLEVEL *) data;

	exit_if_null(w_current);

	i_update_middle_button(w_current, i_callback_edit_move, "Move");
	if (w_current->page_current->selection_head->next != NULL) {
		i_update_status(w_current, "Move Mode");
		w_current->event_state = STARTMOVE;
	} else {
		i_update_status(w_current, "Select objs first");
	}
}

DEFINE_I_CALLBACK(edit_move_hotkey)
{
	TOPLEVEL *w_current = (TOPLEVEL *) data;

	exit_if_null(w_current);

	i_update_middle_button(w_current, i_callback_edit_move_hotkey, "Move");
	if (w_current->page_current->selection_head->next != NULL) {
		o_move_start(w_current, mouse_x, mouse_y);
		w_current->event_state = ENDMOVE;
		w_current->inside_action = 1;
	}
}

DEFINE_I_CALLBACK(edit_delete)
{
	TOPLEVEL *w_current = (TOPLEVEL *) data;

	exit_if_null(w_current);

	i_update_middle_button(w_current, i_callback_edit_delete, "Delete");

	if (w_current->page_current->selection_head->next != NULL) {
                o_delete(w_current);

		/* if you delete the objects you must go into select
		 * mode after the delete */
        	w_current->event_state = SELECT;
		i_update_status(w_current, "Select Mode");
        	w_current->inside_action = 0;
	}
}

DEFINE_I_CALLBACK(edit_edit)
{
	TOPLEVEL *w_current = (TOPLEVEL *) data;

	exit_if_null(w_current);

	i_update_middle_button(w_current, i_callback_edit_edit, "Edit");
	if (w_current->page_current->selection_head->next != NULL) {
		o_edit(w_current,
		       w_current->page_current->selection_head->next);
	}
}

DEFINE_I_CALLBACK(edit_slot)
{
	TOPLEVEL *w_current = (TOPLEVEL *) data;

	exit_if_null(w_current);

	i_update_middle_button(w_current, i_callback_edit_slot, "Slot");
	if (w_current->page_current->selection_head->next != NULL) {
		o_slot_start(w_current,
			     w_current->page_current->selection_head->next);
	}
}

DEFINE_I_CALLBACK(edit_color)
{
	TOPLEVEL *w_current = (TOPLEVEL *) data;

	exit_if_null(w_current);

	i_update_middle_button(w_current, i_callback_edit_color, "Color");

	color_edit_dialog(w_current);
}

/* rotate all objects in the selection list by 90 degrees */
DEFINE_I_CALLBACK(edit_rotate_90)
{
        TOPLEVEL *w_current = (TOPLEVEL *) data;

        exit_if_null(w_current);

        i_update_status(w_current, "Rotate Mode");
        i_update_middle_button(w_current, i_callback_edit_rotate_90, "Rotate");

        w_current->event_state = ENDROTATEP;
}

/* rotate all objects in the selection list by 90 degrees */
DEFINE_I_CALLBACK(edit_rotate_90_hotkey)
{
        TOPLEVEL *w_current = (TOPLEVEL *) data;

        exit_if_null(w_current);

        i_update_middle_button(w_current,
			       i_callback_edit_rotate_90_hotkey, "Rotate");

	o_rotate_90(w_current, w_current->page_current->selection_head->next,
		    mouse_x, mouse_y);

        w_current->event_state = SELECT;
        w_current->inside_action = 0;
}

DEFINE_I_CALLBACK(edit_mirror)
{
        TOPLEVEL *w_current = (TOPLEVEL *) data;

        exit_if_null(w_current);

        i_update_status(w_current, "Mirror Mode");
        i_update_middle_button(w_current, i_callback_edit_mirror, "Mirror");

        w_current->event_state = ENDMIRROR;
}

DEFINE_I_CALLBACK(edit_mirror_hotkey)
{
        TOPLEVEL *w_current = (TOPLEVEL *) data;

        exit_if_null(w_current);

        i_update_middle_button(w_current,
			       i_callback_edit_mirror_hotkey, "Mirror");

	o_mirror(w_current, w_current->page_current->selection_head->next,
		mouse_x, mouse_y);

        w_current->event_state = SELECT;
        w_current->inside_action = 0;
}

/* locks all objects in selection list */
DEFINE_I_CALLBACK(edit_lock)
{
	TOPLEVEL *w_current = (TOPLEVEL *) data;

	exit_if_null(w_current);

	i_update_middle_button(w_current, i_callback_edit_lock, "Lock");
	if (w_current->page_current->selection_head->next != NULL) {
		o_lock(w_current);
	}
}

/* unlocks all objects in selection list */
DEFINE_I_CALLBACK(edit_unlock)
{
	TOPLEVEL *w_current = (TOPLEVEL *) data;

	exit_if_null(w_current);

	i_update_middle_button(w_current, i_callback_edit_unlock, "Unlock");
	if (w_current->page_current->selection_head->next != NULL) {
		o_unlock(w_current);
	}
}

DEFINE_I_CALLBACK(edit_translate)
{
	TOPLEVEL *w_current = (TOPLEVEL *) data;

	exit_if_null(w_current);

	i_update_middle_button(w_current,
			       i_callback_edit_translate, "Translate");

	if (w_current->snap == 0) {
		s_log_message("WARNING: Do not translate with snap off!\n");
		s_log_message("WARNING: Turning snap on and continuing "
			      "with translate.\n");
		w_current->snap = 1;
        }

	if (w_current->snap_size != 100) {
		s_log_message("WARNING: Snap grid size is "
			      "not equal to 100!\n");
		s_log_message("WARNING: If you are translating a symbol "
			      "to the origin, the snap grid size should be "
			      "set to 100\n");
        }

	translate_dialog(w_current);
}

/* embedds all objects in selection list */
DEFINE_I_CALLBACK(edit_embed)
{
	TOPLEVEL *w_current = (TOPLEVEL *) data;

	exit_if_null(w_current);

	i_update_middle_button(w_current, i_callback_edit_embed, "Embed");
	if (w_current->page_current->selection_head->next != NULL) {
		o_embed(w_current);
	}
}

/* unembedds all objects in selection list */
DEFINE_I_CALLBACK(edit_unembed)
{
	TOPLEVEL *w_current = (TOPLEVEL *) data;

	exit_if_null(w_current);

	i_update_middle_button(w_current, i_callback_edit_unembed, "Unembed");
	if (w_current->page_current->selection_head->next != NULL) {
		o_unembed(w_current);
	}
}

DEFINE_I_CALLBACK(edit_show_hidden)
{
	TOPLEVEL *w_current = (TOPLEVEL *) data;

	exit_if_null(w_current);

	/* This is a new addition 3/15 to prevent this from executing
	 * inside an action */
	if (w_current->inside_action)
		return;

	i_update_middle_button(w_current,
			       i_callback_attributes_visibility_toggle,
			       "ShowHidden");

	o_edit_show_hidden(w_current, w_current->page_current->object_head);
}

/* View menu */

/* repeat middle shortcut doesn't make sense on redraw, just hit right
 * button */
DEFINE_I_CALLBACK(view_redraw)
{
	TOPLEVEL *w_current = (TOPLEVEL *) data;

	exit_if_null(w_current);
	o_redraw_all(w_current);
}

/* repeat middle shortcut would get into the way of what user is try to do */
DEFINE_I_CALLBACK(view_zoom_full)
{
	TOPLEVEL *w_current = (TOPLEVEL *) data;

	exit_if_null(w_current);

	/* scroll bar stuff */
	a_zoom(w_current, ZOOM_FULL, DONTCARE);
	w_current->DONT_REDRAW = 1;
	w_current->DONT_RECALC = 1;
	w_current->DONT_RESIZE = 1;
	x_hscrollbar_update(w_current);
	x_vscrollbar_update(w_current);
	o_redraw_all(w_current);
	w_current->DONT_RESIZE = 0;
	w_current->DONT_RECALC = 0;
	w_current->DONT_REDRAW = 0;
}

/* repeat middle shortcut would get into the way of what user is try to do */
DEFINE_I_CALLBACK(view_zoom_limits)
{
	TOPLEVEL *w_current = (TOPLEVEL *) data;

	exit_if_null(w_current);

	/* scroll bar stuff */
	a_zoom_limits(w_current, w_current->page_current->object_head);
	w_current->DONT_REDRAW = 1;
	w_current->DONT_RECALC = 1;
	w_current->DONT_RESIZE = 1;
	x_hscrollbar_update(w_current);
	x_vscrollbar_update(w_current);
	o_redraw_all(w_current);
	w_current->DONT_RESIZE = 0;
	w_current->DONT_RECALC = 0;
	w_current->DONT_REDRAW = 0;
}

/* repeat middle shortcut would get into the way of what user is try to do */
DEFINE_I_CALLBACK(view_zoom_box)
{
	TOPLEVEL *w_current = (TOPLEVEL *) data;

	exit_if_null(w_current);

	w_current->event_state = ZOOMBOXSTART;
	i_update_status2(w_current, "Zoom Box");
        w_current->inside_action = 0;
}

DEFINE_I_CALLBACK(view_zoom_box_hotkey)
{
	TOPLEVEL *w_current = (TOPLEVEL *) data;

	exit_if_null(w_current);

	i_update_status2(w_current, "Zoom Box");

 	a_zoom_box_start(w_current, mouse_x, mouse_y);

 	w_current->event_state = ZOOMBOXEND;
	w_current->inside_action = 1;
}

/* repeat middle shortcut would get into the way of what user is try to do */
DEFINE_I_CALLBACK(view_zoom_in)
{
	TOPLEVEL *w_current = (TOPLEVEL *) data;

	exit_if_null(w_current);

	a_zoom(w_current, ZOOM_IN, MENU);
	w_current->DONT_REDRAW = 1;
	w_current->DONT_RECALC = 1;
	w_current->DONT_RESIZE = 1;
	x_hscrollbar_update(w_current);
	x_vscrollbar_update(w_current);
	o_redraw_all(w_current);
	w_current->DONT_RESIZE = 0;
	w_current->DONT_RECALC = 0;
	w_current->DONT_REDRAW = 0;
}

/* repeat middle shortcut would get into the way of what user is try to do */
DEFINE_I_CALLBACK(view_zoom_out)
{
	TOPLEVEL *w_current = (TOPLEVEL *) data;

	exit_if_null(w_current);

	a_zoom(w_current, ZOOM_OUT, MENU);
	w_current->DONT_REDRAW = 1;
	w_current->DONT_RECALC = 1;
	w_current->DONT_RESIZE = 1;
	x_hscrollbar_update(w_current);
	x_vscrollbar_update(w_current);
	o_redraw_all(w_current);
	w_current->DONT_RESIZE = 0;
	w_current->DONT_RECALC = 0;
	w_current->DONT_REDRAW = 0;
}

/* repeat middle shortcut would get into the way of what user is try
 * to do */
DEFINE_I_CALLBACK(view_zoom_in_hotkey)
{
	TOPLEVEL *w_current = (TOPLEVEL *) data;

	exit_if_null(w_current);

	a_zoom(w_current, ZOOM_IN, HOTKEY);
	w_current->DONT_REDRAW = 1;
	w_current->DONT_RECALC = 1;
	w_current->DONT_RESIZE = 1;
	x_hscrollbar_update(w_current);
	x_vscrollbar_update(w_current);
	o_redraw_all(w_current);
	w_current->DONT_RESIZE = 0;
	w_current->DONT_RECALC = 0;
	w_current->DONT_REDRAW = 0;
}

/* repeat middle shortcut would get into the way of what user is try to do */
DEFINE_I_CALLBACK(view_zoom_out_hotkey)
{
	TOPLEVEL *w_current = (TOPLEVEL *) data;

	exit_if_null(w_current);

	a_zoom(w_current, ZOOM_OUT, HOTKEY);
	w_current->DONT_REDRAW = 1;
	w_current->DONT_RECALC = 1;
	w_current->DONT_RESIZE = 1;
	x_hscrollbar_update(w_current);
	x_vscrollbar_update(w_current);
	o_redraw_all(w_current);
	w_current->DONT_RESIZE = 0;
	w_current->DONT_RECALC = 0;
	w_current->DONT_REDRAW = 0;
}

DEFINE_I_CALLBACK(view_pan)
{
	TOPLEVEL *w_current = (TOPLEVEL *) data;

	exit_if_null(w_current);

	w_current->event_state = STARTPAN;
	i_update_status(w_current, "Pan Mode");

	/* I don't know if this would get in the way */
	i_update_middle_button(w_current, i_callback_view_pan, "Pan");
	w_current->inside_action = 0;
}

DEFINE_I_CALLBACK(view_pan_hotkey)
{
	TOPLEVEL *w_current = (TOPLEVEL *) data;

	exit_if_null(w_current);

	/* I don't know if this would get in the way */
	i_update_middle_button(w_current, i_callback_view_pan_hotkey, "Pan");

	/*
         * I have NO idea what ramifications removing the next line has,
	 * only that it makes the code work when drawing a net and panning
	 * at the same time.  Jeff McNeal - 11-19-98
	 * w_current->inside_action = 0;
	 * I think it's okay - Ales 12/13/98 */

	a_pan(w_current, mouse_x, mouse_y);

	/* if we are drawing a net, don't change the event state,
	 * because we want to continue drawing a net.  If we are
	 * just panning, then continue in select mode.
	 * Jeff McNeal - 11-19-98 */
	if(!(w_current->event_state == DRAWNET ||
	     w_current->event_state == NETCONT ||
             w_current->event_state == STARTDRAWNET )) {
		w_current->event_state = SELECT;
		i_update_status(w_current, "Select Mode");
	}
}

DEFINE_I_CALLBACK(view_updatenets)
{
	TOPLEVEL *w_current = (TOPLEVEL *) data;

	i_update_middle_button(w_current,
			       i_callback_view_updatenets, "Update");

	o_ales_disconnect_update(w_current->page_current);
        o_redraw_all(w_current);
}

DEFINE_I_CALLBACK(page_manager)
{
	TOPLEVEL *w_current = (TOPLEVEL *) data;

	exit_if_null(w_current);

	setup_page_selector(w_current);
}

DEFINE_I_CALLBACK(page_next)
{
	TOPLEVEL *w_current = (TOPLEVEL *) data;

	exit_if_null(w_current);

	if (w_current->page_current->next != NULL &&
	    w_current->page_current->next->pid != -1) {

		w_current->page_current = w_current->page_current->next;
	}

	s_page_goto(w_current, w_current->page_current);
        i_set_filename(w_current, w_current->page_current->page_filename);
	x_scrollbars_update(w_current);
	o_redraw_all(w_current);
	update_page_manager(NULL, w_current);

}

DEFINE_I_CALLBACK(page_prev)
{
	TOPLEVEL *w_current = (TOPLEVEL *) data;

	exit_if_null(w_current);

	if (w_current->page_current->prev != NULL &&
			w_current->page_current->prev->pid != -1) {

		w_current->page_current = w_current->page_current->prev;
	}

	s_page_goto(w_current, w_current->page_current);
	i_set_filename(w_current, w_current->page_current->page_filename);
	x_scrollbars_update(w_current);
	o_redraw_all(w_current);
	update_page_manager(NULL, w_current);
}

DEFINE_I_CALLBACK(page_new)
{
	TOPLEVEL *w_current = (TOPLEVEL *) data;
	char *filename = NULL;

	exit_if_null(w_current);

	if (getcwd(w_current->cwd, 256)) {
		/* TODO: use strdups */
		/* TODO: perform a NULL check */
		/* the 20 is a hack and needs to be changed */
        	filename = malloc(sizeof(char) *
				  (strlen(w_current->cwd) +
				   strlen(w_current->series_name) +
				   strlen("/_##########.sch") +
				   1));

		w_current->num_untitled++;
		sprintf(filename, "%s/%s_%d.sch",
			w_current->cwd,
			w_current->series_name,
			w_current->num_untitled);

	} else {
		fprintf(stderr, "Cannot get cwd!\n");
	}

	s_page_new(w_current, filename);

	update_page_manager(NULL, w_current);
	i_set_filename(w_current, w_current->page_current->page_filename);
	s_log_message("New Page created [%s]\n",
		      w_current->page_current->page_filename);
	o_redraw_all(w_current);
}

DEFINE_I_CALLBACK(page_close)
{
	TOPLEVEL *w_current = (TOPLEVEL *) data;
	PAGE *p_current;

	exit_if_null(w_current);

	if (w_current->page_current->CHANGED) {
		setup_saveas_file_selector(
			w_current, CLOSE,
			w_current->page_current->page_filename);
		return;
	}

	if (w_current->page_current->prev) {
		if (w_current->page_current->prev->pid != -1) {
			p_current = w_current->page_current->prev;
			s_log_message("Closing [%s]\n",
				      w_current->page_current->page_filename);
			s_page_free(w_current, w_current->page_current);
			w_current->page_current = p_current;

			s_page_goto(w_current, w_current->page_current);
			i_set_filename(w_current,
				       w_current->page_current->page_filename);
			x_scrollbars_update(w_current);
			o_redraw_all(w_current);
			update_page_manager(NULL, w_current);
			return;
		}
	}

	if (w_current->page_current->next) {
		if (w_current->page_current->next->pid != -1) {
			p_current = w_current->page_current->next;
			s_log_message("Closing [%s]\n",
				      w_current->page_current->page_filename);
			s_page_free(w_current, w_current->page_current);
			w_current->page_current = p_current;
			s_page_goto(w_current, w_current->page_current);
			i_set_filename(w_current,
				       w_current->page_current->page_filename);
			x_scrollbars_update(w_current);
			o_redraw_all(w_current);
			update_page_manager(NULL, w_current);
			return;
		}
	}

	/* finally go here if you can't delete the page */
	s_log_message("Cannot close current page\n");
}

DEFINE_I_CALLBACK(page_discard)
{
	TOPLEVEL *w_current = (TOPLEVEL *) data;
	PAGE *p_current;

	exit_if_null(w_current);

	if (w_current->page_current->prev) {
		if (w_current->page_current->prev->pid != -1) {
			p_current = w_current->page_current->prev;
			s_log_message("Discarding page [%s]\n",
				      w_current->page_current->page_filename);
			s_page_free(w_current, w_current->page_current);
			w_current->page_current = p_current;
			s_page_goto(w_current, w_current->page_current);
			i_set_filename(w_current,
				       w_current->page_current->page_filename);
			x_scrollbars_update(w_current);
			o_redraw_all(w_current);
			update_page_manager(NULL, w_current);
			return;
		}
	}

	if (w_current->page_current->next) {
		if (w_current->page_current->next->pid != -1) {
			p_current = w_current->page_current->next;
			s_log_message("Discarding page [%s]\n",
				      w_current->page_current->page_filename);
			s_page_free(w_current, w_current->page_current);
			w_current->page_current = p_current;
			s_page_goto(w_current, w_current->page_current);
			i_set_filename(w_current,
				       w_current->page_current->page_filename);
			x_scrollbars_update(w_current);
			o_redraw_all(w_current);
			update_page_manager(NULL, w_current);
			return;
		}
	}

	/* finally go here if you can't delete the page */
	s_log_message("Cannot close current page\n");
}

DEFINE_I_CALLBACK(page_print)
{
	TOPLEVEL *w_current = (TOPLEVEL *) data;

	s_page_print_all(w_current);
}

/* Add menu */
DEFINE_I_CALLBACK(add_component)
{
	TOPLEVEL *w_current = (TOPLEVEL *) data;

	exit_if_null(w_current);

	setup_place_file_selector(w_current);
	i_update_middle_button(w_current,
			       i_callback_add_component, "Component");
	i_update_status2(w_current, "Select Mode");
}

DEFINE_I_CALLBACK(add_attribute)
{
	TOPLEVEL *w_current = (TOPLEVEL *) data;

	exit_if_null(w_current);

	setup_attr_selector(w_current);
	i_update_middle_button(w_current, i_callback_add_attribute,
			       "Attribute");
	i_update_status2(w_current, "Select Mode");
}

DEFINE_I_CALLBACK(add_net)
{
	TOPLEVEL *w_current = (TOPLEVEL *) data;

	exit_if_null(w_current);

	/* need to click */
	w_current->event_state = STARTDRAWNET;
	i_update_middle_button(w_current, i_callback_add_net, "Net");
	i_update_status2(w_current, "Net Mode");
	/* somewhere you need to nearest point locking... */
	w_current->inside_action = 0;
}

DEFINE_I_CALLBACK(add_net_hotkey)
{
	TOPLEVEL *w_current = (TOPLEVEL *) data;

	exit_if_null(w_current);

	/* need to click */
	i_update_middle_button(w_current, i_callback_add_net_hotkey, "Net");
	i_update_status2(w_current, "Net Mode");

	w_current->event_state = STARTDRAWNET;

	o_net_start(w_current, mouse_x, mouse_y);

	w_current->event_state=DRAWNET;
	w_current->inside_action = 1;
}

DEFINE_I_CALLBACK(add_text)
{
	TOPLEVEL *w_current = (TOPLEVEL *) data;

	exit_if_null(w_current);

	text_input_dialog(w_current);
        w_current->inside_action = 0;
}

DEFINE_I_CALLBACK(add_line)
{
	TOPLEVEL *w_current = (TOPLEVEL *) data;

	exit_if_null(w_current);

	w_current->event_state = DRAWLINE;
	i_update_middle_button(w_current, i_callback_add_line, "Line");
	i_update_status2(w_current, "Line Mode");
        w_current->inside_action = 0;
}

DEFINE_I_CALLBACK(add_line_hotkey)
{
	TOPLEVEL *w_current = (TOPLEVEL *) data;

	exit_if_null(w_current);

	i_update_middle_button(w_current, i_callback_add_line_hotkey, "Line");
	i_update_status2(w_current, "Line Mode");

 	o_line_start(w_current, mouse_x, mouse_y);

 	w_current->event_state = ENDLINE;
	w_current->inside_action = 1;
}

DEFINE_I_CALLBACK(add_box)
{
	TOPLEVEL *w_current = (TOPLEVEL *) data;

	exit_if_null(w_current);

	w_current->event_state = DRAWBOX;
	i_update_middle_button(w_current, i_callback_add_box, "Box");
	i_update_status2(w_current, "Box Mode");
        w_current->inside_action = 0;
}

DEFINE_I_CALLBACK(add_box_hotkey)
{
	TOPLEVEL *w_current = (TOPLEVEL *) data;

	exit_if_null(w_current);

	i_update_middle_button(w_current, i_callback_add_box_hotkey, "Box");
	i_update_status2(w_current, "Box Mode");

 	o_box_start(w_current, mouse_x, mouse_y);

 	w_current->event_state = ENDBOX;
	w_current->inside_action = 1;
}

DEFINE_I_CALLBACK(add_circle)
{
	TOPLEVEL *w_current = (TOPLEVEL *) data;

	exit_if_null(w_current);

	w_current->event_state = DRAWCIRCLE;
	i_update_middle_button(w_current, i_callback_add_circle, "Circle");
	i_update_status2(w_current, "Circle Mode");
	w_current->inside_action = 0;
}

DEFINE_I_CALLBACK(add_circle_hotkey)
{
	TOPLEVEL *w_current = (TOPLEVEL *) data;

	exit_if_null(w_current);

	i_update_middle_button(w_current, i_callback_add_circle_hotkey,
			       "Circle");
	i_update_status2(w_current, "Circle Mode");

        o_circle_start(w_current, mouse_x, mouse_y);

	w_current->event_state = ENDCIRCLE;
	w_current->inside_action = 1;
}

DEFINE_I_CALLBACK(add_arc)
{
	TOPLEVEL *w_current = (TOPLEVEL *) data;

	exit_if_null(w_current);

	w_current->event_state = DRAWARC;
	i_update_middle_button(w_current, i_callback_add_arc, "Arc");
	i_update_status2(w_current, "Arc Mode");
	w_current->inside_action = 0;
}

DEFINE_I_CALLBACK(add_arc_hotkey)
{
	TOPLEVEL *w_current = (TOPLEVEL *) data;

	exit_if_null(w_current);

	i_update_middle_button(w_current, i_callback_add_arc_hotkey, "Arc");
	i_update_status2(w_current, "Arc Mode");

        o_arc_start(w_current, mouse_x, mouse_y);

	w_current->event_state = ENDARC;
	w_current->inside_action = 1;
}

DEFINE_I_CALLBACK(add_pin)
{
	TOPLEVEL *w_current = (TOPLEVEL *) data;

	exit_if_null(w_current);

	w_current->event_state = DRAWPIN;
	i_update_middle_button(w_current, i_callback_add_pin, "Pin");
	i_update_status2(w_current, "Pin Mode");
        w_current->inside_action = 0;
}

DEFINE_I_CALLBACK(add_pin_hotkey)
{
	TOPLEVEL *w_current = (TOPLEVEL *) data;

	exit_if_null(w_current);

	i_update_middle_button(w_current, i_callback_add_pin_hotkey, "Pin");
	i_update_status2(w_current, "Pin Mode");

	o_pin_start(w_current, mouse_x, mouse_y);

	w_current->event_state = ENDPIN;
	w_current->inside_action = 1;
}

/* Hierarchy menu */
DEFINE_I_CALLBACK(hierarchy_open_symbol)
{
	TOPLEVEL *w_current = (TOPLEVEL *) data;
	char *filename;

	exit_if_null(w_current);

	if (w_current->page_current->selection_head->next != NULL) {
		/* only allow going into symbols */
		if (w_current->page_current->selection_head->next->
		    type == OBJ_COMPLEX) {
			filename = w_current->page_current->selection_head->
				next->complex_basename;
			s_log_message("Searching for source [%s]\n", filename);
			s_hierarchy_load_all(w_current, filename);
			i_set_filename(w_current,
				       w_current->page_current->page_filename);
			o_redraw_all(w_current);
			update_page_manager(NULL, w_current);
		}
        }
}

/* Attributes menu */
DEFINE_I_CALLBACK(attributes_attach)
{
	TOPLEVEL *w_current = (TOPLEVEL *) data;

	exit_if_null(w_current);

	/* This is a new addition 3/15 to prevent this from executing
	 * inside an action */
	if (w_current->inside_action) {
		return;
	}

	/* do we want to update the shortcut outside of the ifs? */
	/* probably, if this fails the user may want to try again */
	i_update_middle_button(w_current, i_callback_attributes_attach,
			       "Attach");

	if (w_current->page_current->selection_head->next != NULL) {
                if (w_current->page_current->selection_head->next->next !=
		    NULL) {
			/* HACK: fix this? next next bit? hack */
                        o_attrib_attach(
				w_current,
				w_current->page_current->object_head,
				w_current->page_current->selection_head->
				next->next,
				w_current->page_current->selection_head->next);
		}
	}
}

DEFINE_I_CALLBACK(attributes_detach)
{
	TOPLEVEL *w_current = (TOPLEVEL *) data;

	exit_if_null(w_current);

	/* This is a new addition 3/15 to prevent this from executing
	 * inside an action */
	if (w_current->inside_action) {
		return;
	}

	/* same note as above on i_update_middle_button */
	i_update_middle_button(w_current, i_callback_attributes_detach,
			       "Detach");

	if (w_current->page_current->selection_head->next != NULL) {
                if (w_current->page_current->selection_head->next->next !=
		    NULL) {
                        o_attrib_detach_all(
				w_current,
				w_current->page_current->selection_head,
				w_current->page_current->object_head);
                        o_redraw_selected(w_current);
		}
	}
}

DEFINE_I_CALLBACK(attributes_show_name)
{
	TOPLEVEL *w_current = (TOPLEVEL *) data;

	exit_if_null(w_current);

	/* This is a new addition 3/15 to prevent this from executing
	 * inside an action */
	if (w_current->inside_action) {
		return;
	}

	i_update_middle_button(w_current, i_callback_attributes_show_name,
			       "ShowN");

	if (w_current->page_current->selection_head->next != NULL) {
		o_attrib_toggle_show_name_value(
			w_current,
			w_current->page_current->selection_head->next,
			SHOW_NAME);
        }
}

DEFINE_I_CALLBACK(attributes_show_value)
{
	TOPLEVEL *w_current = (TOPLEVEL *) data;

	exit_if_null(w_current);

	/* This is a new addition 3/15 to prevent this from executing
	 * inside an action */
	if (w_current->inside_action) {
		return;
	}

	i_update_middle_button(w_current, i_callback_attributes_show_value,
			       "ShowV");

	if (w_current->page_current->selection_head->next != NULL) {
		o_attrib_toggle_show_name_value(
			w_current,
			w_current->page_current->selection_head->next,
			SHOW_VALUE);
        }
}

DEFINE_I_CALLBACK(attributes_show_both)
{
	TOPLEVEL *w_current = (TOPLEVEL *) data;

	exit_if_null(w_current);

	/* This is a new addition 3/15 to prevent this from executing
	 * inside an action */
	if (w_current->inside_action) {
		return;
	}

	i_update_middle_button(w_current, i_callback_attributes_show_both,
			       "ShowB");

	if (w_current->page_current->selection_head->next != NULL) {
		o_attrib_toggle_show_name_value(
			w_current,
			w_current->page_current->selection_head->next,
			SHOW_NAME_VALUE);
        }
}

DEFINE_I_CALLBACK(attributes_visibility_toggle)
{
	TOPLEVEL *w_current = (TOPLEVEL *) data;

	exit_if_null(w_current);

	/* This is a new addition 3/15 to prevent this from executing
	 * inside an action */
	if (w_current->inside_action) {
		return;
	}

	i_update_middle_button(w_current,
			       i_callback_attributes_visibility_toggle,
			       "VisToggle");

	if (w_current->page_current->selection_head->next != NULL) {
		o_attrib_toggle_visibility(
			w_current,
			w_current->page_current->selection_head->next);
        }
}

/* Script menu */
/* not currently implemented */
DEFINE_I_CALLBACK(script_console)
{
	TOPLEVEL *w_current = (TOPLEVEL *) data;

	exit_if_null(w_current);
	printf("Sorry but this is a non-functioning menu option\n");
}

/* Layers menu */

/* Options menu */

/* repeat last command doesn't make sense on options either??? (does it?) */
DEFINE_I_CALLBACK(options_text_size)
{
	TOPLEVEL *w_current = (TOPLEVEL *) data;

	exit_if_null(w_current);
	text_size_dialog(w_current);
}

DEFINE_I_CALLBACK(options_snap_size)
{
	TOPLEVEL *w_current = (TOPLEVEL *) data;

	exit_if_null(w_current);
	snap_size_dialog(w_current);
}

/* repeat last command doesn't make sense on options either??? (does
 * it?) */
DEFINE_I_CALLBACK(options_afeedback)
{
	TOPLEVEL *w_current = (TOPLEVEL *) data;

	exit_if_null(w_current);

	if (w_current->actionfeedback_mode == BOUNDINGBOX) {
                w_current->actionfeedback_mode = OUTLINE;
		s_log_message("Action feedback mode set to OUTLINE\n");
	} else {
                w_current->actionfeedback_mode = BOUNDINGBOX;
		s_log_message("Action feedback mode set to BOUNDINGBOX\n");
	}
}

DEFINE_I_CALLBACK(options_grid)
{
	TOPLEVEL *w_current = (TOPLEVEL *) data;

	exit_if_null(w_current);

        if (w_current->grid) {
                w_current->grid = 0;
		s_log_message("Grid OFF\n");
	} else {
                w_current->grid = 1;
		s_log_message("Grid ON\n");
	}

        o_redraw_all(w_current);
}

DEFINE_I_CALLBACK(options_snap)
{
	TOPLEVEL *w_current = (TOPLEVEL *) data;

        if (w_current->snap) {
                w_current->snap = 0;
		s_log_message("Snap OFF (CAUTION!)\n");
        } else {
                w_current->snap = 1;
		s_log_message("Snap ON\n");
	}
}

DEFINE_I_CALLBACK(options_show_status)
{
	TOPLEVEL *w_current = (TOPLEVEL *) data;

	exit_if_null(w_current);
	x_log_setup_win(w_current);
}

/* for now this prints out the font_set structure */
DEFINE_I_CALLBACK(misc)
{
	TOPLEVEL *w_current = (TOPLEVEL *) data;

	o_ales_print_hash(w_current->page_current->ales_table);
	/* o_ntext_print_set();*/
}

/* HACK: be sure that you don't use the widget parameter in this one,
 * since it is being called with a null, I suppose we should call it
 * with the right param. */
DEFINE_I_CALLBACK(cancel)
{
	TOPLEVEL *w_current = (TOPLEVEL *) data;

	exit_if_null(w_current);

	w_current->inside_action=0;

	/* leave this on for now... but it might have to change */
	/* this is problematic since we don't know what the right mode */
	/* (when you cancel inside an action) should be */
	w_current->event_state = SELECT;
	i_update_status(w_current, "Select Mode");

        o_redraw_all(w_current); /* do I want to deselect sel */

	/* clear the key guile command-sequence */
        gh_eval_str("(set! current-command-sequence '())");

        /* see above comment hack */
#if 0
	set_cursor_normal();
#endif

        /* it is possible to cancel in the middle of a complex place
         * so lets be sure to clean up the complex_place_head
         * structure and also clean up the attrib_place_head.
         * remember these don't remove the head structure */
        o_list_delete_rest(w_current,
			   w_current->page_current->complex_place_head);
        o_list_delete_rest(w_current,
			   w_current->page_current->attrib_place_head);

	/* also free internal current_attribute */
	o_attrib_free_current(w_current);
}

DEFINE_I_CALLBACK(help_about)
{
	TOPLEVEL *w_current = (TOPLEVEL *) data;

	exit_if_null(w_current);
	about_dialog(w_current);
}

DEFINE_I_CALLBACK(options_show_coord)
{
	TOPLEVEL *w_current = (TOPLEVEL *) data;

	exit_if_null(w_current);
	coord_dialog (w_current, mouse_x, mouse_y);
}

#if 0 /* experimental */
DEFINE_I_CALLBACK(preview)
{
	TOPLEVEL *w_current = (TOPLEVEL *) data;

	exit_if_null(w_current);
	setup_preview(w_current);
}
#endif
