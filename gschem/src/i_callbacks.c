/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 * Copyright (C) 1998-2000 Ales V. Hvezda
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
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <libgeda/libgeda.h>

#include "../include/globals.h"
#include "../include/x_states.h"
#include "../include/prototype.h"

#define DELIMITERS ", "

/* Kazu Hirata <kazu@seul.org> on July 25, 1999 - Returns a pointer to
 * the last '.' in the given string. If there is none, the function
 * returns a pointer to the first null character in the string. If you
 * want to change the extention using the return value of the
 * function, you need to do pointer arithmetic, assuming your fname is
 * defined as a constant. :-) Note that, if the only '.' appears as
 * the first character, it is ignored. */
static const char *
fnameext_get(const char* fname)
{
	const char *p = strrchr(fname, '.');

	if((p == NULL) || (p == fname)) {
		p = &fname[strlen(fname)];
	}
	return p;
}

/* Kazu Hirata <kazu@seul.org> on July 25, 1999 - The function removes
 * an extention including a '.' if any and returns the new string in a
 * newly allocated memory. If there is no '.' after the first
 * character, then the function simply returns a copy of fname. If
 * memory allocation fails, the function returns NULL. */
static char *
fnameext_remove(const char *fname)
{
	const char *p = fnameext_get(fname);
	char *fname_new;
	int len;

	if(*p == '\0') {
		fname_new = u_basic_strdup(p);
	} else {
		len = (p - fname); /*  + 1; this extra was causing grief */
		fname_new = (char *) malloc(sizeof(char) * (len + 1));
		if(fname_new == NULL) {
			return NULL;
		}
		strncpy(fname_new, fname, len);
		fname_new[len] = '\0';
	}
	return fname_new;
}

/* Kazu Hirata <kazu@seul.org> on July 25, 1999 - The function adds an
 * extention and returns the new string in a newly allocated
 * memory. ext must have '.'  as the first character. If memory
 * allocation fails, the function returns NULL. */
static char *
fnameext_add(const char *fname, const char* ext)
{
	return u_basic_strdup_multiple(fname, ext, NULL);
}

/* evey i_callback functions have the same footprint */
#define DEFINE_I_CALLBACK(name)				\
	void						\
	i_callback_ ## name(gpointer data,		\
			    guint callback_action,	\
			    GtkWidget *widget)

/* right now, all callbacks execpt for the ones on the File menu have
 * the middle button shortcut. Let me (Ales) know if we should also
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
	o_undo_savestate(w_current, UNDO_ALL);
}

DEFINE_I_CALLBACK(file_new_window)
{
	TOPLEVEL *w_current;
       
	w_current = x_window_create_new();

	exit_if_null(w_current);

	if (getcwd(w_current->cwd, 256)) {
		if (w_current->page_current->page_filename) {
               	 	free(w_current->page_current->page_filename);
        	}

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

/* don't use the widget parameter on this function, or do some
 * checking... */
/* since there is a call: widget = NULL, data = 0 (will be w_current) */
/* This should be renamed to page_open perhaps... */
DEFINE_I_CALLBACK(file_open)
{

	TOPLEVEL *w_current = (TOPLEVEL *) data;

	exit_if_null(w_current);

	x_fileselect_setup(w_current, FILESELECT, OPEN);

#if 0 /* replaced by above */
	setup_open_file_selector(w_current);
#endif
}

DEFINE_I_CALLBACK(file_script)
{
	TOPLEVEL *w_current = (TOPLEVEL *) data;

	exit_if_null(w_current);
	setup_script_selector(w_current);
}

/* don't use the widget parameter on this function, or do some
 * checking... */
/* since there is a call: widget = NULL, data = 0 (will be w_current) */
DEFINE_I_CALLBACK(file_save)
{
	TOPLEVEL *w_current = (TOPLEVEL *) data;

	exit_if_null(w_current);

	/* TODO: probably there should be a flag that says whether
	 * page_filename is derived from untitled_name or specified by
	 * a user. Some twisted people might name their files like
	 * untitled_name. :-) */
	if (strstr(w_current->page_current->page_filename,
		   w_current->untitled_name)) {
		x_fileselect_setup(w_current, FILESELECT, SAVEAS_NONE);
#if 0 /* replaced with x_fileselect_setup */
		setup_saveas_file_selector(
			w_current,
			SAVEAS,
			w_current->page_current->page_filename);
#endif
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
	x_fileselect_setup(w_current, FILESELECT, SAVEAS_NONE);

#if 0 /* replaced with above */
	setup_saveas_file_selector(w_current,
				   SAVEAS,
				   w_current->page_current->page_filename);
#endif
}

DEFINE_I_CALLBACK(file_print)
{
	TOPLEVEL *w_current = (TOPLEVEL *) data;
	char *base=NULL;
        char *ps_filename=NULL;

	exit_if_null(w_current);

	/* get the base file name */
	if (strcmp(fnameext_get(w_current->page_current->page_filename),
		   ".sch") == 0) {
		/* the filename ends with .sch */
		base = fnameext_remove(w_current->page_current->page_filename);
	} else {
		/* the filename does not end with .sch */
		base = u_basic_strdup(w_current->page_current->page_filename);
	}
	if(base == NULL) {
		/* TODO: do something */
	}

	/* add ".ps" tp the base filename */
	ps_filename = fnameext_add(base, ".ps");
	free(base);

	if (output_filename) {
		x_print_setup(w_current, output_filename);
	} else {
		x_print_setup(w_current, ps_filename);
	}

	if (ps_filename) {
		free(ps_filename);
	}
}

DEFINE_I_CALLBACK(file_write_png)
{
	TOPLEVEL *w_current = (TOPLEVEL *) data;
	char *base=NULL;
        char *img_filename=NULL;

	exit_if_null(w_current);

#ifndef HAS_LIBGDGEDA
	/* TODO: integrate these to messages */
	fprintf(stderr,
		"libgdgeda not installed or disabled, "
		"so this feature is disabled\n");
	s_log_message(
		"libgdgeda not installed or disabled, "
		"so this feature is disabled\n");
	return;
#endif

	/* get the base file name */
	if (strcmp(fnameext_get(w_current->page_current->page_filename),
		   ".sch") == 0) {
		/* the filename ends with .sch */
		base = fnameext_remove(w_current->page_current->page_filename);
	} else {
		/* the filename does not end with .sch */
		base = u_basic_strdup(w_current->page_current->page_filename);
	}
	if(base == NULL) {
		/* TODO: do something */
	}

	/* add ".ps" tp the base filename */
	img_filename = fnameext_add(base, ".png");
	free(base);

	if (output_filename) {
		x_image_setup(w_current, output_filename);
	} else {
		x_image_setup(w_current, img_filename);
	}

	if (img_filename) {
		free(img_filename);
	}
}

/* don't use the widget parameter on this function, or do some
   checking... */
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
/* TODO: Need a cleaner way of doing this. This routine is used by the
 * delete event signals */
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

DEFINE_I_CALLBACK(edit_undo)
{
	TOPLEVEL *w_current = (TOPLEVEL *) data;

	o_undo_callback(w_current, UNDO_ACTION);
}

DEFINE_I_CALLBACK(edit_redo)
{
	TOPLEVEL *w_current = (TOPLEVEL *) data;

	o_undo_callback(w_current, REDO_ACTION);
}

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
	if (o_select_return_first_object(w_current)) {
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
	if (o_select_return_first_object(w_current)) {
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
	if (o_select_return_first_object(w_current)) {
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
	if (o_select_return_first_object(w_current)) {
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

	if (o_select_return_first_object(w_current)) {
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
	o_edit(w_current, w_current->page_current->selection2_head->next);
}

DEFINE_I_CALLBACK(edit_text)
{
	TOPLEVEL *w_current = (TOPLEVEL *) data;
	OBJECT *object;

	exit_if_null(w_current);

	i_update_middle_button(w_current, i_callback_edit_text, "Edit Text");
	object = o_select_return_first_object(w_current);
	if (object) {
		if (object->type == OBJ_TEXT) {
			o_text_edit(w_current, object);
		}
	}
}


#if 0 /* obsolete */
DEFINE_I_CALLBACK(edit_stretch)
{
	TOPLEVEL *w_current;

	w_current = (TOPLEVEL *) data;

	exit_if_null(w_current);

	i_update_middle_button(w_current, i_callback_edit_stretch, "Stretch");
	if (o_select_return_first_object(w_current)) {
		i_update_status(w_current, "Stretch Mode");
		w_current->event_state = STARTSTRETCH;
	} else {
		i_update_status(w_current, "Select objs first");
	}

}

/* obsolete */
DEFINE_I_CALLBACK(edit_stretch_hotkey)
{
	TOPLEVEL *w_current;

	w_current = (TOPLEVEL *) data;

	exit_if_null(w_current);

	i_update_middle_button(w_current, i_callback_edit_stretch_hotkey, 
			       "Stretch");
	if (o_select_return_first_object(w_current)) {
		/* only stretch if it's a valid object */
		if (o_stretch_start(w_current, mouse_x, mouse_y)) {
			w_current->event_state = ENDSTRETCH;
			w_current->inside_action = 1;
		}
	}
}
#endif

DEFINE_I_CALLBACK(edit_slot)
{
	TOPLEVEL *w_current = (TOPLEVEL *) data;
        OBJECT *object;

	exit_if_null(w_current);

        object = o_select_return_first_object(w_current);

	i_update_middle_button(w_current, i_callback_edit_slot, "Slot");
	if (object) {
		o_slot_start(w_current, object);
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
	SELECTION *s_current;

        exit_if_null(w_current);

	if (w_current->page_current->selection2_head) {
		s_current = w_current->page_current->selection2_head->next;
        	i_update_middle_button(w_current,
			       i_callback_edit_rotate_90_hotkey, "Rotate");
		o_rotate_90(w_current, s_current, mouse_x, mouse_y);
	}

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
        OBJECT *object;

        exit_if_null(w_current);

	object = o_select_return_first_object(w_current);

	if (object) {
        	i_update_middle_button(w_current,
			       i_callback_edit_mirror_hotkey, "Mirror");

		o_mirror(w_current, 
			 w_current->page_current->selection2_head->next, 
			 mouse_x, mouse_y);
	}

        w_current->event_state = SELECT;
        w_current->inside_action = 0;
}

/* locks all objects in selection list */
DEFINE_I_CALLBACK(edit_lock)
{
	TOPLEVEL *w_current = (TOPLEVEL *) data;

	exit_if_null(w_current);

	i_update_middle_button(w_current, i_callback_edit_lock, "Lock");

        if (o_select_return_first_object(w_current)) {
		o_lock(w_current);
	}
}

/* unlocks all objects in selection list */
DEFINE_I_CALLBACK(edit_unlock)
{
	TOPLEVEL *w_current = (TOPLEVEL *) data;

	exit_if_null(w_current);

	i_update_middle_button(w_current, i_callback_edit_unlock, "Unlock");
        if (o_select_return_first_object(w_current)) {
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
        if (o_select_return_first_object(w_current)) {
		o_embed(w_current);
	}
}

/* unembedds all objects in selection list */
DEFINE_I_CALLBACK(edit_unembed)
{
	TOPLEVEL *w_current = (TOPLEVEL *) data;

	exit_if_null(w_current);

	i_update_middle_button(w_current, i_callback_edit_unembed, "Unembed");
        if (o_select_return_first_object(w_current)) {
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
			       i_callback_edit_show_hidden,
			       "ShowHidden");

	o_edit_show_hidden(w_current, w_current->page_current->object_head);
}

DEFINE_I_CALLBACK(edit_linetype)
{
	TOPLEVEL *w_current = (TOPLEVEL *) data;
	OBJECT *object;

	exit_if_null(w_current);

	i_update_middle_button(w_current, i_callback_edit_color, "Edit Line Type");

        if ( (object = o_select_return_first_object(w_current)) ) {
		line_type_dialog(w_current, object);
	}
}

/* PB : support for fill dialog */
DEFINE_I_CALLBACK(edit_filltype)
{
	TOPLEVEL *w_current = (TOPLEVEL *) data;
	OBJECT *object;

	exit_if_null(w_current);
	
	i_update_middle_button(w_current, i_callback_edit_color, "Edit Fill Type");

	if ( (object = o_select_return_first_object(w_current)) ) {
		fill_type_dialog(w_current, object);
	}
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
	o_undo_savestate(w_current, UNDO_VIEWPORT_ONLY);
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
	o_undo_savestate(w_current, UNDO_VIEWPORT_ONLY);
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
	o_undo_savestate(w_current, UNDO_VIEWPORT_ONLY);
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
	o_undo_savestate(w_current, UNDO_VIEWPORT_ONLY);
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
	o_undo_savestate(w_current, UNDO_VIEWPORT_ONLY);
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
	o_undo_savestate(w_current, UNDO_VIEWPORT_ONLY);
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

	/* I have NO idea what ramifications removing the next line has,
	 * only that it makes the code work when drawing a net and panning
	 * at the same time.  Jeff McNeal - 11-19-98
	 * w_current->inside_action = 0;
	 * I think it's okay - Ales 12/13/98 */

	a_pan(w_current, mouse_x, mouse_y);

	/* Jeff McNeal on Nov 19, 1998 - if we are drawing a net,
	 * don't change the event state, because we want to continue
	 * drawing a net. If we are just panning, then continue in
	 * select mode.  */
	if(!(w_current->event_state == DRAWNET ||
	     w_current->event_state == NETCONT ||
             w_current->event_state == STARTDRAWNET )) {
		w_current->event_state = SELECT;
		i_update_status(w_current, "Select Mode");
	}
	o_undo_savestate(w_current, UNDO_VIEWPORT_ONLY);
}

DEFINE_I_CALLBACK(view_update_cues)
{
	TOPLEVEL *w_current = (TOPLEVEL *) data;

	i_update_middle_button(w_current,
			       i_callback_view_update_cues, "Update Cues");

	o_conn_disconnect_update(w_current->page_current);
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
	PAGE *new_page;
	TOPLEVEL *w_current = (TOPLEVEL *) data;

	exit_if_null(w_current);

	if (w_current->page_current->next != NULL &&
	    w_current->page_current->next->pid != -1) {

		if (w_current->enforce_hierarchy == TRUE) { 
			new_page = s_hierarchy_find_next_page(w_current->
							      page_current, 
					 w_current->page_current->page_control);
		} else {
			new_page = w_current->page_current->next;
		}

		if (new_page) {
			w_current->page_current = new_page;
		} else {
			return;
		}
	}

	s_page_goto(w_current, w_current->page_current);
        i_set_filename(w_current, w_current->page_current->page_filename);
	x_scrollbars_update(w_current);
	o_redraw_all(w_current);
	update_page_manager(NULL, w_current);

}

DEFINE_I_CALLBACK(page_prev)
{
	PAGE *new_page;
	TOPLEVEL *w_current = (TOPLEVEL *) data;

	exit_if_null(w_current);

	if (w_current->page_current->prev != NULL &&
			w_current->page_current->prev->pid != -1) {

		if (w_current->enforce_hierarchy == TRUE) { 
			new_page = s_hierarchy_find_prev_page(w_current->
							      page_current, 
					 w_current->page_current->page_control);
		} else {
			new_page = w_current->page_current->prev;
		}

		if (new_page) { 
			w_current->page_current = new_page;
		} else {
			return;
		}
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
	PAGE *p_save;

	exit_if_null(w_current);

	if (w_current->page_current->CHANGED) {
		x_fileselect_setup(w_current, FILESELECT, SAVEAS_CLOSE);

#if 0 /* replaced with above */
		setup_saveas_file_selector(
			w_current, CLOSE,
			w_current->page_current->page_filename);
#endif
		return;
	}

	/* Can we go up in the hierarchy first? */
	p_current = s_hierarchy_find_page(w_current->page_head, 
					  w_current->page_current->up);
	if (p_current) {
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
	/* because it's the last page being displayed */
	/* s_log_message("Cannot close current page\n");*/
	/* now the code creates a new page, and closes the old one */
	p_save = w_current->page_current;
	i_callback_page_new(w_current, 0, NULL);
	w_current->page_current = p_save;	

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

}

/* TODO: may have memory leak? */
DEFINE_I_CALLBACK(page_revert)
{
	TOPLEVEL *w_current = (TOPLEVEL *) data;
	char *filename;
	int page_control;
	int up;

	exit_if_null(w_current);

	filename = u_basic_strdup(w_current->page_current->page_filename);

	/* save this for later */
	page_control = w_current->page_current->page_control;
	up = w_current->page_current->up;

	s_page_free(w_current, w_current->page_current);
	s_page_new(w_current, filename);

	/* now re open it */
	w_current->DONT_REDRAW = 1;
        f_open(w_current, w_current->page_current->page_filename);
        i_set_filename(w_current, w_current->page_current->page_filename);

	/* make sure we maintain the hierarchy info */
	w_current->page_current->page_control = page_control;
	w_current->page_current->up = up;

	x_repaint_background(w_current);
	x_window_setup_world(w_current);
	x_manual_resize(w_current);
	a_zoom_limits(w_current, w_current->page_current->object_head);
	o_undo_savestate(w_current, UNDO_ALL);

	/* now update the scrollbars */
	x_hscrollbar_update(w_current);
	x_vscrollbar_update(w_current);
	update_page_manager(NULL, w_current);
	w_current->DONT_REDRAW = 0;

	o_redraw_all(w_current);
	free(filename);
}

DEFINE_I_CALLBACK(page_discard)
{
	TOPLEVEL *w_current = (TOPLEVEL *) data;
	PAGE *p_current;
	PAGE *p_save;

	exit_if_null(w_current);

	/* Can we go up in the hierarchy first? */
	p_current = s_hierarchy_find_page(w_current->page_head, 
					  w_current->page_current->up);
	if (p_current) {
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
	/* because it's the last page being displayed */
	/* s_log_message("Cannot close current page\n");*/
	/* now the code creates a new page, and closes the old one */

	p_save = w_current->page_current;
	i_callback_page_new(w_current, 0, NULL);
	w_current->page_current = p_save;	

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
}

DEFINE_I_CALLBACK(page_print)
{
	TOPLEVEL *w_current = (TOPLEVEL *) data;

	s_page_print_all(w_current);
}

/* Buffer menu */
DEFINE_I_CALLBACK(buffer_copy1)
{
	TOPLEVEL *w_current = (TOPLEVEL *) data;

	exit_if_null(w_current);

	if (w_current->page_current->selection2_head->next == NULL)
		return;

	i_update_middle_button(w_current, i_callback_buffer_copy1, "Copy 1");
	o_buffer_copy(w_current, 0);
}

DEFINE_I_CALLBACK(buffer_copy2)
{
	TOPLEVEL *w_current = (TOPLEVEL *) data;

	exit_if_null(w_current);

	if (w_current->page_current->selection2_head->next == NULL)
		return;

	i_update_middle_button(w_current, i_callback_buffer_copy2, "Copy 2");
	o_buffer_copy(w_current, 1);
}

DEFINE_I_CALLBACK(buffer_copy3)
{
	TOPLEVEL *w_current = (TOPLEVEL *) data;

	exit_if_null(w_current);

	if (w_current->page_current->selection2_head->next == NULL)
		return;

	i_update_middle_button(w_current, i_callback_buffer_copy3, "Copy 3");
	o_buffer_copy(w_current, 2);
}

DEFINE_I_CALLBACK(buffer_copy4)
{
	TOPLEVEL *w_current = (TOPLEVEL *) data;

	exit_if_null(w_current);

	if (w_current->page_current->selection2_head->next == NULL)
		return;

	i_update_middle_button(w_current, i_callback_buffer_copy4, "Copy 4");
	o_buffer_copy(w_current, 3);
}

DEFINE_I_CALLBACK(buffer_copy5)
{
	TOPLEVEL *w_current = (TOPLEVEL *) data;

	exit_if_null(w_current);

	if (w_current->page_current->selection2_head->next == NULL)
		return;

	i_update_middle_button(w_current, i_callback_buffer_copy5, "Copy 5");
	o_buffer_copy(w_current, 4);
}

DEFINE_I_CALLBACK(buffer_cut1)
{
	TOPLEVEL *w_current = (TOPLEVEL *) data;

	exit_if_null(w_current);

	if (w_current->page_current->selection2_head->next == NULL)
		return;

	i_update_middle_button(w_current, i_callback_buffer_cut1, "Cut 1");
	o_buffer_cut(w_current, 0);
}

DEFINE_I_CALLBACK(buffer_cut2)
{
	TOPLEVEL *w_current = (TOPLEVEL *) data;

	exit_if_null(w_current);

	if (w_current->page_current->selection2_head->next == NULL)
		return;

	i_update_middle_button(w_current, i_callback_buffer_cut2, "Cut 2");
	o_buffer_cut(w_current, 1);
}

DEFINE_I_CALLBACK(buffer_cut3)
{
	TOPLEVEL *w_current = (TOPLEVEL *) data;

	exit_if_null(w_current);

	if (w_current->page_current->selection2_head->next == NULL)
		return;

	i_update_middle_button(w_current, i_callback_buffer_cut3, "Cut 3");
	o_buffer_cut(w_current, 2);
}

DEFINE_I_CALLBACK(buffer_cut4)
{
	TOPLEVEL *w_current = (TOPLEVEL *) data;

	exit_if_null(w_current);

	if (w_current->page_current->selection2_head->next == NULL)
		return;

	i_update_middle_button(w_current, i_callback_buffer_cut4, "Cut 4");
	o_buffer_cut(w_current, 3);
}

DEFINE_I_CALLBACK(buffer_cut5)
{
	TOPLEVEL *w_current = (TOPLEVEL *) data;

	exit_if_null(w_current);

	if (w_current->page_current->selection2_head->next == NULL)
		return;

	i_update_middle_button(w_current, i_callback_buffer_cut5, "Cut 5");
	o_buffer_cut(w_current, 4);
}

DEFINE_I_CALLBACK(buffer_paste1)
{
	TOPLEVEL *w_current = (TOPLEVEL *) data;

	exit_if_null(w_current);

	i_update_middle_button(w_current, i_callback_buffer_paste1, "Paste 1");
	if (object_buffer[0] != NULL) {
		if (object_buffer[0]->next != NULL) {
			i_update_status(w_current, "Paste 1 Mode ");
			w_current->event_state = STARTPASTE;
			w_current->buffer_number = 0;
			w_current->inside_action = 1;
		}
	} else { 
		i_update_status(w_current, "Empty buffer");
	}
}

DEFINE_I_CALLBACK(buffer_paste2)
{
	TOPLEVEL *w_current = (TOPLEVEL *) data;

	exit_if_null(w_current);

	i_update_middle_button(w_current, i_callback_buffer_paste2, "Paste 2");
	if (object_buffer[1] != NULL) {
		if (object_buffer[1]->next != NULL) {
			i_update_status(w_current, "Paste 2 Mode ");
			w_current->event_state = STARTPASTE;
			w_current->buffer_number = 1;
			w_current->inside_action = 1;
		}
	} else {
		i_update_status(w_current, "Empty buffer");
	}
}

DEFINE_I_CALLBACK(buffer_paste3)
{
	TOPLEVEL *w_current = (TOPLEVEL *) data;

	exit_if_null(w_current);

	i_update_middle_button(w_current, i_callback_buffer_paste3, "Paste 3");
	if (object_buffer[2] != NULL) {
		if (object_buffer[2]->next != NULL) {
			i_update_status(w_current, "Paste 3 Mode ");
			w_current->event_state = STARTPASTE;
			w_current->buffer_number = 2;
			w_current->inside_action = 1;
		}
	} else { 
		i_update_status(w_current, "Empty buffer");
	}
}

DEFINE_I_CALLBACK(buffer_paste4)
{
	TOPLEVEL *w_current = (TOPLEVEL *) data;

	exit_if_null(w_current);

	i_update_middle_button(w_current, i_callback_buffer_paste4, "Paste 4");
	if (object_buffer[3] != NULL) {
		if (object_buffer[3]->next != NULL) {
			i_update_status(w_current, "Paste 4 Mode ");
			w_current->event_state = STARTPASTE;
			w_current->buffer_number = 3;
			w_current->inside_action = 1;
		}
	} else {
		i_update_status(w_current, "Empty buffer");
	}
}

DEFINE_I_CALLBACK(buffer_paste5)
{
	TOPLEVEL *w_current = (TOPLEVEL *) data;

	exit_if_null(w_current);

	i_update_middle_button(w_current, i_callback_buffer_paste5, "Paste 5");
	if (object_buffer[4] != NULL) {
		if (object_buffer[4]->next != NULL) {
			i_update_status(w_current, "Paste 5 Mode ");
			w_current->event_state = STARTPASTE;
			w_current->buffer_number = 4;
			w_current->inside_action = 1;
		}
	} else {
		i_update_status(w_current, "Empty buffer");
	}
}

DEFINE_I_CALLBACK(buffer_paste1_hotkey)
{
	TOPLEVEL *w_current = (TOPLEVEL *) data;

	exit_if_null(w_current);

	if (object_buffer[0] == NULL) {
		return;
	}

	if (object_buffer[0]->next == NULL)  {
		return;
	
	}
	
	o_buffer_paste_start(w_current, mouse_x, mouse_y, 0);
}

DEFINE_I_CALLBACK(buffer_paste2_hotkey)
{
	TOPLEVEL *w_current = (TOPLEVEL *) data;

	exit_if_null(w_current);

	if (object_buffer[1] == NULL) {
		return;
	}

	if (object_buffer[1]->next == NULL)  {
		return;
	
	}

	o_buffer_paste_start(w_current, mouse_x, mouse_y, 1);
}

DEFINE_I_CALLBACK(buffer_paste3_hotkey)
{
	TOPLEVEL *w_current = (TOPLEVEL *) data;

	exit_if_null(w_current);

	if (object_buffer[2] == NULL) {
		return;
	}

	if (object_buffer[2]->next == NULL)  {
		return;
	
	}

	o_buffer_paste_start(w_current, mouse_x, mouse_y, 2);
}

DEFINE_I_CALLBACK(buffer_paste4_hotkey)
{
	TOPLEVEL *w_current = (TOPLEVEL *) data;

	exit_if_null(w_current);

	if (object_buffer[3] == NULL) {
		return;
	}

	if (object_buffer[3]->next == NULL)  {
		return;
	
	}

	o_buffer_paste_start(w_current, mouse_x, mouse_y, 3);
}

DEFINE_I_CALLBACK(buffer_paste5_hotkey)
{
	TOPLEVEL *w_current = (TOPLEVEL *) data;

	exit_if_null(w_current);

	if (object_buffer[4] == NULL) {
		return;
	}

	if (object_buffer[4]->next == NULL)  {
		return;
	
	}

	o_buffer_paste_start(w_current, mouse_x, mouse_y, 4);
}

/* Add menu */
DEFINE_I_CALLBACK(add_component)
{
	TOPLEVEL *w_current = (TOPLEVEL *) data;

	exit_if_null(w_current);

	x_fileselect_setup(w_current, COMPSELECT, -1);

#if 0 /* replaced by above */
	setup_place_file_selector(w_current);
#endif
	i_update_middle_button(w_current,
			       i_callback_add_component, "Component");
	i_update_status2(w_current, "Select Mode");
}

DEFINE_I_CALLBACK(add_attribute)
{
	TOPLEVEL *w_current = (TOPLEVEL *) data;

	exit_if_null(w_current);

	attrib_edit_dialog(w_current, NULL, FROM_MENU);
	i_update_middle_button(w_current, i_callback_add_attribute,
			       "Attribute");
	i_update_status2(w_current, "Select Mode");
}

DEFINE_I_CALLBACK(add_attribute_hotkey)
{
	TOPLEVEL *w_current = (TOPLEVEL *) data;

	exit_if_null(w_current);

	attrib_edit_dialog(w_current, NULL, FROM_HOTKEY);
	i_update_middle_button(w_current, i_callback_add_attribute_hotkey,
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

DEFINE_I_CALLBACK(add_bus)
{
	TOPLEVEL *w_current = (TOPLEVEL *) data;

	exit_if_null(w_current);

	/* need to click */
	w_current->event_state = STARTDRAWBUS;
	i_update_middle_button(w_current, i_callback_add_bus, "Bus");
	i_update_status2(w_current, "Bus Mode");
	/* somewhere you need to nearest point locking... */
	w_current->inside_action = 0;
}

DEFINE_I_CALLBACK(add_bus_hotkey)
{
	TOPLEVEL *w_current = (TOPLEVEL *) data;

	exit_if_null(w_current);

	/* need to click */
	i_update_middle_button(w_current, i_callback_add_bus_hotkey, "Bus");
	i_update_status2(w_current, "Bus Mode");

	w_current->event_state = STARTDRAWBUS;

	o_bus_start(w_current, mouse_x, mouse_y);

	w_current->event_state=DRAWBUS;
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
DEFINE_I_CALLBACK(hierarchy_down_schematic)
{
	TOPLEVEL *w_current = (TOPLEVEL *) data;
	char *attrib=NULL;
	char *current_filename=NULL;
	int count=0;
	OBJECT *object=NULL;
	PAGE *save_first_page=NULL;
	PAGE *parent=NULL;
	int loaded_flag=FALSE;
	int page_control = 0;
	int saved_page_control = 0;
        int pcount = 0;
	int looking_inside=FALSE;

	exit_if_null(w_current);

	object = o_select_return_first_object(w_current);
	if (object != NULL) {
		/* only allow going into symbols */
		if (object->type == OBJ_COMPLEX) {

			parent = w_current->page_current;
			attrib = o_attrib_search_name_single_count(object,
							           "source",
							           count);

			/* if above is null, then look inside symbol */
			if (attrib == NULL) {
				attrib = o_attrib_search_name(object->
							      complex->
	 						      prim_objs,
							      "source", 
							      count);
				looking_inside = TRUE;
#if DEBUG
				printf("going to look inside now\n");
#endif
			}

			while (attrib) {

				/* look for source=filename,filename, ... */
				pcount = 0;
				current_filename = u_basic_breakup_string(
								attrib, 
							        pcount);

				/* loop over all filenames */
        			while(current_filename != NULL) {

					s_log_message(
						"Searching for source [%s]\n", 
					        current_filename);
					saved_page_control = page_control;
					page_control = 
				           s_hierarchy_down_schematic_single(
							   w_current, 
							   current_filename, 
							   parent,
							   page_control);

					if (page_control != -1)  {
						a_zoom_limits(w_current, 
				          		      w_current->
							      page_current->
							      object_head);
						o_undo_savestate(w_current, 
								 UNDO_ALL);
					}


					/* save the first page */
					if (!loaded_flag && 
					     page_control != -1 && 
					     page_control != 0) {
						save_first_page = w_current->
							          page_current;
					}

					/* now do some error fixing */
					if (page_control == -1) {
						s_log_message(
					  	  "Cannot find source [%s]\n", 
						  current_filename);
						fprintf(stderr, 
						   "Cannot find source [%s]\n", 
						   current_filename); 

						/* restore this for the next */
						/* page */
						page_control = 
							saved_page_control;
					} else {
						/* this only signifies that */
						/* we tried */
						loaded_flag = TRUE;
					}

					free(current_filename);
					pcount++;
					current_filename = 
							u_basic_breakup_string(
								  attrib, 
								  pcount);
				}

				if (attrib) {
					free(attrib);
				}

				if (current_filename) {
					free(current_filename);
				}

				count++;

				/* continue looking outside first */
				if (!looking_inside) {
					attrib = 
					   o_attrib_search_name_single_count(
							     object,
							     "source",
							     count);
				} 

				/* okay we were looking outside and didn't */
				/* find anything, so now we need to look */
				/* inside the symbol */
				if (!looking_inside && attrib == NULL && 
				    !loaded_flag ) {
					looking_inside = TRUE;
#if DEBUG
					printf("switching to go to look inside\n");
#endif
				}

				if (looking_inside) {
#if DEBUG
					printf("looking inside\n");
#endif
					attrib = o_attrib_search_name(
							     object->complex->
							     prim_objs,
							     "source",
							     count);
				}
	} 

			if (loaded_flag) {
	
				if (save_first_page) {
					w_current->page_current = 
							save_first_page;
				}
				i_set_filename(w_current, w_current->
					page_current->page_filename);
				a_zoom_limits(w_current, 
				      w_current->page_current->object_head);
				o_redraw_all(w_current);
				update_page_manager(NULL, w_current);
			}
		}
        }
}

DEFINE_I_CALLBACK(hierarchy_down_symbol)
{
	TOPLEVEL *w_current = (TOPLEVEL *) data;
	OBJECT *object;
	char *filename;

	exit_if_null(w_current);

	object = o_select_return_first_object(w_current);
	if (object != NULL) {
		/* only allow going into symbols */
		if (object->type == OBJ_COMPLEX) {
			filename = u_basic_strdup_multiple( 
				object->complex_clib, "/", 
				object->complex_basename, NULL);
			s_log_message("Searching for symbol [%s]\n", filename);
			s_hierarchy_down_symbol(w_current, filename, 
				 		w_current->page_current);
			i_set_filename(w_current,
				       w_current->page_current->page_filename);
			a_zoom_limits(w_current, 
				      w_current->page_current->object_head);
			o_undo_savestate(w_current, UNDO_ALL);
			o_redraw_all(w_current);
			update_page_manager(NULL, w_current);
			free(filename);
		}
        }
}

DEFINE_I_CALLBACK(hierarchy_up)
{
	TOPLEVEL *w_current = (TOPLEVEL *) data;

	exit_if_null(w_current);

	s_hierarchy_up(w_current, w_current->page_current->up);
	i_set_filename(w_current, w_current->page_current->page_filename);
	o_redraw_all(w_current);
	update_page_manager(NULL, w_current);
}

/* Attributes menu */
DEFINE_I_CALLBACK(attributes_attach)
{
	TOPLEVEL *w_current = (TOPLEVEL *) data;
	OBJECT *first_object;
	SELECTION *s_current;

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

	/* skip over head */
	s_current = w_current->page_current->selection2_head->next;
	if (!s_current) {
		return;
	}

	first_object = s_current->selected_object; 
	if (!first_object) {
		return;	
	}

	/* skip over first object */
	s_current = s_current->next;
	while (s_current != NULL) {
		if (s_current->selected_object) {
			o_attrib_attach(w_current,
				w_current->page_current->object_head,
				s_current->selected_object,
				first_object);
			w_current->page_current->CHANGED=1;
		}
		s_current = s_current->next;
	}
	o_undo_savestate(w_current, UNDO_ALL);
}

DEFINE_I_CALLBACK(attributes_detach)
{
	TOPLEVEL *w_current = (TOPLEVEL *) data;
	SELECTION *s_current;
	OBJECT *o_current;

	exit_if_null(w_current);

	/* This is a new addition 3/15 to prevent this from executing
	 * inside an action */
	if (w_current->inside_action) {
		return;
	}

	/* same note as above on i_update_middle_button */
	i_update_middle_button(w_current, i_callback_attributes_detach,
			       "Detach");

	/* skip over head */
	s_current = w_current->page_current->selection2_head->next;
	while (s_current != NULL) {
		o_current = s_current->selected_object;
		if (o_current) {
			if (o_current->attribs) {
				o_attrib_free_all(w_current, 
						  o_current->attribs);
				o_current->attribs = NULL;
				w_current->page_current->CHANGED=1;
			}
		}
		s_current = s_current->next;
	}
	o_undo_savestate(w_current, UNDO_ALL);
}

DEFINE_I_CALLBACK(attributes_show_name)
{
	TOPLEVEL *w_current = (TOPLEVEL *) data;
	OBJECT *object;

	exit_if_null(w_current);

	object = o_select_return_first_object(w_current);

	/* This is a new addition 3/15 to prevent this from executing
	 * inside an action */
	if (w_current->inside_action) {
		return;
	}

	i_update_middle_button(w_current, i_callback_attributes_show_name,
			       "ShowN");

	if (object != NULL) {
		o_attrib_toggle_show_name_value(w_current, 
                                                w_current->page_current->
                                                selection2_head->next,
						SHOW_NAME);
        }
}

DEFINE_I_CALLBACK(attributes_show_value)
{
	TOPLEVEL *w_current = (TOPLEVEL *) data;
	OBJECT *object;

	exit_if_null(w_current);

	object = o_select_return_first_object(w_current);

	/* This is a new addition 3/15 to prevent this from executing
	 * inside an action */
	if (w_current->inside_action) {
		return;
	}

	i_update_middle_button(w_current, i_callback_attributes_show_value,
			       "ShowV");

	if (object != NULL) {
		o_attrib_toggle_show_name_value(w_current, 
                                                w_current->page_current->
                                                selection2_head->next,
						SHOW_VALUE);
        }
}

DEFINE_I_CALLBACK(attributes_show_both)
{
	TOPLEVEL *w_current = (TOPLEVEL *) data;
	OBJECT *object;

	exit_if_null(w_current);

	object = o_select_return_first_object(w_current);

	/* This is a new addition 3/15 to prevent this from executing
	 * inside an action */
	if (w_current->inside_action) {
		return;
	}

	i_update_middle_button(w_current, i_callback_attributes_show_both,
			       "ShowB");

	if (object != NULL) {
		o_attrib_toggle_show_name_value(w_current, 
					        w_current->page_current->
					        selection2_head->next,
						SHOW_NAME_VALUE);
        }
}

DEFINE_I_CALLBACK(attributes_visibility_toggle)
{
	TOPLEVEL *w_current = (TOPLEVEL *) data;
	OBJECT *object;

	exit_if_null(w_current);

	object = o_select_return_first_object(w_current);

	/* This is a new addition 3/15 to prevent this from executing
	 * inside an action */
	if (w_current->inside_action) {
		return;
	}

	i_update_middle_button(w_current,
			       i_callback_attributes_visibility_toggle,
			       "VisToggle");

	if (object != NULL) {
		o_attrib_toggle_visibility(w_current, 
					   w_current->page_current->
					   selection2_head->next);
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

DEFINE_I_CALLBACK(options_show_log_window)
{
	TOPLEVEL *w_current = (TOPLEVEL *) data;

	exit_if_null(w_current);
	x_log_setup_win(w_current);
}

/* this is Ales' catch all misc callback */
DEFINE_I_CALLBACK(misc)
{
	TOPLEVEL *w_current = (TOPLEVEL *) data;

#if 0 /* old demonstration code on getting all attribs from an object */
	if (w_current->page_current->selection_head->next != NULL) {
		attrib_objects = o_attrib_return_attribs(
					      w_current->page_current->
					      object_head,
				              w_current->page_current->
				              selection_head->next);


		if (attrib_objects) {
			while(attrib_objects[i] != NULL) {
				o_attrib_get_name_value(
						attrib_objects[i]->text->string, 
						name, value);
                		printf("%d : %s\n", i, 
					attrib_objects[i]->text->string);
				printf("   name: %s\n", name);
				printf("   value: %s\n", value);
                		i++;
			}
			o_attrib_free_returned(attrib_objects);
		}

        }
#endif
	/*o_selection_print_all(w_current->page_current->selection2_head);*/
	/* printf("\n\nUNDO:\n"); */
	/* s_undo_print_all(w_current->page_current->undo_bottom);*/


#if 0 /* no longer needed and old */
	/* In this case w_current->page_current->next can be safely null */	
	/* new demonstration code which shows how to use o_attrib_add_attrib */
	object = o_attrib_add_attrib(w_current, "name=value", VISIBLE, 
				    SHOW_NAME_VALUE, 
				    w_current->page_current->
				    selection_head->next);


 	/* for debugging purposes */
	printf("%d %s %s\n", object->sid, object->name, object->text->string);
	/*o_conn_print_hash(w_current->page_current->conn_table);*/
	/* o_text_print_set();*/
#endif
}

/* this is Ales' second catch all misc callback */
DEFINE_I_CALLBACK(misc2)
{
#if 0
	TOPLEVEL *w_current = (TOPLEVEL *) data;

	SELECTION *s_current = NULL;
	OBJECT *o_current = NULL;

	if (w_current->page_current->selection2_head->next == NULL)
		return;

	/* o_buffer_cut(w_current);*/
	/*o_buffer_copy(w_current);*/
#endif
}

/* this is Ales' third catch all misc callback */
DEFINE_I_CALLBACK(misc3)
{
#if 0
	TOPLEVEL *w_current = (TOPLEVEL *) data;

	OBJECT *o_current;
	PAGE *p_current;
	OBJECT *new_object;
	OBJECT *o_saved = NULL;
	SELECTION *temp_list = NULL;

	if (object_buffer[0] == NULL) {
		return;
	}

#if DEBUG
	o_current = object_buffer[0];
	while(o_current != NULL) {
		printf("- %s\n", o_current->name);
		o_current = o_current->next;
	}
#endif


	/* o_buffer_paste_start(w_current, mouse_x, mouse_y);*/
#endif
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

DEFINE_I_CALLBACK(help_hotkeys)
{
	TOPLEVEL *w_current = (TOPLEVEL *) data;

	exit_if_null(w_current);
	x_dialog_hotkeys(w_current);
}

DEFINE_I_CALLBACK(options_show_coord_window)
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

/* these is a special wrapper function which cannot use the above */
/* DEFINE_I_CALLBACK macro */

/* When invoked (via signal delete_event), closes the current window */
/* if this is the last window, quit gschem */
/* used when you click the close button on the window which sends a DELETE */
/* signal to the app */
void i_callback_close_wm ( GtkWidget *widget, GdkEvent *event, 
	                   gpointer data ) 
{

	TOPLEVEL *w_current = (TOPLEVEL *) data;
	exit_if_null(w_current);

	x_window_close(w_current);
}

