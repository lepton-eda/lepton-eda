/* gEDA - GPL Electronic Design Automation
 * gnetlist - gEDA Netlist
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
#include <signal.h>

#include <libgeda/libgeda.h>

#include "../include/globals.h"
#include "../include/papersizes.h"
#include "../include/prototype.h"

/* global_wid always increments, it needs to be unique per gnetlist run */
static int num_projects = 0;
static int global_wid = 0;

/* head pointer to window structure, this points to all the windows that
   currently exist */
static TOPLEVEL *project_head = NULL;
static TOPLEVEL *project_tail = NULL;

/* add to the end of the list */
TOPLEVEL *s_project_add(TOPLEVEL * w_head, TOPLEVEL * pr_current)
{
    if (w_head == NULL) {
	pr_current->prev = NULL;
	pr_current->next = NULL;
	return (pr_current);
    } else {
	pr_current->prev = w_head;
	pr_current->next = NULL;
	w_head->next = pr_current;
	return (w_head->next);
    }
}

void s_project_add_head()
{
    project_tail = project_head = (TOPLEVEL *) malloc(sizeof(TOPLEVEL));
    project_head->wid = -1;
}

void s_project_free_head()
{
    free(project_head);
}

/* deletes specified window from w_head list */
/* doesn't do the actual destroy though */
void s_project_delete(TOPLEVEL * w_head, TOPLEVEL * pr_current)
{

    if (w_head == NULL || pr_current == NULL) {
	/* error condition hack */
	return;
    }

    if (pr_current->next)
	pr_current->next->prev = pr_current->prev;

    if (pr_current->prev)
	pr_current->prev->next = pr_current->next;

    s_page_free_all(pr_current, pr_current->page_tail);

}

void s_project_setup_world(TOPLEVEL * pr_current)
{
    pr_current->init_left = 0;
    pr_current->init_top = 0;
    /* init_right and _bottom are set before this function is called */
    pr_current->min_zoom = 0;
    pr_current->max_zoom = 8;


/* part of page mechanism addition commented out
	pr_current->zoom_factor = 0;
*/
}

void s_project_setup_rest(TOPLEVEL * pr_current)
{
    pr_current->num_untitled = 0;

    pr_current->start_x = -1;
    pr_current->start_y = -1;
    pr_current->save_x = -1;
    pr_current->save_y = -1;
    pr_current->last_x = -1;
    pr_current->last_y = -1;
    pr_current->loc_x = -1;
    pr_current->loc_y = -1;
    pr_current->distance = -1;
    pr_current->event_state = -1;
    pr_current->inside_action = 0;
    pr_current->snap = 1;
    pr_current->grid = 1;

    pr_current->current_attribute = NULL;
    pr_current->current_visible = -1;	/* not sure on these */
    pr_current->current_show = -1;

    pr_current->internal_basename = NULL;
    pr_current->internal_clib = NULL;

    pr_current->series_name = NULL;
    pr_current->untitled_name = NULL;
    pr_current->font_directory = NULL;
    pr_current->scheme_directory = NULL;

/* part of page mechanism addition commented out 
	pr_current->zoom_factor=0;
*/
    pr_current->override_color = -1;
    pr_current->inside_redraw = 0;

    /* init some important variables */
    /* important when reading in nets and pins */
    pr_current->override_net_color = -1;
    pr_current->override_pin_color = -1;

    /* Don't init these to zero here... once we are done with all init
     *  will these be inited to zero 
     * pr_current->DONT_DRAW_CONN=0;
     * pr_current->DONT_RESIZE=0;
     * pr_current->DONT_EXPOSE=0;
     * pr_current->DONT_RECALC=0;
     */

    pr_current->FORCE_CONN_UPDATE = 0;
    pr_current->ADDING_SEL = 0;
    pr_current->REMOVING_SEL = 0;

    pr_current->drawbounding_action_mode = FREE;
    pr_current->last_drawb_mode = -1;
    pr_current->CONTROLKEY = 0;
    pr_current->SHIFTKEY = 0;
    pr_current->last_callback = NULL;

    pr_current->cswindow = NULL;
    pr_current->aswindow = NULL;
    pr_current->fowindow = NULL;
    pr_current->fswindow = NULL;

    pr_current->tiwindow = NULL;
    pr_current->tewindow = NULL;
    pr_current->exwindow = NULL;
    pr_current->aawindow = NULL;
    pr_current->trwindow = NULL;
    pr_current->tswindow = NULL;
    pr_current->pswindow = NULL;
    pr_current->pwindow = NULL;
    pr_current->iwindow = NULL;
    pr_current->abwindow = NULL;
    pr_current->hkwindow = NULL;
    pr_current->cowindow = NULL;

    pr_current->coord_world = NULL;
    pr_current->coord_screen = NULL;

    /* pr_current->preview = NULL;experimental widget */

    pr_current->width = 1;
    pr_current->height = 1;
    pr_current->snap_size = 100;

    /* special init of net_consolidate, since it's used in libgeda */
    pr_current->net_consolidate = FALSE;

    /* The following is an attempt at getting (deterministic) defaults */
    /* for the following variables */
    pr_current->attribute_promotion = FALSE;
    pr_current->promote_invisible = FALSE;
    pr_current->keep_invisible = FALSE;

    pr_current->hierarchy_netattrib_separator = NULL;
    pr_current->hierarchy_netname_separator = NULL;
    pr_current->hierarchy_uref_separator = NULL;
}

/* stays the same */
TOPLEVEL *s_project_create_new(void)
{
  TOPLEVEL *pr_current = NULL;

  /* allocate new window structure */
  pr_current = (TOPLEVEL *) malloc(sizeof(TOPLEVEL));

  pr_current->wid = global_wid;

  /* the default coord sizes */
  /* real ones set in rc file */
  /* pr_current->init_right = WIDTH_C;*/
  /* pr_current->init_bottom = HEIGHT_C;*/

  s_project_setup_world(pr_current);

  /* do other var fill in */
  s_project_setup_rest(pr_current);

  /* set the rest of the variables */
  i_vars_set(pr_current);

  /* Put head node on page list... be sure to free this somewhere hack */
  s_page_add_head(pr_current);

  /* Now create a blank page */
  pr_current->page_tail = s_page_add(pr_current,
                                     pr_current->page_tail, "unknown");
  /* this is correct */

  s_page_setup(pr_current->page_tail);

  /* setup page_current link */
  pr_current->page_current = pr_current->page_tail;

  /* Special case init */
  set_window(pr_current, pr_current->init_left, pr_current->init_right,
             pr_current->init_top, pr_current->init_bottom);

  global_wid++;
  num_projects++;

  project_tail = s_project_add(project_tail, pr_current);


  /* renable the events */
  pr_current->DONT_DRAW_CONN = 0;
  pr_current->DONT_RESIZE = 0;
  pr_current->DONT_EXPOSE = 0;
  pr_current->DONT_RECALC = 0;

  return (pr_current);
}

/* stays the same ???????????? */
void s_project_close(TOPLEVEL * pr_current)
{

    /* make sure project_tail stays correct and doesn't dangle */
    /* project_head can't dangle since it has a head node, which is */
    /* NEVER deallocated (only at the very end) */
    if (project_tail == pr_current) {
	project_tail = pr_current->prev;
    }


    if (pr_current->series_name) {
	free(pr_current->series_name);
    }

    if (pr_current->untitled_name) {
	free(pr_current->untitled_name);
    }

    if (pr_current->font_directory) {
	free(pr_current->font_directory);
    }

    if (pr_current->scheme_directory) {
	free(pr_current->scheme_directory);
    }

    /* close the log file */
    s_log_close();

    /* free all fonts */
    /* if you close a window, then you free the font set... */
    /* this is probably a bad idea... */
    o_text_freeallfonts(pr_current);

    s_project_delete(project_head, pr_current);

    num_projects = num_projects - 1;

    free(pr_current);

    /* just closed last window, so quit */
    if (num_projects == 0) {
	gnetlist_quit();
    }
}

void s_project_close_all()
{
    TOPLEVEL *pr_current;
    TOPLEVEL *w_prev;

    pr_current = project_tail;

    /* loop over all windows to close */
    /* going backwards */
    /* wid == -1 is the head and we are done. */
    while (pr_current != NULL && pr_current->wid != -1) {
	w_prev = pr_current->prev;
	s_project_close(pr_current);
	pr_current = w_prev;
    }

    /* now free the head */
    /* only if all the windows are gone */

    if (project_head->next == NULL && num_projects == 0) {
	s_project_free_head();
    }
}

TOPLEVEL *s_project_get_ptr(int wid)
{
    TOPLEVEL *pr_current;

    pr_current = project_head;

    while (pr_current != NULL) {
	if (pr_current->wid == wid) {
	    return (pr_current);
	}

	pr_current = pr_current->next;
    }

    return (NULL);
}
