/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 * Copyright (C) 1998-2008 Ales Hvezda
 * Copyright (C) 1998-2008 gEDA Contributors (see ChangeLog for details)
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
#include <math.h>
#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "gschem.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

static int undo_file_index=0;
static int prog_pid=0;

static char* tmp_path = NULL;

/* this is additional number of levels (or history) at which point the */
/* undo stack will be trimmed, it's used a safety to prevent running out */ 
/* of entries to free */
#define UNDO_PADDING  5

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_undo_init(void)
{
  prog_pid = getpid();

  tmp_path = g_strdup (getenv("TMP"));
  if (tmp_path == NULL) {
     tmp_path = g_strdup ("/tmp");
  }
#if DEBUG
  printf("%s\n", tmp_path);
#endif
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  
 *  <B>flag</B> can be one of the following values:
 *  <DL>
 *    <DT>*</DT><DD>UNDO_ALL
 *    <DT>*</DT><DD>UNDO_VIEWPORT_ONLY
 *  </DL>
 */
void o_undo_savestate(GSCHEM_TOPLEVEL *w_current, int flag)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  char *filename = NULL;
  GList *object_list = NULL;
  int levels;
  UNDO *u_current;
  UNDO *u_current_next;

  /* save autosave backups if necessary */
  o_autosave_backups(w_current);

  if (w_current->undo_control == FALSE) {
    return;
  }

  if (w_current->undo_type == UNDO_DISK && flag == UNDO_ALL) {

    /* Increment the number of operations since last backup if 
       auto-save is enabled */
    if (toplevel->auto_save_interval != 0) {
      toplevel->page_current->ops_since_last_backup++;
    }

    filename = g_strdup_printf("%s%cgschem.save%d_%d.sch",
                               tmp_path, G_DIR_SEPARATOR,
                               prog_pid, undo_file_index++);

    /* Changed from f_save to o_save when adding backup copy creation. */
    /* f_save manages the creaton of backup copies. 
       This way, f_save is called only when saving a file, and not when
       saving an undo backup copy */
    o_save_curr_page (toplevel, filename);


  } else if (w_current->undo_type == UNDO_MEMORY && flag == UNDO_ALL) {

    /* Increment the number of operations since last backup if 
       auto-save is enabled */
    if (toplevel->auto_save_interval != 0) {
      toplevel->page_current->ops_since_last_backup++;
    }

    object_list = o_glist_copy_all (toplevel,
                                    s_page_objects (toplevel->page_current),
                                    object_list, SELECTION_FLAG);
  }

  /* Clear Anything above current */
  if (toplevel->page_current->undo_current) {
    s_undo_remove_rest(toplevel,
                       toplevel->page_current->undo_current->next);
    toplevel->page_current->undo_current->next = NULL;
  } else { /* undo current is NULL */
    s_undo_remove_rest(toplevel,
                       toplevel->page_current->undo_bottom);
    toplevel->page_current->undo_bottom = NULL;
  }

  toplevel->page_current->undo_tos = toplevel->page_current->undo_current;

  toplevel->page_current->undo_tos =
  s_undo_add(toplevel->page_current->undo_tos,
             flag, filename, object_list,
             toplevel->page_current->left,
             toplevel->page_current->top,
             toplevel->page_current->right,
             toplevel->page_current->bottom,
             toplevel->page_current->page_control,
             toplevel->page_current->up);

  toplevel->page_current->undo_current =
      toplevel->page_current->undo_tos;

  if (toplevel->page_current->undo_bottom == NULL) {
    toplevel->page_current->undo_bottom =
        toplevel->page_current->undo_tos;
  }

#if DEBUG
  printf("\n\n---Undo----\n");
  s_undo_print_all(toplevel->page_current->undo_bottom);
  printf("BOTTOM: %s\n", toplevel->page_current->undo_bottom->filename);
  printf("TOS: %s\n", toplevel->page_current->undo_tos->filename);
  printf("CURRENT: %s\n", toplevel->page_current->undo_current->filename);
  printf("----\n");
#endif

  g_free(filename);

  /* Now go through and see if we need to free/remove some undo levels */ 
  /* so we stay within the limits */

  /* only check history every 10 undo savestates */
  if (undo_file_index % 10) {
    return;
  }

  levels = s_undo_levels(toplevel->page_current->undo_bottom);

#if DEBUG
  printf("levels: %d\n", levels);
#endif

  if (levels >= w_current->undo_levels + UNDO_PADDING) {
    levels = levels - w_current->undo_levels;

#if DEBUG
    printf("Trimming: %d levels\n", levels);
#endif

    u_current = toplevel->page_current->undo_bottom;
    while(u_current && levels > 0) {
      u_current_next = u_current->next;

      if (u_current->filename) {
#if DEBUG
        printf("Freeing: %s\n", u_current->filename);
#endif
        unlink(u_current->filename);
        g_free(u_current->filename);
      }

      if (u_current->object_list) {
        s_delete_object_glist (toplevel, u_current->object_list);
        u_current->object_list = NULL;
      }

      u_current->next = NULL;
      u_current->prev = NULL;
      g_free(u_current);

      u_current = u_current_next;
      levels--;
    }

    /* Because we use a pad you are always garanteed to never */
    /* exhaust the list */
    u_current->prev = NULL;
    toplevel->page_current->undo_bottom = u_current;

#if DEBUG
    printf("New current is: %s\n", u_current->filename);
#endif
  }

#if DEBUG
  printf("\n\n---Undo----\n");
  s_undo_print_all(toplevel->page_current->undo_bottom);
  printf("BOTTOM: %s\n", toplevel->page_current->undo_bottom->filename);
  printf("TOS: %s\n", toplevel->page_current->undo_tos->filename);
  printf("CURRENT: %s\n", toplevel->page_current->undo_current->filename);
  printf("----\n");
#endif

}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
char *o_undo_find_prev_filename(UNDO *start)
{
  UNDO *u_current;

  u_current = start->prev;

  while(u_current) {
    if (u_current->filename) {
      return(u_current->filename);
    }
    u_current = u_current->prev;
  }

  return(NULL); 
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
GList *o_undo_find_prev_object_head (UNDO *start)
{
  UNDO *u_current;

  u_current = start->prev;

  while(u_current) {
    if (u_current->object_list) {
      return u_current->object_list;
    }
    u_current = u_current->prev;
  }

  return(NULL); 
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  <B>type</B> can be one of the following values:
 *  <DL>
 *    <DT>*</DT><DD>UNDO_ACTION
 *    <DT>*</DT><DD>REDO_ACTION
 *  </DL>
 */
void o_undo_callback(GSCHEM_TOPLEVEL *w_current, int type)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  UNDO *u_current;
  UNDO *u_next;
  UNDO *save_bottom;
  UNDO *save_tos;
  UNDO *save_current;
  int save_logging;
  int find_prev_data=FALSE;
  int prev_status;

  char *save_filename;

  if (w_current->undo_control == FALSE) {
    s_log_message(_("Undo/Redo disabled in rc file\n"));
    return;
  }

  if (toplevel->page_current->undo_current == NULL) {
    return;
  }

  if (type == UNDO_ACTION) {
    u_current = toplevel->page_current->undo_current->prev;
  } else {
    u_current = toplevel->page_current->undo_current->next;
  }

  u_next = toplevel->page_current->undo_current;

  if (u_current == NULL) {
    return;
  }

  if (u_next->type == UNDO_ALL && u_current->type == UNDO_VIEWPORT_ONLY) {
#if DEBUG
    printf("Type: %d\n", u_current->type);
    printf("Current is an undo all, next is viewport only!\n");
#endif
    find_prev_data = TRUE;

    if (w_current->undo_type == UNDO_DISK) {
      u_current->filename = o_undo_find_prev_filename(u_current);
    } else {
      u_current->object_list = o_undo_find_prev_object_head (u_current);
    }
  }

  /* save filename */
  save_filename = g_strdup (toplevel->page_current->page_filename);

  /* save structure so it's not nuked */
  save_bottom = toplevel->page_current->undo_bottom;
  save_tos = toplevel->page_current->undo_tos;
  save_current = toplevel->page_current->undo_current;
  toplevel->page_current->undo_bottom = NULL;
  toplevel->page_current->undo_tos = NULL;
  toplevel->page_current->undo_current = NULL;

  if (w_current->undo_type == UNDO_DISK && u_current->filename) {
    PAGE *p_new;
    s_page_delete (toplevel, toplevel->page_current);
    p_new = s_page_new(toplevel, u_current->filename);
    s_page_goto (toplevel, p_new);
  } else if (w_current->undo_type == UNDO_MEMORY && u_current->object_list) {
    PAGE *p_new;
    s_page_delete (toplevel, toplevel->page_current);
    p_new = s_page_new (toplevel, save_filename);
    s_page_goto (toplevel, p_new);
  }

  /* temporarily disable logging */
  save_logging = do_logging;
  prev_status = toplevel->DONT_REDRAW;
  toplevel->DONT_REDRAW = 1;
  do_logging = FALSE;

  if (w_current->undo_type == UNDO_DISK && u_current->filename) {

    f_open(toplevel, u_current->filename, NULL);

    x_manual_resize(w_current);
    toplevel->page_current->page_control = u_current->page_control;
    toplevel->page_current->up = u_current->up;
    toplevel->page_current->CHANGED=1;

  } else if (w_current->undo_type == UNDO_MEMORY && u_current->object_list) {

    s_page_delete_objects (toplevel, toplevel->page_current);

    s_page_append_list (toplevel, toplevel->page_current,
                        o_glist_copy_all (toplevel, u_current->object_list,
                                          NULL, NORMAL_FLAG));

    x_manual_resize(w_current);
    toplevel->page_current->page_control = u_current->page_control;
    toplevel->page_current->up = u_current->up;
    toplevel->page_current->CHANGED=1;
  }

  /* do misc setups */
  set_window(toplevel, toplevel->page_current,
             u_current->left, u_current->right,
             u_current->top, u_current->bottom);
  x_hscrollbar_update(w_current);
  x_vscrollbar_update(w_current);

  /* restore logging */
  do_logging = save_logging;

  /* set filename right */
  g_free(toplevel->page_current->page_filename);
  toplevel->page_current->page_filename = save_filename;

  /* final redraw */
  x_pagesel_update (w_current);
  x_multiattrib_update (w_current);

  /* Let the caller to decide if redraw or not */
  /* toplevel->DONT_REDRAW = 0; */
  toplevel->DONT_REDRAW = prev_status;

  if (!toplevel->DONT_REDRAW) {
    o_invalidate_all (w_current);
  }
  i_update_menus(w_current);

  /* restore saved undo structures */
  toplevel->page_current->undo_bottom = save_bottom;
  toplevel->page_current->undo_tos = save_tos;
  toplevel->page_current->undo_current = save_current;

  if (type == UNDO_ACTION) {
    if (toplevel->page_current->undo_current) {
      toplevel->page_current->undo_current =
          toplevel->page_current->undo_current->prev;
      if (toplevel->page_current->undo_current == NULL) {
        toplevel->page_current->undo_current =
            toplevel->page_current->undo_bottom;
      }
    }
  } else { /* type is REDO_ACTION */
    if (toplevel->page_current->undo_current) {
      toplevel->page_current->undo_current =
          toplevel->page_current->undo_current->next;
      if (toplevel->page_current->undo_current == NULL) {
        toplevel->page_current->undo_current =
            toplevel->page_current->undo_tos;
      }
    }
  }

  /* don't have to free data here since filename, object_list are */
  /* just pointers to the real data (lower in the stack) */
  if (find_prev_data) {
    u_current->filename = NULL;
    u_current->object_list = NULL;
  }

#if DEBUG
  printf("\n\n---Undo----\n");
  s_undo_print_all(toplevel->page_current->undo_bottom);
  printf("TOS: %s\n", toplevel->page_current->undo_tos->filename);
  printf("CURRENT: %s\n", toplevel->page_current->undo_current->filename);
  printf("----\n");
#endif
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_undo_cleanup(void)
{
  int i;
  char *filename;

  for (i = 0 ; i < undo_file_index; i++) {
    filename = g_strdup_printf("%s%cgschem.save%d_%d.sch", tmp_path,
                               G_DIR_SEPARATOR, prog_pid, i);
    unlink(filename);
    g_free(filename);
  }

  g_free(tmp_path);
  tmp_path = NULL;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_undo_remove_last_undo(GSCHEM_TOPLEVEL *w_current)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  if (toplevel->page_current->undo_current == NULL) {
    return;
  }

  if (toplevel->page_current->undo_current) {
    toplevel->page_current->undo_current =
        toplevel->page_current->undo_current->prev;
    if (toplevel->page_current->undo_current == NULL) {
      toplevel->page_current->undo_current =
          toplevel->page_current->undo_bottom;
    }
  }



}
