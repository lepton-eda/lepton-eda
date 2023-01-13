/* Lepton EDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2015 gEDA Contributors
 * Copyright (C) 2017-2023 Lepton EDA Contributors
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
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



/*! \brief Return the value of "modify-viewport" configuration key.
 *
 * \par Function Description
 *
 * This function reads the value of "modify-viewport" configuration
 * setting in "schematic.undo" group, which determines
 * if undo/redo operations are allowed to change pan and zoom (i.e. viewport)
 * when "undo-panzoom" option (in gschemrc) is set to "disabled".
 *
 * Configuration setting description:
 * key:   modify-viewport
 * group: schematic.undo
 * type:  boolean
 * default value: false
 *
 * \return TRUE if undo/redo can modify viewport, FALSE otherwise.
 */

static gboolean
o_undo_modify_viewport()
{
  gboolean result = FALSE; /* option's default value */
  gchar* cwd = g_get_current_dir();

  EdaConfig* cfg = eda_config_get_context_for_path (cwd);

  g_free (cwd);

  if (cfg == NULL)
  {
    return result;
  }

  GError* err = NULL;
  gboolean val = eda_config_get_boolean (cfg,
                                         "schematic.undo",
                                         "modify-viewport",
                                         &err);
  if (err == NULL)
  {
    result = val;
  }

  g_clear_error (&err);

  return result;
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
void
o_undo_savestate (GschemToplevel *w_current,
                  LeptonPage *page,
                  int flag)
{
  LeptonToplevel *toplevel = gschem_toplevel_get_toplevel (w_current);
  char *filename = NULL;
  GList *object_list = NULL;
  int levels;
  LeptonUndo *u_current;
  LeptonUndo *u_current_next;

  GschemPageView *view = gschem_toplevel_get_current_page_view (w_current);
  g_return_if_fail (view != NULL);

  g_return_if_fail (page != NULL);

  GschemPageGeometry *geometry = gschem_page_view_get_page_geometry (view);

  /* save autosave backups if necessary */
  o_autosave_backups(w_current);

  if (w_current->undo_control == FALSE) {
    return;
  }

  if (flag == UNDO_ALL) {

    /* Increment the number of operations since last backup if
       auto-save is enabled */
    if (toplevel->auto_save_interval != 0) {
      page->ops_since_last_backup++;
    }

    /* HACK */
    /* Before we save the undo state, consolidate nets as necessary */

    /* This is where the net consolidation call would have been
     * triggered before it was removed from o_save_buffer().
     */
    lepton_net_object_consolidate (page);
  }

  if (w_current->undo_type == UNDO_DISK && flag == UNDO_ALL) {

    filename = g_strdup_printf("%s%clepton-schematic.save%d_%d.sch",
                               tmp_path, G_DIR_SEPARATOR,
                               prog_pid, undo_file_index++);

    /* Changed from f_save to o_save when adding backup copy creation. */
    /* f_save manages the creaton of backup copies.
       This way, f_save is called only when saving a file, and not when
       saving an undo backup copy */
    o_save (lepton_page_objects (page), filename, NULL);

  } else if (w_current->undo_type == UNDO_MEMORY && flag == UNDO_ALL) {
    object_list = o_glist_copy_all (lepton_page_objects (page),
                                    object_list);
  }

  /* Clear Anything above current */
  if (page->undo_current) {
    lepton_undo_remove_rest (page->undo_current->next);
    page->undo_current->next = NULL;
  } else { /* undo current is NULL */
    lepton_undo_remove_rest (page->undo_bottom);
    page->undo_bottom = NULL;
  }

  page->undo_tos = page->undo_current;

  if (geometry != NULL) {
    page->undo_tos =
      lepton_undo_add (page->undo_tos,
                       flag, filename, object_list,
                       (geometry->viewport_left + geometry->viewport_right) / 2,
                       (geometry->viewport_top + geometry->viewport_bottom) / 2,
                       /* scale */
                       MAX (((double) abs (geometry->viewport_right - geometry->viewport_left) / geometry->screen_width),
                            ((double) abs (geometry->viewport_top - geometry->viewport_bottom) / geometry->screen_height)),
                       page->page_control,
                       page->up);
  } else {
    page->undo_tos = lepton_undo_add (page->undo_tos,
                                      flag, filename, object_list,
                                      0, /* center x */
                                      0, /* center y */
                                      0, /* scale */
                                      page->page_control,
                                      page->up);
  }

  page->undo_current =
      page->undo_tos;

  if (page->undo_bottom == NULL) {
    page->undo_bottom =
        page->undo_tos;
  }

#if DEBUG
  printf("\n\n---Undo----\n");
  lepton_undo_print_all (page->undo_bottom);
  printf("BOTTOM: %s\n", page->undo_bottom->filename);
  printf("TOS: %s\n", page->undo_tos->filename);
  printf("CURRENT: %s\n", page->undo_current->filename);
  printf("----\n");
#endif

  g_free(filename);

  /* Now go through and see if we need to free/remove some undo levels */
  /* so we stay within the limits */

  /* only check history every 10 undo savestates */
  if (undo_file_index % 10) {
    return;
  }

  levels = lepton_undo_levels (page->undo_bottom);

#if DEBUG
  printf("levels: %d\n", levels);
#endif

  if (levels >= w_current->undo_levels + UNDO_PADDING) {
    levels = levels - w_current->undo_levels;

#if DEBUG
    printf("Trimming: %d levels\n", levels);
#endif

    u_current = page->undo_bottom;

    while (levels > 0) {
      /* Because we use a pad you are always guaranteed to never */
      /* exhaust the list */
      g_assert (u_current != NULL);

      u_current_next = u_current->next;

      if (u_current->filename) {
#if DEBUG
        printf("Freeing: %s\n", u_current->filename);
#endif
        unlink(u_current->filename);
        g_free(u_current->filename);
      }

      if (u_current->object_list) {
        lepton_object_list_delete (u_current->object_list);
        u_current->object_list = NULL;
      }

      u_current->next = NULL;
      u_current->prev = NULL;
      g_free(u_current);

      u_current = u_current_next;
      levels--;
    }

    g_assert (u_current != NULL);
    u_current->prev = NULL;
    page->undo_bottom = u_current;

#if DEBUG
    printf("New current is: %s\n", u_current->filename);
#endif
  }

#if DEBUG
  printf("\n\n---Undo----\n");
  lepton_undo_print_all (page->undo_bottom);
  printf("BOTTOM: %s\n", page->undo_bottom->filename);
  printf("TOS: %s\n", page->undo_tos->filename);
  printf("CURRENT: %s\n", page->undo_current->filename);
  printf("----\n");
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
void
o_undo_savestate_old (GschemToplevel *w_current, int flag)
{
  GschemPageView *page_view = gschem_toplevel_get_current_page_view (w_current);
  g_return_if_fail (page_view != NULL);

  LeptonPage *page = gschem_page_view_get_page (page_view);

  o_undo_savestate (w_current, page, flag);
}


/*! \brief Save viewport undo information.
 *
 * \par Function Description
 *
 * This function saves undo information marking the record as a
 * viewport only one.
 *
 * \param [in] w_current The current schematic window instance.
 */
void
o_undo_savestate_viewport (GschemToplevel *w_current)
{
  g_return_if_fail (w_current != NULL);

  LeptonPage *page = schematic_window_get_active_page (w_current);

  o_undo_savestate (w_current, page, UNDO_VIEWPORT_ONLY);
}


/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
char*
o_undo_find_prev_filename (LeptonUndo *start)
{
  LeptonUndo *u_current;

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
GList *o_undo_find_prev_object_head (LeptonUndo *start)
{
  LeptonUndo *u_current;

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
 *  If \a redo is TRUE, do "redo" instead of "undo".
 */
void
o_undo_callback (GschemToplevel *w_current,
                 LeptonPage *page,
                 gboolean redo)
{
  LeptonToplevel *toplevel = gschem_toplevel_get_toplevel (w_current);
  LeptonUndo *u_current;
  LeptonUndo *u_next;
  LeptonUndo *save_bottom;
  LeptonUndo *save_tos;
  LeptonUndo *save_current;
  int save_logging;
  int find_prev_data=FALSE;

  char *save_filename;

  g_return_if_fail (w_current != NULL);
  g_return_if_fail (page != NULL);

  if (page->undo_current == NULL) {
    return;
  }

  if (!redo)
  {
    /* Undo action. */
    u_current = page->undo_current->prev;
  }
  else
  {
    /* Redo action. */
    u_current = page->undo_current->next;
  }

  u_next = page->undo_current;

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
  save_filename = g_strdup (lepton_page_get_filename (page));

  /* save structure so it's not nuked */
  save_bottom = page->undo_bottom;
  save_tos = page->undo_tos;
  save_current = page->undo_current;
  page->undo_bottom = NULL;
  page->undo_tos = NULL;
  page->undo_current = NULL;

  o_select_unselect_all (w_current);

  if ((w_current->undo_type == UNDO_DISK && u_current->filename) ||
      (w_current->undo_type == UNDO_MEMORY && u_current->object_list)) {
    /* delete objects of page */
    lepton_page_delete_objects (page);

    /* Free the objects in the place list. */
    lepton_object_list_delete (page->place_list);
    page->place_list = NULL;

    schematic_window_active_page_changed (w_current);
  }

  /* temporarily disable logging */
  save_logging = do_logging;
  do_logging = FALSE;

  if (w_current->undo_type == UNDO_DISK && u_current->filename) {

    /*
     * F_OPEN_RESTORE_CWD: go back from tmp directory,
     * so that local config files can be read:
    */
    f_open (toplevel, page, u_current->filename, F_OPEN_RESTORE_CWD, NULL);

  } else if (w_current->undo_type == UNDO_MEMORY && u_current->object_list) {

    lepton_page_append_list (page,
                             o_glist_copy_all (u_current->object_list,
                                               NULL));
  }

  page->page_control = u_current->page_control;
  page->up = u_current->up;
  gschem_toplevel_page_content_changed (w_current, page);

  GschemPageView *view = gschem_toplevel_get_current_page_view (w_current);
  g_return_if_fail (view != NULL);

  GschemPageGeometry *geometry = gschem_page_view_get_page_geometry (view);

  if (w_current->undo_panzoom || o_undo_modify_viewport())
  {
    if (u_current->scale != 0) {
      gschem_page_geometry_set_viewport (geometry,
                                         u_current->x,
                                         u_current->y,
                                         u_current->scale);
      gschem_page_view_invalidate_all (view);
    } else {
      gschem_page_view_zoom_extents (view, u_current->object_list);
    }
  }

  /* restore logging */
  do_logging = save_logging;

  /* set filename right */
  lepton_page_set_filename (page, save_filename);
  g_free(save_filename);

  /* final redraw */
  page_select_widget_update (w_current);
  x_multiattrib_update (w_current);
  i_update_menus(w_current);

  /* restore saved undo structures */
  page->undo_bottom = save_bottom;
  page->undo_tos = save_tos;
  page->undo_current = save_current;

  if (!redo)
  {
    /* Undo action. */
    if (page->undo_current) {
      page->undo_current = page->undo_current->prev;
      if (page->undo_current == NULL) {
        page->undo_current = page->undo_bottom;
      }
    }
  }
  else
  {
    /* Redo action. */
    if (page->undo_current) {
      page->undo_current = page->undo_current->next;
      if (page->undo_current == NULL) {
        page->undo_current = page->undo_tos;
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
  lepton_undo_print_all (page->undo_bottom);
  printf("TOS: %s\n", page->undo_tos->filename);
  printf("CURRENT: %s\n", page->undo_current->filename);
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
    filename = g_strdup_printf("%s%clepton-schematic.save%d_%d.sch", tmp_path,
                               G_DIR_SEPARATOR, prog_pid, i);
    unlink(filename);
    g_free(filename);
  }

  g_free(tmp_path);
  tmp_path = NULL;
}
