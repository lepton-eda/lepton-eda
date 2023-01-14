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

static char* tmp_path = NULL;


/* this is additional number of levels (or history) at which point the */
/* undo stack will be trimmed, it's used a safety to prevent running out */
/* of entries to free */
#define UNDO_PADDING  5


/*! \brief Return current undo file index for backup names.
 *
 * \return The current file index.
 */
int
schematic_undo_get_file_index ()
{
  return undo_file_index;
}


/*! \brief Return temp directory for undo.
 *
 * \return The temp directory name.
 */
char*
schematic_undo_get_tmp_path ()
{
  return tmp_path;
}


/*! \brief Set temp directory path for undo.
 *
 * \par Function Description
 *
 * The function sets temp directory for undo to \a path.  Use
 * NULL to unset the value.
 *
 * \param [in] path A new temp path value.
 */
void
schematic_undo_set_tmp_path (char* path)
{
  g_free (tmp_path);
  tmp_path = g_strdup (path);
}


/*! \brief Return undo backup file name by index.
 *
 * \par Function Description
 *
 * This function returns an undo backup copy file name by given \a
 * index.  The name is absolute and is formed from the path to a
 * temporary directory, program file name, the PID of the running
 * program, and the index.
 *
 * \param [in] index An integer to create file name for.
 * \return A new undo backup file name string.
 *
 * \note
 * Caller must g_free returned character string.
 */
char*
schematic_undo_index_to_filename (int index)
{
  char *filename =
    g_strdup_printf ("%s%clepton-schematic.save%d_%d.sch",
                     schematic_undo_get_tmp_path (),
                     G_DIR_SEPARATOR,
                     getpid (),
                     index);
  return filename;
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

gboolean
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

    filename = schematic_undo_index_to_filename (undo_file_index++);

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
