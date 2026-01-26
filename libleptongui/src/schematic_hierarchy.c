/* Lepton EDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2016 gEDA Contributors
 * Copyright (C) 2017-2026 Lepton EDA Contributors
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
#include "schematic.h"

/*! \brief */
static int page_control_counter=0;

int
schematic_hierarchy_get_page_control_counter ()
{
  return page_control_counter;
}

void
schematic_hierarchy_increment_page_control_counter ()
{
  page_control_counter++;
}


/*! \brief Set the error \a EDA_ERROR_NOLIB.
 *
 *  \par Function Description
 *  Sets the \c GError argument \p err to \a EDA_ERROR_NOLIB.
 *  This function is intended to be called from Scheme until a
 *  Scheme replacement function is introduced.
 *
 *  \param [in,out] err The \c GError pointer.
 */
void
schematic_hierarchy_set_error_nolib (GError **err)
{
  g_set_error (err, EDA_ERROR, EDA_ERROR_NOLIB,
               _("Schematic not found in source library."));
}


/*!
 *  \brief Search for schematic associated source files and load them.
 *  \par Function Description
 *  This function searches the associated source file refered by the
 *  <B>filename</B> and loads it.  If the page is already in the list of
 *  pages it will return the <B>pid</B> of that page.
 *
 *  \param [in] w_current     The SchematicWindow object.
 *  \param [in] filename      Schematic file name.
 *  \param [in] parent        The parent page of the schematic.
 *  \param [in] page_control
 *  \param [out] err         Location to return a GError on failure.
 *  \return The page loaded, or NULL if failed.
 */
LeptonPage *
s_hierarchy_down_schematic_single (SchematicWindow *w_current,
                                   gchar *filename,
                                   LeptonPage *parent,
                                   int page_control,
                                   GError **err)
{
  LeptonPage *found = NULL;
  LeptonPage *forbear;

  LeptonToplevel *toplevel = schematic_window_get_toplevel (w_current);

  g_return_val_if_fail ((toplevel != NULL), NULL);

  gchar *normalized_filename = f_normalize_filename (filename, NULL);
  found = lepton_toplevel_search_page (toplevel, normalized_filename);
  g_free (normalized_filename);

  if (found) {
    /* check whether this page is in the parents list */
    for (forbear = parent;
         forbear != NULL
           && lepton_page_get_pid (found) != lepton_page_get_pid (forbear)
           && lepton_page_get_up (forbear) >= 0;
         forbear = lepton_toplevel_search_page_by_id (lepton_toplevel_get_pages (toplevel),
                                                      lepton_page_get_up (forbear)))
      ; /* void */

    if (forbear != NULL
        && lepton_page_get_pid (found) == lepton_page_get_pid (forbear))
    {
      g_set_error (err, EDA_ERROR, EDA_ERROR_LOOP,
                   _("Hierarchy contains a circular dependency."));
      return NULL;  /* error signal */
    }
    lepton_toplevel_goto_page (toplevel, found);
    if (page_control != 0) {
      lepton_page_set_page_control (found, page_control);
    }
    lepton_page_set_up (found, lepton_page_get_pid (parent));
    return found;
  }

  found = lepton_page_new (toplevel, filename);

  schematic_file_open (w_current,
                       found,
                       lepton_page_get_filename (found),
                       NULL);

  if (page_control == 0) {
    schematic_hierarchy_increment_page_control_counter ();
    lepton_page_set_page_control (found,
                                  schematic_hierarchy_get_page_control_counter ());
  } else {
    lepton_page_set_page_control (found, page_control);
  }

  lepton_page_set_up (found, lepton_page_get_pid (parent));

  return found;
}


/*! \brief Search for the parent page of a page in hierarchy.
 *  \par Function Description
 *  This function searches the parent page of page \a page in the
 *  hierarchy.
 *
 *  It returns a pointer on the page if found, NULL otherwise.
 *
 *  \param [in] current_page The reference page for the search.
 *  \returns A pointer on the page found or NULL if not found.
 */
LeptonPage *
s_hierarchy_find_up_page (LeptonPage *current_page)
{
  g_return_val_if_fail (current_page != NULL, NULL);

  LeptonToplevel *toplevel =
    lepton_page_get_toplevel (current_page);
  g_return_val_if_fail (toplevel != NULL, NULL);

  if (current_page->up < 0) {
    g_debug(_("There are no schematics above the current one!"));
    return NULL;
  }

  return lepton_toplevel_search_page_by_id (toplevel->pages, current_page->up);
}


/*! \brief Load a subpage
 *
 *  \par Function Description
 *  Implements s_hierarchy_down_schematic(), but without changing variables
 *  related to the UI.
 *
 *  - Ensures a duplicate page is not loaded
 *  - Does not change the current page
 *  - Does not modify the most recent "up" page
 *
 *  \param [in]  w_current The current #SchematicWindow environment.
 *  \param [in]  page      The current page.
 *  \param [in]  filename  The name of the subpage to open.
 *  \param [in,out] error \c GError structure for error reporting,
 *                        or NULL to disable error reporting.
 *  \return A pointer to the subpage or NULL if an error occured.
 */
LeptonPage*
s_hierarchy_load_subpage (SchematicWindow *w_current,
                          LeptonPage *page,
                          const char *filename,
                          GError **error)
{
  char *string;
  LeptonPage *subpage = NULL;

  g_return_val_if_fail (filename != NULL, NULL);
  g_return_val_if_fail (page != NULL, NULL);

  LeptonToplevel *toplevel = lepton_page_get_toplevel (page);

  g_return_val_if_fail (toplevel != NULL, NULL);

  SCM string_s = scm_call_1 (scm_c_public_ref ("lepton library",
                                               "get-source-library-file"),
                             scm_from_utf8_string (filename));

  if (scm_is_false (string_s)) {
    g_set_error (error,
                 EDA_ERROR,
                 EDA_ERROR_NOLIB,
                 _("Schematic not found in source library."));
  } else {
    string = scm_to_utf8_string (string_s);
    gchar *normalized = f_normalize_filename (string, error);

    subpage = lepton_toplevel_search_page (toplevel, normalized);

    if (subpage == NULL) {
      int success;

      subpage = lepton_page_new (toplevel, string);
      success = schematic_file_open (w_current,
                                     subpage,
                                     lepton_page_get_filename (subpage),
                                     error);

      if (success) {
        subpage->page_control = ++page_control_counter;
      } else {
        lepton_page_delete (toplevel, subpage);
        subpage = NULL;
      }
    }

    g_free (normalized);
  }

  return subpage;
}
