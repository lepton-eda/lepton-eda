/* Lepton EDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2016 gEDA Contributors
 * Copyright (C) 2017-2025 Lepton EDA Contributors
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
                                   const gchar *filename,
                                   LeptonPage *parent,
                                   int page_control,
                                   GError **err)
{
  gchar *string;
  LeptonPage *found = NULL;
  LeptonPage *forbear;

  g_return_val_if_fail ((w_current != NULL), NULL);

  LeptonToplevel *toplevel = schematic_window_get_toplevel (w_current);

  g_return_val_if_fail ((toplevel != NULL), NULL);
  g_return_val_if_fail ((filename != NULL), NULL);
  g_return_val_if_fail ((parent != NULL), NULL);

  SCM string_s = scm_call_1 (scm_c_public_ref ("lepton library",
                                               "get-source-library-file"),
                             scm_from_utf8_string (filename));

  if (scm_is_false (string_s)) {
    g_set_error (err, EDA_ERROR, EDA_ERROR_NOLIB,
                 _("Schematic not found in source library."));
    return NULL;
  }

  string = scm_to_utf8_string (string_s);

  gchar *normalized_filename = f_normalize_filename (string, NULL);
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
    g_free (string);
    return found;
  }

  found = lepton_page_new (toplevel, string);

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

  g_free (string);

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

/*! \brief Find page hierarchy below a page.
 *  \par Function Description
 *  This function traverses the hierarchy tree of pages and returns a
 *  flat list of pages that are below \a p_current.
 *
 *  \param w_current The SchematicWindow structure.
 *  \param p_current The LeptonPage to traverse hierarchy for.
 *  \param flags Flags controlling form of return value.
 *  \return A GList of LeptonPage pointers.
 *
 *  \warning
 *  Caller must destroy returned GList with g_list_free().
 */
GList *
s_hierarchy_traversepages (SchematicWindow *w_current,
                           LeptonPage *p_current,
                           gint flags)
{
  LeptonObject *o_current;
  LeptonPage *child_page;
  char *filename = NULL;
  static GList *pages = NULL;
  const GList *iter;

  g_return_val_if_fail ((p_current != NULL), NULL);

  /* init static variables the first time*/
  if (!(flags & HIERARCHY_INNERLOOP)) {
    pages = NULL;
  }

  /* preorder traversing */
  /* check whether we already visited this page */
  if (g_list_find (pages, p_current) != NULL)
  {
    return pages;  /* drop the page subtree */
  }
  pages = g_list_append (pages, p_current);

  /* walk throught the page objects and search for underlaying schematics */
  for (iter = lepton_page_objects (p_current);
       iter != NULL ;
       iter = g_list_next (iter)) {
    o_current = (LeptonObject *)iter->data;

    /* only complex things like symbols can contain attributes */
    if (!lepton_object_is_component (o_current)) continue;

    filename =
      lepton_attrib_search_attached_attribs_by_name (o_current, "source", 0);

    /* if above is NULL, then look inside symbol */
    if (filename == NULL) {
      filename =
        lepton_attrib_search_inherited_attribs_by_name (o_current, "source", 0);
    }

    if (filename == NULL) continue;

    /* we got a schematic source attribute
       lets load the page and dive into it */
    GError *err = NULL;
    child_page =
      s_hierarchy_down_schematic_single (w_current, filename, p_current, 0, &err);
    if (child_page != NULL) {
      /* call the recursive function */
      s_hierarchy_traversepages (w_current, child_page, flags | HIERARCHY_INNERLOOP);
    } else {
      g_message (_("Failed to descend hierarchy into '%1$s': %2$s"),
                 filename, err->message);
      g_error_free (err);
    }

    g_free (filename);
    filename = NULL;
  }

  return pages;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \note
 *  Test function which only prints the name of a page and its number.
 */
gint
s_hierarchy_print_page (LeptonPage *p_current,
                        void * data)
{
  printf("pagefilename: %s pageid: %d\n",
         lepton_page_get_filename (p_current),
         lepton_page_get_pid (p_current));
  return 0;
}
