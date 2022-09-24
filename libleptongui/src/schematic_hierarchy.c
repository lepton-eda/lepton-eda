/* Lepton EDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2016 gEDA Contributors
 * Copyright (C) 2017-2022 Lepton EDA Contributors
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
#include "gschem.h"

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
 *  \param [in] w_current     The GschemToplevel object.
 *  \param [in] filename      Schematic file name.
 *  \param [in] parent        The parent page of the schematic.
 *  \param [in] page_control
 *  \param [out] err         Location to return a GError on failure.
 *  \return The page loaded, or NULL if failed.
 */
LeptonPage *
s_hierarchy_down_schematic_single (GschemToplevel *w_current,
                                   const gchar *filename,
                                   LeptonPage *parent,
                                   int page_control,
                                   GError **err)
{
  gchar *string;
  LeptonPage *found = NULL;
  LeptonPage *forbear;

  g_return_val_if_fail ((w_current != NULL), NULL);

  LeptonToplevel *toplevel = gschem_toplevel_get_toplevel (w_current);

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
         forbear != NULL && found->pid != forbear->pid && forbear->up >= 0;
         forbear = lepton_toplevel_search_page_by_id (toplevel->pages, forbear->up))
      ; /* void */

    if (forbear != NULL && found->pid == forbear->pid) {
      g_set_error (err, EDA_ERROR, EDA_ERROR_LOOP,
                   _("Hierarchy contains a circular dependency."));
      return NULL;  /* error signal */
    }
    lepton_toplevel_goto_page (toplevel, found);
    if (page_control != 0) {
      found->page_control = page_control;
    }
    found->up = parent->pid;
    g_free (string);
    return found;
  }

  found = lepton_page_new (toplevel, string);

  schematic_file_open (w_current,
                       found,
                       lepton_page_get_filename (found),
                       NULL);

  if (page_control == 0) {
    page_control_counter++;
    found->page_control = page_control_counter;
  } else {
    found->page_control = page_control;
  }

  found->up = parent->pid;

  g_free (string);

  return found;
}


/*! \brief Search for the parent page of a page in hierarchy.
 *  \par Function Description
 *  This function searches the parent page of page \a page in the
 *  hierarchy. It checks all the pages in the list \a page_list.
 *
 *  It returns a pointer on the page if found, NULL otherwise.
 *
 *  \note
 *  The page \a current_page must be in the list \a page_list.
 *
 *  \param [in] current_page The reference page for the search.
 *  \returns A pointer on the page found or NULL if not found.
 */
LeptonPage *
s_hierarchy_find_up_page (LeptonPage *current_page)
{
  g_return_val_if_fail (current_page != NULL, NULL);

  LeptonToplevel *toplevel = current_page->toplevel;
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
 *  \param [in]  page
 *  \param [in]  filename
 *  \param [out] error
 *  \return A pointer to the subpage or NULL if an error occured.
 */
LeptonPage*
s_hierarchy_load_subpage (GschemToplevel *w_current,
                          LeptonPage *page,
                          const char *filename,
                          GError **error)
{
  char *string;
  LeptonPage *subpage = NULL;

  g_return_val_if_fail (filename != NULL, NULL);
  g_return_val_if_fail (page != NULL, NULL);

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

    subpage = lepton_toplevel_search_page (page->toplevel, normalized);

    if (subpage == NULL) {
      int success;

      subpage = lepton_page_new (page->toplevel, string);
      success = schematic_file_open (w_current,
                                     subpage,
                                     lepton_page_get_filename (subpage),
                                     error);

      if (success) {
        subpage->page_control = ++page_control_counter;
      } else {
        lepton_page_delete (page->toplevel, subpage);
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
 *  flat list of pages that are below \a p_current. There are two \a
 *  flags that can be used to control the way that the return value is
 *  constructed: <B>HIERARCHY_NODUPS</B> returns a list without
 *  duplicate pages, and <B>HIERARCHY_POSTORDER</B> traverses the
 *  hierarchy tree and returns a postorder list instead of preorder.
 *
 *  \param w_current The GschemToplevel structure.
 *  \param p_current The LeptonPage to traverse hierarchy for.
 *  \param flags Flags controlling form of return value.
 *  \return A GList of LeptonPage pointers.
 *
 *  \warning
 *  Caller must destroy returned GList with g_list_free().
 */
GList *
s_hierarchy_traversepages (GschemToplevel *w_current,
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
  if (!(flags & HIERARCHY_POSTORDER)) {
    /* check whether we already visited this page */
    if ((flags & HIERARCHY_NODUPS)
        && (g_list_find (pages, p_current) != NULL)) {
      return pages;  /* drop the page subtree */
      }
    pages = g_list_append (pages, p_current);
  }

  /* walk throught the page objects and search for underlaying schematics */
  for (iter = lepton_page_objects (p_current);
       iter != NULL ;
       iter = g_list_next (iter)) {
    o_current = (LeptonObject *)iter->data;

    /* only complex things like symbols can contain attributes */
    if (!lepton_object_is_component (o_current)) continue;

    filename =
      o_attrib_search_attached_attribs_by_name (o_current, "source", 0);

    /* if above is NULL, then look inside symbol */
    if (filename == NULL) {
      filename =
        o_attrib_search_inherited_attribs_by_name (o_current, "source", 0);
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

  /* postorder traversing */
  if (flags & HIERARCHY_POSTORDER) {
    /* check whether we already visited this page */
    if ((flags & HIERARCHY_NODUPS)
        && (g_list_find (pages, p_current) != NULL)) {
      return pages;  /* don't append it */
    }
    pages = g_list_append (pages, p_current);
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
         lepton_page_get_filename (p_current), p_current->pid);
  return 0;
}

/*! \brief Search for a page preceding a given page in hierarchy.
 *  \par Function Description
 *  This function searches the previous sibling of page \a page in the
 *  hierarchy. It checks all the pages preceding \a page in the list
 *  \a page_list.
 *
 *  It returns a pointer on the page if found, NULL otherwise.
 *
 *  \note
 *  The page \a current_page must be in the list \a page_list.
 *
 *  \param [in] page_list    The list of pages in which to search.
 *  \param [in] current_page The reference page for the search.
 *  \returns A pointer on the page found or NULL if not found.
  */
LeptonPage *
s_hierarchy_find_prev_page (LeptonPageList *page_list,
                            LeptonPage *current_page)
{
  const GList *iter;

  iter = g_list_find (lepton_list_get_glist (page_list), current_page);
  for (iter = g_list_previous (iter);
       iter != NULL;
       iter = g_list_previous (iter)) {

    LeptonPage *page = (LeptonPage *)iter->data;
    if (page->page_control == current_page->page_control) {
      return page;
    }
  }

  return NULL;
}

/*! \brief Search for a page following a given page in hierarchy.
 *  \par Function Description
 *  This function searches the next sibling of page \a page in the
 *  hierarchy. It checks all the pages following \a page in the list
 *  \a page_list.
 *
 *  It returns a pointer on the page if found, NULL otherwise.
 *
 *  \note
 *  The page \a current_page must be in the list \a page_list.
 *
 *  \param [in] page_list    The list of pages in which to search.
 *  \param [in] current_page The reference page for the search.
 *  \returns A pointer on the page found or NULL if not found.
  */
LeptonPage *
s_hierarchy_find_next_page (LeptonPageList *page_list,
                            LeptonPage *current_page)
{
  const GList *iter;

  iter = g_list_find (lepton_list_get_glist (page_list), current_page);
  for (iter = g_list_next (iter);
       iter != NULL;
       iter = g_list_next (iter)) {

    LeptonPage *page = (LeptonPage *)iter->data;
    if (lepton_page_get_page_control (page) == lepton_page_get_page_control (current_page))
    {
      return page;
    }
  }

  return NULL;
}
