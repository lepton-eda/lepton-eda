/* Lepton EDA library
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2016 gEDA Contributors
 * Copyright (C) 2017-2020 Lepton EDA Contributors
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
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#include "libgeda_priv.h"

/*! \brief */
static int page_control_counter=0;

/*!
 *  \brief Search for schematic associated source files and load them.
 *  \par Function Description
 *  This function searches the associated source file refered by the
 *  <B>filename</B> and loads it.  If the <B>flag</B> is set to
 *  <B>HIERARCHY_NORMAL_LOAD</B> and the page is already in the list of
 *  pages it will return the <B>pid</B> of that page.
 *  If the <B>flag</B> is set to <B>HIERARCHY_FORCE_LOAD</B> then this
 *  function will load the page again with a new page id. The second case
 *  is mainly used by gnetlist where pushed down schematics MUST be unique.
 *
 *  \param [in] toplevel      The TOPLEVEL object.
 *  \param [in] filename      Schematic file name.
 *  \param [in] parent        The parent page of the schematic.
 *  \param [in] page_control
 *  \param [in] flag          sets whether to force load
 *  \param [out] err         Location to return a GError on failure.
 *  \return The page loaded, or NULL if failed.
 *
 *  \note
 *  This function finds the associated source files and
 *  loads all up
 *  It only works for schematic files though
 *  this is basically push
 *  flag can either be HIERARCHY_NORMAL_LOAD or HIERARCHY_FORCE_LOAD
 *  flag is mainly used by gnetlist where pushed down schematics MUST be unique
 */
PAGE *
s_hierarchy_down_schematic_single(TOPLEVEL *toplevel, const gchar *filename,
                                  PAGE *parent, int page_control, int flag,
                                  GError **err)
{
  gchar *string;
  PAGE *found = NULL;
  PAGE *forbear;

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

  switch (flag) {
  case HIERARCHY_NORMAL_LOAD:
    {
      gchar *filename = f_normalize_filename (string, NULL);
      found = s_page_search (toplevel, filename);
      g_free (filename);

      if (found) {
        /* check whether this page is in the parents list */
        for (forbear = parent;
             forbear != NULL && found->pid != forbear->pid && forbear->up >= 0;
             forbear = s_page_search_by_page_id (toplevel->pages, forbear->up))
          ; /* void */

        if (forbear != NULL && found->pid == forbear->pid) {
          g_set_error (err, EDA_ERROR, EDA_ERROR_LOOP,
                       _("Hierarchy contains a circular dependency."));
          return NULL;  /* error signal */
        }
        s_page_goto (toplevel, found);
        if (page_control != 0) {
          found->page_control = page_control;
        }
        found->up = parent->pid;
        g_free (string);
        return found;
      }

      found = s_page_new (toplevel, string);

      f_open (toplevel, found, s_page_get_filename (found), NULL);
    }
    break;

  case HIERARCHY_FORCE_LOAD:
    {
      found = s_page_new (toplevel, string);
      f_open (toplevel, found, s_page_get_filename (found), NULL);
    }
    break;

  default:
    g_return_val_if_reached (NULL);
  }

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

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void
s_hierarchy_down_symbol (TOPLEVEL *toplevel, const CLibSymbol *symbol,
                         PAGE *parent)
{
  PAGE *page;
  gchar *filename;

  filename = s_clib_symbol_get_filename (symbol);

  page = s_page_search (toplevel, filename);
  if (page) {
    /* change link to parent page since we
     * can come here from any parent and must
     * come back to the same page */
    page->up = parent->pid;
    s_page_goto (toplevel, page);
    g_free (filename);
    return;
  }

  page = s_page_new (toplevel, filename);
  g_free(filename);

  s_page_goto (toplevel, page);

  f_open(toplevel, page, s_page_get_filename (page), NULL);

  page->up = parent->pid;
  page_control_counter++;
  page->page_control = page_control_counter;

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
 *  \param [in] page_list    The list of pages in which to search.
 *  \param [in] current_page The reference page for the search.
 *  \returns A pointer on the page found or NULL if not found.
 */
PAGE *
s_hierarchy_find_up_page (GedaPageList *page_list, PAGE *current_page)
{
  g_return_val_if_fail (current_page != NULL, NULL);
  if (current_page->up < 0) {
    g_debug(_("There are no schematics above the current one!"));
    return NULL;
  }

  return s_page_search_by_page_id (page_list, current_page->up);
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
PAGE*
s_hierarchy_load_subpage (PAGE *page, const char *filename, GError **error)
{
  char *string;
  PAGE *subpage = NULL;

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

    subpage = s_page_search (page->toplevel, normalized);

    if (subpage == NULL) {
      int success;

      subpage = s_page_new (page->toplevel, string);
      success = f_open (page->toplevel, subpage, s_page_get_filename (subpage), error);

      if (success) {
        subpage->page_control = ++page_control_counter;
      } else {
        s_page_delete (page->toplevel, subpage);
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
 *  \param toplevel The TOPLEVEL structure.
 *  \param p_current The PAGE to traverse hierarchy for.
 *  \param flags Flags controlling form of return value.
 *  \return A GList of PAGE pointers.
 *
 *  \warning
 *  Caller must destroy returned GList with g_list_free().
 */
GList *
s_hierarchy_traversepages (TOPLEVEL *toplevel, PAGE *p_current, gint flags)
{
  OBJECT *o_current;
  PAGE *child_page;
  char *filename = NULL;
  static GList *pages = NULL;
  const GList *iter;

  g_return_val_if_fail ((toplevel != NULL), NULL);
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
  for (iter = s_page_objects (p_current);
       iter != NULL ;
       iter = g_list_next (iter)) {
    o_current = (OBJECT *)iter->data;

    /* only complex things like symbols can contain attributes */
    if (o_current->type != OBJ_COMPONENT) continue;

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
      s_hierarchy_down_schematic_single (toplevel, filename, p_current, 0,
                                         HIERARCHY_NORMAL_LOAD, &err);
    if (child_page != NULL) {
      /* call the recursive function */
      s_hierarchy_traversepages (toplevel, child_page, flags | HIERARCHY_INNERLOOP);
    } else {
      s_log_message (_("Failed to descend hierarchy into '%1$s': %2$s"),
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
s_hierarchy_print_page (PAGE *p_current, void * data)
{
  printf("pagefilename: %s pageid: %d\n",
         s_page_get_filename (p_current), p_current->pid);
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
PAGE *
s_hierarchy_find_prev_page (GedaPageList *page_list, PAGE *current_page)
{
  const GList *iter;

  iter = g_list_find (geda_list_get_glist (page_list), current_page);
  for (iter = g_list_previous (iter);
       iter != NULL;
       iter = g_list_previous (iter)) {

    PAGE *page = (PAGE *)iter->data;
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
PAGE *
s_hierarchy_find_next_page (GedaPageList *page_list, PAGE *current_page)
{
  const GList *iter;

  iter = g_list_find (geda_list_get_glist (page_list), current_page);
  for (iter = g_list_next (iter);
       iter != NULL;
       iter = g_list_next (iter)) {

    PAGE *page = (PAGE *)iter->data;
    if (page->page_control == current_page->page_control) {
      return page;
    }
  }

  return NULL;
}
