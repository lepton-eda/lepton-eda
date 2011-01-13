/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2010 gEDA Contributors (see ChangeLog for details)
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

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

/*! \brief */
static int page_control_counter=0;

/*! \todo Finish function documentation!!!
 *  \brief Search for schematic associated source files and load them.
 *  \par Function Description
 *  This function searches the associated source file refered by the
 *  <B>filename</B> and loads it.  If the <B>flag</B> is set to
 *  <B>HIERARCHY_NORMAL_LOAD</B> and the page is allready in the list of
 *  pages it will return the <B>pid</B> of that page.
 *  If the <B>flag</B> is set to <B>HIERARCHY_FORCE_LOAD</B> then this
 *  function will load the page again with a new page id. The second case
 *  is mainly used by gnetlist where pushed down schematics MUST be unique.
 *
 *  \param [in] toplevel     The TOPLEVEL object.
 *  \param [in] filename      Schematic file name.
 *  \param [in] parent        The parent page of the schematic.
 *  \param [in] page_control
 *  \param [in] flag
 *  \return The page loaded, or NULL if failed.
 *
 *  \note
 *  This function goes and finds the associated source files and
 *  loads all up
 *  It only works for schematic files though
 *  this is basically push
 *  flag can either be HIERARCHY_NORMAL_LOAD or HIERARCHY_FORCE_LOAD
 *  flag is mainly used by gnetlist where pushed down schematics MUST be unique
 */
PAGE *
s_hierarchy_down_schematic_single(TOPLEVEL *toplevel, const gchar *filename,
                                  PAGE *parent, int page_control, int flag)
{
  gchar *string;
  PAGE *found = NULL;
  PAGE *forbear;

  g_return_val_if_fail ((toplevel != NULL), NULL);
  g_return_val_if_fail ((filename != NULL), NULL);
  g_return_val_if_fail ((parent != NULL), NULL);

  string = s_slib_search_single(filename);
  if (string == NULL) {
    return NULL;
  }

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

        if (found->pid == forbear->pid) {
          s_log_message(_("hierarchy loop detected while visiting page:\n"
                          "  \"%s\"\n"), found->page_filename);
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

      f_open (toplevel, found, found->page_filename, NULL);
    }
    break;

  case HIERARCHY_FORCE_LOAD:
    {
      found = s_page_new (toplevel, string);
      f_open (toplevel, found, found->page_filename, NULL);
    }
    break;
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
    s_page_goto (toplevel, page);
    g_free (filename);
    return;
  }

  page = s_page_new (toplevel, filename);
  g_free(filename);

  s_page_goto (toplevel, page);

  f_open(toplevel, page, page->page_filename, NULL);

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
  if (current_page->up < 0) {
    s_log_message(_("There are no schematics above the current one!\n"));
    return NULL;
  }

  return s_page_search_by_page_id (page_list, current_page->up);
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
    if (o_current->type != OBJ_COMPLEX) continue;

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
    child_page =
      s_hierarchy_down_schematic_single (toplevel, filename, p_current, 0,
                                         HIERARCHY_NORMAL_LOAD);
    if (child_page != NULL) {
      /* call the recursive function */
      s_hierarchy_traversepages (toplevel, child_page, flags | HIERARCHY_INNERLOOP);
    } else {
      s_log_message (_("ERROR in s_hierarchy_traverse: "
                       "schematic not found: %s\n"),
                     filename);
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
 *  Test function which only prints the name of a page and it's number.
 */
gint
s_hierarchy_print_page (PAGE *p_current, void * data)
{
  printf("pagefilename: %s pageid: %d\n",
         p_current->page_filename, p_current->pid);
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
