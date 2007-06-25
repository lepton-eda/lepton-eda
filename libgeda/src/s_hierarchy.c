/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
 * Copyright (C) 1998-2007 Ales Hvezda
 * Copyright (C) 1998-2007 gEDA Contributors (see ChangeLog for details)
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
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_ASSERT_H
#include <assert.h>
#endif

#include <gtk/gtk.h>
#include <libguile.h>

#include "defines.h"
#include "struct.h"
#include "defines.h"
#include "globals.h"
#include "o_types.h"

#include "../include/prototype.h"

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
 *  \param [in] w_current     The TOPLEVEL object.
 *  \param [in] filename      Schematic file name.
 *  \param [in] parent        The parent page of the schematic.
 *  \param [in] page_control
 *  \param [in] flag
 *  \return The number of pages loaded, -1 otherwise.
 *
 *  \note
 *  This function goes and finds the associated source files and
 *  loads all up
 *  It only works for schematic files though
 *  this is basically push
 *  flag can either be HIERARCHY_NORMAL_LOAD or HIERARCHY_FORCE_LOAD
 *  flag is mainly used by gnetlist where pushed down schematics MUST be unique
 */
int s_hierarchy_down_schematic_single(TOPLEVEL *w_current,
				      const gchar *filename, PAGE *parent,
				      int page_control, int flag) 
{
  gchar *string;
  PAGE *found;
  PAGE *forbear;

  string = s_slib_search_single(filename);
  if (string == NULL) {
    return -1;
  }

  switch (flag) {
    case HIERARCHY_NORMAL_LOAD:
    {
      found = s_page_search (w_current, string);
      
      if (found) {
	/* check whether this page is in the parents list */
	for (forbear = parent; 
	     forbear != NULL && found->pid != forbear->pid && forbear->up >= 0;
	     forbear = s_page_search_pid(w_current, forbear->up))
	  ; /* void */

	if (found->pid == forbear->pid) {
	  s_log_message("hierarchy loop detected while visiting page:\n"
			"  \"%s\"\n",found->page_filename);
	  return -1;  /* error signal */
	}
        s_page_goto (w_current, found);
        if (page_control != 0) {
          found->page_control = page_control;
        }
        found->up = parent->pid;
        g_free (string);
        return found->page_control;
      }
      
      found = s_page_new (w_current, string);
      s_page_goto (w_current, found);
      
      f_open (w_current, found->page_filename);
    }
    break;

    case HIERARCHY_FORCE_LOAD:
    {
      PAGE *page = s_page_new (w_current, string);
      s_page_goto (w_current, page);
      f_open (w_current, page->page_filename);
    }
    break;
  }

  if (page_control == 0) {
    page_control_counter++;
    w_current->page_current->page_control = page_control_counter;
  } else {
    w_current->page_current->page_control = page_control;
  }

  w_current->page_current->up = parent->pid;

  s_page_goto(w_current, w_current->page_current);

  g_free (string);

  return(page_control_counter);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \note
 *  This function goes and finds the associated source files and loads ALL up
 *  only works for schematic files though
 *  this is basically push
 */
void s_hierarchy_down_schematic_multiple (TOPLEVEL *w_current,
					  const gchar *filename, PAGE *parent) 
{
  char *string=NULL;
  PAGE *save_first_page=NULL;
  PAGE *found;
  int loaded_schematics=0;

  s_slib_search (NULL, SLIB_SEARCH_START);

  string = s_slib_search (filename, SLIB_SEARCH_NEXT);
  while (string != NULL) {

    found = s_page_new(w_current, string);

    if (found) {
      w_current->page_current = found;
      s_page_goto(w_current, found);
      if (string) 
        g_free(string);
      return;
    }

    f_open(w_current, w_current->page_current->page_filename);

    if (loaded_schematics == 0) {
      page_control_counter++;
      save_first_page = w_current->page_current;
      /* parent->down = w_current->page_current; not needed */
      w_current->page_current->page_control = 
        page_control_counter;
      loaded_schematics=1;
    } else {
      w_current->page_current->page_control = 
        page_control_counter;
    }

    w_current->page_current->up = parent->pid;
    /* w_current->page_current->down = NULL; not needed */

    if (string) 
      g_free(string);

    string = s_slib_search(filename, SLIB_SEARCH_NEXT);
  }

  s_slib_search(NULL, SLIB_SEARCH_DONE);

  g_free (string);

  if (loaded_schematics) {
    w_current->page_current = save_first_page;
  }

  s_page_goto (w_current, w_current->page_current);
  
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void s_hierarchy_down_symbol (TOPLEVEL *w_current,
			      const CLibSymbol *symbol, PAGE *parent)
{
  PAGE *page;
  gchar *filename;

  filename = s_clib_symbol_get_filename (symbol);

  page = s_page_search (w_current, filename);
  if (page) {
    s_page_goto (w_current, page);
    g_free (filename);
    return;
  }

  page = s_page_new (w_current, filename);
  g_free(filename);

  s_page_goto (w_current, page);

  f_open(w_current, page->page_filename);

  page->up = parent->pid;
  page_control_counter++;
  page->page_control = page_control_counter;

}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void s_hierarchy_up(TOPLEVEL *w_current, int pid)
{
  PAGE *p_current;

  if (pid < 0) {
    s_log_message("There are no schematics above the current one!\n");
    return;
  }

  p_current = s_hierarchy_find_page(w_current->page_head, pid);

  if (p_current) {
    s_page_goto(w_current, p_current);
  } else {
    s_log_message("Cannot find any schematics above the current one!\n");
    s_log_message("Maybe toplevel schematic page was closed/discarded?\n");
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *  This function traverses the hierarchy tree of pages and returns a flat
 *  list of pages that are below the current page. There are two
 *  <B>flags</B>: <B>HIERARCHY_NODUPS</B>: returns a list without
 *  duplicate pages 
 *  <B>HIERARCHY_POSTORDER</B>: traverses the hierarchy tree and
 *  returns a postorder list instead of preorder.
 *
 *  \return A GList of PAGE pointers.
 *
 *  \warning
 *  Call must free returned GList.
 */
GList *s_hierarchy_traversepages(TOPLEVEL *w_current,
				 gint flags)
{
  PAGE *p_current;
  OBJECT *o_current;
  char *filename = NULL;
  gint page_control = 0;
  static GList *pages = NULL;
  
  /* init static variables the first time*/
  if (!(flags & HIERARCHY_INNERLOOP)) {
    pages = NULL;
  }

  p_current = w_current->page_current;

  /* preorder traversing */
  if (!(flags & HIERARCHY_POSTORDER)) {
    /* check whether we already visited this page */
    if ((flags & HIERARCHY_NODUPS)
	&& (g_list_find(pages, p_current) != NULL)) {
      return pages;  /* drop the page subtree */
      }
    pages = g_list_append(pages, p_current);
  }

  /* walk throught the page objects and search for underlaying schematics */
  for (o_current = p_current->object_head;
       o_current != NULL ;
       o_current = o_current->next) {

    /* only complex things like symbols can contain attributes */
    if (o_current->type == OBJ_COMPLEX) {
      filename = o_attrib_search_name_single_count(o_current,
						   "source", 0);
      
      /* if above is NULL, then look inside symbol */
      if (filename == NULL) {
	filename = o_attrib_search_name(o_current->
				      complex->prim_objs, "source", 0);
      }

      if (filename != NULL) {
	/* we got a schematic source attribute 
	   lets load the page and dive into it */
	page_control =s_hierarchy_down_schematic_single(w_current,
							filename,
							p_current,
							0,
							HIERARCHY_NORMAL_LOAD);
	if (page_control != -1) {
	  /* call the recursive function */
	  s_hierarchy_traversepages(w_current,
				    flags | HIERARCHY_INNERLOOP);
	  s_hierarchy_up(w_current, w_current->page_current->up);
	}
	else {
	  s_log_message("ERROR in s_hierarchy_traverse: "
			"schematic not found: %s\n", 
			filename);
	}
	
	g_free(filename);
	filename = NULL;
      }
    }
  }

  /* postorder traversing */
  if (flags & HIERARCHY_POSTORDER) {
    /* check whether we already visited this page */
    if ((flags & HIERARCHY_NODUPS)
	&& (g_list_find(pages, p_current) != NULL)) {
      return pages;  /* don't append it */
    }
    pages = g_list_append(pages, p_current);
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
gint s_hierarchy_print_page(PAGE *p_current, void * data)
{
  printf("pagefilename: %s pageid: %d\n", 
         p_current->page_filename, p_current->pid);
  return 0;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
PAGE *s_hierarchy_find_prev_page (PAGE *p_start, int page_control) 
{
  PAGE *p_current;	

  for (p_current = p_start->prev;
       p_current != NULL;
       p_current = p_current->prev) {
    if (p_current->page_control == page_control) {
      return p_current;
    }
  }

  return NULL;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
PAGE *s_hierarchy_find_next_page (PAGE *p_start, int page_control)
{
  PAGE *p_current;	

  for (p_current = p_start->next;
       p_current != NULL;
       p_current = p_current->next) {
    if (p_current->page_control == page_control) {
      return p_current;
    }
  }

  return NULL;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
PAGE *s_hierarchy_find_page (PAGE *p_start, int pid)
{
  PAGE *p_current = p_start;	

  for (p_current = p_start;
       p_current != NULL;
       p_current = p_current->next) {
    if (p_current->pid == pid) {
      return p_current;
    }
  }

  return NULL;
}
