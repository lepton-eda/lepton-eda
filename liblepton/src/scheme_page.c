/* Lepton EDA library - Scheme API
 * Copyright (C) 2010 Peter Brett <peter@peter-b.co.uk>
 * Copyright (C) 2010-2016 gEDA Contributors
 * Copyright (C) 2017-2021 Lepton EDA Contributors
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 */

/*!
 * \file scheme_page.c
 * \brief Scheme API page manipulation procedures.
 */

#include <config.h>

#include "liblepton_priv.h"
#include "libleptonguile_priv.h"

SCM_SYMBOL (edascm_string_format_sym , "string-format");

/*! \brief Get a list of open pages.
 * \par Function Description
 * Retrieves a Scheme list of currently-opened pages.
 *
 * \note Scheme API: Implements the %active-pages procedure of the
 * (lepton core page) module.
 *
 * \return a Scheme list of #LeptonPage smobs.
 */
SCM_DEFINE (active_pages, "%active-pages", 0, 0, 0,
            (), "Retrieve a list of currently-opened pages")
{
  LeptonToplevel *toplevel = edascm_c_current_toplevel ();
  SCM lst = SCM_EOL;
  SCM rlst;
  GList *page_list = lepton_list_get_glist (toplevel->pages);

  while (page_list != NULL) {
    lst = scm_cons (edascm_from_page ((LeptonPage*) page_list->data), lst);
    page_list = g_list_next (page_list);
  }

  rlst = scm_reverse (lst);
  scm_remember_upto_here_1 (lst);
  return rlst;
}

/*! \brief Get a list of objects in a page.
 * \par Function Description
 * Retrieves the contents of a the #LeptonPage smob \a page_s as a Scheme
 * list of #LeptonObject smobs.
 *
 * \note Scheme API: Implements the %page-contents procedure of the
 * (lepton core page) module.
 *
 * \return a list of #LeptonObject smobs.
 */
SCM_DEFINE (page_contents, "%page-contents", 1, 0, 0,
            (SCM page_s), "Get a page's contents.")
{
  LeptonPage *page;

  /* Ensure that the argument is a page smob */
  SCM_ASSERT (EDASCM_PAGEP (page_s), page_s,
              SCM_ARG1, s_page_contents);

  page = edascm_to_page (page_s);

  return edascm_from_object_glist (s_page_objects (page));
}

/*! \brief Remove an object from a page.
 * \par Function Description
 * Removes \a obj_s from \a page_s.  If \a obj_s is attached to a
 * #LeptonPage other than \a page_s, or to a component
 * #LeptonObject, throws a Scheme error. If \a obj_s is not
 * attached to a page, does nothing.
 *
 * \note Scheme API: Implements the %page-remove! procedure of the
 * (lepton core page) module.
 *
 * \return \a page_s.
 */
SCM_DEFINE (page_remove_x, "%page-remove!", 2, 0, 0,
            (SCM page_s, SCM obj_s), "Remove an object from a page.")
{
  /* Ensure that the arguments have the correct types. */
  SCM_ASSERT (EDASCM_PAGEP (page_s), page_s,
              SCM_ARG1, s_page_remove_x);
  SCM_ASSERT (EDASCM_OBJECTP (obj_s), obj_s,
              SCM_ARG2, s_page_remove_x);

  LeptonPage *page = edascm_to_page (page_s);
  LeptonObject *obj = edascm_to_object (obj_s);

  /* Check that the object is not attached to something else. */
  LeptonPage *curr_page = lepton_object_get_page (obj);
  if ((curr_page != NULL && curr_page != page) ||
      (lepton_object_get_parent (obj) != NULL))
  {
    scm_error (edascm_object_state_sym, s_page_remove_x,
               _("Object ~A is attached to a component or different page"),
               scm_list_1 (obj_s), SCM_EOL);
  }

  /* Check that object is not attached as an attribute. */
  if (obj->attached_to != NULL) {
    scm_error (edascm_object_state_sym, s_page_remove_x,
               _("Object ~A is attached as an attribute"),
               scm_list_1 (obj_s), SCM_EOL);
  }

  /* Check that object doesn't have attributes. */
  if (obj->attribs != NULL) {
    scm_error (edascm_object_state_sym, s_page_remove_x,
               _("Object ~A has attributes"),
               scm_list_1 (obj_s), SCM_EOL);
  }

  if (curr_page == NULL) return obj_s;

  lepton_object_emit_pre_change_notify (obj);
  s_page_remove (page, obj);
  page->CHANGED = 1; /* Ugh. */
  /* If the object is currently selected, unselect it. */
  o_selection_remove (page->selection_list, obj);
  lepton_object_emit_change_notify (obj);

  /* Object cleanup now managed by Guile. */
  edascm_c_set_gc (obj_s, 1);
  return page_s;
}

/*! \brief Create a page from a string representation.
 * \par Function Description
 * Returns a page with filename \a filename_s created by parsing \a
 * str_s. Throws an error if \a str_s contains invalid gEDA file
 * format syntax.
 *
 * \note Scheme API: Implements the %string->page procedure of the
 * (lepton core page) module.
 *
 * \param filename_s Filename for new page.
 * \param str_s      String to parse to create page.
 * \return a new page created by parsing \a str_s.
 */
SCM_DEFINE (string_to_page, "%string->page", 2, 0, 0,
            (SCM filename_s, SCM str_s),
            "Create a new page from a string.")
{
  /* Ensure that the arguments are strings */
  SCM_ASSERT (scm_is_string (filename_s), filename_s,
              SCM_ARG1, s_string_to_page);
  SCM_ASSERT (scm_is_string (str_s), str_s,
              SCM_ARG2, s_string_to_page);

  LeptonToplevel *toplevel = edascm_c_current_toplevel ();
  char *filename = scm_to_utf8_string (filename_s);
  LeptonPage *page = s_page_new (toplevel, filename);
  free (filename);

  size_t len;
  GError * err = NULL;
  char *str = scm_to_utf8_stringn (str_s, &len);
  GList *objects = o_read_buffer (page, NULL, str, len,
                                  s_page_get_filename(page), &err);
  free (str);

  if (err) {
      SCM error_message = scm_from_utf8_string (err->message);

      g_error_free(err);
      scm_error (edascm_string_format_sym, s_string_to_page,
                 _("Parse error: ~s"), scm_list_1 (error_message), SCM_EOL);
  }

  s_page_append_list (page, objects);

  return edascm_from_page (page);
}

/*!
 * \brief Create the (lepton core page) Scheme module.
 * \par Function Description
 * Defines procedures in the (lepton core page) module. The module can
 * be accessed using (use-modules (lepton core page)).
 */
static void
init_module_lepton_core_page (void *unused)
{
  /* Register the functions */
  #include "scheme_page.x"

  /* Add them to the module's public definitions. */

  scm_c_export (s_active_pages,
                s_page_contents,
                s_page_remove_x,
                s_string_to_page, NULL);
}

/*!
 * \brief Initialise the basic Lepton EDA page manipulation procedures.
 * \par Function Description
 * Registers some Scheme procedures for working with #LeptonPage
 * smobs. Should only be called by edascm_init().
 */
void
edascm_init_page ()
{
  /* Define the (lepton core page) module */
  scm_c_define_module ("lepton core page",
                       (void (*)(void*)) init_module_lepton_core_page,
                       NULL);
}
