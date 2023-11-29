/* Lepton EDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2016 gEDA Contributors
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
/*!
 * \file gschem_find_text_state.c
 *
 * \brief Stores state of a find text operation
 */

#include <config.h>
#include "gschem.h"
#include <liblepton/glib_compat.h>


enum
{
  COLUMN_FILENAME,
  COLUMN_STRING,
  COLUMN_OBJECT,
  COLUMN_COUNT
};


G_DEFINE_TYPE (GschemFindTextState, gschem_find_text_state, GSCHEM_TYPE_BIN);


typedef void (*NotifyFunc)(void*, void*);


static void
assign_store (GschemFindTextState *state, GSList *objects, gboolean filter_text);

static void
clear_store (GschemFindTextState *state);

static GSList*
find_objects_using_pattern (GSList *pages,
                            const char *text,
                            gboolean include_hidden);

static GSList*
find_objects_using_regex (GSList *pages,
                          const char *text,
                          gboolean include_hidden);

static GSList*
find_objects_using_substring (GSList *pages,
                              const char *text,
                              gboolean include_hidden);

static GSList*
find_objects_using_check (GSList *pages);

static GSList*
get_pages (GschemToplevel *w_current,
           GList *pages,
           gboolean descend);

static void
get_property (GObject *object, guint param_id, GValue *value, GParamSpec *pspec);

static GList*
get_subpages (GschemToplevel *w_current,
              LeptonPage *page);

static void
object_weakref_cb (LeptonObject *object, GschemFindTextState *state);

static void
remove_object (GschemFindTextState *state, LeptonObject *object);

static void
select_cb (GtkTreeSelection *selection, GschemFindTextState *state);

static void
set_property (GObject *object, guint param_id, const GValue *value, GParamSpec *pspec);


/*! \brief find instances of a given string
 *
 *  Finds instances of a given string and displays the result inside this
 *  widget.
 *
 *  \param [in] w_current The GschemToplevel structure.
 *  \param [in] state The GschemFindTextState structure.
 *  \param [in] pages a list of pages to search
 *  \param [in] type the type of find to perform
 *  \param [in] text the text to find
 *  \param [in] descend Descend the page hierarchy
 *  \param [in] include_hidden Include hidden objects.
 *  \return the number of objects found
 */
int
gschem_find_text_state_find (GschemToplevel *w_current,
                             GschemFindTextState *state,
                             GList *pages,
                             int type,
                             const char *text,
                             gboolean descend,
                             gboolean include_hidden)
{
  int count;
  GSList *objects = NULL;
  GSList *all_pages;
  gboolean filter_text = TRUE;

  all_pages = get_pages (w_current, pages, descend);

  switch (type) {
    case FIND_TYPE_SUBSTRING:
      objects = find_objects_using_substring (all_pages, text, include_hidden);
      break;

    case FIND_TYPE_PATTERN:
      objects = find_objects_using_pattern (all_pages, text, include_hidden);
      break;

    case FIND_TYPE_REGEX:
      objects = find_objects_using_regex (all_pages, text, include_hidden);
      break;

    case FIND_TYPE_CHECK:
      filter_text = FALSE;
      objects = find_objects_using_check (all_pages);
      break;

    default:
      break;
  }

  g_slist_free (all_pages);

  assign_store (state, objects, filter_text);
  count = g_slist_length (objects);
  g_slist_free (objects);

  return count;
}


/*! \brief create a new widget for showing the find text state
 *
 *  \return the new find text state widget
 */
GtkWidget*
gschem_find_text_state_new ()
{
  return GTK_WIDGET (g_object_new (GSCHEM_FIND_TEXT_STATE_TYPE,
                                   NULL));
}



/*! \brief places object in the store so the user can see them
 *
 *  \param [in] state
 *  \param [in] objects the list of objects to put in the store
 */
static void
assign_store (GschemFindTextState *state, GSList *objects, gboolean filter_text)
{
  GSList *object_iter;

  g_return_if_fail (state != NULL);
  g_return_if_fail (state->store != NULL);

  clear_store (state);

  object_iter = objects;

  while (object_iter != NULL) {
    char *basename;
    LeptonObject *object = (LeptonObject*) object_iter->data;
    const char *str;
    GtkTreeIter tree_iter;

    object_iter = g_slist_next (object_iter);

    if (object == NULL) {
      g_warning ("NULL object encountered");
      continue;
    }

    if (object->page == NULL) {
      g_warning ("NULL page encountered");
      continue;
    }

    if (filter_text && !lepton_object_is_text (object))
    {
      g_warning ("expecting a text object");
      continue;
    }

    if (filter_text) {
      str = lepton_text_object_get_string (object);
    } else {
      str = scm_to_utf8_string (scm_call_1 (scm_c_public_ref ("schematic symbol check",
                                                              "object-blaming-info"),
                                            scm_from_pointer (object, NULL)));
    }

    if (str == NULL) {
      g_warning ("NULL string encountered");
      continue;
    }

    lepton_object_weak_ref (object, (NotifyFunc) object_weakref_cb, state);

    gtk_list_store_append (state->store, &tree_iter);

    basename = g_path_get_basename (lepton_page_get_filename (object->page));

    gtk_list_store_set (state->store,
                        &tree_iter,
                        COLUMN_FILENAME, basename,
                        COLUMN_STRING, str,
                        COLUMN_OBJECT, object,
                        -1);

    g_free (basename);
  }
}


/*! \brief Dispose of the object
 */
static void
dispose (GObject *object)
{
  GschemFindTextState *state = GSCHEM_FIND_TEXT_STATE (object);

  if (state->store) {
    clear_store (state);
    g_object_unref (state->store);
    state->store = NULL;
  }

  /* lastly, chain up to the parent dispose */
  GschemFindTextStateClass* cls = GSCHEM_FIND_TEXT_STATE_GET_CLASS (object);
  g_return_if_fail (cls != NULL);
  GObjectClass* parent_cls = G_OBJECT_CLASS (g_type_class_peek_parent (cls));
  g_return_if_fail (parent_cls != NULL);
  parent_cls->dispose (object);
}


/*! \brief initialize class
 *
 *  \param [in] klass The class for initialization
 */
static void
gschem_find_text_state_class_init (GschemFindTextStateClass *klass)
{
  G_OBJECT_CLASS (klass)->dispose  = dispose;

  G_OBJECT_CLASS (klass)->get_property = get_property;
  G_OBJECT_CLASS (klass)->set_property = set_property;

  g_signal_new ("select-object",                     /* signal_name  */
                G_OBJECT_CLASS_TYPE (klass),         /* itype        */
                (GSignalFlags) 0,                    /* signal_flags */
                0,                                   /* class_offset */
                NULL,                                /* accumulator  */
                NULL,                                /* accu_data    */
                g_cclosure_marshal_VOID__POINTER,    /* c_marshaller */
                G_TYPE_NONE,                         /* return_type  */
                1,                                   /* n_params     */
                G_TYPE_POINTER
                );
}


/*! \brief delete all items from the list store
 *
 *  This function deletes all items in the list store and removes all the weak
 *  references to the objects.
 *
 *  \param [in] state
 */
static void
clear_store (GschemFindTextState *state)
{
  GtkTreeIter iter;
  gboolean valid;

  g_return_if_fail (state != NULL);
  g_return_if_fail (state->store != NULL);

  valid = gtk_tree_model_get_iter_first (GTK_TREE_MODEL (state->store), &iter);

  while (valid) {
    GValue value = G_VALUE_INIT;

    gtk_tree_model_get_value (GTK_TREE_MODEL (state->store),
                              &iter,
                              COLUMN_OBJECT,
                              &value);

    if (G_VALUE_HOLDS_POINTER (&value)) {
      LeptonObject *object = (LeptonObject*) g_value_get_pointer (&value);

      lepton_object_weak_unref (object, (NotifyFunc) object_weakref_cb, state);
    }

    g_value_unset (&value);

    valid = gtk_tree_model_iter_next (GTK_TREE_MODEL (state->store), &iter);
  }

  gtk_list_store_clear (state->store);
}


/*! \brief Find all text objects that match a pattern
 *
 *  \param pages the list of pages to search
 *  \param text the pattern to match
 *  \param include_hidden Include hidden objects.
 *  \return a list of objects that match the given pattern
 */
static GSList*
find_objects_using_pattern (GSList *pages,
                            const char *text,
                            gboolean include_hidden)
{
  GSList *object_list = NULL;
  GSList *page_iter = pages;
  GPatternSpec *pattern;

  g_return_val_if_fail (text != NULL, NULL);

  pattern = g_pattern_spec_new (text);

  while (page_iter != NULL) {
    const GList *object_iter;
    LeptonPage *page = (LeptonPage*) page_iter->data;

    page_iter = g_slist_next (page_iter);

    if (page == NULL) {
      g_warning ("NULL page encountered");
      continue;
    }

    object_iter = lepton_page_objects (page);

    while (object_iter != NULL) {
      LeptonObject *object = (LeptonObject*) object_iter->data;
      const char *str;

      object_iter = g_list_next (object_iter);

      if (object == NULL) {
        g_warning ("NULL object encountered");
        continue;
      }

      if (!lepton_object_is_text (object))
      {
        continue;
      }

      if (!(lepton_text_object_is_visible (object) || include_hidden)) {
        continue;
      }

      str = lepton_text_object_get_string (object);

      if (str == NULL) {
        g_warning ("NULL string encountered");
        continue;
      }

      if (g_pattern_spec_match_string (pattern, str))
      {
        object_list = g_slist_prepend (object_list, object);
      }
    }
  }

  g_pattern_spec_free (pattern);

  return g_slist_reverse (object_list);
}


/*! \brief Find all text objects that match a regex
 *
 *  \param pages the list of pages to search
 *  \param text the regex to match
 *  \param include_hidden Include hidden objects.
 *  \return a list of objects that match the given regex
 */
static GSList*
find_objects_using_regex (GSList *pages,
                          const char *text,
                          gboolean include_hidden)
{
  GError *ierror = NULL;
  GSList *object_list = NULL;
  GSList *page_iter = pages;
  GRegex *regex;

  g_return_val_if_fail (text != NULL, NULL);

  regex = g_regex_new (text,
                       (GRegexCompileFlags) 0,
                       (GRegexMatchFlags) 0,
                       &ierror);

  if (ierror != NULL) {
    return NULL;
  }

  while (page_iter != NULL) {
    const GList *object_iter;
    LeptonPage *page = (LeptonPage*) page_iter->data;

    page_iter = g_slist_next (page_iter);

    if (page == NULL) {
      g_warning ("NULL page encountered");
      continue;
    }

    object_iter = lepton_page_objects (page);

    while (object_iter != NULL) {
      LeptonObject *object = (LeptonObject*) object_iter->data;
      const char *str;

      object_iter = g_list_next (object_iter);

      if (object == NULL) {
        g_warning ("NULL object encountered");
        continue;
      }

      if (!lepton_object_is_text (object))
      {
        continue;
      }

      if (!(lepton_text_object_is_visible (object) || include_hidden)) {
        continue;
      }

      str = lepton_text_object_get_string (object);

      if (str == NULL) {
        g_warning ("NULL string encountered");
        continue;
      }

      if (g_regex_match (regex, str, (GRegexMatchFlags) 0, NULL)) {
        object_list = g_slist_prepend (object_list, object);
      }
    }
  }

  g_regex_unref (regex);

  return g_slist_reverse (object_list);
}


/*! \brief Find all text objects that contain a substring
 *
 *  \param pages the list of pages to search
 *  \param text the substring to find
 *  \param include_hidden Include hidden objects.
 *  \return a list of objects that contain the given substring
 */
static GSList*
find_objects_using_substring (GSList *pages,
                              const char *text,
                              gboolean include_hidden)
{
  GSList *object_list = NULL;
  GSList *page_iter = pages;

  g_return_val_if_fail (text != NULL, NULL);

  while (page_iter != NULL) {
    const GList *object_iter;
    LeptonPage *page = (LeptonPage*) page_iter->data;

    page_iter = g_slist_next (page_iter);

    if (page == NULL) {
      g_warning ("NULL page encountered");
      continue;
    }

    object_iter = lepton_page_objects (page);

    while (object_iter != NULL) {
      LeptonObject *object = (LeptonObject*) object_iter->data;
      const char *str;

      object_iter = g_list_next (object_iter);

      if (object == NULL) {
        g_warning ("NULL object encountered");
        continue;
      }

      if (!lepton_object_is_text (object))
      {
        continue;
      }

      if (!(lepton_text_object_is_visible (object) || include_hidden)) {
        continue;
      }

      str = lepton_text_object_get_string (object);

      if (str == NULL) {
        g_warning ("NULL string encountered");
        continue;
      }

      if (strstr (str, text) != NULL) {
        object_list = g_slist_prepend (object_list, object);
      }
    }
  }

  return g_slist_reverse (object_list);
}



static GSList*
scm_to_gslist (SCM list_s)
{
  SCM object_s;
  GSList *objects = NULL;

  SCM_ASSERT (scm_is_true (scm_list_p (list_s)), list_s, 1, "scm_to_gslist");

  scm_dynwind_begin ((scm_t_dynwind_flags) 0);
  scm_dynwind_unwind_handler ((void (*)(void *)) g_slist_free,
                              objects,
                              (scm_t_wind_flags) 0);

  while (!scm_is_null (list_s)) {
    object_s = SCM_CAR (list_s);
    objects = g_slist_prepend (objects,
                               (gpointer) scm_to_pointer (object_s));
    list_s = SCM_CDR (list_s);
  }

  scm_remember_upto_here_1 (list_s);

  scm_dynwind_end ();

  return g_slist_reverse (objects);
}



/*! \brief Find all blamed objects after symbol check
 *
 *  \param pages
 *  \param text
 *  \param error
 *  \return a list of objects that are blamed
 */
static GSList*
find_objects_using_check (GSList *pages)
{
  LeptonPage *page = (LeptonPage*) pages->data;
  LeptonToplevel *toplevel = page->toplevel;
  scm_dynwind_begin ((scm_t_dynwind_flags) 0);
  edascm_dynwind_toplevel (toplevel);
  GSList *objects = scm_to_gslist (scm_call_0 (scm_c_public_ref ("schematic symbol check",
                                                                 "check-symbol")));
  scm_dynwind_end ();
  return objects;
}


/*! \brief obtain a list of pages for an operation
 *
 *  Descends the hierarchy of pages, if selected, and removes duplicate pages.
 *
 *  \param [in] pages the list of pages to begin search
 *  \param [in] descend alose locates subpages
 *  \return a list of all the pages
 */
static GSList*
get_pages (GschemToplevel *w_current,
           GList *pages,
           gboolean descend)
{
  GList *input_list = g_list_copy (pages);
  GSList *output_list = NULL;
  GHashTable *visit_list = g_hash_table_new (NULL, NULL);

  while (input_list != NULL) {
    LeptonPage *page = (LeptonPage*) input_list->data;

    input_list = g_list_delete_link (input_list, input_list);

    if (page == NULL) {
      g_warning ("NULL page encountered");
      continue;
    }

    /** \todo the following function becomes available in glib 2.32 */
    /* if (g_hash_table_contains (visit_list, page)) { */

    if (g_hash_table_lookup_extended (visit_list, page, NULL, NULL)) {
      continue;
    }

    output_list = g_slist_prepend (output_list, page);
    g_hash_table_insert (visit_list, page, NULL);

    if (descend) {
      input_list = g_list_concat (input_list, get_subpages (w_current, page));
    }
  }

  g_hash_table_destroy (visit_list);

  return g_slist_reverse (output_list);
}


/*! \brief Get a property
 *
 *  \param [in]     object
 *  \param [in]     param_id
 *  \param [in,out] value
 *  \param [in]     pspec
 */
static void
get_property (GObject *object, guint param_id, GValue *value, GParamSpec *pspec)
{
  /* GschemFindTextState *state = GSCHEM_FIND_TEXT_STATE (object); */

  switch (param_id) {
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, param_id, pspec);
  }
}


/*! \brief get the subpages of a schematic page
 *
 *  if any subpages are not loaded, this function will load them.
 *
 *  \param [in] page the parent page
 *  \return a list of all the subpages
 */
static GList*
get_subpages (GschemToplevel *w_current,
              LeptonPage *page)
{
  const GList *object_iter;
  GList *page_list = NULL;

  g_return_val_if_fail (page != NULL, NULL);

  object_iter = lepton_page_objects (page);

  while (object_iter != NULL) {
    char *attrib;
    char **filenames;
    char **iter;
    LeptonObject *object = (LeptonObject*) object_iter->data;

    object_iter = g_list_next (object_iter);

    if (object == NULL) {
      g_warning ("NULL object encountered");
      continue;
    }

    if (!lepton_object_is_component (object))
    {
      continue;
    }

    attrib = o_attrib_search_attached_attribs_by_name (object,
                                                       "source",
                                                       0);

    if (attrib == NULL) {
      attrib = o_attrib_search_inherited_attribs_by_name (object,
                                                          "source",
                                                          0);
    }

    if (attrib == NULL) {
      continue;
    }

    filenames = g_strsplit (attrib, ",", 0);

    if (filenames == NULL) {
      continue;
    }

    for (iter = filenames; *iter != NULL; iter++) {
      LeptonPage *subpage = s_hierarchy_load_subpage (w_current, page, *iter, NULL);

      if (subpage != NULL) {
        page_list = g_list_prepend (page_list, subpage);
      }
    }

    g_strfreev (filenames);
  }

  return g_list_reverse (page_list);
}


/*! \brief initialize a new instance
 *
 *  \param [in] state the new instance
 */
static void
gschem_find_text_state_init (GschemFindTextState *state)
{
  GtkTreeViewColumn *column;
  GtkCellRenderer *renderer;
  GtkWidget *scrolled;
  GtkTreeSelection *selection;
  GtkWidget *tree_widget;

  g_return_if_fail (state != NULL);

  state->store = gtk_list_store_new(COLUMN_COUNT,
                                    G_TYPE_STRING,
                                    G_TYPE_STRING,
                                    G_TYPE_POINTER);

  scrolled = gtk_scrolled_window_new (NULL, NULL);
  gtk_container_add (GTK_CONTAINER (state), scrolled);

  /* show scrollbars only when needed: */
  gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (scrolled),
                                  GTK_POLICY_AUTOMATIC,
                                  GTK_POLICY_AUTOMATIC);

  tree_widget = gtk_tree_view_new_with_model (GTK_TREE_MODEL (state->store));
  gtk_container_add (GTK_CONTAINER (scrolled), tree_widget);

  /* filename column */

  column = gtk_tree_view_column_new();
  gtk_tree_view_column_set_resizable (column, TRUE);
  gtk_tree_view_column_set_title (column, _("Filename"));

  gtk_tree_view_append_column (GTK_TREE_VIEW (tree_widget), column);

  renderer = gtk_cell_renderer_text_new();
  gtk_tree_view_column_pack_start(column, renderer, TRUE);
  gtk_tree_view_column_add_attribute(column, renderer, "text", 0);

  /* text column */

  column = gtk_tree_view_column_new();
  gtk_tree_view_column_set_resizable (column, TRUE);
  gtk_tree_view_column_set_title (column, _("Text"));

  gtk_tree_view_append_column (GTK_TREE_VIEW (tree_widget), column);

  renderer = gtk_cell_renderer_text_new();
  gtk_tree_view_column_pack_start(column, renderer, TRUE);
  gtk_tree_view_column_add_attribute(column, renderer, "text", 1);

  /* attach signal to detect user selection */

  selection = gtk_tree_view_get_selection (GTK_TREE_VIEW (tree_widget));
  g_signal_connect (selection, "changed", G_CALLBACK (select_cb), state);
}


/*! \brief callback for an object that has been destroyed
 *
 *  \param [in] object the object that has been destroyed
 *  \param [in] state
 */
static void
object_weakref_cb (LeptonObject *object, GschemFindTextState *state)
{
  g_return_if_fail (state != NULL);

  remove_object (state, object);
}


/*! \brief remove an object from the store
 *
 *  This function gets called in response to the object deletion. And, doesn't
 *  dereference the object.
 *
 *  This function doesn't remove the weak reference, under the assumption that
 *  the object is being destroyed.
 *
 *  \param [in] state
 *  \param [in] object the object to remove from the store
 */
static void
remove_object (GschemFindTextState *state, LeptonObject *object)
{
  GtkTreeIter iter;
  gboolean valid;

  g_return_if_fail (object != NULL);
  g_return_if_fail (state != NULL);
  g_return_if_fail (state->store != NULL);

  valid = gtk_tree_model_get_iter_first (GTK_TREE_MODEL (state->store), &iter);

  while (valid) {
    GValue value = G_VALUE_INIT;

    gtk_tree_model_get_value (GTK_TREE_MODEL (state->store),
                              &iter,
                              COLUMN_OBJECT,
                              &value);

    if (G_VALUE_HOLDS_POINTER (&value)) {
      LeptonObject *other = (LeptonObject*) g_value_get_pointer (&value);

      if (object == other) {
        g_value_unset (&value);
        valid = gtk_list_store_remove (state->store, &iter);
        continue;
      }
    }

    g_value_unset (&value);
    valid = gtk_tree_model_iter_next (GTK_TREE_MODEL (state->store), &iter);
  }
}


/*! \brief callback for user selecting an item
 *
 *  \param [in] selection
 *  \param [in] state
 */
static void
select_cb (GtkTreeSelection *selection, GschemFindTextState *state)
{
  GtkTreeIter iter;
  gboolean success;

  g_return_if_fail (selection != NULL);
  g_return_if_fail (state != NULL);

  success = gtk_tree_selection_get_selected (selection, NULL, &iter);

  if (success) {
    GValue value = G_VALUE_INIT;

    gtk_tree_model_get_value (GTK_TREE_MODEL (state->store),
                              &iter,
                              COLUMN_OBJECT,
                              &value);

    if (G_VALUE_HOLDS_POINTER (&value)) {
      LeptonObject *object = (LeptonObject*) g_value_get_pointer (&value);

      if (object != NULL) {
        g_signal_emit_by_name (state, "select-object", object);
      } else {
        g_warning ("NULL object encountered");
      }
    }

    g_value_unset (&value);
  }
}


/*! \brief Set a gobject property
 *
 *  \param [in]     object
 *  \param [in]     param_id
 *  \param [in,out] value
 *  \param [in]     pspec
 */
static void
set_property (GObject *object, guint param_id, const GValue *value, GParamSpec *pspec)
{
  /* GschemFindTextState *state = GSCHEM_FIND_TEXT_STATE (object); */

  switch (param_id) {
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, param_id, pspec);
  }
}
