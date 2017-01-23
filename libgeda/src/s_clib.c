/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
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

/*! \file s_clib.c
 *  \brief The component library system
 *
 *  <B>clib</B> stands for component library.
 *
 *  The <b>component library</b> is made up of a number of
 *  <b>component sources</b>, each of which in turn makes available a
 *  number of component <b>symbols</b>.  Each source may be either a
 *  directory on disk containing symbol files, a command in the system
 *  PATH which can generate gEDA symbol data (e.g. from a database),
 *  or a Scheme function which can do likewise.  A component source is
 *  represented by a #CLibSource instance.
 *
 *  The component library system manages component sources and
 *  symbols, and abstracts the interface to the underlying storage.
 *
 *  To initialise the component library, s_clib_init() is called.  To
 *  clean up when it is no longer needed, s_clib_free() should be
 *  called.
 *
 *  A directory which contains one or more symbol files in gEDA
 *  format may be used as a component source. Each symbol file should
 *  have a filename ending in ".sym" (case insensitive).  A
 *  component source based on a directory can be added using
 *  s_clib_add_directory().  Symbol files with filenames starting with
 *  a period "." are ignored.
 *
 *  An executable program in the system search path may be used as a
 *  component source, and it must conform with the specification given
 *  on page \ref libcmds.  A component source based on a command may
 *  be added using s_clib_add_command().
 *
 *  Scheme functions may be used as a component source; for more
 *  information, please see page \ref libscms.  A component source
 *  based on Scheme functions may be added using s_clib_add_scm().
 *
 *  Each symbol is identified by its \b name, which is stored in the
 *  saved schematic file.  The name must be a valid for storage in a
 *  gEDA schematic file as the "basename" of a "component" object.
 *  For symbols from directory sources, the filename of the symbol is
 *  taken as the symbol name.  For a command source, the name may be
 *  any permissible string.  Guidelines to follow:
 *
 *    -# Do not begin a symbol name with "EMBEDDED"
 *    -# Do not use whitespace, or any of the characters "<tt>/:!*?</tt>".
 *    -# Try to use unique names.
 *
 *  The component database may be queried using s_clib_search().  A
 *  null-terminated buffer containing symbol data (suitable for
 *  loading using o_read_buffer()) may be obtained using
 *  s_clib_symbol_get_data().  If an exact symbol name is known, the
 *  symbol data may be requested directly using
 *  s_clib_symbol_get_data_by_name().
 *
 *
 *  \section libcmds Library Commands
 *
 *  A program or set of programs can be used as a component source.  The procedure used to add such a source from a gEDA rc file is:
 *
 *  <code>
 *  (component-library-command listcmd getcmd name)
 *  </code>
 *
 *  This is implemented by g_rc_component_library_command(), which is
 *  a wrapper for s_clib_add_command().
 *
 *  The list command will be executed with no further arguments, and
 *  should output a list of available symbol names on stdout.  The get
 *  command will have a symbol name appended to it as the final
 *  argument, and should output gEDA symbol data on stdout.
 *
 *  If the command cannot successfully complete, it should exit with
 *  non-zero exit status.  Anything it has output on stdout will be
 *  ignored, and any stderr output displayed to the user.
 *
 *  \section libscms Library Scheme Procedures
 *
 *  A set of Scheme procedures can be used as a component source.  The
 *  procedure used to add such a source from a gEDA rc file is:
 *
 *  <code>
 *  (component-library-funcs listfunc getfunc name)
 *  </code>
 *
 *  This is implemented by g_rc_component_library_funcs(), which is a
 *  wrapper for s_clib_add_scm().
 *
 *  \b listfunc and \b getfunc must both be Guile procedures. \b
 *  listfunc takes no arguments, and returns a list of symbol
 *  names. \b getfunc takes a symbol name as an argument, and returns
 *  gEDA symbol data in a string, or \b \#f if not known.
 */

#include <config.h>

#include <stdio.h>
#include <glib.h>

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#ifdef HAVE_SYS_WAIT_H
#include <sys/wait.h>
#else
#define WIFSIGNALED(x) 0
#define WTERMSIG(x)    0
#define WIFEXITED(x)   1
#define WEXITSTATUS(x) 0
#endif

#include <time.h>

#include "libgeda_priv.h"

/* Constant definitions
 * ===================
 */

/*! All symbols in directory sources end with this string. Must be
 *  lowercase. */
#define SYM_FILENAME_FILTER ".sym"

/*! Library command mode used to fetch list of symbols */
#define CLIB_LIST_CMD       "list"

/*! Library command mode used to fetch symbol data */
#define CLIB_DATA_CMD       "get"

/*! Maximum number of symbol cache entries */
#define CLIB_MAX_SYMBOL_CACHE 128
/*! When symbol cache gets full, remove down to this level */
#define CLIB_MIN_SYMBOL_CACHE 96

/* Type definitions
 * ================
 */

/*! Valid types of component source */
enum CLibSourceType {
  CLIB_NONE = 0,
  /*! Directory source */
  CLIB_DIR,
  /*! Command source */
  CLIB_CMD,
  /*! Guile function source */
  CLIB_SCM,
};

/*! Stores data about a particular component source */
struct _CLibSource {
  /*! Type of source */
  enum CLibSourceType type;
  /*! Name of source */
  gchar *name;
  /*! Available symbols (#CLibSymbol) */
  GList *symbols;

  /*! Path to directory */
  gchar *directory;

  /*! Command & arguments for listing symbols */
  gchar *list_cmd;
  /*! Command & arguments for retrieving symbol data */
  gchar *get_cmd;

  /*! Scheme function for listing symbols */
  SCM list_fn;
  /*! Scheme function for retrieving symbol data */
  SCM get_fn;
};

/*! Stores data about a particular symbol */
struct _CLibSymbol {
  /*! The source this symbol is available from */
  CLibSource *source;
  /*! The name of this symbol */
  gchar *name;
};

/*! Symbol data cache entry */
typedef struct _CacheEntry CacheEntry;
struct _CacheEntry {
  /*! Pointer to symbol */
  CLibSymbol *ptr;
  /*! Symbol data */
  gchar *data;
  /*! Last access */
  time_t accessed;
};

/* Static variables
 * ================
 */

/*! Holds the list of all known component sources */
static GList *clib_sources = NULL;

/*! Caches results of s_clib_search().  The key of the hashtable is a
 *  string describing the search that was carried out, and the value
 *  is a list of symbol pointers. */
static GHashTable *clib_search_cache = NULL;

/*! Caches symbol data.  The key of the hashtable is a symbol pointer,
 *  and the value is a #CacheEntry structure containing the data and
 *  the time it was last used. */
static GHashTable *clib_symbol_cache = NULL;

/* Local static functions
 * ======================
 */
static void free_symbol (gpointer data, gpointer user_data);
static void free_symbol_cache_entry (gpointer data);
static void free_source (gpointer data, gpointer user_data);
static gint compare_source_name (gconstpointer a, gconstpointer b);
static gint compare_symbol_name (gconstpointer a, gconstpointer b);
static void cache_find_oldest (gpointer key, gpointer value, gpointer user_data);
static gchar *run_source_command (const gchar *command);
static CLibSymbol *source_has_symbol (const CLibSource *source,
				      const gchar *name);
static gchar *uniquify_source_name (const gchar *name);
static void refresh_directory (CLibSource *source);
static void refresh_command (CLibSource *source);
static void refresh_scm (CLibSource *source);
static gchar *get_data_directory (const CLibSymbol *symbol);
static gchar *get_data_command (const CLibSymbol *symbol);
static gchar *get_data_scm (const CLibSymbol *symbol);

/*! \brief Initialise the component library.
 *  \par Function Description
 *  Resets and initialises the component library.
 *
 *  \warning This function must be called before any other functions
 *  from s_clib.c.
 */
void s_clib_init ()
{
  if (clib_sources != NULL) {
    s_clib_free ();
  }

  if (clib_search_cache != NULL) {
    s_clib_flush_search_cache();
  } else {
    clib_search_cache = g_hash_table_new_full ((GHashFunc) g_str_hash,
                                               (GEqualFunc)g_str_equal,
                                               (GDestroyNotify) g_free,
                                               (GDestroyNotify) g_list_free);
  }

  if (clib_symbol_cache != NULL) {
    s_clib_flush_symbol_cache();
  } else {
    clib_symbol_cache =
      g_hash_table_new_full ((GHashFunc) g_direct_hash,
                             (GEqualFunc) g_direct_equal,
                             NULL,
                             (GDestroyNotify) free_symbol_cache_entry);
  }
}

/*! \brief Iterator callback for freeing a symbol.
 *  \par Function Description
 *  Private function used only in s_clib.c.
 */
static void free_symbol (gpointer data, gpointer user_data)
{
  CLibSymbol *symbol = data;
  if (symbol != NULL) {
    if (symbol->source != NULL) {
      symbol->source = NULL;
    }
    if (symbol->name != NULL) {
      g_free (symbol->name);
      symbol->name = NULL;
    }
    g_free(symbol);
  }
}

/*! \brief Iterator callback for freeing a symbol cache entry.
 *  \par Function Description
 *  Private function used only in s_clib.c.
 */
static void free_symbol_cache_entry (gpointer data)
{
  CacheEntry *entry = data;
  g_return_if_fail (entry != NULL);
  g_free (entry->data);
  g_free (entry);
}

/*! \brief Iterator callback for freeing a source.
 *  \par Function Description
 *  Private function used only in s_clib.c.
 */
static void free_source (gpointer data, gpointer user_data)
{
  CLibSource *source = data;
  if (source != NULL) {
    if (source->name != NULL) {
      g_free (source->name);
      source->name = NULL;
    }
    if (source->symbols != NULL) {
      g_list_foreach (source->symbols, (GFunc) free_symbol, NULL);
      g_list_free (source->symbols);
      source->symbols = NULL;
    }
    if (source->directory != NULL) {
      g_free (source->directory);
      source->directory = NULL;
    }
    if (source->list_cmd != NULL) {
      g_free (source->list_cmd);
      source->list_cmd = NULL;
    }
    if (source->get_cmd != NULL) {
      g_free (source->get_cmd);
      source->get_cmd = NULL;
    }
    if (source->type == CLIB_SCM) {
      scm_gc_unprotect_object (source->list_fn);
      scm_gc_unprotect_object (source->get_fn);
    }
  }
}

/*! \brief Free all memory used by the component library.
 *  \par Function Description
 *  Should be called at program exit to clean up any remaining data
 *  being used by the component library system.
 */
void s_clib_free ()
{
  if (clib_sources != NULL) {
    g_list_foreach (clib_sources, (GFunc) free_source, NULL);
    g_list_free (clib_sources);
    clib_sources = NULL;
  }
}

/*! \brief Compare two component sources by name.
 *  \par Function Description
 *  Compare two component sources by name, case-insensitively.
 *  Typically used when calling g_list_sort().  Private function used
 *  only in s_clib.c.  Argument order is as strcasecmp().
 *
 *  \param a First source to compare
 *  \param b Second source to compare
 *
 *  \return As strcasecmp().
 */
static gint compare_source_name (gconstpointer a, gconstpointer b)
{
  const CLibSource *src1 = a;
  const CLibSource *src2 = b;

  g_return_val_if_fail ((src1 != NULL), 0);
  g_return_val_if_fail ((src2 != NULL), 0);

  g_return_val_if_fail ((src1->name != NULL), 0);
  g_return_val_if_fail ((src2->name != NULL), 0);

  return strcasecmp(src1->name, src2->name);
}

/*! \brief Compare two component symbols by name.
 *  \par Function Description
 *  Compare two component symbols by name, case-insensitively.
 *  Typically used when calling g_list_sort().  Private function used
 *  only in s_clib.c.  Argument order is as strcasecmp().
 *
 *  \param a First symbol to compare
 *  \param b Second symbol to compare
 *
 *  \return As strcasecmp().
 */
static gint compare_symbol_name (gconstpointer a, gconstpointer b)
{
  const CLibSymbol *sym1 = a;
  const CLibSymbol *sym2 = b;

  g_return_val_if_fail ((sym1 != NULL), 0);
  g_return_val_if_fail ((sym2 != NULL), 0);

  g_return_val_if_fail ((sym1->name != NULL), 0);
  g_return_val_if_fail ((sym2->name != NULL), 0);

  return strcasecmp(sym1->name, sym2->name);
}

/*! \brief Iterator callback for finding oldest symbol cache entry
 *  \par Function Description
 *  Private function used only in s_clib.c.
 */
static void cache_find_oldest (gpointer key,
                               gpointer value,
                               gpointer user_data)
{
  CacheEntry *current = value;
  CacheEntry **oldest = user_data;

  if (current->accessed < (*oldest)->accessed) {
    *oldest = current;
  }
}

/*! \brief Execute a library command.
 *  \par Function Description
 *  Execute a library command, returning the standard output, or \b
 *  NULL if the command fails for some reason.  The system \b PATH is
 *  used to find the program to execute.
 *  The command can write messages to the standard error output. They
 *  are forwarded to the libgeda logging mechanism.
 *
 *  Private function used only in s_clib.c.
 *
 *  \todo This is probably generally useful.
 *
 *  \param command  Command string to execute.
 *  \return The program's output, or \b NULL on failure.
 */
static gchar *run_source_command (const gchar *command)
{
  gchar *standard_output = NULL;
  gchar *standard_error = NULL;
  gint exit_status;
  GError *e = NULL;
  gboolean success = FALSE;

  g_return_val_if_fail((command != NULL), NULL);

  g_spawn_command_line_sync (command,
                             &standard_output,
                             &standard_error,
                             &exit_status,
                             &e);

  if (e != NULL) {
    s_log_message (_("Library command failed [%s]: %s\n"), command,
		   e->message);
    g_error_free (e);

  } else if (WIFSIGNALED(exit_status)) {
    s_log_message (_("Library command failed [%s]: Uncaught signal %i.\n"),
                   command, WTERMSIG(exit_status));

  } else if (WIFEXITED(exit_status) && WEXITSTATUS(exit_status)) {
    s_log_message (_("Library command failed [%s]\n"), command);
    s_log_message(_("Error output was:\n%s\n"), standard_error);

  } else {
    success = TRUE;
  }

  /* forward library command messages */
  if (success && standard_error != NULL)
    s_log_message ("%s", standard_error);

  g_free (standard_error);

  if (success) return standard_output;

  g_free (standard_output);
  return NULL;
}

/*! \brief Get a list of available component sources.
 *  \par Function Description
 *  Gets the current list of sources.
 *  \warning The GList returned should be freed when no longer
 *  needed. The returned value is not guaranteed to remain valid over
 *  calls to s_clib_add_directory() or s_clib_add_command().
 *  \return A \b GList of CLibSource.
 */
GList *s_clib_get_sources (const gboolean sorted)
{
  GList *l = g_list_copy(clib_sources);
  if (sorted) {
    l = g_list_sort (l, (GCompareFunc) compare_source_name);
  }
  return l;
}

/*! \brief Find any symbols within a source with a given name.
 *  \par Function Description
 *  Iterates through the symbol list of the given source, checking if
 *  there is already a symbol with the given name.  If there is
 *  such a symbol, it is returned.
 *
 *  \param source The source to check.
 *  \param name The symbol name to look for.
 *  \return The matching symbol, or \b NULL if no match was found.
 */
static CLibSymbol *source_has_symbol (const CLibSource *source,
				      const gchar *name)
{
  GList *symlist;
  CLibSymbol *symbol;

  for (symlist = g_list_first(source->symbols);
       symlist != NULL;
       symlist = g_list_next(symlist)) {

    symbol = (CLibSymbol *) symlist->data;

    if (strcmp (symbol->name, name) == 0) return symbol;
  }

  return NULL;
}

/*! \brief Make sure a source name is unique.
 *  \par Function Description
 *  Checks if a source already exists with the given \a name.  If one
 *  does, appends a number to the source name.  If \a name is not
 *  already in use, returns it as is.  The return value is always a
 *  newly-allocated string, and should be freed.
 *  it.
 */
static gchar *uniquify_source_name (const gchar *name)
{
  gchar *newname = NULL;
  gint i = 0;

  if (s_clib_get_source_by_name (name) == NULL) {
    return g_strdup (name);
  }

  do {
    g_free (newname);
    i++;
    newname = g_strdup_printf ("%s<%i>", name, i);
  } while (s_clib_get_source_by_name (newname) != NULL);

  s_log_message (_("Library name [%s] already in use.  Using [%s].\n"),
                 name, newname);

  return newname;
}

/*! \brief Rescan a directory for symbols.
 *  \par Function Description
 *  Rescans a directory for symbols.
 *
 *  \todo Does this need to do something more sane with subdirectories
 *  than just skipping them silently?
 *
 *  Private function used only in s_clib.c.
 */
static void refresh_directory (CLibSource *source)
{
  CLibSymbol *symbol;
  GDir *dir;
  const gchar *entry;
  gchar *low_entry;
  gchar *fullpath;
  gboolean isfile;
  GError *e = NULL;

  g_return_if_fail (source != NULL);
  g_return_if_fail (source->type == CLIB_DIR);

  /* Clear the current symbol list */
  g_list_foreach (source->symbols, (GFunc) free_symbol, NULL);
  g_list_free (source->symbols);
  source->symbols = NULL;

  /* Open the directory for reading. */
  dir = g_dir_open (source->directory, 0, &e);

  if (e != NULL) {
    s_log_message (_("Failed to open directory [%s]: %s\n"),
		   source->directory, e->message);
    g_error_free (e);
    return;
  }

  while ((entry = g_dir_read_name (dir)) != NULL) {
    /* skip ".", ".." & hidden files */
    if (entry[0] == '.') continue;

    /* skip subdirectories (for now) */
    fullpath = g_build_filename (source->directory, entry, NULL);
    isfile = g_file_test (fullpath, G_FILE_TEST_IS_REGULAR);
    g_free (fullpath);
    if (!isfile) continue;

    /* skip filenames that we already know about. */
    if (source_has_symbol (source, entry) != NULL) continue;

    /* skip filenames which don't have the right suffix. */
    low_entry = g_utf8_strdown (entry, -1);
    if (!g_str_has_suffix (low_entry, SYM_FILENAME_FILTER)) {
      g_free (low_entry);
      continue;
    }
    g_free (low_entry);

    /* Create and add new symbol record */
    symbol = g_new0 (CLibSymbol, 1);
    symbol->source = source;
    symbol->name = g_strdup(entry);

    /* Prepend because it's faster and it doesn't matter what order we
     * add them. */
    source->symbols = g_list_prepend (source->symbols, symbol);
  }

  entry = NULL;
  g_dir_close (dir);

  /* Now sort the list of symbols by name. */
  source->symbols = g_list_sort (source->symbols,
				 (GCompareFunc) compare_symbol_name);

  s_clib_flush_search_cache();
  s_clib_flush_symbol_cache();
}

/*! \brief Re-poll a library command for symbols.
 *  \par Function Description
 *  Runs a library command, requesting a list of available symbols,
 *  and updates the source with the new list.
 *
 *  Private function used only in s_clib.c.
 */
static void refresh_command (CLibSource *source)
{
  gchar *cmdout;
  TextBuffer *tb;
  const gchar *line;
  CLibSymbol *symbol;
  gchar *name;

  g_return_if_fail (source != NULL);
  g_return_if_fail (source->type == CLIB_CMD);

  /* Clear the current symbol list */
  g_list_foreach (source->symbols, (GFunc) free_symbol, NULL);
  g_list_free (source->symbols);
  source->symbols = NULL;

  /* Run the command to get the list of symbols */
  cmdout = run_source_command (source->list_cmd);
  if (cmdout == NULL) return;

  /* Use a TextBuffer to help reading out the lines of the output */
  tb = s_textbuffer_new (cmdout, -1);

  while (1) {
    line = s_textbuffer_next_line (tb);
    if (line == NULL) break;
    if (line[0] == '.') continue;  /* TODO is this sane? */

    name = geda_string_get_first_line (g_strdup(line));

    /* skip symbols already known about */
    if (source_has_symbol (source, name) != NULL) {
      g_free (name);
      continue;
    }

    symbol = g_new0 (CLibSymbol, 1);
    symbol->source = source;
    symbol->name = name;

    /* Prepend because it's faster and it doesn't matter what order we
     * add them. */
    source->symbols = g_list_prepend (source->symbols, symbol);
  }

  s_textbuffer_free (tb);
  g_free (cmdout);

  /* Sort all symbols by name. */
  source->symbols = g_list_sort (source->symbols,
				 (GCompareFunc) compare_symbol_name);

  s_clib_flush_search_cache();
  s_clib_flush_symbol_cache();
}

/*! \brief Re-poll a scheme procedure for symbols.
 *  \par Function Description
 *  Calls a Scheme procedure to obtain a list of available symbols,
 *  and updates the source with the new list
 *
 *  Private function used only in s_clib.c.
 */
static void refresh_scm (CLibSource *source)
{
  SCM symlist;
  SCM symname;
  CLibSymbol *symbol;
  char *tmp;

  g_return_if_fail (source != NULL);
  g_return_if_fail (source->type == CLIB_SCM);

  /* Clear the current symbol list */
  g_list_foreach (source->symbols, (GFunc) free_symbol, NULL);
  g_list_free (source->symbols);
  source->symbols = NULL;

  symlist = scm_call_0 (source->list_fn);

  if (scm_is_false (scm_list_p (symlist))) {
    s_log_message (_("Failed to scan library [%s]: Scheme function returned non-list\n"),
		   source->name);
    return;
  }

  while (!scm_is_null (symlist)) {
    symname = SCM_CAR (symlist);
    if (!scm_is_string (symname)) {
      s_log_message (_("Non-string symbol name while scanning library [%s]\n"),
		     source->name);
    } else {
      symbol = g_new0 (CLibSymbol, 1);
      symbol->source = source;

      /* Need to make sure that the correct free() function is called
       * on strings allocated by Guile. */
      tmp = scm_to_utf8_string (symname);
      symbol->name = g_strdup(tmp);
      free (tmp);

      /* Prepend because it's faster and it doesn't matter what order we
       * add them. */
      source->symbols = g_list_prepend (source->symbols, symbol);
    }

    symlist = SCM_CDR (symlist);
  }

  /* Now sort the list of symbols by name. */
  source->symbols = g_list_sort (source->symbols,
				 (GCompareFunc) compare_symbol_name);

  s_clib_flush_search_cache();
  s_clib_flush_symbol_cache();
}

/*! \brief Rescan all available component libraries.
 *  \par Function Description
 *  Resets the list of symbols available from each source, and
 *  repopulates it from scratch.  Useful e.g. for checking for new
 *  symbols.
 */
void s_clib_refresh ()
{
  GList *sourcelist;
  CLibSource *source;

  for (sourcelist = clib_sources;
       sourcelist != NULL;
       sourcelist = g_list_next(sourcelist)) {

    source = (CLibSource *) sourcelist->data;
    switch (source->type)
      {
      case CLIB_DIR:
	refresh_directory(source);
	break;
      case CLIB_CMD:
	refresh_command (source);
	break;
      case CLIB_SCM:
	refresh_scm (source);
	break;
      default:
	g_critical("s_clib_refresh: source %p has bad source type %i\n",
                   source, (gint) source->type);
        break;
      }
  }
}

/*! \brief Get a named component source.
 *  \par Function Description
 *  Iterates through the known component sources, checking if there is
 *  a source with the given \a name.
 *
 *  \param name The source name to look for.
 *
 *  \return The matching source, or \b NULL if no match was found.
 */
const CLibSource *s_clib_get_source_by_name (const gchar *name)
{
  GList *sourcelist;
  CLibSource *source;

  for (sourcelist = clib_sources;
       sourcelist != NULL;
       sourcelist = g_list_next(sourcelist)) {

    source = (CLibSource *) sourcelist->data;
    if (strcmp (source->name, name) == 0) {
      return source;
    }
  }

  return NULL;
}

/*! \brief Add a directory of symbol files to the library
 *  \par Function Description
 *  Adds a directory containing symbol files to the library.  Only
 *  files ending with #SYM_FILENAME_FILTER are considered to be symbol
 *  files.  A \a name may be specified for the source; if \a name is
 *  \b NULL, the basename of the directory as returned by
 *  g_path_get_basename() is used.
 *
 *  \param directory The path of the directory to add.
 *  \param name      A descriptive name for the directory.
 *  \return The #CLibSource associated with the directory.
 */
const CLibSource *s_clib_add_directory (const gchar *directory,
					const gchar *name)
{
  CLibSource *source;
  gchar *intname, *realname;

  if (directory == NULL) {
    return NULL;
  }

  if (name == NULL) {
    intname = g_path_get_basename (directory);
    realname = uniquify_source_name (intname);
    g_free (intname);
  } else {
    realname = uniquify_source_name (name);
  }

  source = g_new0 (CLibSource, 1);
  source->type = CLIB_DIR;
  source->directory = g_strdup (directory);
  source->name = realname;

  refresh_directory (source);

  /* Sources added later get scanned earlier */
  clib_sources = g_list_prepend (clib_sources, source);

  return source;
}

/*! \brief Add symbol-generating commands to the library
 *  \par Function Description
 *  Adds a set of commands which can generate symbols to the
 *  library. \a list_cmd and \a get_cmd should be strings consisting
 *  of an executable name followed by any arguments required.
 *  Executables are resolved using the current PATH.  See page \ref
 *  libcmds for more information on library commands.
 *
 *  \param list_cmd The executable & arguments used to list available
 *                   symbols.
 *  \param get_cmd  The executable & arguments used to retrieve symbol
 *                   data.
 *  \param name      A descriptive name for the component source.
 *  \return The CLibSource associated with the component source.
 */
const CLibSource *s_clib_add_command (const gchar *list_cmd,
                                      const gchar *get_cmd,
				      const gchar *name)
{
  CLibSource *source;
  gchar *realname;

  if (name == NULL) {
    s_log_message (_("Cannot add library: name not specified\n"));
    return NULL;
  }

  realname = uniquify_source_name (name);

  if (list_cmd == NULL || get_cmd == NULL) {
    s_log_message (_("Cannot add library [%s]: both 'list' and "
                     "'get' commands must be specified.\n"),
		   realname);
  }

  source = g_new0 (CLibSource, 1);
  source->type = CLIB_CMD;
  source->name = realname;

  source->list_cmd = g_strdup (list_cmd);
  source->get_cmd = g_strdup (get_cmd);

  refresh_command (source);

  /* Sources added later get sacnned earlier */
  clib_sources = g_list_prepend (clib_sources, source);

  return source;
}

/*! \brief Add symbol-generating Scheme procedures to the library.
 *  \par Function Description
 *  Adds a source to the library based on Scheme procedures.  See page
 *  \ref libscms for more information. Two procedures are required: \a
 *  listfunc must return a Scheme list of symbol names, and \a getfunc
 *  must return a string containing symbol data when passed a symbol
 *  name.
 *
 *  \param listfunc A Scheme function returning a list of symbols.
 *  \param getfunc  A Scheme function returning symbol data.
 *  \param name     A descriptive name for the component source.
 *
 *  \return         The new CLibSource.
 */
const CLibSource *s_clib_add_scm (SCM listfunc, SCM getfunc, const gchar *name)
{
  CLibSource *source;
  gchar *realname;

  if (name == NULL) {
    s_log_message (_("Cannot add library: name not specified\n"));
    return NULL;
  }

  realname = uniquify_source_name (name);

  if (scm_is_false (scm_procedure_p (listfunc))
      && scm_is_false (scm_procedure_p (getfunc))) {
    s_log_message (_("Cannot add Scheme-library [%s]: callbacks must be closures\n"),
		   realname);
    return NULL;
  }

  source = g_new0 (CLibSource, 1);
  source->type = CLIB_SCM;
  source->name = realname;
  source->list_fn = scm_gc_protect_object (listfunc);
  source->get_fn = scm_gc_protect_object (getfunc);

  refresh_scm (source);

  clib_sources = g_list_prepend (clib_sources, source);

  return source;
}

/*! \brief Get the name of a source.
 *  \par Function Description
 *  Get the name of a source for use e.g. in displaying a GUI.
 *
 *  \param source Source to be examined.
 *  \return Name of source.
*/
const gchar *s_clib_source_get_name (const CLibSource *source)
{
  if (source == NULL) return NULL;
  return source->name;
}

/*! \brief Get a list of symbols available from a given source.
 *  \par Function Description
 *  Get a \b GList containing all of the symbols available from \a
 *  source.
 *
 *  \warning The returned \b GList will not be consistent over a call to
 *  s_clib_refresh().  It should be freed when no longer needed.
 *
 *  \param source Source to be examined.
 *  \return A \b GList of #CLibSymbol.
 */
GList *s_clib_source_get_symbols (const CLibSource *source)
{
  if (source == NULL) return NULL;
  return g_list_copy(source->symbols);
}


/*! \brief Get the name of a symbol.
 *  \par Function Description
 *  Get the name of a symbol.  The symbol name uniquely identifies it
 *  to libgeda.
 *
 *  \param symbol Symbol to be examined.
 *  \return Name of symbol.
*/
const gchar *s_clib_symbol_get_name (const CLibSymbol *symbol)
{
  if (symbol == NULL) return NULL;
  return symbol->name;
}

/*! \brief Get a filename for editing a symbol.
 *  \par Function Description
 *  Get the filename of the file a symbol was loaded from, if possible
 *  (e.g. to allow loading for user editing).
 *
 *  \warning The returned string should be freed when no longer
 *  needed.
 *
 *  \todo This is hack until there is a way to edit documents in
 *  gschem which do not have a file in the filesystem associated with
 *  them.
 *
 *  \deprecated This function is a temporary workaround.
 *
 *  \param symbol Symbol to be examined.
 *  \return Filename of symbol.
 */
gchar *s_clib_symbol_get_filename (const CLibSymbol *symbol)
{
  if (symbol == NULL) return NULL;

  if (symbol->source->type != CLIB_DIR) return NULL;

  return g_build_filename(symbol->source->directory, symbol->name, NULL);
}

/*! \brief Get the source to which a symbol belongs.
 *  \par Function Description
 *  Get the source which a symbol is associated.
 *
 *  \param symbol Symbol to be examined.
 *  \return Source which owns symbol.
*/
const CLibSource *s_clib_symbol_get_source (const CLibSymbol *symbol)
{
  if (symbol == NULL) return NULL;
  return symbol->source;
}

/*! \brief Get symbol data from a directory source.
 *  \par Function Description
 *  Get symbol data from a directory data source.  The return value
 *  should be free()'d when no longer needed.
 *
 *  Private function used only in s_clib.c.
 *
 *  \param symbol Symbol to get data for.
 *  \return Allocated buffer containing symbol data.
 */
static gchar *get_data_directory (const CLibSymbol *symbol)
{
  gchar *filename = NULL;
  gchar *data = NULL;
  GError *e = NULL;

  g_return_val_if_fail ((symbol != NULL), NULL);
  g_return_val_if_fail ((symbol->source->type == CLIB_DIR), NULL);

  filename = g_build_filename(symbol->source->directory,
			      symbol->name, NULL);

  g_file_get_contents (filename, &data, NULL, &e);

  if (e != NULL) {
    s_log_message (_("Failed to load symbol from file [%s]: %s\n"),
		   filename, e->message);
    g_error_free (e);
  }

  g_free (filename);
  return data;
}

/*! \brief Get symbol data from a library command.
 *  \par Function Description
 *  Get symbol data from a library command.  The return value should
 *  be free()'d when no longer needed.
 *
 *  Private function used only in s_clib.c.
 *
 *  \param symbol Symbol to get data for.
 *  \return Allocated buffer containing symbol data.
 */
static gchar *get_data_command (const CLibSymbol *symbol)
{
  gchar *command;
  gchar *result;

  g_return_val_if_fail ((symbol != NULL), NULL);
  g_return_val_if_fail ((symbol->source->type == CLIB_CMD), NULL);

  command = g_strdup_printf ("%s %s", symbol->source->get_cmd,
                          symbol->name);

  result = run_source_command ( command );

  g_free (command);

  return result;
}

/*! \brief Get symbol data from a Scheme-based component source.
 *  \par Function Description
 *  Get symbol data from a Scheme-based component source.  The return
 *  value should be free()'d when no longer needed.
 *
 *  Private function used only in s_clib.c.
 *
 *  \param symbol Symbol to get data for.
 *  \return Allocated buffer containing symbol data.
 */
static gchar *get_data_scm (const CLibSymbol *symbol)
{
  SCM symdata;
  char *tmp;
  gchar *result;

  g_return_val_if_fail ((symbol != NULL), NULL);
  g_return_val_if_fail ((symbol->source->type == CLIB_SCM), NULL);

  symdata = scm_call_1 (symbol->source->get_fn,
			scm_from_utf8_string (symbol->name));

  if (!scm_is_string (symdata)) {
    s_log_message (_("Failed to load symbol data [%s] from source [%s]\n"),
		   symbol->name, symbol->source->name);
    return NULL;
  }

  /* Need to make sure that the correct free() function is called
   * on strings allocated by Guile. */
  tmp = scm_to_utf8_string (symdata);
  result = g_strdup(tmp);
  free (tmp);

  return result;
}

/*! \brief Get symbol data.
 *  \par Function Description
 *  Get the unparsed gEDA-format data corresponding to a symbol from
 *  the symbol's data source.  The return value should be free()'d
 *  when no longer needed.
 *
 *  On failure, returns \b NULL (the error will be logged).
 *
 *  \param symbol Symbol to get data for.
 *  \return Allocated buffer containing symbol data.
 */
gchar *s_clib_symbol_get_data (const CLibSymbol *symbol)
{
  CacheEntry *cached;
  gchar *data;
  gpointer symptr;
  gint n;

  g_return_val_if_fail ((symbol != NULL), NULL);
  g_return_val_if_fail ((symbol->source != NULL), NULL);

  /* Trickery to bypass effects of const */
  symptr = (gpointer) symbol;

  /* First, try the cache. */
  cached = g_hash_table_lookup (clib_symbol_cache, symptr);
  if (cached != NULL) {
    cached->accessed = time(NULL);
    return g_strdup(cached->data);
  }

  /* If the symbol wasn't found in the cache, get it directly. */
  switch (symbol->source->type)
    {
    case CLIB_DIR:
      data = get_data_directory (symbol);
      break;
    case CLIB_CMD:
      data = get_data_command (symbol);
      break;
    case CLIB_SCM:
      data = get_data_scm (symbol);
      break;
    default:
      g_critical("s_clib_symbol_get_data: source %p has bad source type %i\n",
                 symbol->source, (gint) symbol->source->type);
      return NULL;
    }

  if (data == NULL) return NULL;

  /* Cache the symbol data */
  cached = g_new (CacheEntry, 1);
  cached->ptr = (CLibSymbol *) symptr;
  cached->data = g_strdup (data);
  cached->accessed = time (NULL);
  g_hash_table_insert (clib_symbol_cache, symptr, cached);

  /* Clean out the cache if it's too full */
  n = g_hash_table_size (clib_symbol_cache);
  if (n > CLIB_MAX_SYMBOL_CACHE) {
    for ( ; n > CLIB_MIN_SYMBOL_CACHE; n--) {
      g_hash_table_foreach (clib_symbol_cache,
                            (GHFunc) cache_find_oldest,
                            &cached);
      g_hash_table_remove (clib_symbol_cache, cached->ptr);
    }
  }

  return data;
}

/*! \brief Find all symbols matching a pattern.
 *
 *  \par Function Description
 *  Searches the library, returning all symbols whose
 *  names match \a pattern.
 *
 *  Two search modes are available: \b CLIB_EXACT, where \a pattern is
 *  compared to the symbol name using strcmp(), and \b CLIB_GLOB,
 *  where \a pattern is assumed to be a glob pattern (see the GLib
 *  documentation for details of the glob syntax applicable).
 *
 *  \warning The #CLibSymbol instances in the \b GList returned belong
 *  to the component library, and should be considered constants; they
 *  should not be manipulated or free()'d.  On the other hand, the \b
 *  GList returned must be freed with \b g_list_free() when no longer
 *  needed.  Note that the values returned will be invalidated by a
 *  call to s_clib_free() or s_clib_refresh().
 *
 *  \param pattern The pattern to match against.
 *  \param mode    The search mode to use.
 *  \return A \b GList of matching #CLibSymbol structures.
 */
GList *s_clib_search (const gchar *pattern, const CLibSearchMode mode)
{
  GList *sourcelist;
  GList *symlist;
  GList *result = NULL;
  CLibSource *source;
  CLibSymbol *symbol;
  GPatternSpec *globpattern = NULL;
  gchar *key;
  gchar keytype;

  if (pattern == NULL) return NULL;

  /* Use different cache keys depending on what sort of search is being done */
  switch (mode)
    {
    case CLIB_GLOB:
      keytype = 'g';
      break;
    case CLIB_EXACT:
      keytype = 's';
      break;
    default:
      g_critical ("s_clib_search: Bad search mode %i\n", mode);
      return NULL;
    }
  key = g_strdup_printf("%c%s", keytype, pattern);

  /* Check to see if the query is already in the cache */
  result = (GList *) g_hash_table_lookup (clib_search_cache, key);
  if (result != NULL) {
    g_free (key);
    return g_list_copy (result);
  }

  if (mode == CLIB_GLOB) {
    globpattern = g_pattern_spec_new(pattern);
  }

  for (sourcelist = clib_sources;
       sourcelist != NULL;
       sourcelist = g_list_next(sourcelist)) {

    source = (CLibSource *) sourcelist->data;

    for (symlist = source->symbols;
	 symlist != NULL;
	 symlist = g_list_next(symlist)) {

      symbol = (CLibSymbol *) symlist->data;

      switch (mode)
	{
	case CLIB_EXACT:
	  if (strcmp (pattern, symbol->name) == 0) {
	    result = g_list_prepend (result, symbol);
	  }
	  break;
	case CLIB_GLOB:
	  if (g_pattern_match_string (globpattern, symbol->name)) {
	    result = g_list_prepend (result, symbol);
	  }
	  break;
	}
    }
  }

  result = g_list_reverse (result);

  if (globpattern != NULL) {
    g_pattern_spec_free (globpattern);
  }

  g_hash_table_insert (clib_search_cache, key, g_list_copy (result));
  /* __don't__ free key here, it's stored by the hash table! */

  return result;
}




/*! \brief Flush the symbol name lookup cache.
 *  \par Function Description
 *  Clears the hashtable which caches the results of s_clib_search().
 *  You shouldn't ever need to call this, as all functions which
 *  invalidate the cache are supposed to make sure it's flushed.
 */
void s_clib_flush_search_cache ()
{
  g_hash_table_remove_all (clib_search_cache);  /* Introduced in glib 2.12 */
}


/*! \brief Flush the symbol data cache.
 *  \par Function Description
 *  Clears the hashtable which caches the results of s_clib_symbol_get_data().
 *  You shouldn't ever need to call this, as all functions which
 *  invalidate the cache are supposed to make sure it's flushed.
 */
void s_clib_flush_symbol_cache ()
{
  g_hash_table_remove_all (clib_symbol_cache);  /* Introduced in glib 2.12 */
}

/*! \brief Invalidate all cached data about a symbol.
 * \par Function Description
 * Removes all cached symbol data for \a symbol.
 *
 * \param symbol Symbol to flush cached data for.
 */
void
s_clib_symbol_invalidate_data (const CLibSymbol *symbol)
{
  g_hash_table_remove (clib_symbol_cache, (gpointer) symbol);
}

/*! \brief Get symbol structure for a given symbol name.
 *  \par Function Description
 *  Return the first symbol found with the given \a name.  If more
 *  than one matching symbol is found or no matches are found at all,
 *  emits a log message warning the user.
 *
 *  \param name The symbol name to match against.
 *  \return The first matching symbol, or NULL if none found.
 */
const CLibSymbol *s_clib_get_symbol_by_name (const gchar *name)
{
  GList *symlist = NULL;
  const CLibSymbol *retval;

  symlist = s_clib_search (name, CLIB_EXACT);

  if (symlist == NULL) {
    s_log_message (_("Component [%s] was not found in the component library\n"),
                   name);
    return NULL;
  }

  if (g_list_next (symlist) != NULL) { /* More than one symbol */
    s_log_message (_("More than one component found with name [%s]\n"),
                   name);
  }

  retval = (CLibSymbol *) symlist->data;
  g_list_free (symlist);

  return retval;
}

/*! \brief Get symbol data for a given symbol name.
 *  \par Function Description
 *  Return the data for the first symbol found with the given name.
 *  This is a helper function for the schematic load system, as it
 *  will always want to load symbols given only their name.
 *
 *  On failure, returns \b NULL (the error will be logged).
 *
 *  \param name The symbol name to match against.
 *  \return Allocated buffer containing symbol data.
 */
gchar *s_clib_symbol_get_data_by_name (const gchar *name)
{
  const CLibSymbol *symbol;

  symbol = s_clib_get_symbol_by_name (name);
  if (symbol == NULL) return NULL;
  return s_clib_symbol_get_data (symbol);
}

/*! \brief Get a list of symbols used.
 *  \par Function Description
 *
 *  Scan a #TOPLEVEL structure's object list looking for symbols, and
 *  return them in a list.
 *
 *  \warning The #CLibSymbol instances in the \b GList returned belong
 *  to the component library, and should be considered constants; they
 *  should not be manipulated or free()'d.  On the other hand, the \b
 *  GList returned must be freed with \b g_list_free() when no longer
 *  needed.  Note that the values returned will be invalidated by a
 *  call to s_clib_free() or s_clib_refresh().
 *
 *  \bug Only includes components which are not embedded, but they
 *  should (probably) also appear in the list.
 *
 *  \param toplevel #TOPLEVEL structure to scan.
 *  \return GList of symbols.
 */
GList *s_toplevel_get_symbols (const TOPLEVEL *toplevel)
{
  GList *result = NULL;
  GList *iter = NULL;
  OBJECT *o = NULL;
  PAGE *page;
  GList *symlist = NULL;
  CLibSymbol *sym = NULL;
  const GList *p_iter;
  const GList *o_iter;

  g_return_val_if_fail ((toplevel != NULL), NULL);

  for ( p_iter = geda_list_get_glist( toplevel->pages );
        p_iter != NULL;
        p_iter = g_list_next( p_iter )) {
    page = (PAGE *)p_iter->data;
    for (o_iter = s_page_objects (page);
         o_iter != NULL;
         o_iter = g_list_next (o_iter)) {
      o = (OBJECT *)o_iter->data;
      if (o->type != OBJ_COMPLEX) continue;
      if (o->complex_basename == NULL)  continue;

      /* Since we're not looking at embedded symbols, the first
       * component with the given name will be the one we need.
       * N.b. we don't use s_clib_get_symbol_by_name() because it's
       * spammeh. */
      symlist = s_clib_search (o->complex_basename, CLIB_EXACT);
      if (symlist == NULL) continue;
      sym = (CLibSymbol *) symlist->data;
      g_list_free (symlist);

      /* We do the list insertion by evilly comparing pointers.  This
       * is okay, because we always take the first symbol with the
       * given name, and symbol pointers don't change while this
       * function is running (we hope).  Note that this creates a
       * sorted list.*/
      for (iter = result;
           iter != NULL;
           iter = g_list_next(iter)) {
        if (iter->data == sym) {
          break; /* Already in list */
        }
        if (compare_symbol_name (iter->data, sym) > 0) {
          /* not in list yet, and gone past point where it should go */
          result = g_list_insert_before (result, iter, sym);
          break;
        }
      }
      if (iter == NULL) {
        /* not in list yet, and at end of list */
        result = g_list_append (result, sym);
      }
    }
  }

  return result;
}
