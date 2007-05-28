/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
 * Copyright (C) 1998-2007 gEDA Contributors
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

/*! \file s_clib.c
 *  \brief The component library system
 *
 *  <B>clib</B> stands for component library.
 *
 *  The <b>component library</b> is made up of a number of
 *  <b>component sources</b>, each of which in turn makes available a
 *  number of component <b>symbols</b>.  Each source may be either a
 *  directory on disk containing symbol files, or a command in the
 *  system PATH which can generate gEDA symbol data (e.g. from a
 *  database).  A component source is represented by a CLibSource
 *  instance.
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
 *  have a filename ending in ".sym" (case sensitive).  A
 *  component source based on a directory can be added using
 *  s_clib_add_directory().  Symbol files with filenames starting with
 *  a period "." are ignored.
 *
 *  An executable program in the system search path may be used as a
 *  component source, and it must conform with the specification given
 *  on page \ref libcmds.  A component source based on a command may
 *  be added using s_clib_add_command().
 *
 *  Each symbol is identified by its \b name, which is stored in the
 *  saved schematic file.  The name must be a valid for storage in a
 *  gEDA schematic file as the "basename" of a "component" object.
 *  For symbols from directory sources, the filename of the symbol is
 *  taken as the symbol name.  For a command source, the name may be
 *  any permissible string.  Guidelines to follow:
 *
 *    -# Do not begin a symbol name with "<tt>EMBEDDED</tt>"
 *    -# Do not use whitespace, or any of the characters "<tt>/:!*?</tt>".
 *    -# Try to use unique names.
 *  
 *  The component database may be queried using s_clib_glob().  A
 *  null-terminated buffer containing symbol data (suitable for
 *  loading using o_read_buffer()) may be obtained using
 *  s_clib_symbol_get_data().  If an exact symbol name is known, the
 *  symbol data may be requested directly using
 *  s_clib_symbol_get_data_by_name().
 *
 *  \todo
 *    -# Categorisation of symbols.
 *
 *    -# Case-insensitive matching of symbol file extensions (both ".sym"
 *       and ".SYM" should match).
 *
 *  \page libcmds Library Commands
 *
 *  A library command should implement this specification.  Note that
 *  as additional features may be added to the component library in
 *  the future, ideally a library command should only respond to the
 *  commands detailed here.
 *
 *  The command line syntax for a library command is:
 *
 *  <tt>libcmd \<mode\> [mode arguments]</tt>
 *
 *  All diagnostic and error information should be printed to standard
 *  error.  Only data should be printed to standard output.  All data
 *  output from a library command should be encoded using UTF8.
 *
 *  If an error occurs, the command must exit with non-zero exit
 *  status, with any diagnostic information should be printed on
 *  standard error.
 *
 *  \section libcmds_modes Modes
 *
 *  <tt>libcmd help</tt>
 *
 *  If \b help is passed as the mode, a command may output a help
 *  message.  This mode is optional.
 *
 *  <tt>libcmd list</tt>
 *
 *  If \b list is passed as the mode, a command must output a list of
 *  the symbols it provides, separated by newlines.  Lines beginning
 *  with a period '.' are ignored.  If an error occurs, the command
 *  must exit with non-zero exit status.
 *
 *  <tt>libcmd get \<symbolname\>
 *
 *  If \b get is passed as the mode, a command must output the symbol
 *  data corresponding to \b symbolname.  If \b symbolname is unknown
 *  to the command, the command must exit with non-zero exit status.
 *  
 */

#include <config.h>

#include <glib.h>

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

#include <gtk/gtk.h>
#include <libguile.h>

#include <sys/wait.h>

#include "defines.h"
#include "struct.h"
#include "globals.h"
#include "o_types.h"
#include "colors.h"
#include "i_vars.h"
#include "prototype.h"

/* Constant definitions
 * ===================
 */

/*! All symbols in directory sources end with this string */
#define SYM_FILENAME_FILTER ".sym"

/*! Library command mode used to fetch list of symbols */
#define CLIB_LIST_CMD       "list"

/*! Library command mode used to fetch symbol data */
#define CLIB_DATA_CMD       "get"

/* Type definitions
 * ================
 */

/*! Valid types of component source */
enum CLibSourceType { 
  /*! Directory source */
  CLIB_DIR, 
  /*! Command source */
  CLIB_CMD };

/*! Stores data about a particular component source */
struct _CLibSource {
  /*! Type of source */
  enum CLibSourceType type;
  /*! Path to directory or name of executable */
  gchar *path_cmd;
  /*! Name of source */
  gchar *name;
  /*! Available symbols (CLibSymbol) */
  GList *symbols;
};

/*! Stores data about a particular symbol */
struct _CLibSymbol {
  /*! The source this symbols is available from */
  CLibSource *source;
  /*! The name of this symbol */
  gchar *name;
};

/* Static variables
 * ================
 */

/*! Holds the list of all known component sources */
static GList *clib_sources = NULL;

/* Local static functions
 * ======================
 */

static void free_symbol (gpointer data, gpointer user_data);
static void free_source (gpointer data, gpointer user_data);
static gint compare_source_name (gconstpointer a, gconstpointer b);
static gint compare_symbol_name (gconstpointer a, gconstpointer b);
static gchar *run_source_command (gchar **argv);
static CLibSymbol *source_has_symbol (const CLibSource *source, 
				      const gchar *name);
static void refresh_directory (CLibSource *source);
static void refresh_command (CLibSource *source);
static gchar *get_data_directory (const CLibSymbol *symbol);
static gchar *get_data_command (const CLibSymbol *symbol);

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
  }
}

/*! \brief Iterator callback for freeing a source.
 *  \par Function Description
 *  Private function used only in s_clib.c.
 */
static void free_source (gpointer data, gpointer user_data)
{
  CLibSource *source = data;
  if (source != NULL) {
    if (source->path_cmd != NULL) {
      g_free (source->path_cmd);
      source->path_cmd = NULL;
    }
    if (source->name != NULL) {
      g_free (source->name);
      source->name = NULL;
    }
    if (source->symbols != NULL) {
      g_list_foreach (source->symbols, (GFunc) free_symbol, NULL);
      g_list_free (source->symbols);
      source->symbols = NULL;
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

  g_assert (src1 != NULL);
  g_assert (src2 != NULL);

  g_assert (src1->name != NULL);
  g_assert (src2->name != NULL);

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

  g_assert (sym1 != NULL);
  g_assert (sym2 != NULL);

  g_assert (sym1->name != NULL);
  g_assert (sym2->name != NULL);

  return strcasecmp(sym1->name, sym2->name);
}


/*! \brief Execute a library command.
 *  \par Function Description
 *  Execute a library command, returning the standard output, or \b
 *  NULL if the command fails for some reason.  The system \b PATH is
 *  used to find the program to execute.
 *
 *  Private function used only in s_clib.c.
 *
 *  \param argv null-terminated list of arguments.  The name of the
 *              program to execute should appear first.
 *
 *  \return The program's output, or \b NULL on failure.
 */
static gchar *run_source_command (gchar **argv)
{
  gchar *standard_output = NULL;
  gchar *standard_error = NULL;
  gint exit_status;
  GError *e = NULL;
  gchar *command = NULL;
  gboolean success = FALSE;
  
  g_spawn_sync (NULL, /* Use gschem's CWD */
		argv,
		NULL, /* No special environment */
		G_SPAWN_SEARCH_PATH,
		NULL, /* No setup function (not portable anyway) */
		NULL, /* No user data */
		&standard_output,
		&standard_error,
		&exit_status,
		&e);

  command = g_strjoinv (" ", argv);

  if (e != NULL) {
    s_log_message ("Library command failed [%s]: %s\n", command, 
		   e->message);
    g_error_free (e);

  } else if (WIFSIGNALED(exit_status)) {
    s_log_message ("Library command failed [%s]: Uncaught signal %i.\n",
		   command, WTERMSIG(exit_status));
    
  } else if (!WIFEXITED(exit_status)) {
    s_log_message ("Library command failed [%s]\n", command);
    s_log_message("Error output was:\n%s\n", standard_error);

  } else {
    success = TRUE;
  }

  g_free (command);
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
GList *s_clib_get_sources ()
{
  GList *l = g_list_copy(clib_sources);
  l = g_list_sort (l, (GCompareFunc) compare_source_name);
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
  gchar *fullpath;
  gboolean isfile;
  GError *e = NULL;

  g_assert (source != NULL);
  g_assert (source->type == CLIB_DIR);

  /* Clear the current symbol list */
  g_list_foreach (source->symbols, (GFunc) free_symbol, NULL);
  g_list_free (source->symbols);
  source->symbols = NULL;  

  /* Open the directory for reading. */
  dir = g_dir_open (source->path_cmd, 0, &e);

  if (e != NULL) {
    s_log_message ("Failed to open directory [%s]: %s\n",
		   source->path_cmd, e->message);
    g_error_free (e);
    return;
  }

  while ((entry = g_dir_read_name (dir)) != NULL) {
    /* skip ".", ".." & hidden files */
    if (entry[0] == '.') continue;

    /* skip subdirectories (for now) */
    fullpath = g_build_filename (source->path_cmd, entry, NULL);
    isfile = g_file_test (fullpath, G_FILE_TEST_IS_REGULAR);
    g_free (fullpath);
    if (!isfile) continue;

    /* skip filenames that don't match the filter or that we already
     * know about. */
    if (!g_str_has_suffix (entry, SYM_FILENAME_FILTER)
	|| (source_has_symbol (source, entry) != NULL)) {
      continue;
    }

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
  gchar *argv[3];

  g_assert (source != NULL);
  g_assert (source->type == CLIB_CMD);

  /* Clear the current symbol list */
  g_list_foreach (source->symbols, (GFunc) free_symbol, NULL);
  g_list_free (source->symbols);
  source->symbols = NULL;  

  /* Run the command to get the list of symbols */
  argv[0] = source->path_cmd;
  argv[1] = CLIB_LIST_CMD;
  argv[2] = NULL;
  cmdout = run_source_command ( argv );
  if (cmdout == NULL) return;

  /* Use a TextBuffer to help reading out the lines of the output */
  tb = s_textbuffer_new (cmdout, -1);

  while (1) {
    line = s_textbuffer_next_line (tb);
    if (line == NULL) break;
    if (line[0] == '.') continue;  /* TODO is this sane? */

    name = remove_nl(g_strdup(line));

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
}

/*! \brief Rescan all available component libraries.
 *  \par Function Description
 *  Resets the list of symbols available from each source, and
 *  repopulates it from scratch.  Useful e.g. for checking for new
 *  symbols.
 *
 *  \todo Disabled for now because it would break cached CLibSymbols used
 *  all over the place (e.g. in #st_object).
 */
void s_clib_refresh ()
{
#if 0
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
      default:
	g_assert_not_reached();
      }
  }
#endif
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
 *  \param directory The path of the directory to add (UTF8).
 *  \param name      A descriptive name for the directory.
 *  \return The #CLibSource associated with the directory.
 */
const CLibSource *s_clib_add_directory (const gchar *directory, 
					const gchar *name)
{
  CLibSource *source;
  gchar *realname;

  if (directory == NULL) {
    return NULL;
  }
  
  if (name == NULL) {
    realname = g_path_get_basename (directory);
  } else {
    realname = g_strdup(name);
  }  

  source = s_clib_get_source_by_name (realname);
  if (source != NULL) {
    s_log_message ("Cannot add library [%s]: name in use.",
		   realname);
    g_free (realname);
    return NULL;
  }

  source = g_new0 (CLibSource, 1);
  source->type = CLIB_DIR;
  source->path_cmd = g_strdup (directory);
  source->name = realname;

  refresh_directory (source);

  /* Sources added later get scanned earlier */
  clib_sources = g_list_prepend (clib_sources, source);

  return source;
}

/*! \brief Add a symbol-generating command to the library
 *  \par Function Description
 *  Adds a command which can generate symbols to the library.  See
 *  page \ref libcmds for more information on library commands.  A \a
 *  name may be specified for the source; if \a name is \b NULL, the
 *  command is used as the name.
 *  
 *  \param command The executable to run, resolved using the \b PATH
 *                 environment variable.
 *  \param name    A descriptive name for the command.
 *  \return The CLibSource associated with the command.
 */
const CLibSource *s_clib_add_command (const gchar *command,
				      const gchar *name)
{
  CLibSource *source;
  gchar *realname;

  if (command == NULL) {
    return NULL;
  }

  if (name == NULL) {
    realname = g_strdup (command);
  } else {
    realname = g_strdup (name);
  }
  
  source = s_clib_get_source_by_name (realname);
  if (source != NULL) {
    s_log_message ("Cannot add library [%s]: name in use.",
		   realname);
    g_free (realname);
    return NULL;
  }

  source = g_new0 (CLibSource, 1);
  source->type = CLIB_CMD;
  source->path_cmd = g_strdup (command);
  source->name = realname;

  refresh_command (source);

  /* Sources added later get scanned earlier */
  clib_sources = g_list_prepend (clib_sources, source);

  return source;
}


/*! \brief Get the name of a source.
 *  \par Function Description
 *  Get the name of a source for use e.g. in displaying a GUI.
 *
 *  \todo Make this do something cleverer than just returning
 *  \b path_cmd.
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

  return g_build_filename(symbol->source->path_cmd, symbol->name, NULL);
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

  g_assert (symbol != NULL);
  g_assert (symbol->source->type == CLIB_DIR);

  filename = g_build_filename(symbol->source->path_cmd, 
			      symbol->name, NULL);

  g_file_get_contents (filename, &data, NULL, &e);

  if (e != NULL) {
    s_log_message ("Failed to load symbol from file [%s]: %s\n",
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
  gchar *argv[4];

  g_assert (symbol != NULL);
  g_assert (symbol->source->type == CLIB_CMD);
  
  argv[0] = symbol->source->path_cmd;
  argv[1] = CLIB_DATA_CMD;
  argv[2] = symbol->name;
  argv[3] = NULL;

  return run_source_command ( argv );
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
  g_assert (symbol != NULL);

  switch (symbol->source->type)
    {
    case CLIB_DIR:
      return get_data_directory (symbol);
    case CLIB_CMD:
      return get_data_command (symbol);
    default:
      g_assert_not_reached();
    }
}


/*! \brief Find all symbols matching a glob pattern.  \par Function
 *  Description Searches the library, returning all symbols whose
 *  names match \a glob (see the GLib documentation for details of the
 *  glob syntax applicable).
 *
 *  \warning The #CLibSymbol instances in the \b GList returned belong
 *  to the component library, and should be considered constants; they
 *  should not be manipulated or free()'d.  On the other hand, the \b
 *  GList returned must be freed with \b g_list_free() when no longer
 *  needed.  Note that the values returned will be invalidated by a
 *  call to s_clib_free() or s_clib_refresh().
 *
 *  \param glob The glob pattern to match against.
 *  \return A \b GList of matching #CLibSymbol structures.
 */
GList *s_clib_glob (const gchar *glob)
{  
  GList *sourcelist;
  GList *symlist;
  GList *result = NULL;
  CLibSource *source;
  CLibSymbol *symbol;
  GPatternSpec *pattern;

  if (glob == NULL) return NULL;

  pattern = g_pattern_spec_new(glob);

  for (sourcelist = clib_sources; 
       sourcelist != NULL; 
       sourcelist = g_list_next(sourcelist)) {

    source = (CLibSource *) sourcelist->data;

    for (symlist = source->symbols;
	 symlist != NULL;
	 symlist = g_list_next(symlist)) {
    
      symbol = (CLibSymbol *) symlist->data;

      if (g_pattern_match_string (pattern, symbol->name)) {
	result = g_list_prepend (result, symbol);
      }

    }
    
  }

  result = g_list_reverse (result);

  g_pattern_spec_free (pattern);

  return result;
}

/*! \brief Get symbol data for a given symbol name.
 *  \par Function Description
 *  Return the data for the first symbol found with the given name.
 *  This is a helper function for the schematic load system, as it
 *  will always want to load symbols given only their name.
 *
 *  On failure, returns \b NULL (the error will be logged).
 *
 *  \todo Speed this up repeated calls by caching the #CLibSymbol
 *  pointers found for each name requested.
 *
 *  \param name The symbol name to match against.
 *  \return Allocated buffer containing symbol data.
 */
gchar *s_clib_symbol_get_data_by_name (const gchar *name)
{
  GList *sourcelist;
  CLibSource *source;
  CLibSymbol *symbol;

  for (sourcelist = clib_sources; 
       sourcelist != NULL; 
       sourcelist = g_list_next(sourcelist)) {

    source = (CLibSource *) sourcelist->data;

    symbol = source_has_symbol (source, name);

    if (symbol != NULL) {
      return s_clib_symbol_get_data (symbol);
    }
    
  }

  return NULL;
}
