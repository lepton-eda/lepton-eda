/* gsch2pcb
 * Copyright (C) 2003-2016 gEDA Contributors
 * Copyright (C) 2017-2023 Lepton EDA Contributors
 *
 *  Bill Wilson    billw@wt.net
 *
 *  This program is free software which I release under the GNU General Public
 *  License. You may redistribute and/or modify this program under the terms
 *  of that license as published by the Free Software Foundation; either
 *  version 2 of the License, or (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.  Version 2 is in the
 *  COPYRIGHT file in the top level directory of this distribution.
 *
 *  To get a copy of the GNU General Puplic License, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */


#include <config.h>
#include <version.h>

#include <ctype.h>

#include "sch2pcb.h"
#include "liblepton_priv.h"

#define SEP_STRING "--------\n"


gchar*
pcb_element_get_refdes (PcbElement *element)
{
  return element->refdes;
}

void
pcb_element_set_refdes (PcbElement *element,
                        gchar *val)
{
  g_free (element->refdes);
  element->refdes = g_strdup (val);
}


gchar*
pcb_element_get_value (PcbElement *element)
{
  return element->value;
}

void
pcb_element_set_value (PcbElement *element,
                       gchar *val)
{
  g_free (element->value);
  element->value = g_strdup (val);
}


gchar*
pcb_element_get_description (PcbElement *element)
{
  return element->description;
}

void
pcb_element_set_description (PcbElement *element,
                             gchar *val)
{
  g_free (element->description);
  element->description = g_strdup (val);
}


gchar*
pcb_element_get_changed_description (PcbElement *element)
{
  return element->changed_description;
}

void
pcb_element_set_changed_description (PcbElement *element,
                                     gchar *val)
{
  g_free (element->changed_description);
  element->changed_description = g_strdup (val);
}


gchar*
pcb_element_get_changed_value (PcbElement *element)
{
  return element->changed_value;
}

void
pcb_element_set_changed_value (PcbElement *element,
                               gchar *val)
{
  g_free (element->changed_value);
  element->changed_value = g_strdup (val);
}


gchar*
pcb_element_get_flags (PcbElement *element)
{
  return element->flags;
}

void
pcb_element_set_flags (PcbElement *element,
                       gchar *val)
{
  g_free (element->flags);
  element->flags = g_strdup (val);
}


gchar*
pcb_element_get_tail (PcbElement *element)
{
  return element->tail;
}

void
pcb_element_set_tail (PcbElement *element,
                      gchar *val)
{
  g_free (element->tail);
  element->tail = g_strdup (val);
}


gchar*
pcb_element_get_x (PcbElement *element)
{
  return element->x;
}

void
pcb_element_set_x (PcbElement *element,
                   gchar *val)
{
  g_free (element->x);
  element->x = g_strdup (val);
}


gchar*
pcb_element_get_y (PcbElement *element)
{
  return element->y;
}

void
pcb_element_set_y (PcbElement *element,
                   gchar *val)
{
  g_free (element->y);
  element->y = g_strdup (val);
}


gchar*
pcb_element_get_pkg_name_fix (PcbElement *element)
{
  return element->pkg_name_fix;
}

void
pcb_element_set_pkg_name_fix (PcbElement *element,
                              gchar *val)
{
  g_free (element->pkg_name_fix);
  element->pkg_name_fix = g_strdup (val);
}


gchar
pcb_element_get_res_char (PcbElement *element)
{
  return element->res_char;
}

void
pcb_element_set_res_char (PcbElement *element,
                          gchar val)
{
  element->res_char = val;
}


gboolean
pcb_element_get_still_exists (PcbElement *element)
{
  return element->still_exists;
}

void
pcb_element_set_still_exists (PcbElement *element,
                              gboolean val)
{
  element->still_exists = val;
}


gboolean
pcb_element_get_new_format (PcbElement *element)
{
  return element->new_format;
}

void
pcb_element_set_new_format (PcbElement *element,
                            gboolean val)
{
  element->new_format = val;
}


gboolean
pcb_element_get_hi_res_format (PcbElement *element)
{
  return element->hi_res_format;
}

void
pcb_element_set_hi_res_format (PcbElement *element,
                              gboolean val)
{
  element->hi_res_format = val;
}



gboolean
pcb_element_get_quoted_flags (PcbElement *element)
{
  return element->quoted_flags;
}

void
pcb_element_set_quoted_flags (PcbElement *element,
                              gboolean val)
{
  element->quoted_flags = val;
}


gboolean
pcb_element_get_omit_PKG (PcbElement *element)
{
  return element->omit_PKG;
}

void
pcb_element_set_omit_PKG (PcbElement *element,
                          gboolean val)
{
  element->omit_PKG = val;
}



static GList *pcb_element_list = NULL;

GList*
sch2pcb_get_pcb_element_list ()
{
  return pcb_element_list;
}


static gboolean bak_done;


static gchar *empty_footprint_name;

char*
sch2pcb_get_empty_footprint_name ()
{
  return empty_footprint_name;
}

void
sch2pcb_set_empty_footprint_name (char *val)
{
  g_free (empty_footprint_name);
  empty_footprint_name = g_strdup (val);
}


static int n_changed_value;

int
sch2pcb_get_n_changed_value ()
{
  return n_changed_value;
}

void
sch2pcb_set_n_changed_value (int val)
{
  n_changed_value = val;
}


static int n_deleted;

int
sch2pcb_get_n_deleted ()
{
  return n_deleted;
}

void
sch2pcb_set_n_deleted (int val)
{
  n_deleted = val;
}


static int n_empty;

int
sch2pcb_get_n_empty ()
{
  return n_empty;
}

void
sch2pcb_set_n_empty (int val)
{
  n_empty = val;
}


static int n_fixed;

int
sch2pcb_get_n_fixed ()
{
  return n_fixed;
}

void
sch2pcb_set_n_fixed (int val)
{
  n_fixed = val;
}


static int n_none;

int
sch2pcb_get_n_none ()
{
  return n_none;
}

void
sch2pcb_set_n_none (int val)
{
  n_none = val;
}


static int n_PKG_removed_old;

int
sch2pcb_get_n_PKG_removed_old ()
{
  return n_PKG_removed_old;
}

void
sch2pcb_set_n_PKG_removed_old (int val)
{
  n_PKG_removed_old = val;
}


static gint n_preserved;

int
sch2pcb_get_n_preserved ()
{
  return n_preserved;
}

void
sch2pcb_set_n_preserved (int val)
{
  n_preserved = val;
}


static int n_unknown;

int
sch2pcb_get_n_unknown ()
{
  return n_unknown;
}

void
sch2pcb_set_n_unknown (int val)
{
  n_unknown = val;
}


static gboolean need_PKG_purge;

gboolean
sch2pcb_get_need_PKG_purge ()
{
  return need_PKG_purge;
}

void
sch2pcb_set_need_PKG_purge (gboolean val)
{
  need_PKG_purge = val;
}


static gboolean preserve;

gboolean
sch2pcb_get_preserve ()
{
  return preserve;
}

void
sch2pcb_set_preserve (gboolean val)
{
  preserve = val;
}


static gint verbose;

gint
sch2pcb_get_verbose_mode ()
{
  return verbose;
}

void
sch2pcb_increment_verbose_mode ()
{
  verbose += 1;
}


/**
 * Build and run a command. No redirection or error handling is
 * done.  Format string is split on whitespace. Specifiers %l and %s
 * are replaced with contents of positional args. To be recognized,
 * specifiers must be separated from other arguments in the format by
 * whitespace.
 *  - %l expects a GList, contents used as separate arguments
 *  - %s expects a gchar*, contents used as a single argument
 * @param[in] format  used to specify command to be executed
 * @param[in] ...     positional parameters
 */
static gboolean
build_and_run_command (const gchar *format, ...)
{
  va_list vargs;
  gchar ** split;
  GList *tmp = NULL;
  gint num_split;
  gint i;
  gint status;
  gboolean result = FALSE;
  gchar *standard_output = NULL;
  gchar *standard_error = NULL;
  GError * error = NULL;

  va_start (vargs, format);
  split = g_strsplit_set (format, " \t\n\v", 0);
  num_split = g_strv_length (split);
  for (i = 0; i < num_split; ++i) {
    gchar *chunk = split[i];
    if (strcmp (chunk, "%l") == 0) {
      /* append contents of list into command args - shared data */
      tmp = g_list_concat (tmp, g_list_copy (va_arg (vargs, GList*)));
    } else if (strcmp (chunk, "%s") == 0) {
      /* insert contents of string into output */
      tmp = g_list_append (tmp, va_arg (vargs, gchar*));
    } else {
      /* bare string, use as is */
      tmp = g_list_append (tmp, chunk);
    }
  }
  va_end (vargs);

  if (tmp) {
    /* we have something in the list, build & call command */
    GList *p;
    gint i = 0;
    gchar ** args = g_new0 (gchar*, g_list_length (tmp) + 1/* NULL terminate the list */);

    if (verbose)
      printf ("Running command:\n\t");

    for (p = tmp; p; p = g_list_next (p)) {
      args[i++] = (gchar*) p->data;
      if (verbose)
        printf ("%s ", (char*)p->data);
    }

    if (verbose)
      printf ("\n%s", SEP_STRING);

    if (g_spawn_sync (".",                  /* Working directory */
                      args,                 /* argv */
                      NULL,                 /* envp */
                      G_SPAWN_SEARCH_PATH,  /* flags */
                      NULL,                 /* child_setup */
                      NULL,                 /* user data */
                      &standard_output,     /* standard output */
                      &standard_error,      /* standard error */
                      &status,              /* exit status return */
                      &error)) {            /* GError return */
      if (verbose)
        fputs(standard_output, stdout);
      if (status == 0)
        result = TRUE;
      else {
        if (standard_error)
          fputs(standard_error, stderr);
      }
    }
    else {
      fprintf(stderr, "Failed to execute external program: %s\n", error->message);
      g_error_free(error);
    }

    if (verbose)
      printf ("\n%s", SEP_STRING);

    g_free(standard_error);
    g_free (standard_output);

    g_free (args);
    /* free the list, but leave data untouched */
    g_list_free (tmp);
  }

  g_strfreev (split);

  return result;
}


static gchar *
token (gchar * string, gchar ** next, gboolean * quoted_ret)
{
  static gchar *str;
  gchar *s, *ret;
  gboolean quoted = FALSE;

  if (string)
    str = string;
  if (!str || !*str) {
    if (next)
      *next = str;
    return g_strdup ("");
  }
  while (*str == ' ' || *str == '\t' || *str == ',' || *str == '\n')
    ++str;
  if (*str == '"') {
    quoted = TRUE;
    if (quoted_ret)
      *quoted_ret = TRUE;
    ++str;
    for (s = str; *s && *s != '"' && *s != '\n'; ++s);
  } else {
    if (quoted_ret)
      *quoted_ret = FALSE;
    for (s = str;
         *s && (*s != ' ' && *s != '\t' && *s != ',' && *s != '\n'); ++s);
  }
  ret = g_strndup (str, s - str);
  str = (quoted && *s) ? s + 1 : s;
  if (next)
    *next = str;
  return ret;
}

static gchar *
fix_spaces (gchar * str)
{
  gchar *s;

  if (!str)
    return NULL;
  for (s = str; *s; ++s)
    if (*s == ' ' || *s == '\t')
      *s = '_';
  return str;
}

  /* As of 1/9/2004 CVS hi_res Element[] line format:
   *   Element[element_flags, description, pcb-name, value, mark_x, mark_y,
   *       text_x, text_y, text_direction, text_scale, text_flags]
   *   New PCB 1.7 / 1.99 Element() line format:
   *   Element(element_flags, description, pcb-name, value, mark_x, mark_y,
   *       text_x, text_y, text_direction, text_scale, text_flags)
   *   Old PCB 1.6 Element() line format:
   *   Element(element_flags, description, pcb-name, value,
   *       text_x, text_y, text_direction, text_scale, text_flags)
   *
   *   (mark_x, mark_y) is the element position (mark) and (text_x,text_y)
   *   is the description text position which is absolute in pre 1.7 and
   *   is now relative.  The hi_res mark_x,mark_y and text_x,text_y resolutions
   *   are 100x the other formats.
   */
PcbElement *
pcb_element_line_parse (gchar * line)
{
  PcbElement *el = NULL;
  gchar *s, *t, close_char;
  gint state = 0, elcount = 0;

  if (strncmp (line, "Element", 7))
    return NULL;

  el = g_new0 (PcbElement, 1);

  s = line + 7;
  while (*s == ' ' || *s == '\t')
    ++s;

  if (*s == '[')
    el->hi_res_format = TRUE;
  else if (*s != '(') {
    g_free (el);
    return NULL;
  }

  el->res_char = el->hi_res_format ? '[' : '(';
  close_char = el->hi_res_format ? ']' : ')';

  el->flags = token (s + 1, NULL, &el->quoted_flags);
  el->description = token (NULL, NULL, NULL);
  el->refdes = token (NULL, NULL, NULL);
  el->value = token (NULL, NULL, NULL);

  el->x = token (NULL, NULL, NULL);
  el->y = token (NULL, &t, NULL);

  el->tail = g_strdup (t ? t : "");
  if ((s = strrchr (el->tail, (gint) '\n')) != NULL)
    *s = '\0';

  /* Count the tokens in tail to decide if it's new or old format.
   * Old format will have 3 tokens, new format will have 5 tokens.
   */
  for (s = el->tail; *s && *s != close_char; ++s) {
    if (*s != ' ') {
      if (state == 0)
        ++elcount;
      state = 1;
    } else
      state = 0;
  }
  if (elcount > 4)
    el->new_format = TRUE;

  fix_spaces (el->description);
  fix_spaces (el->refdes);
  fix_spaces (el->value);

  /* Don't allow elements with no refdes to ever be deleted because
   * they may be desired pc board elements not in schematics.  So
   * initialize still_exists to TRUE if empty or non-alphanumeric
   * refdes.
   */
  if (!*el->refdes || !isalnum ((gint) (*el->refdes)))
    el->still_exists = TRUE;

  return el;
}

void
pcb_element_free (PcbElement * el)
{
  if (!el)
    return;
  g_free (el->flags);
  g_free (el->description);
  g_free (el->changed_description);
  g_free (el->changed_value);
  g_free (el->refdes);
  g_free (el->value);
  g_free (el->x);
  g_free (el->y);
  g_free (el->tail);
  g_free (el->pkg_name_fix);
  g_free (el);
}


void
sch2pcb_pcb_element_list_append (PcbElement *element)
{
  pcb_element_list = g_list_append (pcb_element_list, element);
}


PcbElement*
pcb_element_exists (PcbElement *el_test,
                    gboolean record)
{
  GList *list;
  PcbElement *el;

  for (list = pcb_element_list; list; list = g_list_next (list)) {
    el = (PcbElement *) list->data;

    if (strcmp (el_test->refdes, el->refdes))
      continue;
    if (strcmp (el_test->description, el->description)) { /* footprint */
      if (record)
        el->changed_description = g_strdup (el_test->description);
    } else {
      if (record) {
        if (strcmp (el_test->value, el->value))
          el->changed_value = g_strdup (el_test->value);
        el->still_exists = TRUE;
      }
      return el;
    }
  }
  return NULL;
}

/* A problem is that new PCB 1.7 file elements have the
 * (mark_x,mark_y) value set to wherever the element was created and
 * no equivalent of a gschem translate symbol was done.
 *
 * So, file elements inserted can be scattered over a big area and
 * this is bad when loading a file.new.pcb into an existing PC
 * board.  So, do a simple translate if (mark_x,mark_y) is
 * (arbitrarily) over 1000.  I'll assume that for values < 1000 the
 * element creator was concerned with a sane initial element
 * placement.  Unless someone has a better idea?  Don't bother with
 * pre PCB 1.7 formats as that would require parsing the mark().
 * Current m4 elements use the old format but they seem to have a
 * reasonable initial mark().
 */
static void
simple_translate (PcbElement * el)
{

  el->x=strdup("0");
  el->y=strdup("0");
}


gboolean
sch2pcb_insert_element (FILE *f_out,
                        gchar *element_file,
                        gchar *footprint,
                        gchar *refdes,
                        gchar *value)
{
  FILE *f_in;
  PcbElement *el;
  gchar *fmt, *s, buf[1024];
  gboolean retval = FALSE;

  if ((f_in = fopen (element_file, "r")) == NULL) {
    s = g_strdup_printf ("insert_element() can't open %s", element_file);
    perror (s);
    g_free (s);
    return FALSE;
  }
  /* Scan the file to detect whether it's actually a PCB
   * layout. Assumes that a PCB layout will have a "PCB" line. */
  while ((fgets (buf, sizeof (buf), f_in)) != NULL) {
    for (s = buf; *s == ' ' || *s == '\t'; ++s);
    s[3] = 0;                   /* Truncate line */
    if (strncmp ("PCB", s, sizeof (buf)) == 0) {
      printf ("Warning: %s appears to be a PCB layout file. Skipping.\n",
              element_file);
      fclose (f_in);
      return FALSE;
    }
  }
  rewind (f_in);

  /* Copy the file element lines.  Substitute new parameters into the
   * Element() or Element[] line and strip comments.
   */
  while ((fgets (buf, sizeof (buf), f_in)) != NULL) {
    for (s = buf; *s == ' ' || *s == '\t'; ++s);
    if ((el = pcb_element_line_parse (s)) != NULL) {
      simple_translate (el);
      fmt = (gchar*) (el->quoted_flags ?
                      "Element%c\"%s\" \"%s\" \"%s\" \"%s\" %s %s%s\n" :
                      "Element%c%s \"%s\" \"%s\" \"%s\" %s %s%s\n");

      fprintf (f_out, fmt,
               el->res_char, el->flags, footprint, refdes, value,
               el->x, el->y, el->tail);
      retval = TRUE;
    } else if (*s != '#')
      fputs (buf, f_out);
    pcb_element_free (el);
  }
  fclose (f_in);
  return retval;
}


gchar*
sch2pcb_find_element (gchar *dir_path,
                      gchar *element)
{
  GDir *dir;
  gchar *path, *name, *s, *found = NULL;

  if ((dir = g_dir_open (dir_path, 0, NULL)) == NULL) {
    s = g_strdup_printf ("sch2pcb_find_element can't open dir \"%s\"", dir_path);
    perror (s);
    g_free (s);
    return NULL;
  }
  if (verbose > 1)
    printf ("\t  Searching: \"%s\" for \"%s\"\n", dir_path, element);
  while ((name = (gchar *) g_dir_read_name (dir)) != NULL) {
    path = g_strconcat (dir_path, "/", name, NULL);
    found = NULL;

    /* if we got a directory name, then recurse down into it */
    if (g_file_test (path, G_FILE_TEST_IS_DIR))
      found = sch2pcb_find_element (path, element);

    /* otherwise assume it is a file and see if it is the one we want */
    else {
      if (verbose > 1)
        printf ("\t           : %s\t", name);
      if (!strcmp (name, element))
        found = g_strdup (path);
      else {
        gchar *tmps;
        tmps = g_strconcat (element, ".fp", NULL);
        if (!strcmp (name, tmps))
          found = g_strdup (path);
        g_free (tmps);
      }
      if (verbose > 1)
        printf ("%s\n", found ? "Yes" : "No");
    }
    g_free (path);
    if (found)
      break;
  }
  g_dir_close (dir);
  return found;
}

/* The gnetlist backend gnet-gsch2pcb.scm generates PKG_ lines:
 *
 *        PKG_footprint(footprint{-fp0-fp1},refdes,value{,fp0,fp1})
 *
 * where fp1 and fp2 (if they exist) are the extra footprint
 * components when specifying footprints like "DIL 14 300".  This is
 * needed for m4 macros.
 *
 * A complication is if the footprint references a file element with
 * spaces embedded in the name.  The gnetlist backend will interpret
 * these as fp0, fp1, ... args and the footprint will in this case
 * incorrectly have '-' inserted where the spaces should be.  So, if
 * there are additional args, reconstruct the portion of the name
 * given by the args with spaces for later use.  Eg. if the footprint
 * is "100 Pin jack", we will have
 *
 *      PKG_100-Pin-jack(100-Pin-jack,refdes,value,Pin,jack)
 *
 *  So put "Pin jack" into pkg_name_fix so if this element is searched
 *  as a file element we can munge the description to what it should
 *  be, eg:
 *
 *      100-Pin-jack -> 100 Pin jack
 */
PcbElement*
pcb_element_pkg_to_element (gchar *pkg_line)
{
  PcbElement *el;
  gchar **args, *s;
  gint n, n_extra_args, n_dashes;

  if (strncmp (pkg_line, "PKG_", 4)
      || (s = strchr (pkg_line, (gint) '(')) == NULL)
    return NULL;

  args = g_strsplit (s + 1, ",", 12);
  if (!args[0] || !args[1] || !args[2]) {
    fprintf (stderr, "Bad package line: %s\n", pkg_line);
    return NULL;
  }
  fix_spaces (args[0]);
  fix_spaces (args[1]);
  fix_spaces (args[2]);

  el = g_new0 (PcbElement, 1);
  el->description = g_strdup (args[0]);
  el->refdes = g_strdup (args[1]);
  el->value = g_strdup (args[2]);
  if ((s = strchr (el->value, (gint) ')')) != NULL)
    *s = '\0';

  /* If the component value has a comma, eg "1k, 1%", the gnetlist generated
   * PKG line will be
   *
   *   PKG_XXX(`R0w8',`R100',`1k, 1%'),
   *
   * but after processed by m4, the input to gsch2pcb will be
   *
   *   PKG_XXX(R0w8,R100,1k, 1%).
   *
   * So the quoting info has been lost when processing for file
   * elements.  So here try to detect and fix this.  But I can't
   * handle the situation where the description has a '-' and the
   * value has a comma because gnet-gsch2pcb.scm munges the
   * description with '-' when there are extra args.
   */
  for (n_extra_args = 0; args[3 + n_extra_args] != NULL; ++n_extra_args);
  s = el->description;
  for (n_dashes = 0; (s = strchr (s + 1, '-')) != NULL; ++n_dashes);

  n = 3;
  if (n_extra_args == n_dashes + 1) { /* Assume there was a comma in the value, eg "1K, 1%" */
    s = el->value;
    el->value = g_strconcat (s, ",", fix_spaces (args[n]), NULL);
    g_free (s);
    if ((s = strchr (el->value, (gint) ')')) != NULL)
      *s = '\0';
    n = 4;
  }
  if (args[n]) {
    el->pkg_name_fix = g_strdup (args[n]);
    for (n += 1; args[n] != NULL; ++n) {
      s = el->pkg_name_fix;
      el->pkg_name_fix = g_strconcat (s, " ", args[n], NULL);
      g_free (s);
    }
    if ((s = strchr (el->pkg_name_fix, (gint) ')')) != NULL)
      *s = '\0';
  }
  g_strfreev (args);

  if (empty_footprint_name && !strcmp (el->description, empty_footprint_name)) {
    if (verbose)
      printf
        ("%s: has the empty footprint attribute \"%s\" so won't be in the layout.\n",
         el->refdes, el->description);
    n_empty += 1;
    el->omit_PKG = TRUE;
  } else if (!strcmp (el->description, "none")) {
    fprintf (stderr,
             "WARNING: %s has a footprint attribute \"%s\" so won't be in the layout.\n",
             el->refdes, el->description);
    n_none += 1;
    el->omit_PKG = TRUE;
  } else if (!strcmp (el->description, "unknown")) {
    fprintf (stderr,
             "WARNING: %s has no footprint attribute so won't be in the layout.\n",
             el->refdes);
    n_unknown += 1;
    el->omit_PKG = TRUE;
  }
  return el;
}


FILE*
sch2pcb_open_file_to_write (char *filename)
{
  FILE *f_out;

  if ((f_out = fopen (filename, "wb")) == NULL)
  {
    return NULL;
  }
  else
  {
    return f_out;
  }
}


void
sch2pcb_close_file (FILE *file)
{
  fclose (file);
}


void
sch2pcb_buffer_to_file (char *buffer,
                        FILE *file)
{
  fputs (buffer, file);
}


void
sch2pcb_update_element_descriptions (gchar *pcb_file,
                                     gchar *bak)
{
  FILE *f_in, *f_out;
  GList *list;
  PcbElement *el, *el_exists;
  gchar *fmt, *tmp, *s, buf[1024];

  for (list = pcb_element_list; list; list = g_list_next (list)) {
    el = (PcbElement *) list->data;
    if (el->changed_description)
      ++n_fixed;
  }
  if (!pcb_element_list
      || sch2pcb_get_n_fixed () == 0)
  {
    fprintf (stderr, "Could not find any elements to fix.\n");
    return;
  }
  if ((f_in = fopen (pcb_file, "r")) == NULL)
    return;
  tmp = g_strconcat (pcb_file, ".tmp", NULL);
  if ((f_out = fopen (tmp, "wb")) == NULL) {
    fclose (f_in);
    return;
  }
  while ((fgets (buf, sizeof (buf), f_in)) != NULL) {
    for (s = buf; *s == ' ' || *s == '\t'; ++s);
    if ((el = pcb_element_line_parse (s)) != NULL
        && (el_exists = pcb_element_exists (el, FALSE)) != NULL
        && el_exists->changed_description) {
      fmt = (gchar*) (el->quoted_flags ?
                      "Element%c\"%s\" \"%s\" \"%s\" \"%s\" %s %s%s\n" :
                      "Element%c%s \"%s\" \"%s\" \"%s\" %s %s%s\n");
      fprintf (f_out, fmt,
               el->res_char,
               el->flags, el_exists->changed_description,
               el->refdes, el->value, el->x, el->y, el->tail);
      printf ("%s: updating element Description: %s -> %s\n",
              el->refdes, el->description, el_exists->changed_description);
      el_exists->still_exists = TRUE;
    } else
      fputs (buf, f_out);
    pcb_element_free (el);
  }
  fclose (f_in);
  fclose (f_out);

  if (!bak_done) {
    build_and_run_command ("mv %s %s", pcb_file, bak);
    bak_done = TRUE;
  }

  build_and_run_command ("mv %s %s", tmp, pcb_file);
  g_free (tmp);
}

void
sch2pcb_prune_elements (gchar *pcb_file,
                        gchar *bak)
{
  FILE *f_in, *f_out;
  GList *list;
  PcbElement *el, *el_exists;
  gchar *fmt, *tmp, *s, buf[1024];
  gint paren_level = 0;
  gboolean skipping = FALSE;

  for (list = pcb_element_list; list; list = g_list_next (list)) {
    el = (PcbElement *) list->data;
    if (!el->still_exists) {
      if (preserve) {
        ++n_preserved;
        fprintf (stderr,
                 "Preserving PCB element not in the schematic:    %s (element   %s)\n",
                 el->refdes, el->description);
      } else
        ++n_deleted;
    } else if (el->changed_value)
      ++n_changed_value;
  }
  if (!pcb_element_list
      || (n_deleted == 0 && !need_PKG_purge && n_changed_value == 0)
    )
    return;
  if ((f_in = fopen (pcb_file, "r")) == NULL)
    return;
  tmp = g_strconcat (pcb_file, ".tmp", NULL);
  if ((f_out = fopen (tmp, "wb")) == NULL) {
    fclose (f_in);
    return;
  }
  while ((fgets (buf, sizeof (buf), f_in)) != NULL) {
    for (s = buf; *s == ' ' || *s == '\t'; ++s);
    if (skipping) {
      if (*s == '(')
        ++paren_level;
      else if (*s == ')' && --paren_level <= 0)
        skipping = FALSE;
      continue;
    }
    el_exists = NULL;
    if ((el = pcb_element_line_parse (s)) != NULL
        && (el_exists = pcb_element_exists (el, FALSE)) != NULL
        && !el_exists->still_exists && !preserve) {
      skipping = TRUE;
      if (verbose)
        printf ("%s: deleted element %s (value=%s)\n",
                el->refdes, el->description, el->value);
      pcb_element_free (el);
      continue;
    }
    if (el_exists && el_exists->changed_value) {
      fmt = (gchar*) (el->quoted_flags ?
                      "Element%c\"%s\" \"%s\" \"%s\" \"%s\" %s %s%s\n" :
                      "Element%c%s \"%s\" \"%s\" \"%s\" %s %s%s\n");
      fprintf (f_out, fmt,
               el->res_char, el->flags, el->description, el->refdes,
               el_exists->changed_value, el->x, el->y, el->tail);
      if (verbose)
        printf ("%s: changed element %s value: %s -> %s\n",
                el->refdes, el->description,
                el->value, el_exists->changed_value);
    } else if (!strncmp (s, "PKG_", 4))
      ++n_PKG_removed_old;
    else
      fputs (buf, f_out);
    pcb_element_free (el);
  }
  fclose (f_in);
  fclose (f_out);

  if (!bak_done) {
    build_and_run_command ("mv %s %s", pcb_file, bak);
    bak_done = TRUE;
  }

  build_and_run_command ("mv %s %s", tmp, pcb_file);
  g_free (tmp);
}


GList*
sch2pcb_parse_schematics (char *str)
{
  /* parse the string using shell semantics */
  gint count;
  gchar** args = NULL;
  GError* error = NULL;
  GList *result = NULL;

  if (g_shell_parse_argv (str, &count, &args, &error))
  {
    int i;
    for (i = 0; i < count; ++i)
    {
      result = g_list_append (result, g_strdup (args[i]));
    }
    g_strfreev (args);
  } else {
    fprintf (stderr,
             "invalid `schematics' option: %s\n",
             error->message);
    g_error_free (error);
  }

  return result;
}
