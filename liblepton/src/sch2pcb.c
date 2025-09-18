/* gsch2pcb
 * Copyright (C) 2003-2016 gEDA Contributors
 * Copyright (C) 2017-2025 Lepton EDA Contributors
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

PcbElement*
pcb_element_new ()
{
  return g_new0 (PcbElement, 1);
}

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


gchar*
pcb_element_line_token (gchar *string,
                        gchar **next,
                        gboolean *quoted_ret)
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
    pcb_element_set_hi_res_format (el, TRUE);
  else if (*s != '(') {
    g_free (el);
    return NULL;
  }

  pcb_element_set_res_char (el, pcb_element_get_hi_res_format (el) ? '[' : '(');
  close_char = pcb_element_get_hi_res_format (el) ? ']' : ')';

  gboolean quoted_flags;
  pcb_element_set_flags (el, pcb_element_line_token (s + 1, NULL, &quoted_flags));
  pcb_element_set_quoted_flags (el, quoted_flags);
  pcb_element_set_description (el, pcb_element_line_token (NULL, NULL, NULL));
  pcb_element_set_refdes (el, pcb_element_line_token (NULL, NULL, NULL));
  pcb_element_set_value (el, pcb_element_line_token (NULL, NULL, NULL));

  pcb_element_set_x (el, pcb_element_line_token (NULL, NULL, NULL));
  pcb_element_set_y (el, pcb_element_line_token (NULL, &t, NULL));

  pcb_element_set_tail (el, g_strdup (t ? t : ""));
  if ((s = strrchr (pcb_element_get_tail (el), (gint) '\n')) != NULL)
    *s = '\0';

  /* Count the tokens in tail to decide if it's new or old format.
   * Old format will have 3 tokens, new format will have 5 tokens.
   */
  for (s = pcb_element_get_tail (el); *s && *s != close_char; ++s) {
    if (*s != ' ') {
      if (state == 0)
        ++elcount;
      state = 1;
    } else
      state = 0;
  }
  if (elcount > 4)
    pcb_element_set_new_format (el, TRUE);

  fix_spaces (pcb_element_get_description (el));
  fix_spaces (pcb_element_get_refdes (el));
  fix_spaces (pcb_element_get_value (el));

  /* Don't allow elements with no refdes to ever be deleted because
   * they may be desired pc board elements not in schematics.  So
   * initialize still_exists to TRUE if empty or non-alphanumeric
   * refdes.
   */
  if (!*pcb_element_get_refdes (el)
      || !isalnum ((gint) (*pcb_element_get_refdes (el))))
    pcb_element_set_still_exists (el, TRUE);

  return el;
}

void
pcb_element_free (PcbElement * el)
{
  if (!el)
    return;
  g_free (pcb_element_get_flags (el));
  g_free (pcb_element_get_description (el));
  g_free (pcb_element_get_changed_description (el));
  g_free (pcb_element_get_changed_value (el));
  g_free (pcb_element_get_refdes (el));
  g_free (pcb_element_get_value (el));
  g_free (pcb_element_get_x (el));
  g_free (pcb_element_get_y (el));
  g_free (pcb_element_get_tail (el));
  g_free (pcb_element_get_pkg_name_fix (el));
  g_free (el);
}


void
sch2pcb_pcb_element_list_append (PcbElement *element)
{
  pcb_element_list = g_list_append (pcb_element_list, element);
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
  if (sch2pcb_get_verbose_mode () > 1)
    printf ("\t  Searching: \"%s\" for \"%s\"\n", dir_path, element);
  while ((name = (gchar *) g_dir_read_name (dir)) != NULL) {
    path = g_strconcat (dir_path, "/", name, NULL);
    found = NULL;

    /* if we got a directory name, then recurse down into it */
    if (g_file_test (path, G_FILE_TEST_IS_DIR))
      found = sch2pcb_find_element (path, element);

    /* otherwise assume it is a file and see if it is the one we want */
    else {
      if (sch2pcb_get_verbose_mode () > 1)
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
      if (sch2pcb_get_verbose_mode () > 1)
        printf ("%s\n", found ? "Yes" : "No");
    }
    g_free (path);
    if (found)
      break;
  }
  g_dir_close (dir);
  return found;
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
