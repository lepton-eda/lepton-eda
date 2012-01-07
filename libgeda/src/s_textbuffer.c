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
#include <glib.h>

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

#include "libgeda_priv.h"

struct _TextBuffer
{
  const gchar *buffer;
  gsize size;

  gchar *line;
  gsize linesize;

  gsize offset;
};

#define TEXT_BUFFER_LINE_SIZE 1024

/*! \brief Create a new managed text buffer.
 *
 *  \par Function description 
 *  Allocates and initialises a new TextBuffer to manage the given data
 *  buffer.
 *
 *  If the size argument is negative, assumes that data is
 *  null-terminated.
 *
 *  \param data The address of the buffer to be managed.
 *  \param size The length of the buffer.
 *  \retval Pointer to a new TextBuffer struct.
 */
TextBuffer *s_textbuffer_new (const gchar *data, const gint size)
{
  TextBuffer *result;
  gsize realsize;

  g_return_val_if_fail ((data != NULL),
                        NULL);

  if (size < 0)
    realsize = strlen(data);
  else
    realsize = size;

  result = g_new0(TextBuffer, 1);

  result->buffer = data;
  result->size = realsize;

  result->linesize = TEXT_BUFFER_LINE_SIZE;
  result->line = g_malloc(result->linesize);

  return result;
}

/*! \brief Clean up a managed text buffer
 *
 *  \par Function description
 *  Cleans up all of the resources associated with a given TextBuffer.
 *
 *  Should be called thus:
 *
 *  \code
 *  tb = s_textbuffer_free (tb);
 *  \endcode
 */
TextBuffer *s_textbuffer_free (TextBuffer *tb)
{
  if (tb == NULL) return NULL;

  g_free (tb->line);
  tb->line = NULL;
  g_free (tb);
  return NULL;
}

/*! \brief Fetch a number of characters from a text buffer
 *
 *  \par Function description
 *  Get some number of characters from a TextBuffer, starting at the
 *  current position.  If the end of the buffer has been reached (and
 *  thus no more characters remain) returns null.  If \a count is -1,
 *  obtains all characters up to and including the next newline.
 *
 *  A newline is detected as '\n', or '\r' together with its
 *  immediately following '\n', or '\r', in that order.  All newlines
 *  are collapsed into a single '\n'.
 *
 *  The returned character array should be considered highly volatile,
 *  and is only valid until the next call to s_textbuffer_next() or
 *  s_textbuffer_next_line().
 *
 *  \param tb    TextBuffer to read from.
 *  \param count Maximum number of characters to read.
 *  \retval      Character array, or NULL if no characters left.
 */
const gchar *
s_textbuffer_next (TextBuffer *tb, const gssize count)
{
  gboolean eol = FALSE;
  gchar c;
  gsize len;

  g_return_val_if_fail (tb != NULL, NULL);

  if (tb->offset >= tb->size) return NULL;

  const gchar *src = tb->buffer + tb->offset;
  gchar *dest = tb->line;
  const gchar *buf_end = tb->buffer + tb->size;

  while (1) {
    if (src >= buf_end) break;
    if (count >= 0 && dest - tb->line >= count) break;
    if (count < 0 && eol) break;

    /* Expand line buffer, if necessary, leaving space for a null */
    len = dest - tb->line + 2;
    if (len >= tb->linesize) {
      tb->linesize += TEXT_BUFFER_LINE_SIZE;
      tb->line = g_realloc(tb->line, tb->linesize);
    }

    eol = FALSE;
    c = *src;
    if (c == '\n') {
      *dest = '\n';
      eol = TRUE;
    } else if (c == '\r') {
      *dest = '\n';
      eol = TRUE;
      /* Peek ahead to absorb a '\n' */
      src++;
      if (src >= buf_end || *src != '\n') src--;
    } else {
      *dest = c;
    }

    src++;
    dest++;
  }

  *dest = 0;
  tb->offset = src - tb->buffer;

  return tb->line;
}
/*! \brief Fetch the next line from a text buffer
 *
 *  \par Function description
 *  Get the next line of characters from a TextBuffer, starting from
 *  the current position.  If the end of the buffer has been reached
 *  (and thus no more characters remain) returns null.
 *
 *  The returned character array should be considered highly volatile,
 *  and is only valid until the next call to s_textbuffer_next() or
 *  s_textbuffer_next_line().
 *
 *  \param tb    TextBuffer to read from.
 *  \retval      Character array, or NULL if no characters left.
 */
const gchar *
s_textbuffer_next_line (TextBuffer *tb)
{
  return s_textbuffer_next (tb, -1);
}
