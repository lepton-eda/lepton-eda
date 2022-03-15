/*! \file glib_compat.h */

#ifndef GLIB_COMPAT_H
#define GLIB_COMPAT_H

#include <glib.h>

G_BEGIN_DECLS

/* Obsolete Glib compatibility stuff.  For more information, please see
https://discourse.gnome.org/t/port-your-module-from-g-memdup-to-g-memdup2-now/5538
*/
#if !GLIB_CHECK_VERSION(2, 68, 0)
inline gpointer
g_memdup2(gconstpointer mem, gsize byte_size)
{
  gpointer new_mem;

  if (mem && byte_size != 0) {
      new_mem = g_malloc(byte_size);
      memcpy(new_mem, mem, byte_size);
  }
  else
    new_mem = NULL;

  return new_mem;
}
#endif

G_END_DECLS

#endif /* GLIB_COMPAT_H */
