/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library - Scheme API
 * Copyright (C) 2013 Peter Brett <peter@peter-b.co.uk>
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

#ifndef __EDASCM_HOOK_PROXY_H__
#define __EDASCM_HOOK_PROXY_H__

G_BEGIN_DECLS

/* ---------------------------------------------------------------- */

/*! \class EdascmHookProxy edascmhookproxy.h "libgeda/edascmhookproxy.h"
 * \ingroup guile_c_iface
 * \brief Object that connects a Guile Scheme level hook to GObject signals.
 *
 * Sometimes, it is useful to be able to manipulate GObjects (for
 * example, GTK+ UI elements) when a Guile Scheme-level hook is run.
 * Unfortunately, this can be tricky to do directly, not least since
 * the <tt>scm_add_hook_x()</tt> procedure requires a closure as an argument.
 *
 * The #EdascmHookProxy makes things much easier by allowing GObjects
 * to simply connect to its \b "run" signal in order to receive a
 * callback whenever the target hook is run.
 *
 * When implementing a \b "run" signal handler, it is important to note
 * that the hook's argument list is passed to the handler as a #gulong
 * and must be packed (with <tt>SCM_PACK()</tt>) before being used with any
 * libguile functions.  For example:
 *
 * \code
 * void
 * my_handler (EdascmHookProxy *proxy, gulong unpacked_args, gpointer user_data)
 * {
 *   SCM args = SCM_PACK (unpacked_args);
 *   // ... //
 * }
 * \endcode
 *
 * The opposite operation to <tt>SCM_PACK()</tt> is <tt>SCM_UNPACK()</tt>.
 *
 * \b Signals: One signal, \b "run".  Called with one parameter, the
 * Scheme argument list passed to the handler as an unpacked Scheme
 * value.
 *
 * \b Properties: One property, \b "hook", the hook to proxy as an
 * unpacked Scheme value.
 *
 * \since 1.10.
 */

#define EDASCM_TYPE_HOOK_PROXY (edascm_hook_proxy_get_type ())
#define EDASCM_HOOK_PROXY(obj) (G_TYPE_CHECK_INSTANCE_CAST ((obj), EDASCM_TYPE_HOOK_PROXY, EdascmHookProxy))
#define EDASCM_HOOK_PROXY_CLASS(klass) (G_TYPE_CHECK_CLASS_CAST ((klass), EDASCM_TYPE_HOOK_PROXY, EdascmHookProxyClass))
#define EDASCM_IS_HOOK_PROXY(obj) (G_TYPE_CHECK_INSTANCE_TYPE ((obj), EDASCM_TYPE_HOOK_PROXY))
#define EDASCM_HOOK_PROXY_GET_CLASS(obj) (G_TYPE_INSTANCE_GET_CLASS ((obj), EDASCM_TYPE_HOOK_PROXY, EdascmHookProxy))

typedef struct _EdascmHookProxyClass EdascmHookProxyClass;
typedef struct _EdascmHookProxy EdascmHookProxy;
typedef struct _EdascmHookProxyPrivate EdascmHookProxyPrivate;

struct _EdascmHookProxyClass
{
  GObjectClass parent_class;

  /* signals */
  void (*run)(EdascmHookProxy *proxy, gulong unpacked_args);
};

struct _EdascmHookProxy
{
  GObject parent_instance;

  /* Private members */
  EdascmHookProxyPrivate *priv;
};

GType edascm_hook_proxy_get_type (void) G_GNUC_CONST;

EdascmHookProxy *edascm_hook_proxy_new_with_hook (SCM hook_s) G_GNUC_WARN_UNUSED_RESULT;

G_END_DECLS

#endif /* ! __EDASCM_HOOK_PROXY_H__ */
