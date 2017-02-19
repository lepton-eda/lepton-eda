/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library - Scheme API
 * Copyright (C) 2013-2014 Peter Brett <peter@peter-b.co.uk>
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

#include "libgeda_priv.h"
#include "libgedaguile_priv.h"

enum {
  PROP_HOOK = 1,
};

/*! \private \memberof EdascmHookProxy
 * Private data for hook proxy. */
struct _EdascmHookProxyPrivate
{
  /*! Closure around the EdascmHookProxy that's called when the hook is
   * run */
  SCM closure;
  /*! The hook that the EdascmHookProxy is a proxy for */
  SCM hook;
};

static void edascm_hook_proxy_finalize (GObject *object);
static void edascm_hook_proxy_set_property (GObject *object, guint property_id,
                                            const GValue *value, GParamSpec *pspec);
static void edascm_hook_proxy_get_property (GObject *object, guint property_id,
                                             GValue *value, GParamSpec *pspec);
static void edascm_hook_proxy_default_run_handler (EdascmHookProxy *proxy,
                                                   SCM args);
static SCM edascm_hook_proxy_closure (SCM args, gpointer user_data);
static void edascm_hook_proxy_connect (EdascmHookProxy *proxy, SCM hook);
static void edascm_hook_proxy_disconnect (EdascmHookProxy *proxy);
static void cclosure_marshal_VOID__SCM (GClosure *closure,
                                        GValue *return_value,
                                        guint n_param_values,
                                        const GValue *param_values,
                                        gpointer invocation_hint,
                                        gpointer marshal_data);

G_DEFINE_TYPE (EdascmHookProxy, edascm_hook_proxy, G_TYPE_OBJECT);

/*! Initialise EdascmHookProxy class. */
static void
edascm_hook_proxy_class_init (EdascmHookProxyClass *klass)
{
  GObjectClass *gobject_class = G_OBJECT_CLASS (klass);
  GParamFlags param_flags;

  g_type_class_add_private (gobject_class, sizeof (EdascmHookProxyPrivate));

  /* Register functions with base class */
  gobject_class->finalize = edascm_hook_proxy_finalize;
  gobject_class->set_property = edascm_hook_proxy_set_property;
  gobject_class->get_property = edascm_hook_proxy_get_property;

  klass->run = edascm_hook_proxy_default_run_handler;

  /* Install properties */
  param_flags = (G_PARAM_READWRITE | G_PARAM_STATIC_NAME | G_PARAM_STATIC_NICK |
                 G_PARAM_STATIC_BLURB);

  g_object_class_install_property (gobject_class, PROP_HOOK,
                                   edascm_param_spec_scm ("hook",
                                                          _("Scheme hook"),
                                                          _("The Scheme-level hook to proxy"),
                                                          param_flags));

  /* Create signals */
  g_signal_new ("run", /* signal name */
                G_TYPE_FROM_CLASS (gobject_class), /* type */
                G_SIGNAL_RUN_FIRST, /* flags */
                G_STRUCT_OFFSET(EdascmHookProxyClass, run), /* class offset */
                NULL, /* accumulator */
                NULL, /* accumulator data */
                cclosure_marshal_VOID__SCM, /* c_marshaller */
                G_TYPE_NONE, /* return type */
                1, /* no. of params */
                EDASCM_TYPE_SCM);
}

/*! Initialise EdascmHookProxy instance. */
static void
edascm_hook_proxy_init (EdascmHookProxy *proxy)
{
  SCM proc = SCM_BOOL_F;

  proxy->priv = G_TYPE_INSTANCE_GET_PRIVATE (proxy,
                                             EDASCM_TYPE_HOOK_PROXY,
                                             EdascmHookProxyPrivate);

  proxy->priv->hook = SCM_UNDEFINED;
  proxy->priv->closure = SCM_UNDEFINED;

  /* Try and create our internal closure */
  proc = edascm_c_make_closure (edascm_hook_proxy_closure, proxy);
  g_return_if_fail (scm_is_true (scm_procedure_p (proc)));
  proxy->priv->closure = scm_gc_protect_object (proc);
}

/*! Finalize an EdascmHookProxy instance. Free all resources held by
 * the instance. */
static void
edascm_hook_proxy_finalize (GObject *object)
{
  EdascmHookProxy *proxy = EDASCM_HOOK_PROXY (object);

  edascm_hook_proxy_disconnect (proxy);
  if (edascm_is_defined (proxy->priv->closure)) {
    scm_gc_unprotect_object (proxy->priv->closure);
  }

  /* Chain up to the parent class */
  G_OBJECT_CLASS (edascm_hook_proxy_parent_class)->finalize (object);
}

/*! Set a property of an EdascmHookProxy instance. */
static void
edascm_hook_proxy_set_property (GObject *object, guint property_id,
                                const GValue *value, GParamSpec *pspec)
{
  EdascmHookProxy *proxy = EDASCM_HOOK_PROXY (object);
  SCM hook;

  switch (property_id) {

  case PROP_HOOK:
    hook = edascm_value_get_scm (value);
    if (!edascm_is_defined (hook)) {
      edascm_hook_proxy_disconnect (proxy);
    } else {
      edascm_hook_proxy_connect (proxy, hook);
    }
    break;

  default:
    G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
    break;
  }
}

/*! Get a property of an EdascmHookProxy instance. */
static void
edascm_hook_proxy_get_property (GObject *object, guint property_id,
                                GValue *value, GParamSpec *pspec)
{
  EdascmHookProxy *proxy = EDASCM_HOOK_PROXY (object);

  switch (property_id) {

  case PROP_HOOK:
    edascm_value_set_scm (value, proxy->priv->hook);
    break;

  default:
    G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
    break;    
  }
}

/*! Connect up an EdascmHookProxy to a new target hook. */
static void
edascm_hook_proxy_connect (EdascmHookProxy *proxy, SCM hook)
{
  g_return_if_fail (EDASCM_IS_HOOK_PROXY (proxy));
  g_return_if_fail (SCM_HOOKP (hook));
  g_return_if_fail (scm_is_true (scm_procedure_p (proxy->priv->closure)));

  if (edascm_is_defined (proxy->priv->hook))
    edascm_hook_proxy_disconnect (proxy);

  proxy->priv->hook = hook;
  scm_gc_protect_object (hook);
  /* \bug What if scm_add_hook_x() fails? */
  scm_add_hook_x (hook, proxy->priv->closure, SCM_UNDEFINED);
}

/*! Disconnect an EdascmHookProxy from its current target hook. */
static void
edascm_hook_proxy_disconnect (EdascmHookProxy *proxy)
{
  g_return_if_fail (EDASCM_IS_HOOK_PROXY (proxy));
  g_return_if_fail (scm_is_true (scm_procedure_p (proxy->priv->closure)));

  if (!edascm_is_defined (proxy->priv->hook)) return;

  /* \bug What if scm_remove_hook_x() fails? */
  scm_remove_hook_x (proxy->priv->hook, proxy->priv->closure);
  scm_gc_unprotect_object (proxy->priv->hook);
  proxy->priv->hook = SCM_UNDEFINED;
}

/*! Emit a signal on an EdascmHookProxy whenever its target hook is run. */
static SCM
edascm_hook_proxy_closure (SCM args, gpointer user_data) {
  g_signal_emit_by_name (user_data,
                         "run",
                         SCM_UNPACK (args));
  return SCM_UNSPECIFIED;
}

/*! \brief Default handler for run signals.
 *
 * Does nothing (but provides a useful example of how to write a "run"
 * signal handler).
 *
 * \param proxy  Hook proxy object.
 * \param args   Hook arguments (as unpacked Scheme value).
 */
static void
edascm_hook_proxy_default_run_handler (EdascmHookProxy *proxy,
                                       SCM args)
{
  g_return_if_fail (EDASCM_IS_HOOK_PROXY (proxy));

  /* Do the most basic of sanity checking on the argument list! */
  g_return_if_fail (scm_is_true (scm_list_p (args)));
}

/*! \brief Callback marshal function for run signals.
 * \par Function Description
 * Based heavily on g_cclosure_marshal_VOID__STRING() from GObject.
 */
static void
cclosure_marshal_VOID__SCM (GClosure *closure,
                            GValue *return_value,
                            guint n_param_values,
                            const GValue *param_values,
                            gpointer invocation_hint,
                            gpointer marshal_data)
{
  typedef void (*MarshalFunc_VOID__SCM) (gpointer data1,
                                         SCM arg_1,
                                         gpointer data2);
  register MarshalFunc_VOID__SCM callback;
  register GCClosure *cc = (GCClosure *) closure;
  register gpointer data1, data2;

  g_return_if_fail (n_param_values == 2);
  if (G_CCLOSURE_SWAP_DATA (closure)) {
    data1 = closure->data;
    data2 = g_value_peek_pointer (param_values + 0);
  } else {
    data1 = g_value_peek_pointer (param_values + 0);
    data2 = closure->data;
  }
  callback = (MarshalFunc_VOID__SCM) (marshal_data ? marshal_data : cc->callback);

  callback (data1,
            edascm_value_get_scm (param_values + 1),
            data2);
}

/* ---------------------------------------------------------------- */

/*! \public \memberof EdascmHookProxy
 * \brief Create a new hook proxy.
 *
 * Create a new hook proxy for the Scheme-level hook \a hook_s.
 *
 * \param hook_s  Hook to be proxied.
 * \return a new #EdascmHookProxy instance.
 */
EdascmHookProxy *
edascm_hook_proxy_new_with_hook (SCM hook_s)
{
  return g_object_new (EDASCM_TYPE_HOOK_PROXY,
                       "hook", hook_s,
                       NULL);
}
