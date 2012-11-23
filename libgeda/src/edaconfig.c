/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's Library
 * Copyright (C) 2011-2012 gEDA Contributors (see ChangeLog for details)
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

#include <errno.h>
#include <glib.h>

#include <glib-object.h>
#include <libgeda_priv.h>

enum _EdaConfigPropertyId {
  PROP_0,
  PROP_CONFIG_FILENAME,
  PROP_CONFIG_PARENT,
  PROP_CONFIG_TRUSTED,
};

/*! \private \memberof EdaConfig
 * Private data for configuration context. */
struct _EdaConfigPrivate
{
  /* Accessed via properties */
  EdaConfig *parent;
  gulong parent_handler_id;
  gboolean trusted;
  gchar *filename;

  /* Other private data */
  GKeyFile *keyfile;
  gboolean loaded;
  gboolean changed;
};

/*! Default value for XDG_CONFIG_DIRS if no system configuration
 * found. */
#define XDG_CONFIG_DIRS_DEFAULT "/etc/xdg/"
/*! Subdirectory of XDG directories (config, data, cache etc.) to check
 * for gEDA files. */
#define XDG_SUBDIR "gEDA"
/*! Filename for gEDA system configuration files */
#define SYSTEM_CONFIG_NAME "geda-system.conf"
/*! Filename for gEDA user configuration files */
#define USER_CONFIG_NAME "geda-user.conf"
/*! Filename for gEDA local configuration files */
#define LOCAL_CONFIG_NAME "geda.conf"

static void eda_config_dispose (GObject *object);
static void eda_config_finalize (GObject *object);
static void eda_config_set_property (GObject *object, guint property_id, const GValue *value, GParamSpec *pspec);
static void eda_config_get_property (GObject *object, guint property_id, GValue *value, GParamSpec *pspec);
static gboolean eda_config_is_descendent (EdaConfig *cfg, EdaConfig *parent);

static void cclosure_marshal_VOID__STRING_STRING (GClosure *closure,
                                                  GValue *return_value,
                                                  guint n_param_values,
                                                  const GValue *param_values,
                                                  gpointer invocation_hint,
                                                  gpointer marshal_data);
static void default_config_changed_handler (EdaConfig *cfg, const gchar *group, const gchar *key);
static void parent_config_changed_handler (EdaConfig *parent, const gchar *group, const gchar* key, EdaConfig *cfg);
static void propagate_key_file_error (GError *src, GError **dest);

/*! Magic helpful GObject macro */
G_DEFINE_TYPE (EdaConfig, eda_config, G_TYPE_OBJECT);

/*! Initialise EdaConfig class. */
static void
eda_config_class_init (EdaConfigClass *klass)
{
  GObjectClass *gobject_class = G_OBJECT_CLASS (klass);
  GParamSpec *pspec;

  g_type_class_add_private (gobject_class, sizeof (EdaConfigPrivate));

  /* Register functions with base class */
  gobject_class->dispose = eda_config_dispose;
  gobject_class->finalize = eda_config_finalize;
  gobject_class->set_property = eda_config_set_property;
  gobject_class->get_property = eda_config_get_property;

  klass->config_changed = default_config_changed_handler;

  /* Register properties */
  pspec = g_param_spec_string ("filename",
                               "Configuration file name",
                               "Set underlying file for EdaConfig",
                               NULL /* default value */,
                               G_PARAM_CONSTRUCT_ONLY | G_PARAM_READWRITE);
  g_object_class_install_property (gobject_class,
                                   PROP_CONFIG_FILENAME,
                                   pspec);

  pspec = g_param_spec_object ("parent",
                               "Configuration context parent",
                               "Set parent configuration context for EdaConfig",
                               EDA_TYPE_CONFIG,
                               G_PARAM_CONSTRUCT | G_PARAM_READWRITE);
  g_object_class_install_property (gobject_class,
                                   PROP_CONFIG_PARENT,
                                   pspec);

  pspec = g_param_spec_boolean ("trusted",
                                "Whether context is trusted",
                                "Set whether configuration context is trusted config source.",
                                FALSE /* default value */,
                                G_PARAM_CONSTRUCT | G_PARAM_READWRITE);
  g_object_class_install_property (gobject_class,
                                   PROP_CONFIG_TRUSTED,
                                   pspec);

  /* Create signals */
  g_signal_new ("config-changed", /* signal name */
                G_TYPE_FROM_CLASS (gobject_class), /* type */
                G_SIGNAL_RUN_FIRST | G_SIGNAL_NO_RECURSE | G_SIGNAL_NO_HOOKS, /* flags */
                G_STRUCT_OFFSET(EdaConfigClass, config_changed), /* class offset */
                NULL, /* accumulator */
                NULL, /* accumulator data */
                cclosure_marshal_VOID__STRING_STRING, /* c_marshaller */
                G_TYPE_NONE, /* return type */
                2, /* no. of params */
                G_TYPE_STRING, G_TYPE_STRING);
}

/*! Initialise EdaConfig instance. */
static void
eda_config_init (EdaConfig *config)
{
  config->priv = G_TYPE_INSTANCE_GET_PRIVATE (config,
                                              EDA_TYPE_CONFIG,
                                              EdaConfigPrivate);

  config->priv->parent = NULL;
  config->priv->keyfile = g_key_file_new ();
  config->priv->loaded = FALSE;
  config->priv->changed = FALSE;
  config->priv->parent_handler_id = 0;
}

/*! Dispose of an EdaConfig instance. Drop all references to other
 * GObjects, but keep the instance otherwise intact. May be run multiple
 * times (due to reference loops).
 */
static void
eda_config_dispose (GObject *object)
{
  EdaConfig *config = EDA_CONFIG (object);

  g_object_set (object,
                "parent", NULL,
                NULL);

  /* Chain up to the parent class */
  G_OBJECT_CLASS (eda_config_parent_class)->dispose (object);
}

/*! Finalize an EdaConfig instance. Free all resources held by the
 * instance. */
static void
eda_config_finalize (GObject *object)
{
  EdaConfig *config = EDA_CONFIG (object);

  g_free (config->priv->filename);
  g_key_file_free (config->priv->keyfile);

  /* Chain up to the parent class */
  G_OBJECT_CLASS (eda_config_parent_class)->finalize (object);
}

/*! Set a property of an EdaConfig instance. */
static void
eda_config_set_property (GObject *object, guint property_id,
                         const GValue *value, GParamSpec *pspec)
{
  EdaConfig *config = EDA_CONFIG (object);
  EdaConfig *parent;
  EdaConfigPrivate *priv = config->priv;
  switch (property_id) {
  case PROP_CONFIG_FILENAME:
    g_free (config->priv->filename);
    config->priv->filename = g_value_dup_string (value);
    break;

  case PROP_CONFIG_PARENT:
    /* Check if new parent is a child context of config (loops are not
     * permitted). */
    parent = g_value_get_object (value);
    if (parent != NULL) {
      g_return_if_fail (EDA_IS_CONFIG (parent));
      g_return_if_fail (!eda_config_is_descendent (parent, config));
    }

    if (priv->parent != NULL) {
      /* Disconnect parent signal handler, if still connected. */
      if (g_signal_handler_is_connected (priv->parent,
                                         priv->parent_handler_id)) {
        g_signal_handler_disconnect (priv->parent,
                                     priv->parent_handler_id);
      }
      g_object_unref (priv->parent);
      priv->parent_handler_id = 0;
    }
    if (parent != NULL) {
      config->priv->parent = g_object_ref (parent);
      /* Connect signal handler to new parent. */
      priv->parent_handler_id =
        g_signal_connect_object (parent,
                                 "config-changed",
                                 (GCallback) parent_config_changed_handler,
                                 config,
                                 G_CONNECT_SWAPPED);
    } else {
      config->priv->parent = NULL;
    }
    break;

  case PROP_CONFIG_TRUSTED:
    config->priv->trusted = g_value_get_boolean (value);
    break;

  default:
    G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
    break;
  }
}

/*! Get a property of an EdaConfig instance. */
static void
eda_config_get_property (GObject *object, guint property_id,
                         GValue *value, GParamSpec *pspec)
{
  EdaConfig *config = EDA_CONFIG (object);
  switch (property_id) {
  case PROP_CONFIG_FILENAME:
    g_value_set_string (value, config->priv->filename);
    break;
  case PROP_CONFIG_PARENT:
    g_value_set_object (value, config->priv->parent);
    break;
  case PROP_CONFIG_TRUSTED:
    g_value_set_boolean (value, config->priv->trusted);
    break;
  default:
    G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
    break;
  }
}

/*! \public \memberof EdaConfig
 * \brief Return the default configuration context.
 *
 * The default context is not associated with any physical path or
 * on-disk configuration file, and has no parent context.  It contains
 * the default configuration used when no configuration file can be
 * loaded.
 *
 * Applications should normally populate the default context with
 * their built-in default configuration settings on start-up, before
 * loading any further configuration files.
 *
 * \return the default #EdaConfig configuration context.
 */
EdaConfig *
eda_config_get_default_context ()
{
  static EdaConfig *config = NULL;
  if (config == NULL) {
    config = g_object_new (EDA_TYPE_CONFIG,
                           "trusted", TRUE,
                           NULL);
    config->priv->loaded = TRUE;
  }
  return config;
}

/*! \public \memberof EdaConfig
 * \brief Return the system configuration context.
 *
 * The system context is used for system-wide configuration.  It is
 * located:
 *
 * -# By searching "${XDG_CONFIG_DIRS}" for a "gEDA/geda-system.conf"
 *    configuration file.
 * -# By checking "${sysconfdir}/gEDA" for a configuration file.
 *
 * Its parent context is the default context.
 *
 * \return the system #EdaConfig configuration context.
 */
EdaConfig *
eda_config_get_system_context ()
{
  static EdaConfig *config = NULL;
  if (config == NULL) {
    gchar *filename = NULL;
    const gchar * const *xdg_dirs;
    int i;

    /* Search for a system configuration file in XDG_CONFIG_DIRS */
    xdg_dirs = g_get_system_config_dirs ();
    for (i = 0; xdg_dirs[i] != NULL; i++) {
      filename = g_build_filename (xdg_dirs[i], XDG_SUBDIR,
                                   SYSTEM_CONFIG_NAME, NULL);
      if (g_file_test (filename, G_FILE_TEST_EXISTS)) {
        break;
      }

      g_free (filename);
      filename = NULL;
    }

    /* If we didn't find a configuration file, just use a filename in
     * the traditional location. */
    if (filename == NULL) {
      filename = g_build_filename (s_path_sys_config (),
                                   SYSTEM_CONFIG_NAME, NULL);
      if (!g_file_test (filename, G_FILE_TEST_EXISTS)) {
        g_free (filename);
        filename = NULL;
      }
    }

    /* Finally, fall back to default XDG location */
    if (filename == NULL) {
      if (xdg_dirs[0] != NULL) {
        filename = g_build_filename (xdg_dirs[0], XDG_SUBDIR,
                                     SYSTEM_CONFIG_NAME, NULL);
      } else {
        filename = g_build_filename (XDG_CONFIG_DIRS_DEFAULT, XDG_SUBDIR,
                                     SYSTEM_CONFIG_NAME, NULL);
      }
    }

    g_return_val_if_fail (filename != NULL, NULL);

    config = g_object_new (EDA_TYPE_CONFIG,
                           "filename", filename,
                           "parent", eda_config_get_default_context (),
                           "trusted", TRUE,
                           NULL);
    g_free (filename);
  }
  return config;
}

/*! \public \memberof EdaConfig
 * \brief Return the user configuration context.
 *
 * The user context is used for user-specific configuration, and is
 * loaded from "${XDG_CONFIG_HOME}/gEDA/geda-user.conf". Its parent
 * context is the system context.
 *
 * \return the user #EdaConfig configuration context.
 */
EdaConfig *
eda_config_get_user_context ()
{
  static EdaConfig *config = NULL;
  if (config == NULL) {
    gchar *filename = NULL;

    /* Search for a user configuration file in XDG_CONFIG_HOME */
    filename = g_build_filename (g_get_user_config_dir (), XDG_SUBDIR,
                                 USER_CONFIG_NAME, NULL);

    config = g_object_new (EDA_TYPE_CONFIG,
                           "filename", filename,
                           "parent", eda_config_get_system_context (),
                           "trusted", TRUE,
                           NULL);

    g_free (filename);
  }
  return config;
}

/*! Change directory to \a filename, and return the current working
 * directory as a newly-allocated string.  If an error occurs, returns
 * NULL and sets \a error. */
static gchar *
chdir_get_current_dir (const gchar *filename, GError **error)
{
  if (g_chdir (filename) != 0) {
      g_set_error (error, G_FILE_ERROR,
                   g_file_error_from_errno (errno),
                   _("Could not change directory to '%s': %s"),
                   filename, g_strerror (errno));
      return NULL;
  }
  return g_get_current_dir ();
}

/*! Recursively searches upwards from \a path, looking for a
 * "geda.conf" file.  If the root directory is reached without finding
 * a configuration file, returns the directory part of \a path (if \a
 * path points to a regular file) or \a path itself (if \a path is a
 * directory).  If an unrecoverable error occurs, returns NULL and
 * sets \a error.
 *
 * \todo find_project_root() is probably generally useful. */
static gchar *
find_project_root (const gchar *path, GError **error)
{
  gchar *dir = NULL;
  gchar *next_dir = NULL;
  gchar *save_cwd = NULL;
  gchar *result = NULL;
  GError *tmp_err = NULL;

  /* Save the current directory. We'll try and get back here when
   * we're done. */
  save_cwd = g_get_current_dir ();

  /* First, try to change directory to the requested path.  This
   * allows us to check that it exists and is a directory, and to
   * normalise the filename all at once. If it's not a directory, we
   * recurse for its containing directory.  Any other errors cause
   * failure.*/
  dir = chdir_get_current_dir (path, &tmp_err);
  if (dir == NULL) {
    if (tmp_err->code == G_FILE_ERROR_NOTDIR) {
      g_error_free (tmp_err);
      tmp_err = NULL;
      next_dir = g_path_get_dirname (path);
      result = find_project_root (next_dir, error);
    }
    goto project_root_done;
  }

  while (1) {

    /* Check if the current working directory contains a project
     * config file. If so, hurrah! We've found the project root. */
    if (g_file_test (LOCAL_CONFIG_NAME, G_FILE_TEST_EXISTS)) {
      result = dir;
      dir = NULL;
      break;
    }

    /* Try to go to the parent directory */
    next_dir = chdir_get_current_dir ("..", &tmp_err);
    if (next_dir == NULL) goto project_root_done;

    /* We were already at the root directory, give up */
    if (strcmp (next_dir, dir) == 0) {
      result = g_strdup (path);
      break;
    }
    g_free (dir);
    dir = next_dir;
    next_dir = NULL;
  }

 project_root_done:
  /* Restore original working directory */
  if (g_chdir (save_cwd) != 0) {
    g_critical (_("Could not restore working directory to '%s': %s"),
                save_cwd, g_strerror (errno));
  }

  /* Propagate error, if there was one */
  if (tmp_err != NULL) {
    g_propagate_prefixed_error (error, tmp_err,
                                _("Could not find project root for '%s': "),
                                path);
  }
  g_free (dir);
  g_free (next_dir);
  g_free (save_cwd);
  return result;
}

/*! \public \memberof EdaConfig
 * \brief Return a local configuration context.
 *
 * Looks for a configuration file named "geda.conf".  If \a path is
 * not a directory, it is truncated and then a file named "geda.conf"
 * is looked for in that directory.  If none is found, the parent
 * directory is checked, and so on until a configuration file is found
 * or the root directory is reached.  If no configuration file was
 * found, the returned context will be associated with a "geda.conf"
 * in the same directory as \a path.
 *
 * \warning Do not assume that the configuration file associated with
 * the context returned by eda_config_get_context_for_path() is
 * located in the directory specified by \a path.
 *
 * By default, the parent context of the returned #EdaConfig will be
 * the user context.
 *
 * Multiple calls to eda_config_get_context_for_path() with the same
 * \a path will return the same configuration context.
 *
 * \param [in] path    Path to search for configuration from.
 * \param [out] error  Location to return error information.
 * \return a local #EdaConfig configuration context for \a path.
 */
EdaConfig *
eda_config_get_context_for_path (const gchar *path, GError **error)
{
  static GHashTable *local_contexts = NULL;
  gchar *root;
  gchar *filename;
  EdaConfig *config = NULL;

  /* Initialise global state */
  if (local_contexts == NULL) {
    local_contexts = g_hash_table_new_full (g_str_hash,
                                            g_str_equal,
                                            g_free,
                                            g_object_unref);
  }

  g_return_val_if_fail (path != NULL, NULL);

  /* Find the project root, and the corresponding configuration
   * filename. */
  root = find_project_root (path, error);
  if (root == NULL) return NULL;

  filename = g_build_filename (root, LOCAL_CONFIG_NAME, NULL);
  g_free (root);

  /* If there's already a context available for this file, return
   * that. Otherwise, create a new context and record it in the global
   * state. */
  config = g_hash_table_lookup (local_contexts, filename);
  if (config == NULL) {
    config = g_object_new (EDA_TYPE_CONFIG,
                           "filename", filename,
                           "parent", eda_config_get_user_context (),
                           "trusted", FALSE,
                           NULL);
    g_hash_table_insert (local_contexts, filename, config);
    filename = NULL; /* Now owned by hashtable */
  }

  g_free (filename);
  return config;
}

/*! \public \memberof EdaConfig
 * \brief Return underlying filename for configuration context.
 *
 * Return the filename of the configuration file associated with the
 * context \a cfg.  May return NULL.  The return value is owned by the
 * API and should not be modified or freed.
 *
 * \param cfg  Configuration context.
 * \return Filename of configuration file for \a cfg.
 */
const gchar *
eda_config_get_filename (EdaConfig *cfg)
{
  g_return_val_if_fail (EDA_IS_CONFIG (cfg), NULL);
  return cfg->priv->filename;
}

/*! \public \memberof EdaConfig
 * \brief Load configuration parameters from file.
 *
 * Attempt to load configuration parameters for the context \a cfg
 * from its associated file.  Returns FALSE and generates a
 * #GFileError on error.  If \a cfg does not have an associated file,
 * does nothing, returns FALSE, and generates a G_FILE_ERROR_FAILED
 * error.
 *
 * \see eda_config_is_loaded(), eda_config_get_filename(),
 * eda_config_save().
 *
 * \param cfg    Configuration context.
 * \param error  Location to return error information.
 * \return TRUE on success, FALSE on failure.
 */
gboolean
eda_config_load (EdaConfig *cfg, GError **error)
{
  g_return_val_if_fail (EDA_IS_CONFIG (cfg), TRUE);

  if (cfg->priv->filename == NULL) {
    g_set_error (error,
                 G_FILE_ERROR,
                 G_FILE_ERROR_FAILED,
                 _("Undefined configuration filename"));
    return FALSE;
  }

  /* This will be the new key file object. */
  GKeyFile *newkeyfile = g_key_file_new ();
  GError *tmp_err = NULL;
  gint status =
    g_key_file_load_from_file (newkeyfile,
                               cfg->priv->filename,
                               (G_KEY_FILE_KEEP_COMMENTS
                                | G_KEY_FILE_KEEP_TRANSLATIONS),
                               &tmp_err);
  if (!status) {
    g_key_file_free (newkeyfile);
    propagate_key_file_error (tmp_err, error);
    return FALSE;
  }

  /* Substitute in new key file object, and reset loaded and changed
   * flags. */
  g_key_file_free (cfg->priv->keyfile);
  cfg->priv->keyfile = newkeyfile;
  cfg->priv->changed = FALSE;
  cfg->priv->loaded = TRUE;

  /* FIXME Should we emit a config-changed signal here? */
  return TRUE;
}

/*! \public \memberof EdaConfig
 * \brief Test whether a configuration context has been loaded.
 *
 * Test whether the configuration context \a cfg has been successfully
 * loaded from disk.
 *
 * \param cfg  Configuration context.
 * \return TRUE if \a cfg has been loaded at some point, FALSE
 * otherwise.
 */
gboolean
eda_config_is_loaded (EdaConfig *cfg)
{
  g_return_val_if_fail (EDA_IS_CONFIG (cfg), TRUE);
  return cfg->priv->loaded;
}

/*! \public \memberof EdaConfig
 * \brief Save changes to a configuration context.
 *
 * Attempt to save configuration parameters for the context \a cfg to
 * its associated file.  Returns FALSE and generates a #GFileError on
 * error.  If \a cfg does not have an associated file, does nothing,
 * returns FALSE, and generates a G_FILE_ERROR_FAILED error.
 *
 * \see eda_config_load(), eda_config_get_filename().
 *
 * \param cfg    Configuration context.
 * \param error  Location to return error information.
 * \return TRUE on success, FALSE on failure.
 */
gboolean
eda_config_save (EdaConfig *cfg, GError **error)
{
  g_return_val_if_fail (EDA_IS_CONFIG (cfg), TRUE);

  if (cfg->priv->filename == NULL) {
    g_set_error (error,
                 G_FILE_ERROR,
                 G_FILE_ERROR_FAILED,
                 _("Undefined configuration filename"));
    return FALSE;
  }

  /* First try and make the directory, if necessary. */
  gchar *dirname = g_path_get_dirname (cfg->priv->filename);
  if (g_mkdir_with_parents (dirname, 0755) != 0) {
    g_set_error (error,
                 G_FILE_ERROR,
                 g_file_error_from_errno (errno),
                 _("Could not create directory '%s': %s"),
                 dirname, g_strerror (errno));
    return FALSE;
  }

  gsize len;
  gchar *buf = g_key_file_to_data (cfg->priv->keyfile, &len, NULL);
  gboolean result = g_file_set_contents (cfg->priv->filename,
                                         buf, len, error);
  g_free (buf);
  if (result) cfg->priv->changed = FALSE;
  return result;
}

/*! \public \memberof EdaConfig
 * \brief Test whether configuration was changed since last saved/loaded.
 *
 * Determine whether the configuration context \a cfg has been altered
 * since it was last synchronised with the on-disk version by loading
 * or saving it.
 *
 * \see eda_config_save(), eda_config_load().
 *
 * \param cfg  Configuration context.
 * \return TRUE if altered since last load/save, FALSE otherwise.
 */
gboolean
eda_config_is_changed (EdaConfig *cfg)
{
  g_return_val_if_fail (EDA_IS_CONFIG (cfg), FALSE);
  return cfg->priv->changed;
}

/*! \public \memberof EdaConfig
 * \brief Get a configuration context's parent context.
 *
 * Return the parent context of the context \a cfg, if it has one; if
 * not, returns NULL.
 *
 * \param cfg  Configuration context.
 * \return parent context of \a cfg, or NULL.
 */
EdaConfig *
eda_config_get_parent (EdaConfig *cfg)
{
  g_return_val_if_fail (EDA_IS_CONFIG (cfg), NULL);
  return cfg->priv->parent;
}

/*! \private \memberof EdaConfig
 * \brief Test whether one configuration context is child of another.
 *
 * Returns TRUE if \a cfg is a descendent context of \a parent,
 * directly or indirectly.
 *
 * \param cfg     Configuration context.
 * \param parent  Context to check if ancestor of \a cfg.
 * \return TRUE if \a parent is ancestor of \a cfg, FALSE otherwise.
 */
static gboolean
eda_config_is_descendent (EdaConfig *cfg, EdaConfig *parent)
{
  g_return_val_if_fail (EDA_IS_CONFIG (cfg), FALSE);
  g_return_val_if_fail (EDA_IS_CONFIG (parent), FALSE);

  EdaConfig *iter = cfg;
  while (iter != NULL) {
    if (iter == parent) return TRUE;
    iter = eda_config_get_parent (iter);
  }
  return FALSE;
}

/*! \public \memberof EdaConfig
 * \brief Set a configuration context's parent context.
 *
 * Sets \a parent as the parent context of \a cfg.  If \a parent is
 * NULL, sets \a cfg as having no parent context.  Inheritance loops
 * are not permitted.
 *
 * \warning Normally, application code should avoid using this
 *          function; keeping to the default configuration inheritance
 *          structure is recommended in order to ensure consistent
 *          behaviour of all libgeda applications.
 *
 * \param cfg     Configuration context.
 * \param parent  New parent context for \a cfg.
 */
void
eda_config_set_parent (EdaConfig *cfg, EdaConfig *parent)
{
  g_return_if_fail (EDA_IS_CONFIG (cfg));
  g_object_set (cfg, "parent", parent, NULL);
}

/*! \public \memberof EdaConfig
 * \brief Test whether a context is trusted.
 *
 * Returns TRUE if \a cfg is a "trusted" configuration context
 * (i.e. if it is permitted as a source for risky configuration
 * parameters such as system commands).
 *
 * \param cfg  Configuration context.
 * \return TRUE if \a cfg is trusted.
 */
gboolean
eda_config_is_trusted (EdaConfig *cfg)
{
  g_return_val_if_fail (EDA_IS_CONFIG (cfg), FALSE);
  return cfg->priv->trusted;
}

/*! \public \memberof EdaConfig
 * \brief Set whether a context is trusted.
 *
 * Set whether the configuration context \a cfg is trusted as a source
 * for risky configuration parameters.
 *
 * \warning You should not set a configuration context as trusted
 *          unless you are certain that it originated from a safe
 *          source (e.g. by interacting with the user to verify it).
 *
 * \param cfg      Configuration context.
 * \param trusted  TRUE if \a cfg should be trusted; FALSE otherwise.
 */
void
eda_config_set_trusted (EdaConfig *cfg, gboolean trusted)
{
  g_return_if_fail (EDA_IS_CONFIG (cfg));
  g_object_set (cfg, "trusted", trusted, NULL);
}

/*! \public \memberof EdaConfig
 * \brief Get a configuration contexts first trusted ancestor.
 *
 * Returns the first trusted configuration context that \a cfg
 * inherits from, or \a cfg if \a cfg is trusted.  If no trusted
 * context is found, returns NULL.
 *
 * \param cfg  Configuration context.
 * \return first trusted ancestor of \a cfg, or NULL.
 */
EdaConfig *
eda_config_get_trusted_context (EdaConfig *cfg)
{
  g_return_val_if_fail (EDA_IS_CONFIG (cfg), NULL);
  EdaConfig *iter = cfg;
  while (iter != NULL) {
    if (eda_config_is_trusted (iter)) return iter;
    iter = eda_config_get_parent (iter);
  }
  return NULL;
}

/*! \brief Turn hashtable into key list.
 *
 * Convert a hashtable with string keys and empty values into an array
 * of string pointers.  Used by eda_config_get_groups() and
 * eda_config_get_keys().
 */
static gchar **
hash_table_keys_array (GHashTable *table, gsize *length)
{
  gsize len = g_hash_table_size (table);
  gchar **result = g_new0 (gchar *, 1 + len);
  GHashTableIter iter;
  gpointer key;
  int i = 0;
  g_hash_table_iter_init (&iter, table);
  while (g_hash_table_iter_next (&iter, &key, NULL)) {
    g_hash_table_iter_steal (&iter);
    result[i++] = (gchar *) key;
  }
  result[i] = NULL;

  g_hash_table_destroy (table);
  if (length != NULL) *length = len;
  return result;
}

/*! \public \memberof EdaConfig
 * \brief Return a list of a configuration context's available groups.
 *
 * Returns a list of the all groups available in \a cfg and its parent
 * contexts.  The value returned by eda_config_get_groups() is a
 * newly-allocated NULL-terminated array of strings.  Use g_strfreev()
 * to free it.  The \a length argument is an optional return location
 * for the number of groups returned.
 *
 * \see eda_config_has_group().
 *
 * \todo The current implementation is not enormously efficient; we
 * can do better!
 *
 * \param cfg     Configuration context.
 * \param length  Return location for number of groups.
 * \return a newly-allocated NULL-terminated array of strings.
 */
gchar **
eda_config_get_groups (EdaConfig *cfg, gsize *length)
{
  g_return_val_if_fail (EDA_IS_CONFIG (cfg), NULL);

  GHashTable *group_table = g_hash_table_new_full (g_str_hash, g_str_equal,
                                                   g_free, NULL);

  /* Build a hashtable with all groups in current and parent contexts
   * as keys. */
  EdaConfig *curr = cfg;
  while (curr != NULL) {
    gsize len, i;
    gchar **local_groups = g_key_file_get_groups (curr->priv->keyfile, &len);
    for (i = 0; i < len; i++) {
      g_hash_table_insert (group_table, local_groups[i], NULL);
    }
    g_free (local_groups); /* Keys are now owned by hashtable, don't
                              need to use g_strfreev(). */

    curr = eda_config_get_parent (curr);
  }

  return hash_table_keys_array (group_table, length);
}

/*! \public \memberof EdaConfig
 * \brief Test whether a configuration context has a particular group.
 *
 * Tests whether the configuration context \a cfg, or any of its
 * parent contexts, contains the \a group.
 *
 * \see eda_config_get_keys().
 *
 * \param cfg    Configuration context.
 * \param group  Group to check for.
 * \return TRUE if \a cfg or any of its ancestors contains \a group,
 * otherwise FALSE.
 */
gboolean
eda_config_has_group (EdaConfig *cfg, const gchar *group)
{
  g_return_val_if_fail (EDA_IS_CONFIG (cfg), FALSE);
  g_return_val_if_fail (group != NULL, FALSE);

  EdaConfig *curr;
  for (curr = cfg; curr != NULL; curr = eda_config_get_parent (curr)) {
    if (g_key_file_has_group (curr->priv->keyfile, group)) {
      return TRUE;
    }
  }
  return FALSE;
}

/*! \public \memberof EdaConfig
 * \brief Get the keys available in a particular configuration group.
 *
 * Get a list of all keys available in the specified \a group in the
 * configuration context \a cfg and its parent contexts.  The value
 * returned by eda_config_get_keys() is a newly-allocated
 * NULL-terminated array of strings.  Use g_strfreev() to free it.
 * The \a length argument is an optional return location for the
 * number of keys returned.  If an error occurs, returns NULL.
 *
 * \see eda_config_has_key().
 *
 * \param cfg     Configuration context.
 * \param group   Group to get key list for.
 * \param length  Return location for number of keys, or NULL.
 * \param error   Return location for error information.
 */
gchar **
eda_config_get_keys (EdaConfig *cfg, const gchar *group, gsize *length,
                     GError **error)
{
  g_return_val_if_fail (EDA_IS_CONFIG (cfg), NULL);

  GHashTable *key_table = NULL;
  EdaConfig *curr;
  for (curr = cfg; curr != NULL; curr = eda_config_get_parent (curr)) {
    gsize len, i;
    gchar **local_keys = g_key_file_get_keys (curr->priv->keyfile,
                                              group, &len, NULL);
    /* Skip files that don't provide the requested group */
    if (local_keys == NULL) continue;

    /* Create keytable if not already created. */
    if (key_table == NULL) {
      key_table = g_hash_table_new_full (g_str_hash, g_str_equal, g_free, NULL);
    }

    for (i = 0; i < len; i++) {
      g_hash_table_insert (key_table, local_keys[i], NULL);
    }
    g_free (local_keys); /* Keys are now owned by hashtable, don't
                            need to use g_strfreev(). */
  }

  /* If the hashtable was never created, it means that no matching
   * group was found. */
  if (key_table == NULL) {
    g_set_error (error, EDA_CONFIG_ERROR,
                 EDA_CONFIG_ERROR_GROUP_NOT_FOUND,
                 _("Configuration does not have group '%s'"),
                 group ? group : "(null)");
    return NULL;
  }

  return hash_table_keys_array (key_table, length);
}

/*! \public \memberof EdaConfig
 * \brief Test whether a configuration context has a particular key.
 *
 * Tests whether the configuration context \a cfg, or any of its
 * parent contexts, contains the parameter specified by \a group and
 * \a key.  If \a group was not found, returns FALSE and sets \a error.
 *
 * \note This function does not folow the rules for GError strictly;
 *       the return value both carries meaning and signals an error.
 *       To use this function, you must pass a GError pointer in \a
 *       error, and check whether it is not NULL to see if an error
 *       occurred.
 *
 * \see eda_config_get_keys().
 *
 * \param cfg    Configuration context.
 * \param group  Group to look for \a key in.
 * \param key    Key to check for.
 * \param error  Return location for error information.
 * \return TRUE if \a cfg or any of its ancestors contains \a group
 * and \a key, otherwise FALSE.
 */
gboolean
eda_config_has_key (EdaConfig *cfg, const gchar *group,
                    const gchar *key, GError **error)
{
  return (eda_config_get_source (cfg, group, key, error) != NULL);
}

/*! \public \memberof EdaConfig
 * \brief Obtain the originating context for a configuration
 * parameter.
 *
 * Returns the configuration context (either \a cfg or one of its
 * parent contexts) in which the configuration parameter with the
 * given \a group and \a key has a value specified.  If the group or
 * key cannot be found, returns FALSE and sets \a error.
 *
 * \see eda_config_is_inherited().
 *
 * \param cfg    Configuration context.
 * \param group  Configuration group name.
 * \param key    Configuration key name.
 * \param error  Return location for error information.
 */
EdaConfig *
eda_config_get_source (EdaConfig *cfg, const gchar *group,
                       const gchar *key, GError **error)
{
  g_return_val_if_fail (EDA_IS_CONFIG (cfg), FALSE);
  g_return_val_if_fail (group != NULL, FALSE);
  g_return_val_if_fail (key != NULL, FALSE);

  if (!eda_config_has_group (cfg, group)) {
    g_set_error (error, EDA_CONFIG_ERROR,
                 EDA_CONFIG_ERROR_GROUP_NOT_FOUND,
                 _("Configuration does not have group '%s'"),
                 group);
    return NULL;
  }

  EdaConfig *curr;
  for (curr = cfg; curr != NULL; curr = eda_config_get_parent (curr)) {
    if (g_key_file_has_key (curr->priv->keyfile, group, key, NULL)) {
      return curr;
    }
  }

  g_set_error (error, EDA_CONFIG_ERROR,
               EDA_CONFIG_ERROR_KEY_NOT_FOUND,
               _("Configuration does not have key '%s'"),
               key);
  return NULL;
}

/*! \public \memberof EdaConfig
 * \brief Test whether a configuration parameter is inherited.
 *
 * Tests whether the value of the configuration parameter with the
 * given \a group and \a key is specified in the context \a cfg, or
 * whether it is inherited from a parent context of \a cfg.
 *
 * \note This function does not folow the rules for GError strictly;
 *       the return value both carries meaning and signals an error.
 *       To use this function, you must pass a GError pointer in \a
 *       error, and check whether it is not NULL to see if an error
 *       occurred.
 *
 * \see eda_config_get_source().
 *
 * \param cfg    Configuration context.
 * \param group  Configuration group name.
 * \param key    Configuration key name.
 * \param error  Return location for error information.
 */
gboolean
eda_config_is_inherited (EdaConfig *cfg, const gchar *group,
                         const gchar *key, GError **error)
{
  return (eda_config_get_source (cfg, group, key, error) != cfg);
}

/*! \public \memberof EdaConfig
 * \brief Get the value of a configuration parameter as a string.
 *
 * Get the value of the configuration parameter specified by \a group
 * and \a key in the configuration context \a cfg, as a string.  If an
 * error occurs, NULL is returned and \a error is set.
 *
 * The returned string is owned by the caller, and should be freed
 * with g_free() when no longer needed.
 *
 * \see eda_config_set_string().
 *
 * \param cfg    Configuration context.
 * \param group  Configuration group name.
 * \param key    Configuration key name.
 * \param error  Return location for error information.
 * \return configuration value as string, or NULL.
 */
gchar *
eda_config_get_string (EdaConfig *cfg, const gchar *group,
                       const gchar *key, GError **error)
{
  cfg = eda_config_get_source (cfg, group, key, error);
  if (cfg == NULL) return NULL;

  GError *tmp_err = NULL;
  gchar *result =
    g_key_file_get_string (cfg->priv->keyfile, group, key, &tmp_err);
  propagate_key_file_error (tmp_err, error);
  return result;
}

/*! \public \memberof EdaConfig
 * \brief Get the value of a configuration parameter as a boolean.
 *
 * Get the value of the configuration parameter specified by \a group
 * and \a key in the configuration context \a cfg, as a boolean.  If
 * an error occurs, FALSE is returned and \a error is set.
 *
 * \note This function does not folow the rules for GError strictly;
 *       the return value both carries meaning and signals an error.
 *       To use this function, you must pass a GError pointer in \a
 *       error, and check whether it is not NULL to see if an error
 *       occurred.
 *
 * \see eda_config_set_boolean().
 *
 * \param cfg    Configuration context.
 * \param group  Configuration group name.
 * \param key    Configuration key name.
 * \param error  Return location for error information.
 * \return configuration value as a boolean.
 */
gboolean
eda_config_get_boolean (EdaConfig *cfg, const gchar *group,
                        const gchar *key, GError **error)
{
  cfg = eda_config_get_source (cfg, group, key, error);
  if (cfg == NULL) return FALSE;

  GError *tmp_err = NULL;
  gboolean result =
    g_key_file_get_boolean (cfg->priv->keyfile, group, key, &tmp_err);
  propagate_key_file_error (tmp_err, error);
  return result;
}

/*! \public \memberof EdaConfig
 * \brief Get the value of a configuration parameter as an integer.
 *
 * Get the value of the configuration parameter specified by \a group
 * and \a key in the configuration context \a cfg, as an integer.  If
 * an error occurs, 0 is returned and \a error is set.
 *
 * \note This function does not folow the rules for GError strictly;
 *       the return value both carries meaning and signals an error.
 *       To use this function, you must pass a GError pointer in \a
 *       error, and check whether it is not NULL to see if an error
 *       occurred.
 *
 * \see eda_config_set_int().
 *
 * \param cfg    Configuration context.
 * \param group  Configuration group name.
 * \param key    Configuration key name.
 * \param error  Return location for error information.
 * \return configuration value as a integer.
 */
gint
eda_config_get_int (EdaConfig *cfg, const gchar *group,
                    const gchar *key, GError **error)
{
  cfg = eda_config_get_source (cfg, group, key, error);
  if (cfg == NULL) return 0;

  GError *tmp_err = NULL;
  gint result =
    g_key_file_get_integer (cfg->priv->keyfile, group, key, &tmp_err);
  propagate_key_file_error (tmp_err, error);
  return result;
}

/*! \public \memberof EdaConfig
 * \brief Get the value of a configuration parameter as a double.
 *
 * Get the value of the configuration parameter specified by \a group
 * and \a key in the configuration context \a cfg, as a double.  If
 * an error occurs, 0.0 is returned and \a error is set.
 *
 * \note This function does not folow the rules for GError strictly;
 *       the return value both carries meaning and signals an error.
 *       To use this function, you must pass a GError pointer in \a
 *       error, and check whether it is not NULL to see if an error
 *       occurred.
 *
 * \see eda_config_set_double().
 *
 * \param cfg    Configuration context.
 * \param group  Configuration group name.
 * \param key    Configuration key name.
 * \param error  Return location for error information.
 * \return configuration value as a double.
 */
gdouble
eda_config_get_double (EdaConfig *cfg, const gchar *group,
                       const gchar *key, GError **error)
{
  cfg = eda_config_get_source (cfg, group, key, error);
  if (cfg == NULL) return 0.0;

  GError *tmp_err = NULL;
  gdouble result =
    g_key_file_get_double (cfg->priv->keyfile, group, key, &tmp_err);
  propagate_key_file_error (tmp_err, error);
  return result;
}


/*! \public \memberof EdaConfig
 * \brief Get the value of a configuration parameter as a string list.
 *
 * Get the value of the configuration parameter specified by \a group
 * and \a key in the configuration context \a cfg, as a
 * newly-allocated NULL-terminated array of strings.  If an error
 * occurs, NULL is returned and \a error is set.  The returned value
 * should be freed with g_strfreev() when no longer needed.
 *
 * \see eda_config_set_string_list().
 *
 * \param cfg    Configuration context.
 * \param group  Configuration group name.
 * \param key    Configuration key name.
 * \param length Return location for array length, or NULL.
 * \param error  Return location for error information.
 * \return configuration value as an array of strings.
 */
gchar **
eda_config_get_string_list (EdaConfig *cfg, const gchar *group,
                            const gchar *key, gsize *length, GError **error)
{
  cfg = eda_config_get_source (cfg, group, key, error);
  if (cfg == NULL) return NULL;

  GError *tmp_err = NULL;
  gchar **result =
    g_key_file_get_string_list (cfg->priv->keyfile, group, key,
                                length, &tmp_err);
  propagate_key_file_error (tmp_err, error);
  return result;
}

/*! \public \memberof EdaConfig
 * \brief Get the value of a configuration parameter as a boolean list.
 *
 * Get the value of the configuration parameter specified by \a group
 * and \a key in the configuration context \a cfg, as a
 * newly-allocated array of booleans.  If an error occurs, NULL is
 * returned and \a error is set.
 *
 * \see eda_config_set_boolean_list().
 *
 * \param cfg    Configuration context.
 * \param group  Configuration group name.
 * \param key    Configuration key name.
 * \param length Return location for array length.
 * \param error  Return location for error information.
 * \return configuration value as an array of booleans.
 */
gboolean *
eda_config_get_boolean_list (EdaConfig *cfg, const gchar *group,
                             const gchar *key, gsize *length, GError **error)
{
  cfg = eda_config_get_source (cfg, group, key, error);
  if (cfg == NULL) return NULL;

  GError *tmp_err = NULL;
  gboolean *result =
    g_key_file_get_boolean_list (cfg->priv->keyfile, group, key,
                                 length, &tmp_err);
  propagate_key_file_error (tmp_err, error);
  return result;
}

/*! \public \memberof EdaConfig
 * \brief Get the value of a configuration parameter as an integer list.
 *
 * Get the value of the configuration parameter specified by \a group
 * and \a key in the configuration context \a cfg, as a
 * newly-allocated array of integers.  If an error occurs, NULL is
 * returned and \a error is set.
 *
 * \see eda_config_set_int_list().
 *
 * \param cfg    Configuration context.
 * \param group  Configuration group name.
 * \param key    Configuration key name.
 * \param length Return location for array length.
 * \param error  Return location for error information.
 * \return configuration value as an array of integers.
 */
gint *
eda_config_get_int_list (EdaConfig *cfg, const gchar *group,
                         const gchar *key, gsize *length, GError **error)
{
  cfg = eda_config_get_source (cfg, group, key, error);
  if (cfg == NULL) return NULL;

  GError *tmp_err = NULL;
  gint *result =
    g_key_file_get_integer_list (cfg->priv->keyfile, group, key,
                                 length, &tmp_err);
  propagate_key_file_error (tmp_err, error);
  return result;
}

/*! \public \memberof EdaConfig
 * \brief Get the value of a configuration parameter as a double list.
 *
 * Get the value of the configuration parameter specified by \a group
 * and \a key in the configuration context \a cfg, as a
 * newly-allocated array of doubles.  If an error occurs, NULL is
 * returned and \a error is set.
 *
 * \see eda_config_set_double_list().
 *
 * \param cfg    Configuration context.
 * \param group  Configuration group name.
 * \param key    Configuration key name.
 * \param length Return location for array length.
 * \param error  Return location for error information.
 * \return configuration value as an array of doubles.
 */
gdouble *
eda_config_get_double_list (EdaConfig *cfg, const gchar *group,
                            const gchar *key, gsize *length, GError **error)
{
  cfg = eda_config_get_source (cfg, group, key, error);
  if (cfg == NULL) return NULL;

  GError *tmp_err = NULL;
  gdouble *result =
    g_key_file_get_double_list (cfg->priv->keyfile, group, key,
                                length, &tmp_err);
  propagate_key_file_error (tmp_err, error);
  return result;
}

/*! \public \memberof EdaConfig
 * \brief Set the value of a configuration parameter from a string.
 *
 * Set the value of the configuration parameter specified by \a group
 * and \a key in the configuration context \a cfg from a string.
 *
 * \see eda_config_get_string().
 *
 * \param cfg    Configuration context.
 * \param group  Configuration group name.
 * \param key    Configuration key name.
 * \param value  New value for parameter.
 */
void
eda_config_set_string (EdaConfig *cfg, const char *group,
                       const char *key, const char *value)
{
  g_key_file_set_string (cfg->priv->keyfile, group, key, value);
  g_signal_emit_by_name (cfg, "config-changed", group, key);
}

/*! \public \memberof EdaConfig
 * \brief Set the value of a configuration parameter from a boolean.
 *
 * Set the value of the configuration parameter specified by \a group
 * and \a key in the configuration context \a cfg from a boolean.
 *
 * \see eda_config_get_boolean().
 *
 * \param cfg    Configuration context.
 * \param group  Configuration group name.
 * \param key    Configuration key name.
 * \param value  New value for parameter.
 */
void
eda_config_set_boolean (EdaConfig *cfg, const char *group,
                        const char *key, gboolean value)
{
  g_key_file_set_boolean (cfg->priv->keyfile, group, key, value);
  g_signal_emit_by_name (cfg, "config-changed", group, key);
}

/*! \public \memberof EdaConfig
 * \brief Set the value of a configuration parameter from an integer.
 *
 * Set the value of the configuration parameter specified by \a group
 * and \a key in the configuration context \a cfg from an integer.
 *
 * \see eda_config_get_integer().
 *
 * \param cfg    Configuration context.
 * \param group  Configuration group name.
 * \param key    Configuration key name.
 * \param value  New value for parameter.
 */
void
eda_config_set_int (EdaConfig *cfg, const char *group,
                    const char *key, gint value)
{
  g_key_file_set_integer (cfg->priv->keyfile, group, key, value);
  g_signal_emit_by_name (cfg, "config-changed", group, key);
}

/*! \public \memberof EdaConfig
 * \brief Set the value of a configuration parameter from a double.
 *
 * Set the value of the configuration parameter specified by \a group
 * and \a key in the configuration context \a cfg from a double.
 *
 * \see eda_config_get_double().
 *
 * \param cfg    Configuration context.
 * \param group  Configuration group name.
 * \param key    Configuration key name.
 * \param value  New value for parameter.
 */
void
eda_config_set_double (EdaConfig *cfg, const char *group,
                       const char *key, gdouble value)
{
  g_key_file_set_double (cfg->priv->keyfile, group, key, value);
  g_signal_emit_by_name (cfg, "config-changed", group, key);
}

/*! \public \memberof EdaConfig
 * \brief Set the value of a configuration parameter from a string list.
 *
 * Set the value of the configuration parameter specified by \a group
 * and \a key in the configuration context \a cfg from a list of
 * strings.
 *
 * \see eda_config_get_string_list().
 *
 * \param cfg    Configuration context.
 * \param group  Configuration group name.
 * \param key    Configuration key name.
 * \param list   List of new values for parameter.
 * \param length Number of values in \a list.
 */
void
eda_config_set_string_list (EdaConfig *cfg, const char *group,
                            const char *key, const gchar * const list[],
                            gsize length)
{
  g_key_file_set_string_list (cfg->priv->keyfile, group, key,
                              list, length);
  g_signal_emit_by_name (cfg, "config-changed", group, key);
}

/*! \public \memberof EdaConfig
 * \brief Set the value of a configuration parameter from a boolean list.
 *
 * Set the value of the configuration parameter specified by \a group
 * and \a key in the configuration context \a cfg from a list of
 * booleans.
 *
 * \see eda_config_get_boolean_list().
 *
 * \param cfg    Configuration context.
 * \param group  Configuration group name.
 * \param key    Configuration key name.
 * \param list   List of new values for parameter.
 * \param length Number of values in \a list.
 */
void
eda_config_set_boolean_list (EdaConfig *cfg, const char *group,
                             const char *key, gboolean list[], gsize length)
{
  g_key_file_set_boolean_list (cfg->priv->keyfile, group, key,
                               list, length);
  g_signal_emit_by_name (cfg, "config-changed", group, key);
}

/*! \public \memberof EdaConfig
 * \brief Set the value of a configuration parameter from an integer list.
 *
 * Set the value of the configuration parameter specified by \a group
 * and \a key in the configuration context \a cfg from a list of
 * integers.
 *
 * \see eda_config_get_int_list().
 *
 * \param cfg    Configuration context.
 * \param group  Configuration group name.
 * \param key    Configuration key name.
 * \param list   List of new values for parameter.
 * \param length Number of values in \a list.
 */
void
eda_config_set_int_list (EdaConfig *cfg, const char *group,
                         const char *key, gint list[], gsize length)
{
  g_key_file_set_integer_list (cfg->priv->keyfile, group, key,
                               list, length);
  g_signal_emit_by_name (cfg, "config-changed", group, key);
}


/*! \public \memberof EdaConfig
 * \brief Set the value of a configuration parameter from a double list.
 *
 * Set the value of the configuration parameter specified by \a group
 * and \a key in the configuration context \a cfg from a list of
 * doubles.
 *
 * \see eda_config_get_double_list().
 *
 * \param cfg    Configuration context.
 * \param group  Configuration group name.
 * \param key    Configuration key name.
 * \param list   List of new values for parameter.
 * \param length Number of values in \a list.
 */
void
eda_config_set_double_list (EdaConfig *cfg, const char *group,
                            const char *key, gdouble list[], gsize length)
{
  g_key_file_set_double_list (cfg->priv->keyfile, group, key,
                              list, length);
  g_signal_emit_by_name (cfg, "config-changed", group, key);
}

/*! \brief Callback marshal function for config-changed signals.
 * \par Function Description
 * Based heavily on g_cclosure_marshal_VOID__STRING() from GObject.
 */
static void
cclosure_marshal_VOID__STRING_STRING (GClosure *closure,
                                      GValue *return_value,
                                      guint n_param_values,
                                      const GValue *param_values,
                                      gpointer invocation_hint,
                                      gpointer marshal_data)
{
  typedef void (*MarshalFunc_VOID__STRING_STRING) (gpointer data1,
                                                   gpointer arg_1,
                                                   gpointer arg_2,
                                                   gpointer data2);
  register MarshalFunc_VOID__STRING_STRING callback;
  register GCClosure *cc = (GCClosure *) closure;
  register gpointer data1, data2;

  g_return_if_fail (n_param_values == 3);
  if (G_CCLOSURE_SWAP_DATA (closure)) {
    data1 = closure->data;
    data2 = g_value_peek_pointer (param_values + 0);
  } else {
    data1 = g_value_peek_pointer (param_values + 0);
    data2 = closure->data;
  }
  callback = (MarshalFunc_VOID__STRING_STRING) (marshal_data ? marshal_data : cc->callback);

  callback (data1,
            (gpointer) g_value_get_string (param_values + 1),
            (gpointer) g_value_get_string (param_values + 2),
            data2);
}

/*! \brief Default handler for config change signals.
 *
 * Sets the changed flag for \a cfg.
 *
 * \param cfg     Configuration context.
 * \param group   Configuration group name.
 * \param key     Configuration key name.
 * \param cfg     Child configuration context.
 */
static void
default_config_changed_handler (EdaConfig *cfg, const gchar *group,
                                const gchar* key)
{
  cfg->priv->changed = TRUE;
}

/*! \brief Emit config change signals for inherited configuration.
 *
 * Signal handler used by configuration contexts with parent contexts
 * to emit signals. When the value of a configuration parameter that
 * is inherited from the parent context is changed in the parent
 * context, re-emits the signal.
 *
 * \param parent  Parent configuration context.
 * \param group   Configuration group name.
 * \param key     Configuration key name.
 * \param cfg     Child configuration context.
 */
static void
parent_config_changed_handler (EdaConfig *cfg, const gchar *group,
                               const gchar* key, EdaConfig *parent)
{
  if (eda_config_is_inherited (cfg, group, key, NULL)) {
    g_signal_emit_by_name (cfg, "config-changed", group, key);
  }
}

/*! \brief Get #EdaConfig error domain.
 * \par Function Description
 * Return the domain for errors relating to configuration contexts.
 *
 * \warning You should not call this function directly; use
 * EDA_CONFIG_ERROR instead.
 *
 * \return a #GQuark representing the error domain.
 */
GQuark
eda_config_error_quark (void)
{
  return g_quark_from_static_string ("eda-config-error-quark");
}

/*! \brief Create an #EdaConfigError from a #GKeyFileError.
 *
 * Propagate an error returned by a #GKeyFile function, converting any
 * #GKeyFileError found into a #EdaConfigError.  The \a src error will
 * be freed.
 *
 * \note We do this so that we can move away from using a #GKeyFile
 *       internally if we want to at some point.
 *
 * \param src   Error to propagate.
 * \param dest  Target #GError to set with error information.
 */
static void
propagate_key_file_error (GError *src, GError **dest)
{
  if (src == NULL) return;
  if (dest == NULL) {
    g_error_free (src);
    return;
  }
  g_return_if_fail (*dest == NULL);
  g_propagate_error (dest, src);

  if ((*dest)->domain != G_KEY_FILE_ERROR) {
    return;
  }

  gint code;

  switch ((*dest)->code) {
  case G_KEY_FILE_ERROR_UNKNOWN_ENCODING:
    code = EDA_CONFIG_ERROR_UNKNOWN_ENCODING;
    break;
  case G_KEY_FILE_ERROR_PARSE:
    code = EDA_CONFIG_ERROR_PARSE;
    break;
  case G_KEY_FILE_ERROR_KEY_NOT_FOUND:
    code = EDA_CONFIG_ERROR_KEY_NOT_FOUND;
    break;
  case G_KEY_FILE_ERROR_GROUP_NOT_FOUND:
    code = EDA_CONFIG_ERROR_GROUP_NOT_FOUND;
    break;
  case G_KEY_FILE_ERROR_INVALID_VALUE:
    code = EDA_CONFIG_ERROR_INVALID_VALUE;
    break;
  case G_KEY_FILE_ERROR_NOT_FOUND:
  default:
    g_return_if_reached ();
  }

  (*dest)->domain = EDA_CONFIG_ERROR;
  (*dest)->code = code;
}
