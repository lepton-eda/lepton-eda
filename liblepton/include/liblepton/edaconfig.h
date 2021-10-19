/* Lepton EDA library
 * Copyright (C) 2011-2013 gEDA Contributors
 * Copyright (C) 2017-2021 Lepton EDA Contributors
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 */

#ifndef __EDA_CONFIG_H__
#define __EDA_CONFIG_H__

G_BEGIN_DECLS

/* ---------------------------------------------------------------- */

/*! \class EdaConfig edaconfig.h "liblepton/edaconfig.h"
 * \brief Hierarchical configuration context for gEDA configuration data.
 *
 * Configuration parameters in gEDA are always evaluated within a
 * configuration context.  Each context is associated with a
 * configuration file (although the file does not necessarily need to
 * exist).
 *
 * Each configuration context may have a parent context.  If, when
 * looking up a parameter, it has no value set in the selected
 * context, the parent context is checked, and so on.
 *
 * Configuration contexts are represented by instances of the
 * #EdaConfig class. Since this is a a subclass of #GObject, the
 * instances are reference counted.  You can increment and decrement
 * their reference counters using g_object_ref() and g_object_unref().
 *
 * Normally, you shouldn't create a configuration context directly;
 * you should obtain the configuration context associated with a path
 * using eda_config_get_context_for_path().
 */

/*! Domain for errors relating to EdaConfig operations. */
#define EDA_CONFIG_ERROR eda_config_error_quark ()

/*! Error numbers for errors relating to EdaConfig operations. */
typedef enum {
  EDA_CONFIG_ERROR_UNKNOWN_ENCODING,
  EDA_CONFIG_ERROR_PARSE,
  EDA_CONFIG_ERROR_KEY_NOT_FOUND,
  EDA_CONFIG_ERROR_GROUP_NOT_FOUND,
  EDA_CONFIG_ERROR_INVALID_VALUE
} EdaConfigError;

GQuark eda_config_error_quark (void);

/* ---------------------------------------------------------------- */

#define EDA_TYPE_CONFIG (eda_config_get_type ())
#define EDA_CONFIG(obj) (G_TYPE_CHECK_INSTANCE_CAST ((obj), EDA_TYPE_CONFIG, EdaConfig))
#define EDA_CONFIG_CLASS(klass) (G_TYPE_CHECK_CLASS_CAST ((klass), EDA_TYPE_CONFIG, EdaConfigClass))
#define EDA_IS_CONFIG(obj) (G_TYPE_CHECK_INSTANCE_TYPE ((obj), EDA_TYPE_CONFIG))
#define EDA_IS_CONFIG_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), EDA_TYPE_CONFIG))
#define EDA_CONFIG_GET_CLASS(obj) (G_TYPE_INSTANCE_GET_CLASS ((obj), EDA_TYPE_CONFIG, EdaConfigClass))

typedef struct _EdaConfigClass EdaConfigClass;
typedef struct _EdaConfig EdaConfig;
typedef struct _EdaConfigPrivate EdaConfigPrivate;

struct _EdaConfigClass
{
  GObjectClass parent_class;

  /* signals */
  void (*config_changed)(EdaConfig *cfg, const gchar *group, const gchar *key);
};

struct _EdaConfig
{
  GObject parent_instance;

  /* Private members */
  EdaConfigPrivate *priv;
};

GType eda_config_get_type (void) G_GNUC_CONST;

/* ---------------------------------------------------------------- */

EdaConfig *eda_config_get_context_for_path (const gchar *path) G_GNUC_WARN_UNUSED_RESULT;

EdaConfig *eda_config_get_cache_context (void);
EdaConfig *eda_config_get_default_context (void);
EdaConfig *eda_config_get_system_context (void);
EdaConfig *eda_config_get_user_context (void);

const gchar *eda_config_get_filename (EdaConfig *cfg);
gboolean eda_config_load (EdaConfig *cfg, GError **err);
gboolean eda_config_is_loaded (EdaConfig *cfg);

gboolean eda_config_save (EdaConfig *cfg, GError **error);
gboolean eda_config_is_changed (EdaConfig *cfg);

EdaConfig *eda_config_get_parent (EdaConfig *cfg);
void eda_config_set_parent (EdaConfig *cfg, EdaConfig *parent);

gboolean eda_config_is_trusted (EdaConfig *cfg);
void eda_config_set_trusted (EdaConfig *cfg, gboolean trusted);
EdaConfig *eda_config_get_trusted_context (EdaConfig *cfg);

gchar **eda_config_get_groups (EdaConfig *cfg, gsize *length) G_GNUC_WARN_UNUSED_RESULT;
gboolean eda_config_has_group (EdaConfig *cfg, const gchar *group);
gchar **eda_config_get_keys (EdaConfig *cfg, const gchar *group, gsize *length, GError **error) G_GNUC_WARN_UNUSED_RESULT;
gboolean eda_config_has_key (EdaConfig *cfg, const gchar *group, const gchar *key, GError **err);

gboolean eda_config_is_inherited (EdaConfig *cfg, const gchar *group, const gchar *key, GError **err);
EdaConfig *eda_config_get_source (EdaConfig *cfg, const gchar *group, const gchar *key, GError **err);

/* ---------------------------------------------------------------- */

gchar *eda_config_get_string (EdaConfig *cfg, const gchar *group, const gchar *key, GError **err) G_GNUC_WARN_UNUSED_RESULT;
gboolean eda_config_get_boolean (EdaConfig *cfg, const gchar *group, const gchar *key, GError **err);
gint eda_config_get_int (EdaConfig *cfg, const gchar *group, const gchar *key, GError **err);
gdouble eda_config_get_double (EdaConfig *cfg, const gchar *group, const gchar *key, GError **err);
gchar **eda_config_get_string_list (EdaConfig *cfg, const gchar *group, const gchar *key, gsize *length, GError **err) G_GNUC_WARN_UNUSED_RESULT;
gboolean *eda_config_get_boolean_list (EdaConfig *cfg, const gchar *group, const gchar *key, gsize *length, GError **err) G_GNUC_WARN_UNUSED_RESULT;
gint *eda_config_get_int_list (EdaConfig *cfg, const gchar *group, const gchar *key, gsize *length, GError **err) G_GNUC_WARN_UNUSED_RESULT;
gdouble *eda_config_get_double_list (EdaConfig *cfg, const gchar *group, const gchar *key, gsize *length, GError **err) G_GNUC_WARN_UNUSED_RESULT;


void eda_config_set_string (EdaConfig *cfg, const char *group, const char *key, const char *value);
void eda_config_set_boolean (EdaConfig *cfg, const char *group, const char *key, gboolean value);
void eda_config_set_int (EdaConfig *cfg, const char *group, const char *key, gint value);
void eda_config_set_double (EdaConfig *cfg, const char *group, const char *key, gdouble value);
void eda_config_set_string_list (EdaConfig *cfg, const char *group, const char *key, const gchar * const list[], gsize length);
void eda_config_set_boolean_list (EdaConfig *cfg, const char *group, const char *key, gboolean list[], gsize length);
void eda_config_set_int_list (EdaConfig *cfg, const char *group, const char *key, gint list[], gsize length);
void eda_config_set_double_list (EdaConfig *cfg, const char *group, const char *key, gdouble list[], gsize length);

gboolean eda_config_remove_key (EdaConfig *cfg, const char *group, const char *key, GError **error);
gboolean eda_config_remove_group (EdaConfig *cfg, const char *group, GError **error);

void config_set_legacy_mode(gboolean legacy);
gboolean config_get_legacy_mode();

EdaConfig*
eda_config_get_anyfile_context (const gchar* path, EdaConfig* parent, gboolean trusted);

const char*
config_error_type (GError **error);

const char*
config_error_code (GError **error);

char*
config_error_message (GError **error);

gboolean
config_error_file_not_found (GError *error);

G_END_DECLS

#endif /* !__EDA_CONFIG_H__ */
