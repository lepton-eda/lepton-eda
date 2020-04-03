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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 */

#ifndef __EDASCM_VALUE_H__
#define __EDASCM_VALUE_H__

G_BEGIN_DECLS

/* ---------------------------------------------------------------- */
/* GValue support */

/*! \class EdascmSCM edascmvalue.h "liblepton/edascmvalue.h"
 * \ingroup guile_c_iface
 * \brief Fundamental GType for Guile Scheme objects.
 *
 * This GType provides an easy way for Guile's SCM values to be used
 * directly with the GValue polymorphic type system.
 */

#define EDASCM_TYPE_SCM (edascm_scm_get_type ())
#define EDASCM_VALUE_HOLDS_SCM(value) (G_TYPE_CHECK_VALUE_TYPE ((value), EDASCM_TYPE_SCM))

GType edascm_scm_get_type (void) G_GNUC_CONST;

void edascm_value_set_scm (GValue *value, SCM v_scm);
SCM edascm_value_get_scm (const GValue *value);



/* ---------------------------------------------------------------- */
/* GParamSpec support */

/*! \class EdascmParamSCM edascmvalue.h "liblepton/edascmvalue.h"
 * \ingroup guile_c_iface
 * \brief Parameter specification metadata type for Guile Scheme objects.
 *
 * This GParamSpec can be used with #EdascmSCM and the GObject
 * property system to define GObjects that have Guile Scheme objects
 * as properties.
 */

#define EDASCM_TYPE_PARAM_SCM (edascm_param_spec_scm_get_type ())
#define EDASCM_IS_PARAM_SPEC_SCM(pspec) (G_TYPE_CHECK_INSTANCE_TYPE ((pspec), EDASCM_TYPE_PARAM_SCM))
#define EDASCM_PARAM_SPEC_SCM(pspec) (G_TYPE_CHECK_INSTANCE_CAST ((pspec), EDASCM_TYPE_PARAM_SCM, EdascmParamSpecSCM))

GType edascm_param_spec_scm_get_type (void) G_GNUC_CONST;

typedef struct _EdascmParamSpecSCM EdascmParamSpecSCM;

struct _EdascmParamSpecSCM
{
  GParamSpec parent_instance;
};

GParamSpec *edascm_param_spec_scm (const gchar *name,
                                   const gchar *nick,
                                   const gchar *blurb,
                                   GParamFlags flags) G_GNUC_WARN_UNUSED_RESULT;

G_END_DECLS

#endif /* ! __EDASCM_VALUE_H__ */
