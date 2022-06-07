/* Lepton EDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2014 gEDA Contributors
 * Copyright (C) 2017-2022 Lepton EDA Contributors
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
/*!
 * \file gschem_options.c
 *
 * \brief
 */

#include <config.h>

#include <stdio.h>
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include "gschem.h"



/*! \private
 */
enum
{
  PROP_0,
  PROP_GRID_MODE,
  PROP_MAGNETIC_NET_MODE,
  PROP_NET_RUBBER_BAND_MODE,
  PROP_SNAP_MODE,
  PROP_SNAP_SIZE
};



G_DEFINE_TYPE (GschemOptions, gschem_options, G_TYPE_OBJECT);


static void
gschem_options_class_init (GschemOptionsClass *klass);

static void
get_property (GObject *object, guint param_id, GValue *value, GParamSpec *pspec);

static void
gschem_options_init (GschemOptions *adapter);

static void
set_property (GObject *object, guint param_id, const GValue *value, GParamSpec *pspec);



/*! \brief Cycle grid mode to the next option
 *
 *  \param options These options
 */
void
gschem_options_cycle_grid_mode (GschemOptions *options)
{
  GRID_MODE next_grid_mode;

  g_return_if_fail (options != NULL);

  next_grid_mode = (GRID_MODE) ((options->grid_mode + 1) % GRID_MODE_COUNT);

  gschem_options_set_grid_mode (options, next_grid_mode);
}



/*! \brief Cycle magnetic net mode to the next option
 *
 *  \param options These options
 */
void
gschem_options_cycle_magnetic_net_mode (GschemOptions *options)
{
  gboolean next_magnetic_net_mode;

  g_return_if_fail (options != NULL);

  next_magnetic_net_mode = !options->magnetic_net_mode;

  gschem_options_set_magnetic_net_mode (options, next_magnetic_net_mode);
}



/*! \brief Cycle net rubber band mode to the next option
 *
 *  \param options These options
 */
void
gschem_options_cycle_net_rubber_band_mode (GschemOptions *options)
{
  gboolean next_net_rubber_band_mode;

  g_return_if_fail (options != NULL);

  next_net_rubber_band_mode = !options->net_rubber_band_mode;

  gschem_options_set_net_rubber_band_mode (options, next_net_rubber_band_mode);
}



/*! \brief Cycle snap mode to the next option
 *
 *  \param options These options
 */
void
gschem_options_cycle_snap_mode (GschemOptions *options)
{
  SNAP_STATE next_snap_mode;

  g_return_if_fail (options != NULL);

  /* toggle to the next snap state */
  next_snap_mode = (SNAP_STATE) ((options->snap_mode + 1) % SNAP_STATE_COUNT);

  gschem_options_set_snap_mode (options, next_snap_mode);
}



/*! \brief Get the grid mode
 *
 *  \param [in] options These options
 *  \return The grid mode
 */
GRID_MODE
gschem_options_get_grid_mode (GschemOptions *options)
{
  g_return_val_if_fail (options != NULL, GRID_MODE_MESH);

  return (GRID_MODE) options->grid_mode;
}



/*! \brief Get the magnetic net mode
 *
 *  \param [in] options These options
 *  \return The magnetic net mode
 */
gboolean
gschem_options_get_magnetic_net_mode (GschemOptions *options)
{
  g_return_val_if_fail (options != NULL, DEFAULT_MAGNETIC_NET_MODE);

  return options->magnetic_net_mode;
}



/*! \brief Get the rubber-band net mode
 *
 *  \param [in] options These options
 *  \return The rubber-band net mode
 */
gboolean
gschem_options_get_net_rubber_band_mode (GschemOptions *options)
{
  g_return_val_if_fail (options != NULL, DEFAULT_NET_RUBBER_BAND_MODE);

  return options->net_rubber_band_mode;
}



/*! \brief Get the snap mode
 *
 *  \param [in] options These options
 *  \return The snap mode
 */
SNAP_STATE
gschem_options_get_snap_mode (GschemOptions *options)
{
  g_return_val_if_fail (options != NULL, SNAP_GRID);

  return options->snap_mode;
}



/*! \brief Get the snap size
 *
 *  \param [in] options These options
 *  \return The snap size
 */
int
gschem_options_get_snap_size (GschemOptions *options)
{
  g_return_val_if_fail (options != NULL, DEFAULT_SNAP_SIZE);

  return options->snap_size;
}



/*! \brief Create a new gschem options
 *
 *  \returns A new set of options
 */
GschemOptions*
gschem_options_new ()
{
  return GSCHEM_OPTIONS (g_object_new (GSCHEM_TYPE_OPTIONS, NULL));
}


/*! \brief Scale the snap size down
 *
 *  \param [in] options These options
 */
void
gschem_options_scale_snap_down (GschemOptions *options)
{
  g_return_if_fail (options != NULL);

  if ((options->snap_size % 2) == 0) {
    gschem_options_set_snap_size (options, options->snap_size / 2);
  }
}



/*! \brief Scale the snap size up
 *
 *  \param [in] options These options
 */
void
gschem_options_scale_snap_up (GschemOptions *options)
{
  g_return_if_fail (options != NULL);

  gschem_options_set_snap_size (options, options->snap_size * 2);
}



/*! \brief Set the grid mode
 *
 *  If the grid mode is invalid the default grid mode is set.
 *
 *  \param [in] options These options
 *  \param [in] grid_mode The grid mode
 */
void
gschem_options_set_grid_mode (GschemOptions *options, GRID_MODE grid_mode)
{
  g_return_if_fail (options != NULL);

  switch (grid_mode) {
    case GRID_MODE_NONE:
    case GRID_MODE_DOTS:
    case GRID_MODE_MESH:
      options->grid_mode = grid_mode;
      break;
    default:
      options->grid_mode = default_grid_mode;
      break;
  }

  g_object_notify (G_OBJECT (options), "grid-mode");
}



/*! \brief Set the magnetic net mode
 *
 *  \param [in] options These options
 *  \param [in] enabled Magnetic net mode
 */
void
gschem_options_set_magnetic_net_mode (GschemOptions *options, gboolean enabled)
{
  g_return_if_fail (options != NULL);

  options->magnetic_net_mode = enabled;

  g_object_notify (G_OBJECT (options), "magnetic-net-mode");
}



/*! \brief Set the net rubber band mode
 *
 *  Sets whether nets rubberband as you move them (or connecting comps)
 *
 *  \param [in] options These options
 *  \param [in] enabled Net rubber band mode
 */
void
gschem_options_set_net_rubber_band_mode (GschemOptions *options, gboolean enabled)
{
  g_return_if_fail (options != NULL);

  options->net_rubber_band_mode = enabled;

  g_object_notify (G_OBJECT (options), "net-rubber-band-mode");
}



/*! \brief Set the snap mode
 *
 *  \param [in] options These options
 *  \param [in] snap_mode The snap mode
 */
void
gschem_options_set_snap_mode (GschemOptions *options, SNAP_STATE snap_mode)
{
  g_return_if_fail (options != NULL);

  options->snap_mode = snap_mode;

  g_object_notify (G_OBJECT (options), "snap-mode");
}



/*! \brief Set the snap size
 *
 *  If the snap size is outside the range of valid values, the value will be
 *  assigned to the closest limit.
 *
 *  \param [in] options These options
 *  \param [in] snap_size The snap size
 */
void
gschem_options_set_snap_size (GschemOptions *options, int snap_size)
{
  g_return_if_fail (options != NULL);

  if (snap_size < MINIMUM_SNAP_SIZE) {
    snap_size = MINIMUM_SNAP_SIZE;
  }

  if (snap_size > MAXIMUM_SNAP_SIZE) {
    snap_size = MAXIMUM_SNAP_SIZE;
  }

  options->snap_size = snap_size;

  g_object_notify (G_OBJECT (options), "snap-size");
}



/*! \private
 *  \brief Initialize gschem options class
 *
 *  \param [in] klass The class for the gschem options
 */
static void
gschem_options_class_init (GschemOptionsClass *klass)
{
  G_OBJECT_CLASS (klass)->get_property = get_property;
  G_OBJECT_CLASS (klass)->set_property = set_property;

  g_object_class_install_property (G_OBJECT_CLASS (klass),
                                   PROP_GRID_MODE,
                                   g_param_spec_int ("grid-mode",
                                                     "Grid Mode",
                                                     "Grid Mode",
                                                     0,
                                                     (GRID_MODE_COUNT - 1),
                                                     DEFAULT_GRID_MODE,
                                                     (GParamFlags) (G_PARAM_READWRITE
                                                                    | G_PARAM_STATIC_STRINGS
                                                                    | G_PARAM_CONSTRUCT)));

  g_object_class_install_property (G_OBJECT_CLASS (klass),
                                   PROP_MAGNETIC_NET_MODE,
                                   g_param_spec_boolean ("magnetic-net-mode",
                                                         "Magnetic Net Mode",
                                                         "magnetic Net Mode",
                                                         DEFAULT_MAGNETIC_NET_MODE,
                                                         (GParamFlags) (G_PARAM_READWRITE
                                                                        | G_PARAM_STATIC_STRINGS
                                                                        | G_PARAM_CONSTRUCT)));

  g_object_class_install_property (G_OBJECT_CLASS (klass),
                                   PROP_NET_RUBBER_BAND_MODE,
                                   g_param_spec_boolean ("net-rubber-band-mode",
                                                         "Net Rubber Band Mode",
                                                         "Net Rubber Band Mode",
                                                         DEFAULT_NET_RUBBER_BAND_MODE,
                                                         (GParamFlags) (G_PARAM_READWRITE
                                                                        | G_PARAM_STATIC_STRINGS
                                                                        | G_PARAM_CONSTRUCT)));

  g_object_class_install_property (G_OBJECT_CLASS (klass),
                                   PROP_SNAP_MODE,
                                   g_param_spec_int ("snap-mode",
                                                     "Snap Mode",
                                                     "Snap Mode",
                                                     0,
                                                     SNAP_STATE_COUNT - 1,
                                                     SNAP_GRID,
                                                     (GParamFlags) (G_PARAM_READWRITE
                                                                    | G_PARAM_STATIC_STRINGS
                                                                    | G_PARAM_CONSTRUCT)));

  g_object_class_install_property (G_OBJECT_CLASS (klass),
                                   PROP_SNAP_SIZE,
                                   g_param_spec_int ("snap-size",
                                                     "Snap Size",
                                                     "Snap Size",
                                                     MINIMUM_SNAP_SIZE,
                                                     MAXIMUM_SNAP_SIZE,
                                                     DEFAULT_SNAP_SIZE,
                                                     (GParamFlags) (G_PARAM_READWRITE
                                                                    | G_PARAM_STATIC_STRINGS
                                                                    | G_PARAM_CONSTRUCT)));
}



/*! \private
 *  \brief Get a property
 *
 *  \param [in]     object
 *  \param [in]     param_id
 *  \param [in,out] value
 *  \param [in]     pspec
 */
static void
get_property (GObject *object, guint param_id, GValue *value, GParamSpec *pspec)
{
  GschemOptions *options = GSCHEM_OPTIONS (object);

  switch (param_id) {
    case PROP_GRID_MODE:
      g_value_set_int (value, gschem_options_get_grid_mode (options));
      break;

    case PROP_MAGNETIC_NET_MODE:
      g_value_set_boolean (value, gschem_options_get_magnetic_net_mode (options));
      break;

    case PROP_NET_RUBBER_BAND_MODE:
      g_value_set_boolean (value, gschem_options_get_net_rubber_band_mode (options));
      break;

    case PROP_SNAP_MODE:
      g_value_set_int (value, gschem_options_get_snap_mode (options));
      break;

    case PROP_SNAP_SIZE:
      g_value_set_int (value, gschem_options_get_snap_size (options));
      break;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, param_id, pspec);
  }
}



/*! \brief Initialize GschemOptions instance
 *
 *  \param [in,out] selection
 */
static void
gschem_options_init (GschemOptions *options)
{
}



/*! \brief Set a property
 *
 *  \param [in,out] object
 *  \param [in]     param_id
 *  \param [in]     value
 *  \param [in]     pspec
 */
static void
set_property (GObject *object, guint param_id, const GValue *value, GParamSpec *pspec)
{
  GschemOptions *options = GSCHEM_OPTIONS (object);

  switch (param_id) {
    case PROP_GRID_MODE:
      gschem_options_set_grid_mode (options, (GRID_MODE) g_value_get_int (value));
      break;

    case PROP_MAGNETIC_NET_MODE:
      gschem_options_set_magnetic_net_mode (options, g_value_get_boolean (value));
      break;

    case PROP_NET_RUBBER_BAND_MODE:
      gschem_options_set_net_rubber_band_mode (options, g_value_get_boolean (value));
      break;

    case PROP_SNAP_MODE:
      gschem_options_set_snap_mode (options, (SNAP_STATE) g_value_get_int (value));
      break;

    case PROP_SNAP_SIZE:
      gschem_options_set_snap_size (options, g_value_get_int (value));
      break;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, param_id, pspec);
  }
}
