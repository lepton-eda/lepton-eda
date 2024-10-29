/* Lepton EDA library
 * Copyright (C) 2016 Peter Brett <peter@peter-b.co.uk>
 * Copyright (C) 2016 gEDA Contributors
 * Copyright (C) 2017-2025 Lepton EDA Contributors
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

#ifndef __EDA_PATHS_H__
#define __EDA_PATHS_H__

G_BEGIN_DECLS

/* ----------------------------------------------------------------
 * Lepton EDA standard directories
 * ---------------------------------------------------------------- */

const gchar * const *eda_get_system_data_dirs(void);
const gchar * const *eda_get_system_config_dirs(void);
const gchar *eda_get_user_data_dir(void);
const gchar *eda_get_user_config_dir(void);
const gchar *eda_get_user_cache_dir(void);

/* ----------------------------------------------------------------
 * Initialisation
 * ---------------------------------------------------------------- */

void eda_paths_init (void);

G_END_DECLS

#endif /* !__EDA_PATHS_H__ */
