/* Lepton EDA library
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2016 gEDA Contributors
 * Copyright (C) 2017-2026 Lepton EDA Contributors
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 */

#ifndef STRUCT_H
#define STRUCT_H

#include <glib.h>  /* Include needed to make GList work. */

/* Wrappers around a new list mechanism */

/* The list of selected objects of a page. */
typedef struct _LeptonList LeptonSelection;
/* The list of currently open pages. */
typedef struct _LeptonList LeptonPageList;

/* lepton-schematic structures */

/* The connection data. */
typedef struct st_conn LeptonConn;

/* Managed text buffers */
typedef struct _TextBuffer TextBuffer;

/* Component library data */
/* Component library source which may be a directory containing
 * symbol files, a command in the system PATH that can generate
 * the symbol data, or a Scheme function which can do likewise. */
typedef struct _CLibSource CLibSource;
/* The structure storing particular symbol data, namely its source
 * as defined above and name. */
typedef struct _CLibSymbol CLibSymbol;


/*! \brief Structure for connections between LeptonObjects
 *
 * The st_conn structure contains a single connection
 * to another object.
 * The connection system in s_conn.c uses this struct
 */
struct st_conn {
  /*! \brief The "other" object connected to this one */
  LeptonObject *other_object;
  /*! \brief type of connection. Always in reference to how the "other"
    object is connected to the current one */
  int type;
  /*! \brief x coord of the connection position */
  int x;
  /*! \brief y coord of the connection position */
  int y;
  /*! \brief which endpoint of the current object caused this connection */
  int whichone;
  /*! \brief which endpoint of the "other" object caused this connection */
  int other_whichone;
};

/*! \brief Type of callback function for object damage notification */
typedef int(*ChangeNotifyFunc)(void *, LeptonObject *);

#endif
