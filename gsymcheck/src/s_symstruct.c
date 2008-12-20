/* gEDA - GPL Electronic Design Automation
 * gsymcheck - gEDA Symbol Check 
 * Copyright (C) 1998-2008 Ales Hvezda
 * Copyright (C) 1998-2008 gEDA Contributors (see ChangeLog for details)
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
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#include <config.h>

#include <stdio.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <libgeda/libgeda.h>

#include "../include/struct.h"
#include "../include/globals.h"
#include "../include/prototype.h"

/* call this for every symbol that needs to be checked */
SYMCHECK *
s_symstruct_init(void)
{
  SYMCHECK *s_symcheck;
	
  s_symcheck = (SYMCHECK *) g_malloc(sizeof(SYMCHECK));

  s_symcheck->info_messages = NULL;
  s_symcheck->warning_messages = NULL;
  s_symcheck->error_messages = NULL;

  s_symcheck->graphical_symbol=FALSE;
  s_symcheck->missing_device_attrib=FALSE;
  s_symcheck->device_attribute_incorrect=FALSE;
  s_symcheck->device_attribute=NULL;

  s_symcheck->missing_pinseq_attrib=FALSE;
  s_symcheck->multiple_pinseq_attrib=FALSE;
  s_symcheck->duplicate_pinseq_attrib=FALSE;

  s_symcheck->missing_pinnumber_attrib=FALSE;
  s_symcheck->multiple_pinnumber_attrib=FALSE;
  s_symcheck->duplicate_pinnumber_attrib=FALSE;

  s_symcheck->missing_numslots_attrib=FALSE;
  s_symcheck->slotting_errors=FALSE;
  s_symcheck->found_oldpin_attrib=FALSE;
  s_symcheck->found_oldslot_attrib=FALSE;
  s_symcheck->unattached_attribs=FALSE;
  s_symcheck->found_net=FALSE;
  s_symcheck->found_bus=FALSE;
  s_symcheck->found_connection=FALSE;
  s_symcheck->found_footprint=FALSE;
  s_symcheck->found_refdes=FALSE;

  s_symcheck->numpins=0;
  s_symcheck->numnetpins=0;
  s_symcheck->numslots=0;
  s_symcheck->numslotpins=0;
  s_symcheck->error_count=0;
  s_symcheck->warning_count=0;

  s_symcheck->missing_pintype_attrib=FALSE;
  s_symcheck->multiple_pintype_attrib=FALSE;
  s_symcheck->duplicate_pintype_attrib=FALSE;

  return(s_symcheck);
}

void
s_symstruct_print(SYMCHECK *s_current)
{
  GList *list;
  char *msg;

  if (verbose_mode > 2) {
    list = s_current->info_messages;
    while (list != NULL) {
      msg = (char *) list->data;     
      /* printf("found info: %s\n", msg); */
      if (msg) { 
        s_log_message("Info: %s", msg);
        g_free(msg);
      }

      list = g_list_next(list);
    }
  }

  if (verbose_mode > 1) {
    list = s_current->warning_messages;
    while (list != NULL) {
      msg = (char *) list->data;     
     
      /* printf("found warning: %s\n", msg); */
      if (msg) { 
        s_log_message("Warning: %s", msg);
        g_free(msg);
      }

      list = g_list_next(list);
    }
  }

  if (verbose_mode > 0) {
    list = s_current->error_messages;
    while (list != NULL) {
      msg = (char *) list->data;     
     
      /* printf("found error: %s\n", msg); */
      if (msg && verbose_mode) { 
        s_log_message("ERROR: %s", msg);
        g_free(msg);
      }

      list = g_list_next(list);
    }
  }
}

void
s_symstruct_free(SYMCHECK *s_current)
{
  if (s_current) {

    g_free(s_current->device_attribute);

    g_free(s_current);
  }
}
