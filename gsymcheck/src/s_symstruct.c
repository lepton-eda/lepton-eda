/* gEDA - GPL Electronic Design Automation
 * gsymcheck - gEDA Symbol Check 
 * Copyright (C) 1998-2000 Ales V. Hvezda
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
	
  s_symcheck = (SYMCHECK *) malloc(sizeof(SYMCHECK));

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

  s_symcheck->numpins=0;
  s_symcheck->error_count=0;

  return(s_symcheck);
}

void
s_symstruct_print(SYMCHECK *s_current)
{
  GList *list;
  char *msg;
  
  list = s_current->info_messages;
  while (list != NULL) {
     msg = (char *) list->data;     
   /* printf("found info: %s\n", msg); */
     if (msg && verbose_mode) { 
       s_log_message("Info: %s", msg);
     }

     if (msg)
       free(msg);

     list = list->next;
  } 

  list = s_current->warning_messages;
  while (list != NULL) {
     msg = (char *) list->data;     
     
   /* printf("found warning: %s\n", msg); */
     if (msg && verbose_mode) { 
       s_log_message("Warning: %s", msg);
     }

     if (msg)
       free(msg);

     list = list->next;
  } 

  list = s_current->error_messages;
  while (list != NULL) {
     msg = (char *) list->data;     
     
   /* printf("found error: %s\n", msg); */
     if (msg && verbose_mode) { 
       s_log_message("ERROR: %s", msg);
     }

     if (msg)
       free(msg);

     list = list->next;
  } 
   
#if 0	
  if (s_current->slotting_errors) {
    if (verbose_mode) s_log_message("- ERROR: number of slotting errors: %d\n", s_current->slotting_errors);
  }

  if (s_current->found_net) {
    if (verbose_mode) s_log_message("- ERROR: number of nets found inside: %d\n", s_current->found_net);
  }

  if (s_current->found_bus) {
    if (verbose_mode) s_log_message("- ERROR: number of buses found inside: %d\n", s_current->found_bus);
  }

  if (s_current->found_connection) {
    if (verbose_mode) s_log_message("- ERROR: number of connections found inside: %d\n", s_current->found_connection);
  }
  
  if (s_current->found_oldpin_attrib) {
    if (verbose_mode) s_log_message("- ERROR: found old pin#=# attribute(s)\n");
  }

  if (s_current->found_oldslot_attrib) {
    if (verbose_mode) s_log_message("- ERROR: found old slot#=# attribute(s)\n");
  }
#endif
}

void
s_symstruct_free(SYMCHECK *s_current)
{
  if (s_current) {

    if (s_current->device_attribute) {
      free(s_current->device_attribute);
    }

    free(s_current);
  }
}
