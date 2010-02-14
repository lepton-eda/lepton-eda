/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2010 gEDA Contributors (see ChangeLog for details)
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

#include <stdio.h>
#include <sys/types.h>
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include "libgeda_priv.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

/*! \brief */
struct st_menu {
  char *menu_name;
  SCM menu_items;
};

static int menu_index=0;

#define MAX_MENUS	32

/* and eventually make this unlimited */
/* hack hack */
static struct st_menu menu[MAX_MENUS];

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
int s_menu_return_num(void) 
{
  return(menu_index);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
SCM s_menu_return_entry(int index, char **menu_name) 
{
  if (menu_name == NULL) {
    return SCM_BOOL_F;
  }

  if (index > MAX_MENUS || index < 0) {
    *menu_name = NULL;
    return SCM_BOOL_F;
  }

  *menu_name = menu[index].menu_name;
  return(menu[index].menu_items);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
int s_menu_add_entry(char *new_menu, SCM menu_items) 
{
  if (new_menu == NULL) {
    return(-1); 
  }

  if (menu_index >= MAX_MENUS) {
    return(-1); 
  }

  menu[menu_index].menu_name = g_strdup (new_menu);
  scm_gc_protect_object (menu_items);
  menu[menu_index].menu_items = menu_items;
  menu_index++;
  
  return(menu_index);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void s_menu_print()
{
  int i;

  for (i = 0; i < menu_index; i++) {
    printf("Name; %s\n", menu[i].menu_name);
    scm_display (menu[i].menu_items, scm_current_output_port ());
    printf("\n");
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void s_menu_free()
{
  int i;

  for (i = 0; i < menu_index; i++) {
    if (menu[i].menu_name) {
      g_free(menu[i].menu_name);
      menu[i].menu_name = NULL;
      scm_gc_unprotect_object (menu[i].menu_items);
    }		
  }

  menu_index=0;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void s_menu_init()
{
  int i;
  for (i = 0; i < MAX_MENUS; i++) {
    menu[i].menu_name = NULL;	
  } 
}
