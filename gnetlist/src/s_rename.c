/* gEDA - GPL Electronic Design Automation
 * gnetlist - gEDA Netlist
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */

 /*
  * 2005/05/02  Almost totally reimplemented to support dynamic allocation.
  *
  * Changes are Copyright (C) 2005 Carlos A. R. Azevedo
  */
  
#include <config.h>

#include <stdio.h>
#include <ctype.h>
#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_ASSERT_H
#include <assert.h>
#endif

#include <libgeda/libgeda.h>

#include "../include/globals.h"
#include "../include/prototype.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

typedef struct {
    void * next;
    char * src;
    char * dest;
} RENAME;

typedef struct {
    void * next_set;
    RENAME * first_rename;
    RENAME * last_rename;
} SET;

static SET * first_set = NULL;
static SET * last_set = NULL;

void s_rename_init(void)
{
    if (first_set)
    {
        fprintf(stderr,"ERROR: Overwriting a valid rename list.\n");
        exit(-1);
    }
}

void s_rename_destroy_all(void)
{
    RENAME * temp;
    void * to_free;
    
    for (; first_set;)
    {
        for (temp = first_set->first_rename; temp;)
        {
            g_free(temp->src);
            g_free(temp->dest);
	    to_free = temp;
	    temp = temp->next;
	    g_free(to_free);
        }
	to_free = first_set;
	first_set = first_set->next_set;
	g_free(to_free);
    }
    last_set = NULL;
}

void s_rename_next_set(void)
{
    SET * new_set;
    
    new_set = g_malloc(sizeof(SET));
    memset(new_set,0,sizeof(SET));
    if (first_set)
    {
        last_set->next_set = new_set;
	last_set = new_set;
    }
    else
    {
        first_set = last_set = new_set;
    }
}

void s_rename_print(void)
{
    SET * temp_set;
    RENAME * temp_rename;
    int i;

    for (i = 0, temp_set = first_set; temp_set; temp_set = temp_set->next_set, i++)
    {
        for (temp_rename = temp_set->first_rename; temp_rename; temp_rename = temp_rename->next)
        {
            printf("%d) Source: _%s_", i, temp_rename->src);
            printf(" -> Dest: _%s_\n", temp_rename->dest);
        } 
    }
}

/* if the src is found, return true */
/* if the dest is found, also return true, but warn user */
/* If quiet_flag is true than don't print anything */
int s_rename_search(char *src, char *dest, int quiet_flag)
{
    RENAME * temp;

    if (last_set)
    {
        for (temp = last_set->first_rename; temp; temp = temp->next)
        {
            if (strcmp(src, temp->src) == 0) 
            {
                return (TRUE);
            }

            if (strcmp(dest, temp->src) == 0) 
	    {
                if (!quiet_flag) 
	        {
                    fprintf(stderr,"WARNING: Trying to rename something twice:\n\t%s and %s\nare both a src and dest name\n", dest, temp->src);
                    fprintf(stderr,"This warning is okay if you have multiple levels of hierarchy!\n");
                }
                return (TRUE);
            }
	}
    }
    return (FALSE);
}

static void s_rename_add_lowlevel (const char *src, const char *dest)
{
    RENAME *new_rename;

    g_return_if_fail(last_set != NULL);

    new_rename = g_malloc(sizeof (RENAME));

    g_return_if_fail(new_rename != NULL);

    new_rename->next = NULL;
    new_rename->src = g_strdup(src);
    new_rename->dest = g_strdup(dest);

    if (last_set->first_rename == NULL)
    {
        last_set->first_rename = last_set->last_rename = new_rename;
    }
    else
    {
        last_set->last_rename->next = new_rename;
        last_set->last_rename = new_rename;
    }
}

void s_rename_add(char *src, char *dest)
{
    int flag;
    RENAME * last;
    RENAME * temp;
    RENAME * new_rename;
    SET * new_set;

    if (src == NULL || dest == NULL) 
    {
        return;
    }

    flag = s_rename_search(src, dest, FALSE);

    if (flag) 
    {
        /* If found follow the original behaviour, limiting the operation to the current end-of-list */
	last = last_set->last_rename;
	for (temp = last_set->first_rename; ; temp = temp->next)
	{
        if ((strcmp(dest, temp->src) == 0)
            && (strcmp(src, temp->dest) != 0))
        {
            /* we found a -> b, while adding c -> a.
             * hence we would have c -> a -> b, so add c -> b.
             * avoid renaming if b is same as c!
             */
#if DEBUG
            printf("Found dest [%s] in src [%s] and that had a dest as: [%s]\n"
                   "So you want rename [%s] to [%s]\n",
                   dest, temp->src, temp->dest, src, temp->dest);
#endif
            s_rename_add_lowlevel(src, temp->dest);

        }
        else if ((strcmp(src, temp->src) == 0)
                 && (strcmp(dest, temp->dest) != 0))
        {
            /* we found a -> b, while adding a -> c.
             * hence b <==> c, so add c -> b.
             * avoid renaming if b is same as c!
             */
#if DEBUG
            printf("Found src [%s] that had a dest as: [%s]\n"
                   "Unify nets by renaming [%s] to [%s]\n",
                   src, temp->dest, dest, temp->dest);
#endif
            s_rename_add_lowlevel(dest, temp->dest);
        }
            if (temp == last)
            {
                break;
            }
        }
    } 
    else 
    {
        /* Check for a valid set */
	if (first_set == NULL)
	{
	    new_set = g_malloc(sizeof(SET));
	        memset(new_set,0,sizeof(SET));
		first_set = last_set = new_set;
	}    
        new_rename = g_malloc(sizeof(RENAME));
	     new_rename->next = NULL;
             new_rename->src = g_strdup(src);
             new_rename->dest = g_strdup(dest);
	     if (last_set->first_rename == NULL)
	     {
	         last_set->first_rename = last_set->last_rename = new_rename;
	     }
	     else
	     {
	         last_set->last_rename->next = new_rename;
		 last_set->last_rename = new_rename;
	     } 
    }
}

void s_rename_all_lowlevel(NETLIST * netlist_head, char *src, char *dest)
{
    NETLIST *nl_current = NULL;
    CPINLIST *pl_current;

    nl_current = netlist_head;

    while (nl_current != NULL) 
    {
        if (nl_current->cpins) 
	{
            pl_current = nl_current->cpins;
            while (pl_current != NULL) 
	    {
                if (pl_current->net_name != NULL) 
		{
                    if (strcmp(pl_current->net_name, src) == 0) 
		    {
                        pl_current->net_name = g_strdup(dest);
                    }
                }
                pl_current = pl_current->next;
            }
        }
        nl_current = nl_current->next;
    }
}

void s_rename_all(TOPLEVEL * pr_current, NETLIST * netlist_head)
{
    RENAME * temp;
    
#if DEBUG
    s_rename_print();
#endif

    if (last_set)
    {
        for (temp = last_set->first_rename; temp; temp = temp->next)
        {
            verbose_print("R");
            s_rename_all_lowlevel(netlist_head, temp->src, temp->dest);
	}
    }
}


SCM g_get_renamed_nets(SCM scm_level)
{
    SCM pairlist = SCM_EOL;
    SCM outerlist = SCM_EOL;
    SET * temp_set;
    RENAME * temp_rename;
    char *level;

    level = SCM_STRING_CHARS (scm_level);

    for (temp_set = first_set; temp_set; temp_set = temp_set->next_set)
    {
        for (temp_rename = temp_set->first_rename; temp_rename; temp_rename = temp_rename->next)
        {
            pairlist = scm_list_n (scm_makfrom0str (temp_rename->src), scm_makfrom0str (temp_rename->dest), SCM_UNDEFINED);
            outerlist = scm_cons (pairlist, outerlist);
        }
    }
    return (outerlist);
}
