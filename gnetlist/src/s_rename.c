/* gEDA - GPL Electronic Design Automation
 * gnetlist - gEDA Netlist
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
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111 USA
 */

#include <config.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <ctype.h>

#include <libgeda/libgeda.h>

#include "../include/globals.h"
#include "../include/prototype.h"

typedef struct {
    char *src;
    char *dest;
} RENAME;

#define MAX_RENAME 64
#define MAX_SETS 10

/* size is fixed... TODO: maybe make this dynamic */
static RENAME rename_pairs[MAX_SETS][MAX_RENAME];

static int rename_counter = 0;
static int cur_set = 0;

void s_rename_init(void)
{
    int i, j;

    for (i = 0; i < MAX_SETS; i++) {
	for (j = 0; j < MAX_RENAME; j++) {
	    rename_pairs[i][j].src = NULL;
	    rename_pairs[i][j].dest = NULL;
	}
    }
    rename_counter = 0;
    cur_set = 0;
}

void s_rename_destroy_all(void)
{
    int i, j;

    for (i = 0; i < MAX_SETS; i++) {
	for (j = 0; j < MAX_RENAME; j++) {

	    if (rename_pairs[i][j].src) {
		free(rename_pairs[i][j].src);
		rename_pairs[i][j].src = NULL;
	    }

	    if (rename_pairs[i][j].dest) {
		free(rename_pairs[i][j].dest);
		rename_pairs[i][j].dest = NULL;
	    }
	}
    }
    rename_counter = 0;
    cur_set = 0;
}

void s_rename_next_set(void)
{
    if (cur_set == MAX_SETS) {
	fprintf(stderr,
		"Increase number of rename_pair sets in s_net.c\n");
	exit(-1);
    }
    cur_set++;
    rename_counter = 0;
}

void s_rename_print(void)
{
    int i,j;

    for (i = 0; i < MAX_SETS; i++) {
	for (j = 0; j < MAX_RENAME; j++) {
	    if (rename_pairs[i][j].src) {
		printf("%d) Source: _%s_", i, rename_pairs[i][j].src);
	    }

	    if (rename_pairs[i][j].dest) {
		printf(" -> Dest: _%s_\n", rename_pairs[i][j].dest);
	    } 
	}
    }
}

/* if the src is found, return true */
/* if the dest is found, also return true, but warn user */
/* If quiet_flag is true than don't print anything */
int s_rename_search(char *src, char *dest, int quiet_flag)
{
    int i;
    for (i = 0; i < rename_counter; i++) {

	if (rename_pairs[cur_set][i].src && rename_pairs[cur_set][i].dest) {

	    if (strcmp(src, rename_pairs[cur_set][i].src) == 0) {
		return (TRUE);
	    }

	    if (strcmp(dest, rename_pairs[cur_set][i].src) == 0) {
		if (!quiet_flag) {
		    fprintf(stderr,
			    "WARNING: Trying to rename something twice:\n\t%s and %s\nare both a src and dest name\n",
			    dest, rename_pairs[cur_set][i].src);
		    fprintf(stderr,
			    "This warning is okay if you have multiple levels of hierarchy!\n");
		}
		return (TRUE);
	    }
	}

    }

    return (FALSE);
}

void s_rename_add(char *src, char *dest)
{
    int flag;
    int i;

    if (src == NULL || dest == NULL) {
	return;
    }

    flag = s_rename_search(src, dest, FALSE);

    if (flag) {
	for (i = 0; i < rename_counter; i++) {
	    if (rename_pairs[cur_set][i].src
		&& rename_pairs[cur_set][i].dest) {
		if (strcmp(dest, rename_pairs[cur_set][i].src) == 0) {
#if DEBUG
		    printf
			("Found dest [%s] in src [%s] and that had a dest as: [%s]\nSo you want rename [%s] to [%s]\n",
			 dest, rename_pairs[cur_set][i].src,
			 rename_pairs[cur_set][i].dest,
			 src, rename_pairs[cur_set][i].dest);
#endif

		    rename_pairs[cur_set][rename_counter].src =
			(char *) malloc(sizeof(char) * (strlen(src) + 1));
		    strcpy(rename_pairs[cur_set][rename_counter].src, src);
		    rename_pairs[cur_set][rename_counter].dest =
			(char *) malloc(sizeof(char) *
					(strlen
					 (rename_pairs[cur_set][i].dest) +
					 1));
		    strcpy(rename_pairs[cur_set][rename_counter].dest,
			   rename_pairs[cur_set][i].dest);
		    rename_counter++;
		}
	    }
	}
    } else {

	rename_pairs[cur_set][rename_counter].src =
	    (char *) malloc(sizeof(char) * (strlen(src) + 1));
	strcpy(rename_pairs[cur_set][rename_counter].src, src);
	rename_pairs[cur_set][rename_counter].dest =
	    (char *) malloc(sizeof(char) * (strlen(dest) + 1));
	strcpy(rename_pairs[cur_set][rename_counter].dest, dest);
	rename_counter++;
    }

}

void s_rename_all_lowlevel(NETLIST * netlist_head, char *src, char *dest)
{
    NETLIST *nl_current = NULL;
    CPINLIST *pl_current;

    nl_current = netlist_head;

    while (nl_current != NULL) {
	if (nl_current->cpins) {
	    pl_current = nl_current->cpins;
	    while (pl_current != NULL) {

		if (pl_current->net_name != NULL) {

		    if (strcmp(pl_current->net_name, src) == 0) {

			/* this is a bad idea */
			/* because inside nets-> */
			/* there is another pointer */
			/*free(pl_current->net_name); */

			pl_current->net_name =
			    malloc(sizeof(char) * (strlen(dest) + 1));
			strcpy(pl_current->net_name, dest);
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
    int i;

#if DEBUG
    s_rename_print();
#endif

    for (i = 0; i < rename_counter; i++) {

	verbose_print("R");

#if DEBUG 
	printf("%d Renaming: %s -> %s\n", i, rename_pairs[cur_set][i].src,
	       rename_pairs[cur_set][i].dest);
#endif

	s_rename_all_lowlevel(netlist_head,
			      rename_pairs[cur_set][i].src,
			      rename_pairs[cur_set][i].dest);
    }
}


SCM g_get_renamed_nets(SCM scm_level)
{
    SCM pairlist = SCM_EOL;
    SCM outerlist = SCM_EOL;
    int i = 0, j = 0;
    char *level;

    level = gh_scm2newstr(scm_level, NULL);
    free(level);

    for (i = 0; i < MAX_SETS; i++) {
	for (j = 0; j < MAX_RENAME; j++) {
	    if (rename_pairs[i][j].src && rename_pairs[i][j].dest) {
		pairlist = gh_list(gh_str2scm(rename_pairs[i][j].src,
					      strlen(rename_pairs[i]
						     [j].src)),
				   gh_str2scm(rename_pairs[i][j].dest,
					      strlen(rename_pairs[i]
						     [j].dest)),
				   SCM_UNDEFINED);
		outerlist = gh_cons(pairlist, outerlist);
	    }
	}
    }

    return (outerlist);
}
