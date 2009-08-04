
/*----------------------------------------------------------------*/
/*! \file
 * Linked list sorting code taken from 
 * http://www.chiark.greenend.org.uk/~sgtatham/algorithms/listsort.html
 * and hacked to serve in gattrib by SDB.
 *
 */

/*
 * Demonstration code for sorting a linked list.
 * 
 * The algorithm used is Mergesort, because that works really well
 * on linked lists, without requiring the O(N) extra space it needs
 * when you do it on arrays.
 * 
 * This code can handle singly and doubly linked lists, and
 * circular and linear lists too. For any serious application,
 * you'll probably want to remove the conditionals on `is_circular'
 * and `is_double' to adapt the code to your own purpose. 
 * 
 */

/*
 * This file is copyright 2001 Simon Tatham.
 * 
 * Permission is hereby granted, free of charge, to any person
 * obtaining a copy of this software and associated documentation
 * files (the "Software"), to deal in the Software without
 * restriction, including without limitation the rights to use,
 * copy, modify, merge, publish, distribute, sublicense, and/or
 * sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following
 * conditions:
 * 
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
 * OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT.  IN NO EVENT SHALL SIMON TATHAM BE LIABLE FOR
 * ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF
 * CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

/* ---  We don't need these 'cause they are already defined elsewhere --- 
 * #define FALSE 0 
 * #define TRUE 1
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <ctype.h>

#ifdef HAVE_STRING_H
#include <string.h>
#endif

/*------------------------------------------------------------------
 * Gattrib specific includes
 *------------------------------------------------------------------*/
#include <libgeda/libgeda.h>       /* geda library fcns  */
#include "../include/struct.h"     /* typdef and struct declarations */
#include "../include/prototype.h"  /* function prototypes */
#include "../include/globals.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif


/*----------------------------------------------------------------*/
/*! \brief Compare values of string data
 *
 * Comparison function -- compare values of string data.
 * \param al pointer to first STRING_LIST item to be compared
 * \param bl pointer to second STRING_LIST item to be compared
 * \returns +ve if al > bl, -ve if al < bl, 0 if al = bl
 */
/*----------------------------------------------------------------*/
int cmp(STRING_LIST *al, STRING_LIST *bl) {
  char *a = al->data;
  char *b = bl->data;

  if (al->pos != bl->pos)
    return al->pos - bl->pos;

  while (*a && *b)
    {
      if (isdigit ((int) *a) && isdigit ((int) *b))
	{
	  int ia = atoi (a);
	  int ib = atoi (b);
	  if (ia != ib)
	    return ia - ib;
	  while (isdigit ((int) *a))
	    a++;
	  while (isdigit ((int) *b))
	    b++;
	}
      else if (tolower ((int) *a) != tolower ((int) *b))
	return tolower ((int) *a) - tolower ((int) *b);
      a++;
      b++;
    }
  if (*a)
    return 1;
  if (*b)
    return -1;
  return 0;
}

/*----------------------------------------------------------------*/
/*! \brief Sort the linked list
 *
 * This is the actual sort function. Notice that it returns the new
 * head of the list. (It has to, because the head will not
 * generally be the same element after the sort.) So unlike sorting
 * an array, where you can do
 * 
 * - sort(myarray);
 * 
 * you now have to do
 * 
 * - list = listsort(mylist);
 *
 * \param list The linked STRING_LIST to be sorted
 * \param is_circular TRUE if this is a circularly linked list
 * \param is_double TRUE if this is a doubly-linked list
 * \returns a pointer to the new head of the list
 */
/*----------------------------------------------------------------*/
STRING_LIST *listsort(STRING_LIST *list, int is_circular, int is_double) {
    STRING_LIST *p, *q, *e, *tail, *oldhead;
    int insize, nmerges, psize, qsize, i;

    /*
     * Silly special case: if `list' was passed in as NULL, return
     * NULL immediately.
     */
    if (!list)
	return NULL;

    insize = 1;

    while (1) {
        p = list;
	oldhead = list;		       /* only used for circular linkage */
        list = NULL;
        tail = NULL;

        nmerges = 0;  /* count number of merges we do in this pass */

        while (p) {
            nmerges++;  /* there exists a merge to be done */
            /* step `insize' places along from p */
            q = p;
            psize = 0;
            for (i = 0; i < insize; i++) {
                psize++;
		if (is_circular)
		    q = (q->next == oldhead ? NULL : q->next);
		else
		    q = q->next;
                if (!q) break;
            }

            /* if q hasn't fallen off end, we have two lists to merge */
            qsize = insize;

            /* now we have two lists; merge them */
            while (psize > 0 || (qsize > 0 && q)) {

                /* decide whether next element of merge comes from p or q */
                if (psize == 0) {
		    /* p is empty; e must come from q. */
		    e = q; q = q->next; qsize--;
		    if (is_circular && q == oldhead) q = NULL;
		} else if (qsize == 0 || !q) {
		    /* q is empty; e must come from p. */
		    e = p; p = p->next; psize--;
		    if (is_circular && p == oldhead) p = NULL;
		} else if (cmp(p,q) <= 0) {
		    /* First element of p is lower (or same);
		     * e must come from p. */
		    e = p; p = p->next; psize--;
		    if (is_circular && p == oldhead) p = NULL;
		} else {
		    /* First element of q is lower; e must come from q. */
		    e = q; q = q->next; qsize--;
		    if (is_circular && q == oldhead) q = NULL;
		}

                /* add the next element to the merged list */
		if (tail) {
		    tail->next = e;
		} else {
		    list = e;
		}
		if (is_double) {
		    /* Maintain reverse pointers in a doubly linked list. */
		    e->prev = tail;
		}
		tail = e;
            }

            /* now p has stepped `insize' places along, and q has too */
            p = q;
        }
	if (is_circular) {
	    tail->next = list;
	    if (is_double)
		list->prev = tail;
	} else
	    tail->next = NULL;

        /* If we have done only one merge, we're finished. */
        if (nmerges <= 1)   /* allow for nmerges==0, the empty list case */
            return list;

        /* Otherwise repeat, merging lists twice the size */
        insize *= 2;
    }
}

