/* Lepton EDA attribute editor
 * Copyright 2001 Simon Tatham.
 * Copyright 2004-2013 gEDA Contributors
 * Copyright 2017-2026 Lepton EDA Contributors
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
#include <liblepton/liblepton.h>
#include "../include/struct.h"     /* typdef and struct declarations */
#include "../include/prototype.h"  /* function prototypes */
#include "../include/globals.h"


/*----------------------------------------------------------------*/
/*! \brief Compare values of string data
 *
 * Comparison function -- compare values of string data.
 * \param a [in] pointer to first string to be compared
 * \param b [in] pointer to second string to be compared
 * \returns +1 if a > b, -1 if a < b, 0 if a = b
 */
/*----------------------------------------------------------------*/
int
cmp (char *a, char *b) {
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
