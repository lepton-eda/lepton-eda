/* gEDA - GNU Electronic Design Automation
 * libgeda - include files
 * Copyright (C) 1998 Ales V. Hvezda
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
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 */

/* this file is no longer used */

#define MILS_PER	1000
#define BORDER 		1000	
/* if you change this one you must recalc correction factors */
/* perhaps we will do this in the code ! */

/* To calculate the correction factor for each: */
/* if calcaspect < required_aspect : */
/* 	Width_correction = (width+border) - (height+border)*1.3333 */
/* else */
/* 	Height_correction = ((width+border)/1.3333) - (height+border) */


#define WIDTH_A		11*MILS_PER+BORDER+667
#define HEIGHT_A	8.5*MILS_PER+BORDER

#define WIDTH_B		17*MILS_PER+BORDER
#define HEIGHT_B	11*MILS_PER+BORDER+1500

#define WIDTH_C		22*MILS_PER+BORDER+1000
#define HEIGHT_C	17*MILS_PER+BORDER

#define WIDTH_D		34*MILS_PER+BORDER
#define HEIGHT_D	22*MILS_PER+BORDER+3250

#define WIDTH_E		44*MILS_PER+BORDER+1667
#define HEIGHT_E	34*MILS_PER+BORDER

