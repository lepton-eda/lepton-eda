/*  mk_verilog_syms.c: 
 *
 *  Create a set of n-input gate symbols for the geda verilog netlister.
 *
 *  symbols created:  and[2-9], nand[2-9], nor[2-9], or[2-9], nor[2-9],
 *  xor[2-9], xnor[2-9].
 *
 * Copyright (C) 1999 Mike Jarabek
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
 *
 */

#include <stdio.h>
#include <math.h>

/* local defines */

/* gEDA colour, these should probably come from somewhere else */
#define BLACK		0
#define WHITE		1
#define RED		2
#define GREEN		3
#define BLUE		4
#define YELLOW		5
#define CYAN		6
#define GREY		7


/* external prototypes */
int GetStringDisplayLength(char *str,int font_size);  /* char_width.c */



/* local prototypes */
int MakeSymbol(FILE *fp, unsigned int pins, int inputBubbles, 
	       int outputBubbles,
	       int (*body)(FILE *, int, int, unsigned int, unsigned int));

int AndBody(FILE *fp, int x, int y, unsigned int pins, unsigned int colour);
int OrBody(FILE *fp, int x, int y, unsigned int pins, unsigned int colour);
int XorBody(FILE *fp, int x, int y, unsigned int pins, unsigned int colour);
int Pin(FILE *fp, int x1, int y1, int x2, int y2, int bubble);
int PinAttribute(FILE *fp, int x, int y, unsigned int n, char *value); 
int WidenBody(FILE *fp, int x, int y, unsigned int pins, unsigned int colour);

/* globals */
unsigned int PinSpacing = 200;
unsigned int PinLength  = 300;

struct Table
{
  char         *name;     /* base name of part */
  unsigned int suffix;    /* suffix for part, 1 for normal, 2 for deMorgan.. */
  int (*body)(FILE *, int, int, unsigned int, unsigned int);  /* body style */
  int          inputBubbles;  /* where to draw bubbles */
  int          outputBubbles;
};


struct Table generate[] = {
  {"and",  1, AndBody, 0, 0},
  {"and",  2, OrBody,  1, 1},
  {"nand", 1, AndBody, 0, 1},
  {"nand", 2, OrBody,  1, 0},
  {"or",   1, OrBody,  0, 0},
  {"or",   2, AndBody, 1, 1},
  {"nor",  1, OrBody,  0, 1},
  {"nor",  2, AndBody, 1, 0},
  {"xor",  1, XorBody, 0, 0},
  {"xnor", 1, XorBody, 0, 1}
};

unsigned int nGenerate = sizeof(generate)/sizeof(struct Table);

int
main(int argc, char **argv)
{
  int i,j;
  int rc;
  char name[127];

  FILE *fp;

  printf("Making verilog symbols\n");

  for(i=0; i<nGenerate; i++) /* loop over table entries */
    for(j=2; j<10; j++)      /* loop over number of pins */
      {
	/* build file name */
	sprintf(name, "sym/%s%u-%u.sym",
		generate[i].name, j, generate[i].suffix);
	printf("Working on:%s\n",name);
	fp = fopen(name, "w");
	if(fp == NULL)
	  {
	    fprintf(stderr,"Error: Unable to create file `%s' in %s()\n",
		    name, __FUNCTION__);
	    return 1;
	  }

	rc = MakeSymbol(fp, j, 
			generate[i].inputBubbles, generate[i].outputBubbles,
			generate[i].body);
	if(rc)
	  {
	    fprintf(stderr,"Error: Symbol creation failed in %s()\n",
		    __FUNCTION__);
	    return 1;
	  }

	/* and finally add the device attribute */
	fprintf(fp,"T 400 0 5 8 0 0 0\ndevice=%s\n",generate[i].name);
	/* and the positional pin directive */
	fprintf(fp,"T 400 100 5 8 0 0 0\nVERILOG_PORTS=POSITIONAL\n");

	fclose(fp);
      }

  return 0;
}

/* output a complete symbol having the desired body
 * and requested number of pins.  Draw bubbles on the input and/or
 * output pins.
 */
int
MakeSymbol(FILE *fp, unsigned int pins, int inputBubbles, int outputBubbles,
	   int (*body)(FILE *, int, int, unsigned int, unsigned int))
{
  unsigned int i;
  int rc;

  int bodyx, bodyy;       /* origin of body */
  int outputx, outputy;   /* origin of output pin */
  int firstx, firsty;     /* first end of first input pin */
  int totalHeight;

  int pinx, piny;

  int bodyWidth  = 700;
  int bodyHeight = 600;
  unsigned int pinCount = 1;

  char pinName[20];       /* temp for pinnname */

  if(fp == NULL)
    {
      fprintf(stderr, "Error: NULL file pointer passed to %s()\n",
	      __FUNCTION__);
      return 1;
    }

  if(body == NULL)
    {
      fprintf(stderr, "Error: NULL body drawing function pointer passed "
	      "to %s()\n",
	      __FUNCTION__);
      return 1;
    }

  /* do special pin spacing for 2 pins */
  if(pins == 2)
    PinSpacing = 400;
  else
    PinSpacing = 200;


  /* first, compute locations needed */
  totalHeight = bodyHeight;
  if(pins >= 3) /* do we need to add to the height ? */
    totalHeight += (pins-3) * PinSpacing;

  
  bodyx = PinLength;
  bodyy = totalHeight/2;

  outputx = bodyx+bodyWidth;
  outputy = bodyy;

  firstx  = PinLength;
  firsty  = 100;

  /* draw the body */
  rc = (*body)(fp, bodyx, bodyy, pins, GREEN);
  if(rc)
    {
      fprintf(stderr, "Error: Body function failed in %s()\n",
	      __FUNCTION__);
      return 1;
    }

  /* draw the pins and attach appropriate attributes */
  rc = Pin(fp, outputx, outputy, outputx+PinLength, outputy, outputBubbles);
  if(rc)
    {
      fprintf(stderr, "Error: Pin drawing function failed in %s() "
	      "for output pin\n",
	      __FUNCTION__);
      return 1;
    }
  /* attach pin attribute */
  rc = PinAttribute(fp, outputx, outputy, pinCount++, "OUT"); 
  if(rc)
    {
      fprintf(stderr, "Error: Pin Attribute function failed for output pin "
	      "in %s()\n",__FUNCTION__);
      return 1;
    }

  /* do input pins */
  for(i=0; i<pins; i++)
    {
      /* calculate the position of the pin */
      pinx = firstx;
      piny = firsty+i*PinSpacing;
      /* output a pin */
      rc = Pin(fp, pinx, piny, pinx-PinLength, piny, inputBubbles);
      if(rc)
	{
	  fprintf(stderr,"Error: Pin drawing function failed for pin %u "
		  "in %s()\n",
		  i, __FUNCTION__);
	  return 1;
	}
      /* output the attributes */
      sprintf(pinName,"IN%u",i);
      rc = PinAttribute(fp, pinx, piny, pinCount++, pinName);
      if(rc)
	{
	  fprintf(stderr,"Error: Pin Attributes function failed for pin %u "
		  "in %s()\n", i, __FUNCTION__);
	  return 1;
	}

    }


  return 0;
}




/* produce an and shaped body at the given offset.
 * The origin is along the horizontal midline of the shape.
 */
int 
AndBody(FILE *fp, int x, int y, unsigned int pins, unsigned int colour)
{

  if(fp == NULL)
    {
      fprintf(stderr, "Error: NULL file pointer passed to %s()\n",
	      __FUNCTION__);
      return 1;
    }

  /* top and bottom lines */
  fprintf(fp, "L %d %d %d %d %u\n",
	  x, y+300, x+400, y+300, colour); 
  fprintf(fp, "L %d %d %d %d %u\n",
	  x, y-300, x+400, y-300, colour);
 
  /* left line */
  fprintf(fp, "L %d %d %d %d %u\n",
	  x, y-300, x, y+300, colour);

  /* arc at right */
  fprintf(fp, "A %d %d %d %d %d %u\n",
	  x+400, y, 300, -90, 180, colour);

  return WidenBody(fp, x, y, pins, colour);
}

/* produce a or shaped body at the given offset.
 * The origin is along the horizontal midline of the shape.
 */
int OrBody(FILE *fp, int x, int y, unsigned int pins, unsigned int colour)
{
  if(fp == NULL)
    {
      fprintf(stderr, "Error: NULL file pointer passed to %s()\n",
	      __FUNCTION__);
      return 1;
    }

  /* top and bottom lines */
  fprintf(fp, "L %d %d %d %d %u\n",
	  x, y+300, x+300, y+300, colour);
  fprintf(fp, "L %d %d %d %d %u\n",
	  x, y-300, x+300, y-300, colour);

  /* left arc */
  fprintf(fp, "A %d %d %d %d %d %u\n",
	  x-260, y, 400, -48, 97, colour); 

  /* right top and bottom arcs */
  fprintf(fp, "A %d %d %d %d %d %u\n",
	  x+300, y+100, 400, 270, 76, colour); 
  fprintf(fp, "A %d %d %d %d %d %u\n",
	  x+300, y-100, 400, 90, -76, colour);
 
  return WidenBody(fp, x, y, pins, colour);
}

/* produce a xor shaped body at the given offset.
 * The origin is along the horizontal midline of the shape.
 */
int XorBody(FILE *fp, int x, int y, unsigned int pins, unsigned int colour)
{
  if(fp == NULL)
    {
      fprintf(stderr, "Error: NULL file pointer passed to %s()\n",
	      __FUNCTION__);
      return 1;
    }

  /* top and bottom lines */
  fprintf(fp, "L %d %d %d %d %u\n",
	  x+100, y+300, x+300, y+300, colour);
  fprintf(fp, "L %d %d %d %d %u\n",
	  x+100, y-300, x+300, y-300, colour);

  /* left arc 1 */
  fprintf(fp, "A %d %d %d %d %d %u\n",
	  x-260, y, 400, -48, 97, colour);  
 /* left arc 2 */
  fprintf(fp, "A %d %d %d %d %d %u\n",
	  x-160, y, 400, -48, 97, colour); 
  

  /* right top and bottom arcs */
  fprintf(fp, "A %d %d %d %d %d %u\n",
	  x+300, y+100, 400, 270, 76, colour); 
  fprintf(fp, "A %d %d %d %d %d %u\n",
	  x+300, y-100, 400, 90, -76, colour); 

  return WidenBody(fp, x, y, pins, colour);
}


/* output two line segments that will serve to `widen' the
 * body of one of the above gates
 */
int 
WidenBody(FILE *fp, int x, int y, unsigned int pins, unsigned int colour)
{
  unsigned int distanceNeeded;

  if(fp == NULL)
    {
      fprintf(stderr, "Error: NULL file pointer passed to %s()\n",
	      __FUNCTION__);
      return 1;
    }

  /* only need to do work if the number of pins > 2 */
  if(pins <= 3)
    return 0;


  /* Compute the amount needed to add to both top and bottom */
  distanceNeeded = (pins-3) * PinSpacing/2;

  /* output the line segments */
  /* for the top */
  fprintf(fp, "L %d %d %d %d %u\n",
	  x, y+300, x, y+300+distanceNeeded, colour);
  /* for the bottom */
  fprintf(fp, "L %d %d %d %d %u\n",
	  x, y-300, x, y-300-distanceNeeded, colour);

  return 0;
}


/* Draw a pin, optionally with a bubble
 */
int 
Pin(FILE *fp, int x1, int y1, int x2, int y2, int bubble) 
{
  int bx, by;              /* bubble x and y locations */
  int px1, py1, px2, py2;  /* pin x and y locations */
  int dx, dy;
  int br = 50;             /* bubble radius */

  double x,y;              /* unit vector in direction of line */
  double denom;

  if(fp == NULL)
    {
      fprintf(stderr, "Error: NULL file pointer passed to %s()\n",
	      __FUNCTION__);
      return 1;
    }

  /* figure where pin ends are */
  if(bubble) /* is there a bubble? */
    { /* yes there is a bubble */

      /* compute a unit vector */
      dx = x2-x1;
      dy = y2-y1;
      denom = sqrt( dx*dx + dy*dy);
      if(denom < 1e-6)
	{
	  fprintf(stderr, "Error: Length of pin too small in %s()\n",
		  __FUNCTION__);
	  return 1;
	}
      
      x = dx / denom;
      y = dy / denom;

      /* figure center of bubble */
      bx = x1 + x * br;
      by = y1 + y * br;

      /* figure location of first line end */
      px1 = x1 + x * 2 * br;
      py1 = y1 + y * 2 * br;
      
      px2 = x2;
      py2 = y2;
      

      /* draw the bubble */
      fprintf(fp, "V %d %d %d %d\n",
	      (int) bx, (int) by, br, CYAN);

    }
  else
    { /* no bubble, nothing special to do */

      px1 = x1; py1 = y1;
      px2 = x2; py2 = y2;

    }

  /* draw the pin */
  fprintf(fp, "P %d %d %d %d %d\n",
	  px1, py1, px2, py2, WHITE);

  return 0;
}

int 
PinAttribute(FILE *fp, int x, int y, unsigned int n, char *value) 
{
  if(fp == NULL)
    {
      fprintf(stderr, "Error: NULL file pointer passed to %s()\n",
	      __FUNCTION__);
      return 1;
    }
  
  if(value == NULL)
    {
      fprintf(stderr, "Error: NULL value pointer passed to %s()\n",
	      __FUNCTION__);
      return 1;
    }

  fprintf(fp, "{\n");
  fprintf(fp, "T %d %d %u %u 0 0 0\n",x, y, YELLOW, 8);
  fprintf(fp, "pin%u=%s\n",n, value);
  fprintf(fp, "}\n");
  
  return 0;
}
