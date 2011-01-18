
/*

    geda_sym_format.h - gEDA symbol write function
    Copyright (C) 2002-2010 Mario Pascucci <m.pas@libero.it>

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
    or
    visit the URL http://www.fsf.org/

*/



/* general sizes */
#define SCALE		100
#define PIN_SPACE	3*SCALE
#define PIN_LEN		3*SCALE
#define LABEL_SPACE	2*SCALE
#define DOT_SIZE	SCALE
#define CLK_SIZE	SCALE


/* text alignment */
#define EDATEXT_LB	0
#define EDATEXT_LC	1
#define EDATEXT_LT	2
#define EDATEXT_CB	3
#define EDATEXT_CC	4
#define EDATEXT_CT	5
#define EDATEXT_RB	6
#define EDATEXT_RC	7
#define EDATEXT_RT	8


/* color macros */
#define EDACOLOR_REF	8     /* detached attrib = RED */
#define EDACOLOR_LABEL	9     /* text and label = GREEN */
#define EDACOLOR_ATTRIB	5     /* attached attribute = YELLOW */
#define EDACOLOR_PIN	1     /* pin = WHITE */
#define EDACOLOR_DOT	6     /* logic bubble = CYAN */
#define EDACOLOR_BODY	3     /* body part = GREEN */


/* text size macros */
#define EDASIZE_LABEL	10    /* label size */
#define EDASIZE_ATTRIB	10    /* attribute */
#define EDASIZE_PIN	8     /* pin number size */
#define EDASIZE_TYPE	8     /* type of pin */



/* scale for text */
static int text_scale[] = {
  0,
  8,
  10
};


/*
 * Format of gEDA .sym files
 *
 * text
 * T x y color size visibility show_name_value angle alignment
 *
 * pin
 * P x1 y1 x2 y2 color
 *
 * line
 * L x1 y1 x2 y2 color width end type length space
 *
 * box
 * type  x  y  width height color width end type length space filling fillwidth angle1 pitch1 angle2 pitch2
 *     Example:
 *     B 33000 67300 2000 2000 3 60 0 2 75 50 0 -1 -1 -1 -1 -1
 *
 * circle
 *     V 38000 67000 900 3 0 0 2 75 50 0 -1 -1 -1 -1 -1
 *     type  x  y radius color width end type length space filling fillwidth angle1 pitch1 angle2 pitch2
 *
 * arc
 *     type  x  y  radius  start_angle  sweep_angle  color width end type length space
 *     Example:
 *     A 30600 75000 2000 0 45 3 0 0 3 75 50
 * 
 */


#define PI    3.141592653588



int initangle(float dx, float dy)
{
  float a,b;

  if (dx == 0.0)
  {
    if (dy >= 0.0) return 90;
    else return 270;
  }
  if (dy == 0.0)
  {
    if (dx >= 0.0) return 0;
    else return 180;
  }
  a = atan2(dy,dx);
  b = 180.0/PI * a;
  return (int) b;
}



/* write down the gEDA symbol file 
    most of the work must be done for
    correct symbol rendering
*/
int write_sym(void)
{
  int i,xs,ys,xe,ye;
  int xt,yt,xb,yb;
  int labely;
  int txtangle;
  int pinx,piny,pinalign;     /* pin number */
  int npinx,npiny,npinalign;  /* pin name */
  int tpinx,tpiny,tpinalign;   /* pin type */
  
  xe = ye = xs = ys = txtangle = pinx = piny = pinalign =
    npinx = npiny = npinalign = tpinx = tpiny = tpinalign = 0;
  if (pincount == 0)
  {
    fprintf(stderr,"Unable to write down sym file, no pin definition available\n");
    return -1;
  }
  /* if 'VECTOR' keyword was found, but no draw instruction
   * makes file .sym.part that stands for "partial" symbol */
  if (vector_found && vector_count == 0)
    strcat(fnsym,".part");
  if (!(fsym = fopen(fnsym,"wb")))
  {
    perror("Opening sym output file");
    return -1;
  }
  xt = sizex + PIN_LEN;
  xb = PIN_LEN;
  yt = sizey + PIN_LEN;
  yb = PIN_LEN;
  /* gEDA file version */
  fprintf(fsym,"v %s\n",sym_version);
  /* reference: U? */
  fprintf(fsym,"T %d %d %d %d 1 1 0 %d\n",xt,yt+SCALE,EDACOLOR_REF,EDASIZE_ATTRIB,EDATEXT_RB);
  if (use_old_version)
    fprintf(fsym,"uref=%s?\n",ref_str);
  else
    fprintf(fsym,"refdes=%s?\n",ref_str);
  /* part name */
  fprintf(fsym,"T %d %d %d %d 1 0 0 %d\n",xb,yt+SCALE,EDACOLOR_LABEL,EDASIZE_LABEL,EDATEXT_LB);
  fprintf(fsym,"%s\n",part_aliases[0]);
  /* device name */
  fprintf(fsym,"T %d %d %d %d 0 0 0 %d\n",xb,yt+3*SCALE,EDACOLOR_ATTRIB,EDASIZE_ATTRIB,EDATEXT_LB);
  fprintf(fsym,"device=%s\n",part_aliases[0]);
  if (vector_count == 0)
  {
    /* main box */
    fprintf(fsym,"B %d %d %d %d %d 0 0 0 -1 -1 0 -1 -1 -1 -1 -1\n",xb,yb,sizex,sizey,EDACOLOR_BODY);
  }
  else
  {
    /* drawing vectors */
    for (i = 0; i< vector_count; i++)
    {
      switch (part_vectors[i].type)
      {
	case 'T':
	  /* text */
	  xs = (int) (part_vectors[i].p[0] * PIN_SPACE + 0.5) + PIN_LEN;
	  ys = (int) yt - (part_vectors[i].p[1] * PIN_SPACE + 0.5);
	  fprintf(fsym,"T %d %d %d %d 1 0 0 %d\n",xs,ys,EDACOLOR_BODY,
	      text_scale[(int)part_vectors[i].p[2]],EDATEXT_LB);
	  fprintf(fsym,"%s\n",part_vectors[i].str);
	  free(part_vectors[i].str);
	  break;
	case 'L':
	  /* line */
	  xs = (int) (part_vectors[i].p[0] * PIN_SPACE + 0.5) + PIN_LEN;
	  ys = (int) yt - (part_vectors[i].p[1] * PIN_SPACE + 0.5);
	  xe = (int) (part_vectors[i].p[2] * PIN_SPACE + 0.5) + PIN_LEN;
	  ye = (int) yt - (part_vectors[i].p[3] * PIN_SPACE + 0.5);
	  fprintf(fsym,"L %d %d %d %d %d 0 0 0 -1 -1\n",xs,ys,xe,ye,
	      EDACOLOR_BODY);
	  break;
	case 'V':
	  /* circle */
	  xs = (int) (part_vectors[i].p[0] * PIN_SPACE + PIN_LEN + 0.5);
	  ys = (int) yt - (part_vectors[i].p[1] * PIN_SPACE + 0.5);
	  xe = (int) (part_vectors[i].p[2] * PIN_SPACE + 0.5); /* radius */
	  fprintf(fsym,"V %d %d %d %d 0 0 0 -1 -1 0 -1 -1 -1 -1 -1\n",xs,ys,xe,
	      EDACOLOR_BODY);
	  break;
	case 'A':
	  /* arc */
	  xs = (int) (part_vectors[i].p[0] * PIN_SPACE + PIN_LEN + 0.5);
	  ys = (int) yt - (part_vectors[i].p[1] * PIN_SPACE + 0.5);
	  xe = initangle(part_vectors[i].p[2],-part_vectors[i].p[3]);  /* init angle */
	  ye = initangle(part_vectors[i].p[4],-part_vectors[i].p[5]);  /* end angle */
	  if (xe < 0) xe += 360;
	  if (ye < 0) ye += 360;
	  ye -= xe;
	  if (abs(ye) > 90)
	  {
	    if (ye > 0)
	      ye -= 360;
	    else
	      ye += 360;
	  }
	  pinx = (int) (part_vectors[i].p[6] * PIN_SPACE + 0.5);  /* radius */
  	  fprintf(fsym,"A %d %d %d %d %d %d 0 0 0 -1 -1\n",xs,ys,pinx,xe,ye,
	      EDACOLOR_BODY);
	  break;
      }
    }
  }
  /* pin definiton and drawing */
  labely = 5 * SCALE;
  for (i=0; i<pincount; i++)
  {
    if ((pinlist[i].num == 0) && (pinlist[i].type == PIN_TYPE_PWR) && 
	((pinlist[i].pos == 'T') || (pinlist[i].pos == 'B')))
    {
      /* is a "virtual" pin, i.e. a net */
      fprintf(fsym,"T %d %d %d %d 0 0 0 %d\n",xb,yt+labely,EDACOLOR_ATTRIB,
	  EDASIZE_ATTRIB,EDATEXT_LB);
      fprintf(fsym,"net=%s:%d\n",pinlist[i].name,pinlist[i].pin);
      labely += LABEL_SPACE;
      continue;
    }
    switch (pinlist[i].pos)
    {
      case 'L':
        xs = xb;
	xe = 0;
	ys = ye = yt - pinlist[i].num * PIN_SPACE;
	txtangle = 0;
	/* pin number */
	pinx = xs - SCALE;
	piny = ys + SCALE/2;
	pinalign = EDATEXT_RB;
	/* pin name */
	npinx = xs + SCALE;
	npiny = ys;
	npinalign = EDATEXT_LB;
	/* pin type */
	tpinx = xs + SCALE;
	tpiny = ys;
	tpinalign = EDATEXT_LT;
	if ((pinlist[i].flags & PIN_CLKFLAG) != 0)
	{
	  /* there is "CLK" modifier */
	  npinx += CLK_SIZE/2;
	  tpinx += CLK_SIZE/2;
	  fprintf(fsym,"L %d %d %d %d %d 0 0 0 -1 -1\n",xs,ys+CLK_SIZE,xs+CLK_SIZE,ys,
	      EDACOLOR_BODY);
	  fprintf(fsym,"L %d %d %d %d %d 0 0 0 -1 -1\n",xs,ys-CLK_SIZE,xs+CLK_SIZE,ys,
	      EDACOLOR_BODY);
	}
	if ((pinlist[i].flags & PIN_DOTFLAG) != 0)
	{
	  /* there is "DOT" modifier */
	  xs -= DOT_SIZE;
	  pinx -= DOT_SIZE/2;
	  fprintf(fsym,"V %d %d %d %d 0 0 0 -1 -1 0 -1 -1 -1 -1 -1\n",xs+DOT_SIZE/2,ys,DOT_SIZE/2,
	      EDACOLOR_DOT);
	}
	break;
      case 'R':
        xs = xt;
	xe = xt + PIN_LEN;
	ys = ye = yt - pinlist[i].num * PIN_SPACE;
	txtangle = 0;
	/* pin number */
	pinx = xs + SCALE;
	piny = ys + SCALE/2;
	pinalign = EDATEXT_LB;
	/* pin name */
	npinx = xs - SCALE;
	npiny = ys;
	npinalign = EDATEXT_RB;
	/* pin type */
	tpinx = xs - SCALE;
	tpiny = ys;
	tpinalign = EDATEXT_RT;
	if ((pinlist[i].flags & PIN_CLKFLAG) != 0)
	{
	  /* there is "CLK" modifier */
	  npinx -= CLK_SIZE/2;
	  tpinx -= CLK_SIZE/2;
	  fprintf(fsym,"L %d %d %d %d %d 0 0 0 -1 -1\n",xs,ys+CLK_SIZE,xs-CLK_SIZE,ys,
	      EDACOLOR_BODY);
	  fprintf(fsym,"L %d %d %d %d %d 0 0 0 -1 -1\n",xs,ys-CLK_SIZE,xs-CLK_SIZE,ys,
	      EDACOLOR_BODY);
	}
	if ((pinlist[i].flags & PIN_DOTFLAG) != 0)
	{
	  /* there is "DOT" modifier */
	  xs += DOT_SIZE;
	  pinx += DOT_SIZE/2;
	  fprintf(fsym,"V %d %d %d %d 0 0 0 -1 -1 0 -1 -1 -1 -1 -1\n",xs-DOT_SIZE/2,ys,DOT_SIZE/2,
	      EDACOLOR_DOT);
	}
	break;
      case 'T':
        ys = yt;
	ye = yt + PIN_LEN;
	xs = xe = xb + pinlist[i].num * PIN_SPACE;
	txtangle = 90;
	/* pin number */
	pinx = xs - SCALE/2;
	piny = ys + SCALE;
	pinalign = EDATEXT_LB;
	/* pin name */
	npinx = xs;
	npiny = ys - SCALE;
	npinalign = EDATEXT_RB;
	/* pin type */
	tpinx = xs;
	tpiny = ys - SCALE;
	tpinalign = EDATEXT_RT;
	if ((pinlist[i].flags & PIN_CLKFLAG) != 0)
	{
	  /* there is "CLK" modifier */
	  npiny -= CLK_SIZE/2;
	  tpiny -= CLK_SIZE/2;
	  fprintf(fsym,"L %d %d %d %d %d 0 0 0 -1 -1\n",xs-CLK_SIZE,ys,xs,ys-CLK_SIZE,
	      EDACOLOR_BODY);
	  fprintf(fsym,"L %d %d %d %d %d 0 0 0 -1 -1\n",xs,ys-CLK_SIZE,xs+CLK_SIZE,ys,
	      EDACOLOR_BODY);
	}
	if ((pinlist[i].flags & PIN_DOTFLAG) != 0)
	{
	  /* there is "DOT" modifier */
	  ys += DOT_SIZE;
	  piny += DOT_SIZE/2;
	  fprintf(fsym,"V %d %d %d %d 0 0 0 -1 -1 0 -1 -1 -1 -1 -1\n",xs,ys-DOT_SIZE/2,DOT_SIZE/2,
	      EDACOLOR_DOT);
	}
	break;
      case 'B':
        ys = yb;
	ye = yb - PIN_LEN;	/* should be always 0 */
	xs = xe = xb + pinlist[i].num * PIN_SPACE;
	txtangle = 270;
	/* pin number */
	pinx = xs + SCALE/2;
	piny = ys - SCALE;
	pinalign = EDATEXT_LB;
	/* pin name */
	npinx = xs;
	npiny = ys + SCALE;
	npinalign = EDATEXT_RB;
	/* pin type */
	tpinx = xs;
	tpiny = ys + SCALE;
	tpinalign = EDATEXT_RT;
	if ((pinlist[i].flags & PIN_CLKFLAG) != 0)
	{
	  /* there is "CLK" modifier */
	  npiny += CLK_SIZE/2;
	  tpiny += CLK_SIZE/2;
	  fprintf(fsym,"L %d %d %d %d %d 0 0 0 -1 -1\n",xs-CLK_SIZE,ys,xs,ys+CLK_SIZE,
	      EDACOLOR_BODY);
	  fprintf(fsym,"L %d %d %d %d %d 0 0 0 -1 -1\n",xs,ys+CLK_SIZE,xs+CLK_SIZE,ys,
	      EDACOLOR_BODY);
	}
	if ((pinlist[i].flags & PIN_DOTFLAG) != 0)
	{
	  /* there is "DOT" modifier */
	  ys -= DOT_SIZE;
	  piny -= DOT_SIZE/2;
	  fprintf(fsym,"V %d %d %d %d 0 0 0 -1 -1 0 -1 -1 -1 -1 -1\n",xs,ys+DOT_SIZE/2,DOT_SIZE/2,
	      EDACOLOR_DOT);
	}
	break;
    }
    /* pin draw */
    fprintf(fsym,"P %d %d %d %d %d\n",xs,ys,xe,ye,EDACOLOR_PIN);
    /* pin attributes */
    fputs("{\n",fsym);
    /* pin sequence and number */
    if (partspp != 0)	/* handling for parts per package = 0 */
    {
      fprintf(fsym,"T %d %d %d %d 1 1 %d %d\n",pinx,piny,EDACOLOR_ATTRIB,
	  EDASIZE_PIN,txtangle,pinalign);
    }
    else
    {
      /* if no pin number in the original symbol, make pin number hidden */
      fprintf(fsym,"T %d %d %d %d 0 1 %d %d\n",pinx,piny,EDACOLOR_ATTRIB,
	  EDASIZE_PIN,txtangle,pinalign);
    }
    if (use_old_version)
      fprintf(fsym,"pin%d=%d\n",i+1,pinlist[i].pin);
    else
    {
      fprintf(fsym,"pinnumber=%d\n",pinlist[i].pin);
      fprintf(fsym,"T %d %d %d %d 0 1 %d %d\n",pinx,piny,EDACOLOR_ATTRIB,
	  EDASIZE_PIN,txtangle,pinalign);
      fprintf(fsym,"pinseq=%d\n",i+1);
    }
    /* pin name */
    /* hides pin name if symbol is VECTOR drawed and '-n' switch given */
    if ((vector_count != 0) && (pin_name_hidden))
    {
      fprintf(fsym,"T %d %d %d %d 0 1 %d %d\n",npinx,npiny,EDACOLOR_LABEL,
	  EDASIZE_LABEL,txtangle,npinalign);
    }
    else
    {
      fprintf(fsym,"T %d %d %d %d 1 1 %d %d\n",npinx,npiny,EDACOLOR_LABEL,
	  EDASIZE_LABEL,txtangle,npinalign);
    }
    if (use_old_version)
      fprintf(fsym,"label=%s\n",pinlist[i].name);
    else
      fprintf(fsym,"pinlabel=%s\n",pinlist[i].name);
    /* pin type */
    fprintf(fsym,"T %d %d %d %d 0 1 %d %d\n",tpinx,tpiny,EDACOLOR_ATTRIB,
	EDASIZE_LABEL,txtangle,tpinalign);
    if (use_old_version)
      fprintf(fsym,"type=%s\n",edapintype[pinlist[i].type]);
    else
      fprintf(fsym,"pintype=%s\n",edapintype[pinlist[i].type]);
    fputs("}\n",fsym);
  }
  fclose(fsym);
  return 0;
}



