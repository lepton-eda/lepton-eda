/* convert_sym.c:
 *
 *  Convert a Viewlogic symbol/schematic to gEDA gschem format
 *
 *  accept one argument, the name of the file to 
 *  convert, converted output is displayed on stdout.
 *
 *     Copyright (C) 1999  Mike Jarabek
 *   
 *   This program is free software; you can redistribute it and/or
 *   modify it under the terms of the GNU General Public License
 *   as published by the Free Software Foundation; either version 2
 *   of the License, or (at your option) any later version.
 *   
 *   This program is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *   GNU General Public License for more details.
 *   
 *   You should have received a copy of the GNU General Public License
 *   along with this program; if not, write to the Free Software
 *   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111 USA
 *
 * 	$Id$	 
 */

#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <math.h>

#include <config.h> /* added by AVH for integration into the gEDA tarball */
#include <libgeda/colors.h>

#if 0 /* removed by AVH just to make a -Wall -Werror happy */
#ifndef lint
static char vcid[] = "$Id$";
#endif /* lint */
#endif

#ifndef OPTARG_IN_UNISTD
extern char *optarg;
extern int optind;
#endif

/* local defines */
#define MAX_TEXTLEN      1024
#define MAX_NODES         300
#define MAX_POINTS        300

/* Viewdraw Colours
 *
 * 0  Black   |  4  Red       |   8  Gray       |  12  Lt. Red
 * 1  Blue    |  5  Magenta   |   9  Lt. Blue   |  13  Lt. Magenta
 * 2  Green   |  6  Brown     |  10  Lt. Green  |  14  Yellow
 * 3  Cyan    |  7  Lt. Gray  |  11  Lt. Cyan   |  15  White
 *
 * Geda Colours:
 *
 * 0  BLACK   |  1  WHITE     |   2  RED        |   3  GREEN
 * 4  BLUE    |  5  YELLOW    |   6  CYAN       |   7  GREY
 *
 */

/* tables */
int colormap[16] =   /* index is viewlogic colour, entry is geda color */
{
  0, /*  0 */
  4, /*  1 */
  3, /*  2 */
  6, /*  3 */
  2, /*  4 */
  2, /*  5 */
  7, /*  6 */
  7, /*  7 */
  7, /*  8 */
  4, /*  9 */
  3, /* 10 */
  6, /* 11 */
  2, /* 12 */
  2, /* 13 */
  5, /* 14 */
  1, /* 15 */
};

/* attribute translation table */
struct Translation {
  char *origName;       /* name as it appears on a viewlogic schematic */
  char *newName;        /* name as it should appear in gEDA */
  unsigned int action;  /* what to do with this name */
};

/* action codes for translation action */
#define REPLACE_NAME     1
#define KILL             2
#define WARN_USER        3

struct Translation translations[] = 
{
  {"PKG_TYPE", "PHYSICAL", REPLACE_NAME},
  {"PART_SPEC","device",   REPLACE_NAME},
  {"LEVEL",    "",         KILL},
  {"P/D_NUM",  "",         KILL},
  {"NC",       "",         KILL},
  {"REFDES",   "uref",     REPLACE_NAME},
  /*{"PINTYPE",  "",         KILL},*/
  {"NAME",     "",         KILL},
  {"LABEL",    "",         KILL},
};

unsigned int nTranslations = sizeof(translations)/sizeof(struct Translation);

/* local function prototypes */
int convert_file(FILE *fp);
unsigned int strindex(char *s, char c);
unsigned int strrindex(char *s, char c);
void         strtolower(char *s);
int get_continued_string(char *buf, size_t buffer_size, FILE *fp);
int get_style(FILE *fp, unsigned int *colour, 
	      unsigned int *fillstyle,
	      unsigned int *linestyle);
void set_orientation(int *angle, int *mirror, int orientation);

/* conversion readers */
void do_nop(FILE *fp);
void do_bounding_box(FILE *fp);
void do_unnatached_attribute(FILE *fp);
void do_attached_attribute(FILE *fp);
void do_text(FILE *fp);
void do_line(FILE *fp);
void do_pin(FILE *fp);
void do_box(FILE *fp);
void do_circle(FILE *fp);
void do_arc(FILE *fp);
void do_label(FILE *fp);
void do_net_start(FILE *fp);
void do_net_node(FILE *fp);
void do_net_segment(FILE *fp);
void do_net_segment_bus(FILE *fp);
void do_instance(FILE *fp);

/* output objects */
void text_object(int x, int y, unsigned int color, unsigned int size,
		 unsigned int visibility, unsigned int show_name_value, 
		 int angle, char *text, unsigned int origin);
void attribute_object(int x, int y, unsigned int color, unsigned int  size,
		      unsigned int visibility, unsigned int show_name_value, 
		      int angle, char *name, char *value, unsigned int origin);
void line_object( int x1, int y1, int x2, int y2, unsigned int color);
void circle_object(int bx, int by, unsigned int radius, unsigned int bcolor);
void pin_object(int x1, int y1, int x2, int y2, unsigned int color);
void box_object(int x1, int y1, unsigned int width, unsigned int height,
		unsigned int color);
void arc_object(int x1, int y1, unsigned int radius, 
		int start_angle, int sweep_angle, unsigned int color);
void net_segment(int x1, int y1, int x2, int y2, unsigned int color);
void bus_segment(int x1, int y1, int x2, int y2, unsigned int color);
void complex_object(int x, int y, unsigned int selectable, 
	       int angle, unsigned int mirror, char *name);
void begin_attach(void);
void end_attach(void);
void reset_attributes(void);

/* externals */
int GetStringDisplayLength(char *str,int font_size);



/* globals */
int attach_pending = 0;    /* keep track of whether the last object */
                           /* read may have attachments pending. */
int add_attributes = 0;    /* keep track of whether we are adding attributes */
			   /* to some previous object */
int pin_attributes = 0;    /* when true, we are adding attributes to a pin */
int net_attributes = 0;    /* when true, we are adding atrributes to a net */
int complex_attributes = 0;/* when true, we are adding attibutes to a complex*/
int pin_count      = 0;    /* to keep track of the number of pins */
int reading_net    = 0;    /* used to keep track of when we are reading a net*/
int segment_count  = 0;    /* the number of net segments read for a net */
int net_nodes_x[MAX_NODES];
int net_nodes_y[MAX_NODES];
int scale          = 10;   /* scale factor for viewlogic-geda conversion */
int symbol_mode    = 0;
int records_processed = 0; /* used to keep track of the number of viewlogic
			    * records processed for diagnostics
			    */

int minx = 0;              /* bounding box for symbol */
int miny = 0;
int maxx = 0;
int maxy = 0;

void
usage(char *cmd)
{
  fprintf(stderr,
	  "Usage:\n\t%s [-s] <viewlogic_filename>\n"
	  " Where:\n"
	  "\t-s                   converting symbol file.\n" 
	  "\t<viewlogic_filename> is the name of the file you\n"
	  "\t\t\t  want to convert to gEDA format\n", cmd);
  exit(1);
}

int
main(int argc, char **argv)
{

  FILE *infile;
  char *infileName;

  int ch;

  while ((ch = getopt (argc, argv, "s?h")) != -1) {
    switch (ch) {

    case 's':
      symbol_mode = 1;
      break;
      
    case '?':
    case 'h':
    default:
      usage(argv[0]);
      break;
    }
  }

  if (optind == argc)
    usage(argv[0]);


  /* 'parse' arguments */
  infileName = argv[optind];

  infile=fopen(infileName,"r");
  if(infile == NULL)
    {
      fprintf(stderr,"Error: Unable to open file `%s' in %s()\n",
	      infileName,__FUNCTION__);
      return 1;
    }

  convert_file(infile);

  fclose(infile);

  return 0;
}

/* convert the given file to geda */
int
convert_file(FILE *fp)
{
  int c;
  int text_len;

  char buf[MAX_TEXTLEN];

  /* output pre-amble */
  printf("v 20000704\n"); /* Set the version of the file to a fixed date AVH */
  reset_attributes();


  while((c =fgetc(fp)) != EOF) /* fetch record type */
    {
      switch(c) /* branch to appropriate handler */
	{
	case 'D': 
	  do_bounding_box(fp);
	  break;
	    
	case 'U':
	  do_unnatached_attribute(fp);
	  break;

	case 'A':
	  do_attached_attribute(fp);
	  break;

	case 'T':
	  do_text(fp);
	  break;

	case 'l':
	  do_line(fp);
	  break;

	case 'P':
	  do_pin(fp);
	  break;

	case 'b':
	  do_box(fp);
	  break;

	case 'c':
	  do_circle(fp);
	  break;

	case 'a':
	  do_arc(fp);
	  break;

	case 'L':
	  do_label(fp);
	  break;

	  /* net stuff */
	case 'N':
	  do_net_start(fp);
	  break;
	case 'J':
	  do_net_node(fp);
	  break;
	case 'S':
	  do_net_segment(fp);
	  break;
	case 'B':
	  do_net_segment_bus(fp);
	  break;
	
	case 'I':
	  do_instance(fp);
	  break;
	  /* ZZZZ */

	case 'V': case 'K': case 'Y': case 'i': case 'E':
	case 'Z':
	  do_nop(fp);
	  break;
	
	case 'Q':
	  fprintf(stderr,"Warning 'Q' record found and not handled at"
		  "record %d, contact maintainer\n",records_processed);
	  do_nop(fp);
	  break;

	case 'C':  /* connected pin record */
	  do_nop(fp);
	  break;

	case 'X':  /* unconnected pin record */
	  do_nop(fp);
	  break;

	default: /* just read in the record and trash it */
	  fgets(buf, MAX_TEXTLEN, fp);
	  /* nuke trailing CR, if there */
	  text_len=strlen(buf);
	  if(buf[text_len-1] == '\n')
	    {
	      buf[text_len-1] = 0;
	    }
	  fprintf(stderr,"Warning: Unrecognized record #%d:\n'%c%s'\n",
		  records_processed, c, buf);
	}
      records_processed++;
    }

  /* output post-amble */
  reset_attributes();


  return 0;
}

void
do_nop(FILE *fp)
{
  char text[MAX_TEXTLEN];

  fgets(text,MAX_TEXTLEN,fp);
}

void
do_bounding_box(FILE *fp)
{
  /* just fetch the values and store */
  if(fscanf(fp,"%d %d %d %d\n", &minx, &miny, &maxx, &maxy) != 4)
    {
      fprintf(stderr,"Error: Invalid bounding box record #%d in %s()\n",
	      records_processed, __FUNCTION__);
      exit(1);
    }

  minx *= scale;
  miny *= scale;
  maxx *= scale;
  maxy *= scale;

  if (symbol_mode == 0)
    box_object(minx, miny, maxx-minx, maxy-miny, GRAPHIC_COLOR);
}


void
do_unnatached_attribute(FILE *fp)
{
  int x,y,angle;
  unsigned int dummy, color, size, origin, viewvis;
  unsigned int linestyle, fillstyle, index;
  unsigned int visibility, show_name_value;
  char text[MAX_TEXTLEN], *name, *value;

  /* if we are inside of a pin definition, terminate */
  reset_attributes();

  /* for the moment just represent as text */
  
  /* viewlogic unnatached attributes have this format:
   * U #X #Y #SIZE #ROTATION #origin #Visibility ATTR_TEXT
   */
  if(fscanf(fp,"%d %d %u %d %u %u", &x, &y, &size, &angle, &origin, 
	    &viewvis) != 6)
    {
      fprintf(stderr,"Error: Invalid Unattached attribute record #%d "
	      "in %s()\n",
	      records_processed, __FUNCTION__);
      exit(1);
    }

  /* read in the text */
  get_continued_string(text, MAX_TEXTLEN, fp);


  x *= scale;   /* correct coordinates */
  y *= scale;
  color = DETACHED_ATTRIBUTE_COLOR;

  get_style(fp, &dummy, &fillstyle, &linestyle);

  /* evaluate visibility for attributes */
  switch(viewvis)
    {
    case 0:   /* not at all visibile */
      visibility = 0;
      show_name_value = 0;
      break;

    case 1:   /* attribute and name visible */
      visibility = 1;
      show_name_value = 0;
      break;

    case 2:   /* only name visible */
      visibility = 2;
      show_name_value = 1;
      break;

    case 3:   /* only value visible */
      visibility = 1;
      show_name_value = 1;
      break;
      
    default:
      fprintf(stderr,"Error: Invalid visibility value %d in "
	      "viewlogic file at record #%d in function %s()\n",
	      viewvis, records_processed, __FUNCTION__);
      return;
    }

  /* find name and value pair */
  name = text;
  index = strindex(text,'=');
  text[index] = 0;
  value = &text[index+1];

  attribute_object( x, y, color, size, visibility, show_name_value, angle,
		    name, value, origin );

}

void
do_attached_attribute(FILE *fp)
{
  int x,y,angle;
  unsigned int color, dummy, size, origin, viewvis;
  unsigned int visibility, show_name_value;
  unsigned int fillstyle, linestyle, index;
  char text[MAX_TEXTLEN],text2[MAX_TEXTLEN],*name,*value;

  /* for the moment just represent as text */
  
  /* attached attributes have the following format:
   *    A #X #Y #SIZE #ROTATION #ORIGIN #VISIBILITY ATTR_TEXT
   */
  if(fscanf(fp,"%d %d %u %d %u %u", &x, &y, &size, &angle, &origin, 
	 &viewvis) != 6)
    {
      fprintf(stderr,"Error: Invalid attached attribute record #%d"
	      " in %s()\n", records_processed, __FUNCTION__);
      exit(1);
    }

  x *= scale;   /* correct coordinates */
  y *= scale;

  /* read in the text */
  get_continued_string(text, MAX_TEXTLEN, fp);

  color = ATTRIBUTE_COLOR;
  get_style(fp, &dummy, &fillstyle, &linestyle);

  /* evaluate visibility for attributes */
  switch(viewvis)
    {
    case 0:   /* not at all visibile */
      visibility = 0;
      show_name_value = 0;
      break;

    case 1:   /* attribute and name visible */
      visibility = 1;
      show_name_value = 0;
      break;

    case 2:   /* only name visible */
      visibility = 2;
      show_name_value = 1;
      break;

    case 3:   /* only value visible */
      visibility = 1;
      show_name_value = 1;
      break;
      
    default:
      fprintf(stderr,"Error: Invalid visibility value %d in "
	      "viewlogic file at record #%d, in function %s()\n",
	      viewvis, records_processed, __FUNCTION__);
      return;
    }

  begin_attach();
  if(pin_attributes) /* are we adding to a pin ? */
    {
      if(text[0] == '#')
	{
	  strncpy(text2, text, MAX_TEXTLEN);
#ifdef HAVE_SNPRINTF
	  snprintf(text, MAX_TEXTLEN, "pin%d=%s",pin_count,&text2[2]);
#else
	  sprintf(text, "pin%d=%s",pin_count,&text2[2]);
#endif
	  visibility = 1;         /* overide any previous settings */
	  show_name_value = 1;
	}
    }

  /* find name and value pair */
  name = text;
  index = strindex(text,'=');
  text[index] = 0;
  value = &text[index+1];

  attribute_object( x, y, color, size, visibility, show_name_value, angle,
		    name, value, origin );
}

void 
do_text(FILE *fp)
{
  int x,y,angle;
  unsigned int size, origin;
  unsigned int color, linestyle, fillstyle, show_name_value;
  unsigned int visibility;
  char text[MAX_TEXTLEN];

  
  /* if we are inside of a pin definition, terminate */
  reset_attributes();

  /* viewlogic text have the following format:
   *  T #X #Y #SIZE #ROTATION #ORIGIN TEXT
   */
  if(fscanf(fp,"%d %d %u %d %u",&x, &y, &size, &angle, 
	    &origin) != 5)
    {
      fprintf(stderr,"Error: Invalid text record #%d in %s()\n",
	      records_processed, __FUNCTION__);
      exit(1);
    }


  /* read in the text */
  get_continued_string(text, MAX_TEXTLEN, fp);

  x *= scale;   /* correct coordinates */
  y *= scale;
  color = TEXT_COLOR;
  visibility = 1;
  show_name_value = 0;

  /* get possible colour change */
  get_style(fp, &color, &fillstyle, &linestyle);

  text_object(x, y, color, size, visibility, show_name_value, angle, text, \
	      origin);

}

void
do_line(FILE *fp)
{
  int x[MAX_POINTS],y[MAX_POINTS];
  unsigned int pairs,color,i;
  unsigned int fillstyle, linestyle;
  

  /* if we are inside of a pin definition, terminate */
  reset_attributes();

      
  /* the viewlogic line primitive is composed of 
   *         l #PAIRS #X1 #Y1 #X2 #Y2 ...   - Line
   */

  if(fscanf(fp,"%d",&pairs) != 1)
    {
      fprintf(stderr,"Error: Unable to read number of line pairs "
	      "for record #%d, in %s()\n",
	      records_processed, __FUNCTION__);
      exit(1);
    }

  /* scan in all the co-ordinate pairs and pop them into our array */
  for (i=0; i < pairs; i++) 
    {
      if(fscanf(fp,"%d %d", &x[i], &y[i]) != 2)
	{
	  fprintf(stderr,"Error: unable to read %dth coodinate pair "
		  "for record #%d, in %s()\n",
		  i+1, records_processed, __FUNCTION__);
	  exit(1);
	}
	  
      x[i] *= scale;
      y[i] *= scale;
    }
  getc(fp); /* slurp up last CR */

  color = GRAPHIC_COLOR;
  /* now check for an optional style record */
  get_style(fp, &color, &fillstyle, &linestyle);

  /* now, output the line as a series of geda lines */
  for(i=1; i<pairs; i++)
      line_object(x[i-1],y[i-1],x[i],y[i],color);

}


void 
do_pin(FILE *fp)
{
  unsigned int pindir, pinsense, color;
  unsigned int radius, radius2, bcolor;
  int x1, y1, x2, y2, bx, by;

  /* if we are inside of a pin definition, terminate */
  reset_attributes();
      
  /* viewlogic pin primitives have the following format:
   *  P #PININSTANCE #X1 #Y1 #X2 #Y2 # #PINDIRECTION #PINSENSE
   */

  if(fscanf(fp,"%*d %d %d %d %d %*d %u %u\n",&x1, &y1, &x2, &y2, 
	    &pindir, &pinsense) != 6)
    {
      fprintf(stderr,"Error:Invalid pin record #%d in %s()\n",
	      records_processed, __FUNCTION__);
      exit(1);
    }
	
  x1 *= scale;
  y1 *= scale;
  x2 *= scale;
  y2 *= scale;
  color = PIN_COLOR;


  /* if this pin has to be of negative polarity, add a bitty bubble
   * and adjust the size of the pin
   */
  radius = 25;
  radius2 = 2*radius;
  bcolor = LOGIC_BUBBLE_COLOR;
  
  if(pinsense == 1) {
    
    /* print "pindir:" pindir */
    /* get the bubble on the right end */

    /* one of the coordinates will match up with the bounding box
     * then just choose the other end for the bubble
     */

    if(x1 == minx)         /* left hand side of pin touches bounding box */
      {    
	bx = x2-radius;
	by = y2;
	x2 -= radius2;
      } 
    else if (x1 == maxx)   /* left end touches right side */
      { 
	bx = x2+radius;
	by = y2;
	x2 += radius2;
      }
    else if (x2 == minx)   /* right end touches left side */
      {
	bx = x1-radius;
	by = y1;
	x1 -= radius2;
      } 
    else if (x2 == maxx)   /* right end touches right side */
      {
	bx = x1+radius;
	by = y1;
	x1 += radius2;
      }
    else if (y1 == miny)   /* left end touches bottom */
      {
	bx = x2;
	by = y2-radius;
	y2 -= radius2;
      }
    else if (y1 == maxy)   /* left end touches top */
      {
	bx = x2;
	by = y2+radius;
	y2 += radius2;
      }
    else if (y2 == miny)   /* right end touches bottom */
      {
	bx = x1;
	by = y1-radius;
	y1 -= radius2;
      }
    else if (y2 == maxy)   /* right end touches top */
      {
	bx = x1;
	by = y1+radius;
	y1 += radius2;
      } 
    else
      {
	fprintf(stderr,"Error: Unable to determine pin sense "
		"for record #%d in %s()\n"
		"\tminx: %d, miny: %d, maxx: %d, maxy: %d\n",
		records_processed, __FUNCTION__, minx, miny, maxx, maxy);
	return;
      }

    circle_object(bx,by,radius,bcolor);
  }

  pin_object(x1,y1,x2,y2,color);


  add_attributes = 1;   /* add attributes */
  attach_pending = 1;   /* signal that an attachment could be coming */
  pin_attributes = 1;   /* and that they are pin attributes */
  pin_count++;          /* bump the number of pins */

}

void 
do_box(FILE *fp)
{
  int x1, y1, x2, y2, width, height;
  unsigned int color, fillstyle, linestyle;

  
  /* if we are inside of a pin definition, terminate */
  reset_attributes();

  /* a viewlogic box has the following format:
   *  b #X1 #Y1 #X2 #Y2
   * geda view of a box has the corner, width and height
   */
  if(fscanf(fp, "%d %d %d %d\n", &x1, &y1, &x2, &y2) != 4)
    {
      fprintf(stderr, "Error: Invalid box record #%d in %s()\n",
	      records_processed, __FUNCTION__);
      exit(1);
    }

  x1 *= scale;
  y1 *= scale;
  x2 *= scale;
  y2 *= scale;

  width = x2-x1;
  height = y2-y1;
  color  = GRAPHIC_COLOR;

  get_style(fp, &color, &fillstyle, &linestyle);

  box_object(x1,y1,width,height,color);
}

void
do_circle(FILE *fp)
{
  int x, y;
  unsigned int radius, color;
  unsigned int fillstyle, linestyle;

  /* if we are inside of a pin definition, terminate */
  reset_attributes();

  /* a circle has the following format:
   *  c #x #y #radius
   */
  if(fscanf(fp,"%d %d %u\n",&x, &y, &radius) != 3)
    {
      fprintf(stderr,"Error: Invalid circle record #%d in %s()\n",
	      records_processed, __FUNCTION__);
      exit(1);
    }
  
  x *= scale;
  y *= scale;
  radius *=  scale;
  color = GRAPHIC_COLOR;

  get_style(fp, &color, &fillstyle, &linestyle);

  circle_object(x,y,radius,color);
}

void 
do_arc(FILE *fp)
{
  int x1, y1, x2, y2, x3, y3;
  unsigned int color, fillstyle, linestyle;
  double x2p, y2p, x3p, y3p, yop, xop, xo, yo; 
  double to_rad;
  double gstart, sweep_angle, start_angle, end_angle;
  double radius;

  /* if we are inside of a pin definition, terminate */
  reset_attributes();

  /* arcs have the following format:
   *   a #X1 #Y1 #X2 #Y2 #X3 #Y3
   * we need to transform this into the geda convention of
   *   center, radius, start angle, stop angle.
   */

  if(fscanf(fp,"%d %d %d %d %d %d\n", 
	    &x1, &y1, &x2, &y2, &x3, &y3) != 6)
    {
      fprintf(stderr,"Error: Invalid arc record #%d, in %s()\n",
	      records_processed, __FUNCTION__);
      exit(1);
    }
      
  x1 *= scale;
  y1 *= scale;
  x2 *= scale;
  y2 *= scale;
  x3 *= scale;
  y3 *= scale;
  color = GRAPHIC_COLOR;
  get_style(fp, &color, &fillstyle, &linestyle);

  x2p = x2 - x1;
  y2p = y2 - y1;

  x3p = x3 - x1;
  y3p = y3 - y1;

  /* printf("Buffer: `%s'\n",buf);
   * printf("Values: x2p: %f, y2p: %f, x3p: %f, y3p: %f\n",x2p, y2p, x3p, y3p);
   */
  if(fabs(x2p * y3p - y2p * x3p) < 0.00001)
    {
      /* some miscreant entered a degenerate arc, just output lines */
      line_object(x1,y1,x2,y2,color);
      line_object(x2,y2,x3,y3,color);
      return;
    }
      
  yop = ((x2p * ( x3p*x3p + y3p*y3p ) - x3p * ( x2p*x2p + y2p*y2p )) / 
	 (2 * (x2p * y3p - y2p * x3p)));

  xop = (x2p*x2p - 2*y2p*yop) / (2 * x2p);

  xo  = xop + x1;
  yo  = yop + y1;


  radius = sqrt(xop*xop + yop*yop);

  /* calculate start and end angles */
  to_rad = 180.0/atan2(0,-1);
  start_angle = atan2(y1-yo, x1-xo) * to_rad;
  end_angle = atan2(y3-yo, x3-xo) * to_rad;

  if(start_angle > end_angle)
    {
      gstart = end_angle;
      sweep_angle = start_angle - end_angle;
    } 
  else 
    {
      gstart = start_angle;
      sweep_angle = end_angle - start_angle;
    }

  /* end_angle   = 
   * end_angle   = int(atan2(y1-yo, x1-xo) * to_rad) % 360;
   * start_angle = int(atan2(y3-yo, x3-xo) * to_rad) % 360;
   */

  
  arc_object((int)xo,(int)yo, (unsigned int)radius,
	     (int)gstart,(int)sweep_angle, color);

}

void 
do_label(FILE *fp)
{
  int x, y, angle, foo;
  unsigned int color, size, origin, visibility, show_name_value;
  unsigned int fillstyle, linestyle;
  char text[MAX_TEXTLEN];
  char text2[MAX_TEXTLEN];

  /* labels have the following format:
   *   L #X #Y #SIZE #ROTATION #ORIGIN #GLOBAL #VISIBILITY #OVERBAR TEXT
   */

  /* reproduce as simple text, unless it is a label for an object */

  if(fscanf(fp,"%d %d %u %d %d %d %d %*d", 
	    &x, &y, &size, &angle, &origin, &foo, &visibility) != 7)
    {
      fprintf(stderr,"Error: Invalid label record #%d in %s()\n",
	      records_processed, __FUNCTION__);
      exit(1);
    }

  x *= scale;
  y *= scale;
  color = ATTRIBUTE_COLOR;
  show_name_value = 0;

  /* read in the text */
  get_continued_string(text2, MAX_TEXTLEN, fp);
  get_style(fp, &color, &fillstyle, &linestyle);

  /* if we are inside a pin definition, mangle the pin name */
  if(net_attributes == 1) /* a label on a net is its netname */
    {
#ifdef HAVE_SNPRINTF
      snprintf(text, MAX_TEXTLEN, "label=%s", text2);
#else
      sprintf(text, "label=%s", text2);
#endif
      show_name_value = 1;
    }

  else if(complex_attributes == 1) /* a label on a complex is its designator */
    {
#ifdef HAVE_SNPRINTF
      snprintf(text, MAX_TEXTLEN, "uref=%s", text2);
#else
      sprintf(text, "uref=%s", text2);
#endif
      show_name_value = 1;
    }
  else if(pin_attributes == 1)  /* a label on a pin is it's pinname */
    {
#ifdef HAVE_SNPRINTF
      snprintf(text, MAX_TEXTLEN, "pinlabel=%s", text2);
#else
      sprintf(text, "pinlabel=%s", text2);
#endif
      show_name_value = 1;
    }
  else
    strcpy(text,text2);     /* don't need to do anything, just copy */

  begin_attach();
  text_object(x, y, color, size, visibility, show_name_value, angle, text, \
	      origin);
}

/* four functions for doing net stuff */
void 
do_net_start(FILE *fp)
{
  reset_attributes();

  fscanf(fp,"%*d\n");  /* just dispose of the net instance number */
      
  reading_net = 1;
  segment_count = 1;
}

void 
do_net_node(FILE *fp)
{
  int x,y,type;

  /* in geda nets are composed of a number of segments, gschem connects
   * them together on the screen,  since viewlogic stores a net as a series
   * of nodes we need to tabulate them and only output segments when we
   * get connectivity information
   *
   * net segments have the following format:
   *  J #X #Y #SEGNUM  - Net segment
   */

  if(segment_count > MAX_NODES)
    {
      fprintf(stderr,"Error: too many nodes on a net at record #%d, "
	      "in %s(), try increasing\n"
	      "\tMAX_NODES\n", records_processed, __FUNCTION__);
      exit(1); /* this is fatal */
    }

  /* get the current info */
  if(fscanf(fp,"%d %d %d\n",&x, &y, &type) < 2)
    {
      fprintf(stderr,"Error: Invalid net node record #%d in %s()\n",
	      records_processed, __FUNCTION__);
      exit(1);
    }

  x *= scale;
  y *= scale;

  net_nodes_x[segment_count] = x;
  net_nodes_y[segment_count] = y;

  segment_count++;

}

void
do_net_segment(FILE *fp)
{
  unsigned int n1, n2, color;

  reset_attributes();

  /* net segment connectivity records have the following format:
   *  S #N1 #N2            - Net connectivity, Node N1 is connected to N2
   */

  if(fscanf(fp,"%u %u\n",&n1, &n2) != 2)
    {
      fprintf(stderr,"Error: Invalid net segment record #%d in %s()\n",
	      records_processed, __FUNCTION__);
      exit(1);
    }
      
  color = NET_COLOR;

  /* output a geda net segment */
  net_segment(net_nodes_x[n1], net_nodes_y[n1], 
	      net_nodes_x[n2], net_nodes_y[n2], color);

  /* there could be attributes to follow */
  add_attributes = 1;   /* add attributes */
  attach_pending = 1;   /* signal that an attachment could be coming */
  net_attributes = 1;   /* and that they are net attributes */
}

void
do_net_segment_bus(FILE *fp)
{
  unsigned int n1, n2, color;

  reset_attributes();

  /* bus net segment connectivity records have the following format:
   *  B #N1 #N2            - Net connectivity, Node N1 is bussed to N2
   */

  if(fscanf(fp,"%u %u\n",&n1, &n2) != 2)
    {
      fprintf(stderr,"Error: Invalid bus segment record #%d in %s()\n",
	      records_processed, __FUNCTION__);
      exit(1);
    }

  color = BUS_COLOR;

  /* output a geda bus segment */
  bus_segment(net_nodes_x[n1], net_nodes_y[n1], 
	      net_nodes_x[n2], net_nodes_y[n2], color);

  /* there could be attributes to follow */
  add_attributes = 1;   /* add attributes */
  attach_pending = 1;   /* signal that an attachment could be coming */
  net_attributes = 1;   /* and that they are net attributes */
}

void 
do_instance(FILE *fp)
{
  char lib[MAX_TEXTLEN], name[MAX_TEXTLEN], symName[MAX_TEXTLEN];
  unsigned int extension, selectable;
  int x, y, angle, orientation, mirror;
  float scale_factor;

  reset_attributes();

  /* a component instance has the following format
   *  I #instance LIB:NAME #PAGE #X #Y #ROTATION #MAGNIFICATION '
   */

  lib[0] = 0;
  name[0] = 0;
  extension = 9999;
  x = 0;
  y = 0;

  if(fscanf(fp,"%*d %[a-zA-Z0-9]:%[a-zA-Z0-9] %u %d %d %d %g %*s\n",
	    lib, name, &extension, &x, &y, &orientation, &scale_factor) < 5)
    {
      fprintf(stderr,"Error: Invalid instance record #%d in %s()\n"
	      "lib:'%s', name:'%s'\n"
	      "extension:%d, x:%d, y:%d\n",
	      records_processed, __FUNCTION__, lib,name,extension, x, y);
      exit(1);
    }
      
  x *= scale;
  y *= scale;
  selectable = 1;

  set_orientation(&angle, &mirror, orientation);
      
  /* fix case */
  strtolower(name);

  /* produce proper file name: */
#ifdef HAVE_SNPRINTF
  snprintf(symName, MAX_TEXTLEN, "%s-%d.sym",name,extension);
#else
  sprintf(symName, "%s-%d.sym",name,extension);
#endif

  complex_object(x, y, selectable, angle, mirror, symName);

  /* there could be attributes to follow */
  add_attributes = 1;     /* add attributes */
  attach_pending = 1;     /* signal that an attachment could be coming */
  complex_attributes = 1; /* and that they are complex attributes */

}

/* Viewlogic mirror over y-axis, but gschem mirror over x-axis. */
/* This makes (270, 1) viewlogic -> (90, 1) gschem */
/*        and (90, 1)  viewlogic -> (270, 1) gschem */ 
/*        and (180, 1)  viewlogic -> (0, 1) gschem */ 
/*        and (0, 1)  viewlogic -> (180, 1) gschem */ 
void
set_orientation(int *angle, int *mirror, int orientation)
{
  switch (orientation) {
  case 0:  /* 0 rotation, 0 mirror */
    *angle = 0;
    *mirror = 0;
    break;
  case 1:  /* 90 rotation, 0 mirror */
    *angle = 90;
    *mirror = 0;
    break;
  case 2:  /* 180 rotation, 0 mirror */
    *angle = 180;
    *mirror = 0;
    break;
  case 3:  /* 270 rotation, 0 mirror */
    *angle = 270;
    *mirror = 0;
    break;
  case 4:  /* 180 rotation, 1 mirror */
    *angle = 0;
    *mirror = 1;
    break;
  case 5:  /* 90 rotation, 1 mirror */
    *angle = 270;
    *mirror = 1;
    break;
  case 6:  /* 0 rotation, 1 mirror */
    *angle = 180;
    *mirror = 1;
  case 7:  /* 270 rotation, 1 mirror */
    *angle = 90;
    *mirror = 1;
    break;
  default:
    fprintf(stderr,"Error: Invalid orientation value %d at record %d\n",
	    orientation, records_processed);
  }
}

/* YYYY */

/* output a geda text object */
void
text_object(int x, int y, unsigned int color, unsigned int size,
	    unsigned int visibility, unsigned int show_name_value, 
	    int angle, char *text, unsigned int origin)
{
  unsigned int text_size;
  unsigned int textlen;

  /* fudge the text size, in viewdraw it is actually the height
   * in geda it is the point size.  The Variable text_size contains
   * the height of the text in points, size contains the height of
   * the text in viewlogic units.
   */
  text_size = (int)(size * 0.72);

  /* emulate the viewdraw text origin by shifting the text around */
  
  /* if the origin is one of the ones that are along the center line,
   * adjust y
   */
  if ( (origin == 2) || (origin == 5) || (origin == 8) ) 
    {
      y -= (size * scale) / 2;
    }

  if( (origin == 1) || (origin == 4) || (origin == 7) ) 
    {
      y -= size * scale;
    }

  /* approximate the length of the text */

  switch(show_name_value)
    {
    case 0:   /* measure whole text length */
      textlen = GetStringDisplayLength(text,text_size);
      break;
  
    case 1:   /* measure just the value part */
      textlen = GetStringDisplayLength(&text[strindex(text,'=') + 1],
				       text_size);
      break;

    case 2:   /* measure just the name part */
      textlen = GetStringDisplayLength(text, text_size) 
	- GetStringDisplayLength(&text[strindex(text,'=')],text_size);
      break;
      
    default:
      fprintf(stderr,"Error: invalid show_name_value: %d at record #%d, "
	      "in %s()\n",
	      show_name_value, records_processed, __FUNCTION__);
      return;
    }

  /* if the origin is one of the middle ones
   * fix the x coordinate
   */
  if( (origin == 4) || (origin == 5) || (origin == 6) ) 
    {
      x -= textlen / 2;
    }

  if( (origin == 7) || (origin == 8) || (origin == 9) )
    {
      x -= textlen;
    }

  /* currently angled/rotated text is not supported, print a
   * warning, and force the orientation
   */
  if(angle != 0)
    {
      angle = 0;
      fprintf(stderr,"Warning: Forcing text '%s' into standard alignment "
	      "at record #%d\n",
	      text,records_processed);
    }

  /* force text to start on a 10 unit boundary */
  if((x % 10) < 5)
    x = x - (x % 10);
  else
    x = x + (10 - (x % 10));

  if((y % 10) < 5)
    y = y - (y % 10);
  else
    y = y + (10 - (x % 10));
    

  /* XXX Fix GEDA!, text size limited to 79 chars! */
  if(strlen(text) >= 79)
    {
      fprintf(stderr,"Warning: Text '%s' truncated to 79 chars at "
	      "record #%d!! FIXME! in %s()\n",
	      text, records_processed, __FUNCTION__);
      text[79]=0;
    }


  printf( "T %d %d %u %u %u %u %d\n%s\n", x, y, color, text_size, 
	  visibility, show_name_value, angle, text);

}


void
attribute_object(int x, int y, unsigned int color, unsigned int  size,
		 unsigned int visibility, unsigned int show_name_value, 
		 int angle, char *name, char *value, unsigned int origin) 
{

  char text[MAX_TEXTLEN], tmpName[MAX_TEXTLEN];
  unsigned int i, done;

  /* make a copy of the attribute to work with */
  strncpy(tmpName, name, MAX_TEXTLEN-1);
  tmpName[MAX_TEXTLEN-1] = 0;   /* terminate in case strncpy doesnt */
  
  /* look up attribute name in translation attribute list
   * and translate or print approprate message 
   */
  done = 0;
  for(i=0; (i<nTranslations) && !done; i++)
    {

#ifdef DEBUG
      printf("Comparing `%s' to `%s' in %s()\n",tmpName,
	     translations[i].origName,__FUNCTION__);
#endif

      if(strcmp(tmpName,translations[i].origName) == 0) /* match? */
	switch(translations[i].action)
	  {
	  case REPLACE_NAME:
	    strncpy(tmpName, translations[i].newName, MAX_TEXTLEN-1);
	    done = 1;
	    break;

	  case KILL:
	    fprintf(stderr,"Warning: Killing attribute `%s=%s' at (%d,%d)"
		    " from record #%d\n",
		    tmpName, value,x,y,records_processed);
	    done = 1;
	    return;

	  case WARN_USER:
	    fprintf(stderr,"Warning: attribute name `%s=%s' at (%d,%d) "
		    "at record #%d, found during conversion\n"
		    "\tpassing it through unchanged\n",
		    tmpName,value,x,y,records_processed);
	    done = 1;
	    break;
	  default:
	    fprintf(stderr,"Error: Unknown action code for attribute\n"
		    "`%s=%s' at record #%d in %s()\n",
		    tmpName,value,records_processed,__FUNCTION__);
	    exit(1);
	  }
    }	  

  /* just add an = into the middle */
#ifdef HAVE_SNPRINTF
  snprintf(text, MAX_TEXTLEN, "%s=%s", tmpName, value);
#else  
  sprintf(text, "%s=%s", tmpName, value);
#endif

  text_object( x, y, color, size, visibility, show_name_value, \
	       angle, text, origin); 

}


void
line_object(int x1, int y1, int x2, int y2, unsigned int color)
{
  printf( "L %d %d %d %d %u\n",x1,y1,x2,y2,color);
}

void
circle_object(int bx, int by, unsigned int radius, unsigned int bcolor)
{  
  printf("V %d %d %u %u\n",bx,by,radius,bcolor);
}

void
pin_object(int x1, int y1, int x2, int y2, unsigned int color)
{
    printf("P %d %d %d %d %u\n",x1,y1,x2,y2,color);
}

void
box_object(int x1, int y1, unsigned int width, unsigned int height,
	   unsigned int color)
{
  printf("B %d %d %u %u %u\n",x1,y1,width,height,color);
}

void
arc_object(int x1, int y1, unsigned int radius, 
	   int start_angle, int sweep_angle, unsigned int color)
{
  printf("A %d %d %u %d %d %u\n",x1, y1, radius, 
	 start_angle, sweep_angle, color);
}

void
net_segment(int x1, int y1, int x2, int y2, unsigned int color )
{
  printf("N %d %d %d %d %u\n", x1, y1, x2, y2, color);
}

void
bus_segment(int x1, int y1, int x2, int y2, unsigned int color )
{
  printf("U %d %d %d %d %u\n", x1, y1, x2, y2, color);
}

void
complex_object(int x, int y, unsigned int selectable, 
	       int angle, unsigned int mirror, char *name)
{
  printf("C %d %d %u %d %u %s\n", x, y, selectable, angle, mirror, name);
}


void
begin_attach(void)
{
  if(attach_pending == 1)  /* begin an attachment if one is pending*/
    {
      printf("{\n");
      attach_pending = 0;
    }
}

void
end_attach(void) 
{
  printf("}\n");
}

void
reset_attributes(void)
{

  /* if we are inside of some kind of attribute attachment
   * terminate it, but only if we output the begin_attach.
   */
  if((add_attributes == 1) && (attach_pending == 0))
    {
      end_attach();
    }

  attach_pending = 0;    /* keep track of whether the last object */
                         /* read may have attachments pending. */

  add_attributes = 0;    /* keep track of whether we are adding attributes */
			 /* to some previous object */
  pin_attributes = 0;    /* when true, we are adding attributes to a pin */
  net_attributes = 0;    /* when true, we are adding atrributes to a net */
  complex_attributes = 0;/* when true, we are addint attibutes to a complex */
}

/* read a string possibly containing continuation characters
 * from the given stream 
 */
int 
get_continued_string(char *buf, size_t buffer_size, FILE *fp)
{
  int c;
  size_t text_len;

  /* skip leading whitespace */
  while(isspace((c=fgetc(fp))));
  ungetc(c,fp);  /* push back last char, cause it's not whitespace */
  
  /* read in the text */
  fgets(buf, buffer_size, fp);
  records_processed++;
  /* nuke trailing CR, if there */
  text_len=strlen(buf);
  if(buf[text_len-1] == '\n')
    {
      buf[text_len-1] = 0;
      text_len--;
    }

  /* check for continuation chars */
  while((c = getc(fp)) == '+')
    {
      c = getc(fp);                         /* suck in space */
      fgets(&buf[text_len], MAX_TEXTLEN-text_len,fp);  /* read in next chunk */
      records_processed++;
      text_len=strlen(buf);                 /* update text length */
      if(buf[text_len-1] == '\n')           /* nuke any trailing CR's */
	{
	  buf[text_len-1] = 0;
	  text_len--;
	}
    }
  ungetc(c,fp);   /* push back last char, obviously wasn't a + */

#ifdef DEBUG
  printf("Buffer:'%s' in %s()\n",buf,__FUNCTION__);
#endif

  return 0;
}

/* read in a possible style record to modify the current style */
int get_style(FILE *fp, unsigned int *colour, 
	      unsigned int *fillstyle,
	      unsigned int *linestyle)
{
  int c;

  c = getc(fp);
  if(c == 'Q') /* do we have a modifier? */
    {
      if(fscanf(fp,"%u %u %u\n", colour, fillstyle, linestyle) != 3)
	{
	  fprintf(stderr,"Error: Invalid modifier record #%d in %s()\n",
		  records_processed, __FUNCTION__);
	  exit(1);
	}

      /* re-map colour into a geda colour */
      if(*colour > 15)
	{
	  fprintf(stderr,"Error: Invalid colour number %u in record #%d, "
		  "in %s()\n",
		  *colour,records_processed, __FUNCTION__);
	  exit(1);
	}
      *colour = colormap[*colour]; 
      records_processed++;
    }
  else
    ungetc(c,fp); /* false alarm */

  return 0;
}



/* return the index of character c in the string pointed to by s
 */
unsigned int
strindex(char *s, char c)
{
  char *p;
  unsigned int i;

  for(p=s, i=0; *p; p++,i++)
    if(*p == c)
      return i;

  return 0;
}

/* return the index of the last occurance of character c in 
 * the string pointed to by s
 */
unsigned int 
strrindex(char *s, char c)
{
  char *p;
  unsigned int i, loc;

  loc = 0;
  for(p=s, i=0; *p; p++, i++)
    if(*p == c)
      loc = i;

  return loc;
}

/* convert a string to lower case */
void
strtolower(char *s)
{
  char *p;

  for(p=s; *p; p++)
    *p = tolower(*p);

}
