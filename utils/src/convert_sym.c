/* convert_sym.c:
 *
 *  Convert a Viewlogic symbol/schematic to gEDA gschem format
 *
 *  accept one argument, the name of the file to 
 *  convert, converted output is displayed on stdout.
 *
 *     Copyright (C) 1999-2002  Mike Jarabek
 *
 *     CHANGE HISTORY:
 *
 *     Updated 8/16/2005 by Jeff McLamb (mclamb AT bustech.com)
 *       - Updated to support gEDA file format version 1
 *       - Added capability to import more graphic styles from ViewDraw
 *       - Corrected bug associated with absense of library reference in
 *         local ViewDraw symbols
 *       - Removed command-line option -s; no longer necessary
 *       - Mapped ViewDraw "SIGNAL" attribute to gEDA "net" attribute
 *       - Mapped ViewDraw "HETERO" attribute to a new "split" attribute
 *       - Mapped ViewDraw "PINTYPE" attributes to correct gEDA pintypes
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

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

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
#define MAX_NODES        1024
#define MAX_POINTS       1024

/* gEDA style enumerators */
typedef enum {END_NONE, END_SQUARE, END_ROUND} OBJECT_END;
typedef enum {TYPE_SOLID, TYPE_DOTTED, TYPE_DASHED, TYPE_CENTER,
              TYPE_PHANTOM, TYPE_ERASE} OBJECT_TYPE;
typedef enum {FILLING_HOLLOW, FILLING_FILL, FILLING_MESH, FILLING_HATCH,
              FILLING_VOID} OBJECT_FILLING;
typedef enum {NORMAL_PIN, BUS_PIN} OBJECT_PINTYPE;

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

/* ViewDraw fill styles:
 * 0  = Hollow, no fill
 * 1  = Solid, full fill
 * 2  = Grey92, mostly fill, thick array of empty space
 * 4  = Grey50, 50% dot fill
 * 6  = Grey08, thicker array of dots
 * 7  = Grey04, thin array of dots
 * 8  = Diagdn2, widely-spaced diagonals from top left to bottom right
 * 11 = Diagdn1, narrowly-spaced diagonals from top left to bottom right
 * 13 = Diagup2, widely-spaced diagonals from bottom left to top right
 * 16 = Diagup1, narrowly-spaced diagonals from bottom left to top right
 * 19 = Horiz, narrowly-spaced horizontal lines
 * 21 = Vert, narrowly-spaced vertical lines
 * 22 = Grid2, widely-spaced square grid
 * 23 = Grid1, narrowly-spaced square grid
 * 24 = X2, widely-spaced diagonal crosshatch
 * 25 = X1, narrowly-spaced diagonal crosshatch
*/

/* ViewDraw line styles:
 * 0 = Solid
 * 1 = Dash
 * 2 = Center, alternating short and long dashes
 * 3 = Phantom, alternating dash and two dots
 * 4 = Big dash
 * 5 = Dot
 * 6 = Dash-dot, alternating dashes and dots
 * 7 = Medium dash
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

/* Fill style structure */
struct FillStyle {
  OBJECT_FILLING fill_type;  /* gEDA object fill type */
  int fill_width;            /* width of the fill lines */
  int fill_angle1;           /* first angle of fill lines */
  int fill_pitch1;           /* first pitch/spacing of fill lines */
  int fill_angle2;           /* second angle of fill lines */
  int fill_pitch2;           /* second pitch/spacing of fill lines */
};

/* index is ViewDraw fill style, entry is above gEDA FillStyle struct */
struct FillStyle fillmap[26] = 
{  
  /* 0  = Hollow, no fill */
  {FILLING_HOLLOW, -1,  -1, -1,  -1, -1},
  /* 1  = Solid, full fill */
  {FILLING_FILL,   -1,  -1, -1,  -1, -1},
  /* 2  = Grey92, mostly fill, thick array of empty space */
  {FILLING_MESH,   20,  45, 30, 135, 30},
  /* 3  = Undefined; assume hollow, no fill */
  {FILLING_HOLLOW, -1,  -1, -1,  -1, -1},
  /* 4  = Grey50, 50% dot fill */
  {FILLING_MESH,   10,  45, 20, 135, 20},
  /* 5  = Undefined; assume hollow, no fill */
  {FILLING_HOLLOW, -1,  -1, -1,  -1, -1},
  /* 6  = Grey08, thicker array of dots */
  {FILLING_MESH,    0,  45, 40, 135, 40}, /* Can't do dots; use mesh */
  /* 7  = Grey04, sparse array of dots */
  {FILLING_MESH,    0,  45, 80, 135, 80}, /* Can't do dots; use mesh */
  /* 8  = Diagdn2, widely-spaced diagonals from top left to bottom right */
  {FILLING_HATCH,   0, 135, 80,  -1, -1},
  /* 9  = Undefined; assume hollow, no fill */
  {FILLING_HOLLOW, -1,  -1, -1,  -1, -1},
  /* 10 = Undefined; assume hollow, no fill */
  {FILLING_HOLLOW, -1,  -1, -1,  -1, -1},
  /* 11 = Diagdn1, narrowly-spaced diagonals from top left to bottom right */
  {FILLING_HATCH,   0, 135, 40,  -1, -1},
  /* 12 = Undefined; assume hollow, no fill */
  {FILLING_HOLLOW, -1,  -1, -1,  -1, -1},
  /* 13 = Diagup2, widely-spaced diagonals from bottom left to top right */
  {FILLING_HATCH,   0,  45, 80,  -1, -1},
  /* 14 = Undefined; assume hollow, no fill */
  {FILLING_HOLLOW, -1,  -1, -1,  -1, -1},
  /* 15 = Undefined; assume hollow, no fill */
  {FILLING_HOLLOW, -1,  -1, -1,  -1, -1},
  /* 16 = Diagup1, narrowly-spaced diagonals from bottom left to top right */
  {FILLING_HATCH,   0,  45, 40,  -1, -1},
  /* 17 = Undefined; assume hollow, no fill */
  {FILLING_HOLLOW, -1,  -1, -1,  -1, -1},
  /* 18 = Undefined; assume hollow, no fill */
  {FILLING_HOLLOW, -1,  -1, -1,  -1, -1},
  /* 19 = Horiz, narrowly-spaced horizontal lines */
  {FILLING_HATCH,  10,   0, 40,  -1, -1},
  /* 20 = Undefined; assume hollow, no fill */
  {FILLING_HOLLOW, -1,  -1, -1,  -1, -1},
  /* 21 = Vert, narrowly-spaced vertical lines */
  {FILLING_HATCH,  10,  90, 40,  -1, -1},
  /* 22 = Grid2, widely-spaced square grid */
  {FILLING_MESH,    0,   0, 80,  90, 80},
  /* 23 = Grid1, narrowly-spaced square grid */
  {FILLING_MESH,    0,   0, 40,  90, 40},
  /* 24 = X2, widely-spaced diagonal crosshatch */
  {FILLING_MESH,    0,  45, 80, 135, 80},
  /* 25 = X1, narrowly-spaced diagonal crosshatch */
  {FILLING_MESH,    0,  45, 40, 135, 40},
}; /* fillmap */

#define FILL_DEFAULT (struct FillStyle){FILLING_HOLLOW, -1, -1, -1, -1, -1}

/* Line style structure */
struct LineStyle {
  int line_width;             /* width of line */
  OBJECT_END line_capstyle;   /* gEDA line cap style (end style) */
  OBJECT_TYPE line_dashstyle; /* gEDA line dash style */
  int line_dashlength;        /* length of line dashes */
  int line_dashspace;         /* space between line dashes */
};

struct LineStyle linemap[8] = 
{  
  /* 0 = Solid */
  {0, END_NONE, TYPE_SOLID,    -1,  -1},
  /* 1 = Dash */
  {0, END_NONE, TYPE_DASHED,  100, 100},
  /* 2 = Center, alternating short and long dashes */
  {0, END_NONE, TYPE_CENTER,  100, 100},
  /* 3 = Phantom, alternating dash and two dots */
  {0, END_NONE, TYPE_PHANTOM, 100, 100},
  /* 4 = Big dash */
  {0, END_NONE, TYPE_DASHED,  400, 100},
  /* 5 = Dot */
  {0, END_NONE, TYPE_DOTTED,   -1, 100},
  /* 6 = Dash-dot, alternating dashes and dots */
  {0, END_NONE, TYPE_CENTER,  100, 100},
  /* 7 = Medium dash */
  {0, END_NONE, TYPE_DASHED,  200, 100},
}; /* linemap */

#define LINE_DEFAULT (struct LineStyle){0, END_NONE, TYPE_SOLID, -1, -1}

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
  {"PKG_TYPE", "footprint", REPLACE_NAME},
  {"LEVEL",    "",          KILL},
  /* {"NC",       "",          KILL}, */
  {"NAME",     "",          KILL},
  {"LABEL",    "",          KILL},
  {"SIGNAL",   "net",       REPLACE_NAME},
  {"HETERO",   "split",     REPLACE_NAME},
};

unsigned int nTranslations = sizeof(translations)/sizeof(struct Translation);

/* local function prototypes */
int convert_file(FILE *fp);
unsigned int strindex(char *s, char c);
unsigned int strrindex(char *s, char c);
void         strtolower(char *s);
int get_continued_string(char *buf, size_t buffer_size, FILE *fp);
int get_style(FILE *fp, unsigned int *colour, 
	      struct LineStyle *linestyle,
	      struct FillStyle *fillstyle);
void set_orientation(int *angle, int *mirror, int orientation);

/* conversion readers */
void do_nop(FILE *fp);
void do_bounding_box(FILE *fp);
void do_unattached_attribute(FILE *fp);
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
void line_object(int x1, int y1, int x2, int y2, unsigned int color,
                 struct LineStyle *linestyle);
void circle_object(int bx, int by, unsigned int radius, unsigned int bcolor,
                   struct LineStyle *linestyle, struct FillStyle *fillstyle);
void pin_object(int x1, int y1, int x2, int y2, unsigned int color,
                OBJECT_PINTYPE pintype, unsigned int whichend);
void box_object(int x1, int y1, unsigned int width, unsigned int height,
		unsigned int color, struct LineStyle *linestyle,
                struct FillStyle *fillstyle);
void arc_object(int x1, int y1, unsigned int radius, 
		int start_angle, int sweep_angle, unsigned int color,
                struct LineStyle *linestyle);
void net_segment(int x1, int y1, int x2, int y2, unsigned int color);
void bus_segment(int x1, int y1, int x2, int y2, unsigned int color,
                 int ripperdir);
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
/* int symbol_mode    = 0; */
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
	  "Usage:\n\t%s <viewlogic_filename>\n"
	  " Where:\n"
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

  while ((ch = getopt (argc, argv, "?h")) != -1) {
    switch (ch) {
/*
    case 's':
      symbol_mode = 1;
      break;
*/      
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
  printf("v 20050313 1\n"); /* Version timestamp 20050313, file version 1 */
  reset_attributes();


  while((c =fgetc(fp)) != EOF) /* fetch record type */
    {
      switch(c) /* branch to appropriate handler */
	{
	case 'D': 
	  do_bounding_box(fp);
	  break;
	    
	case 'U':
	  do_unattached_attribute(fp);
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
#if 0
  unsigned int color;
  struct LineStyle linestyle;
  struct FillStyle fillstyle;
#endif

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

/* Do not draw the bounding box, only retrieve the min and max values */
#if 0
  if (symbol_mode == 0)
  {
    color = GRAPHIC_COLOR;
    linestyle = LINE_DEFAULT;
    fillstyle = FILL_DEFAULT;
    box_object(minx, miny, maxx-minx, maxy-miny, color, &linestyle,
               &fillstyle);
  }
#endif
}


void
do_unattached_attribute(FILE *fp)
{
  int x,y,angle;
  unsigned int dummy, color, size, origin, viewvis;
  unsigned int index;
  unsigned int visibility, show_name_value;
  char text[MAX_TEXTLEN], *name, *value;
  struct LineStyle linestyle;
  struct FillStyle fillstyle;

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

  linestyle = LINE_DEFAULT;
  fillstyle = FILL_DEFAULT;
  get_style(fp, &dummy, &linestyle, &fillstyle);

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
  if (text[index] == '=')
  {
    text[index] = 0;
    value = &text[index+1];
  }
  else
  {
    value = NULL;
  }

  attribute_object( x, y, color, size, visibility, show_name_value, angle,
		    name, value, origin );

}

void
do_attached_attribute(FILE *fp)
{
  int x,y,angle;
  unsigned int color, dummy, size, origin, viewvis;
  unsigned int visibility, show_name_value;
  unsigned int index;
  char text[MAX_TEXTLEN],text2[MAX_TEXTLEN],*name,*value;
  struct LineStyle linestyle;
  struct FillStyle fillstyle;

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
  linestyle = LINE_DEFAULT;
  fillstyle = FILL_DEFAULT;
  get_style(fp, &dummy, &linestyle, &fillstyle);

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
      visibility = 1;
      show_name_value = 2;
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
    /* translate pintype attribute */
    if (strncmp(text, "PINTYPE=", 8) == 0)
    {
      value = &text[strindex(text,'=')+1];
      if (strcmp(value, "ANALOG") == 0)
      {
#ifdef HAVE_SNPRINTF
        snprintf(text, MAX_TEXTLEN, "pintype=pas");
#else
        sprintf(text, "pintype=pas");
#endif
      }
      else if (strcmp(value, "BI") == 0)
      {
#ifdef HAVE_SNPRINTF
        snprintf(text, MAX_TEXTLEN, "pintype=io");
#else
        sprintf(text, "pintype=io");
#endif
      }
      else if (strcmp(value, "IN") == 0)
      {
#ifdef HAVE_SNPRINTF
        snprintf(text, MAX_TEXTLEN, "pintype=in");
#else
        sprintf(text, "pintype=in");
#endif
      }
      else if (strcmp(value, "OCL") == 0)
      {
#ifdef HAVE_SNPRINTF
        snprintf(text, MAX_TEXTLEN, "pintype=oc");
#else
        sprintf(text, "pintype=oc");
#endif
      }
      else if (strcmp(value, "OEM") == 0)
      {
#ifdef HAVE_SNPRINTF
        snprintf(text, MAX_TEXTLEN, "pintype=oe");
#else
        sprintf(text, "pintype=oe");
#endif
      }
      else if (strcmp(value, "OUT") == 0)
      {
#ifdef HAVE_SNPRINTF
        snprintf(text, MAX_TEXTLEN, "pintype=out");
#else
        sprintf(text, "pintype=out");
#endif
      }
      else if (strcmp(value, "TRI") == 0)
      {
#ifdef HAVE_SNPRINTF
        snprintf(text, MAX_TEXTLEN, "pintype=tri");
#else
        sprintf(text, "pintype=tri");
#endif
      }
      else
      {
        fprintf(stderr,"Error: Invalid or unknown pin type \"%s\" for record "
	        "#%d in %s()\n", value, records_processed, __FUNCTION__);
        exit(1);
      }

      /* find name and value pair */
      name = text;
      index = strindex(text,'=');
      text[index] = 0;
      value = &text[index+1];
      attribute_object( x, y, color, size, visibility, show_name_value,
                        angle, name, value, origin );

      /* done attaching pin attributes */
      return;
    }
    /* attach the pinseq and pinnumber attributes */
    if(text[0] == '#')
    {
      strncpy(text2, text, MAX_TEXTLEN);
#ifdef HAVE_SNPRINTF
      snprintf(text, MAX_TEXTLEN, "pinseq=%d",pin_count);
#else
      sprintf(text, "pinseq=%d",pin_count);
#endif
      /* pinseq is invisible */
      visibility = 0;         /* overide any previous settings */
      show_name_value = 1;

      /* find name and value pair */
      name = text;
      index = strindex(text,'=');
      text[index] = 0;
      value = &text[index+1];
      attribute_object( x, y, color, size, visibility, show_name_value,
                        angle, name, value, origin );
      
#ifdef HAVE_SNPRINTF
      snprintf(text, MAX_TEXTLEN, "pinnumber=%s", &text2[2]);
#else
      sprintf(text, "pinnumber=%s", &text2[2]);
#endif
      /* pinnumber is visible */
      visibility = 1;         /* overide any previous settings */
      show_name_value = 1; 
  
      name = text;
      index = strindex(text,'=');
      text[index] = 0;
      value = &text[index+1];
      attribute_object( x, y, color, size, visibility, show_name_value,
                        angle, name, value, origin );

      /* done attaching pin attributes */
      return;
    }
  }
  
  name = text;
  index = strindex(text,'=');
  if (text[index] == '=')
  {
    text[index] = 0;
    value = &text[index+1];
  }
  else
  {
    value = NULL;
  }
  
  attribute_object( x, y, color, size, visibility, show_name_value, angle,
                    name, value, origin );
}

void 
do_text(FILE *fp)
{
  int x,y,angle;
  unsigned int size, origin;
  unsigned int color, show_name_value;
  unsigned int visibility;
  char text[MAX_TEXTLEN];
  struct LineStyle linestyle;
  struct FillStyle fillstyle;

  
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
  linestyle = LINE_DEFAULT;
  fillstyle = FILL_DEFAULT;
  get_style(fp, &color, &linestyle, &fillstyle);

  text_object(x, y, color, size, visibility, show_name_value, angle, text, \
	      origin);

}

void
do_line(FILE *fp)
{
  int x[MAX_POINTS],y[MAX_POINTS];
  unsigned int pairs,color,i;
  struct LineStyle linestyle;
  struct FillStyle fillstyle;
  

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

  /* slurp up trailing CR/NL */
  if (getc(fp) == '\r')
    getc(fp);

  color = GRAPHIC_COLOR;
  linestyle = LINE_DEFAULT;
  fillstyle = FILL_DEFAULT;
  /* now check for an optional style record */
  get_style(fp, &color, &linestyle, &fillstyle);

  /* now, output the line as a series of geda lines */
  for(i=1; i<pairs; i++)
      line_object(x[i-1],y[i-1],x[i],y[i],color,&linestyle);

}


void 
do_pin(FILE *fp)
{
  unsigned int pindir, pinsense, color;
  unsigned int bradius = 25;
  unsigned int bdiameter = 2*bradius;
  unsigned int bcolor = LOGIC_BUBBLE_COLOR;
  int x1, y1, x2, y2, bx, by, bx1, by1, bx2, by2;
  OBJECT_PINTYPE pintype;
  unsigned int whichend;
  struct LineStyle linestyle;
  struct FillStyle fillstyle;

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
  bx1 = x1;
  by1 = y1;
  bx2 = x2;
  by2 = y2;
  color = PIN_COLOR;


  /* Determine the 'whichend' parameter and the coordinates for the "bubble" */
  /* if we need to use one.  We need a "bubble" when pinsense=1. */
  switch (pindir)
  {
    case 0: /* Pin on top */
      if (y1 > y2)
      {
        whichend = 0;
        bx = x2;
        by = y2+bradius;
        by2 += bdiameter;
      }
      else
      {
        whichend = 1;
        bx = x1;
        by = y1+bradius;
        by1 += bdiameter;
      }
      break;
    case 1: /* Pin on bottom */
      if (y1 < y2)
      {
        whichend = 0;
        bx = x2;
        by = y2-bradius;
        by2 -= bdiameter;
      }
      else
      {
        whichend = 1;
        bx = x1;
        by = y1-bradius;
        by1 -= bdiameter;
      }
      break;
    case 2: /* Pin on left */
      if (x1 < x2)
      {
        whichend = 0;
        bx = x2-bradius;
        by = y2;
        bx2 -= bdiameter;
      }
      else
      {
        whichend = 1;
        bx = x1-bradius;
        by = y1;
        bx1 -= bdiameter;
      }
      break;
    case 3: /* Pin on right */
      if (x1 > x2)
      {
        whichend = 0;
        bx = x2+bradius;
        by = y2;
        bx2 += bdiameter;
      }
      else
      {
        whichend = 1;
        bx = x1+bradius;
        by = y1;
        bx1 += bdiameter;
      }
      break;
    default:
      /* Invalid pin direction */
      fprintf(stderr,"Error: Invalid pin direction %d in "
	      "ViewLogic file at record #%d, in function %s()\n",
	      pindir, records_processed, __FUNCTION__);
      exit(1);
  }

  /* if this pin has to be of negative polarity, add a bitty bubble
   * and adjust the size of the pin
   */
  if(pinsense == 1)
  {
    x1 = bx1;
    y1 = by1;
    x2 = bx2;
    y2 = by2;

    linestyle = LINE_DEFAULT;
    fillstyle = FILL_DEFAULT;

    circle_object(bx,by,bradius,bcolor,&linestyle,&fillstyle);
  }

  /* For now, only normal pins are supported */
  pintype = NORMAL_PIN;

  pin_object(x1,y1,x2,y2,color,pintype,whichend);


  add_attributes = 1;   /* add attributes */
  attach_pending = 1;   /* signal that an attachment could be coming */
  pin_attributes = 1;   /* and that they are pin attributes */
  pin_count++;          /* bump the number of pins */

}

void 
do_box(FILE *fp)
{
  int x1, y1, x2, y2, width, height;
  unsigned int color;
  struct LineStyle linestyle;
  struct FillStyle fillstyle;

  
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

  linestyle = LINE_DEFAULT;
  fillstyle = FILL_DEFAULT;
  get_style(fp, &color, &linestyle, &fillstyle);

  box_object(x1,y1,width,height,color,&linestyle,&fillstyle);
}

void
do_circle(FILE *fp)
{
  int x, y;
  unsigned int radius, color;
  struct LineStyle linestyle;
  struct FillStyle fillstyle;

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

  linestyle = LINE_DEFAULT;
  fillstyle = FILL_DEFAULT;
  get_style(fp, &color, &linestyle, &fillstyle);

  circle_object(x,y,radius,color,&linestyle,&fillstyle);
}

void 
do_arc(FILE *fp)
{
  int x1, y1, x2, y2, x3, y3;
  unsigned int color;
  double x2p, y2p, x3p, y3p, yop, xop, xo, yo; 
  double to_rad;
  double gstart, sweep_angle, start_angle, end_angle;
  double radius;
  struct LineStyle linestyle;
  struct FillStyle fillstyle;

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
  linestyle = LINE_DEFAULT;
  fillstyle = FILL_DEFAULT;
  get_style(fp, &color, &linestyle, &fillstyle);

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
      line_object(x1,y1,x2,y2,color,&linestyle);
      line_object(x2,y2,x3,y3,color,&linestyle);
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
	     (int)gstart,(int)sweep_angle, color, &linestyle);

}

void 
do_label(FILE *fp)
{
  int x, y, angle, global, overbar;
  unsigned int color, size, origin, visibility, show_name_value;
  char text[MAX_TEXTLEN];
  char text2[MAX_TEXTLEN];
  struct LineStyle linestyle;
  struct FillStyle fillstyle;
  int i, length;

  /* labels have the following format:
   *   L #X #Y #SIZE #ROTATION #ORIGIN #GLOBAL #VISIBILITY #OVERBAR TEXT
   */

  /* reproduce as simple text, unless it is a label for an object */

  if(fscanf(fp, "%d %d %u %d %d %d %d %d", &x, &y, &size, &angle, &origin,
            &global, &visibility, &overbar) != 8)
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

  /* if ViewDraw showed the label with an overbar, append a '~' to the */
  /* beginning of the name.  gEDA does not support overbars, so the '~' lets */
  /* the designer know he's dealing with an active low signal */
  if (overbar)
  {
    length = strlen(text2);
    text2[length + 1] = 0;
    for (i = length; i > 0; i--)
      text2[i] = text2[i - 1];
    text2[0] = '~';
  }

  linestyle = LINE_DEFAULT;
  fillstyle = FILL_DEFAULT;
  get_style(fp, &color, &linestyle, &fillstyle);

  /* if we are inside a pin definition, mangle the pin name */
  if(net_attributes == 1) /* a netname on a net is its netname */
    {
#ifdef HAVE_SNPRINTF
      snprintf(text, MAX_TEXTLEN, "netname=%s", text2);
#else
      sprintf(text, "netname=%s", text2);
#endif
      show_name_value = 1;
    }

  else if(complex_attributes == 1) /* a label on a complex is its designator */
    {
#ifdef HAVE_SNPRINTF
      snprintf(text, MAX_TEXTLEN, "refdes=%s", text2);
#else
      sprintf(text, "refdes=%s", text2);
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
  int ripperdir;

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

  /* For now, we'll use ripperdir=0 (no nets connected to bus yet) */
  ripperdir = 0;

  /* output a geda bus segment */
  bus_segment(net_nodes_x[n1], net_nodes_y[n1], 
	      net_nodes_x[n2], net_nodes_y[n2], color, ripperdir);

  /* there could be attributes to follow */
  add_attributes = 1;   /* add attributes */
  attach_pending = 1;   /* signal that an attachment could be coming */
  net_attributes = 1;   /* and that they are net attributes */
}

void 
do_instance(FILE *fp)
{
  char text[MAX_TEXTLEN];
  char lib[MAX_TEXTLEN], name[MAX_TEXTLEN], symName[MAX_TEXTLEN];
  unsigned int extension, selectable;
  int x, y, angle, orientation, mirror;
  float scale_factor;
  int result, index, i;

  reset_attributes();

  /* a component instance has the following format
   *  I #instance LIB:NAME #PAGE #X #Y #ROTATION #MAGNIFICATION '
   */

  text[0] = 0;
  lib[0] = 0;
  name[0] = 0;
  extension = 9999;
  x = 0;
  y = 0;

  /* Instance doesn't necessarily have to have the LIB:NAME convention, so */
  /* read this in as a full string and parse later */
  /* if(fscanf(fp,"%*d %[a-zA-Z0-9]:%[a-zA-Z0-9] %u %d %d %d %g %*s\n", */
  result = fscanf(fp,"%*d %s %u %d %d %d %g %*s\n",
                  text, &extension, &x, &y, &orientation, &scale_factor);
  /* find library and symbol name */
  index = strindex(text, ':');
  if (index > 0 || text[0] == ':')
  {
    text[index] = 0;
    strcpy(lib, text);
    strcpy(name, &text[index+1]);
  }
  else
    strcpy(name, text);
  /* Check for input errors */
  if (result < 6)
  {
    fprintf(stderr,"Error: Invalid instance record #%d in %s()\n"
            "lib:'%s', name:'%s'\n"
            "extension:%d, x:%d, y:%d\n",
            records_processed, __FUNCTION__, lib,name,extension, x, y);
    exit(1);
  }
      
  x *= scale;
  y *= scale;

  /* ViewDraw components are always selectable */
  selectable = 1;

  /* Correct orientation */
  set_orientation(&angle, &mirror, orientation);
      
  /* fix case */
  strtolower(name);

  /* replace dashes in the name with underscores */
  for (i = strlen(name) - 1; i >= 0; i--)
    if (name[i] == '-')
      name[i] = '_';

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

/* ViewDraw mirrors components over the x-axis, but gSchem mirrors over the */
/* y-axis. */
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
#if 0
  unsigned int textlen; 
#endif
  unsigned int numlines;

  /* fudge the text size, in viewdraw it is actually the height
   * in geda it is the point size.  The Variable text_size contains
   * the height of the text in points, size contains the height of
   * the text in viewlogic units.
   */
  text_size = (int)(size * 0.72);

  /* Translate ViewDraw text origin to gEDA
   *  ViewDraw's text origin is specified like this:
   * 1 4 7
   * 2 5 8
   * 3 6 9
   * where 1 is the top left corner of the text and 9 is the bottom right,
   * etc.
   * gEDA's text origin is specified like this:
   * 2 5 8
   * 1 4 7
   * 0 3 6
   * so, the below conversion is necessary
   */
  origin = (((origin - 1) / 3) * 6) + 3 - origin;

  /* No longer necessary to emulate ViewDraw text origin, it is now supported */
  /* by gEDA */
#if 0
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
#endif

  /* Translate ViewDraw text rotation to gEDA text angle
   *  ViewDraw's text rotation is specified like this:
   * 0 = 0 degrees, no rotation
   * 1 = 90 degrees
   * 2 = 180 degrees
   * 3 = 270 degrees
   * gEDA's text angle is specified in degrees, so the below conversion is
   * necessary
   */
  angle *= 90;

  /* gEDA now supports rotated text, so we no longer need to force angle to */
  /* be 0 */
#if 0
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
#endif

  /* Since x and y are automatically multiplied by 10 (the scale factor), */
  /* the below operation is not necessary */
#if 0
  /* force text to start on a 10 unit boundary */
  if((x % 10) < 5)
    x = x - (x % 10);
  else
    x = x + (10 - (x % 10));

  if((y % 10) < 5)
    y = y - (y % 10);
  else
    y = y + (10 - (x % 10));
#endif

  /* Only one line of text per statement allowed in ViewDraw, so this is */
  /* always 1 */
  numlines = 1;

  printf( "T %d %d %u %u %u %u %d %u %u\n%s\n", x, y, color, text_size, 
	  visibility, show_name_value, angle, origin, numlines, text);

}


void
attribute_object(int x, int y, unsigned int color, unsigned int  size,
		 unsigned int visibility, unsigned int show_name_value, 
		 int angle, char *name, char *value, unsigned int origin) 
{

  char text[MAX_TEXTLEN], text2[MAX_TEXTLEN];
  char tmpName[MAX_TEXTLEN], tmpValue[MAX_TEXTLEN];
  unsigned int i, j, done, length;

  /* make a copy of the attribute to work with */
  strncpy(tmpName, name, MAX_TEXTLEN-1);
  tmpName[MAX_TEXTLEN-1] = 0;   /* terminate in case strncpy doesnt */
  if (value == NULL)
    tmpValue[0] = 0;   /* no value with ViewDraw attribute */
  else
  {
    strncpy(tmpValue, value, MAX_TEXTLEN-1);
    tmpValue[MAX_TEXTLEN-1] = 0;   /* terminate in case strncpy doesnt */
  }
  
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
		    tmpName, tmpValue,x,y,records_processed);
	    done = 1;
	    return;

	  case WARN_USER:
	    fprintf(stderr,"Warning: attribute name `%s=%s' at (%d,%d) "
		    "at record #%d, found during conversion\n"
		    "\tpassing it through unchanged\n",
		    tmpName,tmpValue,x,y,records_processed);
	    done = 1;
	    break;
	  default:
	    fprintf(stderr,"Error: Unknown action code for attribute\n"
		    "`%s=%s' at record #%d in %s()\n",
		    tmpName,tmpValue,records_processed,__FUNCTION__);
	    exit(1);
	  }
    }

  /* if attribute name was not replaced/dropped, convert to lowercase */
  /* and pass on */
  if (!done)
    strtolower(tmpName);

  /* If we are changing a ViewDraw SIGNAL attribute to a net attribute, */
  /* replace the ';' delimiter with a ':' */
  if (strcmp(tmpName, "net") == 0)
    tmpValue[strindex(tmpValue, ';')] = ':';

  /* If we are changing a ViewDraw HETERO attribute to a split attribute, */
  /* format the new value correctly */
  if (strcmp(tmpName, "split") == 0)
  {
    strcpy(text2, tmpValue);
    strtolower(text2);
    j = 0;
    length = strlen(text2);
    for (i = 0; i < length; i++)
    {
      /* Drop parentheses */
      if (text2[i] != '(' && text2[i] != ')')
      {
        /* Convert dashes to underscores */
        if (text2[i] == '-')
          tmpValue[j++] = '_';
        /* insert gEDA symbol file extension before comma */
        else if (text2[i] == ',')
        {
#ifdef HAVE_SNPRINTF
          snprintf(&tmpValue[j], MAX_TEXTLEN, "-1.sch,");
#else
          sprintf(&tmpValue[j], "-1.sch,");
#endif
          j += 7;
        }
        else
          tmpValue[j++] = text2[i];
      }
    }
    /* append gEDA symbol file extension to the end */
#ifdef HAVE_SNPRINTF
    snprintf(&tmpValue[j], MAX_TEXTLEN, "-1.sch");
#else
    sprintf(&tmpValue[j], "-1.sch");
#endif
  }

  /* If we have an NC attribute with no value, convert to netname=NC */
  if (strcmp(tmpName, "nc") == 0 && tmpValue[0] == 0)
  {
#ifdef HAVE_SNPRINTF
    snprintf(tmpName, MAX_TEXTLEN, "netname");
    snprintf(tmpValue, MAX_TEXTLEN, "NC");
#else
    snprintf(tmpName, "netname");
    sprintf(tmpValue, "NC");
#endif
    show_name_value = 1;
  }

  /* just add an = into the middle */
#ifdef HAVE_SNPRINTF
  snprintf(text, MAX_TEXTLEN, "%s=%s", tmpName, tmpValue);
#else  
  sprintf(text, "%s=%s", tmpName, tmpValue);
#endif

  text_object( x, y, color, size, visibility, show_name_value, \
	       angle, text, origin); 

}


void
line_object(int x1, int y1, int x2, int y2, unsigned int color,
            struct LineStyle *linestyle)
{
  printf("L %d %d %d %d %u %i %i %i %i %i\n", x1, y1, x2, y2, color,
         linestyle->line_width, linestyle->line_capstyle,
         linestyle->line_dashstyle,linestyle->line_dashlength,
         linestyle->line_dashspace);
}

void
circle_object(int bx, int by, unsigned int radius, unsigned int bcolor,
              struct LineStyle *linestyle, struct FillStyle *fillstyle)
{  
  printf("V %d %d %u %u %i %i %i %i %i %i %i %i %i %i %i\n",
         bx, by, radius, bcolor,
         linestyle->line_width, linestyle->line_capstyle,
         linestyle->line_dashstyle,linestyle->line_dashlength,
         linestyle->line_dashspace,
         fillstyle->fill_type, fillstyle->fill_width,
         fillstyle->fill_angle1, fillstyle->fill_pitch1,
         fillstyle->fill_angle2, fillstyle->fill_pitch2);
}

void
pin_object(int x1, int y1, int x2, int y2, unsigned int color,
           OBJECT_PINTYPE pintype, unsigned int whichend)
{
    printf("P %d %d %d %d %u %u %u\n", x1, y1, x2, y2, color,
           pintype, whichend);
}

void
box_object(int x1, int y1, unsigned int width, unsigned int height,
	   unsigned int color, struct LineStyle *linestyle,
           struct FillStyle *fillstyle)
{
  printf("B %d %d %u %u %u %i %i %i %i %i %i %i %i %i %i %i\n", x1, y1,
         width, height, color,
         linestyle->line_width, linestyle->line_capstyle,
         linestyle->line_dashstyle,linestyle->line_dashlength,
         linestyle->line_dashspace,
         fillstyle->fill_type, fillstyle->fill_width,
         fillstyle->fill_angle1, fillstyle->fill_pitch1,
         fillstyle->fill_angle2, fillstyle->fill_pitch2);
}

void
arc_object(int x1, int y1, unsigned int radius, 
	   int start_angle, int sweep_angle, unsigned int color,
           struct LineStyle *linestyle)
{
  printf("A %d %d %u %d %d %u %i %i %i %i %i\n", x1, y1, radius, 
	 start_angle, sweep_angle, color,
         linestyle->line_width, linestyle->line_capstyle,
         linestyle->line_dashstyle, linestyle->line_dashlength,
         linestyle->line_dashspace);
}

void
net_segment(int x1, int y1, int x2, int y2, unsigned int color )
{
  printf("N %d %d %d %d %u\n", x1, y1, x2, y2, color);
}

void
bus_segment(int x1, int y1, int x2, int y2, unsigned int color, int ripperdir)
{
  printf("U %d %d %d %d %u %i\n", x1, y1, x2, y2, color, ripperdir);
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
  /* nuke trailing CR/NL, if there */
  text_len=strlen(buf);
  while (buf[text_len-1] == '\n' || buf[text_len-1] == '\r')
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
      /* nuke trailing CR/NL, if there */
      while (buf[text_len-1] == '\n' || buf[text_len-1] == '\r')
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
	      struct LineStyle *linestyle,
	      struct FillStyle *fillstyle)
{
  int c;
  unsigned int vdfillstyle, vdlinestyle;

  c = getc(fp);
  if(c == 'Q') /* do we have a modifier? */
    {
      if(fscanf(fp,"%u %u %u\n", colour, &vdfillstyle, &vdlinestyle) != 3)
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

      /* re-map vdfillstyle to gEDA FillStyle */
      if (vdfillstyle > 25)
      {
        fprintf(stderr,"Warning: Invalid fill style %u in record #%d, "
                "in %s().  Assuming \"Hollow\" fill style.\n",
                vdfillstyle,records_processed, __FUNCTION__);
        vdfillstyle = 0;
      }
      memcpy(fillstyle, &fillmap[vdfillstyle], sizeof(struct FillStyle));

      /* re-map vdlinestyle to gEDA LineStyle */
      if (vdlinestyle > 7)
      {
        fprintf(stderr,"Warning: Invalid line style %u in record #%d, "
                "in %s().  Assuming \"Solid\" line style.\n",
                vdlinestyle,records_processed, __FUNCTION__);
        vdlinestyle = 0;
      }
      memcpy(linestyle, &linemap[vdlinestyle], sizeof(struct LineStyle));

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
