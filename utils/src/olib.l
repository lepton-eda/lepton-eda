%{
/*  $Id$

    olib.lex - Orcad to gEDA lib converter
    Copyright (C) 2002 Mario Pascucci <m.pas@libero.it>

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
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
    or
    visit the URL http://www.fsf.org/

*/

#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <ctype.h>
#include <limits.h>
#include <math.h>

/* gEDA file version: change NEW version as soon as available */
#define EDA_VERSION_OLD   "20020209"
#define EDA_VERSION_NEW	  "20020825"


/* defines for flags and types */
#define PIN_DOTFLAG	0x0001
#define PIN_SHORTFLAG	0x0002
#define PIN_CLKFLAG	0x0004

#define PIN_TYPE_IO	1
#define PIN_TYPE_IN	2
#define PIN_TYPE_OUT	3
#define PIN_TYPE_PWR	4
#define	PIN_TYPE_PAS	5
#define PIN_TYPE_OC	6
#define PIN_TYPE_HIZ	7
#define PIN_TYPE_OE	8


#ifndef GCC_VERSION
#define GCC_VERSION (__GNUC__ * 1000 + __GNUC_MINOR__)
#endif /* GCC_VERSION */

#if GCC_VERSION > 2007
#define ATTRIBUTE_UNUSED __attribute__((unused)) 
#else
#define ATTRIBUTE_UNUSED
#endif 

static const ATTRIBUTE_UNUSED char *mp_name = "OrCAD to gEDA lib converter";
static const ATTRIBUTE_UNUSED char *mp_license = "Copyright (c) 2002 Mario Pascucci <m.pas@libero.it>";

static char fnsym[512];	    /* gEDA symbol file name */
static FILE *fsym;	    /* FILE* to gEDA symbol file */
static int aliascount = 0;    /* alias count */
static char part_aliases[200][50];     /* store for aliases of part */
static char sym_prefix[50];	/* prefix for .sym file */

/* gEDA symbol file version */
static char sym_version[10];
static int use_old_version;	  /* flag for old format */

/* pin name options */
static int pin_name_hidden;

static char buff[512];

static int sizex,sizey,partspp;	  /* size and number of part per package */

static int pincount;	    /* pin definitions found until now */

struct pin_def {	/* pin definition struct */
  char	pos;	  /* position: L,T,R,B */
  short	num;	  /* number in side, 0 mean virtual pin */
  short	pin;	  /* real pin number */
  short	flags;	  /* DOT, CLK and SHORT flags */
  short	type;	  /* I/O, IN, OUT, PWR, PASS, OC */
  char  name[50]; /* simbolic name of pin */
};


/* REFERENCE handling */
static char ref_str[20];


/* VECTOR handling */
static int vector_count;	/* number of vector found */

struct vector_def {
  char	type;		/* line, arc, circle, box */
  float p[12];		/* parameters for element drawing */
  char *str;		/* string for "TEXT" statement */
};

static int paramcount;	/* vector param number count */

static struct vector_def part_vectors[100];   /* draw list */

static int vector_found;


/* type of pin */
static char *pintypestr[] = {
  "???",
  "I/O",
  "IN",
  "OUT",
  "PWR",
  "PAS",
  "OC",
  "hiZ",
  "OE"
};


static char *edapintype[] = {
  "???",
  "io",
  "in",
  "out",
  "pwr",
  "pas",
  "oc",
  "tri",
  "oe"
};


 /* pin definition database */
static struct pin_def pinlist[200];


/* generic counter */
static int ii;



/* strip filename-aware chars from part name,
    leave only '.' '-' or '_' and changes space
    in '_' */
char *strip_invalid(char *str)
{
  char *p;

  p = buff;
  while (*str)
  {
    if (isalnum(*str) || strchr("-_. ",*str))
    {
      if (*str == ' ')
	*p = '_';
      else
	*p = *str;
      p++;
    }
    str++;
  }
  *p = 0;
  return buff;
}


/* trim leading and trailing ' in ident */
char *strip_quotes(char *str)
{
  char *p;

  p = buff;
  str++;
  while (*str != '\'')
  {
    *p = *str;
    p++;
    str++;
  }
  *p = 0;
  return buff;
}



/* symbol write function in gEDA .sym format */
#include "geda_sym_format.h"


%}



 /* decimal integer */
INTEGER		[+-]?[0-9]+

 /* floating point decimal, like 123.234 */
DECIMAL		[+-]?[0-9]*[\.][0-9]*

 /* string name, like 'abcd' */
IDENT		'[^'\n]+'

 /* comment, like { comment } */
COMMENT		\{[^\n\}]*\}

 /* pin identifier, example T3 L12 R3 .... */
PIN_ID		[TBLR][0-9]{1,4}



%s wait_END wait_sizex wait_sizey wait_parts pin_list pinrealnum pindata 
%s waitnamestart in_name waitnameend waitpinend vector_list vector_ignore
%s param_list convert convert_ignore_pin reference


%%

%{
  /*******************************************
  handling of:
  PREFIX
  'prefix1'
  'prefix2'
  ...
  ...
  END
  *******************************************/
%}

{COMMENT} {
  /* ignore comments */
  fprintf(stderr,"Ignore comment: %s\n",yytext);
 }


<INITIAL>PREFIX	{
  /* shortcuts for parts name: ignored */
  fprintf(stderr,"Found PREFIX\n");
  BEGIN(wait_END);
 }


<wait_END>^{IDENT}.*\n {
  fprintf(stderr,"Shorthand found, ignored\n");
  /* ignore shortcut list */
 }


<wait_END>END {
  /* founds shortcut list end */
  fprintf(stderr,"Found END for shorthands\n");
  BEGIN(INITIAL);
 }

%{

  /*******************************************
  handling of:
  'IDENT'
  'IDENT'
  ...
  *******************************************/
%}

<INITIAL>^{IDENT} {
  /* founds first part identifier */
  strcpy(part_aliases[0],strip_invalid(yytext));
  snprintf(fnsym,511,"%s-%s-1.sym",sym_prefix,part_aliases[0]);
  fprintf(stderr,"New part name: %s (%s)\n",part_aliases[0],yytext);
  fprintf(stderr,"New .sym name: %s\n",fnsym);
  BEGIN(wait_sizex);
  aliascount = 1;
  pincount = 0;
  vector_found = 0;
  ref_str[0] = 'U';
  ref_str[1] = 0;
 }


<wait_sizex>^REFERENCE {
  /* REFERENCE statement found */
  BEGIN(reference);
 }


<reference>{IDENT} {
  /* get an IDENT, i.e. reference id */
  strcpy(ref_str,strip_quotes(yytext));
  fprintf(stderr,"REFERENCE: %s\n",yytext);
  BEGIN(wait_sizex);
 }


<wait_sizex>^{IDENT} {
  /* found an alias for the part name 
      store alias for future use */
  fprintf(stderr,"Found an alias: %s\n",yytext);
  strcpy(part_aliases[aliascount], strip_invalid(yytext));
  aliascount++;
 }


%{
  /*******************************************
  handling of:
  {X Size =} x      {Y Size =} y      {Parts per Package =} n
  *******************************************/
%}

<wait_sizex>{INTEGER} {
  /* trovata la dimansione X */
  sizex = atoi(yytext);
  fprintf(stderr,"Found X size: %d (%s)\n",sizex,yytext);
  BEGIN(wait_sizey);
 }


<wait_sizey>{INTEGER} {
  /* trovata la dimansione y */
  sizey = atoi(yytext);
  fprintf(stderr,"Found Y size: %d (%s)\n",sizey,yytext);
  BEGIN(wait_parts);
 }


<wait_parts>GRIDARRAY {
  /* found a PGA component, unsupported */
  fprintf(stderr,"Found a GRIDARRAY: unsupported\n");
  BEGIN(INITIAL);
 }


<wait_parts>{INTEGER} {
  /* trovato il parts-per-package */
  /* per ora supportato solo 1 part-per-package */
  partspp = atoi(yytext);
  if (partspp > 1)
  {
    fprintf(stderr,"Found parts per package > 1\n");
    BEGIN(INITIAL);
  }
  else
  {
    fprintf(stderr,"Found parts per package: %d (%s)\n",partspp,yytext);
    BEGIN(pin_list);
    /* size of symbol body */
    sizex = sizex * PIN_SPACE;
    sizey = sizey * PIN_SPACE;
  }
 }


<INITIAL>^{PIN_ID}.+\n |
<INITIAL>^VECTOR.*\n |
<INITIAL>^CONVERT.*\n |
<INITIAL>^TEXT.*\n |
<INITIAL>^LINE.*\n |
<INITIAL>^ARC.*\n |
<INITIAL>^FILL.*\n |
<INITIAL>^CIRCLE.*\n |
<INITIAL>^END.*\n     {
  /* ignored */
  fprintf(stderr,"Ignored: %s",yytext);
 }


%{
  /*******************************************
  handling of:
  VECTOR 'IDENT'  (unsupported)
  or
  VECTOR
  LINE x y x1 y1
  ARC x y dxs dys dxe dye r
  CIRCLE x y r
  FILL x y    (unsupported)
  TEXT x y size 'text'	(partially supported)
  END
  *******************************************/
%}

<pin_list>^VECTOR {
  /* components isn't a simple "box", but it's drawed */
  BEGIN(vector_list);
  vector_count = 0;
  vector_found = -1;
  fprintf(stderr,"Found: VECTOR\n");
 }


<vector_list,vector_ignore>{IDENT} {
  /* the component shape is identical to a previous one 
      unsupported at the moment */
  BEGIN(pin_list);
  fprintf(stderr,"Found unsupported: VECTOR 'IDENT'\n");
 }


<vector_list>LINE {
  /* found LINE vector statement */
  part_vectors[vector_count].type = 'L';
  BEGIN(param_list);
  paramcount = 0;
  fprintf(stderr,"Found: LINE params:");
 }


<vector_list>CIRCLE {
  /* found CIRCLE vector statement */
  part_vectors[vector_count].type = 'V';
  BEGIN(param_list);
  paramcount = 0;
  fprintf(stderr,"Found: CIRCLE params:");
 }


<vector_list>ARC {
  /* found ARC vector statement */
  part_vectors[vector_count].type = 'A';
  BEGIN(param_list);
  paramcount = 0;
  fprintf(stderr,"Found: ARC params:");
 }


<vector_list>FILL {
  /* found FILL vector statement */
  /* no counterpart in gEDA */
  part_vectors[vector_count].type = 'f';
  BEGIN(param_list);
  paramcount = 0;
  fprintf(stderr,"Found unsupported: FILL params:");
 }


<vector_list>TEXT {
  /* found ARC vector statement */
  part_vectors[vector_count].type = 'T';
  BEGIN(param_list);
  paramcount = 0;
  fprintf(stderr,"Found: TEXT params:");
 }


<param_list>{INTEGER} {
  /* found a param */
  part_vectors[vector_count].p[paramcount] = (float) atof(yytext);
  paramcount++;
  BEGIN(param_list);
  fprintf(stderr," %s",yytext);
 }


<param_list>{DECIMAL} {
  /* found a param */
  part_vectors[vector_count].p[paramcount] = (float) atof(yytext);
  paramcount++;
  BEGIN(param_list);
  fprintf(stderr," %s",yytext);
 }


<param_list>{IDENT} {
  /* found a string */
  if (part_vectors[vector_count].type == 'T')
    part_vectors[vector_count].str = strdup(strip_quotes(yytext));
  else
    fprintf(stderr,"Text param found in non text VECTOR instr: %s\n",yytext);
  paramcount++;
  BEGIN(param_list);
  fprintf(stderr," %s",yytext);
 }


<param_list>\n	{
  /* end of params */
  fprintf(stderr,"\n");
  switch (part_vectors[vector_count].type)
  {
    case 'L':
      /* line */
      if (paramcount != 4)
      {
	fprintf(stderr,"LINE: wrong parameters number: %d\n",paramcount);
	BEGIN(vector_list);
      }
      break;
    case 'V':
      /* circle */
      if (paramcount != 3)
      {
	fprintf(stderr,"CIRCLE: wrong parameters number: %d\n",paramcount);
	BEGIN(vector_list);
      }
      break;
    case 'A':
      /* arc */
      if (paramcount != 7)
      {
	fprintf(stderr,"ARC: wrong parameters number: %d\n",paramcount);
	BEGIN(vector_list);
      }
      break;
    case 'f':
      /* fill (ignored) */
      if (paramcount != 2)
      {
	fprintf(stderr,"FILL: wrong parameters number: %d\n",paramcount);
	BEGIN(vector_list);
      }
      break;
    case 'T':
      /* text */
      if (paramcount != 4)
      {
	fprintf(stderr,"TEXT: wrong parameters number: %d\n",paramcount);
	BEGIN(vector_list);
      }
      break;
  }
  vector_count++;
  BEGIN(vector_list);
 }


<vector_list>^END {
  /* end of vector list */
  BEGIN(pin_list);
  fprintf(stderr,"Found: END of VECTOR list\n");
 }


%{
  /*******************************************
  handling of:
  CONVERT 'IDENT'
  or
  CONVERT
  pin_def
  pin_def
  ...
  *******************************************/
%}

<pin_list,convert>^CONVERT {
  /* alternate part drawing */
  /* unsupported */
  BEGIN(convert);
 }


<convert>{IDENT} {
  /* CONVERT 'part' unsupported */
  BEGIN(pin_list);
 }


<convert>^{PIN_ID} {
  /* ignore PIN list in CONVERT */
  BEGIN(convert_ignore_pin);
 }


<convert_ignore_pin>.*\n {
  /* ignore line */
  BEGIN(convert);
 }


<convert>^VECTOR {
  /* found VECTOR list in CONVERT */
  if (vector_found)
    BEGIN(vector_ignore);
  else
  {
    BEGIN(vector_list);
    vector_count = 0;
    vector_found = -1;
  }
 }


<vector_ignore>^END.*\n {
  /* end of ignore */
  BEGIN(pin_list);
 }
<vector_ignore>^VECTOR.*\n |
<vector_ignore>^CONVERT.*\n |
<vector_ignore>^TEXT.*\n |
<vector_ignore>^LINE.*\n |
<vector_ignore>^ARC.*\n |
<vector_ignore>^FILL.*\n |
<vector_ignore>^CIRCLE.*\n {
  /* ignored */
  fprintf(stderr,"Ignored VECTOR: %s",yytext);
 }


%{
  /*******************************************
  handling of:
  pin_def
  pin_def
  ...
  *******************************************/
%}

<pin_list>^{PIN_ID} {
  /* trovato un pin */
  pinlist[pincount].pos = yytext[0];    /* pin side */
  pinlist[pincount].num = atoi(yytext+1);	/* gets pin number */
  pinlist[pincount].flags = 0;
  pinlist[pincount].type = 0;
  fprintf(stderr,"Found pin ID: %c-%d (%s)\n",pinlist[pincount].pos,pinlist[pincount].num,yytext);
  if (partspp == 0)   /* handling of parts without pin number */
  {
    /* assign number to a unnumbered pin from the pin count */
    pinlist[pincount].pin = pincount + 1;
    BEGIN(pindata);
  }
  else
    BEGIN(pinrealnum);
 }


<pin_list>^{IDENT} {
  /* found a part name after pinlist, this mean that part definition ends,
      and starts a new part definition */
  fprintf(stderr,"Writing down new symbol: %s pin: %d  vectors: %d\n",part_aliases[0],pincount,vector_count);
  write_sym();	    /* write the .sym gEDA symbol definition */
  /* restore initial condition */
  ref_str[0] = 'U';
  ref_str[1] = 0;
  pincount = 0;
  aliascount = 1;
  vector_found = 0;
  vector_count = 0;
  strcpy(part_aliases[0],strip_invalid(yytext));
  snprintf(fnsym,511,"%s-%s-1.sym",sym_prefix,part_aliases[0]);
  fprintf(stderr,"New part name: %s (%s)\n",part_aliases[0],yytext);
  fprintf(stderr,"New .sym name: %s\n",fnsym);
  BEGIN(wait_sizex);
 }



<pinrealnum>{INTEGER} {
  /* reads real pin number */
  pinlist[pincount].pin = atoi(yytext);
  fprintf(stderr,"Found pin num: %d (%s)\n",pinlist[pincount].pin,yytext);
  BEGIN(pindata);
 }


<pindata>DOT {
  /* DOT flag found, boolean negate */
  pinlist[pincount].flags |= PIN_DOTFLAG;
  fprintf(stderr,"Found DOT flag\n");
 }


<pindata>CLK {
  /* CLK flag found, clock input */
  pinlist[pincount].flags |= PIN_CLKFLAG;
  fprintf(stderr,"Found CLK flag\n");
 }


<pindata>SHORT {
  /* SHORT flag found, short terminal */
  pinlist[pincount].flags |= PIN_SHORTFLAG;
  fprintf(stderr,"Found SHORT flag\n");
 }


<pindata>IN {
  /* pin type IN */
  pinlist[pincount].type = PIN_TYPE_IN;
  BEGIN(waitnamestart);
  fprintf(stderr,"Pin type IN\n");
 }


<pindata>OUT {
  /* pin type OUT */
  pinlist[pincount].type = PIN_TYPE_OUT;
  BEGIN(waitnamestart);
  fprintf(stderr,"Pin type OUT\n");
 }


<pindata>PWR {
  /* pin type PWR */
  pinlist[pincount].type = PIN_TYPE_PWR;
  BEGIN(waitnamestart);
  fprintf(stderr,"Pin type PWR\n");
 }


<pindata>OC {
  /* pin type OC */
  pinlist[pincount].type = PIN_TYPE_OC;
  BEGIN(waitnamestart);
  fprintf(stderr,"Pin type OC\n");
 }


<pindata>OE {
  /* pin type OE */
  pinlist[pincount].type = PIN_TYPE_OE;
  BEGIN(waitnamestart);
  fprintf(stderr,"Pin type OE\n");
 }


<pindata>PAS {
  /* pin type PAS */
  pinlist[pincount].type = PIN_TYPE_PAS;
  BEGIN(waitnamestart);
  fprintf(stderr,"Pin type PAS\n");
 }


<pindata>hiZ {
  /* pin type hiZ */
  pinlist[pincount].type = PIN_TYPE_HIZ;
  BEGIN(waitnamestart);
  fprintf(stderr,"Pin type hiZ\n");
 }


<pindata>"I/O" {
  /* pin type I/O */
  pinlist[pincount].type = PIN_TYPE_IO;
  BEGIN(waitnamestart);
  fprintf(stderr,"Pin type I/O\n");
 }


<waitnamestart>"'" {
  /* found initial quote */
  ii = 0;
  BEGIN(in_name);
 }


<in_name>'' {
  /* found a ' in pin name */
  pinlist[pincount].name[ii++] = '\'';
 }


<in_name>[^"'"\n] {
  /* get a char from the pin symbolic name */
  pinlist[pincount].name[ii++] = *yytext;
 }


<in_name>"'" {
  /* all ok, waiting for new pin definition */
  pinlist[pincount].name[ii] = 0;
  fprintf(stderr,"Pin label: %s\n",pinlist[pincount].name);
  BEGIN(waitpinend);
 }


<waitpinend>\n {
  /* ok, found a newline, pin definition ends */
  fprintf(stderr,"Pin: %c%d %d %x %s %s\n",pinlist[pincount].pos,pinlist[pincount].num,
    pinlist[pincount].pin, pinlist[pincount].flags, pintypestr[pinlist[pincount].type],
    pinlist[pincount].name);
  pincount++;
  BEGIN(pin_list);
 }


%{
  /* 
   * we don't need to use <*> to match any state, we just don't
   * specify a state
   */
%}

^\n 	{ /* empty lines ignored */ }


[ \t\n\r]    { /* ignored, the \r char is for "DOS" ASCII files */ }





%%


void usage(void)
{
  fprintf(stderr,"Usage:\n");
  fprintf(stderr,"\tolib [options] libname prefix\n");
  fprintf(stderr,"\nWhere:\n\tlibname is the path to OrCAD(TM) library to convert\n");
  fprintf(stderr,"\tprefix is the prefix for the .sym files extracted\n");
  fprintf(stderr,"Options are:\n\t-h\tprint this help and exit\n");
  fprintf(stderr,"\t-o\tuse old format (v 20020209) for .sym file\n");
  fprintf(stderr,"\t\telse use 'v 20020825' as version string in .sym file\n");
  fprintf(stderr,"\t-n\thides pin name if symbol is VECTOR drawed\n");
}

int yywrap()
{
  /* found a part name after pinlist, this mean that part definition ends,
      and starts a new part definition */
  fprintf(stderr,"Writing down new symbol: %s pin: %d  vectors: %d\n",part_aliases[0],pincount,vector_count);      
  write_sym();	    /* write the .sym gEDA symbol definition */
  /* restore initial condition */
  fprintf(stderr,"End of lib file\n");

  return 1;
}


int main (int argc, char *argv[])
{

  FILE	*flib;
  char	fnlib[PATH_MAX];
  int	i;

  strcpy(sym_version,EDA_VERSION_NEW);	  /* handle new gEDA version by default */
  use_old_version = 0;	      /* old version flag */
  pin_name_hidden = 0;	      /* hides pin name on VECTOR drawed symbols */
  opterr = 0;	/* do NOT print default error message for getopt */
  do
  {
    i = getopt(argc,argv,"hon");
    switch (i)
    {
      case 'h':
	usage();
	exit(0);
      case 'o':
	strcpy(sym_version,EDA_VERSION_OLD);
	fprintf(stderr,"Use old (%s) format.\n",sym_version);
	use_old_version = -1;
	break;
      case 'n':
	pin_name_hidden = -1;
	break;
      case '?':
	fprintf(stderr,"Unknown or illegal option :%c\n",(char)optopt);
	usage();
	exit(1);
      case ':':
	fprintf(stderr,"Parameter wrong for option: %c\n",(char)optopt);
	usage();
	exit(1);
    }
  }
  while (i != -1);
  if ((argc - optind) != 2)
  {
    fprintf(stderr,"Library name and prefix are mandatory.\n");
    usage();
    return 1;
  }
  strcpy(fnlib,argv[optind]);
  if (!(flib = fopen(fnlib,"r")))
  {
    perror("lib file");
    return 1;
  }
  strcpy(sym_prefix,argv[optind+1]);
  yyin = flib;
  yylex();
  fclose(flib);
  return 0;  
}


