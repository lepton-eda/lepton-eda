/*************************************************************************/	
/* gmk_sym, a program to create rectangular symbols for gschem           */
/* from a file composed of comma separated lines                         */
/*  From: Jerry O'Keefe, jerryok@pacbell.net                             */
/* Version: 0.000005							 */
/*									 */
/* History:								 */
/* 99/03/23 Fixed pin#= placeholder as define in Component Symbol Guide  */
/* 99/04/02 Fixed pin spacing on right side and changed name to gmk_sym  */
/* 99/04/27 Add Mike Jarabek's updates, alphanumeric pin name support	 */
/*          and improved text spacing                                    */
/* 99/05/02 Add char_width.c support					 */
/*									 */
/* 00/07/12 Major changes to match new styles for text and attributes	 */
/*									 */
/*-----------------------------------------------------------------------*/
/* This program is free software; you can redistribute it and/or modify  */
/* it under the terms of the GNU General Public License as published by  */
/* the Free Software Foundation; either version 2 of the License, or	 */
/* (at your option) any later version.					 */
/* 									 */
/* This program is distributed in the hope that it will be useful,	 */
/* but WITHOUT ANY WARRANTY; without even the implied warranty of	 */
/* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the	 */
/* GNU General Public License for more details.                          */
/*									 */
/*-----------------------------------------------------------------------*/
/* GMK_SYM typical use:                                                  */
/*  gk_sym 7474.txt >7474-3.sym                                          */
/* To build:								 */
/*    gcc -O2 -Wall gmk_sym.c -o gmk_sym     				 */
/*-----------------------------------------------------------------------*/
/*-----------------------------------------------------------------------*/
/* The input file format:				  		 */
/*  1. lines starting with ';' are comment lines, and 			 */
/*     are not processed.						 */
/*  2. The 1st valid line describes a device				 */
/*     1st value: device name						 */
/*     2nd value: visible name					         */
/*     3rd value: visible name location on package,			 */
/*     4th value: box's hoz size, in pins spacings    		         */
/*     5th value: box's ver size, in pins spacings			 */
/*     6th value: uref prefix, like U or J               		 */
/*     7th value: Footprint						 */
/*     8th value: Total number of pins on device (including hidden)	 */
/*  3. All other valid lines describes the symbol's pins		 */
/*     1st value: pin name						 */
/*     2nd value: pin number						 */
/*     3rd value: pin shape, choice of: line, clock, dot&line            */
/*     4th value: side of box to attach the pin,choice of: R, L, T, B    */
/*     5th value: location of pin on side of box, in pin spacings	 */
/*  See the 7474 sample file below					 */
/*************************************************************************/
#if 0
/****************************************************/
;; Filename: 7474.txt
;;   An example of a 7474 symbol make file
; This is a comment line
;; device name ,name, name location(tl,tc,tr,bl,bc,br),X size in pins,Y size in pins 
7474,74HC74,tr,3,5
;;
;; pin name,pin number,shape(line,clock,dot),side(r,l,t,b),pin position
D,2,line,L,1
CLK,3,clock,L,4

Q,5,line,R,1
/Q,6,dot,R,4

CLR,4,dot,T,1
PRE,1,dot,B,1
/****************************************************/
#endif

#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <sys/time.h>
#include <time.h>
#include <errno.h>

/* #include "char_width.c" commented out by AVH, since it's linked in */ 

#define BLACK		0
#define WHITE		1
#define RED		2
#define GREEN		3
#define BLUE		4
#define YELLOW		5
#define CYAN		6
#define GREY		7

#define MAX_FIELDS      10

#define L_SIDE	0
#define R_SIDE	1
#define B_SIDE	2
#define T_SIDE	3

#define LINE_SHAPE  0
#define DOT_SHAPE   1
#define CLOCK_SHAPE 2

extern char *optarg;
extern int optind,opterr,optopt;

int line2fields(char *pBuf,int field_cnt,char *pField[]);
int fields_free(char *pField[]);
int make_pin(int fldcnt,char *pFields[]);
int make_box(int fldcnt,char *pFields[]);
static char *strLabel(char *p, char *pTemp);
void strtrail(char *wrk);
int line_chk(char *pBuf);
#ifndef __CYGWIN32__
int stricmp(char *s, char *p);
#endif
char *TimeStamp(void);

/* add by AVH to make compile warning free */
int GetStringDisplayLength(char *str,int font_size); 

int pin_len=300;
int pin_spacing =300;
int pin_0_x,pin_0_y;
int BoxHeight,BoxWidth;
int net_pin=0;

/***************************************************/
/***************************************************/
int main(int argc,char **argv)
{
  FILE *stream;
  char LineBuf[256];
  int fldcnt,i,c,Debug=0;
  char *pFields[MAX_FIELDS];
  int line_nub=0;

  while ((c = getopt(argc, argv, "?hd:")) != EOF)
        {
        switch (c)
          {
	  case 'd': Debug = 1;
	            break;
	  case '?':
          case 'h': 
                    fprintf(stderr,"usage: %s -dh?\n",argv[0]);
		    exit(0);
		    break;
	  }
        }

  for(i=0;i<MAX_FIELDS;i++)
     pFields[i]=NULL;

  stream=stdin;
  if (argc > 1)
      {
      if ((stream = fopen(argv[1],"r")) == NULL)
         {
         fprintf(stderr, "Cannot open file: %s\n",argv[1]);
         exit(-1);
         }
      }
  line_nub=-1;

  /* printf("v %s\n",TimeStamp()); Original line of code AVH 10/2/00 */
  printf("v 20000704\n"); /* The v character is the version of the file AVH */
  			  /* not a time stamp */
  while (fgets(LineBuf,sizeof(LineBuf)-1,stream) != NULL)
        {
        if (line_chk(LineBuf) < 0)
	   continue;
	if ((fldcnt = line2fields(LineBuf,10,pFields)) > 0)
	   {
	   line_nub++;
	   if (line_nub == 0)
              make_box(fldcnt,pFields);
	   else
              make_pin(fldcnt,pFields);
           fields_free(pFields);
	   }
	}
  fclose(stream);
  return 0;
}

/***************************************************/
/***************************************************/
int fields_free(char *pField[])
{ int i;
  for (i=0; (i<MAX_FIELDS) && (pField[i] != NULL) ;i++)
      {
      free(pField[i]);
      pField[i] = NULL;
      }
  return 0;
}

/***************************************************/
/***************************************************/
int line2fields(char *pBuf,int max_fields,char *pField[])
{ char *p,temp[100];
  int fld_cnt=0;

 if ((p = strchr(pBuf,'\n')) != NULL)
     *p = 0;
 if ((p = strchr(pBuf,'\r')) != NULL)
     *p = 0;
 p = pBuf;
 do {
    pField[fld_cnt] = NULL;
    p = strLabel(p, temp); /* copy the tokens from the string to array */
    pField[fld_cnt] = (char *) malloc(strlen(temp) + 1);
    strcpy(pField[fld_cnt++], temp);
  }  while (*p != 0);
 return fld_cnt;
}

/***************************************************/
/***************************************************/
void cross(int pos_x,int pos_y,int color)
{
   printf("L %d %d %d %d %d\n",pos_x+-50,pos_y,pos_x+50,pos_y,color);
   printf("L %d %d %d %d %d\n",pos_x,pos_y+50,pos_x,pos_y-50,color);
}

/***************************************************/
/***************************************************/
void pin_xy(int dir,char *pin,int font_size,int *x,int *y)
{ int pixs;
  pixs = GetStringDisplayLength(pin,font_size);
  switch(dir)
    {
    case L_SIDE: *x = *x -pixs-100;
                 *y = *y + 50;
	         return;
    case R_SIDE: *x = *x + 100;
                 *y = *y + 50;
	         return;
    case B_SIDE: *x = *x + 75;
                 *y = *y - 150;
	         return;
    case T_SIDE: *x = *x + 75;
                 *y = *y + 75;
	         return;
    }
}

/***************************************************/
/***************************************************/
void pin_add(int pos_x,int pos_y,char *pin,int shape,int dir,char *name)
{ int x,y;
  int name_size=0;
  int xdir=0,ydir=0,font_size=8;
  int shape_offset=0;

  if (shape == 2)
     shape_offset = 75;
  switch (dir)
    {
    case L_SIDE: xdir =  1; ydir =  0;
	    break;
    case R_SIDE: xdir = -1; ydir =  0;
	    break;
    case B_SIDE: xdir =  0; ydir =  1;
	    break;
    case T_SIDE: xdir =  0; ydir = -1;
	    break;
    }

  if (shape == LINE_SHAPE)
     {
     printf("P %d %d %d %d %d\n",pos_x,pos_y,
                                 pos_x-pin_len*xdir,pos_y-pin_len*ydir,
			         WHITE);
     printf("{\n");
     }
  else if (shape == DOT_SHAPE)
     {
     printf("P %d %d %d %d %d\n",pos_x-100*xdir,pos_y-100*ydir,
                                 pos_x-pin_len*xdir,pos_y-pin_len*ydir,
			         WHITE);
     printf("{\n");
     printf("V %d %d 50 %d\n",pos_x-50*xdir,pos_y-50*ydir,CYAN);
     }
  else if (shape == CLOCK_SHAPE)
     {
     printf("P %d %d %d %d %d\n",pos_x,pos_y,
                                 pos_x-pin_len*xdir,pos_y-pin_len*ydir,
				 WHITE);
     printf("{\n");
     printf("L %d %d %d %d %d\n",pos_x-100*ydir,pos_y-100*xdir,
                                 pos_x+100*xdir,pos_y+100*ydir,GREEN);
     printf("L %d %d %d %d %d\n",pos_x+100*ydir,pos_y+100*xdir,
                                 pos_x+100*xdir,pos_y+100*ydir,GREEN);
     }
   x = pos_x;
   y = pos_y;
   pin_xy(dir,pin,font_size,&x,&y);
   printf("T %d %d %d %d 1 1 0 0\n",x,y,YELLOW,font_size);
   printf("pin%d=%s\n",++net_pin,pin);
   if (strlen(name))
     {
     name_size = GetStringDisplayLength(name,font_size);
     switch (dir)
       {
       case L_SIDE: printf("T %d %d %d %d 1 0 0 0\n",pos_x+75+shape_offset,pos_y-50,GREEN,font_size);
	       break;
       case R_SIDE: printf("T %d %d %d %d 1 0 0 0\n",pos_x-name_size-50-shape_offset,pos_y-50,GREEN,font_size);
	       break;
       case B_SIDE: printf("T %d %d %d %d 1 0 0 0\n",pos_x-name_size/2,pos_y+50,GREEN,font_size);
	       break;
       case T_SIDE: printf("T %d %d %d %d 1 0 0 0\n",pos_x-name_size/2,pos_y-150,GREEN,font_size);
	       break;
       }
     printf("%s\n",name);
     }
   printf("}\n");
}

/***************************************************/
/***************************************************/
int make_box(int fldcnt,char *pFields[])
{ int pos_x=300,pos_y=300;
  char name[100],device[100],name_pos[100];
  char uref[100],class[100];
  int pin_width,pin_height,font_size=10;
  int name_size=0;
  int pincount;
  char footprint[100];

  strcpy(device,pFields[0]);
  strcpy(name,pFields[1]);
  strcpy(name_pos,pFields[2]); 
  pin_width  = atoi(pFields[3]); 
  pin_height = atoi(pFields[4]); 
 
  pin_0_x  = pin_spacing; 
  pin_0_y  = pin_spacing*(pin_height + 1); 
  BoxWidth = pin_width * pin_spacing;
  BoxHeight = pin_height * pin_spacing;

  if(fldcnt >=8)
  {
  	strcpy(uref,pFields[5]);
  	strcat(uref,"?"); 
  	if(uref[0]=='U' || uref[0]=='u')strcpy(class,"IC");
  	if(uref[0]=='J' || uref[0]=='j')strcpy(class,"IO");
  	if(uref[0]=='C' || uref[0]=='c')strcpy(class,"IO");
	//  U is for ICs, J or CONN for IO.  We assume no discretes with this tool
	strcpy(footprint,pFields[6]);
	pincount = atoi(pFields[7]);
        printf("T %d %d %d %d 0 0 0 0\n",pos_x,pos_y+BoxHeight+1100,YELLOW,font_size);
        printf("footprint=%s\n",footprint);
        printf("T %d %d %d %d 0 0 0 0\n",pos_x,pos_y+BoxHeight+1300,YELLOW,font_size);
        printf("pins=%d\n",pincount);
  }
  else
  {
	strcpy(class,"IC");
	strcpy(uref,"U?");
  }


  printf("B %d %d %d %d %d\n",pos_x,pos_y,BoxWidth,BoxHeight,GREEN);
  printf("T %d %d %d %d 0 0 0 0\n",pos_x,pos_y+BoxHeight+700,YELLOW,font_size);
  printf("device=%s\n",device);
  printf("T %d %d %d %d 0 0 0 0\n",pos_x,pos_y+BoxHeight+900,YELLOW,font_size);
  printf("class=%s\n",class);
  printf("T %d %d %d %d 1 1 0 0\n",pos_x,pos_y+BoxHeight+500,RED,font_size);
  printf("uref=%s\n",uref);

#if 0
  /* Display pin locations */
  for (i=0;i <= (BoxHeight/pin_spacing);i++)
      cross(pos_x,pos_y+i*pin_spacing,BLUE);

  for (i=0;i <= (BoxWidth/pin_spacing);i++)
      cross(pos_x+i*pin_spacing,pos_y,BLUE);

  cross(pin_0_x,pin_0_y,RED);
#endif
  if (strlen(name))
     {
     name_size = GetStringDisplayLength(name,font_size);
     /* Vaild positions: tl,tc,tr, bl,bc,br */
     if (!stricmp(name_pos,"tl"))
        {
        pos_x = pin_0_x;
        pos_y = pin_0_y+50;
	}
     else if (!stricmp(name_pos,"tc"))
        {
        pos_x = pin_0_x+BoxWidth/2-name_size/2;
        pos_y = pin_0_y+50;
	}
     else if (!stricmp(name_pos,"tr"))
        {
        pos_x = pin_0_x+BoxWidth-name_size/2;
        pos_y = pin_0_y+50;
	}
     else if (!stricmp(name_pos,"bl"))
        {
        pos_x = pin_0_x;
        pos_y = pin_0_y-BoxHeight-175;
	}
     else if (!stricmp(name_pos,"bc"))
        {
        pos_x = pin_0_x+BoxWidth/2-name_size/2;
        pos_y = pin_0_y-BoxHeight-175;
	}
     else if (!stricmp(name_pos,"br"))
        {
        pos_x = pin_0_x+BoxWidth-(name_size)/2;
        pos_y = pin_0_y-BoxHeight-175;
	}
     else
        {
        pos_x = pin_0_x;
        pos_y = pin_0_y+50;
        }
     printf("T %d %d %d %d 1 0 0 0\n",pos_x,pos_y,GREEN,font_size);
     printf("%s\n",name);
     }
  return 0;
}

/***************************************************/
/***************************************************/
int make_pin(int fldcnt,char *pFields[])
{ int pos_x=0,pos_y=0,shape,side=0;
  char pin_name[40];
  char pin[40];
  int pin_pos;

  strcpy(pin_name,pFields[0]);
  strcpy(pin,pFields[1]); 	   // get pin number
  shape = LINE_SHAPE;
  if (!stricmp(pFields[2],"dot"))     // get shape
     shape = DOT_SHAPE;
  if (!stricmp(pFields[2],"clock"))     // get shape
     shape = CLOCK_SHAPE;
  if (!stricmp(pFields[3],"L"))
      side = L_SIDE;
  if (!stricmp(pFields[3],"R"))
      side = R_SIDE;
  if (!stricmp(pFields[3],"B"))
      side = B_SIDE;
  if (!stricmp(pFields[3],"T"))
      side = T_SIDE;
  pin_pos = atoi(pFields[4]);

  pos_x = pin_spacing; 
  if (side == L_SIDE)
     {
     pos_y = pin_0_y - (pin_spacing*pin_pos);
     pos_x = pin_spacing; 
     }
  if (side == R_SIDE)
     {
     pos_y = pin_0_y - (pin_spacing*pin_pos);
     pos_x = pin_spacing + BoxWidth; 
     }
  if (side == B_SIDE)
     {
     pos_x = pin_0_x + (pin_spacing*pin_pos);
     pos_y = pin_spacing;
     }
  if (side == T_SIDE)
     {
     pos_x = pin_0_x + (pin_spacing*pin_pos);
     pos_y = pin_0_y;
     }
  pin_add(pos_x,pos_y,pin,shape,side,pin_name);
  return 0;
}

/***************************************************/
/* Pull a token from a comma separate string       */
/* delete leading and trailing spaces              */
/***************************************************/
static char *strLabel(char *p, char *pTemp)
{
  char *q;
  *pTemp = 0;
  if ((p == NULL) || (pTemp == NULL))
    return NULL;
  q = pTemp;
  while ((*p == ' ') || (*p == '\t'))
    p++;
  while (isprint(*p) && (*p != ','))    /* copy string to pTemp */
    *q++ = *p++;
  *q = 0;                       /* terminate the string     */
  strtrail(pTemp);              /* drop any trailing spaces */
  if (*p == ',')
    p++;
  return p;
}

/************************************************/
/* Clear white spaces from the end of a string  */
/************************************************/
void strtrail(char *wrk)
{
  char *p;
  if (wrk == NULL)
    return;
 if ((p = strchr(wrk,'\n')) != NULL)
     *p = 0;
 if ((p = strchr(wrk,'\r')) != NULL)
     *p = 0;
  while (isspace(*(wrk + strlen(wrk) - 1)))     /* Clear any trailing spaces */
    *(wrk + strlen(wrk) - 1) = 0;
}

/************************************************/
/* Check for empty or comment lines             */
/************************************************/
int line_chk(char *pBuf)
{
  char *p;
  if (pBuf == NULL)
    return -1;
  if ((p = strchr(pBuf,'\n')) != NULL)
     *p = 0;
 if ((p = strchr(pBuf,'\r')) != NULL)
     *p = 0;
  while (isspace(*(pBuf + strlen(pBuf) - 1)))     /* Clear any trailing spaces */
    *(pBuf + strlen(pBuf) - 1) = 0;
  if (*pBuf == ';')
     return -1;
  if (strchr(pBuf,',') == NULL)
      return -1;
  return 0;
}

/************************************************/
/* Compare two string without regard for case   */
/************************************************/
#ifndef __CYGWIN32__
int stricmp(char *s, char *p)
{
  for (; toupper(*s) == toupper(*p); s++, p++)
    if (!(*s))
      return 0;
  return toupper(*s) - toupper(*p);
}
#endif

/********************************/
/*  Create a timestamp string   */
/********************************/
char *TimeStamp(void)
{
  static char time_string[200];
  struct tm *pTm;
  time_t now;

  now = time(NULL);
  pTm = localtime(&now);
  sprintf(time_string, "%d%02d%02d",
          1900+pTm->tm_year, pTm->tm_mon + 1, pTm->tm_mday);
  return time_string;
}

