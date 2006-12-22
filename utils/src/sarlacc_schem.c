/*   Orcad.c  v 0.92
 *   Copyright 1999 Matthew Ettus
 *   For more info email matt@ettus.com
 *   Ths code is released under the terms of the GNU GPL
 *   See www.fsf.org for a copy of the license
 *
 *  Changes 0.94 by <egil@kvaleberg.no>, october 5th 2002
 *    Scaling defaults to 200%
 *    Bus implemented - but still no bus entries!
 *    Check for stack overwrite and other horrors
 *    Changed orcad_xsize/orcad_ysize to sarlacc_dim
 *    Port improved
 *    Command line options
 *
 *  Todo:
 *    Hierarchy
 *    Bus entries
 *    Many details - see BAD
 */

/*   This program will convert an ORCAD SDT IV file to geda format */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <string.h>
#ifdef HAVE_GETOPT_H
#include <getopt.h>
#endif

#include <libgeda/colors.h>

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

/*
 *  command line options
 */
#define SARVERSION "0.94"
#define GEDAVERSION "20020825"

#define DEFAULT_SCALE 200 /* was 100 */

char *symbol_dir = 0;
int scale = DEFAULT_SCALE;

#define TEXTSIZE ((scale <= 100) ? 6 : 10)

/*
 *  orcad
 */
#define GET_TAG(VAL) (VAL & 0x0F) 

int CONV16(char *base,int offset)
{
	int retval;
	retval = ((base[offset+1] & 255) <<8) | (base[offset] & 255);
	if(base[offset+1]&128)
		retval = retval | (65535U << 16);
	return retval;
}

#define CONV32(VAR,POS) (VAR[POS]+VAR[POS+1]*256+VAR[POS+2]*65536+VAR[POS+3]*256*16777216)

#define CONV(X)  ( (scale/10)*X )
#define CONVX(X) CONV(X)
#define CONVY(Y) ( 32700 - ((scale/10)*Y) )

#define HDR_LEN 0x20
#define BYTECOUNT       0x16
#define DATE    0x05
#define PATH    0x3B
#define REV     0x60
#define TITLE   0x64
#define ORG     0x91
#define ADDR1   0xBE
#define ADDR2   0xEB
#define ADDR3   0x118
#define ADDR4   0x145

/* change return type from int to void AVH */
void remove_spaces(char *src)
{
	char *ptr=src;
	while (*ptr != 0)
	{
		if(*ptr == ' ')
			*ptr = '_';
		ptr++;
	}
}

/*
 *  read block from Orcad file
 *  return size
 */
unsigned read_block(int fd, char *block, int block_min_size,int block_max_size)
{
    char sizebuf[2];
    unsigned size;

    read(fd,sizebuf,2);
    size = CONV16(sizebuf,0);
    if (size < block_min_size) {
	fprintf(stderr,"Segment too small; size %d, min is %d\n",
						size, block_min_size);
	exit(1);
    }
    if (size > block_max_size) {
	fprintf(stderr,"Segment too large; size %d, max is %d\n",
						size, block_max_size);
	exit(1);
    }
    if (read(fd,block,size) != size) {
	fprintf(stderr,"File truncated\n");
	exit(1);
    }
    return size;
}

unsigned read_string(char *dest, int dest_size, char *src)
{
    unsigned size = ((unsigned char *)src)[0];

    if (size+1 > dest_size) {
	fprintf(stderr,"Text too large; size %d, max is %d\n",
						size, dest_size-1);
	exit(1);
    }
    strncpy(dest,src+1,size);
    dest[size] = '\0';
    return size;
}

/* change return type from int to void AVH */
void read_string_file(int fd,char *dest, int dest_size)
{
    unsigned char len;

    if (read(fd,&len,1) != 1) {
	fprintf(stderr,"File truncated\n");
	exit(1);
    }
    if (len+1 > dest_size) {
	fprintf(stderr,"Text too large; size %d, max is %d\n",
						len, dest_size-1);
	exit(1);
    }
    if (len > 0) {
	if (read(fd,dest,len) != len) {
	    fprintf(stderr,"File truncated\n");
	    exit(1);
	}
    }
    dest[len] = '\0';
}

/*
 *
 */
void parse_header(int fd1,int fd2)
{
    unsigned char localbuf[32];
    int length;

    read(fd1,localbuf,32);
    if( strncmp(localbuf,"Schematic FILE",14) )
    {
	fprintf(stderr,"\nFile is not an ORCAD 16 Bit Schematic\n");
	exit(1);
    }

    length=CONV32(localbuf,BYTECOUNT);    
    fprintf(stderr,"length: %d\n",length);

    lseek(fd2,length+HDR_LEN,SEEK_SET);
}

/* BAD more titleblock stuff */
void parse_titleblock(int fd)
{
    int size,sheet,total,ypos;
    char localbuf[1000];
    char data[100];
    char pagesize;
	
    size = read_block(fd,localbuf,5,sizeof(localbuf));
    // fprintf(stderr,"\nTitleblock %d bytes\n",size);

    sheet=CONV16(localbuf,0x00);
    total=CONV16(localbuf,0x02);
    fprintf(stderr,"Sheet #%d of %d\n",sheet,total);
    read_string(data,sizeof(data),localbuf+DATE);
    fprintf(stderr,"%s\n",data);

    switch(localbuf[4] && 0x0F)
    {
	case 0: pagesize = 'A'; ypos = 8*scale+scale/2; break;
	case 1: pagesize = 'B'; ypos = 11*scale; break;
	case 2: pagesize = 'C'; ypos = 17*scale; break;
	case 3: pagesize = 'D'; ypos = 22*scale; break;
	case 4: pagesize = 'E'; ypos = 34*scale; break;
	default:  fprintf(stderr,"Unknown Page Size\n");exit(-1);
    }
    if (scale==100) {
	fprintf(stdout,"C %d %d 0 0 0 title-%c.sym\n",CONVX(0),CONVY(ypos),pagesize);
    }
}

/*  BAD  Rotation and mirroring origin issues */
/* Other component label issues */
void parse_component(int fd1,int fd2)
{
    char localbuf[1000];
    char partname[256];
    char filename[512];
    char full_filename[1024];
    int size;
    int x,y;
    int xpos = 0,ypos = 0,xpossav,ypossav;
    int xgeda,ygeda;
    int angle,mirror;
    int i = 0;
    int sarlacc_xsize = 0, sarlacc_ysize = 0;
    int sarlacc_xoffset = 0, sarlacc_yoffset = 0;
    int attribcnt;

    int refx,refy,ref_vis;
    char refdes[32];
    int valx,valy,val_vis;
    char value[64];
    char attrib[64];
    char attribsav[64];

    char flags;
    char vis;
	
    int pointer;
    FILE *cfp;
    char buff[128];
	
    size = read_block(fd1,localbuf,29,sizeof(localbuf));

    x=CONV16(localbuf,0);
    y=CONV16(localbuf,2);

    refx = CONVX(x + CONV16(localbuf,4));
    refy = CONVY(y + CONV16(localbuf,6));
	
    valx = CONVX(x + CONV16(localbuf,8));
    valy = CONVY(y + CONV16(localbuf,10));

    xgeda = CONVX(x);
    ygeda = CONVY(y);

    if(localbuf[12] & 0x80) mirror=1;
    else mirror=0;

    angle=0;
    if (localbuf[12] & 0x20) angle=90;
    if (localbuf[12] & 0x40) angle+=180;
/* BAD decode and use device number, fix rotation offset */

    ref_vis=val_vis=1;

    flags = localbuf[13];
    if (flags & 2) ref_vis=0;
    if (flags & 4) val_vis=0;
/* BAD decode more flags */

    vis = localbuf[14];

    /* 14-27 */

    pointer = 28 + read_string(refdes,sizeof(refdes),localbuf+28) +1;
    pointer = pointer + 1 +read_string(value,sizeof(value),localbuf+pointer);

    read_string_file(fd2,partname,sizeof(partname));
    remove_spaces(partname);
 // fprintf(stderr,"Component %s: %s\n",refdes,partname);
    snprintf(filename,sizeof(filename),"%s-1.sym", partname);
    if (symbol_dir) {
	snprintf(full_filename,sizeof(full_filename),"%s/%s", 
					       symbol_dir, filename);
    } else {
	snprintf(full_filename,sizeof(full_filename),"%s", filename);
    }

    cfp = fopen(full_filename, "r");
    if (cfp != NULL) {
	/* "sarlacc_dim=" set by sarlacc_sym */
	while (!feof(cfp)) {
	  fgets(buff, 128, cfp);
	  if (!strncmp(buff, "sarlacc_dim=", 12)) {
	    sscanf(buff+12, "%d,%d,%d,%d", 
	    &sarlacc_xoffset,&sarlacc_yoffset,&sarlacc_xsize,&sarlacc_ysize);
	  }
	}
	fclose(cfp);

	fprintf(stderr,"ref: %s dim = %d %d %d %d angle = %d mirror = %d\n",
	      refdes,
	      sarlacc_xoffset, sarlacc_yoffset,
	      sarlacc_xsize, sarlacc_ysize, angle, mirror);

	switch (angle) {
	default: /* 0 */
	    if (mirror) {
		xgeda = xgeda + sarlacc_xsize + sarlacc_xoffset;
	    } else {
		xgeda = xgeda - sarlacc_xoffset;
	    }
	    ygeda = ygeda - (sarlacc_ysize + sarlacc_yoffset);
	    break;
	case 90:
	    xgeda = xgeda + sarlacc_ysize + sarlacc_yoffset;
	    if (mirror) {
		/* BAD untested */
		ygeda = ygeda + sarlacc_xoffset;
	    } else {
		ygeda = ygeda - (sarlacc_xsize + sarlacc_xoffset);
	    }
	    break;
	case 180:
	    if (mirror) {
		xgeda = xgeda - sarlacc_xoffset;
	    } else {
		xgeda = xgeda + sarlacc_xsize + sarlacc_xoffset;
	    }
	    ygeda = ygeda + sarlacc_yoffset;
	    break;
	case 270:
	    xgeda = xgeda - sarlacc_yoffset;
	    if (mirror) {
		/* BAD untested */
		ygeda = ygeda - (sarlacc_xsize + sarlacc_xoffset);
	    } else {
		ygeda = ygeda + sarlacc_xoffset;
	    }
	    break;
	}
    } else {
	fprintf(stderr,"Couldn't find symbol %s in file: %s\n"
		       "Position on sheet will be uncertain\n", partname, full_filename);
    }

    fprintf(stdout,"C %d %d 1 %d %d %s\n",
	xgeda,ygeda,angle,mirror,filename);
    fprintf(stdout,"{\n");

#if 0
    /* For sarlacc debugging purposes, it's useful to see
       if a component is mirrored and how much it's rotated */
       fprintf(stdout,"T %d %d %d %d %d 1 0 0\nmirror=%d\n",
				refx,refy,GRAPHIC_COLOR,TEXTSIZE,0,mirror);
       fprintf(stdout,"T %d %d %d %d %d 1 0 0\nrotation=%d\n",
				refx,refy,GRAPHIC_COLOR,TEXTSIZE,0,angle);
#endif
    if (refdes[0] != 0) {
      if (value[0] && refx==valx && refy==valy) {
	/* prevent text overlap */
	refy += scale;
      }
      fprintf(stdout,"T %d %d %d %d %d 1 0 0\nrefdes=%s\n",
	      refx,refy,ATTRIBUTE_COLOR,TEXTSIZE,ref_vis,refdes);
    }

    if (value[0] != 0) {
      fprintf(stdout,"T %d %d %d %d %d 1 0 0\nvalue=%s\n",
	      valx,valy,ATTRIBUTE_COLOR,TEXTSIZE,val_vis,value);
    }

    attribcnt = 0;
    if(flags & 0x40) {
	for(i=0;i<8;i++) {
	  /* This assumes that the last attribute is the footprint */
	    xpos = CONVX(x + CONV16(localbuf,pointer));
	    ypos = CONVY(y + CONV16(localbuf,pointer+2));
	    pointer += 4;
	    size = read_string(attrib,sizeof(attrib),localbuf+pointer);
	    pointer += size + 1;
	    if (size > 0) {
	      attribcnt++;
	      fprintf(stdout,"T %d %d %d %d %d 1 0 0\npattern=%s\n",
				xpos,ypos,ATTRIBUTE_COLOR,TEXTSIZE,
		      ( (flags & (1<<i))?1:0 ),attrib);
	      xpossav = xpos;
	      ypossav = ypos;
	      strcpy(attribsav, attrib);
	    }
	}
    }
    if (attribcnt > 0 && attrib[0]) {
      fprintf(stdout,"T %d %d %d %d %d 1 0 0\n"
		     "footprint=%s\n",
		     xpos,ypos,ATTRIBUTE_COLOR,TEXTSIZE,
	      ( (flags & (1<<i))?1:0 ),attrib);
    }
    fprintf(stdout,"}\n");
}

/*  BAD  Sheets need work  */
void parse_sheet (int fd)
{
    char localbuf[1000];
    char filename[1000];
    char filetext[1000];
    int size;
    int index;
    int n;
    int x1,y1,x2,y2;

    size = read_block(fd,localbuf,15,sizeof(localbuf));
    // fprintf(stderr,"Sheet %d bytes\n",size);

    x1=CONVX(CONV16(localbuf,0));
    y1=CONVY(CONV16(localbuf,2));    

    x2=CONV(CONV16(localbuf,4));
    y2=CONV(CONV16(localbuf,6));
    index = 8;

    /* BAD 5 bytes - dunno? */
    index += 5;

    n = 1+read_string(filename,sizeof(filename),localbuf+index);
    index += n;
    n = 1+read_string(filetext,sizeof(filetext),localbuf+index);
    index += n;

    /* BAD Implement Hierarchy properly! */
    fprintf(stderr,"Hierarchy\n");
    fprintf(stderr,"xy = %d %d %d %d\n",x1,y1,x2,y2);
    for (n=8; n<13; ++n) fprintf(stderr,"%02x ",localbuf[n] & 0xff);
    fprintf(stderr,"\nfile = %s\n",filename);
    fprintf(stderr,"text = %s\n",filetext);

    /* BAD not the way to do it... */
    fprintf(stdout,"C %d %d 0 0 0 include-1.sym\n",x1,y1-y2);
    fprintf(stdout,"{\n");
    fprintf(stdout,"B %d %d %d %d %d 0 0 0 -1 -1 0 -1 -1 -1 -1 -1\n",
					x1,y1-y2,x2,y2,GRAPHIC_COLOR);
    fprintf(stdout,"T %d %d %d %d 0 1 0 0\n"
		   "source=%s\n",x1,y1-y2,ATTRIBUTE_COLOR,TEXTSIZE,filename);
    fprintf(stdout,"T %d %d %d %d 1 1 0 0\n"
		   "%s\n",x1,(y1-y2)-scale,ATTRIBUTE_COLOR,TEXTSIZE,filetext);
    fprintf(stdout,"}\n");
}
	
static int pending_netlabel=0;
static char netlabel[256];
static int netlabel_x, netlabel_y, netlabel_angle;

/* BAD Set wire color properly  */
static void wire_or_bus(int fd, char kind, int color)
{
    char localbuf[32];
    int size;
    int x1,y1,x2,y2;

    size = read_block(fd,localbuf,8,sizeof(localbuf));

    x1=CONVX(CONV16(localbuf,0));
    y1=CONVY(CONV16(localbuf,2));

    x2=CONVX(CONV16(localbuf,4));
    y2=CONVY(CONV16(localbuf,6));                         
    fprintf(stdout,"%c %d %d %d %d %d 0 0 0 -1 -1\n",kind,x1,y1,x2,y2,color);
    if (pending_netlabel) {
      fprintf(stdout,"{\n");
      fprintf(stdout,"T %d %d %d %d 1 1 %d 0\n", netlabel_x, netlabel_y,
			      ATTRIBUTE_COLOR, TEXTSIZE, netlabel_angle);
      fprintf(stdout,"label=%s\n", netlabel); /* BAD netname= */
      fprintf(stdout,"}\n");
      pending_netlabel = 0;
    }
}

void parse_wire (int fd)
{
    wire_or_bus(fd, 'N', NET_COLOR);
}

/*  BAD Haven't implemented GEDA buses */
/*  but guessing that Orcad busses are parsed just like wires... */
void parse_bus (int fd)
{
    wire_or_bus(fd, 'U', BUS_COLOR);
}  

/*  BAD How do we handle junctions in GEDA? */
/* 19990726 I think we don't need to worry
 * ORCAD splits wires at junction points
 */

void parse_junction (int fd)
{
    char localbuf[32];
    int size;

    size = read_block(fd,localbuf,4,sizeof(localbuf));

/*
    x=CONVX(CONV16(localbuf,0));
    y=CONVY(CONV16(localbuf,2));
    fprintf(stderr,"Junctions %d %d\n",x,y); 
*/

}

/* BAD Fix handling of ports */

void parse_port (int fd)
{
    char localbuf[1024];
    char textbuf[1024];
    int size;
    int x,y;
    int w;
    int m;
    int mirror = 0;

    size = read_block(fd,localbuf,7,sizeof(localbuf));

    x = CONVX(CONV16(localbuf,0));
    y = CONVY(CONV16(localbuf,2));
    w = localbuf[4] & 0xff;
    m = localbuf[5] & 0xff;
    read_string(textbuf,sizeof(textbuf),localbuf+6);

    // fprintf(stderr,"PORT %s %d %d %d 0x%x\n",textbuf,x,y,w,m);

    switch (m & 0x60) {
    case 0x40: /* 0101 */
    case 0x20: /* 1010 */
	x += scale + w * (scale/10);
	break;
    case 0x00: /* 0000 */
	       /* 1001 */
    case 0x60: /* 1111 */
	mirror = 1;
	break;
    }
    fprintf(stdout,"C %d %d 1 0 %d input-orcad-1.sym\n",x,y,mirror);
    fprintf(stdout,"{\n"
		   "T %d %d %d 8 1 1 0 0\nvalue=%s\n"
		   "}\n",x,y,GRAPHIC_COLOR,
								      textbuf);
}  

/* BAD Fix Labels attach to wire.  Multiline issues?*/
/* Fix text sizing */
void parse_label (int fd)
{
    char localbuf[1000];
    char textbuf[1000];
    int size;
    int x,y;
    int angle;
    int textsize;

    size = read_block(fd,localbuf,5,sizeof(localbuf));

    x=CONVX(CONV16(localbuf,0));
    y=CONVY(CONV16(localbuf,2));

    read_string(textbuf,sizeof(textbuf),localbuf+0x06);

    angle=0;
    textsize = 5* CONV16(localbuf,4);
    if (textsize<0)
    {
	textsize *= -1;
	angle = 90;
    }
    /*    fprintf(stdout,"T %d %d %d %d 1 1 %d 0\n",x,y,GRAPHIC_COLOR, TEXTSIZE, angle);
	  fprintf(stdout,"net=%s ATTACHME\n",textbuf);                    */
    pending_netlabel = 1;
    strncpy(netlabel, textbuf, 256);
    netlabel_x = x;
    netlabel_y = y;
    netlabel_angle = angle;
}  

/* BAD Fix Entries */

void parse_entry (int fd)
{
    char localbuf[32];
    int size;
    int x,y,type;

    size = read_block(fd,localbuf,5,sizeof(localbuf));
    // fprintf(stderr,"Entry %d bytes\n",size);

    x=CONVX(CONV16(localbuf,0));
    y=CONVY(CONV16(localbuf,2));
    type=localbuf[4];
    fprintf(stderr,"Entry %d %d type %d\n",x,y,type);
}

/* BAD Fix Dashed Lines */

void parse_dashed (int fd)
{
    char localbuf[32];
    int size;
    int x,y,type;

    size = read_block(fd,localbuf,4,sizeof(localbuf));
    fprintf(stderr,"Dashed  %d bytes\n",size);

    x=CONVX(CONV16(localbuf,0));
    y=CONVY(CONV16(localbuf,2));
    type=localbuf[4];                                    
}

/* BAD Fix power */
/* How do netlisters handle power connections/nets? */

void parse_power (int fd)
{
    char localbuf[256];
    char textbuf[256];
    char *symbol;
    int size;
    int x,y,xtext,ytext;
    int angle;
    char type;

    size = read_block(fd,localbuf,5,sizeof(localbuf));
    // fprintf(stderr,"POWER %d bytes\n",size);

    read_string(textbuf,sizeof(textbuf),localbuf+0x05);

    x=CONVX(CONV16(localbuf,0));
    y=CONVY(CONV16(localbuf,2));
    type = localbuf[4];
    switch(type & 0x0C)
    {
	case 0x04: angle = 180; xtext = x; ytext = y - 600; break;
	case 0x08: angle = 90; ytext = y; xtext = x-600; break;
	case 0x0C: angle = 270;ytext = y; xtext = x+600; break;
	default: angle = 0; xtext=x;ytext = y+600;
    }
    switch(type & 0x03)
    {
/*  BAD  GEDA only has bar and circle pix.  Also, they 
 *  All say VCC or VDD, which they should not */
	case 0x02:
		symbol = "vcc-orcad-bar-1.sym";break; /* BAR */
	case 0x00: /* circle */
	case 0x01: /* arrow */
	case 0x03: /* wave */
	default:
		symbol = "vcc-orcad-circle-1.sym";break;
    }
    fprintf(stdout,"C %d %d 1 %d 0 %s\n",x,y,angle,symbol);
    /*    fprintf(stdout,"{\n"
			 "T %d %d %d %d 1 1 %d 0\n"
			 "value=%s\n"
			 "}\n",
	    xtext,ytext,GRAPHIC_COLOR,TEXTSIZE,angle%180,textbuf);*/
    fprintf(stdout,"{\n"
		   "T %d %d %d %d 1 1 %d 0\n"
		   "net=%s:1\n"
		   "}\n",
	    xtext,ytext,GRAPHIC_COLOR,TEXTSIZE,angle%180,textbuf);
}

/*  BAD Fix Text color and check rotation */
/*  BAD Fix multi-line text */

void parse_text (int fd)
{
    char localbuf[1024];
    char textbuf[1024];
    int size;
    int x,y,textsize,angle;

    size = read_block(fd,localbuf,7,sizeof(localbuf));

    x=CONVX(CONV16(localbuf,0));
    y=CONVY(CONV16(localbuf,2));
    read_string(textbuf,sizeof(textbuf),localbuf+6);

    angle=0;
    textsize = TEXTSIZE * CONV16(localbuf,4);
    if (textsize<0)
    {
	textsize *= -1;
	angle = 90;
    }
    fprintf(stdout,"T %d %d %d %d 1 1 %d 0\n",x,y,GRAPHIC_COLOR,textsize,angle);
    fprintf(stdout,"%s\n",textbuf);
}

/* BAD - Markers are unimplemented in gEDA (yet).  
 * They are the no-connects that you can place on pins to
 * exempt them from the connectivity checks in DRC/ERC 
 */

void parse_marker (int fd)
{
    char localbuf[1024];
    int size;

    size = read_block(fd,localbuf,0,sizeof(localbuf));

    /* fprintf(stderr,"MARKER %d\n",size); */
}


int parse_block(int fd1,int fd2)
{
    char tag;
    read(fd1,&tag,1);
    switch(GET_TAG(tag))
    {
	case 0x00:
	    parse_titleblock(fd1);
	    break;
	case 0x01:
	    parse_sheet(fd1);
	    break;
	case 0x02:
	    parse_component(fd1,fd2);
	    break;
	case 0x03:
	    parse_wire(fd1);
	    break;
	case 0x04:
	    parse_bus(fd1);
	    break;
	case 0x05:
	    parse_junction(fd1);
	    break;
	case 0x06:
	    parse_port(fd1);
	    break;
	case 0x07:
	    parse_label(fd1);
	    break;
	case 0x08:
	    parse_entry(fd1);
	    break;
	case 0x09:
	    parse_dashed(fd1);
	    break;
	case 0x0a:
	    parse_power(fd1);
	    break;
	case 0x0b:
	    parse_text(fd1);
	    break;
	case 0x0c:
	    parse_marker(fd1);
	    break;
	case 0x0f:
	    return 0;
	    break;
	default:
	    fprintf(stderr,"\nUnknown Block Tag\n");
	    exit(-1);
	    break;
    }
	
		
    return 1;
}

int
main(int argc, char **argv)
{
    int c;
    int fd1,fd2;

    while ((c = getopt(argc, argv, "d:hs:v")) > 0) {
	switch (c) {
	case 'd':
	    symbol_dir = optarg;
	    break;
	case 's':
	    scale = atoi(optarg);
	    break;
	case 'v':
	    fprintf(stderr,"sarlacc_scheme ver %s\n", SARVERSION);
	    exit(0);
	    break;
	case 'h':
	default:
	    fprintf(stderr,"Convert Oracd schematics file (16 bit format) to gEDA\n");
	  usage:
	    fprintf(stderr,"\nUsage:   %s [options] infile >outfile\n"
			   "\nOptions:"
			   "\n         -d<dir>  directory for symbols (from sarlacc_sym)"
			   "\n         -h       help"
			   "\n         -s<n>    scale <n>%%, default is %d"
			   "\n         -v       version"
			   "\n\n",
			   argv[0],DEFAULT_SCALE);
	    exit(1);
	    break;
	}
    }

    if( optind+1 != argc )
    {
	goto usage;
    }
	
    /* BAD update to latest file format.. */
    fprintf(stdout,"v %s\n",GEDAVERSION);

    fd1 = open(argv[optind],O_RDONLY);
    if( fd1 < 0 )
    {
	fprintf(stderr,"\nCould not open input file: %s\n",argv[optind]);
	exit(1);
    }
    fd2 = open(argv[optind],O_RDONLY);
    if( fd2 < 0 )
    {
	fprintf(stderr,"\n  Could not open input file part deux\n");
	exit(-1);
    }
	
    parse_header(fd1,fd2);
	
    while(parse_block(fd1,fd2));
    fprintf(stderr,"\n Normal End\n");

    return(0);    
}




