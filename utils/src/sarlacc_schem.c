/*   Orcad.c  v 0.92
 *   Copyright 1999 Matthew Ettus
 *   For more info email matt@ettus.com
 *   Ths code is released under the terms of the GNU GPL
 *   See www.fsf.org for a copy of the license
 */

/*   This program will convert an ORCAD SDT IV file to geda format */
 
#include<stdio.h>
#include<stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <string.h>

#define GET_TAG(VAL) (VAL & 0x0F) 
/*
#define CONV16(VAR,POS) ( (VAR[POS]&255)|(VAR[POS + 1]&255)<<8 )
*/


int CONV16(char *base,int offset)
{
	/* int b1, b2, b3; unused variables? AVH */
	int retval;
	retval = ((base[offset+1] & 255) <<8) | (base[offset] & 255);
	if(base[offset+1]&128)
		retval = retval | (65535<<16);
	return retval;
}

#define CONV32(VAR,POS) (VAR[POS]+VAR[POS+1]*256+VAR[POS+2]*65536+VAR[POS+3]*256*16777216)

#define CONVX(X) ( 10*X )
#define CONVY(Y) ( 32700 - (10*Y) )

#define HDR_LEN	0x20
#define BYTECOUNT	0x16
#define DATE 	0x05
#define PATH 	0x3B
#define REV  	0x60
#define TITLE	0x64
#define ORG	0x91
#define ADDR1	0xBE
#define ADDR2	0xEB
#define ADDR3	0x118
#define ADDR4	0x145

#define TEXTCOLOR 3
#define TEXTSIZE 6

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

char read_string(char *dest, char *src)
{
    strncpy(dest,src+1,src[0]);
    dest[src[0]]=0;
    return src[0];
}


/* change return type from int to void AVH */
void read_string_file(int fd,char *dest)
{
    char len;
    read(fd,&len,1);
    read(fd,dest,len);
    dest[len]=0;
}

void parse_header(int fd1,int fd2)
{
    char localbuf[32];
    int length;
    read(fd1,localbuf,32);
    if( strncmp(localbuf,"Schematic FILE",14) )
    {
	fprintf(stderr,"\nFile is not an ORCAD 16 Bit Schematic\n");
	exit(-1);
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
    
    read(fd,localbuf,2);
    size=CONV16(localbuf,0);
    fprintf(stderr,"\nTitleblock %d bytes\n",size);
    read(fd,localbuf,size); 
    sheet=CONV16(localbuf,0x00);
    total=CONV16(localbuf,0x02);
    fprintf(stderr,"Sheet #%d of %d\n",sheet,total);
    read_string(data,localbuf+DATE);
    fprintf(stderr,"%s\n",data);

    switch(localbuf[4] && 0x0F)
    {
	case 0:	pagesize = 'A'; ypos = 850; break;
	case 1:	pagesize = 'B'; ypos = 1100; break;
	case 2:	pagesize = 'C'; ypos = 1700; break;
	case 3:	pagesize = 'D'; ypos = 2200; break;
	case 4:	pagesize = 'E'; ypos = 3400; break;
	default:  fprintf(stderr,"Unknown Page Size\n");exit(-1);
    }
    fprintf(stdout,"C %d %d 0 0 0 title-%c.sym\n",CONVX(0),CONVY(ypos),pagesize);
}

/*  BAD  Rotation and mirroring origin issues */
/* Other component label issues */
void parse_component(int fd1,int fd2)
{
    char localbuf[1000];
    char partname[256];
    int size;
    int x,y;
    int xpos,ypos;
    int xgeda,ygeda;
    int angle,mirror;
    int i;

    int refx,refy,ref_vis;
    char refdes[32];
    int valx,valy,val_vis;
    char value[64];
    char attrib[64];

    char flags;
    char vis;
    
    int pointer;
    
    read(fd1,localbuf,2);
    size=CONV16(localbuf,0);
    read(fd1,localbuf,size);

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
    if(localbuf[12] & 0x20)angle=90;
    if(localbuf[12] & 0x40)angle+=180;
/* BAD decode and use device number, fix rotation offset */

    ref_vis=val_vis=1;

    flags = localbuf[13];
    if(flags & 2) ref_vis=0;
    if(flags & 4) val_vis=0;
/* BAD decode more flags */

    vis = localbuf[14];

    /* 14-27 */

    pointer = 28 + read_string(refdes,localbuf+28) +1;
    pointer = pointer + 1 +read_string(value,localbuf+pointer);


    read_string_file(fd2,partname);
    remove_spaces(partname);
    fprintf(stderr,"Component: %s\n",partname);

    fprintf(stdout,"C %d %d 1 %d %d %s-1.sym\n",
	xgeda,ygeda,angle,mirror,partname);
    fprintf(stdout,"{\n");

    fprintf(stdout,"T %d %d %d %d %d 1 0\nuref=%s\n",
	refx,refy,TEXTCOLOR,TEXTSIZE,ref_vis,refdes);
    fprintf(stdout,"T %d %d %d %d %d 1 0\nvalue=%s\n",
	valx,valy,TEXTCOLOR,TEXTSIZE,val_vis,value);

    if(flags & 0x40)
	for(i=0;i<8;i++)
	{
	    xpos = CONVX(x + CONV16(localbuf,pointer));
	    ypos = CONVY(y + CONV16(localbuf,pointer+2));
	    pointer += 4;
	    size = read_string(attrib,localbuf+pointer);
	    pointer += size + 1;
	    if(size > 0)
		fprintf(stdout,"T %d %d %d %d %d 1 0\npattern=%s\n",
	    		xpos,ypos,TEXTCOLOR,TEXTSIZE,
	    		( (flags & (1<<i))?1:0 ),attrib);
	} 

    fprintf(stdout,"}\n");
}

/*  BAD  Sheets need work  */
void parse_sheet (int fd)
{
    char localbuf[1000];
    /* char sheetname[256]; unused variable? AVH */
    int size;
    int x1,y1,x2,y2;

    read(fd,localbuf,2);
    size=CONV16(localbuf,0);
    fprintf(stderr,"Sheet %d bytes\n",size);
    read(fd,localbuf,size);                   

    x1=CONVX(CONV16(localbuf,0));
    y1=CONVY(CONV16(localbuf,2));    

    x2=CONVX(CONV16(localbuf,4));
    y2=CONVY(CONV16(localbuf,6));    

    fprintf(stderr,"%d %d %d %d\n",x1,y1,x2,y2);

    fprintf(stderr,"SHEET %d bytes\n",size);
    fprintf(stderr,"gEDA doesn't do Hierarchy yet\n");
    exit(-1);
}
 
/* BAD Set wire color properly  */
void parse_wire (int fd)
{
    char localbuf[32];
    int size;
    int x1,y1,x2,y2;

    read(fd,localbuf,2);
    size=CONV16(localbuf,0);
    read(fd,localbuf,size);

    x1=CONVX(CONV16(localbuf,0));
    y1=CONVY(CONV16(localbuf,2));

    x2=CONVX(CONV16(localbuf,4));
    y2=CONVY(CONV16(localbuf,6));                         
    fprintf(stdout,"N %d %d %d %d 4\n",x1,y1,x2,y2);
}

/*  BAD GEDA doesn't do buses yet! */
void parse_bus (int fd)
{
    fprintf(stderr,"GEDA Can't Handle Buses\n");
    exit(-1);
}  

/*  BAD How do we handle junctions in GEDA? */
/* 19990726 I think we don't need to worry
 * ORCAD splits wires at junction points
 */

void parse_junction (int fd)
{
    char localbuf[32];
    int size;
    /* int x,y; unused variable? AVH */

    read(fd,localbuf,2);
    size=CONV16(localbuf,0);
    read(fd,localbuf,size);

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

    read(fd,localbuf,2);
    size=CONV16(localbuf,0);
    read(fd,localbuf,size);
    x=CONVX(CONV16(localbuf,0));
    y=CONVY(CONV16(localbuf,2));

    read_string(textbuf,localbuf+0x06);
    fprintf(stderr,"PORT %s %d %d\n",textbuf,(int)localbuf[4],(int)localbuf[5]);
    fprintf(stdout,"C %d %d 1 0 0 input-orcad-1.sym\n",x,y);
    fprintf(stdout,"{\nT %d %d 3 8 1 1 0\nvalue=%s\n}\n",x,y,textbuf);
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

    read(fd,localbuf,2);
    size=CONV16(localbuf,0);
    read(fd,localbuf,size);
    x=CONVX(CONV16(localbuf,0));
    y=CONVY(CONV16(localbuf,2));

    read_string(textbuf,localbuf+0x06);

    angle=0;
    textsize = 5* CONV16(localbuf,4);
    if (textsize<0)
    {
        textsize *= -1;
        angle = 90;
    }
    fprintf(stdout,"T %d %d 3 6 1 1 %d\n",x,y,angle);
    fprintf(stdout,"label=%s ATTACHME\n",textbuf);                  
}  

/* BAD Fix Entries */

void parse_entry (int fd)
{
    char localbuf[32];
    int size;
    int x,y,type;

    read(fd,localbuf,2);
    size=CONV16(localbuf,0);
    fprintf(stderr,"Entry %d bytes\n",size);
    read(fd,localbuf,size);

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

    read(fd,localbuf,2);
    size=CONV16(localbuf,0);
    fprintf(stderr,"Dashed  %d bytes\n",size);
    read(fd,localbuf,size);

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

    read(fd,localbuf,2);
    size=CONV16(localbuf,0);
    fprintf(stderr,"POWER %d bytes\n",size);
    read(fd,localbuf,size);
    read_string(textbuf,localbuf+0x05);    

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
    fprintf(stdout,"{\nT %d %d %d %d 1 1 %d\nvalue=%s\n}\n",
	xtext,ytext,TEXTCOLOR,TEXTSIZE,angle%180,textbuf);
}

/*  BAD Fix Text color and check rotation */
/*  BAD Fix multi-line text */

void parse_text (int fd)
{
    char localbuf[1024];
    char textbuf[1024];
    int size;
    int x,y,textsize,angle;

    read(fd,localbuf,2);
    size=CONV16(localbuf,0);
    read(fd,localbuf,size);                         

    x=CONVX(CONV16(localbuf,0));
    y=CONVY(CONV16(localbuf,2));
    read_string(textbuf,localbuf+0x06); 

    angle=0;
    textsize = 5* CONV16(localbuf,4);
    if (textsize<0)
    {
	textsize *= -1;
	angle = 90;
    }
    fprintf(stdout,"T %d %d 3 %d 1 1 %d\n",x,y,textsize,angle);
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
    /* int x,y,type; unused variables? AVH */

    read(fd,localbuf,2);
    size=CONV16(localbuf,0);
    read(fd,localbuf,size);      

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
    /* int count=0; unused variables? AVH */
    /* int length; */
    /* int sheet; */
    int fd1,fd2;

    if( argc != 2 )
    {
	fprintf(stderr,"\n  Usage:  %s infile  >outfile\n",argv[0]);
	exit(-1);
    }
    
    fprintf(stdout,"v 1990705\n");

    fd1 = open(argv[1],O_RDONLY);
    if( fd1 < 1 )
    {
	fprintf(stderr,"\n  Could not open input file\n");
	exit(-1);
    }
    fd2 = open(argv[1],O_RDONLY);
    if( fd2 < 1 )
    {
	fprintf(stderr,"\n  Could not open input file part deux\n");
	exit(-1);
    }
    
    parse_header(fd1,fd2);
    
    while(parse_block(fd1,fd2));
    fprintf(stderr,"\n Normal End\n");

    return(0);    
}


