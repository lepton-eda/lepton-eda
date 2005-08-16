/*	This is grenum, an advanced refdes renumber utility for gEDA's gschem.
 *
 *	Copyright (C) 2005  Levente Kovacs
 *
 *	This program is free software; you can redistribute it and/or modify
 *	it under the terms of the GNU General Public License as published by
 *	the Free Software Foundation; either version 2 of the License, or
 *	(at your option) any later version.
 *
 *	This program is distributed in the hope that it will be useful,
 *	but WITHOUT ANY WARRANTY; without even the implied warranty of
 *	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *	GNU General Public License for more details.
 *
 *	You should have received a copy of the GNU General Public License
 *	along with this program; if not, write to the Free Software
 *	Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 * Levente.Kovacs@interware.hu
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#define _GNU_SOURCE
#include <getopt.h>
#include <unistd.h>

#include "grenum.h"
/*#define DEBUG*/

int main(int argc, char *argv[])
	{
	FILE *infile,*outfile;
	char buff[255], infilename[100], outfilename[100], *cp;
	unsigned char flags;
	int c,opt_idx,pages;
	static struct option opts[]={{"version",0,0,'v'},{"help",0,0,'h'},{"pagejump",0,0,'p'},{0,0,0,0}};
	struct refdes_ refdes, refdes_db[MAX_PREFIX_COUNT];

	flags=0x00; /*Clear all flags*/

/* flag bits
 *
 *	bit	function
 *-----------------------
 *	0		pkgskip
 *	1		not implemented
 *	2		not implemented
 *	3		not implemented
 *	4		not implemented
 *	5		not implemented
 *	6		not implemented
 *	7		not implemented
 *
 */
	while(1)
		{
		c=getopt_long(argc,argv,"vhp",opts,&opt_idx);
		if(c==-1)
			break;
		switch(c)
			{
			case 'h':
				printhelp();
				return 0;
			case 'v':
				printver();
				return 0;
			case 'p':
				flags|=0x01; /*Set the pagejump flag*/
				break;
			}
		}
	if(optind==argc)
		{
		printf("grenum: no input file\n");
		printhelp();
		return -1;
		}
	for(c=0;c<MAX_PREFIX_COUNT;c++)
		refdes_db[c].prefix[0]='\0';	/*Zero all the strings in the database.*/

	if((flags&0x01)==0x00)	/*No pagejumps*/
		{
		for(c=0;c<MAX_PREFIX_COUNT;c++)
			refdes_db[c].value=COUNT_START;	/*Fill the values with the COUNT_START value. We start from R1 for example.*/
		}

	for(pages=1;optind<argc;optind++,pages++)
		{
		if((flags&0x01)==0x01)	/*pagejumps*/
			{
			for(c=0;c<MAX_PREFIX_COUNT;c++)
				refdes_db[c].value=PAGE_JMP*pages+COUNT_START;	/*Reset the counters according to page numbers*/
			}
		strcpy(&infilename[0],argv[optind]);	/*Copy the filename to the buffer*/
		if((infile=fopen(&infilename[0], "r")) == NULL)	/*Open file, use r+ for read and write*/
			{
			printf("grenum: unable to open input file\n");
			return -2;
			}
		strcpy(&outfilename[0],&infilename[0]);
		if((outfile=fopen(strcat(&outfilename[0],".tmp"),"w"))==NULL)
			{
			printf("grenum: could not create tmp file\n");
			fclose(infile);	/*Close the file*/
			return -2;
			}
		printf("grenum: processing file %s\n",&infilename[0]);
		while(fgets(&buff[0],sizeof(buff),infile)!=NULL) /*Read one line.*/
			{	/*Process starts here*/
#ifdef DEBUG
			printf("%s",&buff[0]);
#endif
			cp=strstr(&buff[0],"refdes=");	/*seek for a refdes designator*/
			if(cp==NULL)
				{
				if(fputs(&buff[0],outfile)==-1)
					{
					printf("grenum: could not write to tmp file\n");
					fclose(infile);	/*Close the files*/
					fclose(outfile);
					return -2;
					}
				continue;
				}
			switch(parse_refdes(&refdes,&buff[0]))/*We have the current refdes in the refdes structure parsed*/
				{
				case 0:	/*e.g. U1*/
				/*We shall compare the maximum value, shall search for gaps, and set the refes_db.value to the next  free value*/
				for(c=0;c<MAX_PREFIX_COUNT;c++)	/*seek for prefix*/
					{
					if(strcmp(&refdes.prefix[0],&refdes_db[c].prefix[0])==0)	/*Now compare. According to the specs, we *DO NOT* renumber if there are two same refdeses, since it's a multi slotted component. This must be set manually.*/
						{
						if(refdes.value-refdes_db[c].value==1)
							refdes_db[c].value=refdes.value;	/*The next component; update the database*/
						break;	/*continue our job*/
						}
					else if(refdes_db[c].prefix[0]=='\0')
						{	/*No more valid prefix found in the database. New prefix. Update the database*/
						strcpy(&refdes_db[c].prefix[0],&refdes.prefix[0]);
						refdes_db[c].value=refdes.value;
						break;	/*continue our job*/
						}
					}
				if(c==MAX_PREFIX_COUNT)
					{
					printf("grenum: out of memory. Too much refdes prefixes.\n");
					fclose(infile);	/*Close the files*/
					fclose(outfile);
					return -4;
					}
				break;
				case 1:	/*e.g. U?*/
					for(c=0;c<MAX_PREFIX_COUNT;c++)	/*Look up the next value*/
						{
						if(strcmp(&refdes_db[c].prefix[0],&refdes.prefix[0])==0)
							{
							refdes.value=++refdes_db[c].value;	/*Renumber... Finally :-)*/
							break;
							}
						else if(refdes_db[c].prefix[0]=='\0')
							{	/*New prefix*/
							strcpy(&refdes_db[c].prefix[0],&refdes.prefix[0]);	/*Register the prefix to the database*/
							refdes.value=++refdes_db[c].value;	/*Renumber... Finally :-)*/
							break;
							}
						}
					if(c==MAX_PREFIX_COUNT)
						{
						printf("grenum: out of memory. Too much refdes prefixes.\n");
						fclose(infile);	/*Close the files*/
						fclose(outfile);
						return -4;
						}
				sprintf(&buff[0],"refdes=%s%d\n",&refdes.prefix[0],refdes.value);	/*Format the new refdes string*/
				break;
				case -1:	/*e.g. awdf#$%WSf82f8 :-) No "=" signal in the refdes string.*/
					printf("grenum: parse error\n");
					fclose(infile);	/*Close the files*/
					fclose(outfile);
					return -3;
				}
			if(fputs(&buff[0],outfile)==-1)	/*Finally, write the refdes line to the output file*/
				{
				printf("grenum: could not write to tmp file\n");
				fclose(infile);	/*Close the files*/
				fclose(outfile);
				return -2;
				}
			}	/*Process ends here*/
/*		fclose(infile);*/	/*Close the files*/
		fclose(outfile);
		strcpy(&buff[0],&infilename[0]); /*buff has the original infilename*/
/*The next few lines implements the copy program*/
		fseek(infile,0L,SEEK_SET); /*Go to the begining of the infile*/
		outfile=fopen(strcat(&buff[0],".save"),"w");
		if(outfile==NULL)
			{
			printf("grenum: ould not create backup file\n");
			fclose(infile);	/*Close the file*/
			return -2;
			}
		while(fgets(&buff[0],sizeof(buff),infile)!=NULL) /*Read one line.*/
			{
			if(fputs(&buff[0],outfile)==-1)
				{
				printf("grenum: could not write to backup file\n");
				fclose(infile);	/*Close the files*/
				fclose(outfile);
				return -2;
				}
			}
		fclose(infile);
		fclose(outfile);
		rename(&outfilename[0],&infilename[0]);	/*Move the tmpfile to the original*/
		}
	printf("grenum: file(s) successfully processed\n");
	return 0; /*Everything is okay*/
	}

int parse_refdes(struct refdes_ *refdes, char *ref_str)
	{
	int i;
	char buff[256],*cpr,*cp;

/*
 *This function parses the refdes line from the .sch file. It takes a pointer to the
 *complete refdes definition string, and a pointer which points to a refdes structure
 *where it'll store the info.
 *
 *parse_refdes() will return
 *
 *0 if there was a prefix with renumbered value (for example R1,IC3,U5);
 *1 if there was a "?" mark found, and it has to be renumbered (e.g. U?);
 *-1, if there was some uncool thing.
 *
 *The refdes structure is filled with the prefix and the value.
 *
 *Note that if a "?" is found, the value member remains untouched.
 */


	cpr=strstr(ref_str,"=");	/*seek for the "="*/
	if(cpr==NULL)	/*This should not happen*/
		return -1;
	cp=strstr(ref_str,"?");
/*refdes=U1		refdes=IC?
 *      |		         |
 *    *cpr		        cp            
 */
	if(cp!=NULL)
		{	/*Not renumbered yet*/
		strncpy(&refdes->prefix[0],cpr+1,cp-cpr-1);	/*Copy the prefix to the refdes structure*/
		refdes->prefix[cp-cpr-1]='\0';

#ifdef DEBUG
		printf("Prefix=%s\n",&refdes->prefix[0]);
#endif

		return 1;
		}
	for(cp=cpr+1,i=0;(*cp != '\n' && *cp>='A' && *cp<='z');i++,cp++) /*No "?". Copy the prefix*/
		buff[i]=*cp;	/*Fill the buffer from char to char*/
	buff[i]='\0';	/*Terminate with NULL to be a string*/
#ifdef DEBUG
	printf("Prefix=%s\n",&buff[0]);
#endif
	strcpy(&refdes->prefix[0],&buff[0]);	/*Copy to refdes structure*/
	for(i=0,cp;(*cp != '\n' && *cp>='0' && *cp<='9');cp++,i++)
		buff[i]=*cp;	/*Fill the buffer from char to char*/
	buff[i]='\0';	/*Terminate with NULL to be a string*/
#ifdef DEBUG
	printf("Value=%s\n",&buff[0]);
#endif
	refdes->value=abs(atoi(&buff[0]));
	return 0;
	}

void printhelp()
	{
	printver();
	printf("Usage: grenum [-v|--version -h|--help -p|--pagejump] file1.sch file2.sch ...\n\n");
	printf("\t-v|--version\tprints version info\n\t-h|--help\tprints this help\n\t-p|--pagejump\tsets pagejump mode on\n");
	printf("For more information read the README file and/or the manual.\n");
	}


void printver()
	{
	printf("This is grenum, an advanced refdes renumber utility for gEDA's gschem.\n");
	printf("Version %s\n",VERSION);
	printf("Compiled on %s at %s\n",COMP_DATE,COMP_TIME);
	}
