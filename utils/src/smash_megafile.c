/* smash_megafile.c:
 *
 *     Break a Viewlogic metafile into a million little pieces
 *
 *
 * Copyright (C) 1998-2010 Mike Jarabek
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
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111 USA
 *
 *  $Id$
 *
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

#define RECLEN 0x14


int main(int argc, char **argv)
{

  FILE *megafile, *output;
  char *extracted_file;
  char name[127];
  char buffer[127];         /* buffer for megafile index entries */
  char output_name[127];

  int len;

  if( argc != 2 )
    {
      fprintf( stderr, "Usage:\n %s <megafile>\n\n"
	       "Where <megafile> is the name of a viewlogic megafile\n"
	       "whithout any extensions.  The file <megafile>.lib and \n"
	       "<megafile>.tbl must exist in the same directory\n",
	       argv[0]);
      return 1;
    }



  /* open the files */
  strcpy(name,argv[1]);
  strcat(name,".lib");
  megafile = fopen(name, "r");

  if( megafile == NULL )
    {
      fprintf(stderr, "Error: unable to open magefile `%s' for reading\n",
	      name);
      return(1);
    }

  /* create a subdir to hold the exploded files */
  mkdir(argv[1], 0777);    /* try to be friendly */
  

  /* read each table entry and extract the file from the megafile */
  while(!feof(megafile))
    {
      if(fread(buffer, RECLEN, 1, megafile) == 0) break;  /* end of file? */

      /* null terminate buffer */
      buffer[RECLEN+1] = 0;

      /*printf("%s\n",buffer);*/

      /* extract the name and size from the entry */
      sscanf(buffer,"%s %d",name,&len);

      printf("%s:%d\n",name,len);

      /* slurp in the required data and spit it out into the 
       * output directory
       */

      /* allocate some memory to hold the file */
      extracted_file = malloc(len);

      fread(extracted_file, len, 1, megafile);

      /* open up a file to dump in */
      strcpy(output_name, argv[1]);
      strcat(output_name, "/");
      strcat(output_name, name);
      output = fopen(output_name,"wb");
      if(output == NULL)
	{
	  fclose(megafile);
	  fprintf(stderr,"Error: unable to open file `%s' for writing\n",
		  output_name);
	  
	  return 1;
	}

      /* dump to the file */
      fwrite(extracted_file, len, 1, output);
      fclose(output);

      /* and get the ^Z */
      fgetc(megafile);

    }


  fclose(megafile);

  return 0;
}

