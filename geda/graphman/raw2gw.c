/*******************************************************************************/
/*                                                                             */
/* gEDA Suite Project Manager                                                  */
/*                                                                             */
/* Copyright (C) 2002 Piotr Miarecki, sp9rve@eter.ariadna.pl                   */
/*                                                                             */
/* This program is free software; you can redistribute it and/or               */
/* modify it under the terms of the GNU General Public License                 */
/* as published by the Free Software Foundation version 2.                     */
/*                                                                             */
/* This program is distributed in the hope that it will be useful,             */
/* but WITHOUT ANY WARRANTY; without even the implied warranty of              */
/* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the               */
/* GNU General Public License for more details.                                */
/*                                                                             */
/* You should have received a copy of the GNU General Public License           */
/* along with this program; if not, write to the Free Software                 */
/* Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA. */
/*                                                                             */
/*******************************************************************************/

/* 
	REMEBER !!! This is an obsolete tool, distributed to allow using gmanager.
	It will be replaced by graphman - a graphical tool used to manage graphs. 
	
	raw2gw will be removed from distribution in near future !!!
*/

#include <stdio.h>
#include <sys/param.h>
#include <unistd.h>



int main(int iArgn, const char *szArgv[])
{
	FILE *hFile;
	char szDir[MAXPATHLEN];
	
	if (iArgn < 2)
		return 0;

	/* check if the destination file exists, if yes -> cancel processing */
	hFile = fopen(szArgv[2], "r");
	if (hFile != NULL)
	{
		fclose(hFile);
		return -1;
	}
	
	getcwd(szDir, MAXPATHLEN);
	
	hFile = fopen(szArgv[2], "w");
	if (hFile == NULL)
		return 0;
	
	fprintf(hFile, "#!gwave -s\n");
	fprintf(hFile, "!#\n");
	fprintf(hFile, "; gwave script\n");
	fprintf(hFile, "(require-n-wavepanels 2)\n");
	fprintf(hFile, "(set! default-measure1-function 5)\n");
	fprintf(hFile, "(let ((df (if script-target-datafile\n");
	fprintf(hFile, "           script-target-datafile\n");
	fprintf(hFile, "           (find-or-load-wavefile \"%s/%s\"))))\n", szDir, szArgv[1]);
	fprintf(hFile, ")\n");
	fprintf(hFile, "(x-zoom! 0.0 0.0)\n");
	fprintf(hFile, "(wtable-set-xlogscale! #f)\n");
	fprintf(hFile, "(set! default-wavepanel-type 0)\n");
	fprintf(hFile, "(gtk-tooltips-enable gwave-tooltips)\n");
	fprintf(hFile, "(let ((wp (nth-wavepanel 0)))\n");
	fprintf(hFile, " (set-wavepanel-ylogscale! wp #f)\n");
	fprintf(hFile, " (set-wavepanel-type! wp 0)\n");
	fprintf(hFile, ")\n");
	fprintf(hFile, "(let ((wp (nth-wavepanel 1)))\n");
	fprintf(hFile, " (set-wavepanel-ylogscale! wp #f)\n");
	fprintf(hFile, " (set-wavepanel-type! wp 0)\n");
	fprintf(hFile, ")\n");
	
	fclose(hFile);

	return 0;
}
