/* gsch2pcb
|
|  Bill Wilson  billw@wt.net
|
|  This program is free software which I release under the GNU General Public
|  License. You may redistribute and/or modify this program under the terms
|  of that license as published by the Free Software Foundation; either
|  version 2 of the License, or (at your option) any later version.
|
|  This program is distributed in the hope that it will be useful,
|  but WITHOUT ANY WARRANTY; without even the implied warranty of
|  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
|  GNU General Public License for more details.  Version 2 is in the
|  COPYRIGHT file in the top level directory of this distribution.
| 
|  To get a copy of the GNU General Puplic License, write to the Free Software
|  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/

/*  Functions to enable gsch2pcb.c to compile under glib1.2
*/

#include <dirent.h>
#include <unistd.h>
#include <sys/stat.h>

typedef struct
	{
	DIR		*dir;
	}
	GDir;

GDir *
g_dir_open(gchar *path, guint flags, gpointer error)
	{
	GDir	*gdir;
	DIR		*dir;

	dir = opendir(path);
	if (!dir)
		return NULL;
	gdir = g_new0(GDir, 1);
	gdir->dir = dir;
	return gdir;
	}

gchar*
g_dir_read_name(GDir *dir)
	{
	struct dirent *entry;

	while ((entry = readdir(dir->dir)) != NULL)
		{
		if (   !strcmp(entry->d_name, ".")
			|| !strcmp(entry->d_name, "..")
		   )
			continue;
		return entry->d_name;
		}
	return NULL;
	}

void
g_dir_close(GDir *dir)
	{
	closedir(dir->dir);
	g_free(dir);
	}


#define G_FILE_TEST_EXISTS	1
#define	G_FILE_TEST_IS_DIR	2

gboolean
g_file_test(gchar *filename, gint test)
	{
	struct stat		s;

	if ((test & G_FILE_TEST_EXISTS) && (access(filename, F_OK) == 0))
		return TRUE;
	if (   (test & G_FILE_TEST_IS_DIR)
		&& stat(filename, &s) == 0
		&& S_ISDIR (s.st_mode)
	   )
		return TRUE;
	return FALSE;
	}

void
g_spawn_command_line_sync(gchar *command, ...)
	{
	system(command);			/* sort of a punt */
	}

gchar *
g_strrstr(gchar *s, gchar *needle)
	{
	return strstr(s, needle);	/* a punt */
	}
