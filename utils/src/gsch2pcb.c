/* gsch2pcb
|
|  Bill Wilson    billw@wt.net
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


#include <glib.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <unistd.h>
#include <sys/stat.h>

#if !GLIB_CHECK_VERSION(2,0,0)
#include "glib12-compat.c"
#endif


#define	GSC2PCB_VERSION		"1.4"

#define	DEFAULT_PCB_INC		"pcb.inc"

typedef struct
	{
	gchar		*refdes,
				*value,
				*description,
				*changed_description,
				*changed_value;
	gchar		*flags,
				*tail;
	gint		x,
				y;
	gchar		*pkg_name_fix;
	gchar		res_char;

	gboolean	still_exists,
				new_format,
				hi_res_format,
				omit_PKG;
	}
	PcbElement;


typedef struct
	{
	gchar		*part_number,
				*element_name;
	}
	ElementMap;

static GList	*pcb_element_list,
				*element_directory_list,
				*extra_gnetlist_list;

static gchar	*schematics,
				*basename;

static gchar	*m4_command,
				*m4_pcbdir,
				*default_m4_pcbdir,
				*m4_files,
				*m4_override_file;

static gchar	*empty_footprint_name;

static gint		verbose,
				n_deleted,
				n_added_m4,
				n_added_ef,
				n_fixed,
				n_PKG_removed_new,
				n_PKG_removed_old,
				n_preserved,
				n_changed_value,
				n_not_found,
				n_unknown,
				n_none,
				n_empty;

static gboolean	  remove_unfound_elements = TRUE,
                                quiet_mode = FALSE,
				force_element_files,
				preserve,
				fix_elements,
				bak_done,
				need_PKG_purge;


static void
create_m4_override_file()
	{
	FILE	*f;

	if (!m4_command && !m4_pcbdir && !m4_files)
		return;
	m4_override_file = "gnet-gsch2pcb-tmp.scm";
	f = fopen(m4_override_file, "w");
	if (!f)
		{
		m4_override_file = NULL;
		return;
		}
	if (m4_command)
		fprintf(f, "(define m4-command \"%s\")\n", m4_command);
	if (m4_pcbdir)
		fprintf(f, "(define m4-pcbdir \"%s\")\n", m4_pcbdir);
	if (m4_files)
		fprintf(f, "(define m4-files \"%s\")\n", m4_files);
	fclose(f);
	if (verbose)
		{
		printf("Default m4-pcbdir: %s\n", default_m4_pcbdir);
		printf("--------\ngnet-gsch2pcb-tmp.scm override file:\n");
		if (m4_command)
			printf("    (define m4-command \"%s\")\n", m4_command);
		if (m4_pcbdir)
			printf("    (define m4-pcbdir \"%s\")\n", m4_pcbdir);
		if (m4_files)
			printf("    (define m4-files \"%s\")\n", m4_files);
		}
	}

  /* Run gnetlist to generate a netlist and a PCB board file.  gnetlist
  |  has exit status of 0 even if it's given an invalid arg, so do some
  |  stat() hoops to decide if gnetlist successfully generated the PCB
  |  board file (only gnetlist >= 20030901 recognizes -m).
  */
static void
run_gnetlist(gchar *net_file, gchar *pcb_file, gchar *basename, gchar *args)
	{
	gchar		*command,
				*out_file,
				*args1,
				*s;
	GList		*list;
	struct stat	st;
	time_t		mtime;

	if (verbose)
		{
		command = g_strconcat(
					"gnetlist -g PCB -o ", net_file, " ", args, NULL);
		printf("Running command:\n\t%s\n", command);
		printf("--------\n");
		}
	else
		command = g_strconcat(
					"gnetlist -q -g PCB -o ", net_file, " ", args, NULL);
	g_spawn_command_line_sync(command, NULL, NULL, NULL, NULL);
	g_free(command);

	create_m4_override_file();
	if (m4_override_file)
		args1 = g_strconcat("-m ", m4_override_file, " ", args, NULL);
	else
		args1 = g_strdup(args);

	mtime = (stat(pcb_file, &st) == 0) ? st.st_mtime : 0;
	
	if (verbose)
		{
		printf("--------\n");
		command = g_strconcat("gnetlist -g gsch2pcb -o ", pcb_file, 
				" ", args1, NULL);
		printf("Running command:\n\t%s\n", command);
		printf("--------\n");
		}
	else
		command = g_strconcat("gnetlist -q -g gsch2pcb -o ", pcb_file, 
				" ", args1, NULL);

	g_spawn_command_line_sync(command, NULL, NULL, NULL, NULL);

	if (verbose)
		printf("--------\n");

	if (   stat(pcb_file, &st) != 0
		|| mtime == st.st_mtime
	   )
		{
		fprintf(stderr, "gsch2pcb: gnetlist command failed.\n");
		if (m4_override_file)
			fprintf(stderr,
		"    At least gnetlist 20030901 is required for m4-xxx options.\n");
		exit(1);
		}
	g_free(command);
	g_free(args1);
	if (m4_override_file)
		unlink(m4_override_file);

	for (list = extra_gnetlist_list; list; list = list->next)
		{
		s = (gchar *) list->data;
		if (!strstr(s, " -o "))
			out_file = g_strconcat(" -o ", basename, ".", s, NULL);
		else
			out_file = g_strdup(" ");

		if (verbose)
			{
			printf("--------\n");
			command = g_strconcat("gnetlist -g ", s, out_file,
							" ", args, NULL);
			printf("Running command:\n\t%s\n", command);
			printf("--------\n");
			}
		else
			command = g_strconcat("gnetlist -q -g ", s, out_file,
							" ", args, NULL);

		g_spawn_command_line_sync(command, NULL, NULL, NULL, NULL);
		g_free(command);
		g_free(out_file);
		if (verbose)
			printf("--------\n");
		}
	}

static gchar *
token(gchar *string, gchar **next)
	{
	static gchar	*str;
	gchar			*s, *ret;
	gboolean		quoted = FALSE;

	if (string)
		str = string;
	if (!str || !*str)
		{
		if (next)
			*next = str;
		return g_strdup("");
		}
	while (*str == ' ' || *str == '\t' || *str == ',' || *str == '\n')
		++str;
	if (*str == '"')
		{
		quoted = TRUE;
		++str;
		for (s = str; *s && *s != '"' && *s != '\n'; ++s)
			;
		}
	else
		for (s = str;
				*s && (*s != ' ' && *s != '\t' && *s != ',' && *s != '\n');
				++s)
			;
	ret = g_strndup(str, s - str);
	str = (quoted && *s) ? s + 1 : s;
	if (next)
		*next = str;
	return ret;
	}

static gchar *
fix_spaces(gchar *str)
	{
	gchar	*s;

	if (!str)
		return NULL;
	for (s = str; *s; ++s)
		if (*s == ' ' || *s == '\t')
			*s = '_';
	return str;
	}

  /* As of 1/9/2004 CVS hi_res Element[] line format:
  |  Element[element_flags, description, pcb-name, value, mark_x, mark_y,
  |			text_x, text_y, text_direction, text_scale, text_flags]
  |  New PCB 1.7 / 1.99 Element() line format:
  |  Element(element_flags, description, pcb-name, value, mark_x, mark_y,
  |			text_x, text_y, text_direction, text_scale, text_flags)
  |  Old PCB 1.6 Element() line format:
  |  Element(element_flags, description, pcb-name, value, 
  |			text_x, text_y, text_direction, text_scale, text_flags)
  |
  |  (mark_x, mark_y) is the element position (mark) and (text_x,text_y)
  |  is the description text position which is absolute in pre 1.7 and
  |  is now relative.  The hi_res mark_x,mark_y and text_x,text_y resolutions
  |  are 100x the other formats.
  */
PcbElement *
pcb_element_line_parse(gchar *line)
	{
	PcbElement	*el = NULL;
	gchar		*s, *t, close_char;
	gint		state = 0, elcount = 0;

	if (strncmp(line, "Element", 7))
		return NULL;

	el = g_new0(PcbElement, 1);

	if (*(line + 7) == '[')
		el->hi_res_format = TRUE;
	else if (*(line + 7) != '(')
		{
		g_free(el);
		return NULL;
		}

	el->res_char = el->hi_res_format ? '[' : '(';
	close_char = el->hi_res_format ? ']' : ')';

	el->flags = token(line + 8, NULL);
	el->description = token(NULL, NULL);
	el->refdes = token(NULL, NULL);
	el->value  = token(NULL, NULL);

	s = token(NULL, NULL);
	el->x = atoi(s);
	g_free(s);

	s = token(NULL, &t);
	el->y = atoi(s);
	g_free(s);

	el->tail = g_strdup(t ? t : "");
	if ((s = strrchr(el->tail, (gint) '\n')) != NULL)
		*s = '\0';

	/* Count the tokens in tail to decide if it's new or old format.
	|  Old format will have 3 tokens, new format will have 5 tokens.
	*/
	for (s = el->tail; *s && *s != close_char; ++s)
		{
		if (*s != ' ')
			{
			if (state == 0)
				++elcount;
			state = 1;
			}
		else
			state = 0;
		}
	if (elcount > 4)
		el->new_format = TRUE;

	fix_spaces(el->description);
	fix_spaces(el->refdes);
	fix_spaces(el->value);

	/* Don't allow elements with no refdes to ever be deleted
	|  because they may be desired pc board elements not in schematics.
	|  So initialize still_exists to TRUE if empty or non-alphanumeric refdes.
	*/
	if (!*el->refdes || !isalnum((gint) (*el->refdes)))
		el->still_exists = TRUE;

	return el;
	}

static void
pcb_element_free(PcbElement *el)
	{
	if (!el)
		return;
	g_free(el->flags);
	g_free(el->description);
	g_free(el->changed_description);
	g_free(el->changed_value);
	g_free(el->refdes);
	g_free(el->value);
	g_free(el->tail);
	g_free(el->pkg_name_fix);
	g_free(el);
	}

static void
get_pcb_element_list(gchar *pcb_file)
	{
	FILE		*f;
	PcbElement	*el;
	gchar		*s, buf[1024];

	if ((f = fopen(pcb_file, "r")) == NULL)
		return;
	while ((fgets(buf, sizeof(buf), f)) != NULL)
		{
		for (s = buf; *s == ' ' || *s == '\t'; ++s)
			;
		if (!strncmp(s, "PKG_", 4))
			{
			need_PKG_purge = TRUE;
			continue;
			}
		if ((el = pcb_element_line_parse(s)) == NULL)
			continue;
		pcb_element_list = g_list_append(pcb_element_list, el);
		}
	fclose(f);
	}

static PcbElement *
pcb_element_exists(PcbElement *el_test, gboolean record)
	{
	GList	*list;
	PcbElement	*el;

	for (list = pcb_element_list; list; list = list->next)
		{
		el = (PcbElement *) list->data;

		if (strcmp(el_test->refdes, el->refdes))
			continue;
		if (strcmp(el_test->description, el->description))	/* footprint */
			{
			if (record)
				el->changed_description = g_strdup(el_test->description);
			}
		else
			{
			if (record)
				{
				if (strcmp(el_test->value, el->value))
					el->changed_value = g_strdup(el_test->value);
				el->still_exists = TRUE;
				}
			return el;
			}
		}
	return NULL;
	}

  /* A problem is that new PCB 1.7 file elements have the (mark_x,mark_y)
  |  value set to wherever the element was created and no equivalent of a
  |  gschem translate symbol was done.
  |  So, file elements inserted can be scattered over a big area and this is
  |  bad when loading a file.new.pcb into an existing PC board.  So, do a
  |  simple translate if (mark_x,mark_y) is (arbitrarily) over 1000.  I'll
  |  assume that for values < 1000 the element creator was concerned with a
  |  sane initial element placement.  Unless someone has a better idea?
  |  Don't bother with pre PCB 1.7 formats as that would require parsing
  |  the mark().  Current m4 elements use the old format but they seem to
  |  have a reasonable initial mark().
  */
static void
simple_translate(PcbElement *el)
	{
	gint	factor;

	if (el->new_format)
		{
		factor = el->hi_res_format ? 100 : 1;
		if (el->x > 1000 * factor)
			el->x = 500 * factor;
		if (el->y > 1000 * factor)
			el->y = 500 * factor;
		}
	}

static gboolean
insert_element(FILE *f_out, gchar *element_file,
			gchar *footprint, gchar *refdes, gchar *value)
	{
	FILE		*f_in;
	PcbElement	*el;
	gchar		*s, buf[1024];
	gboolean	retval = FALSE;

	if ((f_in = fopen(element_file, "r")) == NULL)
		{
		s = g_strdup_printf("insert_element() can't open %s", element_file);
		perror(s);
		g_free(s);
		return FALSE;
		}
	/* Copy the file element lines.  Substitute new parameters into the
	|  Element() or Element[] line and strip comments.
	*/
	while ((fgets(buf, sizeof(buf), f_in)) != NULL)
		{
		for (s = buf; *s == ' ' || *s == '\t'; ++s)
			;
		if ((el = pcb_element_line_parse(s)) != NULL)
			{
			simple_translate(el);
			fprintf(f_out, "Element%c%s \"%s\" \"%s\" \"%s\" %d %d%s\n",
						el->res_char, el->flags, footprint, refdes, value,
						el->x, el->y, el->tail);
			retval = TRUE;;
			}
		else if (*s != '#')
			fputs(buf, f_out);
		pcb_element_free(el);
		}
	fclose(f_in);
	return retval;
	}


gchar	*
find_element(gchar *dir_path, gchar *element)
	{
	GDir	*dir;
	gchar	*path, *name, *s, *found = NULL;

	if ((dir = g_dir_open(dir_path, 0, NULL)) == NULL)
		{
		s = g_strdup_printf("find_element can't open dir %s", dir_path);
		perror(s);
		g_free(s);
		return NULL;
		}
	if (verbose > 1)
			printf("\t  Searching: %s\n", dir_path);
	while ((name = (gchar *) g_dir_read_name(dir)) != NULL)
		{
		path = g_strconcat(dir_path, "/", name, NULL);
		found = NULL;
		if (g_file_test(path, G_FILE_TEST_IS_DIR))
			found = find_element(path, element);
		else
			{
			if (verbose > 1)
				printf("\t           : %s\t", name);
			if (!strcmp(name, element))
				found = g_strdup(path);
			if (verbose > 1)
				printf("%s\n", found ? "Yes" : "No");
			}
		g_free(path);
		if (found)
			break;
		}
	g_dir_close(dir);
	return found;
	}

gchar *
search_element_directories( PcbElement	*el)
	{
	GList		*list;
	gchar		*s, *elname = NULL, *dir_path, *path = NULL;
	gint		n1, n2;

	/* See comment before pkg_to_element() */
	if (el->pkg_name_fix)
		{
		if (strchr(el->description, '-'))
			{
			n1 = strlen(el->description);
			n2 = strlen(el->pkg_name_fix);
			s = el->description + n1 - n2 - 1;

// printf("n1=%d n2=%d desc:%s fix:%s s:%s\n",
//	n1, n2, el->description, el->pkg_name_fix, s);

			if (   n1 > 0  && n2 < n1
				&& *s == '-'
				&& *(s + 1) == *el->pkg_name_fix
			   )
				{
				s = g_strndup(el->description, n1 - n2 - 1);
				elname = g_strconcat(s, " ", el->pkg_name_fix, NULL);
				g_free(s);
				}
			}
		if (!elname)
			{
			printf("Warning: argument passing may have been confused by\n");
			printf("         a comma in a component value:\n");
			printf("         Check %s %s %s\n",
					el->refdes, el->description, el->value);
			printf("         Maybe just use a space instead of a comma?\n");
			}
		}
	if (!elname)
		elname = g_strdup(el->description);

	if (!strcmp(elname, "unknown"))
		{
		g_free(elname);
		return NULL;
		}
	if (verbose > 1)
		printf("\tSearching directories looking for file element: %s\n",
					elname);
	for (list = element_directory_list; list; list = list->next)
		{
		dir_path = (gchar *) list->data;
		path = find_element(dir_path, elname);
		if (path)
			{
			if (verbose)
				printf("\tFound: %s\n", path);
			break;
			}
		}
	g_free(elname);
	return path;
	}

/* The gnetlist backend gnet-gsch2pcb.scm generates PKG_ lines:
|
|        PKG_footprint(footprint{-fp0-fp1},refdes,value{,fp0,fp1})
|
|  where fp1 and fp2 (if they exist) are the extra footprint components when
|  specifying footprints like "DIL 14 300".  This is needed for m4 macros.
|  A complication is if the footprint references a file element with spaces
|  embedded in the name.  The gnetlist backend will interpret these as
|  fp0, fp1, ... args and the footprint will in this case incorrectly have
|  '-' inserted where the spaces should be.  So, if there are additional
|  args, reconstruct the portion of the name given by the args with spaces
|  for later use.  Eg. if the footprint is "100 Pin jack", we will have
|      PKG_100-Pin-jack(100-Pin-jack,refdes,value,Pin,jack)
|  So put "Pin jack" into pkg_name_fix so if this element is searched
|  as a file element we can munge the description to what it should be, eg:
|      100-Pin-jack -> 100 Pin jack
*/
static PcbElement *
pkg_to_element(FILE *f, gchar *pkg_line)
	{
	PcbElement	*el;
	gchar		**args, *s;
	gint		n, n_extra_args, n_dashes;

	if (   strncmp(pkg_line, "PKG_", 4)
		|| (s = strchr(pkg_line, (gint) '(')) == NULL
	   )
		return NULL;

	args = g_strsplit(s + 1, ",", 12);
	if (!args[0] || !args[1] || !args[2])
		{
		fprintf(stderr, "Bad package line: %s\n", pkg_line);
		return NULL;
		}
	fix_spaces(args[0]);
	fix_spaces(args[1]);
	fix_spaces(args[2]);

	el = g_new0(PcbElement, 1);
	el->description = g_strdup(args[0]);
	el->refdes      = g_strdup(args[1]);
	el->value       = g_strdup(args[2]);
	if ((s = strchr(el->value, (gint) ')')) != NULL)
		*s = '\0';

	/* If the component value has a comma, eg "1k, 1%", the gnetlist generated
	|  PKG line will be PKG_XXX(`R0w8',`R100',`1k, 1%'), but after processed
	|  by m4, the input to gsch2pcb will be PKG_XXX(R0w8,R100,1k, 1%).  So the
	|  quoting info has been lost when processing for file elements.  So here
	|  try to detect and fix this.  But I can't handle the situation where
	|  the description has a '-' and the value has a comma because
	|  gnet-gsch2pcb.scm munges the description with '-' when there are
	|  extra args.
	*/
	for (n_extra_args = 0; args[3 + n_extra_args] != NULL; ++n_extra_args)
		;
	s = el->description;
	for (n_dashes = 0; (s = strchr(s + 1, '-')) != NULL; ++n_dashes)
		;

	n = 3;
	if (n_extra_args == n_dashes + 1)
		{	/* Assume there was a comma in the value, eg "1K, 1%" */
		s = el->value;
		el->value = g_strconcat(s, ",", fix_spaces(args[n]), NULL);
		g_free(s);
		if ((s = strchr(el->value, (gint) ')')) != NULL)
			*s = '\0';
		n = 4;
		}
	if (args[n])
		{
		el->pkg_name_fix = g_strdup(args[n]);
		for (n += 1; args[n] != NULL; ++n)
			{
			s = el->pkg_name_fix;
			el->pkg_name_fix = g_strconcat(s, " ", args[n], NULL);
			g_free(s);
			}
		if ((s = strchr(el->pkg_name_fix, (gint) ')')) != NULL)
			*s = '\0';
		}
	g_strfreev(args);

	if (empty_footprint_name && !strcmp(el->description, empty_footprint_name))
		{
		if (verbose)
			printf(
"%s: has the empty footprint attribute \"%s\" so won't be in the layout.\n",
				el->refdes, el->description);
		n_empty += 1;
		el->omit_PKG = TRUE;
		}
	else if (!strcmp(el->description, "none"))
		{
		fprintf(stderr,
"WARNING: %s has a footprint attribute \"%s\" so won't be in the layout.\n",
				el->refdes, el->description);
		n_none += 1;
		el->omit_PKG = TRUE;
		}
	else if (!strcmp(el->description, "unknown"))
		{
		fprintf(stderr,
		"WARNING: %s has no footprint attribute so won't be in the layout.\n",
				el->refdes);
		n_unknown += 1;
		el->omit_PKG = TRUE;
		}
	return el;
	}

/* Process the newly created pcb file which is the output from
|     gnetlist -g gsch2pcb ...
|  It will have elements found via the m4 interface and PKG_ lines for
|  elements not found.
|  Insert pcb file elements for PKG_ lines if file elements can be found.
|  If there was an existing pcb file, strip out any elements if they are
|  already present so that the new pcb file will only have new elements.
*/
static gint
add_elements(gchar *pcb_file)
	{
	FILE		*f_in, *f_out;
	PcbElement	*el = NULL;
	gchar		*command, *p, *tmp_file, *s, buf[1024];
	gint		total, paren_level = 0;
	gboolean	is_m4, skipping = FALSE;

	if ((f_in = fopen(pcb_file, "r")) == NULL)
		return 0;
	tmp_file = g_strconcat(pcb_file, ".tmp", NULL);
	if ((f_out = fopen(tmp_file, "w")) == NULL)
		{
		fclose(f_in);
		g_free(tmp_file);
		return 0;
		}
	while ((fgets(buf, sizeof(buf), f_in)) != NULL)
		{
		for (s = buf; *s == ' ' || *s == '\t'; ++s)
			;
		if (skipping)
			{
			if (*s == '(')
				++paren_level;
			else if (*s == ')' && --paren_level <= 0)
				skipping = FALSE;
			continue;
			}
		is_m4 = FALSE;
		if ((el = pcb_element_line_parse(s)) != NULL)
			is_m4 = TRUE;
		else
			el = pkg_to_element(f_out, s);
		if (el && pcb_element_exists(el, TRUE))
			{
			skipping = is_m4;
			pcb_element_free(el);
			continue;
			}
		if (!el || el->omit_PKG)
			{
			if (el)
				{
				
				}
			else
				fputs(buf, f_out);
			continue;
			}
		if (!is_m4 || (is_m4 && force_element_files))
			{
			if (verbose && !is_m4)
				printf(
					"%s: need new file element for footprint  %s (value=%s)\n",
					el->refdes, el->description, el->value);
			if (verbose && is_m4 && force_element_files)
				printf(
		"%s: have m4 element %s, but trying to replace with a file element.\n",
					el->refdes, el->description);
			p = search_element_directories(el);
			if (!p && verbose && is_m4 && force_element_files)
				printf("\tNo file element found.\n");

			if (p && insert_element(f_out, p,
						el->description, el->refdes, el->value))
				{
				skipping = is_m4;
				is_m4 = FALSE;
				++n_added_ef;
				if (verbose)
					printf(
					"%s: added new file element for footprint %s (value=%s)\n",
					el->refdes, el->description, el->value);
				}
			else if (!is_m4)
				{
				fprintf(stderr,
					"%s: can't find PCB element for footprint %s (value=%s)\n",
					el->refdes, el->description, el->value);
				if (remove_unfound_elements && !fix_elements)
					{
					fprintf(stderr,
						"So device %s will not be in the layout.\n",
						el->refdes);
					++n_PKG_removed_new;
					}
				else
					{
					++n_not_found;
					fputs(buf, f_out);	/* Copy PKG_ line */
					}
				}
			g_free(p);
			}
		if (is_m4)
			{
			fputs(buf, f_out);
			++n_added_m4;
			if (verbose)
				printf(
					"%s: added new m4 element for footprint   %s (value=%s)\n",
					el->refdes, el->description, el->value);
			}
		pcb_element_free(el);
		if (verbose)
			printf("----\n");
		}
	fclose(f_in);
	fclose(f_out);

	total = n_added_ef + n_added_m4 + n_not_found;
	if (total == 0)
		command = g_strconcat("rm ", tmp_file, NULL);
	else
		command = g_strconcat("mv ", tmp_file, " ", pcb_file, NULL);
	g_spawn_command_line_sync(command, NULL, NULL, NULL, NULL);
	g_free(command);
	g_free(tmp_file);
	return total;
	}

static void
update_element_descriptions(gchar *pcb_file, gchar *bak)
	{
	FILE		*f_in, *f_out;
	GList		*list;
	PcbElement	*el, *el_exists;
	gchar		*command, *tmp, *s, buf[1024];

	for (list = pcb_element_list; list; list = list->next)
		{
		el = (PcbElement *) list->data;
		if (el->changed_description)
			++n_fixed;
		}
	if (!pcb_element_list || n_fixed == 0)
		{
		fprintf(stderr, "Could not find any elements to fix.\n");
		return;
		}
	if ((f_in = fopen(pcb_file, "r")) == NULL)
		return;
	tmp = g_strconcat(pcb_file, ".tmp", NULL);
	if ((f_out = fopen(tmp, "w")) == NULL)
		{
		fclose(f_in);
		return;
		}
	while ((fgets(buf, sizeof(buf), f_in)) != NULL)
		{
		for (s = buf; *s == ' ' || *s == '\t'; ++s)
			;
		if (   (el = pcb_element_line_parse(s)) != NULL
			&& (el_exists = pcb_element_exists(el, FALSE)) != NULL
			&& el_exists->changed_description
		   )
			{
			fprintf(f_out, "Element%c%s \"%s\" \"%s\" \"%s\" %d %d%s\n",
						el->res_char,
						el->flags, el_exists->changed_description,
						el->refdes, el->value, el->x, el->y, el->tail);
			printf("%s: updating element Description: %s -> %s\n",
						el->refdes, el->description,
						el_exists->changed_description);
			el_exists->still_exists = TRUE;
			}
		else
			fputs(buf, f_out);
		pcb_element_free(el);
		}
	fclose(f_in);
	fclose(f_out);

	if (!bak_done)
		{
		command = g_strconcat("mv ", pcb_file, " ", bak, NULL);
		g_spawn_command_line_sync(command, NULL, NULL, NULL, NULL);
		g_free(command);
		bak_done = TRUE;
		}

	command = g_strconcat("mv ", tmp, " ", pcb_file, NULL);
	g_spawn_command_line_sync(command, NULL, NULL, NULL, NULL);
	g_free(command);
	g_free(tmp);
	}

static void
prune_elements(gchar *pcb_file, gchar *bak)
	{
	FILE		*f_in, *f_out;
	GList		*list;
	PcbElement	*el, *el_exists;
	gchar		*command, *tmp, *s, buf[1024];
	gint		paren_level = 0;
	gboolean	skipping = FALSE;

	for (list = pcb_element_list; list; list = list->next)
		{
		el = (PcbElement *) list->data;
		if (!el->still_exists)
			{
			if (preserve)
				{
				++n_preserved;
				fprintf(stderr,
		"Preserving PCB element not in the schematic:    %s (element   %s)\n",
					el->refdes, el->description);
				}
			else
				++n_deleted;
			}
		else if (el->changed_value)
			++n_changed_value;
		}
	if (   !pcb_element_list
		|| (n_deleted == 0 && !need_PKG_purge && n_changed_value == 0)
	   )
		return;
	if ((f_in = fopen(pcb_file, "r")) == NULL)
		return;
	tmp = g_strconcat(pcb_file, ".tmp", NULL);
	if ((f_out = fopen(tmp, "w")) == NULL)
		{
		fclose(f_in);
		return;
		}
	while ((fgets(buf, sizeof(buf), f_in)) != NULL)
		{
		for (s = buf; *s == ' ' || *s == '\t'; ++s)
			;
		if (skipping)
			{
			if (*s == '(')
				++paren_level;
			else if (*s == ')' && --paren_level <= 0)
				skipping = FALSE;
			continue;
			}
		el_exists = NULL;
		if (   (el = pcb_element_line_parse(s)) != NULL
			&& (el_exists = pcb_element_exists(el, FALSE)) != NULL
			&& !el_exists->still_exists
			&& !preserve
		   )
			{
			skipping = TRUE;
			if (verbose)
				printf(
					"%s: deleted element %s (value=%s)\n",
					el->refdes, el->description, el->value);
			pcb_element_free(el);
			continue;
			}
		if (el_exists && el_exists->changed_value)
			{
			fprintf(f_out, "Element%c%s \"%s\" \"%s\" \"%s\" %d %d%s\n",
					el->res_char, el->flags, el->description, el->refdes,
					el_exists->changed_value, el->x, el->y, el->tail);
			if (verbose)
				printf(
					"%s: changed element %s value: %s -> %s\n",
					el->refdes, el->description,
					el->value, el_exists->changed_value);
			}
		else if (!strncmp(s, "PKG_", 4))
			++n_PKG_removed_old;
		else
			fputs(buf, f_out);
		pcb_element_free(el);
		}
	fclose(f_in);
	fclose(f_out);

	if (!bak_done)
		{
		command = g_strconcat("mv ", pcb_file, " ", bak, NULL);
		g_spawn_command_line_sync(command, NULL, NULL, NULL, NULL);
		g_free(command);
		bak_done = TRUE;
		}

	command = g_strconcat("mv ", tmp, " ", pcb_file, NULL);
	g_spawn_command_line_sync(command, NULL, NULL, NULL, NULL);
	g_free(command);
	g_free(tmp);
	}

static void
add_m4_file(gchar *arg)
	{
	gchar	*s;

	if (!m4_files)
		m4_files = g_strdup(arg);
	else
		{
		s = m4_files;
		m4_files = g_strconcat(m4_files, " ", arg, NULL);
		g_free(s);
		}
	}

static gchar *
expand_dir(gchar *dir)
	{
	gchar	*s;

	if (*dir == '~')
		s = g_build_filename((gchar *) g_get_home_dir(), dir + 1, NULL);
	else
		s = g_strdup(dir);
	return s;
	}

static void
add_default_m4_files(void)
	{
	gchar	*path;

	path = g_build_filename((gchar *) g_get_home_dir(),
				".pcb", DEFAULT_PCB_INC, NULL);
	if (g_file_test(path, G_FILE_TEST_IS_REGULAR))
		add_m4_file(path);
	g_free(path);

	if (g_file_test(DEFAULT_PCB_INC, G_FILE_TEST_IS_REGULAR))
		add_m4_file(DEFAULT_PCB_INC);

	}

static void
add_schematic(gchar *sch)
	{
	gchar	*s;

	s = schematics;
	if (s)
		schematics = g_strconcat(s, " ", sch, NULL);
	else
		schematics = g_strdup(sch);
	g_free(s);
	if (!basename && (s = strstr(sch, ".sch")) != NULL)
		basename = g_strndup(sch, s - sch);
	}

static gint
parse_config(gchar *config, gchar *arg)
	{
	if (verbose)
		printf("    %s %s\n", config, arg ? arg : "");

	if (!strcmp(config, "remove-unfound") || !strcmp(config, "r"))
		{
		/* This is default behavior set in header section */
		remove_unfound_elements = TRUE;
		return 0;
		}
	if (!strcmp(config, "keep-unfound") || !strcmp(config, "k"))
		{
		remove_unfound_elements = FALSE;
		return 0;
		}
	if (!strcmp(config, "quiet") || !strcmp(config, "q"))
		{
		quiet_mode = TRUE;
		return 0;
		}
	if (!strcmp(config, "preserve") || !strcmp(config, "p"))
		{
		preserve = TRUE;
		return 0;
		}
	if (!strcmp(config, "use-files") || !strcmp(config, "f"))
		{
		force_element_files = TRUE;
		return 0;
		}
	if (!strcmp(config, "elements-dir") || !strcmp(config, "d"))
		element_directory_list =
				g_list_prepend(element_directory_list, expand_dir(arg));
	else if (!strcmp(config, "output-name") || !strcmp(config, "o"))
		basename = g_strdup(arg);
	else if (!strcmp(config, "schematics"))
		add_schematic(arg);
	else if (!strcmp(config, "m4-command"))
		m4_command = g_strdup(arg);
	else if (!strcmp(config, "m4-pcbdir"))
		m4_pcbdir = g_strdup(arg);
	else if (!strcmp(config, "m4-file"))
		add_m4_file(arg);
	else if (!strcmp(config, "gnetlist"))
		extra_gnetlist_list =
				g_list_append(extra_gnetlist_list, g_strdup(arg));
	else if (!strcmp(config, "empty-footprint"))
		empty_footprint_name = g_strdup(arg);
	else
		return -1;

	return 1;
	}

static void
load_project(gchar *path)
	{
	FILE	*f;
	gchar	*s, buf[1024], config[32], arg[768];

	f = fopen(path, "r");
	if (!f)
		return;
	if (verbose)
		printf("Reading project file: %s\n", path);
	while (fgets(buf, sizeof(buf), f))
		{
		for (s = buf; *s == ' ' || *s == '\t' || *s == '\n'; ++s)
			;
		if (!*s || *s == '#' || *s == '/' || *s == ';')
			continue;
		arg[0] = '\0';
		sscanf(s, "%31s %767[^\n]", config, arg);
		parse_config(config, arg);
		}
    fclose(f);
    }

static void
load_extra_project_files(void)
	{
	gchar			*path;
	static gboolean	done = FALSE;

	if (done)
		return;

	load_project("/etc/gsch2pcb");
	load_project("/usr/local/etc/gsch2pcb");

	path = g_build_filename((gchar *) g_get_home_dir(), ".gsch2pcb", NULL);
	load_project(path);
	g_free(path);

	done = TRUE;
	}

static gchar *usage_string0 =
"usage: gsch2pcb [options] {project | foo.sch [foo1.sch ...]}\n"
"\n"
"Generate a PCB layout file from a set of gschem schematics.\n"
"   gnetlist -g PCB is run to generate foo.net from the schematics.\n"
"   gnetlist -g gsch2pcb is run to get PCB m4 derived elements which\n"
"   match schematic footprints.  For schematic footprints which don't match\n"
"   any PCB m4 layout elements, search a set of file element directories in\n"
"   an attempt to find matching PCB file elements.\n"
"   Output to foo.pcb if it doesn't exist.  If there is a current foo.pcb,\n"
"   output only new elements to foo.new.pcb.\n"
"   If any elements with a non-empty element name in the current foo.pcb\n"
"   have no matching schematic component, then remove those elements from\n"
"   foo.pcb and rename foo.pcb to a foo.pcb.bak sequence.\n"
"\n"
"   \"project\" is a file (not ending in .sch) containing a list of\n"
"   schematics to process and some options.  A schematics line is like:\n"
"       schematics foo1.sch foo2.sch ...\n"
"   Options in a project file are like command line args without the \"-\":\n"
"       output-name myproject\n"
"\n"
"options (may be included in a project file):\n"
"   -d, --elements-dir D  Search D for PCB file elements.  These defaults\n"
"                         are searched if they exist: ./packages,\n"
"                         /usr/local/share/pcb/newlib, /usr/share/pcb/newlib,\n"
"                         (old pcb) /usr/local/lib/pcb_lib, /usr/lib/pcb_lib,\n"
"                         (old pcb) /usr/local/pcb_lib\n"
"   -o, --output-name N   Use output file names N.net, N.pcb, and N.new.pcb\n"
"                         instead of foo.net, ... where foo is the basename\n"
"                         of the first command line .sch file.\n"
"   -f, --use-files       Force using file elements over m4 PCB elements\n"
"                         for new footprints even though m4 elements are\n"
"                         searched for first and may have been found.\n"
"   -r, --remove-unfound  Don't include references to unfound elements in\n"
"                         the generated .pcb files.  Use if you want PCB to\n"
"                         be able to load the (incomplete) .pcb file.\n"
"                         This is the default behavior.\n"
"   -k, --keep-unfound    Keep include references to unfound elements in\n"
"                         the generated .pcb files.  Use if you want to hand\n"
"                         edit or otherwise preprocess the generated .pcb file\n"
"                         before running pcb.\n"
"   -p, --preserve        Preserve elements in PCB files which are not found\n"
"                         in the schematics.  Note that elements with an empty\n"
"                         element name (schematic refdes) are never deleted,\n"
"                         so you really shouldn't need this option.\n"
"   -q, --quiet           Don't tell the user what to do next after running gsch2pcb.\n"
"\n"
"   --m4-file F.inc       Use m4 file F.inc in addition to the default m4\n"
"                         files ./pcb.inc and ~/.pcb/pcb.inc.\n"
"   --m4-pcbdir D         Use D as the PCB m4 files install directory\n"
"                         instead of the default:\n";

static gchar *usage_string1 =
"   --gnetlist backend    A convenience run of extra gnetlist -g commands.\n"
"                         Example:  gnetlist partslist3\n"
"                         Creates:  myproject.partslist3\n"
" --empty-footprint name  See the project.sample file.\n"
"\n"
"options (not recognized in a project file):\n"
"   --fix-elements        If a schematic component footprint is not equal\n"
"                         to its PCB element Description, update the\n"
"                         Description instead of replacing the element.\n"
"                         Do this the first time gsch2pcb is used with\n"
"                         PCB files originally created with gschem2pcb.\n"
"   -v, --verbose         Use -v -v for additional file element debugging.\n"
"   -V, --version\n\n"
;

static void
usage()
	{
	printf(usage_string0);
	printf("                         %s\n", default_m4_pcbdir);
	printf(usage_string1);
	exit(0);
	}

static void
get_args(gint argc, gchar **argv)
	{
	gchar	*opt, *arg, *s;
	gint	i, r;

	for (i = 1; i < argc; ++i)
		{
		opt = argv[i];
		arg = argv[i + 1];
		if (*opt == '-')
			{
			++opt;
			if (*opt == '-')
				++opt;
			if (!strcmp(opt, "version") || !strcmp(opt, "V"))
				{
				printf("gsch2pcb %s\n", GSC2PCB_VERSION);
				exit(0);
				}
			else if (!strcmp(opt, "verbose") || !strcmp(opt, "v"))
				{
				verbose += 1;
				continue;
				}
			else if (!strcmp(opt, "fix-elements"))
				{
				fix_elements = TRUE;
				continue;
				}
			else if (!strcmp(opt, "help") || !strcmp(opt, "h"))
				usage();
			else if (   i < argc
					 && ((r = parse_config(opt, (i < argc - 1) ? arg : NULL))
									>= 0)
					)
				{
				i += r;
				continue;
				}
			printf("gsch2pcb: bad or incomplete arg: %s\n", argv[i]);
			usage();
			}
		else
			{
			if ((s = strstr(argv[i], ".sch")) == NULL)
				{
				load_extra_project_files();
				load_project(argv[i]);
				}
			else
				add_schematic(argv[i]);
			}
		}
	}

gint
main(gint argc, gchar **argv)
	{
	gchar	*pcb_file_name,
			*pcb_new_file_name,
			*bak_file_name,
			*net_file_name,
			*tmp;
	gint	i;
	gboolean initial_pcb = TRUE;

	if (argc < 2)
		usage();

	/* Default m4 dir was /usr/X11R6/lib/X11/pcb/m4, but in more recent
	|  pcb versions it's under /usr/share or /usr/local/share
	*/
	if (g_file_test("/usr/local/share/pcb/m4", G_FILE_TEST_IS_DIR))
		m4_pcbdir = g_strdup("/usr/local/share/pcb/m4");
	else if (g_file_test("/usr/share/pcb/m4", G_FILE_TEST_IS_DIR))
		m4_pcbdir = g_strdup("/usr/share/pcb/m4");
	default_m4_pcbdir = g_strdup(m4_pcbdir ?
		   m4_pcbdir
		: "/usr/X11R6/lib/X11/pcb/m4" /* hardwired in gnet-gsch2pcb.scm */);


	get_args(argc, argv);

	load_extra_project_files();
	add_default_m4_files();

	if (!schematics)
		usage();


	/* Hardwire in directories from Pcb.ad.  PCB as of 20031113 uses share
	|  instead of lib.  Check for standard prefix dirs of /usr and /usr/local.
	|  If PCB is installed elsewhere (eg. /opt) there will need to be a project
	|  elements-dir line.
	*/
	if (g_file_test("packages", G_FILE_TEST_IS_DIR))
		element_directory_list = g_list_append(element_directory_list,
					"packages");
	if (g_file_test("/usr/local/share/pcb/newlib", G_FILE_TEST_IS_DIR))
		element_directory_list = g_list_append(element_directory_list,
					"/usr/local/share/pcb/newlib");
	if (g_file_test("/usr/share/pcb/newlib", G_FILE_TEST_IS_DIR))
		element_directory_list = g_list_append(element_directory_list,
					"/usr/share/pcb/newlib");
	if (g_file_test("/usr/local/lib/pcb_lib", G_FILE_TEST_IS_DIR))	/* old */
		element_directory_list = g_list_append(element_directory_list,
					"/usr/local/lib/pcb_lib");
	if (g_file_test("/usr/lib/pcb_lib", G_FILE_TEST_IS_DIR))		/* old */
		element_directory_list = g_list_append(element_directory_list,
					"/usr/lib/pcb_lib");
	if (g_file_test("/usr/local/pcb_lib", G_FILE_TEST_IS_DIR))		/* old */
		element_directory_list = g_list_append(element_directory_list,
					"/usr/local/pcb_lib");

	net_file_name = g_strconcat(basename, ".net", NULL);
	pcb_file_name = g_strconcat(basename, ".pcb", NULL);
	bak_file_name = g_strconcat(basename, ".pcb.bak", NULL);
	tmp = g_strdup(bak_file_name);

	for (i = 0; g_file_test(bak_file_name, G_FILE_TEST_EXISTS); ++i)
		{
		g_free(bak_file_name);
		bak_file_name = g_strdup_printf("%s%d", tmp, i);
		}
	g_free(tmp);

	if (g_file_test(pcb_file_name, G_FILE_TEST_EXISTS))
		{
		initial_pcb = FALSE;
		pcb_new_file_name = g_strconcat(basename, ".new.pcb", NULL);
		get_pcb_element_list(pcb_file_name);
		}
	else
		pcb_new_file_name = g_strdup(pcb_file_name);

	run_gnetlist(net_file_name, pcb_new_file_name, basename, schematics);

	if (add_elements(pcb_new_file_name) == 0)
		{
		tmp = g_strconcat("rm ", pcb_new_file_name, NULL);
		g_spawn_command_line_sync(tmp, NULL, NULL, NULL, NULL);
		g_free(tmp);
		if (initial_pcb)
			{
			printf("No elements found, so nothing to do.\n");
			exit(0);
			}
		}

	if (fix_elements)
		update_element_descriptions(pcb_file_name, bak_file_name);
	prune_elements(pcb_file_name, bak_file_name);

        /* Report work done during processing */
	if (verbose)
		printf("\n");
	printf("\n----------------------------------\n");
	printf("Done processing.  Work performed:\n");
	if (n_deleted > 0 || n_fixed > 0 || need_PKG_purge || n_changed_value > 0)
		printf("%s is backed up as %s.\n",
				pcb_file_name, bak_file_name);
	if (pcb_element_list && n_deleted > 0)
		printf("%d elements deleted from %s.\n", n_deleted, pcb_file_name);

	if (n_added_ef + n_added_m4 > 0)
		printf("%d file elements and %d m4 elements added to %s.\n",
					n_added_ef, n_added_m4, pcb_new_file_name);
	    else if (n_not_found == 0)
		    printf("No elements to add so not creating %s\n", pcb_new_file_name);
	if (n_not_found > 0)
		{
		printf("%d not found elements added to %s.\n",
				n_not_found, pcb_new_file_name);
		}
	if (n_unknown > 0)
		printf("%d components had no footprint attribute and are omitted.\n",
				n_unknown);
	if (n_none > 0)
		printf("%d components with footprint \"none\" omitted from %s.\n",
				n_none, pcb_new_file_name);
	if (n_empty > 0)
		printf("%d components with empty footprint \"%s\" omitted from %s.\n",
				n_empty, empty_footprint_name, pcb_new_file_name);
	if (n_changed_value > 0)
		printf("%d elements had a value change in %s.\n",
				n_changed_value, pcb_file_name);
	if (n_fixed > 0)
		printf("%d elements fixed in %s.\n", n_fixed, pcb_file_name);
	if (n_PKG_removed_old > 0)
		printf("%d unknown elements removed from %s.\n",
				n_PKG_removed_old, pcb_file_name);
	if (n_PKG_removed_new > 0)
		printf("%d unknown elements removed from %s.\n",
				n_PKG_removed_new, pcb_new_file_name);
	if (n_preserved > 0)
		printf("%d elements not in the schematic preserved in %s.\n",
				n_preserved, pcb_file_name);

        /* Tell user what to do next */
	if (verbose)
		printf("\n");

	if ((n_added_ef + n_added_m4 > 0) && (initial_pcb == FALSE) && (quiet_mode == FALSE))
	   {
	     printf("\nNext steps:\n");
	     printf("1.  Run pcb on your file %s.\n", pcb_file_name);
             printf("2.  From within PCB, select \"File -> Load layout data to paste buffer\"\n");
             printf("    and select %s to load the new footprints into your existing layout.\n", 
		          pcb_new_file_name);
             printf("3.  From within PCB, select \"File -> Load netlist file\" and select \n");
             printf("    %s to load the updated netlist.\n\n", net_file_name);
	   }
	else if ((n_added_ef + n_added_m4 > 0) && (initial_pcb == TRUE))
	   {
	     printf("\nNext steps:\n");
	     printf("1.  Run pcb on your file %s.\n", pcb_file_name);
             printf("    You will find all your footprints in a bundle ready for you to place.\n\n");
	   }
	else 
	   {
	     printf("\n");
	   }
	return 0;
	}
