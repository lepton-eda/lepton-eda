/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
 * Copyright (C) 1998-2008 Ales Hvezda
 * Copyright (C) 1998-2008 gEDA Contributors (see ChangeLog for details)
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
 */

/*! \file o_text_basic.c
 *  \brief functions for the text and fonts
 *
 *  \par The font definitions 
 *
 *  Each letter of the font is defined in a single font symbol file. In
 *  the font symbol file, the character width is defined in the second
 *  line. The first line contains the file format version.
 *
 *  All remaining lines are basic graphical lines. They build the
 *  appearance of the character.
 *
 *  \image html o_text_font_overview.png
 *  \image latex o_text_font_overview.pdf "font overview" width=14cm
 *
 *  The height of capital characters in the font files is 26. The size
 *  of small letters is 16. The space below the zero line is used by
 *  characters like <b>g</b>, <b>p</b> or <b>q</b>. The space above 26
 *  is used by diacritic marks like accents, breve, circumflex mostly in
 *  european characters.
 *
 *  When loading a font definition the basic line objects are stored in
 *  <b>OBJECT->font_prim_objs</b> as a list of OBJECTs.
 *  
 *  All font objects are stored in the hash table #font_loaded when they 
 *  are loaded.
 *
 *  \par The text definitions
 *
 *  The text is stored and printed in several different representations.
 *
 *  In the gEDA files the text is just a string. It is stored unmodified 
 *  in <b>OBJECT->text->string</b>.
 *
 *  If the string is an attribute with an equal sign as delimiter between
 *  an attribute name and an attribute value, then it is possible to
 *  hide some parts of the text. The still visible part of an attribute
 *  is stored in <b>OBJECT->text->disp_string</b>.
 *
 *  \image html o_text_text_overview.png
 *  \image latex o_text_text_overview.pdf "text overview" width=14cm
 *
 *  To draw the text in gschem, the string is interpreted and converted
 *  to a list of basic graphical objects. The basic line objects are
 *  collected from the font character objects.
 *  All basic graphical objects are stored in
 *  <b>OBJECT->text->prim_objs</b>.
 */

#include <config.h>

#include <stdio.h>
#include <math.h>
#include <sys/stat.h>
#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "libgeda_priv.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

/*! Default setting for text draw function. */
void (*text_draw_func)() = NULL;

/*! Hashtable storing font_character (string) as a key, and pointer to data 
 *  \note
 *  This table stays global, thus all functions can access it.
 */
GHashTable *font_loaded = NULL;

/*! Hashtable storing mapping between character and font definition file
 *  \note
 *  This table stays global, thus all functions can access it.
 */
GHashTable *font_char_to_file = NULL;

/*! Size of a tab in characters */
int tab_in_chars = 8;


/*! \brief update the visible part of a string
 *  \par Function Description
 *  If a string is an attribute, then it is possible to hide
 *  the name or the value part of the attribute string.
 *  This functions updates the text->disp_string according
 *  to the object->show_name_value settings
 *  
 *  \param [in] o  The OBJECT to update
 */
static void update_disp_string(OBJECT *o)
{
  char *name = NULL;
  char *value = NULL;
  TEXT *text = o->text;

  g_free (text->disp_string);

  if (o_attrib_get_name_value (text->string, &name, &value)) {
    switch (o->show_name_value) {
      case (SHOW_NAME_VALUE):
        text->disp_string = g_strdup (text->string);
        break;

      case (SHOW_NAME):
        if (name[0] != '\0') {
          text->disp_string = g_strdup (name);
        } else {
          g_critical ("Got an improper attribute: %s\n",
                      text->string);
          text->disp_string = g_strdup ("invalid");
        }
        break;

      case (SHOW_VALUE):
        if (value[0] != '\0') {
          text->disp_string = g_strdup(value);
        } else {
          g_critical ("Got an improper attribute: %s\n",
                      text->string);
          text->disp_string = g_strdup ("invalid");
        }
        break;
    }
    /* free the strings allocated by o_attrib_get_name_value */
    g_free(name);
    g_free(value);
  } else {
    text->disp_string = g_strdup (text->string);
  }
}

/*! \brief calculate and return the boundaries of a text object
 *  \par Function Description
 *  This function calculates the object boudaries of a text \a object.
 *
 *  \param [in]  toplevel  The TOPLEVEL object.
 *  \param [in]  o_current a text object
 *  \param [out] left      the left world coord
 *  \param [out] top       the top world coord
 *  \param [out] right     the right world coord
 *  \param [out] bottom    the bottom world coord
 */
int world_get_text_bounds(TOPLEVEL *toplevel, OBJECT *o_current, int *left,
                          int *top, int *right, int *bottom)
{
  return world_get_object_glist_bounds (toplevel, o_current->text->prim_objs,
                                        left, top, right, bottom);
}

/*! \brief initialize the hash tables for the fonts
 *  \par Function Description
 *  This function initializes the two global hash tables <b>font_loaded</b> 
 *  and <b>font_char_to_file</b> that are used to store the fonts characters.
 */
void o_text_init(void)
{
  if (font_loaded == NULL) {
    font_loaded = g_hash_table_new_full (g_direct_hash,
                                         g_direct_equal,
                                         NULL,
                                         g_free);
  } else {
    fprintf (stderr, "o_text_init: Tried to initialize an already initialized font_loaded hash table!!\n");
  }
  if (font_char_to_file == NULL) {
    font_char_to_file = g_hash_table_new_full (g_direct_hash,
                                               g_direct_equal,
                                               NULL,
                                               g_free);
  }
  else {
    fprintf (stderr, "o_text_init: Tried to initialize an already initialized font_char_to_file hash table!!\n");
  }
  
  return;

}

/*! \brief print informations about some characters
 *  \note
 *  This is a debugging function. Do not use it in regular code.
 */
void o_text_print_set(void)
{
  OBJECT *o_current, *o_font_set;
  GList *iter;
  char i;
	
  for (i = 'A' ; i < 'Z'+1; i++) {
    o_font_set = g_hash_table_lookup (font_loaded,
                                      GUINT_TO_POINTER ((gunichar)i));
    if (o_font_set != NULL) {
      printf("%c: LOADED\n", i);	
      for (iter=o_font_set->font_prim_objs; iter != NULL;
           iter = g_list_next (iter))
      {
        o_current = (OBJECT *)iter->data;
        printf("  %s\n", o_current->name);	
      }
    } else {
      printf("%c: unloaded\n", i);	
    }
  }
}

/*! \brief load a font character into an object
 *  \par Function Description
 *  This function loads a character form a font symbol file.
 *
 *  \param [in] toplevel    The TOPLEVEL object
 *  \param [in] needed_char unicode character to load
 *  return a character OBJECT
 */
GList *o_text_load_font (TOPLEVEL *toplevel, gunichar needed_char)
{
  gchar *temp_string = NULL;
  OBJECT *o_font_set;
  int not_found = FALSE;
  gchar *aux_str2;
  GError *err = NULL;

  /* retrieve the name of the file where the char is defined */
  aux_str2 = g_hash_table_lookup (font_char_to_file,
                                  GUINT_TO_POINTER (needed_char));
  if (aux_str2 == NULL) {
      /* this is needed since WinNT file systems are 
       * case insensitive, and cannot tell the difference 
       * between A.sym and a.sym.  So we create a_.sym -  
       * z_.sym, this loads up the chars 
       */
      if (needed_char >= 'a' && needed_char <= 'z') {
        temp_string = g_strdup_printf("%s%c%c_.sym", 
                toplevel->font_directory, G_DIR_SEPARATOR,
                needed_char);
      } else {
        temp_string = g_strdup_printf("%s%c%c.sym", 
                toplevel->font_directory, G_DIR_SEPARATOR,
                needed_char);
      }
  } else {
    temp_string = g_strdup_printf("%s", aux_str2);
  }
  aux_str2 = NULL;

  if ( access(temp_string, R_OK) != 0 ) {
    gchar outbuf[7];
    gint l;

    /* convert needed_char to a utf-8 string */
    l = g_unichar_to_utf8 (needed_char, outbuf);
    outbuf[l] = '\0';
    s_log_message(_("Could not find character '%s' definition.\n"), outbuf);
    
    g_free (temp_string);
    temp_string = g_build_filename (toplevel->font_directory, "quest.sym", NULL);
    if ( access(temp_string, R_OK) != 0 ) {
      fprintf(stderr, _("Could not load question font char -- check font-directory keyword\n"));
      exit(-1);
    }
    not_found = TRUE; 
  }

  /* Make new object for the font set list */
  o_font_set = (OBJECT*)g_new (OBJECT, 1);	
  
  o_font_set->font_prim_objs = NULL;
  o_font_set->font_text_size = 100;

  o_font_set->name = g_strdup_printf ("%c", needed_char);
  o_font_set->font_prim_objs = NULL;
  
  /* Add it to the list and hash table. Some functions will need it */
  g_hash_table_insert (font_loaded,
                       GUINT_TO_POINTER (needed_char), o_font_set);

  if (not_found == TRUE) {
    /* set the font text size (width) to the question mark's size */
    /* yes, the question mark character was loaded instead of the real char */
    /* 63 == question mark character */

    OBJECT *aux_obj;
    
    aux_obj = g_hash_table_lookup (font_loaded,
                                   GUINT_TO_POINTER ((gunichar)'?'));
    if (aux_obj == NULL) {
      o_text_load_font(toplevel, (gunichar) '?');
      aux_obj = g_hash_table_lookup (font_loaded,
                                     GUINT_TO_POINTER ((gunichar)'?'));
    }

    o_font_set->font_text_size = aux_obj->font_text_size;
  }

  o_font_set->font_prim_objs = o_read(toplevel, o_font_set->font_prim_objs,
				      temp_string, &err);
  if (err != NULL) {
    g_warning ("o_text_basic.c: Failed to read font file: %s\n",
               err->message);
    g_error_free (err);
  }

  g_free(temp_string);

  return(o_font_set->font_prim_objs);
}

/*! \brief count the lines of a text string
 *  \par Function Description
 *  This function just counts the number of lines that are
 *  in the \a string.

 *  \param [in] string  text string to count the lines
 *  \return the number of lines
 */
int o_text_num_lines(const char *string) 
{
  int line_count = 0;
  const gchar *aux;
  gunichar current_char;

  if (string == NULL) {
    return 0;
  }
  
  /* if it's not null, then we have at least one line */
  line_count++;
  /* Count how many \n are in the string */
  aux = string;
  while (aux && ((gunichar) (*aux) != 0) ) {
    current_char = g_utf8_get_char_validated(aux, -1);
    if (current_char == '\n')
      line_count++;
    aux = g_utf8_find_next_char(aux, NULL);
  }

  return (line_count);
}

/*! \brief calculates the height of a text string
 *  \par Function Description
 *  This function calculates the height of a \a string depending
 *  on it's text \a size. The number of lines and the spacing
 *  between the lines are taken into account.
 * 
 *  \param [in] string  the text string
 *  \param [in] size    the text size of the character
 *  \return the total height of the text string
 */
int o_text_height(const char *string, int size) 
{
  int line_count = 0;

  if (string == NULL) {
    return 0;
  }

  /* Get the number of lines in the string */
  line_count = o_text_num_lines(string);
  
  /* 26 is the height of a single char (in mils) */
  /* which represents a character which is 2 pts high */
  /* So size has to be divided in half */
  /* and it's added the LINE_SPACING*character_height of each line */
  return(26*size/2*(1+LINE_SPACING*(line_count-1)));
}

/*! \brief calculate the width of a text
 *  \par Function Description
 *  This function caluculates the width of a text \a string
 *  depending on the text \a size and the width of the individual
 *  characters that are in the text string.
 *  
 *  \param [in] toplevel  The TOPLEVEL object
 *  \param [in] string    The text string
 *  \param [in] size      The text size
 *  \return  the total width of the text.
 */
int o_text_width(TOPLEVEL *toplevel, char *string, int size)
{
  int width=0, max_width=0;
  int size_of_tab_in_coord;
  OBJECT *o_font_set;
  gchar *ptr;
  gunichar previous_char;
  gunichar c = 0;
  if (string == NULL) {
    return 0;
  }
  
  /* Make sure TAB_CHAR_MODEL is loaded before trying to use its text */
  /* size */
  o_font_set = g_hash_table_lookup (
    font_loaded, GUINT_TO_POINTER ((gunichar)TAB_CHAR_MODEL[0]));
  if (o_font_set == NULL) {
    o_text_load_font(toplevel, (gunichar) TAB_CHAR_MODEL[0]);
    o_font_set = (OBJECT *) g_hash_table_lookup (
      font_loaded, GUINT_TO_POINTER ((gunichar)TAB_CHAR_MODEL[0]));
  }
 
  /* Get the maximum tab width's in coordinates */
  size_of_tab_in_coord = tab_in_chars * size * o_font_set->font_text_size;

  for (ptr = string;
       ptr != NULL && *ptr != 0;
       ptr = g_utf8_find_next_char (ptr, NULL))
  {
    previous_char = c;
    c = g_utf8_get_char_validated (ptr, -1);

    if ( (c == (gunichar) '\\') &&
	 (previous_char != (gunichar) '\\') ) {
      continue;
    }
    if ( (c == (gunichar) '_') &&
	 (previous_char == (gunichar) '\\') ) {
      continue;
    }
    switch (c) {
        case ((gunichar)'\n'):
          width = 0;
          break;
        case ((gunichar)'\t'):
          width += (size_of_tab_in_coord - (width % size_of_tab_in_coord));
          break;
        default:
          /* find current_char */
          o_font_set = g_hash_table_lookup (font_loaded,
                                            GUINT_TO_POINTER (c));
          if (o_font_set == NULL) {
            o_text_load_font (toplevel, (gunichar)c);
            /* let do a new search for character c */
            o_font_set = g_hash_table_lookup (font_loaded,
                                              GUINT_TO_POINTER (c));
          }

          if (o_font_set != NULL) {
            width = width + size*o_font_set->font_text_size;
          }

          if (width > max_width) {
            max_width = width;
          }
    }
  }

  /* the size is a fudge factor */
  /* Changed the return value according to a suggestion of Ales. */
  /* Yes, the -size*10 fudge factor should be removed. */
  /* return(max_width - size*10); */
  return max_width;
}

/*! \brief create a complex text object from a string
 *  \par Function Description
 *  This function converts the \a string into a list of basic objects.
 *  All basic objects are appendend to the \a object_list.
 *  The basic objects are collected from the basic font definition 
 *  of each character of they are created as lines for the overbar feature.
 *
 *  \param [in] toplevel    The TOPLEVEL object
 *  \param [in] object_list The list to append the basic objects
 *  \param [in] string      The string to create the object list from
 *  \param [in] size        The size of the text object
 *  \param [in] color       The color of the text object
 *  \param [in] x           The x coord of the text object
 *  \param [in] y           The y coord of the text object
 *  \param [in] alignment   The alignment of the text object
 *  \param [in] angle       The angle of the text object (in 90 degree steps)
 *  
 *  \return the object list of the primary text objects
 */
GList *o_text_create_string (TOPLEVEL *toplevel, char *string, int size,
                             int color, int x, int y, int alignment, int angle)
{
  GList *new_obj_list = NULL;
  GList *start_of_char;
  OBJECT *new_obj;
  int x_offset;
  int y_offset;
  int text_width;
  int text_height;
  int char_height;
  int line_start_x, line_start_y;
  int sign=1;
  int overbar_startx=0, overbar_starty=0;
  int overbar_endx=0, overbar_endy=0;
  int overbar_height_offset = 0;
  gchar *ptr;
  OBJECT *o_font_set;
  gunichar current_char;
  gboolean escape = FALSE, overbar_started = FALSE;
  gboolean finish_overbar, start_overbar, leave_parser = FALSE;
  gboolean draw_character, draw_tabulator, draw_newline;

  /* error condition hack */
  if (string == NULL) {
    return(NULL);
  }

  /* now read in the chars */
  text_height = o_text_height(string, size); 
  char_height = o_text_height("a", size);
  text_width = o_text_width(toplevel, string, size/2);

  switch(angle) {
    case(0):
      sign = -1;
      break;
    case(90):
      sign = 1;
      break;
    case(180):
      sign = 1;
      break;
    case(270):
      sign = -1;
      break;
  }

  if (angle == 0 || angle == 180) {
    y = y - o_text_height("a", size) + text_height;
    
    switch(alignment) {

      case(LOWER_LEFT):
      x_offset = x;
      y_offset = y;
      break;

      case(MIDDLE_LEFT):
      x_offset = x;
      y_offset = y + sign*0.5*text_height;
      break;
	
      case(UPPER_LEFT):
      x_offset = x;
      y_offset = y + sign*text_height;
      break;
	
      case(LOWER_MIDDLE):
      x_offset = x + sign*0.5*text_width;
      y_offset = y;
      break;
	
      case(MIDDLE_MIDDLE):
      x_offset = x + sign*0.5*text_width;
      y_offset = y + sign*0.5*text_height;
      break;
	
      case(UPPER_MIDDLE):
      x_offset = x + sign*0.5*text_width;
      y_offset = y + sign*text_height;
	
      break;
	
      case(LOWER_RIGHT):
      x_offset = x + sign*text_width;
      y_offset = y;
      break;
	
      case(MIDDLE_RIGHT):
      x_offset = x + sign*text_width;
      y_offset = y + sign*0.5*text_height;
      break;
	
      case(UPPER_RIGHT):
      x_offset = x + sign*text_width;
      y_offset = y + sign*text_height;
      break;

      default: 
      fprintf(stderr, "Got an invalid text alignment [%d]\n",
              alignment); 
      fprintf(stderr, "Defaulting to Lower Left");
      alignment = LOWER_LEFT;
      x_offset = x;
      y_offset = y;
      break;
    }
  } else { /* angle is 90 or 270 */
    x = x + sign*(o_text_height("a", size) - text_height);

    switch(alignment) {

      case(LOWER_LEFT):
      x_offset = x;
      y_offset = y;
      break;

      case(MIDDLE_LEFT):
      x_offset = x + sign*0.5*text_height;
      y_offset = y;
      break;

      case(UPPER_LEFT):
      x_offset = x + sign*text_height;
      y_offset = y;
      break;

      case(LOWER_MIDDLE):
      x_offset = x;
      y_offset = y - sign*0.5*text_width;
      break;

      case(MIDDLE_MIDDLE):
      x_offset = x + sign*0.5*text_height;
      y_offset = y - sign*0.5*text_width;
      break;

      case(UPPER_MIDDLE):
      x_offset = x + sign*text_height;
      y_offset = y - sign*0.5*text_width;

      break;

      case(LOWER_RIGHT):
      x_offset = x;
      y_offset = y - sign*text_width;
      break;

      case(MIDDLE_RIGHT):
      x_offset = x + sign*0.5*text_height;
      y_offset = y - sign*text_width;
      break;

      case(UPPER_RIGHT):
      x_offset = x + sign*text_height;
      y_offset = y - sign*text_width;
      break;

      default: 
      fprintf(stderr, "Got an invalid text alignment [%d]\n",
              alignment); 
      fprintf(stderr, "Defaulting to Lower Left");
      alignment = LOWER_LEFT;
      x_offset = x;
      y_offset = y;
      break;
    }

  }

  switch(angle) {
    case(180):
    x_offset = x_offset - text_width;
    y_offset = y_offset - text_height;
    angle = 0;
    break;
  }

#if DEBUG
  printf("width: %d\n", o_text_width(toplevel, string, size/2));
  printf("1 %d %d\n", x_offset, y_offset);
#endif

  line_start_x = x_offset;
  line_start_y = y_offset;

  /* the overbar is 1/4 above the char height. */
  overbar_height_offset = char_height + char_height/4;

  for (ptr = string;
       ptr != NULL && !leave_parser;
       ptr = g_utf8_find_next_char (ptr, NULL)) {

    current_char = g_utf8_get_char_validated (ptr, -1);

    /* reset all actions */
    finish_overbar = FALSE;
    start_overbar = FALSE;
    leave_parser = FALSE;
    draw_character = FALSE;
    draw_tabulator = FALSE;
    draw_newline = FALSE;

    /* state machine to interpret the string:
     * there are two independant state variables overbar_started and escape.
     * The actions are set according to the current character and
     * the two state variables.
     */
    switch (current_char) {
    case '\0':
      /* end of the string */
      if (overbar_started)
        finish_overbar = TRUE;
      leave_parser = TRUE;
      break;
    case '\\':
      if (escape == TRUE) {
        draw_character = TRUE;
        escape = FALSE;
      } else {
        escape = TRUE;
      }
      break;
    case '_':
      if (escape == TRUE) {
        escape = FALSE;
        if (overbar_started == TRUE) {
          finish_overbar = TRUE;
          overbar_started = FALSE;
        } else {
          start_overbar = TRUE;
          overbar_started = TRUE;
        }
      } else {
        draw_character = TRUE;
      }
      break;
    case '\n':
      draw_newline = TRUE;
      if (overbar_started == TRUE) {
        finish_overbar = TRUE;
        start_overbar = TRUE;
      }
      escape = FALSE;
      break;
    case '\t':
      draw_tabulator = TRUE;
      escape = FALSE;
      break;
    default:
      draw_character = TRUE;
      escape = FALSE;
    }

    /* execute all actions set by the state machine
     * Note: It's important that the three actions
     * finish_overbar, draw_newline, start_overbar are executed
     * in exacly that order. It's required to continue overbars
     * over newlines.
     */
    if (draw_character) {
      /* get the character from the hash table */
      o_font_set = g_hash_table_lookup (font_loaded,
                                        GUINT_TO_POINTER (current_char));
      if (o_font_set == NULL) {
        o_text_load_font(toplevel, (gunichar) current_char);
        o_font_set = g_hash_table_lookup (font_loaded,
                                          GUINT_TO_POINTER (current_char));
      }

      /* Only add the character if there are primary object.
         e.g. the space character doesn't have those */
      if (o_font_set->font_prim_objs != NULL) {
        start_of_char = o_glist_copy_all (toplevel,
                                          o_font_set->font_prim_objs,
                                          NULL, NORMAL_FLAG);

        o_complex_set_color(start_of_char, color);
        o_scale(toplevel, start_of_char, size/2, size/2);

        /* Rotate and translate the character to its world position */
        o_glist_rotate_world (toplevel, 0, 0, angle, start_of_char);
        o_glist_translate_world (toplevel, x_offset, y_offset, start_of_char);

        /* Add the character to the list of prim_objs*/
        new_obj_list = g_list_concat (new_obj_list, start_of_char);
      }

      /* Calcule the position of the next character */
      switch(angle) {
      case(0):
        x_offset = (x_offset) + size/2*o_font_set->font_text_size;
        break;
      case(90):
        y_offset = (y_offset) + size/2*o_font_set->font_text_size;
        break;
      case(180):
        x_offset = (x_offset) - size/2*o_font_set->font_text_size;
        break;
      case(270):
        y_offset = (y_offset) - size/2*o_font_set->font_text_size;
        break;
      }
    }

    if (draw_tabulator) {
      gint size_of_tab_in_coord;
      gint rel_char_coord;
      /* Get the maximum tab width's in coordinates */
      size_of_tab_in_coord = (tab_in_chars *
                              o_text_width(toplevel, TAB_CHAR_MODEL, size/2));

      switch (angle) {
      case 0:
      case 180:
        rel_char_coord = x_offset - line_start_x;
        x_offset += (size_of_tab_in_coord -
                     (rel_char_coord % size_of_tab_in_coord));
        break;
      case 90:
        rel_char_coord = y_offset - line_start_y;
        y_offset += (size_of_tab_in_coord -
                     (rel_char_coord % size_of_tab_in_coord));
        break;
      case 270:
        rel_char_coord = line_start_y - y_offset;
        y_offset -= (size_of_tab_in_coord -
                     (rel_char_coord % size_of_tab_in_coord));
        break;
      default:
        fprintf(stderr, "o_text_create_string: Angle not supported\n");
        break;
      }
    }

    if (finish_overbar) {
      switch (angle) {
      case 0:
        overbar_endx = x_offset;
        overbar_endy = y_offset + overbar_height_offset;
        break;
      case 90:
        overbar_endx = x_offset - overbar_height_offset;
        overbar_endy = y_offset;
        break;
      case 180:
        overbar_endx = x_offset;
        overbar_endy = y_offset - overbar_height_offset;
        break;
      case 270:
        overbar_endx = x_offset + overbar_height_offset;
        overbar_endy = y_offset;
        break;
      default:
        fprintf(stderr, "o_text_create_string: Angle not supported\n");
        break;
      }
      /* Now add the overbar (if it is not a zero length overbar) */
      if ((overbar_startx != overbar_endx)
          || (overbar_starty != overbar_endy)) {
        new_obj = o_line_new (toplevel, OBJ_LINE, color,
                              overbar_startx, overbar_starty,
                              overbar_endx, overbar_endy);
        new_obj_list = g_list_append (new_obj_list, new_obj);
      }
    }

    if (draw_newline) {
      switch (angle) {
      case 0:
        x_offset = line_start_x;
        y_offset = line_start_y - char_height * LINE_SPACING;
        break;
      case 90:
        x_offset = line_start_x + char_height * LINE_SPACING;
        y_offset = line_start_y;
        break;
      case 180:
        x_offset = line_start_x;
        y_offset = line_start_y + char_height * LINE_SPACING;
        break;
      case 270:
        x_offset = line_start_x - char_height * LINE_SPACING;
        y_offset = line_start_y;
        break;
      default:
        fprintf(stderr, "o_text_create_string: Angle not supported\n");
        break;
      }
      line_start_x = x_offset;
      line_start_y = y_offset;
    }

    if (start_overbar) {
      switch (angle) {
      case 0:
        overbar_startx = x_offset;
        overbar_starty = y_offset + overbar_height_offset;
        break;
      case 90:
        overbar_startx = x_offset - overbar_height_offset;
        overbar_starty = y_offset;
        break;
      case 180:
        overbar_startx = x_offset;
        overbar_starty = y_offset - overbar_height_offset;
        break;
      case 270:
        overbar_startx = x_offset + overbar_height_offset;
        overbar_starty = y_offset;
        break;
      default:
        fprintf(stderr, "o_text_create_string: Angle not supported\n");
        break;
      }
    }
  }

  return new_obj_list;
}

/*! \brief Creates a text OBJECT and the graphical objects representing it
 *  \par Function Description
 *  Create an OBJECT of type OBJ_TEXT.
 *  Also add the OBJECTs forming the graphical representation of the visible
 *  string, to the text OBJECT's prim_objs list.
 *
 *  \param [in]  toplevel              The TOPLEVEL object.
 *  \param [in]  type                   OBJ_TEXT (TODO: why bother)
 *  \param [in]  color                  The color of the text.
 *  \param [in]  x                      World x coord of text.
 *  \param [in]  y                      World y coord of text.
 *  \param [in]  alignment              How text bounding box aligns on (x, y).
 *  \param [in]  angle                  Angle at which text will appear.
 *  \param [in]  string                 The text (TODO: can be char const *)!
 *  \param [in]  size                   Text size.
 *  \param [in]  visibility             VISIBLE or INVISIBLE.
 *  \param [in]  show_name_value        SHOW_NAME_VALUE or friends.
 *  \return Pointer to text OBJECT.
 *
 *  \note
 *  Caller is responsible for string; this function allocates its own copy.
 */
OBJECT *o_text_new(TOPLEVEL *toplevel,
		   char type, int color, int x, int y, int alignment,
		   int angle, const char *string, int size, 
		   int visibility, int show_name_value)
{
  OBJECT *new_node=NULL;
  TEXT *text;
  char *name = NULL;
  char *value = NULL;

  if (string == NULL) {
    return(NULL);
  }

  new_node = s_basic_new_object(type, "text");

  text = (TEXT *) g_malloc(sizeof(TEXT));

  text->string = g_strdup (string);
  text->disp_string = NULL; /* We'll fix this up later */
  text->length = strlen(string);
  text->size = size;
  text->alignment = alignment;
  text->x = x;
  text->y = y;
  text->angle = angle;

  new_node->text = text;

  new_node->draw_func = text_draw_func;  
  new_node->sel_func = select_func;  

  new_node->color = color;
  new_node->visibility = visibility; 
  new_node->show_name_value = show_name_value;

  update_disp_string (new_node);

  /* now start working on the complex */

  if (visibility == VISIBLE ||
      (visibility == INVISIBLE && toplevel->show_hidden_text)) {
    new_node->text->prim_objs =
      o_text_create_string (toplevel,
                           text->disp_string, size, color,
                           x, y, alignment, angle); 
    new_node->text->displayed_width = o_text_width(toplevel,
                                                   text->disp_string, size/2);
    new_node->text->displayed_height = o_text_height(text->disp_string, size);
  } else {
    new_node->text->prim_objs = NULL;
    new_node->text->displayed_width = 0;
    new_node->text->displayed_height = 0;
  }

  /* Update bounding box */
  new_node->w_bounds_valid = FALSE;

  g_free(name);
  g_free(value);
  return new_node;
}

/*! \brief update the visual boundaries of the text object
 *  \par Function Description
 *  This function updates the boundaries of the object \a o_current.
 *
 *  \param [in]  toplevel  The TOPLEVEL object
 *  \param [in]  o_current The OBJECT to update
 */
void o_text_recalc(TOPLEVEL *toplevel, OBJECT *o_current)
{
  int left, right, top, bottom;

  if (o_current->visibility == INVISIBLE && !toplevel->show_hidden_text) {
    return;
  }

  if ( !world_get_text_bounds(toplevel, o_current, &left, &top, &right, &bottom) )
    return;

  o_current->w_left = left;
  o_current->w_top = top;
  o_current->w_right = right;
  o_current->w_bottom = bottom;
  o_current->w_bounds_valid = TRUE;
}

/*! \brief read a text object from a char buffer
 *  \par Function Description
 *  This function reads a text object from the textbuffer \a tb and 
 *  the text starting with the line \a firstline.
 *  If the line object was read successfully, a new object is
 *  create and appended to the \a object_list.
 *  
 *  \param [in] toplevel     The TOPLEVEL object
 *  \param [in] object_list  list of OBJECTS to append a new text
 *  \param [in] first_line   the first line of the text
 *  \param [in] tb           a text buffer (usually a line of a schematic file)
 *  \param [in] release_ver  The release number gEDA
 *  \param [in] fileformat_ver a integer value of the file format
 *  \return The object list
 */
OBJECT *o_text_read (TOPLEVEL *toplevel,
		    const char *first_line,
		    TextBuffer *tb,
		    unsigned int release_ver,
		    unsigned int fileformat_ver)
{
  OBJECT *new_obj;
  char type; 
  int x, y;
  int color;
  int size;
  int visibility;
  int show_name_value;
  int angle;
  int alignment;
  int num_lines = 0;
  int i;
  char* string = NULL;
  GString *textstr;

  if (fileformat_ver >= 1) {
    sscanf(first_line, "%c %d %d %d %d %d %d %d %d %d\n", &type, &x, &y, 
           &color, &size,
           &visibility, &show_name_value, 
           &angle, &alignment, &num_lines);	
  } else if (release_ver < VERSION_20000220) {
    /* yes, above less than (not less than and equal) is correct. The format */
    /* change occurred in 20000220 */
    sscanf(first_line, "%c %d %d %d %d %d %d %d\n", &type, &x, &y, 
           &color, &size,
           &visibility, &show_name_value, 
           &angle);	
    alignment = LOWER_LEFT; /* older versions didn't have this */
    num_lines = 1; /* only support a single line */
  } else {
    sscanf(first_line, "%c %d %d %d %d %d %d %d %d\n", &type, &x, &y, 
           &color, &size,
           &visibility, &show_name_value, 
           &angle, &alignment);	
    num_lines = 1; /* only support a single line */
  }

  if (size == 0) {
    s_log_message(_("Found a zero size text string [ %c %d %d %d %d %d %d %d %d ]\n"), type, x, y, color, size, visibility, show_name_value, angle, alignment);
  }

  switch(angle) {
	
    case(0):
    case(90):
    case(180):
    case(270):
    break;

    default:
      s_log_message(_("Found an unsupported text angle [ %c %d %d %d %d %d %d %d %d ]\n"),
                    type, x, y, color, size, visibility, show_name_value, angle, alignment);
      s_log_message(_("Setting angle to 0\n"));
      angle=0;
      break;

  }

  switch(alignment) {
    case(LOWER_LEFT):	
    case(MIDDLE_LEFT):	
    case(UPPER_LEFT):	
    case(LOWER_MIDDLE):	
    case(MIDDLE_MIDDLE):	
    case(UPPER_MIDDLE):	
    case(LOWER_RIGHT):	
    case(MIDDLE_RIGHT):	
    case(UPPER_RIGHT):	
			
    break;
		
    default:
      s_log_message(_("Found an unsupported text alignment [ %c %d %d %d %d %d %d %d %d ]\n"),
                    type, x, y, color, size, visibility, show_name_value, angle, alignment);
      s_log_message(_("Setting alignment to LOWER_LEFT\n"));
      alignment = LOWER_LEFT;
      break;
  }

  if (color < 0 || color > MAX_COLORS) {
    s_log_message(_("Found an invalid color [ %s ]\n"), first_line);
    s_log_message(_("Setting color to default color\n"));
    color = DEFAULT_COLOR;
  }

  g_assert(num_lines && num_lines > 0);

  textstr = g_string_new ("");
  for (i = 0; i < num_lines; i++) {
    gchar *line;
    
    line = s_textbuffer_next_line (tb);

    if (line != NULL)
      {
	textstr = g_string_append (textstr, line);
      }
  }
  /* retrieve the character string from the GString */
  string = g_string_free (textstr, FALSE);

  string = remove_last_nl(string);	

  /* convert the character string to UTF-8 if necessary */
  if (!g_utf8_validate (string, -1, NULL)) {
    /* if it is not utf-8, it is ISO_8859-15 */
    gchar *tmp = g_convert (string, strlen (string),
                            "UTF-8", "ISO_8859-15",
                            NULL, NULL, NULL);
    if (tmp == NULL) {
      fprintf (stderr, "Failed to convert text string to UTF-8: %s.\n",
               string);
    } else {
      /* successfully converted string, now use tmp as string */
      g_free (string);
      string = tmp;
    }
  }
  
  new_obj = o_text_new(toplevel, type, color, x, y,
                       alignment, angle, string,
                       size, visibility, show_name_value);
  g_free(string);

  return new_obj;
}

/*! \brief read and set infos of a font object
 *  \par Function Description
 *  This function reads the font definition buffer \a buf and sets 
 *  the width of a character. This function also deals with the special,
 *  invisible character space and newline.
 *  
 *  \param [in] buf  the font definition according to the geda file format
 *  \todo  Investigate why the TAB character is not defined here.
 */
void o_text_set_info_font(char buf[])
{
  char type; 
  int width;
  gunichar character=0;
  gchar *buf_ptr;
  int special=0;
  char *string; 
  OBJECT *o_font_set;

  string = remove_nl (buf);	

  /* parse the font info: */
  buf_ptr = (gchar*)string;
  /*   - type */
  type = *buf_ptr++;
  if (type != INFO_FONT) {
    g_critical ("o_text_set_info_font: Bad font type '%c', expected '%c'\n",
                type, INFO_FONT);
    return;
  }

  while (buf_ptr != NULL && *buf_ptr == ' ') buf_ptr++;
  /*   - character */
  if (buf_ptr != NULL && *buf_ptr != '\0') {
    character = g_utf8_get_char_validated (buf_ptr, -1);
    if (character == (gunichar)-1) {
      s_log_message (_("Failed to validate utf-8 character in font definition: \"%s\".\n"),
                     string);
      return;
    }
    /* move buf_ptr just after character */
    buf_ptr = g_utf8_find_next_char (buf_ptr, NULL);
  }
  while (buf_ptr != NULL && *buf_ptr == ' ') buf_ptr++;
  /*   - width and special */
  if (buf_ptr != NULL) {
    sscanf (buf_ptr, "%d %d\n", &width, &special);
  }

  /* deal with special characters */
  if (special == 1) {
    switch (character) {
      case ((gunichar)'_'):
      /* space */
      character = (gunichar)' ';
      break;
      case ((gunichar)'n'):
      /* newline */
      character = (gunichar)'\n';
      break;
    }
  }

  o_font_set = g_hash_table_lookup (font_loaded,
                                    GUINT_TO_POINTER ((gunichar)character));
  if (o_font_set != NULL) {
    o_font_set->font_text_size = width;
  } else {
    gchar outbuf[7];
    gint l = g_unichar_to_utf8 (character, outbuf);
    outbuf[l] = '\0';
    fprintf(stderr,
            "o_text_set_info_font: character %s not found!!!\n", outbuf);
  }
  
}

/*! \brief Create a string representation of the text object
 *  \par Function Description
 *  This function takes a text \a object and return a string
 *  according to the file format definition.
 *
 *  \param [in] object  a text OBJECT
 *  \return the string representation of the text OBJECT
 */
char *o_text_save(OBJECT *object)
{
  int x, y;
  int color;
  int size;
  char *string;
  char *buf;
  int num_lines;

  x = object->text->x;
  y = object->text->y;

  string = object->text->string;
  size = object->text->size;

  /* Use the right color */
  if (object->saved_color == -1) {
    color = object->color;
  } else {
    color = object->saved_color;
  }

  /* string can have multiple lines (seperated by \n's) */
  num_lines = o_text_num_lines(string);

  buf = g_strdup_printf("%c %d %d %d %d %d %d %d %d %d\n%s", object->type,
                        x, y, color, size, object->visibility, 
			object->show_name_value, object->text->angle, 
			object->text->alignment, num_lines, string);

  return(buf);
}

/*! \brief recreate the graphics of a text object
 *  \par Function Description
 *  This function updates the underlying primary of the text object 
 *  \a o_current.
 *
 *  \param toplevel  The TOPLEVEL object
 *  \param o_current The text object to update
 */
void o_text_recreate(TOPLEVEL *toplevel, OBJECT *o_current)
{
  char *name = NULL;
  char *value = NULL;
  TEXT *text = o_current->text;

  update_disp_string (o_current);

  s_delete_object_glist (toplevel, text->prim_objs);
  text->prim_objs = NULL;

  if (o_current->visibility == VISIBLE ||
      (o_current->visibility == INVISIBLE && toplevel->show_hidden_text)) {

    text->prim_objs = o_text_create_string (toplevel,
                                            text->disp_string,
                                            text->size,
                                            o_current->color,
                                            text->x,
                                            text->y,
                                            text->alignment,
                                            text->angle);

    o_complex_set_saved_color_only(text->prim_objs,
                                   o_current->saved_color);
    text->displayed_width = o_text_width (toplevel,
                                          text->disp_string,
                                          text->size/2);
    text->displayed_height = o_text_height (text->disp_string,
                                            text->size);
  } else {
    /* make sure list is truely free */
    s_delete_object_glist (toplevel, text->prim_objs);
    text->prim_objs = NULL;
    text->displayed_width = 0;
    text->displayed_height = 0;
  }

  o_current->w_bounds_valid = FALSE;

  g_free(name);
  g_free(value);
}

/*! \brief move a text object
 *  \par Function Description
 *  This function changes the position of a text object \a o_current.
 *
 *  \param [in] toplevel     The TOPLEVEL object
 *  \param [in] dx           The x-distance to move the object
 *  \param [in] dy           The y-distance to move the object
 *  \param [in] o_current    The text OBJECT to be moved
 */
void o_text_translate_world(TOPLEVEL *toplevel,
                            int dx, int dy, OBJECT *o_current)
{
  o_current->text->x = o_current->text->x + dx;
  o_current->text->y = o_current->text->y + dy;

  o_glist_translate_world (toplevel, dx, dy, o_current->text->prim_objs);

  /* Update bounding box */
  o_current->w_bounds_valid = FALSE;
}

/*! \brief create a copy of a text object
 *  \par Function Description
 *  This function creates a copy of the text object \a o_current.
 *
 *  \param [in] toplevel     The TOPLEVEL object
 *  \param [in] o_current    The object that is copied
 *  \return a new text object
 */
OBJECT *o_text_copy(TOPLEVEL *toplevel, OBJECT *o_current)
{
  OBJECT *new_obj;
  int color;

  if (o_current->saved_color == -1) {
    color = o_current->color;
  } else {
    color = o_current->saved_color;
  }

  new_obj = o_text_new (toplevel, OBJ_TEXT, color,
                        o_current->text->x, o_current->text->y,
                        o_current->text->alignment,
                        o_current->text->angle,
                        o_current->text->string,
                        o_current->text->size,
                        o_current->visibility,
                        o_current->show_name_value);

  return new_obj;
}

/*! \brief delete a font set
 *  \par Function Description
 *  This is a GHRFunc function that deletes a single font set.
 *
 *  \param [in] key        The hash key (the font charater)
 *  \param [in] value      The value of the hash table (the font object)
 *  \param [in] user_data  Data supplied by the user (the TOPLEVEL object)
 */
static gboolean delete_font_set (gpointer key, gpointer value,
				 gpointer user_data)
{
  OBJECT *tmp = (OBJECT*)value;
  TOPLEVEL *toplevel = (TOPLEVEL*)user_data;

  if (tmp != NULL) {
    if (tmp->font_prim_objs != NULL) {
      s_delete_object_glist (toplevel, tmp->font_prim_objs);
      tmp->font_prim_objs = NULL;
    }
    /* do not use s_delete_object () as tmp is not fully initialized */
    g_free (tmp->name);

    /* Do not free tmp here since it will be freed with the function */
    /* that was specified when the hash table was created. */
  }

  return TRUE;
}

/*! \brief free the font hash tables
 *  \par Function Description
 *  This function destroys the two global font hash tables
 *  <b>font_loaded</b> and <b>font_char_to_file</b>
 */
void o_text_freeallfonts(TOPLEVEL *toplevel)
{
  /* destroy the char-to-objects hastable */
  g_hash_table_foreach_remove (font_loaded,
                               delete_font_set,
                               toplevel);
  g_hash_table_destroy (font_loaded);
  font_loaded = NULL;

  /* destroy the font-to-filename hashtable */
  g_hash_table_destroy (font_char_to_file);
  font_char_to_file = NULL;
 
}

/*! \brief write a text string to a postscript file
 *  \par Function Description
 *  This function writes the single \a string into the postscript file \a fp.
 *
 *  \param [in] fp           pointer to a FILE structure
 *  \param [in] string       The string to print
 *  \param [in] unicode_count Number of items in the unicode table
 *  \param [in] unicode_table Table of unicode items
 *  
 *  \todo investigate whether the TAB character is handled correctly
 */
void o_text_print_text_string(FILE *fp, char *string, int unicode_count, 
			      gunichar *unicode_table)
{
  int len, j;
  gchar *aux;
  gunichar current_char, c;

  if (!string)
  {
    return;
  }

  aux = string;
  len = g_utf8_strlen(string, -1);
  
  fprintf(fp, "(");

  while (aux && ((gunichar) (*aux) != 0)) {
    current_char = g_utf8_get_char_validated(aux, -1);
    if (current_char == '(' || current_char == ')' || current_char == '\\') {
      fprintf(fp, "\\");
    }
  
    c = current_char;
    if (c >= 128) {
      current_char = '?';
      if (unicode_count)  {
        for (j = 0; j < unicode_count; j++)
          if (c == unicode_table[j]) {
	    current_char = j + 128;
            break;
          }
      }
    }


    if (current_char == '\t') {
      /* Output eight spaces instead of the tab character */
      fprintf(fp, "       ");
    } else {
      fprintf(fp, "%c", current_char);
    }

    aux = g_utf8_find_next_char(aux, NULL);
  }

  fprintf(fp,") ");
}

/*! \brief print a text object into a postscript file
 *  \par Function Description
 *  This function writes the postscript representation of the text object
 *  \a o_current into the the file \a fp.
 *  \param [in] toplevel     The TOPLEVEL object
 *  \param [in] fp           pointer to a FILE structure
 *  \param [in] o_current    The OBJECT to print
 *  \param [in] origin_x     x-coord of the postscript origin
 *  \param [in] origin_y     y-coord of the postscript origin
 *  \param [in] unicode_count Number of items in the unicode table
 *  \param [in] unicode_table Table of unicode items
 */
void o_text_print(TOPLEVEL *toplevel, FILE *fp, OBJECT *o_current,
		  int origin_x, int origin_y, 
		  int unicode_count, gunichar *unicode_table)
{
  int alignment;
  char *centering_control = NULL;
  char *p,*s;
  char *output_string = NULL;
  char *name = NULL;
  char *value = NULL;
  int x, y, angle, len, char_height;
  float font_size;


  if (!o_current->text->string) {
    return;
  }

  f_print_set_color(toplevel, fp, o_current->color);


  if (o_attrib_get_name_value(o_current->text->string, &name, &value)) {
    switch(o_current->show_name_value) {
      case(SHOW_NAME_VALUE):
        output_string = g_strdup(o_current->text->string);
        break;

      case(SHOW_NAME):
        if (name[0] != '\0') {
          output_string = g_strdup(name);
        } else {
          fprintf(stderr, 
                  "Got an improper attribute: %s\n", 
                  o_current->text->string);
          output_string = g_strdup("invalid");
        }
        break;

      case(SHOW_VALUE):
        if (value[0] != '\0') {
          output_string = g_strdup(value);
        } else {
          /* you probably can remove this now... */
          /* since improper attributes will never get here */
          fprintf(stderr, 
                  "Got an improper attribute: %s\n", 
                  o_current->text->string);
          output_string = g_strdup("invalid");
        }
        break;
    }
  } else {
    output_string = g_strdup(o_current->text->string);
  }

  /* Apply alignment map to apply when text is 180 degrees rotated.
   * We want the text on the printer to appear upside right, even
   * though mathematically it aught to be upside down.  To make this
   * work, we will reset the angle to 0, when it's equal to 180
   * degrees, then apply a transformation to the origin location as if
   * the text was rotated about that point.  E.g. if the text origin
   * was at the lower left, and the text was rotated by 180 degrees,
   * it would be as if the origin was at the upper right. The same
   * reasoning has been applied to all 8 other text origins.
   * MIDDLE_MIDDLE maps to itself.
   */
  alignment = o_current->text->alignment;
  angle = o_current->text->angle;
  if(angle == 180) {
    angle = 0;        /* reset angle to 0 to make text upright */
    switch(alignment) {
    case(LOWER_LEFT):    alignment = UPPER_RIGHT;
      break;
    case(MIDDLE_LEFT):   alignment = MIDDLE_RIGHT;
      break;
    case(UPPER_LEFT):    alignment = LOWER_RIGHT;
      break;
    case(LOWER_MIDDLE):  alignment = UPPER_MIDDLE;
      break;
    case(MIDDLE_MIDDLE): alignment = MIDDLE_MIDDLE;
      break;
    case(UPPER_MIDDLE):  alignment = LOWER_MIDDLE;
      break;
    case(LOWER_RIGHT):   alignment = UPPER_LEFT;
      break;
    case(MIDDLE_RIGHT):  alignment = MIDDLE_LEFT;
      break;
    case(UPPER_RIGHT):   alignment = LOWER_LEFT;
      break;
    }
  }

  /* Create an appropriate control string for the centering. */
  switch(alignment) {
                                       /* hcenter rjustify vcenter vjustify */
  case(LOWER_LEFT):    centering_control = "false false false false";
    break;
  case(MIDDLE_LEFT):   centering_control = "false false true false";
    break;
  case(UPPER_LEFT):    centering_control = "false false false true";
    break;
  case(LOWER_MIDDLE):  centering_control = "true false false false";
    break;
  case(MIDDLE_MIDDLE): centering_control = "true false true false";
    break;
  case(UPPER_MIDDLE):  centering_control = "true false false true";
    break;
  case(LOWER_RIGHT):   centering_control = "false true false false";
    break;
  case(MIDDLE_RIGHT):  centering_control = "false true true false";
    break;
  case(UPPER_RIGHT):   centering_control = "false true false true";
    break;
  }

  char_height = o_text_height("a", o_current->text->size);
  fprintf(fp,"%s %f [",centering_control,(float)(char_height*LINE_SPACING));

  /* split the line at each newline and print them */
  p = output_string;   /* Current point */
  s = output_string;   /* Start of the current string */
  len = strlen(output_string)+1;
  while(len != 0) {
    /* Have we reached the end of a line? */
    if((*p == '\n') || (*p == '\0')) {
      /* Yes, replace the newline with a NULL and output the string */
      *p = '\0';
      o_text_print_text_string(fp,s,unicode_count,unicode_table);
      /* Update output string start for next string */
      s = p+1; /* One past the current character. */
    }
    p++;   /* Advance to next character */
    len--; /* Keep track of how many characters left to process */
  }

  /* Finish up with the rest of the text print command */
  /* Collect pertinent info about the text location */
  x = o_current->text->x;
  y = o_current->text->y;
  font_size = (((float)(o_current->text->size))
	       * toplevel->postscript_font_scale / 72.0 * 1000.0);
  fprintf(fp,"] %d %d %d %f text\n",angle,x,y,font_size);

  
  g_free(output_string);
  g_free(name);
  g_free(value);
}


/*! \brief rotate a text object around a centerpoint
 *  \par Function Description
 *  This function rotates a text \a object around the point
 *  (\a world_centerx, \a world_centery).
 *  
 *  \param [in] toplevel      The TOPLEVEL object
 *  \param [in] world_centerx x-coord of the rotation center
 *  \param [in] world_centery y-coord of the rotation center
 *  \param [in] angle         The angle to rotate the text object
 *  \param [in] object        The text object
 *  \note only steps of 90 degrees are allowed for the \a angle
 */
void o_text_rotate_world(TOPLEVEL *toplevel,
                         int world_centerx, int world_centery,
                         int angle, OBJECT *object)
{
  int x, y;
  int newx, newy;

  g_return_if_fail(object != NULL);
  g_return_if_fail(object->type == OBJ_TEXT);

  object->text->angle = ( object->text->angle + angle ) % 360;

  x = object->text->x + (-world_centerx);
  y = object->text->y + (-world_centery);

  rotate_point_90(x, y, angle, &newx, &newy);

  x = newx + (world_centerx);
  y = newy + (world_centery);

  o_text_translate_world(toplevel, x-object->text->x, y-object->text->y, object);

  o_text_recreate(toplevel, object);
}


/*! \brief mirror a text object horizontaly at a centerpoint
 *  \par Function Description
 *  This function mirrors a text \a object horizontaly at the point
 *  (\a world_centerx, \a world_centery).
 *  
 *  \param [in] toplevel      The TOPLEVEL object
 *  \param [in] world_centerx x-coord of the mirror position
 *  \param [in] world_centery y-coord of the mirror position
 *  \param [in] object        The text object
 */
void o_text_mirror_world(TOPLEVEL *toplevel,
			 int world_centerx, int world_centery,
			 OBJECT *object)
{
  int origx, origy;
  int x, y;
	
  origx = object->text->x;
  origy = object->text->y;

  x = origx + (-world_centerx);
  y = origy + (-world_centery);

  if ((object->text->angle%180)==0) {
    switch(object->text->alignment) {
      case(LOWER_LEFT):
        object->text->alignment=LOWER_RIGHT;
        break;

      case(MIDDLE_LEFT):
        object->text->alignment=MIDDLE_RIGHT;
        break;

      case(UPPER_LEFT):
        object->text->alignment=UPPER_RIGHT;
        break;

      case(LOWER_RIGHT):
        object->text->alignment=LOWER_LEFT;
        break;

      case(MIDDLE_RIGHT):
        object->text->alignment=MIDDLE_LEFT;
        break;

      case(UPPER_RIGHT):
        object->text->alignment=UPPER_LEFT;
        break;

      default:
        break;
    }
  } else {
    switch(object->text->alignment) {
      case(LOWER_LEFT):
      object->text->alignment=UPPER_LEFT;
      break;

      case(UPPER_LEFT):
      object->text->alignment=LOWER_LEFT;
      break;

      case(LOWER_RIGHT):
      object->text->alignment=UPPER_RIGHT;
      break;

      case(UPPER_RIGHT):
      object->text->alignment=LOWER_RIGHT;
      break;

      case(LOWER_MIDDLE):
      object->text->alignment=UPPER_MIDDLE;
      break;

      case(UPPER_MIDDLE):
      object->text->alignment=LOWER_MIDDLE;
      break;

      default:
      break;
    }
  }

  object->text->x = -x + (world_centerx);
  object->text->y =  y + (world_centery);
	
  o_text_recreate(toplevel, object);
}

/*! \brief Calculates the distance between the given point and the closest
 *  point on the text.
 *
 *  This function will calculate the distance to the text regardless
 *  if the text is visible or not.
 *
 *  \param [in] object       The text OBJECT.
 *  \param [in] x            The x coordinate of the given point.
 *  \param [in] y            The y coordinate of the given point.
 *  \param [in] force_solid  If true, force treating the object as solid.
 *  \return The shortest distance from the object to the point. If the
 *  distance cannot be calculated, this function returns a really large
 *  number (G_MAXDOUBLE).  With an invalid parameter, this funciton
 *  returns G_MAXDOUBLE.
 */
double o_text_shortest_distance (OBJECT *object, int x, int y, int force_solid)
{
  double dx, dy;

  g_return_val_if_fail (object->text != NULL, G_MAXDOUBLE);

  dx = min (x - object->w_left, object->w_right - x);
  dy = min (y - object->w_top, object->w_bottom - y);

  dx = min (dx, 0);
  dy = min (dy, 0);

  return sqrt ((dx * dx) + (dy * dy));
}

/*! \brief Set the string displayed by a text object.
 *  \par Function Description
 *  Updates the text object with a new text string.
 *
 *  \param [in]  toplevel              The TOPLEVEL object.
 *  \param [in]  obj                   The text object.
 *  \param [in]  new_string            The new value.
 */
void o_text_set_string (TOPLEVEL *toplevel, OBJECT *obj,
                        const gchar *new_string)
{
  g_return_if_fail (toplevel != NULL);
  g_return_if_fail (obj != NULL);
  g_return_if_fail (obj->type == OBJ_TEXT);
  g_return_if_fail (obj->text != NULL);
  g_return_if_fail (new_string != NULL);

  g_free (obj->text->string);
  obj->text->string = g_strdup (new_string);
}



/*! \brief Get the string displayed by a text object.
 *  \par Function Description
 *  Retrieve the text string from a text object. The returned string
 *  should be treated as constant.
 *
 *  \param [in]  toplevel              The TOPLEVEL object.
 *  \param [in]  obj                   The text object.
 *  \return The text object's string, or NULL on failure.
 */
const gchar *o_text_get_string (TOPLEVEL *toplevel, OBJECT *obj)
{
  g_return_val_if_fail (toplevel != NULL, NULL);
  g_return_val_if_fail (obj != NULL, NULL);
  g_return_val_if_fail (obj->type == OBJ_TEXT, NULL);
  g_return_val_if_fail (obj->text != NULL, NULL);

  return obj->text->string;
}
