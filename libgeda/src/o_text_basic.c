/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
 * Copyright (C) 1998-2007 Ales Hvezda
 * Copyright (C) 1998-2007 gEDA Contributors (see ChangeLog for details)
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

#define WINONLY	1
#define BACKING 2

/*! \note
 *  font storage and friends are staying global so that all can access
 */

/*! Hashtable storing font_character (string) as a key, and pointer to data */
GHashTable *font_loaded = NULL;

/*! Hashtable storing mapping between character and font definition file */
GHashTable *font_char_to_file = NULL;

/*! Size of a tab in characters */
int tab_in_chars = 8;

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
int world_get_text_bounds(TOPLEVEL *toplevel, OBJECT *o_current, int *left,
                          int *top, int *right, int *bottom)
{
  return world_get_object_list_bounds(toplevel, o_current->text->prim_objs,
                                      left, top, right, bottom);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
OBJECT *o_text_add_head(void)
{
  OBJECT *new_node=NULL;

  new_node = s_basic_init_object("text_head");
  new_node->type = OBJ_HEAD;

  /* don't need to do this for head nodes */
  /* ret = s_basic_link_object(new_node, NULL);*/
  return(new_node);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
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

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_text_print_set(void)
{
  OBJECT *o_current, *o_font_set;
  char i;
	
  for (i = 'A' ; i < 'Z'+1; i++) {
    o_font_set = g_hash_table_lookup (font_loaded,
                                      GUINT_TO_POINTER ((gunichar)i));
    if (o_font_set != NULL) {
      printf("%c: LOADED\n", i);	
      for (o_current=return_tail(o_font_set->font_prim_objs); o_current; 
           o_current=o_current->prev) 
      {
        printf("  %s\n", o_current->name);	
      }
    } else {
      printf("%c: unloaded\n", i);	
    }
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
OBJECT *o_text_load_font(TOPLEVEL *toplevel, gunichar needed_char)
{
  gchar *temp_string = NULL;
  OBJECT *temp_parent, *o_font_set;
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
    temp_string = g_strdup_printf("%s%cquest.sym", toplevel->font_directory, G_DIR_SEPARATOR);
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
  o_font_set->font_prim_objs = o_text_add_head();
  
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
	

  temp_parent = toplevel->page_current->object_parent;
  /* set the addition of attributes to the head node */
  toplevel->page_current->object_parent = o_font_set->font_prim_objs;

  o_font_set->font_prim_objs = o_read(toplevel, o_font_set->font_prim_objs,
				      temp_string, &err);
  if (o_font_set->font_prim_objs == NULL) {
    g_assert (err != NULL);
    g_warning ("o_text_basic.c: Failed to read font file: %s\n",
               err->message);
    g_error_free (err);
  }

  toplevel->page_current->object_parent = temp_parent;

  o_font_set->font_prim_objs = return_head(o_font_set->font_prim_objs);

  g_free(temp_string);

  return(o_font_set->font_prim_objs);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
int o_text_num_lines(char *string) 
{
  int line_count = 0;
  gchar *aux;
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

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
/* You do not need to divide the size in half here. */
int o_text_height(char *string, int size) 
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

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
/* You need to divide the size in half here. */
/*! \todo FIXME consistancy. */
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

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
OBJECT *o_text_create_string(TOPLEVEL *toplevel, OBJECT *object_list,
			     char *string, int size, int color, int x, int y,
			     int alignment, int angle)
{
  OBJECT *temp_tail=NULL;
  OBJECT *temp_list;
  OBJECT *start_of_char;
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
  int last_char_width;
  gboolean overbar_started = FALSE;
  gchar *ptr;
  gchar aux_string[7];
  OBJECT *o_font_set;
  gunichar c=0, previous_char = 0, next_char = 0;
  int escapes_counter = 0;

  temp_list = object_list;


  /* error condition hack */
  if (string == NULL) {
    return(NULL);
  }
	
  /* now read in the chars */
  temp_tail = toplevel->page_current->object_tail;

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

  for (ptr = string;
       ptr != NULL && *ptr != 0;
       previous_char = 
	 g_utf8_get_char_validated (ptr, -1),
	 ptr = g_utf8_find_next_char (ptr, NULL),
	 next_char = g_utf8_get_char_validated(g_utf8_find_next_char (ptr, NULL),-1)) {
    /* Keep track of the previous character and its width.
       They will be used in the overbar and escape characters */
    /*   - build a char string out of the gunichar previous_char */
    gint l = g_unichar_to_utf8 (previous_char, aux_string);
    /*   - end the string */
    aux_string[l] = '\0';
    /*   - finally get the width of the previous character */
    last_char_width = o_text_width(toplevel, aux_string, size/2);

    c = g_utf8_get_char_validated (ptr, -1);

    o_font_set = g_hash_table_lookup (font_loaded, GUINT_TO_POINTER (c));
    if (o_font_set == NULL) {
      o_text_load_font(toplevel, (gunichar) c);
      o_font_set = g_hash_table_lookup (font_loaded, GUINT_TO_POINTER (c));
    }

    start_of_char = temp_list;

    if (o_font_set->font_prim_objs->next != NULL) {
      int rel_char_coord;
      int size_of_tab_in_coord;
      OBJECT *o_font_set_aux;

      /* Make sure TAB_CHAR_MODEL is loaded before trying to use its text */
      /* size */
      o_font_set_aux = g_hash_table_lookup (
        font_loaded, GUINT_TO_POINTER ((gunichar)TAB_CHAR_MODEL[0]));
      if (o_font_set_aux == NULL) {
         o_text_load_font(toplevel, (gunichar) TAB_CHAR_MODEL[0]);
         o_font_set_aux = g_hash_table_lookup (
           font_loaded, GUINT_TO_POINTER ((gunichar)TAB_CHAR_MODEL[0]));
       }
    
      /* Get the maximum tab width's in coordinates */
      size_of_tab_in_coord = tab_in_chars * 
                 o_text_width(toplevel, TAB_CHAR_MODEL, size/2);

      /* Count escape characters. Notice it includes the current character */
      if (c == '\\') {
	escapes_counter++;
      }
      
      /* Ignore escape characters */
      if ( ((c == '\\') && (previous_char != '\\')) ||
	   ((c == '\\') && (escapes_counter == 1)) )
	continue;

      if (c != '\n' && c != '\t') {
         /* only add the character if it is not a newline, a tab character
	    a escape character or the overbar delimiter escape sequence.
	    Add it if it is the escaped escape character */
	if ( ( (c != '\\') &&
	       (!(previous_char == '\\' && c == '_')) ) ||
	     (previous_char == '\\' && c == '\\' && (escapes_counter > 1)) ) {
	  temp_list = o_list_copy_all(toplevel,
				      o_font_set->font_prim_objs->next, 
				      temp_list, NORMAL_FLAG);
	  if (start_of_char != NULL)
	    start_of_char = start_of_char->next;
	  /* does not get here if the character was a newline, tab, or
	     special character.
	     This is correct. */ 
	  o_complex_set_color(start_of_char, color);	
	  o_scale(toplevel, start_of_char, size/2, size/2);
	  
	  /* do this if you want to stack chars */
	  /* we don't want to do that for now */
	  /* Rotate and translate the character to its world position */
	  o_list_rotate_world(toplevel, 0, 0, angle, start_of_char);
	  o_list_translate_world(toplevel, x_offset, y_offset, start_of_char);
	  
	  /* Reset the escapes counter after being processed. Otherwise, 
	     if the next character is also a backslash, it will
	     be processed as if were also escaped */
	  if (previous_char == '\\' && c == '\\') {
	    escapes_counter = 0;
	  }
	} 
      }
      
      /* Now check for special characters */
      
      if ( (c == '_' && previous_char == '\\') || 
	   (c == '\n' && overbar_started) || 
	   (previous_char == '\n' && overbar_started) ||
	   (overbar_started && next_char == 0) ) { 
	/* Found the overbar delimiter sequence 
	   If the character is the newline and the overbar was started,
	   then end it and start it again in the next character, after
	   processing the newline. */
	overbar_height_offset = char_height + (char_height >> 2);
	if ( (!overbar_started) ||
	     (overbar_started && previous_char == '\n') ){
	  /* Start point of the overbar */
	  overbar_started = TRUE;
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
	} else {
	  /* Then this is the end point of the overbar */
	  if (overbar_started && next_char == 0 &&
	      !(c == '_' && previous_char == '\\')) {
	    /* Instead of ending in the last character, end the overbar
	       after the current character (its width is 
	       size/2*o_font_set->font_text_size */
	    last_char_width = -size/2*o_font_set->font_text_size;
	  }
	  switch (angle) {
	    case 0:
	      overbar_endx = x_offset - last_char_width;
	      overbar_endy = y_offset + overbar_height_offset;
	      break;
	    case 90:
	      overbar_endx = x_offset - overbar_height_offset;
	      overbar_endy = y_offset - last_char_width;
	      break;
	    case 180:
	      overbar_endx = x_offset + last_char_width;
	      overbar_endy = y_offset - overbar_height_offset;
	      break;
	    case 270:
	      overbar_endx = x_offset + overbar_height_offset;
	      overbar_endy = y_offset + last_char_width;
	      break;
	    default:
	      fprintf(stderr, "o_text_create_string: Angle not supported\n");
	      break;
	  }
	  /* Now add the overbar (if it is not a zero length overbar) */
	  if ( (overbar_startx != overbar_endx) ||
	       (overbar_starty != overbar_endy) ) {
	    temp_list = o_line_add(toplevel, temp_list, OBJ_LINE, color,
				   overbar_startx, overbar_starty,
				   overbar_endx, overbar_endy);
	  }

	  if (!((c == '\n') && (overbar_started))) {
	    /* If it's the newline character, keep the overbar started, since
	       we have to end this one & start another one in the next line */
	    overbar_started = FALSE;
	  }
	}
	if (c != '\n')
	  continue;
      }
     
      /* If the character is a newline or tab, this code will "continue" */
      switch (c) {
      case '\n':
	/* The character is a newline. Calcule the start of the next line */
        switch (angle) {
            case 0:
              x_offset = line_start_x;
              y_offset = line_start_y - char_height * LINE_SPACING;
              line_start_x = x_offset;
              line_start_y = y_offset;
              continue;	
              break;
            case 90:
              x_offset = line_start_x + char_height * LINE_SPACING;
              y_offset = line_start_y;
              line_start_x = x_offset;
              line_start_y = y_offset;
              continue;	
              break;
            case 180:
              x_offset = line_start_x;
              y_offset = line_start_y + char_height * LINE_SPACING;
              line_start_x = x_offset;
              line_start_y = y_offset;
              continue;	
              break;
            case 270:
              x_offset = line_start_x - char_height * LINE_SPACING;
              y_offset = line_start_y;
              line_start_x = x_offset;
              line_start_y = y_offset;
              continue;	
              break;
            default:
              fprintf(stderr, "o_text_create_string: Angle not supported\n");
              break;
        }
          case '\t':
#if DEBUG
            printf("Found tab character.\n");
            printf("Tab size in coord: %i\n", size_of_tab_in_coord);
            printf("Line start: %i,%i\n", line_start_x, line_start_y);
            printf("Position: %i, %i\n", x_offset, y_offset);
#endif
            switch (angle) {
                case 0:
                case 180:
                  rel_char_coord = x_offset - line_start_x;
#if DEBUG
                  printf("Add: %i\n", (size_of_tab_in_coord - (rel_char_coord % size_of_tab_in_coord)));
#endif
                  x_offset += (size_of_tab_in_coord - (rel_char_coord % size_of_tab_in_coord));
                  continue;	
                  break;
                case 90:
                  rel_char_coord = y_offset - line_start_y;
#if DEBUG
                  printf("Add: %i\n", (size_of_tab_in_coord - (rel_char_coord % size_of_tab_in_coord)));
#endif
                  y_offset += (size_of_tab_in_coord - (rel_char_coord % size_of_tab_in_coord));
                  continue;	
                  break;
                case 270:
                  rel_char_coord = line_start_y - y_offset;
#if DEBUG
                  printf("Add: %i\n", (size_of_tab_in_coord - (rel_char_coord % size_of_tab_in_coord)));
#endif
                  y_offset -= (size_of_tab_in_coord - (rel_char_coord % size_of_tab_in_coord));
                  continue;	
                  break;
                default:
                  fprintf(stderr, "o_text_create_string: Angle not supported\n");
                  break;
            }
      }      
    }

    /* Calcule the position of the next character */
    switch(angle) {
      case(0):	
        x_offset = (x_offset) + 
          size/2*o_font_set->font_text_size;
        break;
		
      case(90):
        y_offset = (y_offset) + 
          size/2*o_font_set->font_text_size;
        break;

      case(180):
        x_offset = (x_offset) - 
          size/2*o_font_set->font_text_size;
        break;
		
      case(270):
        y_offset = (y_offset) - 
          size/2*o_font_set->font_text_size;
        break;
    }
  }

  /* don't set the head */	

  toplevel->page_current->object_tail = temp_tail;

#if DEBUG
  printf("2 %d %d\n", x_offset, y_offset);
#endif
  return(object_list);
}

/*! \todo Finish function documentation!!!
 *  \brief Creates a text OBJECT and the graphical objects representing it
 *  \par Function Description
 *  Create an OBJECT of type OBJ_TEXT and link it to the end of object_list.
 *  Also add the OBJECTs forming the graphical representation of the visible
 *  string, to the text OBJECT's prim_objs list.
 *
 *  \param [in]  toplevel              The TOPLEVEL object.
 *  \param [in]  object_list            OBJECT list onto which to add text.
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
OBJECT *o_text_add(TOPLEVEL *toplevel, OBJECT *object_list,
		   char type, int color, int x, int y, int alignment,
		   int angle, char *string, int size, 
		   int visibility, int show_name_value)
{
  OBJECT *new_node=NULL;
  OBJECT *temp_list=NULL;
  OBJECT *temp_parent=NULL;
  TEXT *text;
  char *name = NULL;
  char *value = NULL; 
  char *output_string = NULL;

  if (string == NULL) {
    return(NULL);
  }

  new_node = s_basic_init_object("text");
  new_node->type = type;

  text = (TEXT *) g_malloc(sizeof(TEXT));

  text->string = g_strdup (string);
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

  /* create the object in the main list */
  /* object_list points to the object */
  /* I use it below as a sanity check to make sure it was linked */
  /* properly */ 
  object_list = (OBJECT *) s_basic_link_object(new_node, object_list);

  /* fix up actual string here */ 
  if (o_attrib_get_name_value(string, &name, &value)) {

    switch(show_name_value) {
      case(SHOW_NAME_VALUE):
        output_string = g_strdup(string);
        break;

      case(SHOW_NAME):
        if (name[0] != '\0') {
          output_string = g_strdup(name);
        } else {
          /* you probably can remove this now... */
          /* since improper attributes will never get here */
          fprintf(stderr, 
                  "Got an improper attribute: %s\n", 
                  string);
          output_string = g_strdup("invalid");

        }
        break;

      case(SHOW_VALUE):
        if (value[0] != '\0') {
          output_string = g_strdup (value);
        } else {
          /* you probably can remove this now... */
          /* since improper attributes will never get here */
          fprintf(stderr, 
                  "Got an improper attribute: %s\n", 
                  string);
          output_string = g_strdup("invalid");
        }
        break;
    }
  } else {
    output_string = g_strdup(string);
  }


  /* now start working on the complex */
  temp_list = o_text_add_head();

  temp_parent = toplevel->page_current->object_parent;
  /* set the addition of attributes to the head node */
  toplevel->page_current->object_parent = temp_list;

  if (visibility == VISIBLE ||
      (visibility == INVISIBLE && toplevel->show_hidden_text)) {
    object_list->text->prim_objs = 
      o_text_create_string(toplevel, temp_list,
                           output_string, size, color,
                           x, y, alignment, angle); 
    object_list->text->displayed_width = o_text_width(toplevel,
                                                      output_string, size/2);
    object_list->text->displayed_height = o_text_height(output_string, size);
  } else {
    object_list->text->prim_objs = NULL;
    object_list->text->displayed_width = 0;
    object_list->text->displayed_height = 0;
    s_delete(toplevel, temp_list);
  }

  toplevel->page_current->object_parent = temp_parent;

  /* Update bounding box */
  o_text_recalc( toplevel, object_list );

  g_free(name);
  g_free(value);
  g_free(output_string);
  return(object_list);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
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
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
OBJECT *o_text_read(TOPLEVEL *toplevel, OBJECT *object_list,
		    const char *first_line,
		    TextBuffer *tb,
		    unsigned int release_ver,
		    unsigned int fileformat_ver)
{
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

  if (fileformat_ver == 1) {
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
    s_log_message(_("Setting color to WHITE\n"));
    color = WHITE;
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
  
  object_list = o_text_add(toplevel, object_list, type, color, x, y,
                           alignment, angle, string, 
                           size, visibility, show_name_value);
  g_free(string);

  return(object_list);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
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

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
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

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_text_recreate(TOPLEVEL *toplevel, OBJECT *o_current)
{
  OBJECT *temp_parent;
  char *name = NULL;
  char *value = NULL;
  char *output_string = NULL;

  if (o_attrib_get_name_value(o_current->text->string, &name, &value)) {
    switch(o_current->show_name_value) {
      case(SHOW_NAME_VALUE):
        output_string = g_strdup(o_current->text->string);
        break;

      case(SHOW_NAME):
        if (name[0] != '\0') {
          output_string = g_strdup(name);
        } else {
          /* you probably can remove this now... */
          /* since improper attributes will never get here */
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

  o_list_delete_rest(toplevel, o_current->text->prim_objs);

  temp_parent = toplevel->page_current->object_parent;
  /* set the addition of attributes to the head node */
  toplevel->page_current->object_parent = o_current->text->prim_objs;

  if (o_current->visibility == VISIBLE ||
      (o_current->visibility == INVISIBLE && toplevel->show_hidden_text)) {

    /* need to create that head node if complex is null */
    if (o_current->text->prim_objs == NULL) {
      o_current->text->prim_objs = o_text_add_head();
    }

    o_current->text->prim_objs = 
      o_text_create_string(toplevel,
                           o_current->text->prim_objs, 
                           output_string, 
                           o_current->text->size, 
                           o_current->color, 
                           o_current->text->x, 
                           o_current->text->y,
                           o_current->text->alignment,
                           o_current->text->angle); 

    o_complex_set_saved_color_only(o_current->text->prim_objs, 
                                   o_current->saved_color);
    o_current->text->displayed_width = o_text_width(toplevel,
                                                    output_string,
                                                    o_current->text->size/2);
    o_current->text->displayed_height = o_text_height(output_string,
                                                      o_current->text->size);
  } else {
    /* make sure list is truely free */
    s_delete_list_fromstart(toplevel, o_current->text->prim_objs);
    o_current->text->prim_objs = NULL;
    o_current->text->displayed_width = 0;
    o_current->text->displayed_height = 0;
  }

  o_text_recalc( toplevel, o_current );

  toplevel->page_current->object_parent = temp_parent;
  g_free(name);
  g_free(value);
  g_free(output_string);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_text_translate_world(TOPLEVEL *toplevel,
                            int x1, int y1, OBJECT *o_current)
{
  o_current->text->x = o_current->text->x + x1;
  o_current->text->y = o_current->text->y + y1;

  o_list_translate_world(toplevel, x1, y1, o_current->text->prim_objs);

  /* Update bounding box */
  o_text_recalc( toplevel, o_current );
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
OBJECT *o_text_copy(TOPLEVEL *toplevel, OBJECT *list_tail, OBJECT *o_current)
{
  OBJECT *new_obj;
  int color;

  if (o_current->saved_color == -1) {
    color = o_current->color;
  } else {
    color = o_current->saved_color;
  }

  new_obj = o_text_add(toplevel, list_tail, OBJ_TEXT,
                       color, 
                       o_current->text->x, o_current->text->y, 
                       o_current->text->alignment, 
                       o_current->text->angle,
                       o_current->text->string, 
                       o_current->text->size, 
                       o_current->visibility, 
                       o_current->show_name_value); 

  return(new_obj);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
static gboolean delete_font_set (gpointer key, gpointer value,
				 gpointer user_data)
{
  OBJECT *tmp = (OBJECT*)value;
  TOPLEVEL *toplevel = (TOPLEVEL*)user_data;

  if (tmp != NULL) {
    if (tmp->font_prim_objs != NULL) {
      s_delete_list_fromstart (toplevel, tmp->font_prim_objs);
      tmp->font_prim_objs = NULL;
    }
    /* do not use s_delete() as tmp is not fully initialized */
    g_free (tmp->name);

    /* Do not free tmp here since it will be freed with the function */
    /* that was specified when the hash table was created. */
  }

  return TRUE;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
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

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_text_print_text_width(FILE *fp, char *output_string)
{
  int len, i, j;
  int starting_character = 0;
  int num_lines, line_number;
  int max_len = -1;
  char *single_line = NULL;
  char *max_length_line = NULL;
  int single_len;

  /* break up the string and find the longest string */
  num_lines = o_text_num_lines(output_string);
  single_line = g_strdup (output_string);  /* larger than needed */
  len = strlen(output_string);
  for (line_number = 0; line_number < num_lines; line_number++)
  {
    j = 0;
    /* break up the string into lines */
    for (i = starting_character; i < len; i++)
    {
      if (output_string[i] != '\n' && output_string[i] != '\0')
      {
        single_line[j] = output_string[i];
      }
      else
      {
        starting_character = i + 1;
        break;
      }
      j++;
    }
    single_line[j] = '\0';

    single_len = strlen(single_line);
    if (single_len > max_len)
    {
      max_len = strlen(single_line);
      g_free(max_length_line);
      max_length_line = g_strdup (single_line);
    }
  }


  fprintf(fp, "(");
  len = strlen(max_length_line);
  for (i = 0 ; i < len; i++) {  
    if (max_length_line[i] == '(' || max_length_line[i] == ')' || max_length_line[i] == '\\' ) {
      fprintf(fp, "\\");
    }

    fprintf(fp, "%c", max_length_line[i]);
  }

  /* convert width to mils */
  /* .95 is a fudge factor */
  fprintf(fp, ") stringwidth pop\n");

  g_free(single_line);
  g_free(max_length_line);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_text_print_text_height(FILE *fp, int size)
{
  fprintf(fp, "%f\n", (float) size);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_text_print_text_height_full(FILE *fp, char *string, int size)
{
  int num_lines = o_text_num_lines(string);
  fprintf(fp, "%f\n", (float) (size*num_lines + 
          size * LINE_SPACING * (num_lines - 1)));
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
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
    fprintf(fp, "%c", current_char);
    aux = g_utf8_find_next_char(aux, NULL);
  }

  fprintf(fp,") ");
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
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

  if (toplevel->print_color) {
    f_print_set_color(fp, o_current->color);
  }


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


/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
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


/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
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

