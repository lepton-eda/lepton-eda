#!/usr/bin/perl
#
# This is a perl script to create a character width table from gEDA's 
#   font .sym files.
#
# MK_CHAR_TAB typical use: 
#  ./mk_char_tab.pl
#     will produce the char width table file: char_width.c
#
  for ($i = 0; $i <= 256; $i++)    # clear the width table
      {
      @width_table[$i] = 0;
      }
  while (defined($file = <../lib/sym/font/*.sym>)) # search the directory for *.sym files
        {
        open(INFO,"<$file");
        @lines = <INFO>;
        close(INFO);

        foreach $line (@lines)       # search for the F type line
              {
              @tokens = split (/\s+/,$line);  # parse the line
              $type   = @tokens[0];
	      if ($type eq "F")
	         {
                 $char_value = ord(@tokens[1]);
                 $char_width = @tokens[2];
                 $is_space = @tokens[3];  # is this the space char?
	         if ($is_space eq "1")
		    {
		    $char_value = 32;	  # if so do fix-up
		    }
                 @width_table[$char_value] = $char_width;
#                 print "$file: @tokens, $type $char_value $char_width $is_space\n";
		 }
              }
	}
#
#  Now we'll build the C file from the width table
#
  $FileToWrite = './char_width.c';
  open(C_file, ">$FileToWrite");

  print C_file "\n#define CHAR_POINTS 2\n\n";
  print C_file "const int char_width[]={\n     ";

  for ($i = 0; $i < 256; $i++)
      {
      print C_file "@width_table[$i]";	# add the char width to the table
      if ($i == 255)			# end of table?
         {
         print C_file "\n};\n" ;
	 }
      elsif (( $i % 16) == 15 )         # end of line?
         {
         print C_file ",\n     ";
	 }
      else
          {
          print C_file ",";
	  }
      }
#
#  Add in the basic string to pixs function
#
  print C_file "\n";
  print C_file "/***************************************************************/\n";
  print C_file "/* GetStringDisplayLength:		                       */\n";
  print C_file "/* inputs: string to be sized 				       */\n";
  print C_file "/*         string\'s font size to use                           */\n";
  print C_file "/* returns: length of string in gEDA points                    */\n";
  print C_file "/***************************************************************/\n";
  print C_file "int GetStringDisplayLength(char *str,int font_size)\n";
  print C_file "{ int width=0;\n";
  print C_file "  int i, len;\n";
  print C_file "  len = strlen(str);\n";
  print C_file "  for (i=0;i<len;i++)\n";
  print C_file "      width += char_width[(int)str[i]];\n";
  print C_file "  width = (font_size*width)/CHAR_POINTS;\n";
  print C_file "  return width;\n";
  print C_file "}\n";
  print C_file "\n";

  close(C_file);

#
# we're done
#
