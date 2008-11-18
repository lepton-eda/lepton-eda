#
# Copyright (C) 2008 Carlos Nieves Onega
# Copyright (C) 2008 other contributors
#                        (see ChangeLog or SCM history for details)
 
# This file is part of gxyrs.

# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 2
# of the License, or (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  
# 02110-1301, USA.

use strict;
use warnings;

package gxyrs;

BEGIN {
	use Exporter   ();
        our ($VERSION, @ISA, @EXPORT, @EXPORT_OK, %EXPORT_TAGS);

        # if using RCS/CVS, this may be preferred
        $VERSION = do { my @r = (q$Revision: 2.21 $ =~ /\d+/g); sprintf "%d."."%02d" x $#r, @r }; # must be all one line, for MakeMaker
        @ISA         = qw(Exporter);
        @EXPORT      = qw(&check_columns &del_line &rotate_comp &subst_col_val &change_col_units &add_number_to_col &translate_col_val @LINE &panelize_xyrs &mul_col_val &gxyrs_get_xy_transform_matrix &gxyrs_apply_xy_transform &swap_columns &insert_column);
        %EXPORT_TAGS = ( );     # eg: TAG => [ qw!name1 name2! ],
        # your exported package globals go here,
        # as well as any optionally exported functions
        @EXPORT_OK   = qw(@LINE $CASE_INSENSITIVE);
}

use vars qw(
        $CASE_INSENSITIVE
);

our @LINE ;# = ( "Package", "Filters" );
#our $CASE_INSENSITIVE;

# Checks if a column number is a number between the specified range.
# 
# Returns 1 if success, 0 otherwise
# If error, prints an error with the function caller (passed as argument)
# to STDERR
sub check_column_number {
    my ($f_name,$number, $min, $max) = @_;

    if ($number !~ /^[0-9]+$/ ) {
	print STDERR "Error: $f_name: wrong column argument: $number.\n";
	return 0;
    }
    # Column number must be a number within line columns number
    if (($number > $max) ||
	($number < $min)) { 
	print STDERR "Error: $f_name: Element number ($number) is not within ($min..$max).\n";
	return 0;
    }
    return 1;
}
	
# This function checks if the data has the pattern specified by the checklist.
# The data to be checked is in the global variable $LINE;
# The check list is a list of elements. Each element is a list with two single elements specifying
# the data number 'n' and the pattern. The element 'nth' of the data list should pass the pattern.
# The first element of the data has the number 1.
# The functions returns 1 if the data passes all the checks and 0 otherwise.
#
# Returns -1 if error, 0 if not match, 1 if match.
#   if no argument is given, returns 1.
#
# Example: check_columns 2, '[0-9]+', 3, 'R[0-9]+';
#   column 2 should be a number, and 
#   column 3 is a string beginning with R followed by a number.
sub check_columns {
	my(@check_list) = @_;   # Check list. Each pair is (column, pattern)
	
	# Pair list expected. The parameter number should be odd.
	if ( (@LINE == 0) || (@check_list & 1 != 0) )  { return 0 };

	for (my $i=0; $i <= @check_list-1; $i+=2) {
	  my($number, $pattern) = ($check_list[$i], $check_list[$i+1]);
	  # Column number must be a number
	  if ($number !~ /^[0-9]+$/ ) {
	  	print STDERR "Error: check_columns: wrong column argument: $number.\n";
	  	return -1;
	  }
	  # Column number must be a number within line columns number
	  if (($number > @LINE) ||
	     ($number <= 0)) { 
	     print STDERR "Error: Check pattern: Element number ($number) is not within (1..".(@LINE).").\n";
	     return -1;
	  }
	  $number-=1; 

	  if (! defined $CASE_INSENSITIVE ) {
	  	print STDERR "Error: Check pattern: variable CASE_INSENSITIVE should be defined.\n";
	  	print STDERR "Changing it to 0 (default).\n";
	  	$CASE_INSENSITIVE=0;
	  }
	  if ($CASE_INSENSITIVE !~ /^[0,1]$/) {
	  	print STDERR "Error: Check pattern: variable CASE_INSENSITIVE ($CASE_INSENSITIVE) should be 0 or 1.\n";
	  	print STDERR "Changing it to 0 (default).\n";
	  	$CASE_INSENSITIVE=0;
	  }
	  # Build the pattern, adding beginning and end marks.
	  $pattern='^'.$pattern.'$';

	  # Test the pattern
	  if ($CASE_INSENSITIVE == 1) {
	      if (! ($LINE[$number] =~ /$pattern/i)) {
		  return 0; # Found one non-matching test. Return FALSE.
	      }
	  }
	  else {
	      if (! ($LINE[$number] =~ /$pattern/)) {
		  return 0; # Found one non-matching test. Return FALSE.
	      }
	  }
	}
	return 1;
}


# Delete the line (global variable @LINE) if it matches the pattern specified.
# It uses check_columns for pattern matching, and if it matches, 
# then the line is deleted.
#
# Returns -1 if error, 0 if not match, 1 if match and changed.
#
# Example: del 2, 'R[0-9]+';
#   delete all lines having a R followed by a number in column number 2.
sub del_line {
	my (@check_list) = @_;
	my $rc = check_columns (@check_list);
	if ($rc == 1) {
		splice(@LINE);
		return 1;
	} else {
	    return $rc;
	}
}

# Rotate the component if it matches the pattern specified.
# It uses check_columns for pattern matching, and if it matches, 
# then the component is rotated.
#
# Returns 1 if match and changed, 0 if not match, -1 if error.
#
# Example: rotate 5, 90, 2, 'R[0-9]+';
#   rotate all components having a R followed by a number in column number 2,
#   by 90º. Rotation angle is in column 5.
sub rotate_comp {
	my ($angle_col, $angle, @check_list) = @_;
	my $rc;

	if (! (check_column_number("rotate_comp",$angle_col, 0, scalar(@LINE)))) {
	    return -1;
	}

	if ($angle !~ /^[+-]??[0-9]*(\.[0-9]+)*$/ ) {
		# Angle doesn't match the expression "number.number", where 
		# ".number" may not exist.
		print STDERR "Error: rotate_comp: wrong angle: $angle.\n";
		return -1;
	}
	$rc = check_columns (@check_list);
	if ($rc < 0) {
	    return $rc;
	}
	if ($rc) {
		$LINE[$angle_col-1] += $angle;
		$LINE[$angle_col-1] -= 360*int($LINE[$angle_col-1] / 360) ;
		return 1;
	}
	return 0;
}

# Replace a column value if it matches the pattern specified.
# It uses check_columns for pattern matching, and if it matches, 
# then the column is replaced.
#
# Returns 1 if match and changed, 0 if not match, -1 if error.
#
# Example: subst 3 , '100nF', 3, '0.1u';
#   if the value in column 3 is "0.1u", then replace it with "100nF".
#
# Example: subst 2 , 'D1', 3, '1n4148';
#   if the value in column 3 is "1n4148", then replace column 2 with "D1".
sub subst_col_val {
	my ($col, $value, @check_list) = @_;
	my $rc;

	if (! (check_column_number("subst_col_val",$col, 0, scalar(@LINE)))) {
	    return -1;
	}

	$rc = check_columns (@check_list);
	if ($rc < 0) {
	    return $rc;
	}
	if ($rc) {
		$LINE[$col-1] = $value;
		return 1;
	}
	return 0;
}

# Change units of a given text.
# Returns:
#   1 if match and changed, 
#   0 if not match, 
#  -1 if units not supported or wrong format.
#  -2 if string to change has no units.
#
# Example: change_text_units 'mm', '102.40mil';
#    replaces string '102.40mil' by the its equivalent in mm: '2.60096mm'
#
sub change_text_units {
    my($return_value) = 0;
    my($desired_units, $string) = @_; # $_[1] MUST be the string to be changed. See below.

	# If the text has no units, warn the user.
	if ($string =~ /^[+-]??[0-9]*(\.[0-9]*)*$/) {
	    print STDERR "Warning: change_text_units: Text ".$string." has no units. Leaving it as is.\n";
	    return -2;
	}

	# If it's only text, then warn the user.
	if ($string !~ /^[+-]??[0-9]/) {
	    print STDERR "Warning: change_text_units: Text $string has no numbers!\n";
	    return -2;
	}

	# If it is already in the desired units, don't change it.
	if ($string =~ /^[+-]??[0-9]*(\.[0-9]*)*$desired_units$/) {
	    return 0;
	}
	if ($desired_units =~ /^mm$/) {
	    # The desired units are mm.
	    if ($string =~ /^[+-]??[0-9]*(\.[0-9]*)*[A-Za-z]*$/) {
		# The format is N.Nunits, where N is a number. 
		if ($string =~ /(.*)mil$/i) {
		    # The original units are mils (1/1000th inches).
		    $string = ($1*0.0254)."mm";
		    $_[1] = $string;
		    return 1;
		}
		elsif ($string =~ /(.*)in$/i) {
		    # The original units are inches.
		    $string = ($1*25.4)."mm";
		    $_[1] = $string;
		    return 1;
		}
		else {
		    # Units are not supported.
		    $_=$string;
		    m/([A-Za-z]*)$/;
		    print STDERR "Changing $string to mm: Units $1 not supported.\n";
		    print STDERR "Line: @LINE\n";
		    return -1;
		}
	    }
	    else {
		# Wrong format. It is not N.Nunits, where N is a number.
		print STDERR "Error: change_text_units: Wrong format: $string.\n";
	        return -1;
	    }
	}
	elsif ($desired_units =~ /^mil$/) {
	    # The desired units are mils (1/1000th inches).
	    if ($string =~ /^[+-]??[0-9]*(\.[0-9]*)*[A-Za-z]*$/) {
		# The format is N.Nunits, where N is a number. 
		if ($string =~ /(.*)mm$/i) {
		    # The original units are mils (1/1000th inches).
		    $string = ($1/0.0254)."mil";
		    $_[1] = $string;
		    return 1;
		}
		elsif ($string =~ /(.*)in$/i) {
		    # The original units are inches.
		    $string = ($1*1000)."mm";
		    $_[1] = $string;
		    return 1;
		}
		else {
		    # Units are not supported.
		    $_=$string;
		    m/([A-Za-z]*)$/;
		    print STDERR "Changing to mils: Units $1 not supported.\n";
		    print STDERR "Line: @LINE\n";
		    return -1;
		}
	    }
	    else {
		# Wrong format. It is not N.Nunits, where N is a number.
		print STDERR "Error: change_units: Wrong format: $string.\n";
	        return -1;
	    }
	}
	else {
	    # Desired units are not supported.
	    print STDERR "Error: Desired units '$desired_units' not supported.\n";
	    return -1;
	}
}


# Change units of a given column number.
# Returns -1 if error, -2 if warning, and 1 if success.
# 
# Example: change_units 'mm', 3, 5;
#    convert all numbers in column 3 and 5 to its equivalent in mm.
#
sub change_col_units {
    my($return_value) = 0;
    my($value);
    my($changed) = 0;
    my($desired_units, @columns) = @_;

    $return_value = 0;
    if (@columns == 0) {return 0};
    if (@LINE == 0) {return 0};
    
    foreach $value (@columns) {

	if (! (check_column_number("change_col_units",$value, 0, scalar(@LINE)))) {
	    return -1;
	}

	# If the column is empty, continue.
	if ($LINE[$value-1] =~ /^\s*$/) {next};
	
	# Change the units
	$changed = change_text_units($desired_units, $LINE[$value-1]);
	if ($changed == -1) {
		print STDERR "Error at column $value\n";
	}
	elsif ($changed == -2) {
		print STDERR "Warning at column $value\n";
	}
	
	# Update return value
	if ( ($changed == 1) && ($return_value == 0) ) {
		$return_value++;
	}
	elsif ( ($changed == -2) && ($changed != -1) ) {
		$return_value = -2;
	}
	elsif ($changed == -1) {
		$return_value = -1;	
	}
    }
    return $return_value;
}


# Adds a number to a value in a given column number.
# Note: offset and the value to be changed can be in different units.
# Returns -1 if error, -2 if warning, and 1 if success.
# 
# Example: offset 3, '102.5mm', 4, 'R[0-9]+' ;
#    if the text in column number 4 is R followed by a number, then adds
#    102.5mm to the value in column 3. 
#    Note: Value in column 3 can be in other units. For example: '640mil'.
# 
sub add_number_to_col {
    my($return_value) = 0;
    my($changed) = 0;
    my($column, $offset, @check_list) = @_;
    my($offset_value) = 0;
    my($units);
    my $rc;

    $return_value = 0;
    
    if (! (check_column_number("add_number_to_col",$column, 0, scalar(@LINE)))) {
	return -1;
    }

    if (@LINE == 0) {return 0};

    $rc = check_columns (@check_list);
    if ($rc < 0) {
	return $rc;
    }
    if ($rc) {
	# If the column is empty, continue.
	if ($LINE[$column-1] =~ /^\s*$/) {next};
	
	# Guess the line's units.
	if ($LINE[$column-1] =~ /^[+-]??[0-9]*(\.[0-9]*)*[a-zA-Z]+$/) {
	    if ($LINE[$column-1] =~ /^.*[0-9]+([a-zA-Z]+)$/) {
		$units = $1;	
	    }
	}
	elsif ($LINE[$column-1] =~ /^[+-]??[0-9]*(\.[0-9]*)*$/) {
	    # Line number has no units.
	    $units = "";

	    if ($offset =~ /^[+-]??[0-9]*(\.[0-9]*)*[a-zA-Z]+$/) {
		print STDERR "Error: add_number_to_col: Offset has units, but number $LINE[$column-1] has no units.\n";
		return -1;
	    }
	}
	else {
		print STDERR "Error: add_number_to_col: Wrong value at column $column: $LINE[$column-1]\n";
		return -1;
	}
	
	# Change offset to line's units.
	if (! $units =~ /^$/) {
	    $changed = change_text_units($units, $offset);
	}
	else {
	    $changed = 1;
	}
	
	# If there's an error changing the offset units, then return.
	if ($changed == -1 ) {
		print STDERR "Error: add_number_to_col: Error with offset parameter: $offset.\n";
		return -1;
	}
	elsif ($changed == -2) {
		print STDERR "Warning: add_number_to_col: warning in offset parameter: $offset.\n";
	}
	
	# Change the units
	$_=$offset;
	/(.*)$units$/i;
	$offset_value = $1;

	$_=$LINE[$column-1];
	/(.*)$units$/i;
	$LINE[$column-1] = $1+$offset_value."$units";	
	
	# Update return value
	if ( ($changed == 1) && ($return_value == 0) ) {
		$return_value++;
	}
	elsif ( ($changed == -2) && ($changed != -1) ) {
		$return_value = -2;
	}
	elsif ($changed == -1) {
		$return_value = -1;	
	}
    }
    return $return_value;
}

# Translate a string if it matches the pattern specified.
# It uses check_columns for pattern matching, and if it matches, 
# then the column is replaced.
#
# Returns -1 if error, 0 if not match, 1 if match and changed.
#
# Example:
#   translate 2, '^([0-9]+)n$','sprintf("%dnF",$1)', 3, 'C[0-9]+';
#     if the value in column 3 is C followed by a number, then 
#     if the value in column 2 is a number followed by 'n', translate it
#     to the same value followed by 'nF'.
#     If there is 'C10' in column 3 and '10n' in column 2, 
#     change '10n' to '10nF'.
sub translate_col_val {
	my($column, $string, $substitution, @checklist) = @_;
	my $rc;

	if (! (check_column_number("translate_col_val",$column, 0, scalar(@LINE)))) {
	    return -1;
	}
	if (@LINE == 0) {return 0};

	if ( (! @checklist) || (@checklist)) {
	    $rc = check_columns (@checklist);
	    if ($rc <= 0) {
		return $rc;
	    }
	    else {
		if ($CASE_INSENSITIVE == 1) {
		    if ($LINE[$column-1] =~ s/$string/$substitution/ixee) {
			return 1;
		    } else {
			return 0;
		    }
		}
		else {
		    if ($LINE[$column-1] =~ s/$string/$substitution/xee) {
			return 1;
		    } else {
			return 0;
		    }
		}
	    }
	}
	return 0;
}

# Panelize the list.
# 
# Warning: this should ALWAYS be the last command in your script.
sub panelize_xyrs {
    my ($no_x, $no_y, $AX, $AY, $X_COL, $Y_COL, $REF_COL, $REF_PATTERN) = @_;
    my ($i, $j);
    my @new_array = ();
    my $refdes;

    if (! (check_column_number("panelize_xyrs", $X_COL, 0, scalar(@LINE)) &&
	   check_column_number("panelize_xyrs", $Y_COL, 0, scalar(@LINE)) &&
	   check_column_number("panelize_xyrs", $REF_COL, 0, scalar(@LINE)) )) {
	return -1;
    }

    $refdes = $LINE[$REF_COL-1];

    for ($i=0; $i<=$no_x-1; $i++) {
 	for ($j=0; $j<=$no_y-1; $j++) {
 	    my @new_line = @LINE;
	    
	    
	    if (length($REF_PATTERN) == 0) {
		$new_line[$REF_COL-1] = $refdes."-$i-$j";
	    }
	    else {
		$new_line[$REF_COL-1] = $refdes.$REF_PATTERN;		
	    }
	    
	    if (($i == 0) && ($j == 0)) {
		$LINE[$REF_COL-1] = $new_line[$REF_COL-1];
		next;
	    }
	    
 	    $new_line[$X_COL-1] = $LINE[$X_COL-1]+$i*$AX;
 	    $new_line[$Y_COL-1] = $LINE[$Y_COL-1]+$j*$AY;
	    
 	    unshift(@new_line, "\n");
 	    push (@new_array, @new_line);
	    
 	}
    }
    push (@LINE, @new_array);
    return 1;
}

# Multiply the number in the specified column by the given amount.
#
# It uses check_columns for pattern matching, and if it matches, 
# then the column is replaced.
# The number to be multiplied may have units at the end.
#
# Returns -1 if error, 0 if not match, 1 if match and changed.
#
# Example: multiply 3, 2.5, 4, 'R[0-9]+' ;
#    if the text in column number 4 is R followed by a number, then multiply
#    the value in column 3 by 2.5. 
#    Note: Value in column 3 may have units or not. For example: '640mil'.

sub mul_col_val {
    my($string_format)='^([+-]??[0-9]*(\.[0-9]*)*)([a-zA-Z]*)$';
    my($column, $factor, @checklist) = @_;
    my $rc;
    
    
    if (! (check_column_number("mul_col_val", $column, 0, scalar(@LINE)))) {
	return -1;
    }
    
    if ($factor !~ /^[+-]??[0-9]*(\.[0-9]*)*$/) {
	print STDERR "Error: mul_col_val: Multiplying factor is not a number.\n";
	return -1;
    }
    
    $rc = check_columns (@checklist);
    
    if ($rc < 0) {
	return $rc;
    }
    if ($rc) {	    
	if ($LINE[$column-1] =~ /$string_format/) {
	    return translate_col_val $column, $string_format,'sprintf("%f%s", $1*'.($factor).',$3)';
	}
	else {
	    print STDERR "Error: mul_col_val: bad number parameter ($LINE[$column-1]) passed to multiply function.\n";
	    return -1;
	}
    }
   
    return 0;
}

# Swap two columns
#
# Given two column numbers, this function swaps the contents of the columns.
#
# Returns -1 if error, 0 if not match, 1 if match and changed.
#
# Example: swap_columns 3, 4, 4, 'R[0-9]+' ;
#    if the text in column number 4 is R followed by a number, then
#    swap columns 3 and 4.
sub swap_columns {
    my($column1, $column2, @checklist) = @_;
    my $rc;

    if (! (check_column_number("swap_columns", $column1, 0, scalar(@LINE)))) {
	return -1;
    }
    if (! (check_column_number("swap_columns", $column2, 0, scalar(@LINE)))) {
	return -1;
    }
    $rc = check_columns (@checklist);
    
    if ($rc < 0) {
	return $rc;
    }
    if ($rc) {	    
	my $temp;

	$temp = $LINE[$column1-1];
	$LINE[$column1-1]=$LINE[$column2-1];
	$LINE[$column2-1]=$temp;

	return 1;
    }
   
    return 0;
}

# Insert a column with the given text in the given position
#
# Insert a column in the given position (0 if it's going to be the 
# first column), with the given value.
#
# Returns -1 if error, 0 if not match, 1 if match and changed.
#
# Example: insert_column 0, 'new_column_value';
#    insert a column in the first position. Column value is 'new_column_value'
sub insert_column {
    my($column1, $value) = @_;

    if (! (check_column_number("insert_column", $column1, 0, scalar(@LINE)))) {
	return -1;
    }

    splice @LINE,$column1,$#LINE-$column1,($value,@LINE[$column1..($#LINE+-1)]);

    return 1;
}

END { }

1;
