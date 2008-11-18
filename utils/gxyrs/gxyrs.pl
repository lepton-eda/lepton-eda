#!/usr/bin/perl -w
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

use lib 'GEDARCDIR/gxyrs'; # Where gxyrs package is located

use gxyrs;
use gxyrs qw($CASE_INSENSITIVE);

# for parsing input options
use Getopt::Long;

use vars qw {
	$REF_COL $FOOTPRINT_COL $X_COL $Y_COL $ANGLE_COL
        $LAYER_COL $VALUE_COL $CASE_INSENSITIVE $LINE_NUMBER
        $LINE $TITLE_LINE
};

my $tempfile;
		

# Set these for debugging purposes
my $DEBUG = 0;
my $DEBUG_GUESSING = 0;
my $DEBUG_COL_LENGTH = 0;
my $DEBUG_RETURN_CODE = 0;

# Initialize global variables
$REF_COL = -1;
$FOOTPRINT_COL = -1;
$X_COL = -1;
$Y_COL = -1;
$ANGLE_COL = -1;
$LAYER_COL = -1;
$VALUE_COL = -1;

#######################################################################
#
# Subroutines
#
#######################################################################


#---------------------------------
# guess_file_format()
#
# Check current line and try to guess column numbers by 
# testing if it is the title line and there are some title keywords.
#---------------------------------
sub guess_file_format () {
    my $num_elems = scalar(@LINE);
    my @positions = ();
    my $position_repeated=0;

    if ($DEBUG_GUESSING == 1) {
	print "guess_file_format processing line $LINE_NUMBER.\n";
	print "Line: @LINE\n";
	print "Number of elements in line: $num_elems.\n";
    }

    # Remove '#' and spacer characters from the beginning of first element
    while ($LINE[0] =~ /^#/) {
	$LINE[0] = substr($LINE[0], 1);
    }
    while ($LINE[0] =~ /^\s+/) {
	$LINE[0] = substr($LINE[0], 1);
    }
    

    # Now try to guess column numbers from column titles.
    for (my $i = 0; $i <= $num_elems-1; $i++) {
	if ($DEBUG_GUESSING == 1) {
	    print "Testing element $i: ".$LINE[$i].".\n";
	}
	if ( ($LINE[$i] =~ /^Designator$/) ||
	     ($LINE[$i] =~ /^RefDesignator$/) ||
	     ($LINE[$i] =~ /^RefDes$/)
	    ) {
	    if ($DEBUG_GUESSING) {
		print "Found reference column: ".($i+1).".\n";
	    }
	    $REF_COL = $i+1;
	    if (grep(/\b$REF_COL\b/, @positions)) {
		$position_repeated = 1;
	    }
	    push(@positions,$REF_COL);
	}
	if ( ($LINE[$i] =~ /^Footprint$/) ||
	     ($LINE[$i] =~ /^TopCell$/) ||
	     ( ($LINE[$i] =~ /^Description$/) && 
	       (! grep(/\bTopCell\b/,@LINE)) )
	    ) {
	    if ($DEBUG_GUESSING) {
		print "Found footprint column: ".($i+1).".\n";
	    }
	    $FOOTPRINT_COL = $i+1;
	    if (grep(/\b$FOOTPRINT_COL\b/, @positions)) {
		$position_repeated = 1;
	    }
	    push(@positions,$FOOTPRINT_COL);
	}
	if ( ($LINE[$i] =~ /^Mid X$/) ||
	     ($LINE[$i] =~ /^X$/)
	    ) {
	    if ($DEBUG_GUESSING) {
		print "Found X column: ".($i+1).".\n";
	    }
	    $X_COL = $i+1;
	}
	if ( ($LINE[$i] =~ /^Mid Y$/) ||
	     ($LINE[$i] =~ /^Y$/)
	    ) {
	    if ($DEBUG_GUESSING) {
		print "Found Y column: ".($i+1).".\n";
	    }
	    $Y_COL = $i+1;
	    if (grep(/\b$Y_COL\b/, @positions)) {
		$position_repeated = 1;
	    }
	    push(@positions,$Y_COL);
	}
	if ( ($LINE[$i] =~ /^[Rr]otation$/) ||
	     ($LINE[$i] =~ /^Rot$/) 
	     
	    ) {
	    if ($DEBUG_GUESSING) {
		print "Found angle column: ".($i+1).".\n";
	    }
	    $ANGLE_COL = $i+1;
	    if (grep(/\b$ANGLE_COL\b/, @positions)) {
		$position_repeated = 1;
	    }
	    push(@positions,$ANGLE_COL);
	}
	if ( ($LINE[$i] =~ /^TB$/) ||
	     ($LINE[$i] =~ /^Side$/) ||
	     ($LINE[$i] =~ /^top\/bottom$/)
	    ) {
	    if ($DEBUG_GUESSING) {
		print "Found layer column: ".($i+1).".\n";
	    }
	    $LAYER_COL = $i+1;
	    if (grep(/\b$LAYER_COL\b/, @positions)) {
		$position_repeated = 1;
	    }
	    push(@positions,$LAYER_COL);
	}
	if ( ($LINE[$i] =~ /^Comment$/) ||
	     ($LINE[$i] =~ /^PartNumber$/) ||
	     ($LINE[$i] =~ /^Value$/)
	    ) {
	    if ($DEBUG_GUESSING) {
		print "Found value column: ".($i+1).".\n";
	    }
	    $VALUE_COL = $i+1; 	
	    if (grep(/\b$VALUE_COL\b/, @positions)) {
		$position_repeated = 1;
	    }
	    push(@positions,$VALUE_COL);
	}
    }

    # If there is any repeated position, warn the user.
    if ($position_repeated == 1) {
	print STDERR "A position was repeated while guessing file format.\n";
    }
    # If not all columns were guessed, or a position is repeated then reset all.
    if ( ($position_repeated == 1) ||
	 ($REF_COL == -1) ||
	 ($FOOTPRINT_COL == -1) ||
	 ($X_COL == -1) ||
	 ($Y_COL == -1) ||
	 ($ANGLE_COL == -1) ||
	 ($LAYER_COL == -1) ||
	 ($VALUE_COL == -1)
	) {

	# All columns should be defined in the same line.
	$REF_COL = -1;
	$FOOTPRINT_COL = -1;
	$X_COL = -1;
	$Y_COL = -1;
	$ANGLE_COL = -1;
	$LAYER_COL = -1;
	$VALUE_COL = -1;
    }

    if ($DEBUG_GUESSING == 1) {
	print "guess_file_format ended processing line $LINE_NUMBER.\n";
    }

}

#---------------------------------
# stdin()
#
# open a temp file, to allow multiple passes to the input file.
#---------------------------------
sub stdin () {
        $tempfile = 'temp_'.time() . '_' . int(rand(1000000));
        open(TEMP, "+>$tempfile") or print STDERR $!;
        local $/ = undef;
        print TEMP <STDIN>;
        seek(TEMP, 0, 0);
        \*TEMP;
}


#---------------------------------
# usage()
#
# prints program usage
#---------------------------------
sub usage {
    print "Usage:\n";
    print "  gxyrs [--tabulate] <input_file> --adjust <adjust_file> --output <outputfile> \n";
    exit(0);
}


#######################################################################
#
# Main program
#
#######################################################################


{

    my @fname=();
    my ($file_in, $adjust_file, $file_out);
    my ($this_line, $line_out);
    my @lengths;
    my $output_delimiter="";

    # Delimiters to try when guessing file format.
    my @delimiters = ('\s+', ',\s*'); 
    my @delimiters_char = (' ', ','); # Used when writing the output file
    my $delimiter_index=-1; 
    
    my $pass;
    my $total_passes = scalar(@delimiters) + 1;
    my $guessing_file_format;
    my $is_comment = 0; # Mark the current line as a comment.
    my $eval_string;

    my $tabulate=0; # Output should be tabulated or not.
    my $verbose;
    my $process_comments;

    my $return_code;

    # Comparisons are case insensitive by default.
    $CASE_INSENSITIVE=0;

    &GetOptions(("help" => \&usage, 
		 "verbose" => \$verbose,
		 "process-comments" => \$process_comments,
		 "tabulate!" => \$tabulate,
		 "caseinsensitive!" => \$CASE_INSENSITIVE,
		 "adjust=s" => \$adjust_file,
		 "eval=s" => \$eval_string,
		 "output=s" => \$file_out,
		 "output-delimiter=s" => \$output_delimiter,
		));

    if ($Getopt::Long::error) {
	print STDERR "Wrong arguments.\n";
	usage();
    }
    
    # Print usage string in event user didn't call out any args
    usage() unless @ARGV;
    
    # Make sure the input schematic exists and we can read it.
    # Also count number of files to open.
    while(@ARGV) {
	my $i=0;
	$fname[$i]=shift(@ARGV);
	print "Checking argument $fname[$i].\n";
 	if (! -r $fname[$i]) {  
 	    die("Input file $fname[$i] does not exist or can not be read");
 	}
	$i++;
    }
    
    $file_in = $fname[0];
	
    # Print usage string in event user didn't specify input, adjust 
    # and output files.
    if (! ($file_in && $file_out && ($adjust_file || $eval_string))) {
	print STDERR "Nothing to do.\n";
	if ($eval_string) {
	    print STDERR "Eval string: $eval_string.\n";
	}
	if ($adjust_file) {
	    print STDERR "Commands file: $adjust_file.\n";
	}
	usage();
    }
    

    if ($file_in !~ '^-$') {
	open(FILE_IN, $file_in) || die ("Can't open input file $file_in: $!");
    }
    else {
	*FILE_IN = &stdin();	
    }

    if (length($output_delimiter) > 1) {
	die ("Output delimiter $output_delimiter must be a character.");
    }

    # Don't know yet the file format.
    $guessing_file_format = 1;
    $TITLE_LINE = 0;

    # Last pass is to process the file. Others are for guessing the
    # file format with several delimiters (one for each element in @delimiters.
    for ($pass = 1; $pass <= $total_passes; $pass++) {
	if (seek(FILE_IN, 0, 0) != 1) {
	    print STDERR "Error when seek input file to 0\n";
	}
	if ($pass >= $total_passes) {
	    open(FILE_OUT, "> ".$file_out) || die ("Can't open output file $file_out: $!");
	}
	
	# Set the return code to 0
	$return_code = 0;
	if ($DEBUG_RETURN_CODE) {
	    print "Setting global return code to 0.\n";
	}

	# Parse file
	$LINE_NUMBER = 0;
	print "Pass number: $pass.\n" if ($DEBUG);
	while ( <FILE_IN> ) {
	    $this_line=$_;
	    $_ = $this_line;

	    # Remove end line characters.
	    chomp($this_line);

	    # For the last pass, use the delimiter guessed.
	    # For the others, get the delimiter from @delimiters array.
	    if ($pass < $total_passes) {
		my $this_delimiter = $delimiters[$pass-1];
		@LINE = split ($this_delimiter, $this_line);
	    } else {
		my $this_delimiter = $delimiters[$delimiter_index];
		@LINE = split ($this_delimiter, $this_line);
	    }
	    $LINE_NUMBER += 1;
	    next if /^@/;
	    next if /^q/;
	    
	    # Test if the line is a comment 
	    # (which shouldn't be processed).
	    if ( ($this_line =~ /^\s*#/) ||
		 ($this_line =~ /^\s*$/) ) {
		if ($DEBUG) {
		    print "Line is comment: @LINE\n";
		}
		$is_comment = 1;
	    } else {
		$is_comment = 0;
	    }
	    
	    # First, some column titles are separated by spaces, so join them.
	    if ($LINE_NUMBER == 1) {
		my $num_elems = scalar(@LINE);
		for (my $i = 0; $i <= $num_elems-1; $i++) {
		    if ($i <= $num_elems-2) {
			# Join titles like "Pad X", "Pad Y", "Mid X", "Mid Y",
			# "ref X", "ref Y" (Protel output)
			if ( ( ($LINE[$i] =~ /^Pad$/) &&
			       ($LINE[$i+1] =~/^[XY]$/) ) ||
			     ( ($LINE[$i] =~ /^Mid$/) &&
			       ($LINE[$i+1] =~/^[XY]$/) ) ||
			     ( ($LINE[$i] =~ /^Ref$/) &&
			       ($LINE[$i+1] =~/^[XY]$/) ) 
			    ) {
			    $LINE[$i] = $LINE[$i]." ".$LINE[$i+1];
			    for (my $j = $i+1; $j <= $num_elems-2; $j++) {
				$LINE[$j] = $LINE[$j+1];
			    }
			    delete $LINE[$num_elems-1];
			    $num_elems -= 1;
			}
		    }
		}
		
	    }
	    
	    if ( ($pass != $total_passes) &&
		 $guessing_file_format ) {
		my $old_guessing_file_format = $guessing_file_format;
		# Try to guess the file format
		guess_file_format();
		# Check if already guessed the file format.
		$guessing_file_format = ( ($REF_COL == -1) ||
					  ($FOOTPRINT_COL == -1) ||
					  ($X_COL == -1) ||
					  ($Y_COL == -1) ||
					  ($ANGLE_COL == -1) ||
					  ($LAYER_COL == -1) ||
					  ($VALUE_COL == -1) );
		
		if (!$guessing_file_format) {
		    if ($old_guessing_file_format == 1) {
			$TITLE_LINE = $LINE_NUMBER;
		    }
		    if ($verbose) {
			print "Found file format. ";
			print "Delimiter is $delimiters[$pass-1].\n";
		    }
		    
		    $delimiter_index = $pass-1;
		}
	    }
	    
	    if (!$guessing_file_format) {
		if (($is_comment == 0) || $process_comments) {
		    my $rc ;

		    # If it's parsing the file then keep the number
		    # of columns constant, joining all the fields 
		    # in the last one.
 		    while ( (@LINE > @lengths) &&
			    (scalar(@lengths) > 0) ) {
 			$LINE[@LINE-2] = $LINE[@LINE-2].
			    $delimiters_char[$delimiter_index].$LINE[@LINE-1];
 			pop @LINE;
 		    }

		    if ($DEBUG_RETURN_CODE) {
			print "Global return code before evaluation: $return_code.\n";
		    }

		    # Run the adjust file.
		    if (defined $adjust_file) {
			$rc = do $adjust_file || 
			    die("Can't use adjust file: $!\n");
		    } else {
			#print "Eval: $eval_string\n";
			$rc = eval $eval_string;
		    }
		    if ($@) {
			print STDERR "Error: ".$@."\n";
			$rc = -10;
		    }
		    
		    # Set $return_code based on priorities.
		    # 1 means the command found a match and was succesful 
		    #   at least one time in the whole file.
		    # 0 means there was no match.
		    # -1 means there was an error.
		    # -2 means there was a warning.
		    if ($DEBUG_RETURN_CODE) {
			print "Return code $rc after evaluation.\n";
		    }
		    if ($rc == -1) {
			$return_code = -1;
		    }
		    elsif (($rc == -2) && ($return_code >= 0)) {
			$return_code = -2;
		    }
		    elsif (($rc == 1) && ($return_code == 0)) {
			$return_code = 1;
		    }
		    elsif (($rc == 0) && ($return_code == 1)) {
			$return_code = 1;
		    }
		    elsif (($rc >= 0) && ($return_code < 0)) {
			# Do nothing
		    }
		    else {
			$return_code = $rc;
		    }
		    if ($DEBUG_RETURN_CODE) {
			print "Global return code: $return_code.\n";
		    }
		}	
		if ( ($pass != $total_passes) &&
		     ( ($is_comment == 0) || $process_comments) ) {
		    if ($DEBUG_COL_LENGTH == 1) {
			print "Measuring column length of line: ".
			    "'$this_line'\n"; 
		    }
		    # Calcule max length array
		    if ($tabulate) {
			my $j=0;
			for (my $i=0; $i <= scalar(@LINE)-1; $i++) {
			    # Allow adding more lines, and checking
			    # element's length right.
			    if ($LINE[$i] eq "\n") {
				$j = $i+1;
				next;
			    }
			    if (! exists $lengths[$i-$j]) {
				$lengths[$i-$j]= length($LINE[$i]);
			    }
			    elsif ($lengths[$i-$j] < length($LINE[$i]) ) {
				$lengths[$i-$j]= length($LINE[$i]);
			    }
			    if ($DEBUG_COL_LENGTH == 1) {
				print "Column ".($i-$j+1).
				    ", length: $lengths[$i].\n"; 
			    }
			}
		    }
		}
		
		# Print the result after processing
		if ($pass >= $total_passes) {
		    if (($is_comment == 1) && !$process_comments) {
			print FILE_OUT "$this_line\n";
		    }
		    elsif (@LINE > 0) {
			my $delimiter;
			my $j=0;
			
			# Set the output delimiter 
			if (length($output_delimiter) != 0) {
			    $delimiter = $output_delimiter;
			}
			else {
			    $delimiter = $delimiters_char[$delimiter_index];
			}

			# Write output
			for (my $i=0; $i <= scalar(@LINE)-1; $i++) {
			    # Allow adding more lines, and handle
			    # new lines.
			    if ($LINE[$i] eq "\n") {
				print FILE_OUT "\n";
				$j = $i+1;
				next;
			    }
			    if (($i-$j) > scalar(@lengths)-1) {
				# If column length array has no number 
				# for this column, print it as is.
				print FILE_OUT $LINE[$i];
				} else {
				    printf FILE_OUT "%-$lengths[$i-$j]s",$LINE[$i];
				}
			    # print the last column without delimiter
			    if ( ($i < @LINE-1) &&
				 (!($LINE[$i+1] eq "\n"))) {
				    print FILE_OUT $delimiter;
			    }
			}
			print FILE_OUT "\n";
		    }
		}
	    }
	    
	}	
	# Close output file.
	if ($pass >= $total_passes) {
	    close(FILE_OUT);
	}
	
	# Print all column numbers (DEBUGGING)
	if ( ($pass != $total_passes) && ($DEBUG_GUESSING==1) ) {
	    print "Reference column: $REF_COL.\n";
	    print "Footprint column: $FOOTPRINT_COL.\n";
	    print "X column:         $X_COL.\n";
	    print "Y column:         $Y_COL.\n";
	    print "Rotation column:  $ANGLE_COL.\n";
	    print "Layer column:     $LAYER_COL.\n";
	    print "Value column:     $VALUE_COL.\n";
	}

	# If last guessing pass is complete, but all columns were not guessed, 
	# exit with an error.
	if ( $guessing_file_format &&
	     ($pass == $total_passes-1) )
	{
	    die ("Can't guess file format.\n");
	}

	# If file format was guessed, but next pass is not the last one,
	# then go to the last one (processing pass).
	if ( (!$guessing_file_format) &&
	     ($pass < $total_passes-1) ) {
	    $pass = $total_passes-1;
	}
    }        
    close(FILE_IN);
    if ($tempfile) {
	unlink $tempfile;
    }

    exit $return_code;
}

