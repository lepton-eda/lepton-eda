#!/usr/bin/perl

##  Library version 0.94
##  This program is released under the terms of the GNU GPL
##  See www.fsf.org for a copy of the license
##
##  Copyright 1999 Matthew Ettus
##  For more info, email matt@ettus.com
##
##  This program will convert ORCAD text libraries to gEDA components
##
##  Changes 0.94 by <egil@kvaleberg.no>, october 5th 2002
##      Implement CLK modifier
##      Implement negation for text ('\'-character)
##      Doubled size to better fit with gEDA symbols
##      Default to U? refdes, fixed refdes and made visible
##      Define sarlacc_dim attribute for sarlacc_scheme
##      Command line options
##
##  Changes 0.93 by <egil@kvaleberg.no>, october 4th 2002
##      Pin attribs modified for 20020825 convention
##      File format to 20020825 convention
##      Give duplicate components correct names
##      Implemented BITMAP
##      Implemented CONVERT
##      Implemented multiple slot component
##      Better syntax check for pin defs
##      Removed negative coordinates by adding an offset
##      Got rid of need to calculate text sizes
##      Got the ARC right
##      Graphics components has invisible pin labels
##      Implemented pintype PWR
##      Implement pintypes (IN, I/O, hiZ, OUT, PAS, OC, OE)
##
##  Todo
##      Implement symbols like THREE_STATE
##      Implement FILL (?)
##

$version = "0.94";
$gedaversion="20020825";

#
# colors
#
$pin_color = 1;         # white
$graphic_color = 3;     # green
$attribute_color = 5;   # yellow
$text_color = 9;        # green

#
# symbol scaling
#

# was 6:
$pinnumsize = 8;
$partnamesize = 10;
# was 100:
$scale = 200;

$pinlen = 3;
$xoffset = $pinlen * $scale;
$yoffset = $pinlen * $scale;

#
# character width table (approximate)
#
@char_width=(
     0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     11,14,14,22,28,28,24,10,12,12,16,20,14,20,12,18,
     26,16,26,20,26,20,24,20,22,26,12,12,20,20,20,20,
     36,29,29,28,26,29,21,30,29,9,21,27,22,32,28,32,
     25,32,26,25,23,27,27,37,27,25,27,12,14,12,20,25,
     10,24,22,22,20,20,12,24,20,8,10,19,9,32,20,20,
     20,24,14,18,12,19,20,28,19,20,21,12,10,12,22,0,
     0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     0,0,0,0,29,29,0,0,0,0,0,0,0,0,0,0,
     0,0,0,0,0,0,32,0,0,0,0,0,0,0,0,0,
     0,0,0,0,24,24,0,0,0,0,0,0,0,0,0,0,
     0,0,0,0,0,0,20,0,0,0,0,0,0,0,0,0
);                                           

sub string_len 
{ 
    $char_points = 2;
    $width = 0;
    @expanded = unpack "C*",$_[0];
    foreach $char (@expanded)
    {
	$width += $char_width[$char];
    }
    $width = ($_[1]*$width)/$char_points;
    return $width;
}         

#
#  draw a pin
#  $x1
#  $x2
#  $dx
#  $dy
#
sub draw_pin
{
	local $sx,$sy;
	local $x2,$y2;
	$sx = 0;
	$sx = 1 if ($dx > 0);
	$sx = -1 if ($dx < 0);
	$sy = 0;
	$sy = 1 if ($dy > 0);
	$sy = -1 if ($dy < 0);

	$x2 = $x1 + $sx * $len;
	$y2 = $y1 + $sy * $len;

	if( $is_clk )
	{
		local $xa,$ya,$xb,$xb;

		$xa = $x1 - $sy * $spacing;
		$xb = $x1 - $sx * $spacing;
		$ya = $y1 - $sx * $spacing;
		$yb = $y1 - $sy * $spacing;
		print COMPONENT "L $xa $ya $xb $yb $graphic_color".
				   " 0 0 0 -1 -1\n";
		$xa = $x1 + $sy * $spacing;
		$ya = $y1 + $sx * $spacing;
		print COMPONENT "L $xa $ya $xb $yb $graphic_color".
				   " 0 0 0 -1 -1\n";
	}

	if ( not ($pinlabel =~ /\$/) && not $is_graphic)
	{
		# negation?
		if ( $pinlabel =~ /\\/ )
		{
			local $w;
			$pinlabel =~ tr/\\//d;
			$w = string_len($pinlabel,$pinnumsize);

			if ($w > 0) {
				local $xa,$ya,$xb,$xb;

				$xa = $x1 + abs($sy) * $spacing
					  - $sx * $spacing;
				$xb = $x1 + abs($sy) * $spacing
					  - $sx * ($spacing + $w);
				$ya = $y1 + abs($sx) * $spacing
					  - $sy * $spacing;
				$yb = $y1 + abs($sx) * $spacing
					  - $sy * ($spacing + $w);
				print COMPONENT "L $xa $ya $xb $yb $text_color".
					" 0 0 0 -1 -1\n";
			}
		}
	}

	if( $dotsize )
	{
		local $rad;

		$rad = $dotsize / 2;
		$x1 += $sx * $rad;
		$y1 += $sy * $rad;
		print COMPONENT "V $x1 $y1 $rad $pin_color ",
				   " 0 0 0 -1 -1 0 -1 -1 -1 -1 -1\n";
		$x1 += $sx * $rad;
		$y1 += $sy * $rad;
	}

	print COMPONENT "P $x1 $y1 $x2 $y2 $pin_color\n";
}

#
#  define a pin
#  $pinseq
#  $place
#  $pinnumbers
#  $pintype
#  $pinlabel
#  $slots
#  $is_graphic
#
sub def_pin
{
	if ($pinnumbers =~ /^(.*)[ \t]+([A-Za-z]+)[ \t]*$/ )
	{
		$pinnumbers = $1;
		$modifier = $2;
	}
	else
	{
		$modifier = "";
	}
	chomp $pinnumbers;

	$len = $pinlen * $scale;
	$visibility = 1;
	$dotsize = 0;
	$is_clk = 0;
	if ($modifier eq "DOT")
	{
		$dotsize = $scale / 2;
	}
	elsif ($modifier eq "SHORT")
	{
		$len = $scale;
	}
	elsif ($modifier eq "CLK")
	{
		$is_clk = 1;
	}
	elsif ($modifier ne "")
	{
		print "Modifier not supported: $modifier\n";
	}

	$is_pwr = 0;
	if ($pintype eq "PWR")
	{
		$is_pwr = 1;
		$visibility = 0;
	}
	elsif (($a = $pintype_assoc{$pintype}) ne "")
	{
		$pintype = $a;
	}
	elsif ($pintype ne "")
	{
		## BAD others are: tp clk pwr
		print "Pintype not supported: $pintype\n";
	}

	# by gEDA convention
	$pinlabel = "Vcc" if ($pinlabel eq "VCC");

	#
	#  extract the pin numbers
	#
	if ($slots > 1)
	{
		$length = @array = split(" ",$pinnumbers);
		if ($length != $slots)
		{
			print "Error: incorrect number of pins $length, should be $slots\n";
			exit;
		}
		if (not $is_pwr)
		{
			$def_slot[$pinseq] = [ @array ];
		}
		$pinnumber = $array[0];
	}
	else
	{
		$pinnumber = $pinnumbers;
	}
	$pinnumber =~ tr/ //d;
	$pinnumber =~ tr/\'//d;
	$pinnumalign = 0;
	$pinnumangle = 0;
	$pinlabelalign = 0;
	$pinlabelangle = 0;

	#
	#  place the label
	#
	$letter = $num = $place;
	$letter =~ tr/0-9//d;
	$num =~ tr/LRTB//d;
	$spacing = $scale / 2;

	if ($letter eq "L")
	{
		$x1 = $xoffset;
		$y1 = $yoffset + $ysize - $num*$scale;
		$dx = -$len;
		$dy = 0;
		$textx = $xoffset - $spacing;
		$texty = $y1 + $scale/10;
		$namex = $xoffset + $spacing;
		$namey = $y1;
		$pinlabelalign = 1;
		$pinnumalign = 6;
	}
	elsif ($letter eq "R")
	{
		$x1 = $xoffset + $xsize;
		$y1 = $yoffset + $ysize - $num*$scale;
		$dx = $len;
		$dy = 0;
		$textx = $xoffset + $xsize + $spacing;
		$texty = $y1 + $scale/10;
		$namex = $xoffset + $xsize - $spacing;
		$namey = $y1;
		$pinlabelalign = 7;
	}
	elsif ($letter eq "T")
	{
		$x1 = $xoffset + $num*$scale;
		$y1 = $yoffset + $ysize;
		$dx = 0;
		$dy = $len;
		$textx = $x1 - $scale/10;
		$texty = $yoffset + $ysize + $spacing;
		$namex = $x1;
		$namey = $yoffset + $ysize - $spacing;
		$pinnumangle = $pinlabelangle = 90;
		$pinlabelalign = 7;
	}
	elsif ($letter eq "B")
	{
		$y1 = $yoffset;
		$x1 = $xoffset + $num*$scale;
		$dx = 0;
		$dy = -$len;
		$textx = $x1 - $scale/10;
		$texty = $yoffset - $spacing;
		$namex = $x1;
		$namey = $yoffset + $spacing;
		$pinnumangle = $pinlabelangle = 90;
		$pinlabelalign = 1;
		$pinnumalign = 6;
	}
	else
	{
		print "Cannot happen: $letter\n";
		exit;
	}

	if( $is_pwr )
	{
		if ($pinnumber ne "")
		{
			print COMPONENT "T $textx $texty $attribute_color $pinnumsize 0 0 0 0\n";
			print COMPONENT "net=$pinlabel:$pinnumber\n";
		}
	}
	else
	{
		draw_pin();

		++$pinseq;

		print COMPONENT "{\n";

		# *EK* use pinnumber= and pinseq= for 20020825
		if ($pinnumber ne "")
		{
			print COMPONENT "T $textx $texty $attribute_color $pinnumsize $visibility 1 $pinnumangle $pinnumalign\n";
			print COMPONENT "pinnumber=$pinnumber\n";
		}
		print COMPONENT "T $textx $texty $attribute_color $pinnumsize 0 0 0 0\n";
		print COMPONENT "pinseq=$pinseq\n";
		print COMPONENT "T $textx $texty $attribute_color $pinnumsize 0 0 0 0\n";
		print COMPONENT "pintype=$pintype\n";

		if ( not ($pinlabel =~ /\$/) )
		{
			# do not show label if graphic package
			$visibility = 0 if ($is_graphic);

			print COMPONENT "T $namex $namey $text_color ",
			"$pinnumsize $visibility 1 $pinlabelangle $pinlabelalign\n";
			print COMPONENT "pinlabel=$pinlabel\n";
		}
		print COMPONENT "}\n";
	}
}

#
#  define all pins
#  $is_graphic
#
sub def_all_pins
{
	$pinseq = 0;

	#
	#  define the pins
	#
	for ($pin = 0; $pin < $pins; ++$pin)
	{
		$place      = $def_place[$pin];
		$pinnumbers = $def_pinnumbers[$pin];
		$pintype    = $def_pintype[$pin];
		$pinlabel   = $def_pinlabel[$pin];

		def_pin();
	}
}

#
#  next component
#  $component
#  $namelist
#  $slots
#
sub def_component
{
	open(COMPONENT,">$component-$convert.sym");
	print COMPONENT "v $gedaversion\n";

	print COMPONENT "T $scale 0 $attribute_color $partnamesize 1 1 0 0\n";
	print COMPONENT "refdes=$refdes?\n";

	# make displayed name also an attribute
	print COMPONENT "T $scale $scale $attribute_color $partnamesize 1 1 0 0\n";
	print COMPONENT "partno=$component\n";
	print COMPONENT "T $scale $scale $attribute_color $partnamesize 0 1 0 0\n";
	print COMPONENT "device=$component\n";


	if ( $slots > 1 )
	{
		print COMPONENT "T $scale $scale $attribute_color $partnamesize 0 0 0 0\n";
		print COMPONENT "slot=1\n";
		print COMPONENT "T $scale $scale $attribute_color $partnamesize 0 0 0 0\n";
		print COMPONENT "numslots=$slots\n";
	}
	else
	{
		print COMPONENT "T $scale $scale $attribute_color $partnamesize 0 0 0 0\n";
		print COMPONENT "numslots=0\n";
	}

	$pins = 0;

	#
	#  scan the pins
	#
	while (($line = <LIBRARY>) =~
	    /(^[LRTB][0-9]+)(.*)[ \t]+([A-Za-z\/]+)[ \t]+'(.*)'/ )
	#      L1            3 DOT     I/O               'OUTPUT'
	{
		$def_place[$pins]        = $1;
		$def_pinnumbers[$pins]   = $2;
		$def_pintype[$pins]      = $3;
		$def_pinlabel[$pins]     = $4;
		++$pins;
	}

	#
	#  define the body
	#

	#skip Orcad bitmaps
	while ($line =~ /\{/)
	{
		$line = <LIBRARY>;
	}

	if ( $line =~ /BITMAP[ \t]+'(.*)'/ )
	{
		$is_graphic = 1;
		def_all_pins();

		# re-use previous vector image
		$def_body = $body{$1};

		$line = <LIBRARY>;
	}
	elsif (( $line = <LIBRARY>) =~ /VECTOR/ )
	{
		$is_graphic = 1;
		def_all_pins();

		$def_body="";
		while (not (( $line = <LIBRARY>) =~ /END/))
		{
			if ($line =~ /LINE/)
			{
				($dummy,$x1,$y1,$x2,$y2)=
					split(" ",$line);
				$x1 = $xoffset + $x1*$scale;
				$y1 = $yoffset + $ysize - $y1*$scale;
				$x2 = $xoffset + $x2*$scale;
				$y2 = $yoffset + $ysize - $y2*$scale;
				$def_body .= "L $x1 $y1 $x2 $y2 $graphic_color".
					   " 0 0 0 -1 -1\n";
			}
			elsif($line =~ /TEXT/)
			{
				# BAD set textsize properly , check multi-line text
				($dummy,$x1,$y1,$textsize,$text)=split(" ",$line);
				$x1 = $xoffset + $x1*$scale;
				$y1 = $yoffset + $ysize - $y1*$scale;

				# print "Text=$text\n";
				if ($text eq "OPEN_L")
				{
					# BAD ignore
				}
				elsif ($text eq "OPEN_H")
				{
					# BAD ignore
				}
				elsif ($text eq "THREE_STATE")
				{
					# BAD ignore
				}
				elsif ($text !~ /'/)
				{
					print "Symbol not supported: $text\n";
				}
				else 
				{
					$text=~ tr/\'//d;
					$textsize *= $pinnumsize;
					$def_body .= "T $x1 $y1 $graphic_color $textsize 1 0 0 0\n".
						   "$text\n";
				}
			}
			elsif ($line =~ /CIRCLE/)
			{
				($dummy,$x1,$y1,$radius) = split(" ",$line);
				$x1 = $xoffset + $x1*$scale;
				$y1 = $yoffset + $ysize - $y1*$scale;
				$radius *= $scale;
				$def_body .= "V $x1 $y1 $radius $graphic_color".
					   " 0 0 0 -1 -1 0 -1 -1 -1 -1 -1\n";
			}
			elsif ($line =~ /ARC/)
			{
				($dummy,$x1,$y1,$dx1,$dy1,$dx2,$dy2,$rad) =
							split(" ",$line);
				$x1 = $xoffset + $x1*$scale;
				$y1 = $yoffset + $ysize - $y1*$scale;
				$factor = 180 / atan2(1,1) / 4;
				$startangle = int ( $factor * atan2 (-$dy1, $dx1));
				$endangle = int ( $factor * atan2 (-$dy2, $dx2));
				$sweepangle = $endangle - $startangle;
				# Orcad brain damage: ARC is always < 180 deg
				if ($sweepangle > 180) {
					$sweepangle -= 360;
				}
				if ($sweepangle < -180) {
					$sweepangle += 360;
				}
				$rad *= $scale;
				$def_body .= "A $x1 $y1 $rad".
					" $startangle $sweepangle $graphic_color".
					" 0 0 0 -1 -1\n";
			}
			elsif ($line =~ /FILL/)
			{
				# BAD Ignore FILL
			}
			else
			{
				print "Unrecognized tag:\n $line \n";
				exit;
			}
		}

		$line = <LIBRARY>;
	}
	else
	{
		$is_graphic = 0;
		def_all_pins();

		$x1 = $xoffset;
		$y1 = $yoffset;

		$def_body = "B $x1 $y1 $xsize $ysize $graphic_color".
			  " 0 0 0 -1 -1 0 -1 -1 -1 -1 -1\n";

	}

	#
	#  define slots, if any
	#
	if ($slots > 1)
	{
		for ($s = 0; $s < $slots; ++$s)
		{
			print COMPONENT "T $scale $scale $attribute_color $partnamesize 0 0 0 0\n";
			print COMPONENT "slotdef=",$s+1;
			$c = ":";

			for ($n = 0; $n < $pinseq; ++$n)
			{
				$pin = $def_slot[$n][$s];
				print COMPONENT "$c$pin";
				$c = ",";
			}
			print COMPONENT "\n";
		}
	}

	#
	#  emit text for body
	#
	print COMPONENT "$def_body";
	$body{$component} = "$def_body";

	#
	# for sarlacc_schem
	#
	print COMPONENT "T $scale $scale $attribute_color $partnamesize 0 0 0 0\n";
	print COMPONENT "sarlacc_dim=$xoffset,$yoffset,$xsize,$ysize\n";

	close(COMPONENT);

	#
	# make copies for multiple component names.
	# device= changed to suit
	#
	foreach $duplicate (@namelist)
	{
		# make duplicate component, remembering to change device name
		`cat $component-1.sym                   |
		 sed "s/partno=$component/partno=$duplicate/" |
		 sed "s/device=$component/device=$duplicate/" |
		 cat >$duplicate-1.sym`;
	}
}

#
#  main loop
#  arg: library name
#
sub main_loop
{
    local $library = $_[0];

    open(LIBRARY,$library ) or die "Can't open $library:  $!\n";

    $line = <LIBRARY>;
    if ( not ($line =~ /Compiled/ ))
    {
	    print $line;
	    print "Not a library file: $library\n";
	    die;
    }

    while (not (<LIBRARY> =~ /PREFIX/)){};
    while (not (<LIBRARY> =~ /END/)){};

    $line = <LIBRARY>;

    while (not (eof LIBRARY))
    {
	    if ($line =~ /\'/)
	    {
		    $component = $line;
		    $component =~ s/\'//g;
		    chomp $component;
		    $component =~ s/\r//g;
		    $component =~ s/\ /_/g;
		    $convert = 1;
		    $refdes="U";

		    # Handle components with multiple names....
		    @namelist = ();
		    while (($line = <LIBRARY>) =~ /^\'/)
		    {
			    $line =~ s/\'//g;
			    chomp $line;
			    $line =~ s/\r//g;
			    $line =~ s/\ /_/g;
			    push @namelist ,( $line);
		    }

		    if ($line =~ /REFERENCE/)
		    {
			    ($dummy,$refdes) = split("\'",$line);
			    $line = <LIBRARY>;
		    }

		    ($dummy,$x,$y,$slots) = split("=}",$line);
		    ($xsize) = split(" ",$x);
		    ($ysize) = split(" ",$y);
		    $xsize *= $scale;
		    $ysize *= $scale;

		    chomp $slots;
		    $slots =~ tr/ //d;

		    # print "Define: $component\n";

		    def_component();
	    }
	    elsif ( $line =~ /CONVERT/ )
	    {
		   $convert += 1;
		   def_component();
	    }
	    else
	    {
		   $line = <LIBRARY>;
	    }
    }
    close(LIBRARY);
}

#
#  main
#

# pin types for Orcad to gEDA
$pintype_assoc{'PWR'} = "pwr";
$pintype_assoc{'IN'}  = "in";
$pintype_assoc{'I/O'} = "io";
$pintype_assoc{'OUT'} = "out";
$pintype_assoc{'hiZ'} = "tri";
$pintype_assoc{'PAS'} = "pas";
$pintype_assoc{'OC'}  = "oc";
$pintype_assoc{'OE'}  = "oe";

$argc = @ARGV;

for ($argn = 0; $argn < @ARGV; ++$argn)
{
	$arg = $ARGV[$argn];
	if ($arg =~ /^-([A-Za-z?])(.*)$/ ) {
		if ($1 eq "s") {
			$scale = $2;
		} elsif ($1 eq "v") {
			print "sarlacc_sym ver $version\n";
		} else {
			print "Convert Oracd symbol library (text format) to gEDA\n";
			print "\nUsage:   sarlacc_sym [options] library",
			   "\nOptions:",
			 # "\n         -d<dir>  directory for symbols",
			   "\n         -h       help",
			   "\n         -s<n>    scale <n>%, default is $scale",
			   "\n         -v       version",
			   "\n\n";
			die;
		}
	} else {
		main_loop($arg);
	}
}








