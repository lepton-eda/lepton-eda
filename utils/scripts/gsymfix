#!/usr/bin/perl -w

my $version = "0.8";

# This script reads a geda symbol and does the following:
# (...see the usage routine below...)

# for parsing input options
use Getopt::Long;

# don't allow -he to be interpreted as --help
$Getopt::Long::autoabbrev=0;

&GetOptions(("help" => \&usage, 
	     "verbose" => \$verbose,
	     "vverbose" => \$vverbose,
	     "version" => \&version
	     ));

usage() if $Getopt::Long::error;
usage() unless @ARGV;

my $found_pinnumber_attr = 0;
my $found_pinseq_attr = 0;
my $found_pintype_attr = 0;
my $found_pinlabel_attr = 0;
my $found_numslots_attr = 0;
my $found_device_attr = 0;
my $found_footprint_attr = 0;
my $seqcnt = 0;
my $skip_line_out = 0;
my $file_line = 0;

my $st_scan = 0;
my $st_pin_start = 1;
my $st_pin_body = 2;

my $state = $st_scan;		# intial state machine state

if($vverbose) { $verbose = 1; }

# make sure the input schematic exists and we can read it
my $fname=shift(@ARGV);
print "\nProcessing symbol file $fname\n";
open(NETLIST,"$fname") or die "Can't open $fname: $!\n";

# open output netlist
$outfname="$fname.fix";
open(OUTNET,">$outfname") or die "Can't open $outfname: $!\n";

# parse netlist
while($line = <NETLIST>) {
  $file_line++;
  #==========================
  if( $state == $st_scan ) {
    if( $line =~ /^P/) { 
      $state = $st_pin_start; 
      if($vverbose){print "Pin start...\n";}
    }
    elsif( $line =~ /^numslots=/) { 
      $found_numslots_attr = 1;
    }
    elsif( $line =~ /^device=/) { 
      $found_device_attr = 1;
    }
    elsif( $line =~ /^footprint=/) { 
      $found_footprint_attr = 1;
    }
    print OUTNET $line;
  }
  #==========================
  elsif( $state == $st_pin_start ) {
    if( $line =~ /^{/) {
      $state = $st_pin_body;
      $found_pinnumber_attr = 0;
      $found_pinseq_attr = 0;
      $found_pintype_attr = 0;
      $found_pinlabel_attr = 0;
      print OUTNET $line;
    } 
    else {
      print STDERR "*** ERROR: No pin left bracket found at line $file_line\n";
      exit(-1);
    }
  }
  #==========================
  elsif( $state == $st_pin_body ) {
    #----------------------
    if( $line =~ /^pinnumber/) {
      $found_pinnumber_attr = 1;
      $pin_num = $line;
      $pin_num =~ s/\n//;
      $pin_num =~ s/^pinnumber=//;
      $pinnumber_attr_line = $prev_line;
      if($vverbose){print "  pinnumber attribute found ($pin_num)\n";}
    }
    #----------------------
    elsif( $line =~ /^pinseq/) {
      $found_pinseq_attr = 1;
      if($vverbose){print "  Pinseq attribute found\n";}
      $seqcnt++;
      if($vverbose){print "  Renumbering attr pinseq=$seqcnt\n";}
      print OUTNET "pinseq=$seqcnt\n";
      $skip_line_out++;
    }
    #----------------------
    elsif( $line =~ /^pintype/) {
      $found_pintype_attr = 1;
      if($vverbose){print "  pintype attribute found\n";}
    }
    #----------------------
    elsif( $line =~ /^pinlabel/) {
      $found_pinlabel_attr = 1;
      if($vverbose){print "  pinlabel attribute found\n";}
    }
    #----------------------
    elsif( $line =~ /^}/) {
      $state = $st_scan;
      if( $found_pinnumber_attr == 0 ) {
        print "*** WARNING: no pinum attribute found at line $file_line\n";
      }
      if( $found_pinseq_attr == 0 ) {
	$seqcnt++;
	if($verbose){print "  Pin $pin_num: Adding attr pinseq=$seqcnt\n";}
	print OUTNET $pinnumber_attr_line;
	print OUTNET "pinseq=$seqcnt\n";
      }
      if( $found_pintype_attr == 0 ) {
	if($verbose){print "  Pin $pin_num: Adding attr pintype=io\n";}
	print OUTNET $pinnumber_attr_line;
	print OUTNET "pintype=io\n";
      }
      if( $found_pinlabel_attr == 0 ) {
	if($verbose){print "  Pin $pin_num: Adding attr pinlabel=n_a\n";}
	print OUTNET $pinnumber_attr_line;
	print OUTNET "pinlabel=n_a\n";
      }
      if($vverbose){print "Pin end...\n";}
    }
    if( $skip_line_out ) {
      $skip_line_out = 0;
    } else {
      print OUTNET $line;
    }
    $prev_line = $line;		# Save line for next pass
  }
}

if( $found_numslots_attr == 0 ) {
  if($verbose){print "  Adding attr numslots=0\n";}
  print OUTNET "T 600 100 9 10 0 0 0 0 1\n";
  print OUTNET "numslots=0\n";
}
if( $found_device_attr == 0 ) {
  if($verbose){print "  Adding attr device=none\n";}
  print OUTNET "T 600 100 9 10 0 0 0 0 1\n";
  print OUTNET "device=none\n";
}
if( $found_footprint_attr == 0 ) {
  if($verbose){print "  Adding attr footprint=unknown\n";}
  print OUTNET "T 600 100 9 10 0 0 0 0 1\n";
  print OUTNET "footprint=unknown\n";
}

close(NETLIST);
close(OUTNET);

print STDERR "Output written to file $outfname\n\n";
exit;


#######################################################################
#
# Subroutines
#
#######################################################################

#---------------------------------
# usage()
#
# prints program usage
#---------------------------------

sub usage {
  my $pname = $0;
  $pname =~ s/.*\///g;

  print "\nUsage:\n\n";
  print "    $pname [option] symbol_file\n";
  print "\n";
  print "    $pname reads a geda symbol file and does the following:\n";
  print "\t- Prints a warning if there is no pinnumber attribute on a pin\n";
  print "\t- Renumbers pinseq attributes on all pins, starting with 1\n";
  print "\t- Adds a pinseq attribute to a pin if none exists\n";
  print "\t- Adds a pinlabel=n_a attribute to a pin if none exists\n";
  print "\t- Adds a pintype=io attribute to a pin if none exists\n";
  print "\t- Adds a numslots=0 attribute to the symbol if none exists\n";
  print "\t- Adds a device=XXX attribute to the symbol if none exists\n";
  print "\t- Adds a footprint=XXX attribute to the symbol if none exists\n";
  print "    The idea here is to fix common cut and paste issues and fix up\n";
  print "    the symbol so that it will pass gsymcheck with no errors or\n";
  print "    warnings.\n";
  print "\n";
  print "    $pname accepts the following options:\n";
  print "\n";
  print "    --help      Displays this help message.\n";
  print "\n";
  print "    --verbose   Enables verbose output.\n";
  print "\n";
  print "    --vverbose  Enables *very* verbose output.\n";
  print "\n";
  print "    --version   Shows the version of this program.\n";
  print "\n";
  print "    $pname was written by Mike Skerritt <mike\@acornpacket.com>\n";
  print "\n\n";
  exit;
}

#---------------------------------
# version()
#
# prints program version
#---------------------------------

sub version {
  my $pname = $0;
  $pname =~ s/.*\///g;
  print "$pname ($0):  Version $version\n";
}


