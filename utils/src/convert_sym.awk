#!/usr/bin/awk -f 
# 	$Id$	
#       $Author$
# awk script to convert viewlogic symbol files to geda files
#
#
# Copyright (C) 1998 Mike Jarabek
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.     
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
#

BEGIN { 
  print "v 19981025";

  reset_attributes();

  reading_net = 0;       # used to keep track of when we are reading a net
  segment_count = 1;     # the number of net segments read for a net

}

# bounding box
$1 == "D" {
  
  # just fetch the values and store

  minx = $2 * 10;
  miny = $3 * 10;
  maxx = $4 * 10;
  maxy = $5 * 10;

}

# unattached attribute
$1 == "U" { 

  # if we are inside of a pin definition, terminate
  reset_attributes();

  # for the moment just represent as text
  
  # viewlogic unnatached attributes have this format:
  # U #X #Y #SIZE #ROTATION #origin #Visibility ATTR_TEXT

  x = ($2) * 10;
  y = ($3) * 10;
  color = 1;
  size = $4;
  angle = $5;
  text = $8;
  # if there are more fields on the line, they must be text
  # this works, but some spacing may be lost
  if (NF > 8) {
    for(i = 9; i <= NF; i++) {
      text = text " " $i;
    }
  }  origin = $6;

  # evaluate visibility for attributes
  viewvis = $7;
  if( viewvis == 0 ) {   # not at all visibile
    visibility = 0;
    show_name_value = 0;
  } else if (viewvis == 1) { # attribute and name visible
    visibility = 1;
    show_name_value = 0;
  } else if (viewvis == 2) { # only name visible
    visibility = 2;
    show_name_value = 1;
  } else if (viewvis == 3) { # only value visible
    visibility = 1;
    show_name_value = 1;
  }


  text_object( x, y, color, size, visibility, show_name_value, angle, text, \
	       origin);

}

# attached attribute
$1 == "A" {
  
  # attached attributes have the following format:
  #    A #X #Y #SIZE #ROTATION #ORIGIN #VISIBILITY ATTR_TEXT

  # extract interesting bits:
 
  x = ($2) * 10;
  y = ($3) * 10;
  color = 1;
  size = $4;
  angle = $5;
  text = $8;
  # if there are more fields on the line, they must be text
  # this works, but some spacing may be lost
  if (NF > 8) {
    for(i = 9; i <= NF; i++) {
      text = text " " $i;
    }
  }
  origin = $6;

  # evaluate visibility for attributes
  viewvis = $7;
  if( viewvis == 0 ) {   # not at all visibile
    visibility = 0;
    show_name_value = 0;
  } else if (viewvis == 1) { # attribute and name visible
    visibility = 1;
    show_name_value = 0;
  } else if (viewvis == 2) { # only name visible
    visibility = 2;
    show_name_value = 1;
  } else if (viewvis == 3) { # only value visible
    visibility = 1;
    show_name_value = 1;
  }

  # output the attachment magic if needed.
  begin_attach();
  text_object( x, y, color, size, visibility, show_name_value, angle, text, \
	       origin);

}  
    
# text primitive
$1 == "T" {
  
  # if we are inside of a pin definition, terminate
  reset_attributes();

  # viewlogic text have the following format:
  #  T #X #Y #SIZE #ROTATION #ORIGIN TEXT

  x = $2 * 10;
  y = $3 * 10;
  color = 3;
  size = $4;
  visibility = 1;
  show_name_value = 0;
  angle = $5;
  text = $7;
  # if there are more fields on the line, they must be text
  # this works, but some spacing may be lost
  if (NF > 7) {
    for(i = 8; i <= NF; i++) {
      text = text " " $i;
    }
  }
  origin = $6

  text_object(x, y, color, size, visibility, show_name_value, angle, text, \
	      origin);
  
}

# line primitive
$1 == "l" {

  # if we are inside of a pin definition, terminate
  reset_attributes();

      
  # the viewlogic line primitive is composed of 
  #         l #PAIRS #X1 #Y1 #X2 #Y2 ...   - Line

  pairs = $2;
  color = 3;

  # represent this as a series of simple geda lines
    for (i=0; i < (pairs-1); i++) {
      x1 = $((i*2)+3) * 10;
      y1 = $((i*2)+4) * 10;
      x2 = $((i*2)+5) * 10;
      y2 = $((i*2)+6) * 10;

      line_object(x1,y1,x2,y2,color);
    }
}

# pin primitive
$1 == "P" {

  # if we are inside of a pin definition, terminate
  reset_attributes();
      
  # viewlogic pin primitives have the following format:
  #  P #PININSTANCE #X1 #Y1 #X2 #Y2 # #PINDIRECTION #PINSENSE
  
  pinsense = $9;
  pindir   = $8;
  

  x1 = $3 * 10;
  y1 = $4 * 10;
  x2 = $5 * 10;
  y2 = $6 * 10;
  color = 1;



  # if this pin has to be of negative polarity, add a bitty bubble
  # and adjust the size of the pin
  radius = 25;
  radius2 = 2*radius;
  bcolor = 6;
  
  if(pinsense == 1) {
    
    #print "pindir:" pindir
    # get the bubble on the right end

    # one of the coordinates will match up with the bounding box
    # then just choose the other end for the bubble

    if(x1 == minx) {    # left hand side of pin touches bounding box
      bx = x2-radius;
      by = y2;
      x2 -= radius2;

    } else if (x1 == maxx) { # left end touches right side 
      bx = x2+radius;
      by = y2;
      x2 += radius2;

    } else if (x2 == minx) { # right end touches left side
      bx = x1-radius;
      by = y1;
      x1 -= radius2;

    } else if (x2 == maxx) { # right end touches right side
      bx = x1+radius;
      by = y1;
      x1 += radius2;

    } else if (y1 == miny) { # left end touches bottom
      bx = x2;
      by = y2-radius;
      y2 -= radius2;

    } else if (y1 == maxy) { # left end touches top
      bx = x2;
      by = y2+radius;
      y2 += radius2;

    } else if (y2 == miny) { # right end touches bottom
      bx = x1;
      by = y1-radius;
      y1 -= radius2;

    } else if (y2 == maxy) { # right end touches top
      bx = x1;
      by = y1+radius;
      y1 += radius2;
    } else {
      print "Pinsense error!";
      exit;
    }
       
   

    circle_object(bx,by,radius,bcolor);
  }

  pin_object(x1,y1,x2,y2,color);


  add_attributes = 1;   # add attributes
  attach_pending = 1;   # signal that an attachment could be coming
  pin_attributes = 1;   # and that they are pin attributes
  pin_count++;          # bump the number of pins
}

# box primitive
$1 == "b" {

  # if we are inside of a pin definition, terminate
  reset_attributes();

  # a viewlogic box has the following format:
  #  b #X1 #Y1 #X2 #Y2
  # geda view of a box has the corner, width and height

  x1 = $2 * 10;
  y1 = $3 * 10;
  x2 = $4 * 10;
  y2 = $5 * 10;

  width = x2-x1;
  height = y2-y1;
  color  = 3;

  box_object(x1,y1,width,height,color);

}

# circle primitive
$1 == "c" {
  
  # if we are inside of a pin definition, terminate
  reset_attributes();

  # a circle has the following format:
  #   c #x #y #radius

  x = $2 * 10;
  y = $3 * 10;
  radius = $4 * 10;
  color = 3;

  circle_object(x,y,radius,color);
} 

# arc primitive
$1 == "a" {

  # if we are inside of a pin definition, terminate
  reset_attributes()

  # arcs have the following format:
  #   a #X1 #Y1 #X2 #Y2 #X3 #Y3
  # we need to transform this into the geda convention of
  # center, radius, start angle, stop angle.

  x1 = $2*10.0;
  y1 = $3*10.0;
  x2 = $4*10.0;
  y2 = $5*10.0;
  x3 = $6*10.0;
  y3 = $7*10.0;

  x2p = x2 - x1;
  y2p = y2 - y1;

  x3p = x3 - x1;
  y3p = y3 - y1;

  yop = (x2p * ( x3p^2 + y3p^2 ) - x3p * ( x2p^2 + y2p^2 )) / \
    (2 * (x2p * y3p - y2p * x3p));

  xop = (x2p^2 - 2*y2p*yop) / (2 * x2p);

  xo  = xop + x1;
  yo  = yop + y1;


  radius = int(sqrt(xop^2 + yop^2));

  # calculate start and end angles
  to_rad = 180.0/atan2(0,-1);
  start_angle = int(atan2(y1-yo, x1-xo) * to_rad);
  end_angle = int(atan2(y3-yo, x3-xo) * to_rad);

  if(start_angle > end_angle) {
    gstart = end_angle;
    sweep_angle = start_angle - end_angle;
  } else {
    gstart = start_angle;
    sweep_angle = end_angle - start_angle;
  }

  #end_angle   = 
  #end_angle   = int(atan2(y1-yo, x1-xo) * to_rad) % 360;
  #start_angle = int(atan2(y3-yo, x3-xo) * to_rad) % 360;


  color = 1;
  
  arc_object(int(xo),int(yo), radius, gstart, sweep_angle, color);

}

# label primitive
$1 == "L" {

  # labels have the following format:
  #    L #X #Y #SIZE #ROTATION #ORIGIN #GLOBAL #VISIBILITY #OVERBAR TEXT

  # reproduce as simple text

  x = $2 * 10;
  y = $3 * 10;
  color = 5;
  size = $4;
  visibility = 1;
  show_name_value = 0;

  # Note from Ales, some ViewDraw symbols of mine had text labels which 
  # had an angle of 3 which is wrong, so I just changed the next line to:
  #  angle = 0

  angle = $5;
  text = $10;
  # if there are more fields on the line, they must be text
  # this works, but some spacing may be lost
  if (NF > 10) {
    for(i = 11; i <= NF; i++) {
      text = text " " $i;
    }
  }
  origin = $6;


  # if we are inside a pin definition, mangle the pin name
  if(pin_attributes == 1) {
    text = "pin" pin_count "=" text;
    show_name_value = 1;   # and show just the name
  } else if(net_attributes == 1) {
    text = "net=" text;               # a label on a net is the net name
    show_name_value = 1;
  } else if(complex_attributes == 1) {
    text = "uref=" text;            # a label on a complex is its designator
    show_name_value = 1;
  }

  begin_attach();
  text_object(x, y, color, size, visibility, show_name_value, angle, text, \
	      origin);
}

# nets
$1 == "N" {

  reset_attributes();


  reading_net = 1;
  segment_count = 1;
  delete net_nodes_x;     # zap the array
  delete net_nodes_y;
}

$1 == "J" {

  # in geda nets are composed of a number of segments, gschem connects
  # them together on the screen,  since viewlogic stores a net as a series
  # of nodes we need to tabulate them and only output segments when we
  # get connectivity information

  # net segments have the following format:
  #  J #X #Y #SEGNUM  - Net segment

  # get the current info
  x = $2*10;
  y = $3*10;

  net_nodes_x[segment_count] = x;
  net_nodes_y[segment_count] = y;

  segment_count++;
}

$1 == "S" {
  
  reset_attributes();

  # net segment connectivity records have the following format:
  #  S #N1 #N2            - Net connectivity, Node N1 is connected to N2

  n1 = int($2);
  n2 = int($3);
  color = 4;

  # output a geda net segment
  net_segment(net_nodes_x[n1], net_nodes_y[n1], \
	      net_nodes_x[n2], net_nodes_y[n2], color);

  # there could be attributes to follow
  add_attributes = 1;   # add attributes
  attach_pending = 1;   # signal that an attachment could be coming
  net_attributes = 1;   # and that they are net attributes

}

$1 == "B" {
  
  reset_attributes();

  # bus segment connectivity records have the following format:
  #  B #N1 #N2            - Bus connectivity, Node N1 is connected to N2

  n1 = int($2);
  n2 = int($3);
  color = 4;

  # output a geda net segment -- since geda does not know about busses -- yet
  net_segment(net_nodes_x[n1], net_nodes_y[n1], \
	      net_nodes_x[n2], net_nodes_y[n2], color);

  # there could be attributes to follow
  add_attributes = 1;   # add attributes
  attach_pending = 1;   # signal that an attachment could be coming
  net_attributes = 1;   # and that they are net attributes

}

# component instance
$1 == "I" {

  reset_attributes();
  # a component instance has the following format
  #  I #instance LIB:NAME #PAGE #X #Y #ROTATION #MAGNIFICATION  

  x = $5*10;
  y = $6*10;
  selectable = 1;
  angle = 0;
  mirror = 0;   # need to add logic to decode rotation and mirror

  # get name
  fullname = $3;
  split(fullname, names, ":");

  # get extension
  extension = $4;

  # produce proper file name:
  name = tolower(names[2]) "." extension ".sym";

  complex_object(x, y, selectable, angle, mirror, name);

  # there could be attributes to follow
  add_attributes = 1;   # add attributes
  attach_pending = 1;   # signal that an attachment could be coming
  complex_attributes = 1;   # and that they are complex attributes
}  

# just junk any records we do not deal with
 {}
  
# utility functions
function text_object( x, y, color, size, visibility, show_name_value, \
		      angle, text, origin) {

  # fudge the text size, in viewdraw it is actually the height
  # in geda it is the point size

  text_size = int(size * 0.72);

  # emulate the viewdraw text origin by shifting the text around
  
  # if the origin is one of the ones that are along the center line,
  # adjust y

  if ( (origin == 2) || (origin == 5) || (origin == 8) ) {
    y -= (size*10) / 2;
  }

  if( (origin == 1) || (origin == 4) || (origin == 7) ) {
    y -= size * 10;
  }

  # approximate the length of the text
  if(show_name_value == 0) {
    text_to_measure = text;
  } else if(show_name_value == 1) {
    split(text,a,"=");
    text_to_measure = a[2]; # measure just the value part
  } else if(show_name_value == 2) {
    split(text,a,"=");
    text_to_measure = a[1]; # measure just the textual part
  }

  textlen = length(text_to_measure) * size * 10;
  # if the origin is one of the middle ones
  # fix the x coordinate
  if( (origin == 4) || (origin == 5) || (origin == 6) ) {
    x -= textlen / 2;
  }

  if( (origin == 7) || (origin == 8) || (origin == 9) ) {
    x -= textlen;
  }


  print "T", x, y, color, text_size, visibility, show_name_value, angle;
  print text;
}

function line_object( x1,y1,x2,y2,color ) {
  
  print "L",x1,y1,x2,y2,color;
}

function circle_object( bx,by,radius,bcolor ) {
  
      print "V",bx,by,radius,bcolor;
}

function pin_object( x1,y1,x2,y2,color ) {

    print "P",x1,y1,x2,y2,color;
}

function box_object( x1,y1,width,height,color ) {

  print "B",x1,y1,width,height,color;
}

function arc_object( x1, y1, radius, start_angle, sweep_angle, color) {

  print "A",x1, y1, radius, start_angle, sweep_angle, color;

}

function net_segment( x1, y1, x2, y2, color ) {

  print "N", x1, y1, x2, y2, color;

}

function complex_object(x, y, selectable, angle, mirror, name) {

  print "C", x, y, selectable, angle, mirror, name;
}

function begin_attach() {

  if(attach_pending == 1) {   # begin an attachment if one is pending
    print "{";
    attach_pending = 0;
  }
}

function end_attach() {

  print "}";
}

function reset_attributes() {

  # if we are inside of some kind of attribute attachment
  # terminate it, but only if we output the begin_attach.
  if((add_attributes == 1) && (attach_pending == 0)) {
    end_attach();
  }

  attach_pending = 0;    # keep track of whether the last object
                         # read may have attachments pending.

  add_attributes = 0;    # keep track of whether we are adding attributes
                         # to some previous object
  pin_attributes = 0;    # when true, we are adding attributes to a pin
  net_attributes = 0;    # when true, we are adding atrributes to a net
  complex_attributes = 0; # when true, we are addint attibutes to a complex
  pin_count      = 0;    # to keep track of the number of pins

}


