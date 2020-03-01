/* Lepton EDA library
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2016 gEDA Contributors
 * Copyright (C) 2017-2020 Lepton EDA Contributors
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */

/*! \file o_picture.c
 *  \brief functions for the picture object
 */

#include <config.h>

#include <stdio.h>
#ifdef HAVE_STRING_H
#include <string.h>
#endif
#include <math.h>

#include <gio/gio.h>

#include "libgeda_priv.h"



static const char* fallback_xpm[] = {
/* columns rows colors chars-per-pixel */
"48 48 240 2 ",
"   c black",
".  c #010000",
"X  c #010100",
"o  c #020100",
"O  c #030100",
"+  c #020200",
"@  c #020202",
"#  c #040300",
"$  c #050400",
"%  c #070400",
"&  c #0E0302",
"*  c #090700",
"=  c #090701",
"-  c #0D0403",
";  c #0A0800",
":  c #0B0900",
">  c #0E0B00",
",  c #1C0705",
"<  c #100D01",
"1  c #130F00",
"2  c #130F01",
"3  c #151001",
"4  c #191300",
"5  c #1D1602",
"6  c #1E1702",
"7  c #210705",
"8  c #230704",
"9  c #220705",
"0  c #230705",
"q  c #260806",
"w  c #2B0A07",
"e  c #2C0805",
"r  c #2E0906",
"t  c #2E0A08",
"y  c #3B0C09",
"u  c #3C0C09",
"i  c #3D0C09",
"p  c #3E0C08",
"a  c #251D02",
"s  c #241C03",
"d  c #251D03",
"f  c #271E02",
"g  c #292003",
"h  c #2C2303",
"j  c #312604",
"k  c #312605",
"l  c #332704",
"z  c #382C05",
"x  c #3C2E05",
"c  c #410C07",
"v  c #420C07",
"b  c #490D07",
"n  c #4D0D07",
"m  c #420D08",
"M  c #440E09",
"N  c #470E09",
"B  c #440E0A",
"V  c #460E0A",
"C  c #490E09",
"Z  c #490E0A",
"A  c #520E07",
"S  c #4D100C",
"D  c #571109",
"F  c #5D110B",
"G  c #5E110A",
"H  c #5A120D",
"J  c #5C120D",
"K  c #5E120C",
"L  c #631109",
"P  c #60120B",
"I  c #641209",
"U  c #61130E",
"Y  c #6A130B",
"T  c #6F140B",
"R  c #6E150C",
"E  c #6D160E",
"W  c #71150D",
"Q  c #73150C",
"!  c #72150D",
"~  c #73160E",
"^  c #77150C",
"/  c #78150B",
"(  c #78150C",
")  c #731711",
"_  c #7A1710",
"`  c #781812",
"'  c #7B1912",
"]  c #403104",
"[  c #403105",
"{  c #423305",
"}  c #413206",
"|  c #453506",
" . c #4B3A07",
".. c #4E3D06",
"X. c #513F07",
"o. c #524007",
"O. c #675009",
"+. c #69520A",
"@. c #71570A",
"#. c #745A0C",
"$. c #7A6109",
"%. c #7C610C",
"&. c #85170C",
"*. c #86180D",
"=. c #8A180C",
"-. c #8A190F",
";. c #901A0F",
":. c #911A0F",
">. c #921A0E",
",. c #9B1B0E",
"<. c #9A1C0F",
"1. c #801810",
"2. c #801A13",
"3. c #861910",
"4. c #871B12",
"5. c #861B13",
"6. c #851B14",
"7. c #881A11",
"8. c #891A11",
"9. c #8F1B10",
"0. c #8D1B12",
"q. c #8B1C15",
"w. c #8C1C14",
"e. c #931B10",
"r. c #971E15",
"t. c #991D13",
"y. c #9F1D10",
"u. c #9E1D11",
"i. c #9D1E12",
"p. c #9C1E13",
"a. c #9F1E13",
"s. c #981D14",
"d. c #9B1E14",
"f. c #9A1F15",
"g. c #991F16",
"h. c #981F17",
"j. c #9B1F17",
"k. c #9D1F15",
"l. c #9E1E14",
"z. c #9C1F16",
"x. c #A11C0E",
"c. c #A01D0F",
"v. c #A31C0E",
"b. c #A21D0F",
"n. c #A31D0F",
"m. c #A41C0E",
"M. c #A71D0E",
"N. c #A61D0F",
"B. c #A91D0F",
"V. c #B31F0F",
"C. c #A11D10",
"Z. c #A11D11",
"A. c #A21D10",
"S. c #A01E12",
"D. c #B21F10",
"F. c #B9200F",
"G. c #B72010",
"H. c #B62111",
"J. c #BA2010",
"K. c #BD2111",
"L. c #BF2212",
"P. c #C62210",
"I. c #CA2310",
"U. c #C82312",
"Y. c #CD2513",
"T. c #D22411",
"R. c #D12412",
"E. c #D52511",
"W. c #D42513",
"Q. c #D22614",
"!. c #D92613",
"~. c #DB2612",
"^. c #DC2613",
"/. c #DE2612",
"(. c #D82715",
"). c #E02712",
"_. c #EC2913",
"`. c #E82814",
"'. c #EA2915",
"]. c #EA2A16",
"[. c #ED2914",
"{. c #EF2914",
"}. c #EC2A16",
"|. c #F32912",
" X c #F52913",
".X c #F62913",
"XX c #F72913",
"oX c #F52A13",
"OX c #F42914",
"+X c #F42A14",
"@X c #F62B14",
"#X c #AB4216",
"$X c #B14010",
"%X c #BB5B11",
"&X c #B95B13",
"*X c #BF6F16",
"=X c #C56B12",
"-X c #C26F12",
";X c #C16916",
":X c #C16F14",
">X c #C77E14",
",X c #C57E16",
"<X c #C87E14",
"1X c #B9930E",
"2X c #BD9312",
"3X c #C39B0F",
"4X c #CD8713",
"5X c #C98517",
"6X c #CB8617",
"7X c #CC8614",
"8X c #CC8615",
"9X c #CF8812",
"0X c #CF8C12",
"qX c #CB8914",
"wX c #D18912",
"eX c #D08C13",
"rX c #D28814",
"tX c #C49813",
"yX c #C79A14",
"uX c #CB9E13",
"iX c #D7A816",
"pX c #DCAE11",
"aX c #DEAC13",
"sX c #DFAD13",
"dX c #DAAA16",
"fX c #DBAA16",
"gX c #DAAA17",
"hX c #DBAB17",
"jX c #DCAB15",
"kX c #D8A51C",
"lX c #D7A81A",
"zX c #D8A919",
"xX c #D9AA19",
"cX c #D8A91A",
"vX c #DFB112",
"bX c #E0AD11",
"nX c #E1AE12",
"mX c #E0AE14",
"MX c #E0B213",
"NX c None",
/* pixels */
"NXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNX",
"NXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNX",
"NXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNX",
"NXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNX",
"NXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNX",
"NXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNX",
"NXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNX",
"NXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNX+XP.NXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNX",
"NXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNX@XT.B.&.NXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNX",
"NXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNX.XE.M.x.,.I NXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNX",
"NXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXXX).V.m.m.x.=.NXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNX",
"NXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNX_.F.m.m.$Xm.<.L NXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNX",
"NXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXoXI.m.m.%X-Xn.c.>.NXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNX",
"NXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNX X~.N.m.m.bXvXn.n.c.^ NXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNX",
"NXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNX_.G.m.v.nXvXvXrXn.y.:.n NXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNX",
"NXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXoXI.m.m.=XvXvXvXvXC.A.y.Q NXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNX",
"NXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNX/.N.m.m.bXpX1XpXaX9XA.y.;.b NXNXNXNXNXNXNXNXNXNXNXNXNXNXNX",
"NXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNX{.J.m.m.wX3XX > [ aXaXA.Z.u.Q NXNXNXNXNXNXNXNXNXNXNXNXNXNXNX",
"NXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNX|.R.m.m.m.vX$.% 1 + aXaX4XZ.u.;.C NXNXNXNXNXNXNXNXNXNXNXNXNXNX",
"NXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNX`.D.m.m.MXvXh O . 4 aXaXjXZ.Z.i.! NXNXNXNXNXNXNXNXNXNXNXNXNXNX",
"NXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXoXU.m.m.rXvXvXa   # g aXjXjX4XS.i.9.C NXNXNXNXNXNXNXNXNXNXNXNXNX",
"NXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNX^.N.m.n.vXvXvXf   5 { jXjXjXjXS.S.i.W NXNXNXNXNXNXNXNXNXNXNXNXNX",
"NXNXNXNXNXNXNXNXNXNXNXNXNXNXNX[.K.m.m.eXvXvXaX..  ] | jXjXjXfX7XS.i.8.N NXNXNXNXNXNXNXNXNXNXNXNX",
"NXNXNXNXNXNXNXNXNXNXNXNXNXNXOX!.N.m.n.vXvXvXaX6 X d } jXjXfXfXfXS.a.t.G NXNXNXNXNXNXNXNXNXNXNXNX",
"NXNXNXNXNXNXNXNXNXNXNXNXNXNX'.G.m.b.MXvXvXaXaX* X : d jXfXfXfXfX8Xa.p.3.e NXNXNXNXNXNXNXNXNXNXNX",
"NXNXNXNXNXNXNXNXNXNXNXNXNX{.W.m.n.rXvXvXaXaXaX$ < ; l fXfXfXfXgXgXa.a.s.K NXNXNXNXNXNXNXNXNXNXNX",
"NXNXNXNXNXNXNXNXNXNXNXNXNX'.G.n.n.vXvXaXaXaXaX$ 2 # o.fXfXfXgXgXgX8Xl.d._ NXNXNXNXNXNXNXNXNXNXNX",
"NXNXNXNXNXNXNXNXNXNXNXNX{.W.n.n.eXvXaXaXaXaXjXz X.3 @.fXfXgXgXgXgXzXl.l.0.Z NXNXNXNXNXNXNXNXNXNX",
"NXNXNXNXNXNXNXNXNXNXNXNX'.G.n.n.vXaXaXaXaXjXjX2XO.j yXfXfXgXgXgXzXzX6Xl.f.E NXNXNXNXNXNXNXNXNXNX",
"NXNXNXNXNXNXNXNXNXNXNX{.W.n.n.eXvXaXaXaXjXjXjXjXfXfXfXfXgXgXgXgXzXzXzXk.f.4.M NXNXNXNXNXNXNXNXNX",
"NXNXNXNXNXNXNXNXNXNXNX'.H.n.n.vXaXaXaXjXjXjXjXuXd s tXgXgXgXgXzXzXzXlX;Xk.r.J NXNXNXNXNXNXNXNXNX",
"NXNXNXNXNXNXNXNXNXNX{.Q.n.n.0XaXaXaXaXjXjXjXfX .o o +.gXgXgXzXzXzXzXlX5Xk.g.' w NXNXNXNXNXNXNXNX",
"NXNXNXNXNXNXNXNXNXNX}.H.n.n.aXaXaXaXjXjXjXjXfXx # = #.gXgXzXzXzXzXlXlXlXk.z.w.V NXNXNXNXNXNXNXNX",
"NXNXNXNXNXNXNXNXNXNX(.n.n.<XmXsXaXjXjXjXjXfXfX2Xk %.iXgXzXzXzXzXlXlXlXlX*Xz.g.U NXNXNXNXNXNXNXNX",
"NXNXNXNXNXNXNXNXNX].L.n.A.A.A.A.Z.Z.Z.&X:X>XqXdXhXhXhXxXxXxXxXcXcXcXcXkX,X#Xg.` q NXNXNXNXNXNXNX",
"NXNXNXNXNXNXNXNXNXY.n.c.A.A.A.Z.Z.Z.Z.S.S.S.S.a.a.a.a.l.l.l.k.k.k.k.z.z.z.z.j.q.B NXNXNXNXNXNXNX",
"NXNXNXNXNXNXNXNXNX/ A Y T ( *.-.e.u.i.S.S.S.a.a.a.a.l.l.l.k.k.k.k.z.z.z.z.j.j.h.H NXNXNXNXNXNXNX",
"NXNXNXNXNXNXNXNXNXNX@ @ & 8 r c v D F P R ~ ~ ~ 1.7.7.7.7.4.4.4.5.5.5.5.6.6.2.) S @ NXNXNXNXNXNX",
"NXNXNXNXNXNXNXNXNXNXNX@ @ @ @ @ @ @ @ @ - 9 7 7 0 m p p p p p p i i i u u u y t , @ NXNXNXNXNXNX",
"NXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNX@ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @   NXNXNXNXNXNX",
"NXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNX@ @ @ @ @ @ @ @ @ @ @ @ @ @ NXNXNXNXNXNXNXNX",
"NXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNX",
"NXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNX",
"NXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNX",
"NXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNX",
"NXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNX",
"NXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNX",
"NXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNXNX"
};



/*! \brief Create picture OBJECT from character string.
 *  \par Function Description
 *  Parses \a first_line and subsequent lines from \a tb, and returns
 *  a newly-created picture #OBJECT.
 *
 *  \param [in]  toplevel       The TOPLEVEL object.
 *  \param [in]  first_line      Character string with picture description.
 *  \param [in]  tb              Text buffer to load embedded data from.
 *  \param [in]  release_ver     libgeda release version number.
 *  \param [in]  fileformat_ver  libgeda file format version number.
 *  \return A pointer to the new picture object, or NULL on error.
 */
OBJECT *o_picture_read (TOPLEVEL *toplevel,
                        const char *first_line,
                        TextBuffer *tb,
                        unsigned int release_ver,
                        unsigned int fileformat_ver,
                        GError **err)
{
  OBJECT *new_obj;
  int x1, y1;
  int width, height, angle;
  int mirrored, embedded;
  int num_conv;
  gchar type;
  const gchar *line = NULL;
  gchar *filename;
  gchar *file_content = NULL;
  guint file_length = 0;

  num_conv = sscanf(first_line, "%c %d %d %d %d %d %d %d\n",
                    &type, &x1, &y1, &width, &height, &angle, &mirrored, &embedded);

  if (num_conv != 8) {
    g_set_error(err, EDA_ERROR, EDA_ERROR_PARSE, _("Failed to parse picture definition"));
    return NULL;
  }

  if (width == 0 || height == 0) {
    s_log_message(_("Found a zero width/height picture "
                    "[ %1$c %2$d %3$d %4$d %5$d ]"),
                  type, x1, y1, width, height);
  }

  if ( (mirrored > 1) || (mirrored < 0)) {
    s_log_message(_("Found a picture with a wrong 'mirrored' parameter: %1$d."),
                  mirrored);
    s_log_message(_("Setting mirrored to 0."));
    mirrored = 0;
  }

  if ( (embedded > 1) || (embedded < 0)) {
    s_log_message(_("Found a picture with a wrong 'embedded' parameter: %1$d."),
                  embedded);
    s_log_message(_("Setting embedded to 0."));
    embedded = 0;
  }

  switch(angle) {

    case(0):
    case(90):
    case(180):
    case(270):
    break;

    default:
      s_log_message(_("Found an unsupported picture angle [ %1$d ]"), angle);
      s_log_message(_("Setting angle to 0."));
      angle=0;
      break;

  }

  filename = g_strdup(s_textbuffer_next_line(tb));
  filename = geda_string_remove_ending_newline (filename);

  /* Handle empty filenames */
  if (strlen (filename) == 0) {
    s_log_message (_("Found an image with no filename."));
    g_free (filename);
    filename = NULL;
  }

  if (embedded == 1) {
    GString *encoded_picture=g_string_new("");
    char finished = 0;

    /* Read the encoded picture */
    do {

      line = s_textbuffer_next_line(tb);
      if (line == NULL) break;

      if (strcmp (line, ".\n") != 0) {
        encoded_picture = g_string_append (encoded_picture, line);
      } else {
        finished = 1;
      }
    } while (finished == 0);

    /* Decode the picture */
    if (encoded_picture != NULL) {
      file_content = s_encoding_base64_decode(encoded_picture->str,
                                              encoded_picture->len,
                                              &file_length);
      g_string_free (encoded_picture, TRUE);
    }

    if (file_content == NULL) {
      s_log_message (_("Failed to load image from embedded data [%1$s]: %2$s"),
                     filename, _("Base64 decoding failed."));
      s_log_message (_("Falling back to file loading. Picture unembedded."));
      embedded = 0;
    }
  }

  /* create the picture */
  /* The picture is described by its upper left and lower right corner */
  new_obj = o_picture_new (toplevel, file_content, file_length, filename,
                           type,
                           x1, y1+height, x1+width, y1,
                           angle, mirrored, embedded);

  g_free (file_content);
  g_free (filename);

  return new_obj;
}

/*! \brief Create a character string representation of a picture OBJECT.
 *  \par Function Description
 *  This function formats a string in the buffer <B>*buff</B> to describe
 *  the picture object <B>*object</B>.
 *
 *  \param [in] object  Picture OBJECT to create string from.
 *  \return A pointer to the picture OBJECT character string.
 *
 *  \note
 *  Caller must g_free returned character string.
 *
 */
gchar*
geda_picture_object_to_buffer (const GedaObject *object)
{
  int width, height, x1, y1;
  gchar *encoded_picture=NULL;
  gchar *out=NULL;
  guint encoded_picture_length;
  const gchar *filename = NULL;

  /* calculate the width and height of the box */
  width  = abs(object->picture->lower_x - object->picture->upper_x);
  height = abs(object->picture->upper_y - object->picture->lower_y);

  /* calculate the lower left corner of the box */
  x1 = object->picture->upper_x;
  y1 = object->picture->upper_y - height; /* move the origin to 0, 0*/

#if DEBUG
  printf("picture: %d %d %d %d\n", x1, y1, width, height);
#endif

  /* Encode the picture if it's embedded */
  if (o_picture_is_embedded (object)) {
    encoded_picture =
      s_encoding_base64_encode( (char *)object->picture->file_content,
                                object->picture->file_length,
                                &encoded_picture_length,
                                TRUE);
    if (encoded_picture == NULL) {
      s_log_message(_("ERROR: unable to encode the picture."));
    }
  }

  /* Cope with null filename */
  filename = o_picture_get_filename (object);
  if (filename == NULL) filename = "";

  if (o_picture_is_embedded (object) &&
      encoded_picture != NULL) {
    out = g_strdup_printf("%c %d %d %d %d %d %c %c\n%s\n%s\n%s",
                          object->type,
                          x1, y1, width, height,
                          object->picture->angle,
                          /* Convert the (0,1) chars to ASCII */
                          (object->picture->mirrored)+0x30,
                          '1',
                          filename,
                          encoded_picture,
                          ".");
  }
  else {
    out = g_strdup_printf("%c %d %d %d %d %d %c %c\n%s",
                          object->type,
                          x1, y1, width, height,
                          object->picture->angle,
                          /* Convert the (0,1) chars to ASCII */
                          (object->picture->mirrored)+0x30,
                          '0',
                          filename);
  }
  g_free(encoded_picture);

  return(out);
}


/*! \brief Create a picture object.
 *  \par Function Description
 *  This function creates a new object representing a picture.
 *
 *  The picture is described by its upper left corner (\a x1, \a y1)
 *  and its lower right corner (\a x2, \a y2).  The \a type parameter
 *  must be equal to #OBJ_PICTURE.
 *
 *  If \a file_content is non-NULL, it must be a pointer to a buffer
 *  containing raw image data.  If loading data from \a file_content
 *  is unsuccessful, and \a filename is non-NULL, an image will
 *  attempt to be loaded from \a filename.  Otherwise, the picture
 *  object will be initially empty.
 *
 *  \param [in]     toplevel      The TOPLEVEL object.
 *  \param [in]     file_content  Raw data of the image file, or NULL.
 *  \param [in]     file_length   Length of raw data buffer
 *  \param [in]     filename      File name backing this picture, or NULL.
 *  \param [in]     type          Must be OBJ_PICTURE.
 *  \param [in]     x1            Upper x coordinate.
 *  \param [in]     y1            Upper y coordinate.
 *  \param [in]     x2            Lower x coordinate.
 *  \param [in]     y2            Lower y coordinate.
 *  \param [in]     angle         Picture rotation angle.
 *  \param [in]     mirrored      Whether the image should be mirrored or not.
 *  \param [in]     embedded      Whether the embedded flag should be set or not.
 *  \return A pointer to a new picture #OBJECT.
 */
OBJECT *o_picture_new (TOPLEVEL *toplevel,
                       const gchar *file_content, gsize file_length,
                       const gchar *filename,
                       char type, int x1, int y1, int x2, int y2, int angle,
                       int mirrored, int embedded)
{
  OBJECT *new_node;
  PICTURE *picture;

  /* create the object */
  new_node = s_basic_new_object(type, "picture");

  picture = geda_picture_new ();
  new_node->picture = picture;

  /* describe the picture with its upper left and lower right corner */
  picture->upper_x = (x1 > x2) ? x2 : x1;
  picture->upper_y = (y1 > y2) ? y1 : y2;
  picture->lower_x = (x1 > x2) ? x1 : x2;
  picture->lower_y = (y1 > y2) ? y2 : y1;

  picture->pixbuf = NULL;
  picture->file_content = NULL;
  picture->file_length = 0;

  picture->ratio = fabs ((double) (x1 - x2) / (y1 - y2));
  picture->filename = g_strdup (filename);
  picture->angle = angle;
  picture->mirrored = mirrored;
  picture->embedded = embedded;

  if (file_content != NULL) {
    GError *error = NULL;
    if (!o_picture_set_from_buffer (toplevel, new_node, filename,
                                    file_content, file_length, &error)) {
      s_log_message (_("Failed to load buffer image [%1$s]: %2$s"),
                     filename, error->message);
      g_error_free (error);

      /* Force the data into the object anyway, so as to prevent data
       * loss of embedded images. */
      picture->file_content = (gchar*) g_memdup (file_content, file_length);
      picture->file_length = file_length;
    }
  }
  if (picture->pixbuf == NULL && filename != NULL) {
    GError *error = NULL;
    if (!o_picture_set_from_file (toplevel, new_node, filename, &error)) {
      s_log_message (_("Failed to load image from [%1$s]: %2$s"),
                     filename, error->message);
      g_error_free (error);
      /* picture not found; try to open a fall back pixbuf */
      picture->pixbuf = o_picture_get_fallback_pixbuf(toplevel);
    }
  }

  return new_node;
}

/*! \brief Get picture bounding rectangle in WORLD coordinates.
 *
 *  On failure, this function sets the bounds to empty.
 *
 *  \param [in]  object    Picture OBJECT to read coordinates from.
 *  \param [out] bounds    The bounds of the picture
 */
void
geda_picture_object_calculate_bounds (const OBJECT *object,
                                      GedaBounds *bounds)
{
  geda_bounds_init (bounds);

  g_return_if_fail (object != NULL);
  g_return_if_fail (object->type == OBJ_PICTURE);
  g_return_if_fail (object->picture != NULL);

  geda_bounds_init_with_points (bounds,
                                object->picture->lower_x,
                                object->picture->lower_y,
                                object->picture->upper_x,
                                object->picture->upper_y);
}

/*! \brief get the position of the left bottom point
 *  \par Function Description
 *  This function gets the position of the bottom left point of a picture object.
 *
 *  \param [in] object   The object to get the position.
 *  \param [out] x       pointer to the x-position
 *  \param [out] y       pointer to the y-position
 *  \return TRUE if successfully determined the position, FALSE otherwise
 */
gboolean
geda_picture_object_get_position (const GedaObject *object, gint *x, gint *y)
{
  g_return_val_if_fail (object != NULL, FALSE);
  g_return_val_if_fail (object->type == OBJ_PICTURE, FALSE);
  g_return_val_if_fail (object->picture != NULL, FALSE);

  if (x != NULL) {
    *x = MIN (object->picture->lower_x, object->picture->upper_x);
  }

  if (y != NULL) {
    *y = MIN (object->picture->lower_y, object->picture->upper_y);
  }

  return TRUE;
}


/*! \brief Get the width/height ratio of an image.
 * \par Function Description

 * Returns the width/height ratio of picture \a object, taking the
 * image rotation into account.
 *
 * \param object    Picture #OBJECT to inspect.
 * \return width/height ratio for \a object.
 */
double
o_picture_get_ratio (OBJECT *object)
{
  g_return_val_if_fail (object != NULL, 1);
  g_return_val_if_fail (object->picture != NULL, 1);

  /* The effective ratio varies depending on the rotation of the
   * image. */
  switch (object->picture->angle) {
  case 0:
  case 180:
    return object->picture->ratio;
  case 90:
  case 270:
    return 1.0 / object->picture->ratio;
  default:
    g_critical (_("Picture %1$p has invalid angle %2$i\n"), object,
                object->picture->angle);
  }
  return 0;
}

/*! \brief Modify the description of a picture OBJECT.
 *  \par Function Description
 *  This function modifies the coordinates of one of the four corner of
 *  the picture. The new coordinates of the corner identified by
 *  <B>whichone</B> are given by <B>x</B> and <B>y</B> in world unit.
 *
 *  The coordinates of the corner is modified in the world coordinate system.
 *  Screen coordinates and boundings are then updated.
 *
 *  \param [in]     toplevel  The TOPLEVEL object.
 *  \param [in,out] object     Picture OBJECT to modify.
 *  \param [in]     x          New x coordinate.
 *  \param [in]     y          New y coordinate.
 *  \param [in]     whichone   Which picture parameter to modify.
 *
 *  <B>whichone</B> can have the following values:
 *  <DL>
 *    <DT>*</DT><DD>PICTURE_UPPER_LEFT
 *    <DT>*</DT><DD>PICTURE_LOWER_LEFT
 *    <DT>*</DT><DD>PICTURE_UPPER_RIGHT
 *    <DT>*</DT><DD>PICTURE_LOWER_RIGHT
 *  </DL>
 */
void o_picture_modify(TOPLEVEL *toplevel, OBJECT *object,
                      int x, int y, int whichone)
{
  int tmp;
  double ratio = o_picture_get_ratio (object);

  o_emit_pre_change_notify (object);

  /* change the position of the selected corner */
  switch(whichone) {
    case PICTURE_UPPER_LEFT:
      object->picture->upper_x = x;
      tmp = abs(object->picture->upper_x - object->picture->lower_x) / ratio;
      if (y < object->picture->lower_y) {
        tmp = -tmp;
      }
      object->picture->upper_y = object->picture->lower_y + tmp;
      break;

    case PICTURE_LOWER_LEFT:
      object->picture->upper_x = x;
      tmp = abs(object->picture->upper_x - object->picture->lower_x) / ratio;
      if (y > object->picture->upper_y) {
        tmp = -tmp;
      }
      object->picture->lower_y = object->picture->upper_y - tmp;
      break;

    case PICTURE_UPPER_RIGHT:
      object->picture->lower_x = x;
      tmp = abs(object->picture->upper_x - object->picture->lower_x) / ratio;
      if (y < object->picture->lower_y) {
        tmp = -tmp;
      }
      object->picture->upper_y = object->picture->lower_y + tmp;
      break;

    case PICTURE_LOWER_RIGHT:
      object->picture->lower_x = x;
      tmp = abs(object->picture->upper_x - object->picture->lower_x) / ratio;
      if (y > object->picture->upper_y) {
        tmp = -tmp;
      }
      object->picture->lower_y = object->picture->upper_y - tmp;
      break;

    default:
      return;
  }

  /* need to update the upper left and lower right corners */
  if(object->picture->upper_x > object->picture->lower_x) {
    tmp                      = object->picture->upper_x;
    object->picture->upper_x = object->picture->lower_x;
    object->picture->lower_x = tmp;
  }

  if(object->picture->upper_y < object->picture->lower_y) {
    tmp                      = object->picture->upper_y;
    object->picture->upper_y = object->picture->lower_y;
    object->picture->lower_y = tmp;
  }

  o_emit_change_notify (object);
}

/*! \brief Modify a picture object's coordinates.
 * \par Function Description
 * Modifies the coordinates of all four corners of a picture \a
 * object.  The picture is adjusted to fit the rectangle enclosed by
 * the points (\a x1, \a y1) and (\a x2, \a y2), and scaled as large
 * as possible to still fit within that rectangle.
 *
 * \param [in,out] object   picture #OBJECT to be modified.
 * \param [in]     x1       x coordinate of first corner of box.
 * \param [in]     y1       y coordinate of first corner of box.
 * \param [in]     x2       x coordinate of second corner of box.
 * \param [in]     y2       y coordinate of second corner of box.
 */
void
o_picture_modify_all (OBJECT *object,
                      int x1,
                      int y1,
                      int x2,
                      int y2)
{
  o_emit_pre_change_notify (object);

  /* Normalise the requested rectangle. */
  object->picture->lower_x = (x1 > x2) ? x1 : x2;
  object->picture->lower_y = (y1 > y2) ? y2 : y1;
  object->picture->upper_x = (x1 > x2) ? x2 : x1;
  object->picture->upper_y = (y1 > y2) ? y1 : y2;

  o_emit_change_notify (object);
}

/*! \brief Rotate picture OBJECT using WORLD coordinates.
 *  \par Function Description
 *  This function rotates the picture described by <B>*object</B> around
 *  the (<B>world_centerx</B>, <B>world_centery</B>) point by <B>angle</B>
 *  degrees.
 *  The center of rotation is in world units.
 *
 *  \param [in]      toplevel      The TOPLEVEL object.
 *  \param [in]      world_centerx  Rotation center x coordinate in
 *                                  WORLD units.
 *  \param [in]      world_centery  Rotation center y coordinate in
 *                                  WORLD units.
 *  \param [in]      angle          Rotation angle in degrees (See note below).
 *  \param [in,out]  object         Picture OBJECT to rotate.
 */
void geda_picture_object_rotate (TOPLEVEL *toplevel,
                            int world_centerx, int world_centery, int angle,
                            OBJECT *object)
{
  int newx1, newy1;
  int newx2, newy2;

  g_return_if_fail (object != NULL);
  g_return_if_fail (object->picture != NULL);
  g_return_if_fail (object->type == OBJ_PICTURE);

  /* Only 90 degree multiple and positive angles are allowed. */
  /* angle must be positive */
  if(angle < 0) angle = -angle;
  /* angle must be a 90 multiple or no rotation performed */
  if((angle % 90) != 0) return;

  object->picture->angle = ( object->picture->angle + angle ) % 360;

  /* The center of rotation (<B>world_centerx</B>, <B>world_centery</B>) is
   * translated to the origin. The rotation of the upper left and lower
   * right corner are then performed. Finally, the rotated picture is
   * translated back to its previous location.
   */
  /* translate object to origin */
  object->picture->upper_x -= world_centerx;
  object->picture->upper_y -= world_centery;
  object->picture->lower_x -= world_centerx;
  object->picture->lower_y -= world_centery;

  /* rotate the upper left corner of the picture */
  geda_point_rotate_90 (object->picture->upper_x, object->picture->upper_y, angle,
                  &newx1, &newy1);

  /* rotate the lower left corner of the picture */
  geda_point_rotate_90 (object->picture->lower_x, object->picture->lower_y, angle,
                  &newx2, &newy2);

  /* reorder the corners after rotation */
  object->picture->upper_x = MIN(newx1,newx2);
  object->picture->upper_y = MAX(newy1,newy2);
  object->picture->lower_x = MAX(newx1,newx2);
  object->picture->lower_y = MIN(newy1,newy2);

  /* translate object back to normal position */
  object->picture->upper_x += world_centerx;
  object->picture->upper_y += world_centery;
  object->picture->lower_x += world_centerx;
  object->picture->lower_y += world_centery;
}

/*! \brief Mirror a picture using WORLD coordinates.
 *  \par Function Description
 *  This function mirrors the picture from the point
 *  (<B>world_centerx</B>,<B>world_centery</B>) in world unit.
 *
 *  The picture is first translated to the origin, then mirrored and
 *  finally translated back at its previous position.
 *
 *  \param [in]     toplevel      The TOPLEVEL object.
 *  \param [in]     world_centerx  Origin x coordinate in WORLD units.
 *  \param [in]     world_centery  Origin y coordinate in WORLD units.
 *  \param [in,out] object         Picture OBJECT to mirror.
 */
void geda_picture_object_mirror(TOPLEVEL *toplevel,
                            int world_centerx, int world_centery,
                            OBJECT *object)
{
  int newx1, newy1;
  int newx2, newy2;

  g_return_if_fail (object != NULL);
  g_return_if_fail (object->picture != NULL);
  g_return_if_fail (object->type == OBJ_PICTURE);

  /* Set info in object. Sometimes it's necessary to change the
   * rotation angle as well as the mirror flag. */
  object->picture->mirrored = !object->picture->mirrored;
  switch (object->picture->angle) {
  case 90:
    object->picture->angle = 270;
    break;
  case 270:
    object->picture->angle = 90;
    break;
  }

  /* translate object to origin */
  object->picture->upper_x -= world_centerx;
  object->picture->upper_y -= world_centery;
  object->picture->lower_x -= world_centerx;
  object->picture->lower_y -= world_centery;

  /* mirror the corners */
  newx1 = -object->picture->upper_x;
  newy1 = object->picture->upper_y;
  newx2 = -object->picture->lower_x;
  newy2 = object->picture->lower_y;

  /* reorder the corners */
  object->picture->upper_x = MIN(newx1,newx2);
  object->picture->upper_y = MAX(newy1,newy2);
  object->picture->lower_x = MAX(newx1,newx2);
  object->picture->lower_y = MIN(newy1,newy2);

  /* translate back in position */
  object->picture->upper_x += world_centerx;
  object->picture->upper_y += world_centery;
  object->picture->lower_x += world_centerx;
  object->picture->lower_y += world_centery;
}

/*! \brief Translate a picture position in WORLD coordinates by a delta.
 *  \par Function Description
 *  This function applies a translation of (<B>x1</B>,<B>y1</B>) to the picture
 *  described by <B>*object</B>. <B>x1</B> and <B>y1</B> are in world units.
 *
 *  \param [in,out] object     Picture GedaObject to translate.
 *  \param [in]     dx         x distance to move.
 *  \param [in]     dy         y distance to move.
 */
void
geda_picture_object_translate (GedaObject *object, int dx, int dy)
{
  g_return_if_fail (object != NULL);
  g_return_if_fail (object->picture != NULL);
  g_return_if_fail (object->type == OBJ_PICTURE);

  /* Do world coords */
  object->picture->upper_x = object->picture->upper_x + dx;
  object->picture->upper_y = object->picture->upper_y + dy;
  object->picture->lower_x = object->picture->lower_x + dx;
  object->picture->lower_y = object->picture->lower_y + dy;
}

/*! \brief Create a copy of a picture.
 *  \par Function Description
 *  This function creates a verbatim copy of the object pointed by
 *  <B>o_current</B> describing a picture.
 *
 *  \param [in]  toplevel   The TOPLEVEL object.
 *  \param [in]  object     Picture OBJECT to copy.
 *  \return The new OBJECT
 */
OBJECT *o_picture_copy(TOPLEVEL *toplevel, OBJECT *object)
{
  OBJECT *new_node;
  PICTURE *picture;

  /* create the object */
  new_node = s_basic_new_object(object->type, "picture");

  picture = (PICTURE*) g_malloc (sizeof(PICTURE));
  new_node->picture = picture;

  new_node->color = geda_object_get_color (object);
  new_node->selectable = geda_object_get_selectable (object);

  /* describe the picture with its upper left and lower right corner */
  picture->upper_x = object->picture->upper_x;
  picture->upper_y = object->picture->upper_y;
  picture->lower_x = object->picture->lower_x;
  picture->lower_y = object->picture->lower_y;

  if (object->picture->file_content != NULL) {
    picture->file_content = (gchar*) g_memdup (object->picture->file_content,
                                               object->picture->file_length);
  } else {
    picture->file_content = NULL;
  }

  picture->file_length = object->picture->file_length;
  picture->filename    = g_strdup (object->picture->filename);
  picture->ratio       = object->picture->ratio;
  picture->angle       = object->picture->angle;
  picture->mirrored    = object->picture->mirrored;
  picture->embedded    = object->picture->embedded;

  /* Get the picture data */
  picture->pixbuf = o_picture_get_pixbuf (toplevel, object);

  return new_node;
}

/*! \brief Embed the image file associated with a picture
 * \par Function Description
 * Verify that a picture has valid data associated with it, and if so,
 * mark it to be embedded.
 *
 *  \param [in]     toplevel     The TOPLEVEL object.
 *  \param [in]     object       The picture OBJECT to embed
 */
void o_picture_embed (TOPLEVEL *toplevel, OBJECT *object)
{
  const gchar *filename = o_picture_get_filename (object);
  gchar *basename;

  if (o_picture_is_embedded (object)) return;

  if (object->picture->file_content == NULL) {
    s_log_message (_("Picture [%1$s] has no image data."), filename);
    s_log_message (_("Falling back to file loading. Picture is still unembedded."));
    object->picture->embedded = 0;
    return;
  }

  object->picture->embedded = 1;

  basename = g_path_get_basename (filename);
  s_log_message (_("Picture [%1$s] has been embedded."), basename);
  g_free (basename);
}


/*! \brief Unembed a picture, reloading the image from disk
 * \par Function Description
 * Verify that the file associated with \a object exists on disk and
 * is usable, and if so, reload the picture and mark it as unembedded.
 *
 *  \param [in]     toplevel     The TOPLEVEL object.
 *  \param [in]     object       The picture OBJECT to unembed
 */
void o_picture_unembed (TOPLEVEL *toplevel, OBJECT *object)
{
  GError *err = NULL;
  const gchar *filename = o_picture_get_filename (object);
  gchar *basename;

  if (!o_picture_is_embedded (object)) return;

  o_picture_set_from_file (toplevel, object, filename, &err);

  if (err != NULL) {
    s_log_message (_("Failed to load image from file [%1$s]: %2$s"),
                   filename, err->message);
    s_log_message (_("Picture is still embedded."));
    g_error_free (err);
    return;
  }

  object->picture->embedded = 0;

  basename = g_path_get_basename(filename);
  s_log_message (_("Picture [%1$s] has been unembedded."), basename);
  g_free(basename);
}

/*! \brief Calculates the distance between the given point and the closest
 * point in the picture.
 *
 *  Interrior points within the picture return a distance of zero.
 *
 *  \param [in] toplevel     The TOPLEVEL object.
 *  \param [in] object       The picture OBJECT.
 *  \param [in] x            The x coordinate of the given point.
 *  \param [in] y            The y coordinate of the given point.
 *  \param [in] force_solid  If true, force treating the object as solid.
 *  \return The shortest distance from the object to the point. With an
 *  invalid parameter, this function returns G_MAXDOUBLE.
 */
double
geda_picture_object_shortest_distance (TOPLEVEL *toplevel, OBJECT *object,
                                       int x, int y, int force_solid)
{
  double dx, dy;
  double x1, y1, x2, y2;

  g_return_val_if_fail (object->picture != NULL, G_MAXDOUBLE);

  x1 = (double)MIN (object->picture->upper_x, object->picture->lower_x);
  y1 = (double)MIN (object->picture->upper_y, object->picture->lower_y);
  x2 = (double)MAX (object->picture->upper_x, object->picture->lower_x);
  y2 = (double)MAX (object->picture->upper_y, object->picture->lower_y);

  dx = MIN (((double)x) - x1, x2 - ((double)x));
  dy = MIN (((double)y) - y1, y2 - ((double)y));

  dx = MIN (dx, 0);
  dy = MIN (dy, 0);

  return hypot (dx, dy);
}

/*! \brief Test whether a picture object is embedded.
 * \par Function Description
 * Returns TRUE if the picture \a object will have its data embedded
 * in a schematic or symbol file; returns FALSE if its data will be
 * obtained from a separate file.
 *
 * \param object    The picture #OBJECT to inspect.
 * \return TRUE if \a object is embedded.
 */
gboolean
o_picture_is_embedded (const OBJECT *object)
{
  g_return_val_if_fail (object != NULL, FALSE);
  g_return_val_if_fail (object->picture != NULL, FALSE);

  return object->picture->embedded;
}

/*! \brief Get a pixel buffer for a picture object.
 * \par Function Description
 * Returns a #GdkPixbuf for the picture object \a object, or NULL if
 * the picture could not be loaded.
 *
 * The returned value should have its reference count decremented with
 * g_object_unref() when no longer needed.
 *
 * \param toplevel  The current #TOPLEVEL.
 * \param object    The picture #OBJECT to inspect.
 * \return A #GdkPixbuf for the picture.
 */
GdkPixbuf *
o_picture_get_pixbuf (TOPLEVEL *toplevel, OBJECT *object)
{
  g_return_val_if_fail (object != NULL, NULL);
  g_return_val_if_fail (object->picture != NULL, NULL);

  if (object->picture->pixbuf != NULL) {
    return GDK_PIXBUF (g_object_ref (object->picture->pixbuf));
  } else {
    return NULL;
  }
}

/*! \brief Get the raw image data from a picture object.
 * \par Function Description
 * Returns the raw image file data underlying the picture \a object,
 * or NULL if the picture could not be loaded.
 *
 * \param toplevel  The current #TOPLEVEL.
 * \param object    The picture #OBJECT to inspect.
 * \param len       Location to store buffer length.
 * \return A read-only buffer of raw image data.
 */
const char *
o_picture_get_data (TOPLEVEL *toplevel, OBJECT *object,
                    size_t *len)
{
  g_return_val_if_fail (object != NULL, NULL);
  g_return_val_if_fail (object->picture != NULL, NULL);

  *len = object->picture->file_length;
  return object->picture->file_content;
}

/*! \brief Set a picture object's contents from a buffer.
 * \par Function Description
 * Sets the contents of the picture \a object by reading image data
 * from a buffer.  The buffer should be in on-disk format.
 *
 * \param toplevel The current #TOPLEVEL.
 * \param object   The picture #OBJECT to modify.
 * \param filename The new filename for the picture.
 * \param data     The new image data buffer.
 * \param len      The size of the data buffer.
 * \param error    Location to return error information.
 * \return TRUE on success, FALSE on failure.
 */
gboolean
o_picture_set_from_buffer (TOPLEVEL *toplevel, OBJECT *object,
                           const gchar *filename,
                           const gchar *data, size_t len,
                           GError **error)
{
  GdkPixbuf *pixbuf;
  GInputStream *stream;
  gchar *tmp;

  g_return_val_if_fail (object != NULL, FALSE);
  g_return_val_if_fail (object->picture != NULL, FALSE);
  g_return_val_if_fail (data != NULL, FALSE);

  /* Check that we can actually load the data before making any
   * changes to the object. */
  stream = G_INPUT_STREAM (g_memory_input_stream_new_from_data (data, len, NULL));
  pixbuf = gdk_pixbuf_new_from_stream (stream, NULL, error);
  g_object_unref (stream);
  if (pixbuf == NULL) return FALSE;

  o_emit_pre_change_notify (object);

  if (object->picture->pixbuf != NULL) {
    g_object_unref (object->picture->pixbuf);
  }
  object->picture->pixbuf = pixbuf;

  object->picture->ratio = ((double) gdk_pixbuf_get_width(pixbuf) /
                            gdk_pixbuf_get_height(pixbuf));

  tmp = g_strdup (filename);
  g_free (object->picture->filename);
  object->picture->filename = tmp;

  gchar *buf = (gchar*) g_realloc (object->picture->file_content,
                                   len);
  /* It's possible that these buffers might overlap, because the
   * library user hates us. */
  memmove (buf, data, len);
  object->picture->file_content = buf;
  object->picture->file_length = len;

  o_emit_change_notify (object);
  return TRUE;
}

/*! \brief Set a picture object's contents from a file.
 * \par Function Description
 * Sets the contents of the picture \a object by reading image data
 * from a file.
 *
 * \param toplevel The current #TOPLEVEL.
 * \param object   The picture #OBJECT to modify.
 * \param filename The filename to load image data from.
 * \param error    Location to return error information.
 * \return TRUE on success, FALSE on failure.
 */
gboolean
o_picture_set_from_file (TOPLEVEL *toplevel, OBJECT *object,
                         const gchar *filename,
                         GError **error)
{
  gchar *buf;
  size_t len;
  gboolean status;

  g_return_val_if_fail (filename != NULL, FALSE);

  if (!g_file_get_contents (filename, &buf, &len, error)) {
    return FALSE;
  }

  status = o_picture_set_from_buffer (toplevel, object, filename,
                                      buf, len, error);
  g_free (buf);
  return status;
}

/*! \brief Get a picture's corresponding filename.
 * \par Function Description
 * Returns the filename associated with the picture \a object.
 *
 * \param object   The picture #OBJECT to inspect.
 * \return the filename associated with \a object.
 */
const gchar *
o_picture_get_filename (const GedaObject *object)
{
  g_return_val_if_fail (object != NULL, NULL);
  g_return_val_if_fail (object->picture != NULL, NULL);

  return object->picture->filename;
}

/*! \brief Get fallback pixbuf for displaying pictures.
 * \par Function Description
 * Returns a pixbuf containing the fallback image to be used if a
 * picture object fails to load.  The returned pixbuf should be freed
 * with g_object_unref() when no longer needed.
 *
 * \return a #GdkPixbuf containing a warning image.
 */
GdkPixbuf *
o_picture_get_fallback_pixbuf (TOPLEVEL *toplevel)
{
  static GdkPixbuf *pixbuf = NULL;
  static gboolean failed = FALSE;

  if (pixbuf == NULL && !failed) {

    pixbuf = gdk_pixbuf_new_from_xpm_data (fallback_xpm);

    if (pixbuf == NULL) {
      g_warning ( _("Failed to load fallback image.\n"));
      failed = TRUE;
    }
  }

  if (failed) return NULL;

  g_assert (GDK_IS_PIXBUF (pixbuf));
  return GDK_PIXBUF (g_object_ref (pixbuf));
}
