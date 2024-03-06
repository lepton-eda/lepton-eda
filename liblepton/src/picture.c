/* Lepton EDA library
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2016 gEDA Contributors
 * Copyright (C) 2021-2024 Lepton EDA Contributors
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
/*! \file picture.c
 */

#include "config.h"

#include "liblepton_priv.h"



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



/*! \brief Allocate a picture
 *
 *  \return a pointer to a picture, which must be freed with lepton_picture_free.
 */
LeptonPicture*
lepton_picture_new ()
{
  return g_new0 (LeptonPicture, 1);
}

/*! \brief Free memory associated with the picture
 *
 *  \param [in] picture the picture to be freed
 */
void
lepton_picture_free (LeptonPicture *picture)
{
  if (picture) {

    g_free (picture->file_content);

    if (picture->pixbuf) {
      g_object_unref (picture->pixbuf);
    }

    g_free (picture->filename);
    g_free (picture);
  }
}


/*! \brief Get fallback pixbuf for displaying pictures.
 * \par Function Description
 * Returns a pixbuf containing the fallback image to be used if a
 * picture object fails to load.  The returned pixbuf should be freed
 * with g_object_unref() when no longer needed.
 *
 * \return a \c GdkPixbuf containing a warning image.
 */
GdkPixbuf *
lepton_picture_get_fallback_pixbuf ()
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
