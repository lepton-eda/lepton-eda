/*******************************************************************************/
/*                                                                             */
/* Message box                                                                 */
/* Copyright (C) 2002 Piotr Miarecki, sp9rve@eter.ariadna.pl                   */
/*                                                                             */
/* This program is free software; you can redistribute it and/or               */
/* modify it under the terms of the GNU General Public License                 */
/* as published by the Free Software Foundation version 2.                     */
/*                                                                             */
/* This program is distributed in the hope that it will be useful,             */
/* but WITHOUT ANY WARRANTY; without even the implied warranty of              */
/* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the               */
/* GNU General Public License for more details.                                */
/*                                                                             */
/* You should have received a copy of the GNU General Public License           */
/* along with this program; if not, write to the Free Software                 */
/* Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA. */
/*                                                                             */
/*******************************************************************************/

#ifdef HAVE_CONFIG_H
#include "../config.h"
#endif
#include <gtk/gtk.h>
#include <stdlib.h>
#include <string.h>
#include "msgbox.h"




#if (HAS_GTK22 == 1)
#define DIALOG_MODE         GTK_DIALOG_MODAL
#else
#define DIALOG_MODE         GTK_WINDOW_DIALOG
#endif



/* 
	Additional message button defines used only locally
*/

#define MSGBOX_MAXBUTTONS    4
#define MSGBOX_BTN_WIDTH     96
#define MSGBOX_BTN_HEIGHT    32
#define MSGBOX_BTN_BORDER    8

#define MSGBOX_GOING         1
#define MSGBOX_DONE          2
#define MSGBOX_CANCELED      3

#define MSGBOX_NOTSELECTED   0x0000FFFFL



/* 
	Private functions and variables
*/

static void MsgBoxCreate(void);
static void MsgBoxButtonClicked(GtkButton *pButtonClicked, gpointer pUserData);

static GtkWindow *pParentWindow, *pWindow = NULL;
static unsigned long dwFlags;
static char *szTitle, *szMessage;
static int iNumber = 0;

static char szButton[8][32] = {
	"Ok",
	"Cancel",
	"Yes",
	"No",
	"Always Ok",
	"Always Cancel",
	"Always Yes",
	"Always No"
};

static GtkButton *pButton[MSGBOX_MAXBUTTONS];
static int iButton[MSGBOX_MAXBUTTONS];
static int iMsgBoxStatus;
static int iMsgBoxSelection = -1; 
static char *szXpmError[], *szXpmWarning[], *szXpmQuestion[], *szXpmMessage[];



/*
	Public functions
*/

int MsgBox(const GtkWindow *pArgParentWindow, const char *szArgTitle, const char *szArgMessage, const unsigned long dwArgFlags)
{
	int i;
	
	/* error if a window already displayed */
	if (pWindow != NULL)
		return MSGBOX_WAIT;
	
	/* save args for recreating the window */
	pParentWindow = GTK_WINDOW(pArgParentWindow);
	dwFlags = dwArgFlags;
	szTitle = (char *) malloc(strlen(szArgTitle) + 1);
	strcpy(szTitle, szArgTitle);
	szMessage = (char *) malloc(strlen(szArgMessage) + 1);
	strcpy(szMessage, szArgMessage);
	
	/* count buttons */
	for (iNumber = 0, i = 0; (1L << i) < MSGBOX__LAST; i ++)
	{
		if ((dwFlags & (1L << i)) == (1L << i))
			iNumber ++;
	}
	
	/* create message box */
	MsgBoxCreate();

	/* wait until selection is done (one or more buttons case) */
	if (iNumber > 0)
	{
		iMsgBoxStatus = MSGBOX_GOING;
		while (iMsgBoxStatus == MSGBOX_GOING)
			while (g_main_iteration(FALSE)) ;
	
		MsgBoxDestroy();
	}

	/* return selection */
	return (iNumber > 0) 
		? iMsgBoxSelection 
		: MSGBOX_WAIT;
}



void MsgBoxDestroy(void)
{
	if (pWindow == NULL)
		return;
	
	/* destroy widgets */
	iMsgBoxStatus = MSGBOX_DONE;
	gtk_widget_destroy(GTK_WIDGET(pWindow));
	while (g_main_iteration(FALSE)) ;

	/* free memory */
	pWindow = NULL;
	free(szTitle);
	free(szMessage);
}

	
	
/* 
	Private functions and variables
*/

static void MsgBoxButtonClicked(GtkButton *pButtonClicked, gpointer pUserData)
{
	int i;

	/* do not execute if destroying by MsgBoxDestroy() */
	if (iMsgBoxStatus != MSGBOX_GOING)
		return;
	
	/* look for the pressed button */
	for (i = 0; i < iNumber; i ++)
		if (pButtonClicked == pButton[i])
			break;

	/* a button pressed, end the loop in MsgBox() */
	if (i != iNumber)
	{
		iMsgBoxSelection = iButton[i];
		iMsgBoxStatus = MSGBOX_DONE;
	}
	
	/* no button pressed */
	else
	{
		/* recreate window */
		while (g_main_iteration(FALSE)) ; /* old window is being removed */
		MsgBoxCreate();
	}
}



static void MsgBoxCreate(void)
{
	GdkBitmap *pMask;
	GdkPixmap *pGdkPixmap;
	GtkLabel *pLabel;
	GtkPixmap *pPixmap;
	GtkStyle *pStyle;
	GtkHBox *pHBox, *pHBoxButtons;
	GtkVBox *pVBox, *pVBoxLeft, *pVBoxRight;
	unsigned long f;
	int iDefault = -1, i, iWidth, iHeight, iX, iY, iW, iH;
	char **szXpm;

	/* create buttons */
	for (iNumber = 0, i = 0; (1L << i) < MSGBOX__LAST; i ++)
	{
		if ((dwFlags & (1L << i)) == (1L << i))
		{
			pButton[iNumber] = GTK_BUTTON(gtk_button_new_with_label(szButton[i]));
			iButton[iNumber] = (1 << i);
			
			GTK_WIDGET_SET_FLAGS(pButton[iNumber], GTK_CAN_DEFAULT);
			
			f = 0x80000000L | ((unsigned long)i << 24);
			if ((dwFlags & f) == f)
				iDefault = iNumber;
			
			iNumber ++;
		}
	}
	
	/* create window */
	pWindow = GTK_WINDOW(gtk_window_new(DIALOG_MODE));
	iWidth = (iNumber > 3 ? iNumber : 4) * (MSGBOX_BTN_WIDTH + 2 * MSGBOX_BTN_BORDER);
	iHeight = 150 - (iNumber == 0 ? MSGBOX_BTN_HEIGHT + 2 * MSGBOX_BTN_BORDER : 0);
	gtk_widget_set_usize(GTK_WIDGET(pWindow), iWidth, iHeight);
	gtk_window_set_modal(GTK_WINDOW(pWindow), TRUE);
	gtk_window_set_position(GTK_WINDOW(pWindow), GTK_WIN_POS_MOUSE);
	gtk_window_set_transient_for(GTK_WINDOW(pWindow), GTK_WINDOW(pParentWindow));
	gtk_window_set_title(GTK_WINDOW(pWindow), szTitle);
	gtk_window_set_policy(GTK_WINDOW(pWindow), FALSE, FALSE, FALSE);
	gtk_widget_show(GTK_WIDGET(pWindow));
	while (g_main_iteration(FALSE)) ;
	gdk_window_get_size(GTK_WIDGET(pParentWindow)->window, &iW, &iH);
	gdk_window_get_position(GTK_WIDGET(pParentWindow)->window, &iX, &iY);
	gdk_window_move(
		GTK_WIDGET(pWindow)->window, 
		iX + (iWidth >= iW ? iWidth - iW : iW - iWidth) / 2,
		iY + (iHeight >= iH ? iHeight - iH : iH - iHeight) /2
		);
	
	/* create vertical container */
	pVBox = GTK_VBOX(gtk_vbox_new(FALSE, 0));
	gtk_container_add(GTK_CONTAINER(pWindow), GTK_WIDGET(pVBox));
	
	/* create horizontal container */
	pHBox = GTK_HBOX(gtk_hbox_new(FALSE, 0));
	gtk_box_pack_start(GTK_BOX(pVBox), GTK_WIDGET(pHBox), TRUE, TRUE, MSGBOX_BTN_BORDER);
	
	/* create picture */
	if (dwFlags & MSGBOX_FATAL)
		szXpm = szXpmError;
	if (dwFlags & MSGBOX_ERROR)
		szXpm = szXpmError;
	else if (dwFlags & MSGBOX_WARNING)
		szXpm = szXpmWarning;
	else if (dwFlags & MSGBOX_QUESTION)
		szXpm = szXpmQuestion;
	else
		szXpm = szXpmMessage;
	pStyle = gtk_widget_get_style(GTK_WIDGET(pParentWindow));
	pGdkPixmap = gdk_pixmap_create_from_xpm_d(
		GTK_WIDGET(pParentWindow)->window, 
		&pMask, 
		&pStyle->bg[GTK_STATE_NORMAL], 
		(gchar **) szXpm
		);
	pPixmap = GTK_PIXMAP(gtk_pixmap_new(pGdkPixmap, pMask));
	gtk_box_pack_start(GTK_BOX(pHBox), GTK_WIDGET(pPixmap), FALSE, FALSE, 0);
	gtk_pixmap_set(pPixmap, pGdkPixmap, pMask);
	gtk_widget_set_usize(GTK_WIDGET(pPixmap), MSGBOX_BTN_WIDTH, -2);
	
	/* create message label */
	pLabel = GTK_LABEL(gtk_label_new(szMessage));
	gtk_label_set_line_wrap(pLabel, TRUE);
	gtk_label_set_justify(GTK_LABEL(pLabel), GTK_JUSTIFY_LEFT);
	gtk_label_set_line_wrap(GTK_LABEL(pLabel), TRUE);
	gtk_box_pack_start(GTK_BOX(pHBox), GTK_WIDGET(pLabel), FALSE, TRUE, 0);
	
	/* create area for buttons */
	pHBoxButtons = GTK_HBOX(gtk_hbox_new(FALSE, 0));
	gtk_widget_set_usize(GTK_WIDGET(pHBoxButtons), -2, MSGBOX_BTN_HEIGHT);
	gtk_box_pack_start(GTK_BOX(pVBox), GTK_WIDGET(pHBoxButtons), FALSE, FALSE, MSGBOX_BTN_BORDER);

	/* place buttons */
	pVBoxLeft = GTK_VBOX(gtk_vbox_new(FALSE, 0));
	gtk_box_pack_start(GTK_BOX(pHBoxButtons), GTK_WIDGET(pVBoxLeft), TRUE, TRUE, 0);
	for (i = 0; i < iNumber; i ++)
	{
		gtk_widget_set_usize(GTK_WIDGET(pButton[i]), MSGBOX_BTN_WIDTH + 2 * MSGBOX_BTN_BORDER, MSGBOX_BTN_HEIGHT + 2 * MSGBOX_BTN_BORDER);
		gtk_signal_connect_object(GTK_OBJECT(pButton[i]), "clicked", GTK_SIGNAL_FUNC(MsgBoxButtonClicked), GTK_OBJECT(pButton[i]));
		gtk_box_pack_start(GTK_BOX(pHBoxButtons), GTK_WIDGET(pButton[i]), FALSE, FALSE, 0);
		gtk_container_set_border_width(GTK_CONTAINER(pButton[i]), 0);
	}
	pVBoxRight = GTK_VBOX(gtk_vbox_new(FALSE, 0));
	gtk_box_pack_start(GTK_BOX(pHBoxButtons), GTK_WIDGET(pVBoxRight), TRUE, TRUE, 0);

	/* default button */
	if (iDefault != -1)
	{
		gtk_widget_grab_focus(GTK_WIDGET(pButton[iDefault]));
		gtk_widget_grab_default(GTK_WIDGET(pButton[iDefault]));
	}
	
	/* rest */
	gtk_widget_show_all(GTK_WIDGET(pWindow));
	gtk_signal_connect_object_after(GTK_OBJECT(pWindow), "delete-event", GTK_SIGNAL_FUNC(MsgBoxButtonClicked), GTK_OBJECT(pWindow));
	while (g_main_iteration(FALSE)) ;
}



/*
	Pixmaps derived from gnome-libs package.
	See http://www.gnome.org
*/

static char *szXpmError[]={
"48 48 395 2",
"Qt c None",
"dS c #33100e",
"eh c #36120f",
"dI c #361210",
"e# c #401512",
"cP c #4e1915",
"dk c #4e1a16",
"bI c #581b16",
"ei c #581c17",
"eg c #5c1e1a",
"dZ c #5c1f1b",
"dv c #66221d",
"e. c #6b241e",
"cx c #6c221d",
"dR c #6d241f",
"dB c #722620",
"c4 c #752620",
"dH c #772822",
"ap c #7d231c",
"ch c #7f2821",
"ea c #81261e",
"d9 c #832b24",
"dY c #832c25",
"ef c #842a24",
"d0 c #85251c",
"cX c #87281e",
"aF c #88281f",
"b7 c #892b24",
"cY c #8a291f",
"eb c #8b2921",
"cO c #8b2c26",
"#U c #8c271d",
"dQ c #8d2e28",
"dD c #8f2a22",
"ec c #8f2b23",
"du c #8f2f28",
"dj c #8f3832",
"ai c #902419",
"cF c #902a1f",
"a6 c #902b22",
"ee c #902d26",
"dL c #90362f",
"d1 c #912920",
"bV c #912d25",
"ah c #922519",
"ay c #92251b",
"d8 c #933028",
"#X c #941f12",
"#2 c #942417",
"bH c #942d25",
"ed c #942e26",
"dX c #943029",
"dA c #94302a",
".s c #952011",
"cB c #95261b",
"bi c #952c23",
"cw c #953028",
".r c #961f11",
".S c #962011",
"#L c #962417",
"#3 c #962418",
"dT c #96291f",
"a# c #962920",
"bu c #962e24",
"dF c #963028",
"dG c #963029",
"d2 c #972c22",
"de c #972d25",
"dq c #972e26",
"dy c #972f27",
"ad c #981f11",
"cA c #98271b",
"cH c #982d23",
"cZ c #982d24",
"dE c #982e25",
"dM c #982e26",
"c3 c #983129",
"#u c #992416",
"ax c #99271b",
"cS c #99281c",
"#E c #99291e",
"co c #992b20",
"cq c #992c21",
"cr c #992c22",
"cl c #9a261b",
"cU c #9a291e",
"c7 c #9a2a1e",
"cn c #9a2b1f",
"dc c #9a2c23",
"cg c #9a3029",
"d7 c #9a312a",
"dP c #9a322a",
".6 c #9b2013",
"#l c #9b281c",
"az c #9b281d",
"dJ c #9b2a1f",
"ao c #9b2c21",
"at c #9c2112",
"#t c #9c2316",
"#M c #9c2518",
"ck c #9c271b",
"d# c #9c2c21",
"ct c #9c2d23",
"d3 c #9c2d24",
"c0 c #9c2f26",
"dz c #9c322a",
"#e c #9d2214",
"ae c #9d2314",
"aj c #9d271b",
"dU c #9d2b21",
"dp c #9d2e23",
"dd c #9d2f25",
"cJ c #9d2f26",
"df c #9d3027",
"cv c #9d3129",
"d6 c #9d3229",
"cN c #9d322a",
"dO c #9d332a",
"dt c #9d332b",
"dW c #9d3a32",
"cT c #9e291f",
"#o c #9e2a1e",
"#T c #9e2a20",
"#Y c #9e2e20",
"bG c #9e3026",
"bU c #9e3027",
"d4 c #9e3127",
"b6 c #9e3128",
"dg c #9e3129",
"ds c #9e3229",
".F c #9f2112",
"cC c #9f291e",
"da c #9f2d23",
"dV c #9f2e23",
"a5 c #9f2e24",
"bh c #9f2f25",
"bt c #9f2f26",
"cK c #9f3026",
"c1 c #9f3128",
"dr c #9f322a",
"cM c #9f332a",
"dh c #9f332b",
"dC c #9f3930",
"di c #9f3e36",
".T c #a02214",
"#f c #a02416",
"#K c #a02518",
"#C c #a02a1f",
"#D c #a02a20",
"#S c #a02b20",
"#9 c #a02c20",
"a. c #a02c21",
"an c #a02d22",
"do c #a02d23",
"aE c #a02e23",
"cs c #a02e24",
"cI c #a02f25",
"bT c #a03127",
"b5 c #a03128",
"cf c #a03229",
"dN c #a0322a",
"#4 c #a1281c",
"#m c #a1291d",
"#n c #a1291e",
"dw c #a12a1f",
"a4 c #a12d22",
"aD c #a12f22",
"db c #a12f24",
"bg c #a12f25",
"bs c #a13026",
"bF c #a13027",
"d. c #a1372d",
".t c #a22314",
"#v c #a22618",
".W c #a2271a",
"cR c #a2271b",
"#. c #a2281c",
"aw c #a2291c",
"c8 c #a22b20",
"aA c #a22d21",
"am c #a22d22",
"cp c #a22e23",
"cW c #a22e24",
"cG c #a22f24",
"cV c #a2362c",
"dK c #a23b32",
"d5 c #a23d35",
"c2 c #a23e36",
".G c #a32313",
".J c #a32518",
"cj c #a32618",
"#1 c #a3271a",
"ag c #a3281b",
"#Q c #a32b1f",
"#R c #a32c20",
"#8 c #a32c21",
"cE c #a32d21",
"c9 c #a3382e",
"ac c #a42212",
".5 c #a42313",
"#d c #a42415",
"#k c #a4291c",
"dm c #a4291d",
"## c #a42a1d",
"#A c #a42a1e",
"#B c #a42b1e",
"#5 c #a42b1f",
".q c #a52212",
"#g c #a52516",
"#h c #a52719",
"#w c #a5271a",
".9 c #a5281a",
".X c #a5281b",
".Y c #a5291b",
"#N c #a5291c",
"cu c #a53d34",
"cL c #a53e35",
".E c #a62212",
"as c #a62213",
".7 c #a62517",
".I c #a62617",
".V c #a62618",
".K c #a62718",
".L c #a62719",
"cD c #a6362b",
"dx c #a63b31",
".R c #a72212",
"#c c #a72414",
".H c #a72415",
".u c #a72516",
".U c #a72517",
"aB c #a7392f",
"dn c #a73a2f",
".3 c #a82211",
".p c #a82212",
".D c #a82312",
".4 c #a82313",
"#b c #a82314",
"#W c #a82414",
".M c #a8291b",
"c6 c #a8291c",
".Z c #a82a1d",
"#6 c #a8372c",
"#7 c #a8382d",
"ak c #a8382e",
"al c #a8392e",
".2 c #a92110",
".Q c #a92111",
".C c #a92211",
"aH c #a92413",
".v c #a92718",
"#I c #a92f20",
"#y c #a93529",
"#z c #a9362a",
"#O c #a9362b",
"#P c #a9372b",
"cm c #a9372c",
"aC c #a93f2f",
"#H c #aa2f20",
"af c #aa3326",
"au c #aa3327",
"#i c #aa3427",
"#j c #aa3428",
"#x c #aa3529",
".o c #ab2313",
".w c #ab2719",
"cz c #ab281a",
"#Z c #ab3224",
"#0 c #ab3225",
".8 c #ab3326",
".B c #ac2211",
"#r c #ac3021",
"#s c #ac3022",
"#J c #ac3123",
"cQ c #ad291b",
".N c #ad2a1b",
"dl c #ad2b1d",
"#V c #ae2212",
".n c #ae2312",
"ar c #af2413",
"av c #af3f33",
".A c #b12311",
".g c #b12615",
"bJ c #b12617",
"bW c #b12718",
".f c #b22514",
".1 c #b32310",
".m c #b32311",
".x c #b32a1a",
".h c #b42717",
"c5 c #b42c1d",
".e c #b52514",
"bj c #b52615",
"#q c #b62412",
"b8 c #b62819",
".P c #b92311",
".d c #ba2614",
"ci c #ba2b1a",
"ab c #bb2513",
".i c #bc2a18",
"a7 c #bd2716",
"#G c #be2512",
"a3 c #c1782e",
"ce c #c18138",
".z c #c32512",
".l c #c32613",
".c c #c32714",
"bE c #c49c3a",
"bD c #c59c39",
".j c #c62c1a",
"aG c #c72816",
"cy c #c72e1d",
"bC c #c79d37",
"aZ c #c89b30",
"bZ c #ca9e34",
"c# c #cb9d31",
"cb c #cba037",
"bX c #cc9d30",
"bN c #cc9f32",
"b9 c #cd8622",
"b0 c #cfa337",
"cc c #cfa338",
"b3 c #cfa53e",
"b4 c #cfa540",
".0 c #d02813",
"bz c #d0a131",
"bR c #d0a53d",
"bS c #d0a53f",
"bn c #d1a230",
"a0 c #d1a333",
"bY c #d1a436",
"ca c #d1a437",
"br c #d1a53c",
"aY c #d2a332",
"bO c #d2a435",
"bd c #d2a436",
"bf c #d2a539",
"cd c #d2aa48",
".b c #d32914",
"by c #d3a332",
"bM c #d3a433",
"a2 c #d3a638",
"be c #d3a63a",
"b2 c #d3aa47",
"aI c #d48d1d",
"a1 c #d4a637",
"b1 c #d4aa44",
"#a c #d52913",
"bA c #d5a533",
"bc c #d5a634",
"bm c #d5a82e",
"bQ c #d5ab43",
"bq c #d6a735",
"bP c #d6ab42",
"bb c #d7a734",
".O c #d82914",
"aX c #d8a733",
"bB c #d8ac40",
"aV c #d9a832",
"bo c #d9ab3e",
"aW c #d9ac32",
"bl c #daa82f",
"aU c #daac2f",
"bp c #daac3f",
"b. c #daae39",
"a9 c #dbac2e",
"ba c #dbad3e",
"b# c #dbaf3a",
"a8 c #dcad2d",
"c. c #dcb03b",
".k c #dd2a14",
"aq c #dd2d17",
"aP c #dda82a",
"aQ c #ddac2a",
"aT c #ddb03a",
"aO c #dead29",
"aR c #deb139",
"bK c #dfad36",
"aN c #dfae28",
"bL c #dfb036",
"bx c #dfb135",
"aS c #dfb445",
"aM c #e0ad25",
"bw c #e0b234",
"aL c #e1ae24",
"#p c #e22b15",
"bk c #e2b232",
"aK c #e3af22",
"aJ c #e4af21",
".y c #eb2d14",
".a c #ed2e17",
".# c #ee301a",
"bv c #ee321b",
"aa c #fa3118",
"#F c #fc3017",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt.#QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt.a.b.c.d.e.f.f.g.h.i.jQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt.k.l.m.n.o.p.q.r.s.t.u.u.v.w.xQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQt.y.z.A.B.C.p.D.E.F.G.H.u.u.I.J.K.L.M.NQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQt.O.P.B.Q.C.p.R.E.S.T.H.u.U.I.V.L.L.W.X.Y.ZQtQtQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQt.0.1.2.Q.3.p.4.4.5.6.5.u.7.I.V.8.8.9.X.Y#.####QtQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQt#a.A.2.C.p.p.4#b#c.H#d#e#f#g.V.L#h#i#j.Y#k#l#m#n#oQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQt#p#q.2.C.p.D.4#b#r#s.u.u#g#t#u#v#w.9.X#x#y#z#A#B#C#D#EQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQt#F#G.B.C.p.D.4#H#I.H#J#J.I.V.K#K#L#M.X#N#k###O#P#Q#R#S#T#UQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQt.b#V.3.p.4.4#W#X#Y#J.U#Z#0.L.L.9#1#2#3#4###A#5#6#7#8#9a.a#QtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtaaab.3.pacad#c.Hae.u.7#Z#0.8af.9.X.Yagahaiaj#5#Q#RakalamanaoapQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtaqar.R.4asat.H.u.u.I.V.V.8auav#j#N#k##awaxayaz#R#8aAaBaCaDaEaFQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtaGaHaIaJaJaKaLaLaMaNaNaOaPaQaRaSaTaUaVaWaXaYaZa0a1a1a2a3a4a5a6QtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQta7.4aJaKaKaLaLaMaNaNaOaQaQa8a9b.b#baaWaXbbbbbcbda1a2bebfbgbhbiQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtbj#caKaKaLbkaMaNaOaOaQa8a8a9blbmbnbobpbbbqbqa1a2a2bebrbrbsbtbuQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtbv.g.HaKaLaMaMbwbxaOaQa8a8a9aUaUaVbybzbAbBbBa1a2a2bCbDbDbEbFbGbHbIQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtbJ.uaLaMaNaNaObKbLa8a9a9aUaVaWaXbbbMbNbObPbQbQbebrbrbRbSbTbUbVQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtbW.uaMaNaNaOaQaQaRaTa9aUaVaWbXbbbbbqbYbZb0beb1b2bRbRb3b4b5b6b7QtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtb8.Ib9aOaOaQaQa8a9aTc.aVaWaXc#c#bqa1a1cacbccbrcdcdb3b4cecfcgchQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtci.Vcj.L.9.X.Yckcl###Ocm#Q#R#8cncocpcqcrcsctbhbFbTcucfcfcvcwcxQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtcycz#h.9.X.Y#k##cAcBcCcD#R#8cEamcFcFcGcHbgcIcJcKb5cfcLcMcNcOcPQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtcQ.9cR#N#k###A#BcScTcUcVa.amcpcWcXcYcZcZbFcKc0c1cfcMc2c3c4QtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtc5c6#N#k###A#B#Q#Rc7c8c9d.d#dadbdcddbsdeb5b5dfdgcMdhdidjdkQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtdldm#m#A#B#Q#R#RcncoamdndodpcsbsbsbTdqdqcfdrdsdhdtdudvQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQt###ndw#Q#R#8cEamcqcqcWdxbgbsbFbTb5cfdydrdhdhdzdAdBQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQt#C#C#S#8cEamcpcpcGbgdxdCdDdEb5cfcfdFdGdhdtdAdHdIQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtdJ#Sa.a.cpcWdbbgbscZdKdLdMcfdNcMdhdOdPdQdRdSQtQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtdTdUandodVbgbsbsbTdqdWcLdrcMcNdPdXdYdZQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtd0d1d2d3bhbhbGbUd4b6d5d6d7d8d9e.e#QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQteaebecbHededeecOefc4egehQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtbIeicPQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt"};

static char *szXpmWarning[]={
"48 48 186 2",
"Qt c None",
"#a c #000000",
".6 c #010000",
".Q c #010100",
"ac c #020100",
".Z c #020200",
".5 c #030100",
"#b c #040300",
"#F c #050400",
".X c #070400",
"#z c #090700",
"aj c #090701",
"#H c #0a0800",
"#A c #0b0900",
".R c #0e0b00",
"#G c #100d01",
".Y c #130f00",
"#L c #130f01",
"#S c #151001",
".7 c #191300",
"#h c #1d1602",
"#s c #1e1702",
"#6 c #241c03",
"## c #251d02",
"#t c #251d03",
"#g c #271e02",
"#c c #292003",
".4 c #2c2303",
"#Y c #312604",
"ar c #312605",
"#I c #332704",
"#Q c #382c05",
"ai c #3c2e05",
"#m c #403104",
".S c #403105",
"#u c #413206",
"#i c #423305",
"#n c #453506",
"ab c #4b3a07",
"a3 c #4d100c",
"#l c #4e3d06",
"#R c #513f07",
"#M c #524007",
"aS c #571109",
"aT c #5d110b",
"aU c #60120b",
"#X c #675009",
"ad c #69520a",
"aL c #6a130b",
"aV c #6e150c",
"aM c #6f140b",
"#T c #71570a",
"aW c #73160e",
"a2 c #731711",
"ak c #745a0c",
"aN c #78150c",
"aH c #781812",
"#P c #7a1710",
".W c #7a6109",
"ag c #7b1912",
"as c #7c610c",
"aX c #801810",
"a1 c #801a13",
"a0 c #851b14",
"aO c #86180d",
"#D c #861910",
"aZ c #861b13",
"#3 c #871b12",
"aY c #881a11",
"#q c #891a11",
".i c #8a180c",
"aP c #8a190f",
"aK c #8b1c15",
"am c #8c1c14",
"#V c #8d1b12",
"#e c #8f1b10",
".L c #901a0f",
".C c #911a0f",
".s c #921a0e",
"aQ c #931b10",
"a. c #971e15",
"#K c #981d14",
"aR c #981f17",
"#w c #991d13",
"af c #991f16",
".m c #9a1c0f",
"#1 c #9a1f15",
".e c #9b1b0e",
"#O c #9b1e14",
"aJ c #9b1f17",
"#C c #9c1e13",
"al c #9c1f16",
".9 c #9d1e12",
"#2 c #9d1f15",
".U c #9e1d11",
"#N c #9e1e14",
".B c #9f1d10",
"#v c #9f1e13",
".r c #a01d0f",
"#d c #a01e12",
".d c #a11c0e",
".E c #a11d10",
".T c #a11d11",
"#y c #a21d0f",
".F c #a21d10",
".y c #a31c0e",
".q c #a31d0f",
".h c #a41c0e",
".u c #a61d0f",
".c c #a71d0e",
".a c #a91d0f",
"aG c #ab4216",
".l c #b14010",
".2 c #b21f10",
".g c #b31f0f",
"#4 c #b62111",
".x c #b72010",
".k c #b9200f",
"aw c #b95b13",
".I c #b9930e",
".N c #ba2010",
".o c #bb5b11",
"#j c #bd2111",
"#W c #bd9312",
"av c #bf2212",
"au c #bf6f16",
"#9 c #c16916",
"ax c #c16f14",
".p c #c26f12",
".P c #c39b0f",
"#7 c #c49813",
".D c #c56b12",
"aF c #c57e16",
"ay c #c77e14",
"#Z c #c79a14",
"#. c #c82312",
"ao c #c87e14",
"ae c #c98517",
".n c #ca2310",
"#0 c #cb8617",
"az c #cb8914",
"#5 c #cb9e13",
"#p c #cc8614",
"#B c #cc8615",
"aI c #cd2513",
".0 c #cd8713",
".K c #cf8812",
"aa c #cf8c12",
"#k c #d08c13",
".V c #d12412",
".O c #d18912",
".# c #d22411",
"a# c #d22614",
".A c #d28814",
"#E c #d42513",
".b c #d52511",
"at c #d7a816",
"#8 c #d7a81a",
"an c #d82715",
"aE c #d8a51c",
"#U c #d8a919",
"aD c #d8a91a",
"#r c #d92613",
"aC c #d9aa19",
"aA c #daaa16",
"#J c #daaa17",
".t c #db2612",
"#o c #dbaa16",
"aB c #dbab17",
"#f c #dc2613",
".8 c #dcab15",
".H c #dcae11",
".G c #de2612",
".J c #deac13",
"aq c #dfad13",
".w c #dfb112",
".f c #e02712",
".v c #e0ad11",
"ap c #e0ae14",
".3 c #e0b213",
".z c #e1ae12",
".1 c #e82814",
"#x c #ea2915",
".j c #ec2913",
"ah c #ec2a16",
".M c #ef2914",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt.#.aQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt.b.c.d.eQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt.f.g.h.h.d.iQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt.j.k.h.h.l.h.mQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt.n.h.h.o.p.q.r.sQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt.t.u.h.h.v.w.q.q.rQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt.j.x.h.y.z.w.w.A.q.B.CQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt.n.h.h.D.w.w.w.w.E.F.BQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt.G.u.h.h.v.H.I.H.J.K.F.B.LQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt.M.N.h.h.O.P.Q.R.S.J.J.F.T.UQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt.V.h.h.h.w.W.X.Y.Z.J.J.0.T.U.LQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt.1.2.h.h.3.w.4.5.6.7.J.J.8.T.T.9QtQtQtQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt#..h.h.A.w.w###a#b#c.J.8.8.0#d.9#eQtQtQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt#f.u.h.q.w.w.w#g#a#h#i.8.8.8.8#d#d.9QtQtQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt#j.h.h#k.w.w.J#l#a#m#n.8.8.8#o#p#d.9#qQtQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQt#r.u.h.q.w.w.w.J#s.Q#t#u.8.8#o#o#o#d#v#wQtQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQt#x.x.h#y.3.w.w.J.J#z.Q#A#t.8#o#o#o#o#B#v#C#DQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQt#E.h.q.A.w.w.J.J.J#F#G#H#I#o#o#o#o#J#J#v#v#KQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQt#x.x.q.q.w.w.J.J.J.J#F#L#b#M#o#o#o#J#J#J#B#N#O#PQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQt#E.q.q#k.w.J.J.J.J.8#Q#R#S#T#o#o#J#J#J#J#U#N#N#VQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQt#x.x.q.q.w.J.J.J.J.8.8#W#X#Y#Z#o#o#J#J#J#U#U#0#N#1QtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQt#E.q.q#k.w.J.J.J.8.8.8.8#o#o#o#o#J#J#J#J#U#U#U#2#1#3QtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQt#x#4.q.q.w.J.J.J.8.8.8.8#5#t#6#7#J#J#J#J#U#U#U#8#9#2a.QtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQta#.q.qaa.J.J.J.J.8.8.8#oabacacad#J#J#J#U#U#U#U#8ae#2afagQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtah#4.q.q.J.J.J.J.8.8.8.8#oai#bajak#J#J#U#U#U#U#8#8#8#2alamQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtan.q.qaoapaq.J.8.8.8.8#o#o#Warasat#J#U#U#U#U#8#8#8#8aualafQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtav.q.F.F.F.F.T.T.TawaxayazaAaBaBaBaCaCaCaCaDaDaDaDaEaFaGafaHQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtaI.q.r.F.F.F.T.T.T.T#d#d#d#d#v#v#v#v#N#N#N#2#2#2#2alalalalaJaKQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtaLaMaNaOaPaQ.U.9#d#d#d#v#v#v#v#N#N#N#2#2#2#2alalalalaJaJaRQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtaSaTaUaVaWaWaWaXaYaYaYaY#3#3#3aZaZaZaZa0a0a1a2a3QtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt"};

static char *szXpmQuestion[]={
"48 48 73 2",
"Qt c None",
".# c #000000",
".Z c #19150d",
"#b c #1a1a1a",
"#d c #221c12",
"#c c #393939",
".G c #654f24",
".T c #6b5527",
".q c #6e5526",
"#g c #705627",
".N c #715929",
"## c #735f3b",
".f c #7c622d",
".3 c #7f6941",
".A c #80632d",
".F c #82662f",
".0 c #876a31",
".K c #8c6d31",
".y c #927335",
"#. c #927c52",
".t c #937435",
".x c #947536",
".4 c #977737",
".S c #997a3b",
".d c #9d7d3a",
".a c #a27f3b",
".C c #a38e55",
".D c #a48b5a",
".e c #a6823c",
"#a c #a68748",
".s c #a8843d",
".u c #aa863e",
".P c #b38d40",
".b c #b89243",
".O c #b99344",
".I c #be9544",
".j c #be9b53",
".7 c #bea272",
".w c #bea363",
".Y c #bfa16a",
".8 c #c09745",
".U c #c39f55",
".B c #c39f57",
".J c #c3a466",
".2 c #c6a76a",
".1 c #c9a45b",
".X c #c9ac73",
"#f c #d2a64c",
".E c #d2a64d",
".L c #d2b06c",
".c c #d8ab4e",
".R c #d8ac58",
".6 c #d8ac5b",
".p c #d8b15f",
".9 c #d8b163",
".5 c #d8b165",
".Q c #d8b56e",
".v c #d8b66e",
".i c #d8ba7b",
".M c #d8be86",
".W c #dab872",
".H c #dcb463",
".V c #dcc494",
"#e c #ddbc74",
".k c #f7c35a",
".z c #f7c96d",
".o c #f7cf7e",
".n c #f7d48c",
".g c #f7d899",
".r c #f7dca5",
".l c #f7dfaf",
".h c #f7e2b8",
".m c #f7e5c0",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt.#.#.#.#.#.#.#.#QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt.#.#.#.a.b.c.d.e.f.#.#.#QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt.#.#.c.g.g.h.g.i.i.i.j.e.#QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQt.#.#.k.l.m.g.n.k.k.k.o.i.p.e.q.#.#QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQt.#.k.l.r.k.s.t.t.u.k.k.o.v.p.e.#.#QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQt.#.w.g.h.k.x.#.#.#.#.y.k.k.z.p.p.A.#.#QtQtQtQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQt.#.z.m.o.x.#.#QtQtQt.#.B.k.o.c.p.e.#.#QtQtQtQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQt.#.C.n.l.c.#.#QtQtQtQt.#.D.k.z.p.c.e.#.#QtQtQtQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQt.#.E.o.k.e.#.#QtQtQtQt.#.D.z.z.v.c.F.#.#QtQtQtQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQt.#.k.o.k.e.#.#QtQtQtQt.#.D.k.v.v.c.G.#.#QtQtQtQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQt.#.H.p.I.F.#.#QtQtQtQt.#.J.z.i.c.e.#.#.#QtQtQtQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQt.#.#.#.#.#.#.#QtQtQtQt.K.L.M.p.c.N.#.#QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQt.#.#.#.#.#QtQtQtQt.#.O.n.M.c.I.#.#.#QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt.#.P.z.M.c.c.#.#.#QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt.#.#.k.n.Q.R.S.#.#QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt.T.U.V.W.X.Y.Z.#QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt.#.0.L.Q.1.2.3.#QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt.#.4.i.5.6.7.#.#QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt.#.8.i.9.6#..#QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt.#.c.M.p.c##.#QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt.#.j.i.c.I.#.#QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt.##a#a.e.A.#.#QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt.##b#c.#.#.#QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt.#.#.#.#QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt#d.#.#.#.#QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt.##e.o.n.I.#.#QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt.#.o.g.g.c.#.#QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt.#.n.n.p.I.#.#QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt.#.n.v.c.I.#.#QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt.##f.p.I#g.#.#QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt.#.#.#.#.#.#QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt.#.#.#.#QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt"};

static char *szXpmMessage[]={
"48 48 294 2",
"Qt c None",
"bV c #000000",
"cH c #010101",
"bt c #050504",
"bW c #080807",
"cB c #090908",
"cC c #0b0b09",
"cJ c #0c0c0b",
"bL c #0d0d0a",
"b7 c #0f0f0e",
"cv c #10100e",
"cr c #12120f",
"bb c #121211",
"bG c #131311",
"cA c #141411",
"ca c #141412",
"cq c #161613",
"bu c #161614",
"cI c #1a1a17",
"bq c #1d1d16",
"cx c #20201c",
"cw c #21211d",
"bC c #23231f",
"cs c #2c2c2a",
"bR c #2f2f29",
"cF c #30302f",
"cD c #31312b",
"cE c #33332f",
"ck c #333331",
"bk c #35352e",
"bc c #383526",
"cy c #383837",
"bF c #3d3d35",
"b3 c #42423a",
"co c #434342",
"cj c #44443b",
"cg c #464644",
"bS c #474742",
"b6 c #4b4b44",
"bB c #4d4c47",
"cG c #4e4e45",
"b2 c #4f4f4e",
"bM c #525248",
"cf c #525249",
"bv c #55554e",
"cp c #56564f",
"bA c #585851",
"be c #5a594c",
"bK c #5c5c5b",
"bd c #5f5e59",
"bp c #696964",
"c# c #696968",
"cu c #6b6b6a",
"ci c #6d6d6d",
"c. c #6f6f6f",
"b1 c #70706b",
"bT c #71716f",
"cz c #737373",
"cb c #75756c",
"bU c #757571",
"bJ c #7b7b7a",
"ch c #7f7f7b",
"bw c #8d8d85",
"bH c #8d8d87",
"b8 c #92928d",
"b9 c #929292",
"bX c #96968e",
"bl c #9d9d99",
"ct c #9e9e9a",
"bs c #a99959",
"bf c #a9a077",
"bE c #a9a9a4",
"af c #ab9b5b",
"b5 c #aeaea9",
"br c #b3a05e",
"cl c #b3b3b0",
"b0 c #b3b3b1",
"bm c #b4b4b2",
"aH c #b9a964",
"ce c #b9b9b1",
"bj c #baa864",
"b4 c #bcbcb9",
"#o c #c1ae67",
"bI c #c3c3c1",
"cc c #c3c3c2",
"#2 c #c4b269",
"aU c #c4b36b",
"#A c #c6b36a",
"bD c #c7c7c5",
"a5 c #c9b76d",
"ba c #cab871",
"bz c #cacac4",
"#N c #ccb96e",
"at c #cdbc70",
"bQ c #cfcfc8",
"cn c #cfcfce",
".Z c #d1bc70",
"a4 c #d1bf73",
"a3 c #d1c076",
"bx c #d4d4d1",
"bN c #d7d7d3",
"aG c #d9c577",
"ae c #d9c677",
"b# c #d9c679",
"aT c #dac777",
"aS c #dac87e",
"bo c #dadad8",
"bh c #dbc77c",
"#a c #dbc878",
"bi c #dbc97c",
"aF c #dbc97e",
"a2 c #dccb84",
".L c #dfcb80",
"bY c #dfdfde",
".m c #e2cd7f",
"b. c #e3d186",
"#n c #e4d07e",
"as c #e4d07f",
"#1 c #e4d17e",
".y c #e5d187",
"aE c #e5d48d",
"#z c #e6d27e",
"aR c #e6d68c",
"bg c #e7d180",
"#M c #e7d480",
".Y c #e8d68b",
"aQ c #e9da96",
"ar c #eada91",
".l c #ebdb99",
".d c #ecd78c",
"a# c #ede7ba",
".0 c #eed681",
"a1 c #eedb8b",
"#p c #efd881",
"#b c #efd882",
".n c #efd986",
".o c #efda8e",
"ad c #efdc8b",
".# c #efdd8f",
".K c #efdd92",
".x c #efdd93",
"bn c #efefee",
"cm c #efefef",
"aD c #f0df92",
"cd c #f0f0f0",
"ag c #f1dc86",
"a7 c #f1dfa1",
"## c #f1e08e",
"aP c #f1e195",
"aC c #f1e19b",
"ap c #f1e3a4",
"az c #f1e6ad",
"#3 c #f2dc8a",
"#0 c #f2e090",
"#L c #f2e093",
"a9 c #f2e097",
"aa c #f2e29a",
"aB c #f2e4a1",
"aV c #f3de87",
".M c #f3df8d",
".c c #f3e093",
"#m c #f3e290",
"#y c #f3e295",
"aO c #f3e296",
"a8 c #f3e3a7",
"aN c #f3e4a1",
"#X c #f3e6a3",
"a0 c #f3e6ad",
"ao c #f3e7ad",
"aY c #f3eac1",
".z c #f4de87",
".b c #f4e191",
".a c #f4e298",
"aq c #f4e395",
"aA c #f4e49d",
"#w c #f4e5a0",
".A c #f4e7b2",
"a6 c #f4e7bb",
"aZ c #f4e9c1",
"aM c #f4e9c4",
"#G c #f4eac9",
"#W c #f4eaca",
".e c #f5e190",
".1 c #f5e293",
".X c #f5e391",
"ac c #f5e393",
".k c #f5e49c",
"#I c #f5e7a2",
"a. c #f5e7b2",
"#v c #f5e9c2",
".J c #f6e38f",
"#Z c #f6e493",
"#O c #f6e496",
"#q c #f6e498",
"#c c #f6e499",
"ab c #f6e591",
"#Y c #f6e593",
"#K c #f6e59b",
"ah c #f6e59d",
"#J c #f6e697",
"#l c #f6e6a0",
"an c #f6e7a7",
"#k c #f6ebbe",
"by c #f6f6f6",
".w c #f7e493",
"#. c #f7e58f",
".9 c #f7e691",
"#x c #f7e695",
".N c #f7e69f",
".8 c #f7e7a0",
".f c #f7e7a5",
"#4 c #f7e8a6",
"au c #f7e8a7",
"#P c #f7eaaf",
"#H c #f7ebb1",
".7 c #f7eec3",
"ay c #f7eed9",
"#u c #f7efd0",
"aL c #f7efd8",
"bP c #f7f7f7",
".W c #f8e691",
".I c #f8e791",
".v c #f8e793",
".i c #f8e795",
".j c #f8e796",
".2 c #f8e8a9",
".g c #f8eaae",
".p c #f8eaaf",
"aW c #f8ebb1",
"#V c #f8ecb8",
"am c #f8ecb9",
"#d c #f8edba",
".O c #f8efcf",
"#6 c #f8f1db",
"aK c #f8f1dc",
"#j c #f8f2db",
"#t c #f8f2de",
".V c #f9e89b",
".u c #f9e89e",
".H c #f9e99b",
".h c #f9e9a7",
"aI c #f9ebb1",
"#8 c #f9efcb",
".q c #f9f0c7",
"#h c #f9f1db",
"aX c #f9f2db",
"#F c #f9f4e0",
".Q c #f9f4e2",
"#i c #f9f4e3",
"ak c #f9f4e5",
"ax c #f9f4e6",
"#7 c #f9f4e7",
".B c #f9f5e3",
".5 c #f9f5e4",
"#s c #f9f5e5",
".6 c #f9f5e6",
"#D c #f9f6e5",
"#R c #f9f6e7",
"#E c #f9f6ea",
".t c #faeaa6",
"#9 c #faecaf",
".U c #faedb7",
"#U c #faeebb",
"av c #faf2d2",
"ai c #faf2d3",
".s c #faf3d0",
".G c #faf3da",
"aJ c #faf3db",
"al c #faf3e5",
"#5 c #faf4d9",
".3 c #faf4e1",
".r c #faf5de",
"#e c #faf5e4",
".P c #faf5e5",
"#g c #faf5e6",
".S c #faf5e7",
"#S c #faf5e8",
".C c #faf6e6",
".4 c #faf6e7",
"#r c #faf6e8",
".T c #faf6e9",
"aw c #faf7ea",
"#C c #faf7eb",
".F c #faf8ec",
"#f c #fbf5e6",
".R c #fbf6e7",
"aj c #fbf7ea",
"#Q c #fbf7ec",
"#T c #fbf7ed",
".D c #fbf8ed",
"#B c #fbf8ee",
".E c #fbf9ef",
"bZ c #fbfbfb",
"bO c #fdfdfd",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt.#.a.b.c.dQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt.e.f.g.h.i.j.k.l.mQtQtQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt.n.o.p.q.r.s.t.u.v.w.x.yQtQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt.z.A.B.C.D.E.F.G.H.I.J.K.LQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt.M.N.O.P.Q.R.S.T.C.U.V.W.X.Y.ZQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt.0.1.2.3.4.Q.5.Q.6.S.7.8.9#.###aQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt#b#c#d#e.P#f#g#h#i#j#k#l.W.W#m#n#oQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt#p#q#d.5#r#r#s.Q#t#u#v#w#x#x#y#z#AQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt#p.1#d.S#B#C#D#E#F#G#H#I#J#K#L#M#NQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt#O#P.P#Q#R#S#T#U#V#W#X#Y#Z#0#1#2QtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt#3#4#5#B#6#7#8#9a.a#aaabacadaeafQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtagahaiajakalamanaoapaq.XarasatQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtauavawaxayazaAaBaCaDaEaFaGaHQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtaIaJaKaLaMaNaOaPaQaRaSaTaUQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtaVaWaXaYaZa0aC.#a1a2a3a4a5QtQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQta6a7a8a9.#b.b#baQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtbbbcbdbebfQtbgbhbibjQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtbkblbmbnbobpbqQtQtbrbsQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtbtbubvbwbxbybzbAbBQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtbCbDbEbFbGbHbIbJbKbLQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtbMbNbObPbQbRbSbTbUQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtbVbWbRbXbYbZb0b1b2b3QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtbVb4b5b6b7b8b9c.c#caQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtbVcbcccdcdcecfcgchciQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtbVbVcjckclcmcnc#cocpQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtbVcqcrbRcsctc.cubVQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtbVbVcvcwcxcyc#czbVQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtcAcBcqbGcCcDcEcFQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtcGcHcAcIcJQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt",
"QtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQtQt"};
