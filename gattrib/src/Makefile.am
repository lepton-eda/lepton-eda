## Process this file with automake to produce Makefile.in
## This Makefile.am created for gattrib by SDB 12.13.2003

AM_CFLAGS = -g -O2 

bin_PROGRAMS = gattrib

## don't forget all *.h files */
if GTK22_SOURCE
gattrib_SOURCES = \
		gtksheet_2_2.c \
		gtkextra-marshal.c \
                gtkitementry_2_2.c \
		globals.c \
		listsort.c \
		parsecmd.c \
		g_register.c \
		g_rc.c \
		i_vars.c \
		i_basic.c \
		x_window.c \
		x_gtksheet.c \
		x_dialog.c \
		x_fileselect.c \
		s_rename.c \
		s_misc.c \
		s_string_list.c \
		s_sheet_data.c \
		s_table.c \
		s_object.c \
		s_attrib.c \
		s_toplevel.c \
		gattrib.c 
else
gattrib_SOURCES = \
		gtksheet_1_2.c \
                gtkitementry_1_2.c \
		globals.c \
		listsort.c \
		parsecmd.c \
		g_register.c \
		g_rc.c \
		i_vars.c \
		i_basic.c \
		x_window.c \
		x_gtksheet.c \
		x_dialog.c \
		x_fileselect.c \
		s_rename.c \
		s_misc.c \
		s_string_list.c \
		s_sheet_data.c \
		s_table.c \
		s_object.c \
		s_attrib.c \
		s_toplevel.c \
		gattrib.c 
endif


INCLUDES = -I$(top_srcdir)/include @GATTRIB_CFLAGS@
gattrib_LDFLAGS = @GATTRIB_LDFLAGS@
gattrib_LDADD = @GATTRIB_LDFLAGS@

## Don't need these right now.
## man_MANS = gattrib.1
## EXTRA_DIST = gattrib.1 

MOSTLYCLEANFILES = *.log *.ps core FILE *~ #*# 
CLEANFILES = *.log *.ps core FILE *~ #*#
DISTCLEANFILES = *.log core FILE *~ #*#
MAINTAINERCLEANFILES = *.log *.ps core FILE *~ #*#






