## Process this file with automake to produce Makefile.in

bin_PROGRAMS = gnetlist 

bin_SCRIPTS = gschem2pcb

# don't forget all *.h files */
gnetlist_SOURCES = \
		i_vars.c g_netlist.c g_rc.c g_register.c globals.c \
		gnetlist.c parsecmd.c s_cpinlist.c s_net.c \
		s_netlist.c s_netattrib.c s_project.c s_traverse.c \
		s_rename.c

#INCLUDES = -I../include @GUILE_INCLUDE@ @X_CFLAGS@  

INCLUDES = @GUILE_INCLUDE@ @X_CFLAGS@  
gnetlist_LDADD = @GUILE_LIB@ @GLIB@ @X_LDFLAGS@ 

# original gnetlist which links in everything
#gnetlist_LDADD = @STROKE_LIB@ @GUILE_LIB@ @GTK@ @GDK@ @GLIB@ @X_LDFLAGS@ 

man_MANS = gnetlist.1

EXTRA_DIST = create_proto gnetlist.1

MOSTLYCLEANFILES = *.log *.ps core FILE *~ prototype.bak
CLEANFILES = *.log *.ps core FILE *~ prototype.bak
DISTCLEANFILES = *.log *.ps core FILE *~ prototype.bak
MAINTAINERCLEANFILES = *.log *.ps core FILE *~ prototype.bak prototype.h

