## Process this file with automake to produce Makefile.in

bin_PROGRAMS = gsymcheck 

# don't forget all *.h files */
gsymcheck_SOURCES = \
		g_rc.c g_register.c globals.c gsymcheck.c \
		parsecmd.c s_check.c s_project.c s_symstruct.c

#INCLUDES = -I../include @GUILE_INCLUDE@ @X_CFLAGS@  

INCLUDES = @GUILE_INCLUDE@ @X_CFLAGS@  
gsymcheck_LDADD = @GLIB@ @X_LDFLAGS@ @GUILE_LIB@ 

man_MANS = gsymcheck.1

EXTRA_DIST = create_proto gsymcheck.1

MOSTLYCLEANFILES = *.log *.ps core FILE *~ prototype.bak
CLEANFILES = *.log *.ps core FILE *~ prototype.bak
DISTCLEANFILES = *.log *.ps core FILE *~ prototype.bak Makefile.in configure
MAINTAINERCLEANFILES = *.log *.ps core FILE *~ prototype.bak prototype.h Makefile.in configure

