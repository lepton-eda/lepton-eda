## Process this file with automake to produce Makefile.in

bin_PROGRAMS = gnetlist 

# don't forget all *.h files */
gnetlist_SOURCES = \
		i_vars.c g_netlist.c g_rc.c g_register.c globals.c \
		gnetlist.c parsecmd.c s_cpinlist.c s_net.c \
		s_netlist.c s_netattrib.c s_traverse.c \
		s_rename.c s_hierarchy.c s_misc.c vams_misc.c

if CCISGCC
AM_CFLAGS = -Wall
endif

INCLUDES = -I$(top_srcdir)/include @GNETLIST_CFLAGS@
gnetlist_LDADD = @GNETLIST_LDFLAGS@

EXTRA_DIST = create_proto 

CPROTO =        cproto
CPROTOCFLAGS =  $(INCLUDES) -I. -I.. -I../include
ALES_HACK =     -D__GNUC__

proto: prototype.h

prototype.h: $(gschem_SOURCES)
	$(CPROTO) $(ALES_HACK) $(CPROTOCFLAGS) *.c > new_proto
	mv new_proto ../include/prototype.h

MOSTLYCLEANFILES = *.log *.ps core FILE *~ prototype.bak
CLEANFILES = *.log *.ps core FILE *~ prototype.bak
DISTCLEANFILES = *.log core FILE *~ prototype.bak
MAINTAINERCLEANFILES = *.log *.ps core FILE *~ prototype.bak prototype.h Makefile.in configure

