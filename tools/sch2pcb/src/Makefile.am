bin_PROGRAMS = lepton-sch2pcb
lepton_sch2pcb_SOURCES = lepton-sch2pcb.c

lepton_sch2pcb_CPPFLAGS = \
	-DPCBM4DIR=\"$(PCBM4DIR)\" \
	-DPCBLIBPATH=\"$(PCBLIBPATH)\" \
	-I$(top_srcdir)
lepton_sch2pcb_CFLAGS = $(GLIB_CFLAGS)
lepton_sch2pcb_LDFLAGS = $(GLIB_LIBS)

AM_CFLAGS = $(GCC_CFLAGS) $(MINGW_CFLAGS)
AM_CPPFLAGS = -I$(top_srcdir)/liblepton/include \
	-I$(top_srcdir) -I$(includedir)
