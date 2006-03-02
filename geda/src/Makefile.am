# gManager Makefile.am

INCLUDES = @GMANAGER_CFLAGS@ -I./ -I$(top_srcdir)/intl

localedir = @datadir@/locale
DEFS = -DPACKAGE_LOCALE_DIR=\"$(localedir)\" @DEFS@

if CCISGCC
AM_CFLAGS =\
	 -Wall -Wimplicit -Wreturn-type -Wunused -Wswitch -Wcomment -Wuninitialized -Wparentheses -Wpointer-arith -Wmissing-prototypes -g -O
endif

bin_PROGRAMS = geda

geda_SOURCES = \
	callbacks.h\
	callbacks.c\
	configfile.h\
	configfile.c\
	doc.h\
	doc.c\
	file.h\
	file.c\
	filechoose.h\
	filechoose.c\
	filetool.h\
	filetool.c\
	global.h\
	interface.h\
	interface.c\
	l_toolbar.h\
	l_toolbar.c\
	log.h\
	log.c\
	m_action.h\
	m_action.c\
	m_file.h\
	m_file.c\
	m_project.h\
	m_project.c\
	m_window.h\
	m_window.c\
	main.c\
	msgbox.h\
	msgbox.c\
	project.h\
	project.c\
	support.h\
	support.c\
	task.h\
	task.c\
	tool.h\
	tool.c\
	txtedit.h\
	txtedit.c\
	window.h\
	window.c

geda_LDFLAGS = @GMANAGER_LDFLAGS@

geda_LDADD = @GTK_LIBS@ @LIBINTL@

MOSTLYCLEANFILES = *.log *~ geda
CLEANFILES = *.log *~ geda
DISTCLEANFILES = *.log core FILE *~ prototype.bak geda
MAINTAINERCLEANFILES = *.log *~ Makefile.in configure geda
