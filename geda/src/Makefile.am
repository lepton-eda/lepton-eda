## Process this file with automake to produce Makefile.in

## Created by Anjuta - will be overwritten
## If you don't want it to overwrite it,
## 	Please disable it in the Anjuta project configuration

INCLUDES = \
	$(GTK_CFLAGS)\
	 -I./ -I$(top_srcdir)/intl

localedir = @datadir@/locale
DEFS = -DPACKAGE_LOCALE_DIR=\"$(localedir)\" @DEFS@

AM_CFLAGS =\
	 -Wall -Wimplicit -Wreturn-type -Wunused -Wswitch -Wcomment -Wuninitialized -Wparentheses -Wpointer-arith -Wmissing-prototypes\
	 -O1\
	 -g

bin_PROGRAMS = geda

geda_SOURCES = \
	callbacks.h\
	callbacks.c\
	config.h\
	config.c\
	doc.h\
	doc.c\
	file.h\
	file.c\
	filesel.h\
	filesel.c\
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

geda_LDFLAGS =

geda_LDADD =  \
	$(GTK_LIBS)

MOSTLYCLEANFILES = *.log *~ geda
CLEANFILES = *.log *~ geda
DISTCLEANFILES = *.log core FILE *~ prototype.bak geda
MAINTAINERCLEANFILES = *.log *~ Makefile.in configure geda

