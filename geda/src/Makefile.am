## Process this file with automake to produce Makefile.in

## Created by Anjuta - will be overwritten
## If you don't want it to overwrite it,
## 	Please disable it in the Anjuta project configuration

INCLUDES = \
	$(GTK_CFLAGS)\
	 -I../include/

AM_CFLAGS =\
	 -Wall -Wimplicit -Wreturn-type -Wunused -Wswitch -Wcomment -Wuninitialized -Wparentheses -Wtraditional -Wpointer-arith -Wmissing-prototypes\
	 -O1\
	 -g

bin_PROGRAMS = geda

geda_SOURCES = \
	support.h\
	support.c\
	main.c\
	interface.h\
	interface.c\
	callbacks.h\
	config.c\
	doc.c\
	file.c\
	filesel.c\
	filetool.c\
	l_toolbar.c\
	log.c\
	m_action.c\
	m_project.c\
	m_window.c\
	callbacks.c\
	msgbox.c\
	project.c\
	task.c\
	tool.c\
	txtedit.c\
	window.c

geda_LDFLAGS = 

geda_LDADD =  \
	$(GTK_LIBS)

