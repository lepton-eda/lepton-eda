# Copyright (C) 2013, 2014 Roland Lutz
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software Foundation,
# Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.

tests =

EXTRA_DIST = $(tests)

tty_colors = {								\
	color_tests=no;							\
	if test "X$(AM_COLOR_TESTS)" = Xno;				\
	then	color_tests=no;						\
	elif test "X$(AM_COLOR_TESTS)" = Xalways;			\
	then	color_tests=yes;					\
	elif test "X$$TERM" != Xdumb && { test -t 1; } 2>/dev/null;	\
	then	color_tests=yes;					\
	fi;								\
	if test $$color_tests = yes;					\
	then	red='[0;31m';						\
		grn='[0;32m';						\
		lgn='[1;32m';						\
		blu='[1;34m';						\
		mgn='[0;35m';						\
		brg='[1m';						\
		std='[m';						\
	else	mgn= red= grn= lgn= blu= brg= std=;			\
	fi;								\
}

installcheck-local:
	@$(tty_colors);							\
	for tc in $(tests);						\
	do	if PYTHONPATH=$(pyexecdir) python $(srcdir)/$$tc;	\
		then	echo "$${grn}PASS$${std}: $$tc";		\
		else	echo "$${red}FAIL$${std}: $$tc";		\
			exit 1;						\
		fi							\
	done
