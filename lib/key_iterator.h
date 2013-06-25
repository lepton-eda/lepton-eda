/* Copyright (C) 2013 Roland Lutz

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software Foundation,
   Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.  */

#ifndef KEY_ITERATOR_H
#define KEY_ITERATOR_H

#include <iterator>

template<typename I> class key_iterator {
	I i;
public:
	typedef typename std::iterator_traits<I>::value_type::first_type
		    value_type;
	typedef typename std::iterator_traits<I>::difference_type
		    difference_type;
	typedef value_type *pointer;
	typedef value_type &reference;
	typedef std::bidirectional_iterator_tag iterator_category;

	key_iterator(I i) : i(i) {
	}
	key_iterator &operator++() {
		i.operator++();
		return *this;
	}
	key_iterator &operator--() {
		i.operator--();
		return *this;
	}
	bool operator==(key_iterator x) {
		return i.operator==(x.i);
	}
	bool operator!=(key_iterator x) {
		return i.operator!=(x.i);
	}
	value_type operator*() {
		return i.operator*().first;
	}
};

template<typename I> static key_iterator<I> iterate_keys(I const &i) {
	return key_iterator<I>(i);
}

#endif
