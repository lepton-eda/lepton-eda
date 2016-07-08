# xorn.geda - Python library for manipulating gEDA files
# Copyright (C) 1998-2010 Ales Hvezda
# Copyright (C) 1998-2010 gEDA Contributors (see ChangeLog for details)
# Copyright (C) 2013-2016 Roland Lutz
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

## \namespace xorn.geda.clib
## The component library system
#
# The <b>component library</b> is made up of a number of <b>component
# sources</b>, each of which in turn makes available a number of
# component <b>symbols</b>.  Each source is represented by a Python
# object which implements the methods \c list() to retrieve a list of
# symbol names, and \c get(symbol) to retrieve the symbol data for a
# given symbol name.
#
# There are two predefined source types: a DirectorySource represents
# a directory on disk containing symbol files, and a CommandSource
# represents a command in the system \c PATH which can generate gEDA
# symbol data (e.g. from a database).
#
# Each symbol is identified by its name, which is stored in the saved
# schematic file.  The name must be valid for storage in a gEDA
# schematic file as the "basename" of a "component" object.  For
# symbols from directory sources, the filename of the symbol is taken
# as the symbol name.  For a command source, the name may be any
# permissible string.  Guidelines to follow:
#
#   -# Do not begin a symbol name with "EMBEDDED"
#   -# Do not use whitespace, or any of the characters "/", ":", "!",
#      "*", or "?".
#   -# Try to use unique names.
#
# The component database may be queried using \ref search.  A revision
# object containing the symbol data may be obtained using \ref
# get_symbol.  If the source of a symbol isn't known, the symbol data
# may be requested using the convenience function \ref lookup_symbol.

import collections, fnmatch, os, shlex, stat, subprocess, sys
from gettext import gettext as _
import xorn.geda.read
import xorn.geda.ref
import xorn.proxy
import xorn.storage

## All symbols in directory sources end with this string.
#
# Must be lowercase.

SYM_FILENAME_FILTER = '.sym'

## Named tuple class for storing data about a particular component source.

Source = collections.namedtuple('Source', ['callback', 'symbols', 'name'])

## List of source triples for all known component sources.

_sources = []

## Cache for search results of \ref search.
#
# The key of the hashtable is a pair <tt>(pattern, glob)</tt>
# describing the search that was carried out, and the value is a list
# of pairs <tt>(source, symbol)</tt>.

_search_cache = {}

## Symbol data cache.
#
# The key of the hashtable is a pair <tt>(source, symbol)</tt>, and
# the value is the data.

_symbol_cache = {}


## Source object representing a directory of symbol files.
#
# This class allows a directory which contains one or more symbol
# files in gEDA format to be used as a component source.  Only files
# ending in ".sym" (case insensitive) are considered to be symbol
# files.  Symbol files with filenames starting with a period "." are
# ignored.

class DirectorySource:
    def __init__(self, directory, recursive):
        ## Path to directory
        self.directory = directory
        ## Whether to recurse into subdirectories.
        self.recursive = recursive

    ## Scan the directory for symbols.

    def list(self):
        if not self.recursive:
            # skip subdirectories and anything else that isn't a
            # regular file (this is what libgeda does)
            entries = (entry for entry in os.listdir(self.directory)
                       if stat.S_ISREG(
                         os.stat(os.path.join(self.directory, entry)).st_mode))
        else:
            entries = (entry for dirpath, dirnames, filenames
                          in sorted(os.walk(self.directory))
                       for entry in sorted(filenames))

        return (entry for entry in entries
                # skip hidden files ("." and ".." are excluded by
                # os.walk but not by os.listdir)
                if entry[0] != '.'
                # skip filenames which don't have the right suffix
                and entry.lower().endswith(SYM_FILENAME_FILTER))

    ## Get symbol data for a given symbol name.

    def get(self, symbol):
        if not self.recursive:
            path = os.path.join(self.directory, symbol)
            if not os.path.isfile(path):  # resolves symlinks
                path = None
        else:
            path = None
            for dirpath, dirnames, filenames in \
                    sorted(os.walk(self.directory)):
                if symbol in filenames:
                    path = os.path.join(dirpath, symbol)
                    break

        if path is not None:
            return xorn.geda.read.read(path)

        raise ValueError, 'symbol "%s" not found in library' % symbol


## Source object representing a pair of symbol-generating commands.
#
# This class allows a program or pair of programs in the system search
# path which can generate symbols to be used as a component source.
#
# \a list_cmd and \a get_cmd should be pre-tokenized invocations
# consisting of an executable name followed by any arguments required.
# Executables are resolved using the current \c PATH.
#
# The list command will be executed with no additional arguments, and
# should output a list of available symbol names on the stdandard
# output.  The get command will have a symbol name appended to it as
# the final argument, and should output gEDA symbol data on standard
# output.
#
# If the command cannot successfully complete, it should exit with
# non-zero exit status.  Anything it has output on stdout will be
# ignored, so stderr should be used for diagnostics.

class CommandSource:
    def __init__(self, list_cmd, get_cmd):
        ## Command and arguments for listing available symbols
        self.list_cmd = list_cmd
        ## Command and arguments for retrieving symbol data
        self.get_cmd = get_cmd

    ## Poll the library command for symbols.
    #
    # Runs the library command, requesting a list of available
    # symbols, and returns the new list.

    def list(self):
        # Run the command to get the list of symbols
        lines = _run_source_command(shlex.split(self.list_cmd),
                                    lambda f: f.readlines())

        for line in lines:
            if not line or line[-1] != '\n':
                raise ValueError, "Missing newline at end of command output"
            if line == '\n':
                raise ValueError, "Command printed an empty line"
            if line[0] == '.':
                raise ValueError, "Command printed line starting with '.'"

        return (line[:-1] for line in lines)

    ## Get symbol data for a given symbol name.

    def get(self, symbol):
        return _run_source_command(
            shlex.split(self.get_cmd) + [symbol],
            lambda f: xorn.geda.read.read_file(f, '<pipe>'))


## Execute a library command.
#
# Executes the command given by the first item of the argument
# sequence \a args, using the system search path to find the program
# to execute.  Calls the function \a callback with a file-like object
# representing the pipe connected to the command's standard output as
# a single argument.  Once \a callback finishes, discards all data
# left on the pipe and waits for the program to terminate.
#
# \returns the value returned by \a callback
#
# \throws ValueError if the command returns a non-zero exit status or
#                    is terminated by a signal

def _run_source_command(args, callback):
    p = subprocess.Popen(
        args, bufsize = 4096, executable = executable,
        stdout = subprocess.PIPE, close_fds = True) # cwd = virtual_cwd

    try:
        return callback(p.stdout)
    finally:
        try:
            p.read()  # avoid deadlock
        finally:
            p.wait()
            if p.returncode < 0:
                raise ValueError, "Library command failed [%s]: " \
                    "Uncaught signal %i" % (args[0], -p.returncode)
            if p.returncode != 0:
                raise ValueError, "Library command failed [%s]: "\
                    "returned exit status %d" % (args[0], p.returncode)

## Update list of symbols available from a component source.
#
# Calls \c source.callback.list() and performs type and uniqueness
# checks on the returned list.  If no errors are found, replaces the
# list of available symbols with the returned list.
#
# \throws TypeError if \a symbols is not iterable
# \throws TypeError if \a symbols yields an item that isn't a string
# \throws ValueError if \a symbols yields a duplicate item

def _update_symbol_list(source):
    try:
        symbols = list(source.callback.list())
    except TypeError:
        raise TypeError, "Failed to scan library [%s]: " \
            "Python function returned non-list" % source.name

    found = set()
    for symbol in symbols:
        if not isinstance(symbol, str) and \
           not isinstance(symbol, unicode):
            raise TypeError, "Non-string symbol name " \
                "while scanning library [%s]" % source.name
        if symbol in found:
            raise ValueError, "Duplicate symbol name " \
                "while scanning library [%s]: %s" % (symbol, source.name)
        found.add(symbol)

    symbols.sort()
    source.symbols[:] = symbols


## Add a component source to the library.
#
# \a callback must implement two methods: \c callback.list() to return
# a list of symbol names, and \c callback.get(symbol) to return the
# symbol data for a given a symbol name.
#
# \param callback  source object which implements \c list and \c get
# \param name      unique descriptive name for the component source
#
# \throws ValueError if another source with this name already exists

def add_source(callback, name):
    if name is None:
        raise ValueError, "Cannot add source: name not specified"
    for source in _sources:
        if source.name == name:
            raise ValueError, "There is already a source called '%s'" % name

    # Sources added later get scanned earlier
    source = Source(callback, [], name)
    _update_symbol_list(source)
    _sources.insert(0, source)

    _search_cache.clear()
    _symbol_cache.clear()

## Get a component source by name.
#
# Iterates through the known component sources, checking if there is a
# source with the given \a name.
#
# \throws ValueError if no matching source was found

def lookup_source(name):
    for source in _sources:
        if source.name == name:
            return source

    raise ValueError

## Make sure a source name is unique.
#
# Checks if a source with the given \a name already exists.  If it
# does, appends a number in angular brackets to the source name making
# it unique.  If \a name is not already in use, returns it as is.

def uniquify_source_name(name):
    i = 0
    newname = name

    while True:
        try:
            lookup_source(newname)
        except ValueError:
            break
        i += 1
        newname = '%s<%i>' % (name, i)

    if i != 0:
        sys.stderr.write(_("Library name [%s] already in use.  Using [%s].\n")
                         % (name, newname))
    return newname

## Rescan all available component libraries.
#
# Resets the list of symbols available from each source, and
# repopulates it from scratch.  Useful e.g. for checking for new
# symbols.

def refresh():
    for source in _sources:
        _update_symbol_list(source)

    _search_cache.clear()
    _symbol_cache.clear()

## Remove all component library sources.

def reset():
    del _sources[:]
    _search_cache.clear()
    _symbol_cache.clear()


## Get symbol object for a given source object and symbol name.
#
# Returns a xorn.geda.ref.Symbol object containing the symbol called
# \a symbol from the component source \a source.
#
# \throws ValueError if the source object's \c get function doesn't
#                    return a xorn.storage.Revision or
#                    xorn.proxy.RevisionProxy instance

def get_symbol(source, symbol):
    assert source is not None
    assert symbol is not None

    # First, try the cache.
    try:
        return _symbol_cache[id(source), symbol]
    except KeyError:
        pass

    # If the symbol wasn't found in the cache, get it directly.
    data = source.callback.get(symbol)
    if isinstance(data, xorn.proxy.RevisionProxy):
        data = data.rev
    if not isinstance(data, xorn.storage.Revision):
        raise ValueError, "Failed to load symbol data [%s] " \
            "from source [%s]" % (symbol, source.name)
    data.finalize()

    symbol_ = xorn.geda.ref.Symbol(symbol, data, False)

    # Cache the symbol data
    _symbol_cache[id(source), symbol] = symbol_

    return symbol_

## Invalidate cached data about a symbol.

def invalidate_symbol_data(source, symbol):
    try:
        del _symbol_cache[id(source), symbol]
    except KeyError:
        pass


## Find all symbols matching a pattern.
#
# Searches the library, returning all symbols whose names match \a
# pattern.  If \a glob is \c True, then \a pattern is assumed to be a
# glob pattern; otherwise, only exact matches are returned.
#
# \returns a list of pairs <tt>(source, symbol)</tt>

def search(pattern, glob = False):
    # Check to see if the query is already in the cache
    try:
        return _search_cache[pattern, glob]
    except KeyError:
        pass

    result = []
    for source in _sources:
        if glob:
            for symbol in fnmatch.filter(source.symbols, pattern):
                result.append((source, symbol))
        elif pattern in source.symbols:
            result.append((source, pattern))

    _search_cache[pattern, glob] = result
    return result[:]

## Get source for a given symbol name.
#
# Returns the source of the first symbol found with the given \a name.
# If more than one matching symbol is found, emits a warning to stderr.
#
# \throws ValueError if the component was not found

def lookup_symbol_source(name):
    symlist = search(name)

    if not symlist:
        raise ValueError, "Component [%s] was not found in the " \
            "component library" % name

    if len(symlist) > 1:
        sys.stderr.write(_("More than one component found "
                           "with name [%s]\n")% name)

    source, symbol = symlist[0]
    assert symbol == name
    return source

## Get symbol object for a given symbol name.
#
# Returns the xorn.geda.ref.Symbol object for the first symbol found
# with the given name.  This is a helper function for the schematic
# load system, as it will always want to load symbols given only their
# name.
#
# \throws ValueError if the component was not found

def lookup_symbol(name):
    return get_symbol(lookup_symbol_source(name), name)


## Return a list of symbols used in a revision.
#
# The list is free of duplicates and preserves the order of the
# symbols as they appear first in the file.  Each symbol is
# represented by its actual xorn.geda.ref.Symbol object.

def used_symbols0(rev):
    symbols = []
    for ob in rev.all_objects():
        data = ob.data()
        if isinstance(data, xorn.storage.Component) \
               and data.symbol not in symbols:
            symbols.append(data.symbol)
    return symbols

## Return a list of symbols used in a revision.
#
# Scan a revision looking for symbols, look them up in the library,
# and return them as a list.  Each symbol is represented by a pair
# <tt>(source, symbol)</tt>.  The returned list only contains symbols
# that were found in the library and is sorted in alphabetical order.
#
# \bug Only includes components which are not embedded, but they
#      should (probably) also appear in the list.

def used_symbols1(rev):
    result = []

    for ob in rev.all_objects():
        data = ob.data()
        if not isinstance(data, xorn.storage.Component):
            continue

        # ignore embedded symbols
        if data.symbol.embedded:
            continue

        # Since we're not looking at embedded symbols, the first
        # component with the given name will be the one we need.
        # N.b. we don't use lookup_symbol_source() because it's
        # spammeh.
        symlist = search(data.symbol.basename)
        if not symlist:
            continue
        result.append(symlist[0])

    result.sort(key = lambda (source, symbol): symbol)
    return result
