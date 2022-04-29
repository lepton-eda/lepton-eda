Lepton Electronic Design Automation
===================================
[![Join the chat at https://gitter.im/Lepton-EDA](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/Lepton-EDA)

Introduction
============

Lepton EDA is a suite of free software tools for designing
electronics. It provides schematic capture, netlisting into
over 30 netlist formats, and many other features. It was forked
from the [gEDA/gaf suite](http://wiki.geda-project.org/geda:gaf)
in late 2016 by most of its active developers at that time.

The GPL Electronic Design Automation (gEDA) project has produced and
continues working on a full GPL'd suite and toolkit of Electronic
Design Automation tools. These tools are used for electrical circuit
design, schematic capture, simulation, prototyping, and
production. Currently, the gEDA project offers a mature suite of free
software applications for electronics design, including schematic
capture, attribute management, bill of materials (BOM) generation,
netlisting, analog and digital
simulation, and printed circuit board (PCB) layout.

The gEDA project was started because of the lack of free EDA tools for
POSIX systems with the primary purpose of advancing the state of free
hardware or open source hardware. The suite is mainly being developed
on the GNU/Linux platform with some development effort going into
making sure the tools run on other platforms as well.

Compatibility with geda-gaf
===========================

Lepton EDA is backward compatible with its predecessor `geda-gaf`
and supports the same file format for symbols and schematics.  We
are planning to support it in future since there are lots of
symbols and schematics created using it.  We cannot promise you to
support any change in `geda-gaf` in future, though.  It's up to
you, our users, to point out what feature you would like to have
in Lepton.

Tools in the Lepton EDA suite
=============================

The major components of the Lepton suite are:

- `liblepton`
  - A library of functions for manipulating Lepton schematics and symbols
    and rendering primitives.

- `libleptongui`
  - A library of GUI functions.

- `lepton-schematic`
  - A schematic editor.

- `lepton-attrib`
  - A spreadsheet-like program for bulk editing of component attributes.

- `lepton-netlist`
  - A highly-flexible, hierarchy-aware utility which parses schematics
    to generate a number of outputs, including netlists for a wide
    variety of PCB layout tools.  It can also generate bills of
    materials and DRC reports for your schematics.

- `lepton-sch2pcb`
  - A command-line utility for streamlining the workflow where
    'PCB' <http://pcb.geda-project.org/> and `lepton-schematic`
    are used together.

- `lepton-symcheck`
  - A utility for checking for common errors in schematic symbol files.

- `lepton-cli`
  - A utility for interactive and batch mode working with Lepton
    EDA Scheme API, exporting schematics into various formats, and
    configuring all the programs of the suite.

Installation
============

Dependencies
------------

In order to compile Lepton EDA from the distributed source
archives, you *must* have the following tools and libraries
installed:

- A C/C++ compiler and standard library (GCC/glibc are recommended).

- The `pkg-config` tool for managing shared libraries.
  <http://pkgconfig.freedesktop.org/>

- Guile ("GNU's Ubiquitous Intelligent Language for Extensions"),
  version 2.2.0 or later.  <http://www.gnu.org/software/guile/>

- GTK+ (the Gimp Toolkit), version 2.24.0 or later.
  <http://www.gtk.org/>

- GNU `gettext`, version 0.18 or newer.
  <http://www.gnu.org/software/gettext/>

- The `lex` tool for generating lexical scanners.  The `flex`
  implementation recommended.  <http://flex.sourceforge.net/>

- The `awk` tool for data processing.  GNU Awk (`gawk`) is
  recommended.  <http://www.gnu.org/software/gawk/>

The following tools and libraries are **highly recommended**:

- GNU `troff` (`groff`). <http://www.gnu.org/software/groff/>

- The freedesktop.org MIME info database.
  <http://freedesktop.org/Software/shared-mime-info>

- The freedesktop.org utilities for manipulating `.desktop` files.
  <http://www.freedesktop.org/software/desktop-file-utils>

The following tools and libraries are optional:

- `libstroke`, a stroke and gesture recognition library.  If this is
  available, `lepton-schematic` will support mouse gesture recognition.
  <http://www.etla.net/libstroke/>

- The `doxygen` API documentation tool.  This is required for
  building the Lepton developer API documentation, not for the
  regular user documentation.  <http://www.doxygen.nl>

- 'Inkscape' or 'ImageMagick' for svg to png or pdf conversion
  This is required for building the Lepton developer API documentation,
  not for the regular user documentation.
  <http://inkscape.org/>
  <http://www.imagemagick.org/script/index.php>

- 'Graphviz' for drawing directed graphs.
  This is required for building the Lepton developer API documentation,
  not for the regular user documentation.
  <http://www.graphviz.org/>

Troubleshooting dependencies
----------------------------

> "I've installed the `libfoo` library, but `./configure` isn't
> picking it up!"

Many modern operating system distributions split a library into two
packages:

1. a `libfoo` package, which contains the files necessary to
   *run* programs which use `libfoo`.

2. a `libfoo-dev` or `libfoo-devel` package, which contains the files
   necessary to *compile* programs which use `libfoo`.

If you're having problems, make sure that you have all of the
necessary `dev` or `devel` packages installed.

> "I have installed two versions of guile, but `./configure`
> uses a wrong one!"

Specify right `guile` binary on the `configure` stage, e.g.:

    ./configure GUILE=/usr/bin/guile-2.2

Installation from a source archive
----------------------------------

First extract the archive to a sensible place:

    tar -xzvf lepton-eda-<version>.tar.gz && cd lepton-eda-<version>

Run the configuration script.  You'll probably want to specify a
custom directory to install Lepton to, for example:


    ./configure --prefix=$HOME/lepton

You can then compile Lepton:

    make

And install it (if you used a `--prefix` outside your $HOME directory,
you may need to run this as root):

    make install

Installation from the git repository
------------------------------------

Lepton uses the `git` version control system.  If you wish to try out
the very latest version of Lepton, you will need to install some extra
tools *in addition to* the ones listed above:

- The `git` version control tool, version 1.6 or newer.
  <http://git-scm.com/>

- GNU Automake, version 1.11.0 or newer.
  <http://www.gnu.org/software/automake/>

- GNU Autoconf, version 2.60 or newer.
  <http://www.gnu.org/software/autoconf/>

- GNU Libtool.  <http://www.gnu.org/software/libtool/>

- GNU Texinfo documentation system.
  <http://www.gnu.org/software/texinfo/>

  Note that on some distributions the TeX support for Texinfo is
  packaged separately.

Once you have these installed, you need to clone the Lepton git
repository:

    git clone https://github.com/lepton-eda/lepton-eda.git

To generate the configure script, run:

    ./autogen.sh

You can then proceed to configure and build Lepton as described above.

Noticeable configure options
----------------------------

### --enable-guild

The time of Scheme code compilation when you launch a Lepton
program first time can be significantly reduced if you compile the
code beforehand:

    ./configure --enable-guild

This option enables in-advance Scheme code compilation, which may
be of help for end users though not always desirable for Lepton
developers.

### --with-gtk3

You can build Lepton with GTK3 GUI support if you enable it on the
configure stage:

    ./configure --with-gtk3

Please note that GTK2 and GTK3 version of *lepton-attrib* require
different additional libraries.  `libgtkextra` is required for the
GTK2 version, and `gtksheet` for GTK3.  The latter can be found
[here](https://github.com/fpaquet/gtksheet).

### --disable-attrib

Installation of *lepton-attrib* can be disabled on the configure
stage:

    ./configure --disable-attrib


### --enable-contrib

This option enables build of several contributed
tools that may come in handy for some tasks.  Please see Lepton
reference manual for more information on what utilities are
available.


Building Lepton developer API documentation
===========================================

Several of the Lepton libraries and applications have doxygen API
documentation available.  To generate the API documentation from the
source code, install doxygen (see [Dependencies](#Dependencies) above.
Next, add `--enable-doxygen` to your `configure` command line, i.e.:

    ./configure --enable-doxygen

To compile the documentation (quite a slow process), run:

    make doxygen

The documentation can then be found in:

    */docs/html/index.html

Getting help
============

There are several ways to get help with installing and using Lepton
and the rest of the gEDA tools:

- Chat with Lepton developers and users at
  [gitter.im](https://gitter.im/Lepton-EDA/Lobby).
  You can sign in there using your Twitter, GitHub or GitLab account
  (no registration is required to read the messages posted there).
  This is probably the fastest way to get in touch with us.

- If you prefer using IRC, there is now a Lepton channel on OFTC:
  irc.oftc.net#lepton-eda. Sometimes you can catch Lepton users or
  developers there.

- [Lepton documentation page](https://lepton-eda.github.io)
  is the first place to search for Lepton specific information.

- [Lepton EDA wiki](https://github.com/lepton-eda/lepton-eda/wiki)
  is a resource containing additional information that did not get
  into the main documentation.

- The gEDA website <http://www.geda-project.org> has more extensive
  information on the gEDA tools, and links to some successful
  projects which use gEDA.

- The gEDA documentation wiki contains a large amount of helpful
  information.  A static copy is included with this distribution; see
  the `docs/wiki/index.html` file.  The wiki is accessible online at
  <http://wiki.geda-project.org/>.

- If the resources above didn't help you resolve your problem, or you
  are having a *design* problem that you want to get help with,
  consider subscribing to and posting your question to the
  `geda-user` mailing list
  (<http://wiki.geda-project.org/geda:mailinglists>).
  You can also [browse](http://www.delorie.com/archives/browse.cgi?p=geda-user)
  and [search](http://www.delorie.com/archives) the mailing list archives.

- If you have discovered a bug, have a feature request, or have
  written a patch to Lepton, please create an item on the lepton-eda
  bug tracker page:
  <https://github.com/lepton-eda/lepton-eda/issues>

License
=======

Lepton EDA (this package) is freely distributable under the GNU Public
License (GPL) version 2.0 or (at your option) any later version.  See
the `COPYING` file for the full text of the license.

The programs and associated files are:

  Copyright (C) 1998-2017 by Ales Hvezda and the respective original
  authors.

  Copyright (C) 2017-2022 Lepton Developers.

See the `AUTHORS` file for a more extensive list of contributors to
Lepton EDA and gEDA.
