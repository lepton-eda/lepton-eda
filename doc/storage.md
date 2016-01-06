Storage library {#storage}
===============

This library provides a medium-level interface for accessing and
manipulating file contents in memory.  Its purpose is to allow code
parts developed independently from each other to use the same memory
representation of objects while being able to rely on some invariants.
In particular, it should be possible to access and manipulate objects
without knowledge of the other modules' object system or metadata.

In practice, this means:

- An object is represented by exactly the information which is
  required to describe it.  Information which is only meaningful to
  some code parts is not managed by the library, such as pointers to
  shared data or weak references.  For example, the data structure
  representing a schematic net is:
\code{.c}
    struct xornsch_net {
        struct xorn_double2d pos;
        struct xorn_double2d size;
        int color;
        bool is_bus;
        bool is_pin;
        bool is_inverted;
    };
\endcode

- File contents are referred to by \ref xorn_revision_t "revisions"
  which are basically copy-on-write smart pointers.  Once a revision
  is marked as “final”, it is not possible make further changes to it
  by means of the library's functions.  (But it is of course possible
  to create a copy of the revision and modify that.)  This allows to
  keep tabs on a certain state of a file, making it easy to implement
  an undo/redo functionality.

- The actual data an object has in a revision is looked up and changed
  using a \ref xorn_object_t or \ref xorn_selection_t key which
  describes the “identity” of one or multiple objects, respectively.

The library's interface is designed to anticipate the operations which
an application will typically perform on the file's contents and
provide abstract functions which express these intentions to allow the
library to do some optimization.  For example, to delete all circles
and arcs with radius zero, instead of

\snippet motivation.c discrete

you would write:

\snippet motivation.c integrated

The implementation could now, for example, choose to express the
selection as a filter which can be applied to individual objects
rather than as a list of object pointers, if this should prove more
efficient.  This way, the application doesn't have to know about but
can still benefit from optimizations to the potentially messy detail
of how the data is internally stored and accessed.

\sa xornstorage.h
\sa \ref using-storage


--------------------------------------------------------------------------------

Copyright (C) 2013-2016 Roland Lutz

Permission is granted to copy, distribute and/or modify this document
under the terms of the [GNU Free Documentation License, Version 1.2]
(http://www.gnu.org/licenses/old-licenses/fdl-1.2.html) or any later
version published by the Free Software Foundation; with no Invariant
Sections, with no Front-Cover Texts, and with no Back-Cover Texts.
