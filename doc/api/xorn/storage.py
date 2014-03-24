# Copyright (C) 2013, 2014 Roland Lutz
#
# Permission is granted to copy, distribute and/or modify this document
# under the terms of the GNU Free Documentation License, Version 1.2 or
# any later version published by the Free Software Foundation; with no
# Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
# Texts.  A copy of the license is included in the ``GNU Free
# Documentation License'' file as part of this distribution.

## \file storage.py
#  \brief Placeholder file for xorn.storage documentation.
#
# This file DOES NOT contain the actual source code of the
# xorn.storage module.  It contains documented stubs of the code from
# which the Doxygen documentation is generated.  For the actual
# definition of the module, see the Python extension in \c src/cpython/.

## \namespace xorn.storage
#  \brief Xorn storage backend.
#
# Python extension providing access to the storage library.
#
# \sa \ref storage
# \sa xornstorage.h

## \brief A particular state of the contents of a file.

class Revision:
    ## \brief Create a new revision, either from scratch or by copying
    #         an existing one.
    #
    # \param rev %Revision to copy, or \c None.
    #
    # There is a slight difference between creating two empty
    # revisions and copying an empty one: only in the second case,
    # objects of one revision will be valid in the other.
    #
    # \throw MemoryError if there is not enough memory

    def __init__(self, rev = None):
        ## \brief Whether the revision is transient.
        #
        # Reading and writing this is equivalent to calling \ref
        # is_transient and \ref finalize.  Cannot be set to \c True.

        self.transient = True

    ## \brief Return whether a revision can be changed.
    #
    # When a revision is created, it is initially \a transient,
    # i.e. changeable.  This can be changed by calling \ref finalize.
    # After that, it can't be changed directly any more---you will
    # have to create a transient copy if you want to change it again.

    def is_transient(self):
        pass

    ## \brief Prevent further changes to a revision.
    #
    # When a revision is created, it is initially \a transient,
    # i.e. changeable.  However, it is typically not desired for a
    # revision to change once it is in its desired state.  Using this
    # function, you can prevent further changes to the revision.  It
    # will still be possible to create a copy of the revision and
    # change that.
    #
    # \return \c None

    def finalize(self):
        pass

    ## \brief Return a list of all objects in a revision.
    #
    # The objects are returned in their actual order.
    #
    # \throw MemoryError if there is not enough memory
    #
    # Example:
    # \snippet functions.py get objects

    def get_objects(self):
        pass

    ## \brief Return whether an object exists in a revision.

    def object_exists(self, ob):
        pass

    ## \brief Get the data of an object in a revision.
    #
    # Changing the returned data will not have an effect on the
    # object; use \ref set_object_data to change the object.
    #
    # \return Returns an instance of the appropriate data class.
    #
    # \throw KeyError    if \a ob doesn't exist in the revision
    # \throw ValueError  if the object type is not supported
    #                    (should not happen)
    # \throw MemoryError if there is not enough memory
    #
    # Example:
    # \snippet functions.py get/set object data

    def get_object_data(self, ob):
        pass

    ## \brief Get the index of an object in a revision's object list.
    #
    # \return Returns the index of \a ob in the object list.
    #
    # \throw KeyError if \a ob doesn't exist in the revision

    def get_object_location(self, ob):
        pass

    ## \brief Add a new object to a transient revision.
    #
    # The object is appended to the end of the object list.
    #
    # \a data must be an instance of one of the object data types
    # (Arc, Box, Circle, Component, Line, Net, Path, Picture, or Text).
    #
    # \return Returns the new object.
    #
    # \throw ValueError  if the revision isn't transient
    # \throw TypeError   if \a data doesn't have a valid type
    # \throw MemoryError if there is not enough memory
    #
    # Example:
    # \snippet functions.py add object

    def add_object(self, data):
        pass

    ## \brief Set the data of an object in a transient revision.
    #
    # If the object does not exist in the revision, it is created and
    # appended to the end of the object list.
    #
    # \param ob   An object which has previously been returned by a Xorn
    #             function for either this revision, one of its
    #             ancestors, or a revision which has a common ancestor
    #             with it.
    #
    # \param data An instance of one of the object data types (Arc,
    #             Box, Circle, Component, Line, Net, Path, Picture, or
    #             Text).  The type may be different from the previous
    #             type of the object.
    #
    # \return \c None
    #
    # \throw ValueError  if the revision isn't transient
    # \throw TypeError   if \a data doesn't have a valid type
    # \throw MemoryError if there is not enough memory
    #
    # Example:
    # \snippet functions.py get/set object data

    def set_object_data(self, ob, data):
        pass

    ## \brief Change the location of an object in the object structure
    #         of a transient revision.
    #
    # Changes the order in which an object is drawn and written to
    # files as compared to its sibling objects.
    #
    # If \a ob and \a insert_before are identical, the revision is
    # left unchanged.
    #
    # \param ob             The object which should be reordered
    # \param insert_before  An object before which \a ob should be
    #                       inserted, or \c None to append it at the end
    #
    # \return \c None
    #
    # \throw ValueError  if the revision isn't transient
    # \throw KeyError    if \a ob or (if not \c None) \a insert_before
    #                    doesn't exist in the revision
    # \throw MemoryError if there is not enough memory

    def relocate_object(self, ob, insert_before):
        pass

    ## \brief Copy an object to a transient revision.
    #
    # The object is appended to the end of the object list.
    #
    # \param self Destination revision (must be transient)
    # \param rev  Source revision (does not need to be transient)
    # \param ob   %Object in the source revision which should be copied
    #
    # \return Returns the new object.
    #
    # \throw ValueError  if \a self isn't transient
    # \throw KeyError    if \a ob doesn't exist in \a rev
    # \throw MemoryError if there is not enough memory

    def copy_object(self, rev, ob):
        pass

    ## \brief Copy some objects to a transient revision.
    #
    # The objects are appended to the end of the object list in an
    # unspecified order.
    #
    # \param self Destination revision (must be transient)
    # \param rev  Source revision (does not need to be transient)
    # \param sel  Objects in the source revision which should be copied
    #
    # \return Returns a selection containing the new objects.
    #
    # \throw ValueError  if \a self isn't transient
    # \throw MemoryError if there is not enough memory

    def copy_objects(self, rev, sel):
        pass

    ## \brief Delete an object from a transient revision.
    #
    # \a ob will stay valid and can later be re-added using \ref
    # set_object_data.
    #
    # \return \c None
    #
    # \throw ValueError if the revision isn't transient
    # \throw KeyError   if \a ob doesn't exist in the revision

    def delete_object(self, ob):
        pass

    ## \brief Delete some objects from a transient revision.
    #
    # Objects that don't exist in the revision are ignored.
    #
    # The objects will stay valid and can later be re-added using \ref
    # set_object_data.
    #
    # \return \c None
    #
    # \throw ValueError if the revision isn't transient

    def delete_objects(self, sel):
        pass

## \brief The identity of an object across revisions.
#
# A value of this type is used as a key to look up and change the
# state of an object in a revision.  It is created by
# Revision.add_object, Revision.copy_object, or (indirectly)
# Revision.copy_objects or returned by one of the other functions of
# this module.
#
# This type can't be directly instantiated.

class Object:
    ## \brief x.__cmp__(y) <==> cmp(x,y)
    def __cmp__(...):
        pass

## \brief The identity of a set of objects across revisions.
#
# A value of this type is used as a set of keys for mass object
# inspection or manipulation and does not designate a specific order
# of the objects.  It is created using one of the \c
# select_<em>something</em> class of functions.
#
# This type can't be directly instantiated.

class Selection:
    pass

## \brief Return a list of objects which are in a revision as well as
#         in a selection.
#
# The objects are not necessarily returned in a meaningful order.
#
# \throw MemoryError if there is not enough memory

def get_selected_objects(rev, sel):
    pass

## \brief Return a list of objects which are in one revision but not
#         in another.
#
# Returns objects in \a to which are not in \a from.  They are not
# necessarily returned in a meaningful order.
#
# \throw MemoryError if there is not enough memory

def get_added_objects(from, to):
    pass

## \brief Return a list of objects which are in one revision but not
#         in another.
#
# Returns objects in \a from which are not in \a to.  They are not
# necessarily returned in a meaningful order.
#
# \throw MemoryError if there is not enough memory

def get_removed_objects(from, to):
    pass

## \brief Return a list of objects which exist in two revisions but
#         have different type or data.
#
# The objects are not necessarily returned in a meaningful order.
#
# \throw MemoryError if there is not enough memory

def get_modified_objects(from, to):
    pass

## \brief Return an empty selection.
#
# \throw MemoryError if there is not enough memory

def select_none():
    pass

## \brief Return a selection containing a single object.
#
# \throw MemoryError if there is not enough memory

def select_object(ob):
    pass

## \brief Return a selection containing all objects in a revision.
#
# \throw MemoryError if there is not enough memory

def select_all(rev):
    pass

## \brief Return a selection containing all objects in a revision
#         except those in a given selection.
#
# \throw MemoryError if there is not enough memory

def select_all_except(rev, sel):
    pass

## \brief Return a selection which contains all the objects in an
#         existing selection plus a given object.

def select_including(sel, ob):
    pass

## \brief Return a selection which contains all the objects in an
#         existing selection minus a given object.

def select_excluding(sel, ob):
    pass

## \brief Return a selection containing the objects in either given
#         selection.
#
# \throw MemoryError if there is not enough memory

def select_union(sel0, sel1):
    pass

## \brief Return a selection containing the objects in both given
#         selections.
#
# \throw MemoryError if there is not enough memory

def select_intersection(sel0, sel1):
    pass

## \brief Return a selection containing the objects contained in one
#         given selection, but not the other.

def select_difference(sel0, sel1):
    pass

## \brief Return whether a selection is empty in a given revision.

def selection_is_empty(rev, sel):
    pass

## \brief Return whether an object exists in a revision and is
#         selected in a selection.

def object_is_selected(rev, sel, ob):
    pass

## \brief Schematic line style.

class LineAttr:
    def __init__(self):
        self.width = 0.
        self.cap_style = 0
        self.dash_style = 0
        self.dash_length = 0.
        self.dash_space = 0.

## \brief Schematic fill style.

class FillAttr:
    def __init__(self):
        self.type = 0
        self.width = 0.
        self.angle0 = 0
        self.pitch0 = 0.
        self.angle1 = 0
        self.pitch1 = 0.

## \brief Schematic arc.

class Arc:
    def __init__(self):
        self.x = 0.
        self.y = 0.
        self.radius = 0.
        self.startangle = 0
        self.sweepangle = 0
        self.color = 0
        self.line = LineAttr()

## \brief Schematic box.

class Box:
    def __init__(self):
        self.x = 0.
        self.y = 0.
        self.width = 0.
        self.height = 0.
        self.color = 0
        self.line = LineAttr()
        self.fill = FillAttr()

## \brief Schematic circle.

class Circle:
    def __init__(self):
        self.x = 0.
        self.y = 0.
        self.radius = 0.
        self.color = 0
        self.line = LineAttr()
        self.fill = FillAttr()

## \brief Schematic component.

class Component:
    def __init__(self):
        self.x = 0.
        self.y = 0.
        self.selectable = False
        self.angle = 0
        self.mirror = False

## \brief Schematic line.

class Line:
    def __init__(self):
        self.x = 0.
        self.y = 0.
        self.width = 0.
        self.height = 0.
        self.color = 0
        self.line = LineAttr()

## \brief Schematic net segment, bus segment, or pin.

class Net:
    def __init__(self):
        self.x = 0.
        self.y = 0.
        self.width = 0.
        self.height = 0.
        self.color = 0
        self.is_bus = False
        self.is_pin = False
        self.is_inverted = False

## \brief Schematic path.

class Path:
    def __init__(self):
        self.pathdata = ""
        self.color = 0
        self.line = LineAttr()
        self.fill = FillAttr()

## \brief Schematic picture.

class Picture:
    def __init__(self):
        self.x = 0.
        self.y = 0.
        self.width = 0.
        self.height = 0.
        self.angle = 0
        self.mirror = False

## \brief Schematic text or attribute.

class Text:
    def __init__(self):
        self.x = 0.
        self.y = 0.
        self.color = 0
        self.text_size = 0
        self.visibility = False
        self.show_name_value = 0
        self.angle = 0
        self.alignment = 0
        self.text = ""
