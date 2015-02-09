# Copyright (C) 1998-2010 Ales Hvezda
# Copyright (C) 1998-2010 gEDA Contributors (see ChangeLog for details)
# Copyright (C) 2013-2015 Roland Lutz
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

## \namespace xorn.fileutils
## Writing files in a safe way.

import errno, os, stat, tempfile

## How many symlinks to follow before returning \c ELOOP.
#
# A more conventional value for this is \c 20 or even \c 8, but gEDA
# uses \c 256, so we're using that, too.

MAXSYMLINKS = 256

## Follow a series of symbolic links and return the destination path.
#
# This function calls \c readlink(2) recursively until some place that
# doesn't contain a symlink is found---either some other kind of file,
# or a non-existing file if the last symlink was broken.  There may
# still be symlinks in the directory components of the returned path.

def follow_symlinks(filename):
    orig_filename = filename
    link_count = 0

    while link_count < MAXSYMLINKS:
        try:
            linkname = os.readlink(filename)
        except OSError as e:
            if e.errno != errno.EINVAL and e.errno != errno.ENOENT:
                raise

            # Found something that isn't a symlink.
            return filename

        link_count += 1

        # If the linkname is not an absolute path name, append it to
        # the directory name of the followed filename.

        # os.path.join throws the first component away if the second
        # component is an absolute path.

        filename = os.path.join(os.path.dirname(filename), linkname)

        # Note that os.path.normpath and os.path.abspath are buggy in
        # the face of symlinks, for example they will happily collapse
        # /etc/foo/../bar into /etc/bar, even though /etc/foo might be
        # a link to /usr/lib/foo.
        #
        # The only safe way to collapse ".." elements is to resolve
        # symlinks (but we aren't going to do this here).

    raise OSError(errno.ELOOP, os.strerror(errno.ELOOP), orig_filename)

## Return the process's file mode creation mask.

def umask():
    # POSIX.1 requires that the umask is a process-wide attribute.
    # I don't see a way to read the umask without a potential race
    # condition.  To avoid at least the resulting security risk, I'll
    # set the umask temporarily to the most restrictive value.
    saved = os.umask(0777)
    os.umask(saved)
    return saved

## Write some data to a file in a reasonably safe way.
#
# Calls \a write_func to write some data to the file named \a
# filename.  The data is first written to a temporary file which is
# then renamed to the final name.  If \a write_func raises an
# exception, the temporary file is removed, and the original file is
# left untouched.
#
# If \a backup is \c True (the default), an existing regular file
# called \a filename will be backed up after the data has successfully
# been written, replacing an existing backup file.  Otherwise, it will
# be overwritten.  An exception to this is if the user doesn't have
# write permission to the existing file and \a overwrite is \c False.
# (This solves gEDA bug #698565.)  If \a filename exists but is not a
# regular file, an exception is raised.
#
# If \a filename is a symbolic link, the (ultimately) linked file is
# replaced, not the link.  The backup is created in the same directory
# as the linked file.
#
# Hard links to \a filename will break.  Also, since the file is
# recreated, existing access control lists, metadata etc. may be lost.
# The function tries to preserve owner, group and attributes but
# doen't treat failure to do so as an error.
#
# \param [in] filename   The name of a file to write to.
# \param [in] write_func A function that takes a file-like object as
#                        an argument and writes the data to it.
# \param [in] overwrite  Overwrite the file even if the user doen't
#                        have write permission.
# \param [in] backup     Back up the original file.  Set this to \c
#                        False if the file was already saved in this
#                        invocation of the program.
# \param [in] fsync      Sync the temporary file to ensure the data is
#                        on disk when it is renamed over the
#                        destination.  This may cause significant lag;
#                        but otherwise, if the system crashes, both
#                        the new and the old file may be lost on some
#                        filesystems (i.e. those that don't guarantee
#                        the data is written to the disk before the
#                        metadata).
#
# \return \c None.
#
# \throw IOError, OSError if a filesystem error occurred

def write(filename, write_func, overwrite = False,
                                backup = True,
                                fsync = True):
    # Get the real filename
    filename = follow_symlinks(filename)

    try:
        st = os.stat(filename)
    except OSError as e:
        if e.errno != errno.ENOENT:
            raise
        st = None
    else:
        # Don't overwrite a read-only destination file (gEDA bug #698565)
        if not overwrite and not os.access(filename, os.W_OK):
            raise IOError(errno.EACCES, os.strerror(errno.EACCES), filename)

        if not stat.S_ISREG(st.st_mode):
            raise Exception, \
                'Refusing to overwrite non-regular file: ' + filename

    # Get the directory in which the real filename lives
    dirname, basename = os.path.split(filename)
    if not dirname:
        dirname = '.'

    # If there is not an existing file with that name, compute the
    # permissions and uid/gid that we will use for the newly-created file.

    if st is None:
        # Use default permissions
        mode = 0666 & ~umask()
        uid = os.getuid()

        dir_st = os.stat(dirname)
        if dir_st.st_mode & stat.S_ISGID:
            gid = dir_st.st_gid
        else:
            gid = os.getgid()
    else:
        mode = st.st_mode
        uid = st.st_uid
        gid = st.st_gid

    f = tempfile.NamedTemporaryFile(
        dir = dirname, prefix = basename + '.', delete = False)

    try:
        try:
            write_func(f)

            # We want to sync the newly written file to ensure the data is
            # on disk when we rename over the destination.  Otherwise if
            # we get a system crash we can lose both the new and the old
            # file on some filesystems (i.e. those that don't guarantee
            # the data is written to the disk before the metadata).

            if fsync:
                os.fsync(f.fileno())
        finally:
            f.close()

        # Do a backup unless requested otherwise.
        if backup and st is not None:
            try:
                os.rename(filename, filename + '~')
            except OSError as e:
                sys.stderr.write(_("Failed to back up '%s': %s\n")
                                 % (filename, e.strerror))

        os.rename(f.name, filename)
    except:
        os.unlink(f.name)
        raise

    # Restore permissions/ownership.  We restore both on a best-effort
    # basis; rather than treating failure as an error, we just log a
    # warning.

    try:
        os.chmod(filename, mode)
    except OSError as e:
        sys.stderr.write(_("Failed to restore permissions on '%s': %s\n")
                         % (filename, e.strerror))
    try:
        os.chown(filename, uid, gid)
    except OSError as e:
        sys.stderr.write(_("Failed to restore ownership on '%s': %s\n")
                         % (filename, e.strerror))
