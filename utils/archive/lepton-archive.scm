#!/usr/bin/env sh
exec @GUILE@ -s "$0" "$@"
!#
;;; Copyright (C) 2019-2022 Lepton EDA Contributors
;;;
;;; Based on Python script by Stuart Brorson:
;;; Copyright (C) 2003 Stuart Brorson <sdb@cloud9.net>
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA


;;; This program is used to create a Lepton EDA design archive.
;;; It operates in two modes: archive mode and extract mode.  In
;;; archive mode it creates a project archive from a bunch of
;;; project files, and in extract mode it extracts the files from
;;; the archive and places them in the local dir.

;;; Detailed description:

;;; Use -h option to get information about program invocation.

;;; Archive mode algorithm:
;;; - In the project directory (current directory), create archive
;;;   file list containing 'gafrc', files listed on command line,
;;;   and contents of the file specified with the --files-from (-f)
;;;   option.
;;; - Make temporary directory with the name of the project.
;;; - Make cache directory in it.
;;; - Create schematic file list from the archive file list.
;;; - Copy all schematics and files from the archive file list
;;;   into the temporary directory.
;;; - Copy symbols from all the schematics into the cache
;;;   directory.
;;; - Copy all SPICE files found in schematics into the cache
;;;   directory.
;;; - Fix gafrc in the temporary directory so lepton-schematic
;;;   could find all symbols and SPICE files in the archive later.
;;; - Create tar.gz archive from all the above files and move it
;;;   to the project directory.
;;; - Remove the temporary directory.

;;; Extract mode algorithm:
;;; - Copy archive file into temp directory.
;;; - Create a list of archive's contents using "tar -t -f
;;;   archive-name"
;;; - Extract files into the temp directory
;;; - Move all extracted files recursively into user's directory.
;;;   Before each move, make sure that no overwrite of existing
;;;   files will occur.

(eval-when (expand load eval)
  (unless (getenv "LIBLEPTON")
    (add-to-load-path "@LEPTON_SCHEME_MODULE_DIRECTORY@")
    (set! %load-compiled-path (cons "@ccachedir@" %load-compiled-path))))

(use-modules (ice-9 ftw)
             (ice-9 getopt-long)
             (ice-9 match)
             (ice-9 popen)
             (ice-9 rdelim)
             (ice-9 streams)
             (ice-9 rdelim)
             (srfi srfi-1)
             (srfi srfi-26)
             (lepton ffi)
             (lepton file-system)
             (lepton library component)
             (lepton library)
             (lepton log)
             (lepton object)
             (lepton os)
             (lepton page)
             (lepton rc)
             (lepton toplevel)
             (lepton version)
             (netlist schematic)
             (netlist schematic-component))

;;; Initialize liblepton library.
(liblepton_init)
(unless (getenv "LEPTON_INHIBIT_RC_FILES")
  (register-data-dirs))
(edascm_init)


;;; Helper functions and data structures.

;;; Default archive name. Use lowercase letters to avoid issues
;;; with case sensitive filesystems.
(define default-archive-name "project-archive")
(define gafrc "gafrc")
;;; Directory holding files to archive. It is the current
;;; directory where the program was invoked.
(define project-directory (getcwd))
;;; Directory for caching schematic files.
(define cache-directory "cache")


;;; Help string.
(define usage-info
  (format #f
          "Usage: ~A [OPTION...] FILES...

Used to create as well as extract Lepton EDA designs from an
archive.  The two modes of operation are \"archive mode\"
(archive creation) and \"extract mode\".  Archive mode is the default.

Command line switches:
  -h,--help             -- Print usage information.
  -V,--version          -- Print version information.
  -v,--verbose          -- Verbose mode.
  -f,--files-from=FILE  -- Additionally read filenames to archive
                           from FILE.
  -e,--extract          -- Extract mode. Mandatory if you want to
                           extract from archive.
  -a,--archive          -- Archive mode. It is the default mode.
  -o,--output=FILE      -- Specify the name of the output archive file
                           in archive mode.   If FILE has no \".tar.gz\"
                           suffix it will be automatically appended.
                           Default file name is \"~A.tar.gz\".

Example usage:
   Create an archive named \"MyArchive.tar.gz\",
   files to store are listed in \"archive-list\":

   lepton-archive -f archive-list -o MyArchive.tar.gz

   The same by using just file basename:

   lepton-archive -f archive-list -o MyArchive

   Verbosely create an archive from files listed on command line:

   lepton-archive -v README sch1.sch sch2.sch sch3.sch

   Extract an archive:
   lepton-archive -e ~A.tar.gz
"
          (basename (car (program-arguments)))
          default-archive-name
          default-archive-name))

(define (print-version)
  (display-lepton-version #:print-name #t #:copyright #t))

(define %option-spec
  '((help (single-char #\h) (value #f))
    (files-from (single-char #\f) (value #t))
    (version (single-char #\V) (value #f))
    (verbose (single-char #\v) (value #f))
    (extract (single-char #\e) (value #f))
    (archive (single-char #\a) (value #f))
    (output (single-char #\o) (value #t))))


(define (format-error s . args)
  (apply format (current-error-port) s args)
  (primitive-exit 1))


(define (wrong-option-handler)
  "Primitive wrong option handler for getopt-long."
  (format-error "Use \"~A -h\" for help\n" (car (program-arguments))))


;;; Options.
(define %options
  (catch #t
    (lambda ()
      (getopt-long (program-arguments) %option-spec))
    (lambda (key . args)
      (wrong-option-handler))))


(define archive-mode? (option-ref %options 'archive #f))
(define extract-mode? (option-ref %options 'extract #f))
(define output-archive-name (option-ref %options 'output #f))
(define archiverc-name (option-ref %options 'files-from #f))
(define help-mode? (option-ref %options 'help #f))
(define version-mode? (option-ref %options 'version #f))
(define verbose-mode? (option-ref %options 'verbose #f))
(define command-line-file-list (option-ref %options '() '()))


(define (debug-format s . args)
  "Formats and outputs S using ARGS when the verbose mode is set."
  (when verbose-mode?
    (display "---- ")
    (apply format (current-error-port) s args)
    (newline)))


(define (prompt-confirmed? prompt default-char)
  (display prompt)
  (let ((input-string (read-line (current-input-port))))
    (display #\newline)
    (and (not (string-ci=? input-string "n"))
         (or (string-ci=? input-string "y")
             (and (not (string-ci=? default-char "n"))
                  (string-ci=? default-char "y"))))))


(define (make-absolute-filename dir-name file-name)
  "Transforms FILE-NAME into an absolute filename using DIR-NAME
as a prefix if it is a relative filename, otherwise leaves it as
is.  Thus, filenames containing special paths like \".\" or \"..\"
or no directory path can be converted into absolute."
  (if (absolute-file-name? file-name)
      file-name
      (string-append dir-name
                     file-name-separator-string
                     file-name)))


(define (check-archive-filename filename)
  "Checks if FILENAME is a valid archive filename."
  (string-suffix-ci? ".tar.gz" filename))


;;; Make up output archive name.
(define (form-output-file-name)
  (let* ((name (or output-archive-name
                   default-archive-name))
         (suffix ".tar.gz")
         (suffix-length (string-length ".tar.gz")))
    (if (string-suffix-ci? suffix name)
        (string-drop-right name suffix-length)
        name)))


(define (report-preserve-existing-file file)
  (format #t "Preserving existing ~S in local directory.\n\n" file)
  #f)


(define* (delete-file* filename #:optional quiet)
  (catch #t
    (lambda () (delete-file filename))
    (lambda (key . args)
      (unless quiet
        (format #t "Cannot delete file ~S.\n~S: ~S\n" filename key args))
      #f)))

(define* (copy-file* old-filename new-filename #:optional quiet)
  (catch #t
    (lambda () (copy-file old-filename new-filename))
    (lambda (key . args)
      (unless quiet
        (format #t "Cannot copy file ~S to ~S.\n~S: ~S\n"
                old-filename
                new-filename
                key
                args))
      #f)))

(define (copy-file-to-dir filename directory-name)
  (copy-file* filename
              (make-absolute-filename directory-name
                                      (basename filename))))

(define (rename-file* old-filename new-filename)
  (catch #t
    (lambda () (rename-file old-filename new-filename))
    (lambda (key . args)
      (or (and (copy-file* old-filename new-filename 'quiet)
               (delete-file* old-filename 'quiet))
          (begin
            (format #t "Cannot rename file ~S to ~S.\n~S: ~S\n"
                    old-filename
                    new-filename
                    key
                    args)
            #f)))))

(define (mkdir* path . args)
  (if (file-exists? path)
      ;; If file exists, and it is directory, do nothing.
      ;; Otherwise, ask the user, if she wants to overwrite it.
      (unless (directory? path)
        (if (prompt-confirmed?
             (format #f "~S already exists and is not directory. Overwrite? [y/N] "
                     path)
             ;; default
             "n")
            (begin
              (delete-file* path)
              (apply mkdir path args))
            (report-preserve-existing-file path)))
      ;; No file exists, just create the directory.
      (apply mkdir path args)))


(define (rmdir* filename)
  (catch #t
    (lambda () (rmdir filename))
    (lambda (key . args)
      (format #t "Cannot remove directory ~S.\n~S: ~S\n"
              filename
              key
              args))))


(define (ls->path ls prefix)
  (define (add-file-name-separator path)
    (if (file-name-separator?
         (string-ref path (1- (string-length path))))
        path
        (string-append path file-name-separator-string)))

  (string-append (add-file-name-separator prefix)
                 (string-join (reverse ls)
                              file-name-separator-string)))


(define (move-files-recursively from-path to-path)
  (define (enter? name stat result)
    (let* ((ls (cons (basename name) (or result '())))
           (directory-name (ls->path ls to-path)))
      (debug-format "Check directory: ~S" directory-name)
      ;; If file exists, and it is directory, do nothing.
      ;; Otherwise, ask the user, if she wants to overwrite it.
      (if (and (file-exists? directory-name)
               (not (directory? directory-name)))
          (if (prompt-confirmed?
               (format #f "~S already exists and is not directory. Overwrite? [y/N] "
                       directory-name)
               ;; default
               "n")
              (delete-file* directory-name)
              (report-preserve-existing-file directory-name))
          ;; No file exists, just create the directory.
          #t)))

  (define (leaf name stat result)
    ;; Increment the number of files processed.
    (let ((new-filename (ls->path (cons (basename name) result)
                                  to-path)))
      (debug-format "Move file ~S to ~S" name new-filename)
      (if (and (file-exists? new-filename)
               (not (prompt-confirmed?
                     (format #f "~S already exists.  Overwrite? [y/N] "
                             new-filename)
                     ;; default
                     "n")))
          (begin
            (report-preserve-existing-file new-filename)
            ;; Remove the file anyway. The user can restore it
            ;; from the same archive later, if needed.
            (delete-file* name))
          (rename-file* name new-filename))
      result))

  ;; Descend to a directory, and make its sibling in another tree.
  (define (down name stat result)
    (if result
        (let* ((ls (cons (basename name) result))
               (directory-name (ls->path ls to-path)))
          (debug-format "Make directory: ~S" directory-name)
          (mkdir* directory-name)
          ls)
        '()))

  ;; Ascend from a directory and remove it.
  (define (up name stat result)
    (let ((directory-name (if (null? result)
                              from-path
                              (ls->path result from-path))))
      (debug-format "Remove empty directory ~S" directory-name)
      (rmdir* directory-name)
      ;; Return value: it is #t when there is nothing to traverse
      ;; more, or the list reflecting the upper directory.
      (or (null? result)
          (cdr result))))

  ;; Likewise for skipped directories.
  (define (skip name stat result)
    (debug-format "Directory skipped.\n")
    result)

  ;; Ignore unreadable files/directories but warn the user.
  (define (error name stat errno result)
    (format (current-error-port) "warning: ~a: ~a~%"
            name (strerror errno))
    result)

  (file-system-fold enter? leaf down up skip error
                    #f
                    from-path))


(define (copy-files-recursively from-path to-path)
  (define (enter? name stat result)
    (let* ((ls (cons (basename name) (or result '())))
           (directory-name (ls->path ls to-path)))
      ;; If file exists, and it is directory, do nothing.
      ;; Otherwise, ask the user, if she wants to overwrite it.
      (if (and (file-exists? directory-name)
               (not (directory? directory-name)))
          (if (prompt-confirmed?
               (format #f "~S already exists and is not directory. Overwrite? [y/N] "
                       directory-name)
               ;; default
               "n")
              (delete-file* directory-name)
              (report-preserve-existing-file directory-name))
          ;; No file exists, just create the directory.
          #t)))

  (define (leaf name stat result)
    ;; Increment the number of files processed.
    (let ((new-filename (ls->path (cons (basename name) result)
                                  to-path)))
      (debug-format "Copy file ~S to ~S" name new-filename)
      (if (and (file-exists? new-filename)
               (not (prompt-confirmed?
                     (format #f "~S already exists.  Overwrite? [y/N] "
                             new-filename)
                     ;; default
                     "n")))
          (report-preserve-existing-file new-filename)
          (copy-file* name new-filename))
      result))

  ;; Descend to a directory, and make its sibling in another tree.
  (define (down name stat result)
    (let* ((ls (cons (basename name) (or result '())))
           (directory-name (ls->path ls to-path)))
      (debug-format "Make directory: ~S" directory-name)
      (mkdir* directory-name)
      ls))

  ;; Ascend from a directory and remove it.
  (define (up name stat result)
    (or (null? result) (cdr result)))

  ;; Likewise for skipped directories.
  (define (skip name stat result)
    (debug-format "Directory skipped.\n")
    result)

  ;; Ignore unreadable files/directories but warn the user.
  (define (error name stat errno result)
    (format (current-error-port) "warning: ~a: ~a~%"
            name (strerror errno))
    result)

  (debug-format "Copy recursively ~S => ~S" from-path to-path)
  (file-system-fold enter? leaf down up skip error
                    #f
                    from-path))


(define (copy-to-dir filename directory-name)
  (debug-format "Copy ~S => ~S" filename directory-name)
  (let ((destination (make-absolute-filename directory-name
                                             (basename filename))))
    (if (directory? filename)
        (copy-files-recursively filename directory-name)
        (copy-file* filename destination))))


(define (remove-temp-dir dir)
  ;; Get rid of temporary directory.
  (debug-format "Remove temporary directory ~S" dir)
  ;; Well, I could make a procedure for recursive removing of
  ;; the directory contents, but it is simpler just to use 'rm'
  ;; here.
  (system* "rm" "-r" "-f" dir))


(define (create-archive-file-list)
  "Create the list of files to archive, including \"gafrc\",
files listed in the command line, and in the file specified by
option \"--files-from\" (\"-f\")."
  (define archiverc
    (let ((files-from archiverc-name))
      (and (string? files-from)
           files-from
           ;; The file must exist.
           (if (file-exists? files-from)
               files-from
               (format-error "Resource file ~S doesn't exist.  Exiting.\n"
                             files-from)))))

  (define not-string-null? (negate string-null?))

  (define (archiverc-filenames)
    (if archiverc
        (with-input-from-file archiverc
          (lambda ()
            (let loop ((line (read-line))
                       (result '()))
              (if (eof-object? line)
                  (filter not-string-null? result)
                  (loop (read-line)
                        (cons (string-trim-both line)
                              result))))))
        ;; The file has not been specified, return empty list.
        '()))

  (define (make-empty-file filename)
    (format #t "Creating ~S in archive." filename)
    (with-output-to-file filename (lambda () (display "")))
    filename)

  (define (add-file filename source)
    (let ((filename (make-absolute-filename project-directory
                                            (expand-env-variables filename))))
      (debug-format "Check ~S" filename)
      (if (file-exists? filename)
          filename
          (match source
            ('command-line
             (format #t "Ignore non-existent file listed in command line: ~S\n"
                     filename)
             #f)
            ('archiverc
             (format #t "Ignore non-existent file listed in ~S: ~S\n"
                     archiverc
                     filename)
             #f)
            ('rc
             (if (prompt-confirmed?
                  (format #f "Non-existent file ~S.\nCreate empty version in local dir? [Y/n] "
                          filename)
                  ;; default
                  "y")
                 (make-empty-file filename)
                 (format-error "You need ~S to create archive. Aborting.\n" filename)))))))

  (define (add-rc-file filename)
    (add-file filename 'rc))

  (define (add-command-line-file filename)
    (add-file filename 'command-line))

  (define (add-archiverc-file filename)
    (add-file filename 'archiverc))

  (delete-duplicates
   (append
    ;; Add RC files.
    (list (add-rc-file gafrc))
    ;; Get names of all schematics and other files to archive.
    ;; First get file names from command line.
    (filter-map add-command-line-file command-line-file-list)
    ;; Next get file names from file, if specified.
    (filter-map add-archiverc-file (archiverc-filenames)))))


(define (create-schematic-file-list filename-list)
  "Creates the list of schematic files to search.  Right now
I just run through FILE-LIST and pull out all files ending in .sch.
Files are saved in list with basename (no path)."
  (define (readable-schematic? filename)
    (and (string-suffix? ".sch" filename)
         (or (file-readable? filename)
             (format-error "Can't access ~S for reading. Exiting." filename))))

  ;; We need to make sure that this file is not already in the list.
  (delete-duplicates (filter readable-schematic? filename-list)))


(define (create-symbol-file-list schematic-file-list)
  "Opens each schematic file in SCHEMATIC-FILE-LIST and collects
component names used."
    (define (schematic-symbols filename)
      (map component-basename
           (filter component?
                   (page-contents (file->page filename)))))

    (delete-duplicates (append-map schematic-symbols schematic-file-list)))


(define (save-symbols symbol-file-list archive-directory)
  "Copies symbols in SYMBOL-FILE-LIST to ARCHIVE-DIRECTORY."
  (for-each
   (lambda (symbol-filename)
     (debug-format "Save symbol ~S" symbol-filename)
     (let ((source (absolute-component-name symbol-filename)))
       (if source
           (copy-file-to-dir source archive-directory)
           (format #t "Symbol ~S not found.\n" symbol-filename))))
   symbol-file-list))


(define (update-rc dir)
  "Updates gafrc by resetting component libraries and adding a new
symbol cache directory."
  (let ((port (open (make-absolute-filename dir (basename gafrc))
                    (logior O_WRONLY O_APPEND))))
    (format port
            "
(reset-component-library)
(component-library ~S)
(reset-source-library)
(source-library ~S)
"
            cache-directory
            cache-directory)
    (close port)))

(define (flatten-tree ls)
  (let loop ((result '())
             (ls ls))
    (if (null? ls)
        result
        (let ((x (car ls))
              (rest (cdr ls)))
          (loop
           (if (list? x)
               (append (flatten-tree x) result)
               (cons x result))
           (cdr ls))))))


(define (source-library-filename filename)
  (define (get-source-or-absolute-filename)
    (or (get-source-library-file filename)
        ;; If we cannot find the filename in the source library,
        ;; maybe it was specified prefixed with a directory name.
        (and (not (string= filename (basename filename)))
             (format #t "File ~S will be copied to the ~S subdirectory of your archive.
This can lead to wrong processing of your schematics by other tools.
Please use file basenames in your schematics to fix this.\n\n"
                     filename
                     cache-directory)
             (make-absolute-filename project-directory filename))))

  (let ((source-filename (get-source-or-absolute-filename)))
    (if (and source-filename
             (file-exists? source-filename))
        source-filename
        (begin
          (debug-format "File ~S does not exist. Check your source-library."
                        filename)
          #f))))


(define (subschematic-page-filenames schematic toplevel-schematics)
  (define (non-toplevel? name)
    (not (member (make-absolute-filename project-directory
                                         name)
                 toplevel-schematics)))

  (let ((files (delete-duplicates
                (flatten-tree (schematic-name-tree schematic)))))
    (filter-map source-library-filename
                (filter non-toplevel? files))))


(define (save-schematics-and-sources toplevel-schematics temp-dir cache-dir)
  (let* ((schematic (file-name-list->schematic toplevel-schematics))
         (subschematic-filenames (subschematic-page-filenames schematic
                                                              toplevel-schematics))
         (schematic-sources (filter-map (lambda (c) (schematic-component-attribute c 'file))
                                        (schematic-components* schematic)))
         (source-filenames (filter-map source-library-filename schematic-sources))
         (symbol-filenames (create-symbol-file-list
                            (append toplevel-schematics subschematic-filenames))))

    (for-each (lambda (sch) (copy-to-dir sch temp-dir)) toplevel-schematics)
    (for-each (lambda (sch) (copy-to-dir sch cache-dir)) subschematic-filenames)
    (for-each (lambda (cir) (copy-to-dir cir cache-dir)) source-filenames)
    (save-symbols symbol-filenames cache-dir)))


;;; Archiver.
(define (archive)
  "Main archiver."

  (define (make-tmp-dir name)
    (mkdir* name #o700)
    name)

  (let* (
         ;; Make a temporary directory.
         (temp/ (make-tmp-dir (tmpnam)))
         ;; Temporary project directory to put files and create
         ;; archive in.
         (temp/project
          (make-tmp-dir
           (make-absolute-filename temp/
                                   (basename project-directory))))
         ;; Add cache directory for symbols.
         (temp/project/cache
          (make-tmp-dir (make-absolute-filename temp/project
                                                cache-directory)))

         (output (form-output-file-name))
         (output.tar (string-append output ".tar"))
         (output.tar.gz (string-append output.tar ".gz"))
         (temp/output.tar
          (make-absolute-filename temp/ (basename output.tar)))
         (temp/output.tar.gz
          (make-absolute-filename temp/ (basename output.tar.gz)))
         (project/output.tar.gz
          (make-absolute-filename project-directory output.tar.gz))
         ;; Create list of files (and directories) to stick into
         ;; archive.  Returned paths point to the absolute paths of
         ;; the files.
         (archive-file-list (create-archive-file-list))
         ;; Create list of schematic files to open and search.
         ;; Returned paths give only the base name (i.e. no path)
         (schematic-file-list (create-schematic-file-list archive-file-list)))

    (debug-format "Temp directory: ~S" temp/)
    (debug-format "Temp project directory: ~S" temp/project)
    (debug-format "Temp cache directory: ~S" temp/project/cache)
    (debug-format "Temp archive name: ~S" temp/output.tar.gz)
    (debug-format "Destination archive name: ~S" project/output.tar.gz)

    ;; Copy all files over to temporary directory.
    (for-each (cut copy-to-dir <> temp/project) archive-file-list)

    ;; Stick symbol & SPICE files into the cache directory.
    (save-schematics-and-sources schematic-file-list
                                 temp/project
                                 temp/project/cache)

    ;; Now create tar file.  We copy remaining files over to /tmp,
    ;; and then tar them all up using a local, relative file
    ;; prefix.

    ;; Update copy of gafrc.
    (update-rc temp/project)

    ;;  Now use this in tar command.
    (system* "tar"
             "-C"
             temp/
             "-c"
             "-f"
             temp/output.tar
             (basename temp/project))
    (system* "gzip" temp/output.tar)
    ;; Now we should have temp/output.tar.gz prepared.

    ;; Now try to move completed archive back to user directory.
    (debug-format "Rename archive to ~S" project/output.tar.gz)
    (if (or (not (file-exists? project/output.tar.gz))
            (prompt-confirmed?
             (format #f "~S already exists. Overwrite? [y/N] " project/output.tar.gz)
             ;; default
             "n"))
        ;; Force renaming.
        (begin
          (when (file-exists? project/output.tar.gz)
            ;; Remove old archive if it exists.
            (delete-file* project/output.tar.gz))
          (rename-file* temp/output.tar.gz project/output.tar.gz)
          (format #t "Project archive ~S created successfully!\n"
                  project/output.tar.gz)
          (remove-temp-dir temp/))
        ;; Otherwise, report the place of the new archive.
        (format #t "Preserving existing archive in local directory.
Your new archive lives in ~S\n" temp/output.tar.gz))))


;;; Extracter.
(define (extract)
  "Extracts the archive into the current directory."

  ;; Temporary directory to extract archive in.
  (define tmpdir
    (let ((name (tmpnam)))
      (mkdir* name #o700)
      name))

  (define string-not-null? (negate string-null?))

  (define (report-non-existing-file filename)
    (format #t "File ~S doesn't exist. Ignoring.\n" filename))

  (define (get-command-output command)
    (apply string
           (stream->list (port->stream (open-input-pipe command)
                                       read-char))))

  (define (extract-file archive-filename filename)
    (debug-format "Extracting ~S" filename)
    (system* "tar" "-f" archive-filename "-x" filename))

  (define (is-simple-file? file)
    (string=? (basename file) file))

  (define (extract-from-archive filename.tar.gz)
    (debug-format "Trying to extract archive ~S." filename.tar.gz)

    (unless (copy-file-to-dir filename.tar.gz tmpdir)
      (format-error "Can't work in the ~S directory.
Check that you have write permission there.\n"
                    tmpdir))

    (chdir tmpdir)

    ;; Change name of file so it can be gunziped.
    (unless (check-archive-filename filename.tar.gz)
      (format-error  "
Error -- the file suffix is not \".tar.gz\"; lepton-archive can't do extraction.
If this archive was created using lepton-archive, you can rename it using
.tar.gz as suffix and try again.  Otherwise, just gunzip and tar -xvf
the file manually.\n"))

    ;; Gunzip the file.
    (system* "gunzip" "-f" filename.tar.gz)
    (let* (
           ;; Get rid of the ".gz" suffix.
           (filename.tar (string-drop-right filename.tar.gz 3))
           ;; Get list of files in the archive.
           (return-string (get-command-output (string-append "tar -t -f " filename.tar)))
           (filename-list (filter string-not-null? (string-split return-string #\newline))))

      ;; Extract all files first.
      (for-each (cut extract-file filename.tar <>) filename-list)

      ;; Clean up /tmp directory.
      (delete-file* filename.tar)
      ;; Don't report errors on deleting tar.gz, since gunzip
      ;; might delete it before.
      (delete-file* filename.tar.gz 'quiet)

      ;; Move files to the project directory.
      (move-files-recursively tmpdir project-directory)

      (chdir project-directory)))

  (define (process-file filename)
    (if (file-exists? filename)
        (extract-from-archive filename)
        (report-non-existing-file filename)))

  (when (null? command-line-file-list)
    (format-error "Must specify a filename for extraction.\n"))

  (for-each process-file command-line-file-list))


;;; Main program.
(define (main)
  (when help-mode?
    (display usage-info)
    (primitive-exit 0))

  (when version-mode?
    (print-version)
    (primitive-exit 0))

  (if (and extract-mode? archive-mode?)
      (format-error "Mutually exclusive option --archive and --extract.\n")

      ;; Archive mode is default. Don't check any options, just check
      ;; extract mode is set or not.
      (if extract-mode?
          (if output-archive-name
              ;; Check for options invalid in extract mode.
              (format-error "Incompatible command line arguments --extract and --output.\n")
              (extract))
          (archive))))


(%with-toplevel (%make-toplevel)
  (lambda ()
    ;; Process all gafrc files.
    (parse-rc "lepton-archive" "gafrc" )
    ;; Init log domain and create log file right away even if
    ;; logging is enabled.
    (init-log "archive")
    (main)))
