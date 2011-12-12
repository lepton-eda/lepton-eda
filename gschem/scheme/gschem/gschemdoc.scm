;; gEDA - GPL Electronic Design Automation
;; libgeda - gEDA's library - Scheme API
;; Copyright (C) 2011 Peter Brett <peter@peter-b.co.uk>
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111 USA
;;

(define-module (gschem gschemdoc)
  #:use-module (gschem core gettext)
  #:use-module (geda os)
  #:use-module (geda object)
  #:use-module (geda attrib)
  #:use-module (gschem util)
  #:use-module (ice-9 optargs)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1))

;; FIXME this should probably be set via configure
(define %configured-sys-doc-dir #f)

(define-public (sys-doc-dir)
  "sys-doc-dir

Get the directory where gEDA documentation is stored."

  ;; Given a gEDA data directory path, guess the installation prefix.
  (define (guess-prefix dir)
    (let ((ofs (string-contains
                dir (string-append separator "share"))))
      (and ofs (substring dir 0 ofs))))

  ;; Guess the gEDA documentation directory, using the gEDA data
  ;; search path.  Guaranteed to only return paths that correspond to
  ;; actual directories.
  (define (guess-docdir)
    (any

     (lambda (dir)
       (let ((docdir
              (string-join (list (guess-prefix dir)
                                 "share" "doc" "geda-gaf")
                           separator)))
         (and (false-if-exception
               (eq? 'directory (stat:type (stat docdir))))
              docdir)))

     (sys-data-dirs)))

  (or (guess-docdir)
      (if (platform? 'win32-native)
          #f
          %configured-sys-doc-dir)))

;; Munge a wiki page name so that it can be used in a filename
(define (wiki-munge name)
  (string-map
   (lambda (c)
     (case c
       ((#\? #\\ #\! #\*) #\_)
       ((#\:) #\-)
       (else c)))
   name))

(define*-public (show-wiki #:optional (page "index"))
  "show-wiki PAGE

Launch a browser to display a page from the offline version of the
gEDA wiki shipped with gEDA/gaf.  The specified PAGE should be a
string containing the page name as used on the live version of the
wiki."

  (show-uri
   (string-append "file://"
                  (string-join (list (sys-doc-dir) "wiki")
                               separator 'suffix)
                  (wiki-munge page)
                  ".html")))

(define %pdf-search-template "http://www.google.com/search?q=~S%20filetype:pdf")

;; Get the value of a named attribute.  Attached attributes are
;; searched first, followed by inherited attributes.  The first
;; non-empty result found is returned.
(define (attribute-value-by-name obj name)
  (define (any-proc attrib)
    (and (equal? name (attrib-name attrib))
         (let ((v (attrib-value attrib)))
           (and (> (string-length v) 0) v))))
  (or (any any-proc (object-attribs obj))
      (any any-proc (inherited-attribs obj))))

;; For each entry in DIRNAME, PROC is called.  If PROC returns a
;; true value, the iteration stops and the result returned by PROC
;; is returned to the caller.
(define (directory-any dirname proc)
  (call/cc
   (lambda (return)
     (let ((dir #f))
       (dynamic-wind
           (lambda () (set! dir (opendir dirname)))
           (lambda ()
             (do ((entry (readdir dir) (readdir dir)))
                 ((eof-object? entry))
               (case entry
                 ((".." ".") #f)
                 (else (let ((result (proc entry)))
                         (if result (return result) #f))))))
           (lambda () (closedir dir))))
     #f)))

;; Searches for and displays documentation in DIRNAME.  A
;; documentation file is expected to start with BASENAME and
;; (optionally) end with EXT.  Comparisons are carried out
;; case-insensitively.  If no file was found, returns #f.
(define* (directory-doc-search dirname basename #:optional (ext ""))
  (define (test-dir-entry entry)
    (let ((filename (string-append dirname separator entry)))
      (and (string-prefix-ci? basename entry)
           (string-suffix-ci? ext entry)
           (file-exists? filename)
           filename)))
  (let ((filename (false-if-exception (directory-any dirname test-dir-entry))))
    (and filename (begin (show-file filename) #t))))

;; Searches for documentation STRING on the Internet, using
;; %pdf-search-template.
;;
;; FIXME string should be URL-encoded.
(define (internet-doc-search string)
  (show-uri (format #f %pdf-search-template string)) #t)

;; Munges a component basename to look more like a device name
(define (munge-basename basename)
  (let* ((rx (make-regexp "(-[0-9]+)?.sym$" regexp/icase))
         (match (regexp-exec rx basename)))
    (if match
        (regexp-substitute #f match 'pre)
        basename)))

(define-public (show-component-documentation obj)
  "show-component-documentation COMPONENT

Find the documentation for COMPONENT, by inspecting (in order) its
\"documentation\", \"device\" and \"value\" attributes, and its
component basename, and searching in the current directory, the system
gEDA documentation directory, and Google.  In most cases, results are
restricted to \".pdf\" files.  The documentation is displayed in the
system associated viewer application."


  (let ((documentation (attribute-value-by-name obj "documentation"))
        (device (attribute-value-by-name obj "device"))
        (value (attribute-value-by-name obj "value"))
        (basename (component-basename obj)))

    ;; Try several different ways of finding documentation
    (or

     ;; 1) Checks based on "documentation=" attribute
     (and
      documentation

      (or
       ;; a) First check the obvious -- does the documentation
       ;;    attribute match a file in the current directory?
       (directory-doc-search (getcwd) documentation)
       ;; b) What about in the documentation directory?
       (directory-doc-search (sys-doc-dir) documentation)
       ;; c) Does the documentation attribute look like a URL?
       (and (any (lambda (prefix) (string-prefix? prefix documentation))
                 '("http://" "ftp://" "file://"))
            (show-uri documentation))

       ;; d) If a documentation attribute was specified at all, search
       ;;    for it with Google.
       (internet-doc-search documentation)))

     ;; 2) Checks based on "device=" and "value=" attributes
     (and
      device value
      (or
       ;; a) Look for a DEVICE-VALUE*.PDF file in the current directory
       (directory-doc-search (getcwd) (string-append device "-" value) ".pdf")
       ;; b) Look for a DEVICE-VALUE*.PDF file in the documentation directory
       (directory-doc-search (sys-doc-dir) (string-append device "-" value) ".pdf")))
     (and
      device
      (or
       ;; c) Look for a DEVICE*.PDF file in the current directory
       (directory-doc-search (getcwd) device ".pdf")
       ;; d) Look for a DEVICE*.PDF file in the documentation directory
       (directory-doc-search (sys-doc-dir) device ".pdf")
       ;; d) If there's a device attribute, search for it with Google.
       (internet-doc-search device)))
     (and
      value
      ;; e) If there's a value attribute, search for it with Google.
      (internet-pdf-search value))

     ;; 3) Checks based on component basename
     (and
      basename (string-length basename) ; Check basename is non-empty

      ;; Munge basename to look more like a device name
      (let ((name (munge-basename basename)))
        (or
         ;; a) Look for BASENAME*.PDF file in current directory
         (directory-doc-search (getcwd) name ".pdf")
         ;; b) Look for BASENAME*.PDF file in documentation directory
         (directory-doc-search (sys-doc-dir) name ".pdf"))))

     ;; 4) Fail miserably
     (error (_ "No documentation found")))))
