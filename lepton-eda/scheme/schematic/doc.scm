;; Lepton EDA Schematic Capture
;; Scheme API
;; Copyright (C) 2011-2014 Peter Brett <peter@peter-b.co.uk>
;; Copyright (C) 2011-2015 gEDA Contributors
;; Copyright (C) 2017-2023 Lepton EDA Contributors
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
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
;;

(define-module (schematic doc)
  #:use-module (ice-9 optargs)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)

  #:use-module (lepton attrib)
  #:use-module (lepton m4)
  #:use-module (lepton object)
  #:use-module (lepton os)

  #:use-module (schematic gettext)
  #:use-module (schematic dialog)
  #:use-module (schematic util)

  #:export (sys-doc-dir)
  #:export (user-doc-dir)
  #:export (show-wiki)
  #:export (show-manual)
  #:export (show-component-documentation))


( define ( doc-show-uri url )
  "Call (schematic util)::show-uri( URL ), catching exceptions.
Display a message box on error."
  ( catch
    #t
    ( lambda()
      ( show-uri url )
    )
    ( lambda ( key subr msg args . rest )
      ( let*
      (
      ( str   (G_ "Could not show documentation:") )
      ( exmsg ( apply format #f msg args ) )
      ( ermsg ( format #f "~a~%'~a in ~a():~%~%~a" str key subr exmsg ) )
      )
        (schematic-message-dialog ermsg)
      )
    )
  )
) ; doc-show-uri()


( define ( doc-show-file fpath )
  ( doc-show-uri (format #f "file://~a" fpath) )
)


(define (sys-doc-dir)
  "Get the directory where documentation is stored."
  ; return:
  %lepton-docdir
)


(define (user-doc-dir)
  "Get the directory where per-user documentation is stored."
  (string-join (list (user-data-dir)
                     "doc" "lepton-eda")
               file-name-separator-string))


;; Munge a wiki page name so that it can be used in a filename
(define (wiki-munge name)
  (string-map
   (lambda (c)
     (case c
       ((#\? #\\ #\! #\*) #\_)
       ((#\:) #\-)
       (else c)))
   name))


( define* ( show-wiki #:optional (page "geda:documentation") )
  "Launch a browser to display a page from the offline version of the
wiki.  The specified PAGE should be a string containing the page
name as used on the live version of the wiki."

  ( define fnames (list (sys-doc-dir) "wiki") )
  ( define path   (string-join fnames file-name-separator-string 'suffix) )
  ( define fpath  (string-append path (wiki-munge page) ".html") )

  ( if ( file-exists? fpath )
    ( doc-show-uri (string-append "file://" fpath) )
    ( schematic-message-dialog (format #f (G_ "File does not exist:~%~a") fpath) )
  )
) ; show-wiki()


( define ( show-manual )
  "Launch a browser to display a main page of the offline version
of the Lepton EDA Reference manual."

  ( define fnames (list (sys-doc-dir) "lepton-manual.html" "index.html") )
  ( define fpath  (string-join fnames file-name-separator-string) )

  ( if ( file-exists? fpath )
    ( doc-show-uri (string-append "file://" fpath) )
    ( schematic-message-dialog (format #f (G_ "File does not exist:~%~a") fpath) )
  )
) ; show-manual()


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
               (if (member entry '(".." "."))
                 #f
                 (let ((result (proc entry)))
                   (if result (return result) #f)))))
           (lambda () (closedir dir))))
     #f)))


;; Searches for and displays documentation in DIRNAME.  A
;; documentation file is expected to start with BASENAME and
;; (optionally) end with EXT.  Comparisons are carried out
;; case-insensitively.  If no file was found, returns #f.
(define* (directory-doc-search dirname basename #:optional (ext ""))
  (define (test-dir-entry entry)
    (let ((filename (string-append dirname file-name-separator-string entry)))
      (and (string-prefix-ci? basename entry)
           (string-suffix-ci? ext entry)
           (file-exists? filename)
           filename)))
  (let ((filename (false-if-exception (directory-any dirname test-dir-entry))))
    (and filename (begin (doc-show-file filename) #t))))


;; Searches for documentation STRING on the Internet, using PDF
;; search template.
;;
;; FIXME string should be URL-encoded.
(define (internet-doc-search string)
  (doc-show-uri
   (format #f
           ;; PDF search template.
           "http://www.google.com/search?q=~S%20filetype:pdf"
           string))
  #t)


;; Munges a component basename to look more like a device name
(define (munge-basename basename)
  (let* ((rx (make-regexp "(-[0-9]+)?.sym$" regexp/icase))
         (match (regexp-exec rx basename)))
    (if match
        (regexp-substitute #f match 'pre)
        basename)))


(define (show-component-documentation obj)
  "show-component-documentation COMPONENT

Find the documentation for COMPONENT, by inspecting (in order) its
\"documentation\", \"device\" and \"value\" attributes, and its
component basename, and searching in the current directory, the user
and system documentation directories, and Google.  In most cases,
results are restricted to \".pdf\" files.  The documentation is
displayed in the system associated viewer application."


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
       ;; b) What about in the documentation directories?
       (directory-doc-search (user-doc-dir) documentation)
       (directory-doc-search (sys-doc-dir) documentation)
       ;; c) Does the documentation attribute look like a URL?
       (and (any (lambda (prefix) (string-prefix? prefix documentation))
                 '("http://" "ftp://" "file://"))
            (doc-show-uri documentation))

       ;; d) If a documentation attribute was specified at all, search
       ;;    for it with Google.
       (internet-doc-search documentation)))

     ;; 2) Checks based on "device=" and "value=" attributes
     (and
      device value
      (let ((device-value (string-append device "-" value)))
        (or
         ;; a) Look for a DEVICE-VALUE*.PDF file in the current directory
         (directory-doc-search (getcwd)  ".pdf")
         ;; b) Look for a DEVICE-VALUE*.PDF file in the documentation directories
         (directory-doc-search (user-doc-dir) device-value ".pdf")
         (directory-doc-search (sys-doc-dir) device-value ".pdf"))))
     (and
      device
      (or
       ;; c) Look for a DEVICE*.PDF file in the current directory
       (directory-doc-search (getcwd) device ".pdf")
       ;; d) Look for a DEVICE*.PDF file in the documentation directories
       (directory-doc-search (user-doc-dir) device ".pdf")
       (directory-doc-search (sys-doc-dir) device ".pdf")
       ;; d) If there's a device attribute, search for it with Google.
       (internet-doc-search device)))
     (and
      value
      ;; e) If there's a value attribute, search for it with Google.
      (internet-doc-search value))

     ;; 3) Checks based on component basename
     (and
      basename (string-length basename) ; Check basename is non-empty

      ;; Munge basename to look more like a device name
      (let ((name (munge-basename basename)))
        (or
         ;; a) Look for BASENAME*.PDF file in current directory
         (directory-doc-search (getcwd) name ".pdf")
         ;; b) Look for BASENAME*.PDF file in documentation directories
         (directory-doc-search (user-doc-dir) name ".pdf")
         (directory-doc-search (sys-doc-dir) name ".pdf"))))

     ;; 4) Fail miserably
     (schematic-message-dialog (G_ "No documentation found")))))
