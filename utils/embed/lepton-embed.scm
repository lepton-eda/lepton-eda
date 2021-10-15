#!/usr/bin/env sh
export GUILE_LOAD_COMPILED_PATH="@ccachedir@:${GUILE_LOAD_COMPILED_PATH}"
exec @GUILE@ "$0" "$@"
!#

;;
;; Lepton EDA
;; lepton-embed - schematic components and pictures embedding utility
;; Copyright (C) 2019 dmn <graahnul.grom@gmail.com>
;; Copyright (C) 2019-2021 Lepton EDA Contributors
;; License: GPLv2+. See the COPYING file
;;

(eval-when (expand load eval)
  (unless (getenv "LIBLEPTON")
    (add-to-load-path "@LEPTON_SCHEME_MODULE_DIRECTORY@")))

(use-modules (ice-9 format)
             (ice-9 getopt-long)
             (lepton ffi))

;; Initialize liblepton library.
(liblepton_init)
(unless (getenv "LEPTON_INHIBIT_RC_FILES")
  (g_register_libgeda_dirs))
(edascm_init)

( primitive-eval '(use-modules (lepton core toplevel)) )
( primitive-eval '(use-modules (lepton object)) )
( primitive-eval '(use-modules (lepton page)) )
( primitive-eval '(use-modules (lepton rc)) )
( primitive-eval '(use-modules (lepton version)) )



; command line options:
;
( define cmd-line-args-spec
( list
  ( list ; --embed (-e)
    'embed
    ( list 'single-char #\e )
    ( list 'value        #f )
  )
  ( list ; --unembed (-u)
    'unembed
    ( list 'single-char #\u )
    ( list 'value        #f )
  )
  ( list ; --pictures (-p)
    'pictures
    ( list 'single-char #\p )
    ( list 'value        #f )
  )
  ( list ; --components (-c)
    'components
    ( list 'single-char #\c )
    ( list 'value        #f )
  )
  ( list ; --suffix (-x)
    'suffix
    ( list 'single-char #\x )
    ( list 'value        #t )
  )
  ( list ; --help (-h)
    'help
    ( list 'single-char #\h )
    ( list 'value        #f )
  )
  ( list ; --version (-V)
    'version
    ( list 'single-char #\V )
    ( list 'value        #f )
  )
)
) ; cmd-line-args-spec



( define ( usage exit-code )
  ( format #t "~
Usage: lepton-embed -e | -u [OPTIONS] FILE ...

Lepton EDA schematic components and pictures embedding/unembedding utility.

Options:
  -e, --embed          Embed: without -p or -c, all components and pictures
  -u, --unembed        Unembed: without -p or -c, all components and pictures
  -p, --pictures       Process pictures only
  -c, --components     Process components only
  -x, --suffix SUFFIX  Keep input files intact, save to FILE.SUFFIX
  -h, --help           Show usage information
  -V, --version        Show version information

Report bugs at <~a>
Lepton EDA homepage: <~a>
"
    ( lepton-version 'bugs )
    ( lepton-version 'url )
  )

  ( primitive-exit exit-code )
)



( define ( version )
  ( display-lepton-version #:print-name #t #:copyright #t )
  ( primitive-exit 0 )
)



( define ( page-open file )
  ( catch #t
  ( lambda()
    ; return:
    ( file->page file )
  )
  ( lambda( ex . args )
    ( format (current-error-port)
              "Cannot open file [~a]:~%  '~a: ~a~%"
              file ex args )
    ; return:
    #f
  )
  ) ; catch
) ; page-open()



( define ( page-save page suffix )

  ( define ( mk-out-file-name file )
    ; return:
    ( if ( string-null? suffix )
      file                         ; if
      ( format #f "~a~a~a.~a"      ; else
        ( dirname file )
        file-name-separator-string
        ( basename file )
        suffix
      )
    )
  ) ; mk-out-file-name()

( let*
  (
  ( file ( page-filename page ) )
  ( out  ( mk-out-file-name file ) )
  )

  ( catch #t
  ( lambda()
    ( with-output-to-file out
      ( lambda()
        ( format #t "~a" (page->string page) )
        ( format (current-error-port) "Saved: [~a]~%" out )
        ; return:
        #t
      )
    )
  )
  ( lambda( ex . args )
    ( format (current-error-port)
              "Cannot save file [~a]:~%  '~a: ~a~%"
              out ex args )
    ; return:
    #f
  )
  ) ; catch

) ; let
) ; page-save()



( define ( embeddable? obj )
  ; return:
  ( or
    ( component? obj )
    ( picture?   obj )
  )
)



( define ( do-embed page embed chk-embeddable )

  ( for-each
  ( lambda( comp )
      ( set-object-embedded! comp embed )
  )
  ( filter chk-embeddable (page-contents page) )
  )

) ; do-embed()



( define ( process-file file embed chk-embeddable suffix )
( let
  (
  ( page ( page-open file ) )
  )

  ( when page
    ( do-embed page embed chk-embeddable )
    ( if ( page-dirty? page )
      ( page-save page suffix )
    )
  )

) ; let
) ; process-file()



( define ( main )
( let*
  (
  ( cmd-line-args  (getopt-long (program-arguments) cmd-line-args-spec) )
  ( files          (option-ref cmd-line-args '()        '()) )
  ( arg-embed      (option-ref cmd-line-args 'embed      #f) )
  ( arg-unembed    (option-ref cmd-line-args 'unembed    #f) )
  ( arg-pics       (option-ref cmd-line-args 'pictures   #f) )
  ( arg-comps      (option-ref cmd-line-args 'components #f) )
  ( suffix         (option-ref cmd-line-args 'suffix     "") )
  ( chk-embeddable embeddable? )
  )

  ( if ( option-ref cmd-line-args 'help #f )
    ( usage 0 )
  )
  ( if ( option-ref cmd-line-args 'version #f )
    ( version )
  )

  ( if ( and arg-embed arg-unembed )
    ( usage 1 )
  )
  ( if ( and (not arg-embed) (not arg-unembed) )
    ( usage 2 )
  )

  ( if ( null? files )
    ( usage 3 )
  )

  ( if arg-pics
    ( set! chk-embeddable picture? )
  )
  ( if arg-comps
    ( set! chk-embeddable component? )
  )
  ( if ( and arg-pics arg-comps )
    ( set! chk-embeddable embeddable? )
  )


  (parse-rc "lepton-embed" "gafrc")

  ( for-each
  ( lambda( file )
    ( process-file file arg-embed chk-embeddable suffix )
  )
    files
  )

) ; let
) ; main()




( %with-toplevel
  ( %make-toplevel )
  ( lambda()
    ( main )
  )
)
