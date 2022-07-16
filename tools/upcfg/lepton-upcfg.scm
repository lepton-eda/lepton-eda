;;
;; Lepton EDA
;; lepton-upcfg - gEDA => Lepton EDA configuration upgrade utility
;; Copyright (C) 2019 dmn <graahnul.grom@gmail.com>
;; Copyright (C) 2019-2022 Lepton EDA Contributors
;; License: GPLv2+. See the COPYING file
;;

(use-modules (ice-9 format)
             (ice-9 rdelim) ; read-line()
             (ice-9 getopt-long)
             (lepton ffi)
             (lepton legacy-config)
             (lepton log)
             (lepton version))

;; Initialize liblepton library.
(liblepton_init)
(unless (getenv "LEPTON_INHIBIT_RC_FILES")
  (register-data-dirs))


; command line options:
;
( define cmd-line-args-spec
( list
  ( list ; --local (-l)
    'local
    ( list 'single-char #\l )
    ( list 'value        #f )
  )
  ( list ; --user (-u)
    'user
    ( list 'single-char #\u )
    ( list 'value        #f )
  )
  ( list ; --copy (-c)
    'copy
    ( list 'single-char #\c )
    ( list 'value        #f )
  )
  ( list ; --overwrite (-x)
    'overwrite
    ( list 'single-char #\x )
    ( list 'value        #f )
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
Usage: lepton-upcfg [OPTIONS] | FILE

Lepton EDA configuration upgrade utility.
Converts geda*.conf configuration files
to corresponding lepton*.conf files.
By default, the upgraded configuration
is printed to standard output.
One of the options can be passed on the
command line to upgrade the configuration
located in predefined locations. Instead of
the options, one can specify the
configuration file path to convert and
redirect the output to some destination file.

Options:
  -l, --local      geda.conf => lepton.conf
                   (in current directory)
  -u, --user       geda-user.conf => lepton-user.conf
                   (in user configuration directory)
  -c, --copy       Write to configuration file, not STDOUT
  -x, --overwrite  Overwrite existing files
  -h, --help       Show usage information
  -V, --version    Show version information

Report bugs at <~a>
Lepton EDA homepage: <~a>
"
    ( lepton-version-ref 'bugs )
    ( lepton-version-ref 'url )
  )

  ( primitive-exit exit-code )
)



( define ( version )
  ( display-lepton-version #:print-name #t #:copyright #t )
  ( primitive-exit 0 )
)



( define ( failure )
  ( upcfg-log "lepton-upcfg: configuration upgrade failed.~%" )
  ( primitive-exit 1 )
)



( define ( prompt-for-user-cfg ids )
( let
  (
  ( i 0 )
  ( ndx #f )
  )

  ( define ( input-error )
    ( upcfg-log "ee: wrong value entered~%" )
    ( failure )
  )

  ( upcfg-log "ii: more than one user configuration files found:~%" )
  ( while ( < i (length ids) )
    ( upcfg-log "~d: ~a~%" (1+ i) (config-file-path (list-ref ids i)) )
    ( set! i (1+ i) )
  )

  ( upcfg-log "Type config file number to upgrade and press Enter: " )
  ( set! ndx (false-if-exception (read-line)) )
  ( set! ndx (false-if-exception (string->number ndx)) )

  ( unless ndx
    ( input-error )
  )

  ( upcfg-log "ii: choice: [ ~a ]~%" ndx )
  ( set! ndx ( 1- ndx ) )
  ( if ( or (< ndx 0) (>= ndx (length ids)) )
    ( input-error )
  )

  ; return:
  ( list-ref ids ndx )

) ; let
) ; prompt-for-user-cfg()



( define ( get-user-cfg-id )
( let
  (
  ( ids (find-user-config-files) )
  )

  ( when ( null? ids )
    ( upcfg-log "ii: no user configuration files found.~%" )
    ( primitive-exit 0 )
  )

  ; return:
  ( if ( > (length ids) 1 )
    ( prompt-for-user-cfg ids ) ; if
    ( list-ref ids 0 )          ; else
  )

) ; let
) ; get-user-cfg-id()




; program entry point:
;
( define ( main )
( let*
  (
  ( cmd-line-args (getopt-long (program-arguments) cmd-line-args-spec) )
  ( files         (option-ref cmd-line-args '() '()) )
  ( copy          (option-ref cmd-line-args 'copy #f) )
  ( overwrite     (option-ref cmd-line-args 'overwrite #f) )
  ( args-len      0  )
  ( cfg-id        #f )
  )

  ( init-log "upcfg" )

  ( set! args-len (length cmd-line-args) )
  ( when ( and (null? files) (or (< args-len 2) (> args-len 4)) )
    ( usage 1 )
  )

  ( if (option-ref cmd-line-args 'help #f)
    ( usage 0 )
  )
  ( if (option-ref cmd-line-args 'version #f)
    ( version )
  )

  ( if (option-ref cmd-line-args 'local #f)
    ( set! cfg-id 'geda-local )
  )
  ( if (option-ref cmd-line-args 'user #f)
    ( set! cfg-id ( get-user-cfg-id ) )
  )

  ( if ( and (null? files) (not cfg-id) )
    ( usage 1 )
  )


  ( upcfg-log
    "ii: upgrading config in [~a]...~%"
    ( if cfg-id
      ( config-file-path cfg-id ) ; if
      ( list-ref files 0 )        ; else
    )
  )

  ( if ( null? files )
    ( or (config-upgrade cfg-id #:copy copy #:overwrite overwrite) (failure) ) ; if
    ( or (config-upgrade-file (list-ref files 0))                  (failure) ) ; else
  )

) ; let
) ; main()



; top-level code:
;
( main )
