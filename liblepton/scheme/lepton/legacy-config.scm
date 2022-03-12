;; Lepton EDA
;; liblepton - Lepton's library - Scheme API
;; Copyright (C) 2019-2020 dmn <graahnul.grom@gmail.com>
;; License: GPLv2+. See the COPYING file
;;

( define-module ( lepton legacy-config )

  #:use-module  ( ice-9 format )
  #:use-module  ( ice-9 match )
  #:use-module  ( ice-9 rdelim )  ; read-string()
  #:use-module  ( lepton config )
  #:use-module  ( lepton os )
  #:use-module  ( lepton log )
  #:use-module  ( lepton legacy-config keylist )

  #:export      ( upcfg-log )
  #:export      ( config-upgrade-old-keys )
  #:export      ( config-upgrade )
  #:export      ( config-upgrade-file )
  #:export      ( config-file-path )
  #:export      ( find-user-config-files )
  #:export      ( warning-option-deprecated )
  #:export      ( warning-option-obsolete )
)



; public:
;
( define ( upcfg-log fmt . args )
( let
  (
  ( msg ( apply format #f fmt args ) )
  )

  ( format (current-error-port) "~a" msg )
  ( log! 'message "~a" (string-trim-right msg #\newline) )

) ; let
) ; upcfg-log()


; get configuration context
;
; [cfg-id]: symbol: 'geda-local, 'geda-user
; {ret}:    requested configuration context or #f on failure
;
( define ( get-cfg-ctx cfg-id )
  ; return:
  ( cond
    (  ( eq? cfg-id 'geda-local )  ( path-config-context (getcwd) )  )
    (  ( eq? cfg-id 'geda-user  )  ( user-config-context )           )
    (  else #f  )
  )
)



; read legacy configuration
;
; [cfg]:                 configuration context to read from
; [keys]:                cfg keys list as defined in (conf keys) module
; [report-absent-keys]:  report absent keys
; {ret}:                 list of entries: ( grp-new key-new val )
;
( define* ( read-old cfg keys #:key (report-absent-keys #f) )
( let
  (
  ( val  #f )
  ( res '() )
  )

  ( define ( add-to-res g k v ) ; group, key, value
    ( set! res ( cons (list g k v) res ) )
  )

  ( for-each
  ( lambda( entry )

    ( match entry
    (

      ( pfn unused grp-old key-old grp-new key-new ) ; get 6 values from [entry]

      ( catch #t
        ( lambda()
          ( set! val ( pfn cfg grp-old key-old ) ) ; pfn() throws
          ( upcfg-log "ii: read   [~a]::~a ~65,4t = [~a]~%" grp-old key-old val )
          ( add-to-res grp-new key-new val )
        )
        ( lambda( ex . args )
          ( if report-absent-keys
            ( upcfg-log "ww: !read  [~a]::~a~%" grp-old key-old )
          )
        )
      ) ; catch()

    )
    ) ; match()

  )
  keys
  )

  ; return:
  res

) ; let
) ; read-old()



; write new configuration
;
; [cfg]: configuration context to write to
; [res]: list returned by read-old()
;
( define ( write-new cfg res )

  ( for-each
  ( lambda( entry )

    ( match entry
    (
      ( grp-new key-new val )                 ; get 3 values from [entry]
      ( set-config! cfg grp-new key-new val ) ; use that values
    )
    )

  )
  res
  )

) ; write-new()



; public:
;
; Upgrade legacy gEDA configuration
;
; NOTE: this function writes the keys defined in the
;       (lepton legacy-config keylist) module.
;       All other keys defined in the source configuration
;       file are ignored.
;
; Read legacy gEDA configuration from geda.conf (in current
; directory) or geda-user.conf file,
; convert it (using new names) and produce the corresponding
; lepton*.conf configuration file.
; We get conversion information (list of keys, old and new names) from
; the list structure defined in the (lepton legacy-config keylist) module.
;
; [cfg-id]:              'geda-local, 'geda-user - config to convert
; [report-absent-keys]:  print messages about missing keys in old cfg file
; [overwrite]:           if #t, overwrite existing new cfg file
; {thr}:                 'ctx 'infile 'load 'outfile 'save
; {ret}:                 new cfg file full name or #f on failure
;
; This function might throw the following exceptions:
; - 'ctx:      cannot get configuration context
; - 'infile:   legacy config file does not exist
; - 'load:     cannot read legacy config file
; - 'outfile:  output config file already exists
; - 'save:     cannot write output config file
;
( define* ( config-upgrade-old-keys cfg-id #:key (report-absent-keys #f) (overwrite #f) )
( let
  (
  ( cfg    #f )
  ( fname  #f )
  ( keys  '() )
  ( data  '() )
  )

  ;
  ; read legacy config file (geda*.conf):
  ;

  ( config-set-legacy-mode! #t ) ; use geda*.conf files

  ( set! cfg ( get-cfg-ctx cfg-id ) )
  ( unless cfg
    ( throw 'ctx "Cannot get config context (input)" )
  )

  ( set! fname (config-filename cfg) )
  ( unless ( and fname (access? fname F_OK) )
    ( throw 'infile "Input config file does not exist:" fname )
  )

  ( upcfg-log "ii: INPUT: ~a~%" fname )
  ( upcfg-log "~%" )

  ( catch #t
    ( lambda()
      ( config-load! cfg )
      ( set! keys (config-keylist) )
      ( set! data
        ( read-old cfg keys #:report-absent-keys report-absent-keys )
      )
    )
    ( lambda( ex . args )
      ( throw 'load "Cannot read config:" ex args )
    )
  )


  ;
  ; write new config file (lepton*.conf):
  ;

  ( upcfg-log "~%" )

  ( config-set-legacy-mode! #f ) ; use lepton*.conf files

  ( set! cfg ( get-cfg-ctx cfg-id ) )
  ( unless cfg
    ( throw 'ctx "Cannot get config context (output)" )
  )

  ( set! fname (config-filename cfg) )
  ( when ( and fname (access? fname F_OK) (not overwrite) )
    ( throw 'outfile "Output config file already exists:" fname )
  )

  ( upcfg-log "ii: OUTPUT: ~a~%" fname )

  ( catch #t
    ( lambda()
      ( write-new cfg data )
      ( config-save! cfg )
    )
    ( lambda( ex . args )
      ( throw 'save "Cannot write config:" ex args )
    )
  )

  ; return:
  fname

) ; let
) ; config-upgrade-old-keys()



; public:
;
( define ( warning-option-deprecated old-name new-group new-key )
  ; return:
  ( format #f
"
WARNING: The RC file function '~A' is deprecated.
RC configuration functions will be removed in an upcoming Lepton EDA
release. Please use .conf configuration files instead:
https://github.com/lepton-eda/lepton-eda/wiki/Configuration-Settings

Put the following lines into the ~A file (in current directory)
or ~A (in user's configuration directory, typically
$HOME/.config/lepton-eda/), replacing MYVALUE with the desired
option's value:

[~A]
~A=MYVALUE

"
    old-name
    ( basename ( config-filename (path-config-context (getcwd)) ) )
    ( basename ( config-filename (user-config-context) ) )
    new-group
    new-key
  )

) ; warning-option-deprecated()



; public:
;
( define ( warning-option-obsolete old-name )
  ; return:
  ( format #f
"
WARNING: The RC file function '~A' is deprecated
and does nothing.
RC configuration functions will be removed in an upcoming Lepton EDA
release. Please use .conf configuration files instead:
https://lepton-eda.github.io/lepton-manual.html/Configuration.html

"
    old-name
  )

) ; warning-option-obsolete()



; "/dir" "subdir" "file" => "/dir/subdir/file"
;
( define ( path-join name . names )
  ( string-join (cons name names) file-name-separator-string )
)


( define ( upcfg-tmp-dir )
  ( path-join ( user-cache-dir ) "upcfg" )
)


; temporary file with upgraded configuration
;
( define ( upcfg-tmp-file )
  ( path-join ( upcfg-tmp-dir ) "lepton.conf" )
)


; [cfg-id]: symbol, either:
; - 'geda-local   =>  $PWD/geda.conf
; - 'geda-user1   =>  $HOME/.gEDA/geda-user.conf
; - 'geda-user2   =>  $XDG_CONFIG_HOME/gEDA/geda-user.conf
; - 'geda-user3   =>  $XDG_CONFIG_HOME/lepton-eda/geda-user.conf
; - 'lepton-local =>  $PWD/lepton.conf
; - 'lepton-user  =>  $XDG_CONFIG_HOME/lepton-eda/lepton-user.conf
;
( define ( config-file-path cfg-id )
  ( define ( geda-local )
    ( path-join (getcwd) "geda.conf" )
  )
  ( define ( geda-user1 )
    ( path-join (getenv "HOME") ".gEDA" "geda-user.conf" )
  )
  ( define ( geda-user2 )
    ( if (getenv "XDG_CONFIG_HOME")
      ( path-join (getenv "XDG_CONFIG_HOME") "gEDA" "geda-user.conf" ) ; if
      ( path-join (getenv "HOME") ".config" "gEDA" "geda-user.conf" )  ; else
    )
  )
  ( define ( geda-user3 )
    ( path-join (user-config-dir) "geda-user.conf" )
  )
  ( define ( lepton-local )
    ( path-join (getcwd) "lepton.conf" )
  )
  ( define ( lepton-user )
    ( path-join (user-config-dir) "lepton-user.conf" )
  )

  ; return:
  ( cond
    ( (eq? cfg-id 'geda-local)    (geda-local)   )
    ( (eq? cfg-id 'geda-user1)    (geda-user1)   )
    ( (eq? cfg-id 'geda-user2)    (geda-user2)   )
    ( (eq? cfg-id 'geda-user3)    (geda-user3)   )
    ( (eq? cfg-id 'lepton-local)  (lepton-local) )
    ( (eq? cfg-id 'lepton-user)   (lepton-user)  )
    ( else
      ( throw 'wrong-cfg-id "config-file-path(): wrong cfg-id" )
    )
  )
) ; config-file-path()


; geda cfg id => lepton cfg id
;
( define ( target-cfg-id src-cfg-id )
  ; return:
  ( cond
    ( (eq? src-cfg-id 'geda-local)  'lepton-local )
    ( (eq? src-cfg-id 'geda-user1)  'lepton-user  )
    ( (eq? src-cfg-id 'geda-user2)  'lepton-user  )
    ( (eq? src-cfg-id 'geda-user3)  'lepton-user  )
    ( else
      ( throw 'wrong-cfg-id "target-cfg-id(): wrong cfg-id" )
    )
  )
)


( define ( config-file-readable? cfg-id )
  ( access? (config-file-path cfg-id) R_OK )
)


( define ( upcfg-mkdir path )

  ( define ( exec-mkdir )
    ( status:exit-val (system* "mkdir" "-p" path) )
  )

  ( upcfg-log "ii: creating directory: [~a]~%" path )

  ( unless ( eq? 0 (exec-mkdir) )
    ( throw 'mkdir "cannot create directory" path )
  )

) ; upcfg-mkdir()


; write [fpath] file contents to STDOUT
;
( define ( upcfg-print-file fpath )
  ( with-input-from-file
    fpath
    ( lambda()
      ( format (current-output-port) "~a" ( read-string (current-input-port) ) )
    )
  )
)


( define* ( upcfg-copy-file src dest #:key (overwrite #t) )

  ( upcfg-log "ii: copying: [~a] => [~a]~%" src dest )

  ( if ( and (not overwrite) (file-exists? dest) )
    ( throw 'file-exists "target file exists" dest )
  )

  ( unless ( false-if-exception ( copy-file src dest ) )
    ( throw 'copy-failed "unable to copy file" src )
  )

) ; upcfg-copy-file()



; upgrade config in config context [ctx]
; {pre}: [ctx] is config-load!()'ed
; {thr}: 'write
;
( define ( upgrade-ctx ctx )
( let
  (
  ( val-old #f )
  ( val-old-read #f )
  )

  ( define ( group-empty? grp )
    ( null? (config-keys ctx grp) )
  )

  ( define ( delete-old-key grp key )
    ( false-if-exception (config-remove-key! ctx grp key) )
    ( if ( group-empty? grp )
      ( false-if-exception (config-remove-group! ctx grp) )
    )
  )

  ( define ( write-new-key grp-new key-new val-old )
    ( false-if-exception (set-config! ctx grp-new key-new val-old) )
  )

  ( define ( read-old-key pfn grp-old key-old )
    ( catch #t
      ( lambda()
        ( set! val-old ( pfn ctx grp-old key-old ) )
        ( set! val-old-read #t )
      )
      ( lambda( ex . args )
        ( set! val-old-read #f )
        ( upcfg-log "ii: skipping:  [~a]::~a - no such key~%" grp-old key-old )
      )
    )
  )

  ( define ( convert-key pfn grp-old key-old grp-new key-new )
    ( read-old-key pfn grp-old key-old )
    ( when val-old-read
      ( upcfg-log "ii: upgrading: [~a]::~a => [~a]::~a~%" grp-old key-old grp-new key-new )
      ( if ( write-new-key grp-new key-new val-old )
        ( delete-old-key grp-old key-old )                      ; if
        ( throw 'write "cannot write new key" grp-new key-new ) ; else
      )
    )
  )

  ( define ( process-entry entry )
    ( match entry ; match 6 values in the [entry] list:
    (
      ( pfn default-val grp-old key-old grp-new key-new )
      ( convert-key pfn grp-old key-old grp-new key-new )
    )
    )
  )


  ( for-each process-entry (config-keylist) )

) ; let
) ; upgrade-ctx()



; - create tmp dir
; - copy config file [fpath] to tmp file in tmp dir
; - upgrade config in that tmp file
;
( define ( upgrade-file fpath )
( let
  (
  ( ctx #f )
  )

  ( upcfg-mkdir (upcfg-tmp-dir) )
  ( upcfg-copy-file fpath (upcfg-tmp-file) )

  ( config-set-legacy-mode! #f )
  ( set! ctx ( path-config-context (upcfg-tmp-dir) ) )

  ( config-load! ctx #:force-load #t )
  ( upgrade-ctx ctx )

  ( if ( config-changed? ctx )
    ( upcfg-log "ii: config has been upgraded~%"  )   ; if
    ( upcfg-log "ii: config has not been changed~%" ) ; else
  )

  ( config-save! ctx ) ; NOTE: config-save!() raises exception on failure

) ; let
) ; upgrade-file()



; public:
;
; Upgrade legacy gEDA configuration
;
; [cfg-id]:    config to upgrade: symbol, see config-file-path()
; [copy]:      #t => write result to config file, #f => print to STDOUT
; [overwrite]: if [copy] is #t, whether to overwrite existing file
;
; {thr}: does not throw exceptions
; {ret}: #t on success, #f on failure
;
( define* ( config-upgrade cfg-id #:key (copy #f) (overwrite #f) )
( let
  (
  ( res #f )
  ( target-id (target-cfg-id cfg-id) )
  )

  ( define ( mk-user-cfg-dir )
    ( unless ( file-exists? (user-config-dir) )
      ( upcfg-mkdir (user-config-dir) )
    )
  )


  ( catch #t
  ( lambda()
    ( unless ( config-file-readable? cfg-id )
      ( throw 'src-file "cannot read config file" (config-file-path cfg-id) )
    )

    ( upgrade-file (config-file-path cfg-id) )

    ( if ( and copy (eq? target-id 'lepton-user) )
      ( mk-user-cfg-dir )
    )

    ( if copy
      ( upcfg-copy-file ; if
        ( upcfg-tmp-file )
        ( config-file-path target-id )
        #:overwrite overwrite
      )
      ( upcfg-print-file (upcfg-tmp-file) ) ; else
    )

    ( set! res #t )
  )
  ( lambda( ex . args )
    ( upcfg-log "ee: ['~a]:~%  ~s~%" ex args )
  )
  ) ; catch()

  ; return:
  res

) ; let
) ; config-upgrade()



; public:
;
; Upgrade legacy gEDA configuration read from file [fpath],
; print result to standard output.
;
; {thr}: does not throw exceptions
; {ret}: #t on success, #f on failure
;
( define ( config-upgrade-file fpath )
( let
  (
  ( res #f )
  )

  ( catch #t
  ( lambda()
    ( unless ( access? fpath R_OK )
      ( throw 'src-file "cannot read config file" fpath )
    )

    ( upgrade-file fpath )
    ( upcfg-print-file (upcfg-tmp-file) )

    ( set! res #t )
  )
  ( lambda( ex . args )
    ( upcfg-log "ee: ['~a]:~%  ~s~%" ex args )
  )
  ) ; catch()

  ; return:
  res

) ; let
) ; config-upgrade-file()



; public:
;
; Find available user configuration files.
;
; {ret}: list of config ids, sorted by file modification time
;        (recently updated first).
;
( define ( find-user-config-files )

  ( define ( mtime>? a b )
    ( >
      ( stat:mtime ( stat (config-file-path a) ) )
      ( stat:mtime ( stat (config-file-path b) ) )
    )
  )

( let*
  (
  ( ids (list 'geda-user1 'geda-user2 'geda-user3) )
  ( ids-readable (filter config-file-readable? ids) )
  ( ids-by-mtime (sort ids-readable mtime>?) )
  )

  ; return:
  ids-by-mtime

) ; let
) ; find-user-config-files()

