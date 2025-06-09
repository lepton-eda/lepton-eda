;; Lepton EDA
;; Copyright (C) 2019 dmn <graahnul.grom@gmail.com>
;; License: GPLv2+. See the COPYING file.
;;
;; Precompilation script
;;

( use-modules ( system base compile ) )
( use-modules ( ice-9 format ) )
( use-modules ( ice-9 ftw    ) )



( define ( dbg-out )
  ; return:
  ( current-error-port )
)



( define ( std-out )
  ; return:
  ( current-output-port )
)



; Compile [ifile] to [ofile]
;
; {ret}: #t on success, #f otherwise
;
( define ( precompile-file ifile ofile )
( let
  (
  ( res #f )
  ( ret #f )
  )

  ; compile-file():
  ;
  ;   {ret}:  [ofile]
  ;   {thr}:  'system-error if [ifile] not found
  ;   {post}: creates directories
  ;
  ( catch #t
  ( lambda()
    ( format (dbg-out) "compiling:~%" )
    ( format (dbg-out) "~a~%" ifile )
    ( set! res ( compile-file ifile #:output-file ofile ) )
    ( format (dbg-out) "compiled:~%" )
    ( format (std-out) "~a~%" res )
    ( set! ret #t )
  )
  ( lambda( ex . args )
    ( format (dbg-out) "  ! failed: [~a]~%  ~a~%" ex args )
    ( false-if-exception ( delete-file ofile ) )
  )
  ) ; catch()

  ; return:
  ret

) ; let
) ; precompile-file()



; Compile *[ext] files in [sdir], put compiled files to [odir]
;
( define* ( precompile-dir sdir odir #:key (recursive #t) (ext ".scm") )
( let
  (
  ( ifile     #f )
  ( ifile-rel #f )
  ( ofile     #f )
  )

  ( define ( chk-level lvl )
    ; return:
    ( if recursive
      #t            ; if
      ( eq? lvl 1 ) ; else
    )
  )

  ( define ( strip-scm-dir str )
    ; return:
    ( string-drop str (+ 1 (string-length sdir)) ) ; +1 <=> "/"
  )

  ( define ( strip-suffix str suffix )
    ; return:
    ( if ( and (not (string-null? suffix)) (string-suffix? suffix str) )
      ( string-drop-right str (string-length suffix) ) ; if
      str                                              ; else
    )
  )

  ( define ( append-suffix str suffix )
    ; return:
    ( format #f "~a~a" str suffix )
  )

  ( define ( eligible fname flag ) ; should [fname] file be compiled?
    ; return:
    ( and
      ( eq? flag 'regular )
      ( string-suffix? ext fname )
    )
  )

  ( define ( nftw-proc filename statinfo flag base level )

    ( when ( and (eligible filename flag) (chk-level level) )

      ( set! ifile filename )
      ( set! ifile-rel (strip-scm-dir ifile) )

      ( set! ofile (format #f "~a/~a" odir ifile-rel) )

      ( set! ofile (strip-suffix  ofile ext) )
      ( set! ofile (append-suffix ofile ".go") )

      ( precompile-file ifile ofile )

    ) ; when

    ; return: // #t => continue walk
    #t

  ) ; nftw-proc()


  ( nftw sdir nftw-proc )

) ; let
) ; precompile-dir()



( define ( precompile scm-dir out-dir recursive ext )

  ; add dir with scm files to %load-path:
  ;
  ( add-to-load-path scm-dir )

  ( precompile-dir scm-dir out-dir #:recursive recursive #:ext ext )

) ; precompile()



( define* ( config-get-env-var name #:key (fail-if-unset #t) (default-val "") )
( let*
  (
  ( value ( getenv name ) )
  )

  ( when ( and (not value) fail-if-unset )
    ( format (dbg-out) "ee: [~a] environment variable is not set~%" name )
    ( primitive-exit 1 )
  )

  ; return:
  ( or value default-val )

) ; let
) ; config-get-env-var()



( define ( config-get-scm-dir )
  ; return:
  ( config-get-env-var "LEPTON_SCM_PRECOMPILE_SCM_DIR" )
)



( define ( config-get-out-dir )
  ; return:
  ( config-get-env-var "LEPTON_SCM_PRECOMPILE_OUT_DIR" )
)



( define ( config-get-recursive )
  ; return:
  ( config-get-env-var
    "LEPTON_SCM_PRECOMPILE_RECURSIVE"
    #:fail-if-unset #f
    #:default-val "1"
  )
)



( define ( config-get-fname-ext )
  ; return:
  ( config-get-env-var
    "LEPTON_SCM_PRECOMPILE_FNAME_EXT"
    #:fail-if-unset #f
    #:default-val ".scm"
  )
)



( define ( dbg-header )

  ( format (dbg-out) "++: ~a: starting...~%"
                     ( current-filename ) )
  ( format (dbg-out) "~%" )

  ( format (dbg-out) "ii: scm dir:     ~a~%" (config-get-scm-dir) )
  ( format (dbg-out) "ii: out dir:     ~a~%" (config-get-out-dir) )

  ( format (dbg-out) "~%" )
  ( format (dbg-out) "ii: %load-compiled-extensions: [~a]~%~%"
                     %load-compiled-extensions )

  ( format (dbg-out) "ii: $GUILE_LOAD_PATH:  ~a~%"
                     ( getenv "GUILE_LOAD_PATH" ) )
  ( format (dbg-out) "ii: %load-path:~%~{  ~a~%~}~%"
                     %load-path )

  ( format (dbg-out) "ii: $GUILE_LOAD_COMPILED_PATH:  ~a~%"
                     ( getenv "GUILE_LOAD_COMPILED_PATH" ) )
  ( format (dbg-out) "ii: %load-compiled-path:~%~{  ~a~%~}~%"
                     %load-compiled-path )

) ; dbg-header()



( define ( dbg-footer )

  ( format (dbg-out) "~%" )
  ( format (dbg-out) "~%--: ~a: done~%"
                     ( current-filename ) )
  ( format (dbg-out) "~%" )

) ; dbg-footer()




;
; top-level code:
;

( dbg-header )

( precompile
  ( config-get-scm-dir )
  ( config-get-out-dir )
  ( config-get-recursive )
  ( config-get-fname-ext )
)

( dbg-footer )



; vim: ft=scheme tabstop=2 softtabstop=2 shiftwidth=2 expandtab

