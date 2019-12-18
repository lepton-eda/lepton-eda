;                                                         -*-Scheme-*-
;
; Init file for lepton-schematic
;

(use-modules (gschem deprecated)
             (schematic gui keymap)
             (schematic gui stroke)
             (schematic netlist)
             (gschem builtins)
             (gschem action)
             (geda config))


; gschem-version string
;
; Specifies the version of this file.
; This number is used to make sure that the rc file is compatible
; with the version of lepton-schematic that is being run.
; The end user should *not* change this value.
;
( gschem-version "@DATE_VERSION@" )


( load-from-path "conf/schematic/deprecated.scm" )
( load-from-path "conf/schematic/attribs.scm"    )
( load-from-path "conf/schematic/stroke.scm"     )
( load-from-path "conf/schematic/keys.scm"       )
( load-from-path "conf/schematic/menu.scm"       )

