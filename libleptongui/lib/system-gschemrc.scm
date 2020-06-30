;                                                         -*-Scheme-*-
;
; Init file for lepton-schematic
;

(use-modules (gschem deprecated)
             (schematic action)
             (schematic builtins)
             (schematic gui keymap)
             (schematic gui stroke)
             (schematic netlist)
             (lepton config))

( load-from-path "conf/schematic/deprecated.scm" )
( load-from-path "conf/schematic/attribs.scm"    )
( load-from-path "conf/schematic/stroke.scm"     )
( load-from-path "conf/schematic/keys.scm"       )
( load-from-path "conf/schematic/menu.scm"       )

