;                                                         -*-Scheme-*-
;;;
;;; Add the default component libraries
;;;

(define geda-sym-path (build-path geda-data-path "sym"))

; NOTE: Some of the below component libraries below are commented out.
;       This was done because there are conflicting filenames within these
;       libraries.  
(for-each
 (lambda (dir)
   (component-library (build-path geda-sym-path dir)))
'(
  "74"
  "4000"
  "IEC417"
  "amphenol"
  "analog"
  "linear"
  "altera"
  "lattice"
  "xilinx"
  "idt"
  "misc"
  "power"
  "philips"
  "minicircuits"
  "st"
  "apex"
  "allegro"
  "irf"
  "transistor"
  "io"
  "titleblock"
  "memory"
  "micro"
  "maxim"
  "national"
  "radio"
  "tube"
  "connector"
  "switch"
  "switcap"
  ;"verilog"
  ;"vhdl"
  "spice"
  "rf"
  "bus"
  "pla"
  "ecl"
  "dec"
  "supervisor"
  "opto"
  "diode"
  "relay"
  "cascade"
  "asic"
  "asicpads"
  ;"gnetman"
  "local"
  ))
