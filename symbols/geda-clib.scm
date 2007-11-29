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
   (if (list? dir)
       (component-library (build-path geda-sym-path (car dir)) (cadr dir))
       (component-library (build-path geda-sym-path dir)))
   )
 (reverse '(
    "local"
  ; Generic symbols
    ("analog" "Basic devices")
    ("connector" "Connectors (generic)")
    ("diode" "Diodes (generic)")
    ("io" "Input/output (generic)")
    ("power" "Power rails")
    ("radio" "Radio elements (generic)")
    ("switch" "Switches (generic)")
    ("titleblock" "Titleblocks (generic)")
    ("IEC417" "IEC 60417")
  ; Common logic series
    ("74" "74-series logic")
    ("4000" "4000-series logic")
    ("ecl" "ECL logic")
  ; Simulation
    ("cascade" "Cascade simulation elements")
    ("spice" "SPICE simulation elements")
    ("switcap" "SWITCAP simulation elements")
  ; ASIC design
    ("asic" "Basic devices (ASIC)")
    ("asicpads" "Contact pads (ASIC)")
  ; Manufacturers
    ("allegro" "Allegro Microsystems")
    ("altera" "Altera")
    ("amphenol" "Connectors (Amphenol)")
    ("apex" "Apex Microtechnology")
    ("dec" "DEC")
    ("idt" "IDT")
    ("irf" "International Rectifier")
    ("lattice" "Lattice Semiconductor")
    ("linear" "Linear Technology")
    ("maxim" "Maxim/Dallas")
    ("minicircuits" "Mini-Circuits")
    ("national" "National Semiconductor")
    ("philips" "Philips Electronics")
    ("st" "ST Microelectronics")
    ("xilinx" "Xilinx")
  ; Misc. stuff
    ("bus" "PC104 bus")
    ("memory" "Memory devices (misc)")
    ("micro" "Microcontrollers (misc)")
    ("transistor" "Transistors (misc)")
    ("tube" "Vacuum tubes (misc)")
    ("rf" "RF elements (misc)")
    ("pla" "Programmable logic arrays (misc)")
    ("supervisor" "Microprocessor supervisors (misc)")
    ("opto" "Optocouplers (misc)")
    ("relay" "Relays (misc)")
    ("misc" "Misc. unsorted symbols")

  ; Other

    ;"verilog"
    ;"vhdl"
    ;"gnetman"
    )))
