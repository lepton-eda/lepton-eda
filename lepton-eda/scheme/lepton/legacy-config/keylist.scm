;; Lepton EDA
;; liblepton - Lepton's library - Scheme API
;; Copyright (C) 2019 dmn <graahnul.grom@gmail.com>
;; Copyright (C) 2019-2020 Lepton EDA Contributors
;; License: GPLv2+. See the COPYING file
;;

( define-module ( lepton legacy-config keylist )
  #:use-module  ( lepton config )
  #:export      ( config-keylist )
)



; functions to retrieve configuration values:
;
( define pfn-get-bool     config-boolean     )
( define pfn-get-str      config-string      )
( define pfn-get-str-list config-string-list )



; configuration keys list:
;
( define keys-list
( list

  ; group: gnetlist / netlist
  ;
  ( list                ; configuration key entry consists of:
    pfn-get-str           ; function to read the old value
    "unnamed_net"         ; default value
    "gnetlist"            ; group name - old (in geda*.conf)
    "default-net-name"    ; key name   - old (in geda*.conf)
    "netlist"             ; group name - new (in lepton*.conf)
    "default-net-name"    ; key name   - new (in lepton*.conf)
  )
  ( list pfn-get-str "unnamed_bus"
         "gnetlist" "default-bus-name"
         "netlist"  "default-bus-name"
  )
  ( list pfn-get-str "net-attribute"
         "gnetlist" "net-naming-priority"
         "netlist"  "net-naming-priority"
  )

  ; group: gnetlist.hierarchy / netlist.hierarchy
  ;
  ( list pfn-get-bool #t
         "gnetlist.hierarchy" "traverse-hierarchy"
         "netlist.hierarchy"  "traverse-hierarchy"
  )
  ; refdes:
  ( list pfn-get-bool #t
         "gnetlist.hierarchy" "mangle-refdes-attribute"
         "netlist.hierarchy"  "mangle-refdes-attribute"
  )
  ( list pfn-get-str "/"
         "gnetlist.hierarchy" "refdes-attribute-separator"
         "netlist.hierarchy"  "refdes-attribute-separator"
  )
  ( list pfn-get-bool #f
         "gnetlist.hierarchy" "refdes-attribute-order"
         "netlist.hierarchy"  "refdes-attribute-order"
  )
  ; netname:
  ( list pfn-get-bool #t
         "gnetlist.hierarchy" "mangle-netname-attribute"
         "netlist.hierarchy"  "mangle-netname-attribute"
  )
  ( list pfn-get-str "/"
         "gnetlist.hierarchy" "netname-attribute-separator"
         "netlist.hierarchy"  "netname-attribute-separator"
  )
  ( list pfn-get-bool #f
         "gnetlist.hierarchy" "netname-attribute-order"
         "netlist.hierarchy"  "netname-attribute-order"
  )
  ; net:
  ( list pfn-get-bool #t
         "gnetlist.hierarchy" "mangle-net-attribute"
         "netlist.hierarchy"  "mangle-net-attribute"
  )
  ( list pfn-get-str "/"
         "gnetlist.hierarchy" "net-attribute-separator"
         "netlist.hierarchy"  "net-attribute-separator"
  )
  ( list pfn-get-bool #f
         "gnetlist.hierarchy" "net-attribute-order"
         "netlist.hierarchy"  "net-attribute-order"
  )

  ; group: gschem.library / schematic.library
  ;
  ( list pfn-get-str-list (list "*")
         "gschem.library"    "component-attributes"
         "schematic.library" "component-attributes"
  )
  ( list pfn-get-bool #f
         "gschem.library"    "sort"
         "schematic.library" "sort"
  )

  ; group: gschem.printing / schematic.printing
  ;
  ( list pfn-get-str "auto"
         "gschem.printing"    "layout"
         "schematic.printing" "layout"
  )
  ( list pfn-get-bool #f
         "gschem.printing"    "monochrome"
         "schematic.printing" "monochrome"
  )
  ( list pfn-get-str ""
         "gschem.printing"    "paper"
         "schematic.printing" "paper"
  )

  ; group: gschem / schematic
  ;
  ( list pfn-get-str "untitled"
         "gschem"    "default-filename"
         "schematic" "default-filename"
  )

) ; list()
) ; keys-list



; public:
;
; get configuration keys list
;
( define ( config-keylist )
  ; return:
  keys-list
)

