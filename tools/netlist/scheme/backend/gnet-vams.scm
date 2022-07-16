;;; Lepton EDA netlister
;;; Copyright (C) 1998-2010 Ales Hvezda
;;; Copyright (C) 1998-2017 gEDA Contributors
;;; Copyright (C) 2018-2020 Lepton EDA Contributors
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.

;;; --------------------------------------------------------------------------
;;;
;;;  VHDL-AMS netlist backend written by Eduard Moser and Martin Lehmann.
;;;  Build on the VHDL backend from Magnus Danielson
;;;
;;; --------------------------------------------------------------------------

(use-modules (srfi srfi-1)
             (srfi srfi-26)
             (ice-9 format)
             (netlist port)
             (netlist schematic-component)
             (netlist schematic)
             (netlist schematic toplevel))

;;; ===================================================================================
;;;                  TOP LEVEL FUNCTION
;;;                        BEGIN

;;;   Write structural VAMS representation of the schematic

;;;   absolutly toplevel function of gEDA gnelist vams mode.
;;;   its evaluate things like output-file, generate-mode, top-attribs
;;;   and starts the major subroutines.

(define (vams output-filename)
  (define (architecture-name output-filename entity)
    (string-append
     (dirname output-filename)
     "/"
     (string-downcase entity)
     "_arc"
     (substring output-filename
                (string-rindex output-filename #\. 0
                               (string-length output-filename))
                (string-length output-filename))))

  (define (entity-name output-filename entity)
    (string-append
     (dirname output-filename)
     "/"
     (string-downcase entity)
     ".vhdl"))

  (let* (
         ;; generate correctly architecture name
         (architecture (vams:change-all-whitespaces-to-underlines
                        (or (schematic-toplevel-attrib (toplevel-schematic)
                                                       'architecture)
                            "default_architecture")))

         ;; generate correctly entity name
         (entity (vams:change-all-whitespaces-to-underlines
                  (or (schematic-toplevel-attrib (toplevel-schematic)
                                                 'entity)
                      "default_entity")))
         (top-attribs (map symbol->string top-attribs))

         ;; search all ports of a schematic. for entity generation only.
         (port-list  (vams:generate-port-list (vams:get-uref top-attribs)))

         ;; search all generic of a schematic. for entity generatin only.
         (generic-list (vams:generate-generic-list top-attribs)))


    ;; generate-mode : 1 (default) -> generate a architecture (netlist) of a
    ;;                                schematic
    ;;                 2           -> is selected a component then generate
    ;;                                a entity of this, else generate
    ;;                                a toplevel entity. called from gschem
    ;;                                normally.

    (cond ((= generate-mode 1)
           (let ((thunk (lambda () (vams:write-secondary-unit architecture
                                                         entity
                                                         (schematic-package-names (toplevel-schematic))))))
             (if output-filename
                 ;; generate output-filename, like
                 ;; (<entity>_arc.<output-file-extension>)
                 (let ((filename (architecture-name output-filename entity)))
                   (message (format #f
                                    "\ngenerating architecture of current schematic in ~A\n"
                                    filename))
                   (with-output-to-file filename thunk))
                 (thunk))))

          ((= generate-mode 2)
           (let* ((top-attribs-refdes (vams:get-uref top-attribs))
                  (refdes (if (null? top-attribs-refdes)
                              entity
                              (get-device top-attribs-refdes)))
                  ;; decide about the right parameters for entity-declaration
                  (thunk (lambda () (vams:write-primary-unit refdes
                                                        port-list
                                                        generic-list))))
             (if output-filename
                 ;; generate output-filename
                 ;; (<device of selected component>.vhdl), else
                 ;; <entity>.vhdl
                 (let ((filename (entity-name output-filename refdes)))
                   (message (format #f
                                    "\n\ngenerating entity of current schematic in ~A\n"
                                    filename))
                   (with-output-to-file filename thunk))
                 (thunk)))))))


;;;                  TOP LEVEL FUNCTION
;;;                        END

;;; ===================================================================================

;;;
;;;              ENTITY GENERATING PART
;;;                     BEGIN


;;; Context clause
;;;
;;; According to IEEE 1076-1993 11.3:
;;;
;;; context_clause := { context_item }
;;; context_item := library_clause | use_clause
;;;
;;; Implementation note:
;;;    Both library and use clauses will be generated, eventually...
;;;    What is missing is the information from gEDA itself, i think.


;;; writes some needed library insertions staticly
;;; not really clever, but a first solution

(define (vams:write-context-clause)
  (format #f "LIBRARY ieee,disciplines;
USE ieee.math_real.all;
USE ieee.math_real.all;
USE work.electrical_system.all;
USE work.all;
"
          ))


;;; Primary unit
;;;
;;; According to IEEE 1076-1993 11.1:
;;;
;;; primary_unit :=
;;;    entity_declaration
;;;  | configuration_declaration
;;;  | package_declaration
;;;
;;; Implementation note:
;;;    We assume that gEDA does not generate either a configuration or
;;;    package declaration. Thus, only a entity declaration will be generated.
;;;
;;; According to IEEE 1076-1993 1.1:
;;;
;;; entity_declaration :=
;;;    ENTITY identifier IS
;;;       entity_header
;;;       entity_declarative_part
;;;  [ BEGIN
;;;       entity_statement_part ]
;;;    END [ ENTITY ] [ entity_simple_name ] ;
;;;
;;; Implementation note:
;;;    We assume that no entity declarative part and no entity statement part
;;;    is to be produced. Further, it is good custom in VAMS-93 to append
;;;    both the entity keyword as well as the entity simple name to the
;;;    trailer, therefore this is done to keep VAMS compilers happy.
;;;
;;; According to IEEE 1076-1993 1.1.1:
;;;
;;; entity_header :=
;;;  [ formal_generic_clause ]
;;;  [ formal_port_clause ]
;;;
;;; Implementation note:
;;;    Initially we will assume that there is no generic clause but that there
;;;    is an port clause. We would very much like to have generic and the port
;;;    clause should be conditional (consider writting a test-bench).


;;; this routine managed the complete entity-declaration of a component
;;; or a schematic. It requires the entity-name, all ports and generics
;;; of this entity and the output-port. the output-port defines where
;;; this all should wrote to.

(define (vams:write-primary-unit entity port-list generic-list)

  (format #t "~A
-- Entity declaration --

ENTITY ~A IS
~A~
~A~
END ENTITY ~A;

"
          (vams:write-context-clause)
          entity
          (vams:write-generic-clause generic-list)
          (vams:write-port-clause port-list)
          entity))



;;; GENERIC & PORT Clause
;;;
;;; According to IEEE 1076-1993 1.1.1:
;;;
;;; entity_header :=
;;;  [ formal_generic_clause ]
;;;  [ formal_port_clause ]
;;;
;;; generic_clause :=
;;;    GENERIC ( generic_list ) ;
;;;
;;; port_clause :=
;;;    PORT ( port_list ) ;
;;;
;;; According to IEEE 1076-1993 1.1.1.2:
;;;
;;; port_list := port_interface_list
;;;
;;; According to IEEE 1076-1993 4.3.2.1:
;;;
;;; interface_list := interface_element { ; interface_element }
;;;
;;; interface_element := interface_declaration
;;;
;;; According to IEEE 1076-1993 4.3.2:
;;;
;;; interface_declaration :=
;;;    interface_constant_declaration
;;;  | interface_signal_declaration
;;;  | interface_variable_declaration
;;;  | interface_file_declaration
;;;
;;; interface_signal_declaration :=
;;;  [ SIGNAL ] identifier_list : [ mode ] subtype_indication [ BUS ]
;;;  [ := static_expression ]
;;;
;;; mode := IN | OUT | INOUT | BUFFER | LINKAGE
;;;
;;; Implementation note:
;;;    Since the port list must contain signals will only the interface
;;;    signal declaration of the interface declaration be valid. Further,
;;;    we may safely assume that the SIGNAL symbol will not be needed.
;;;    The identifier list is reduced to a signle name entry, mode is set
;;;    to in, out or inout due to which part of the port list it comes from.
;;;    The mode types supported are in, out and inout where as buffer and
;;;    linkage mode is not supported. The subtype indication is currently
;;;    hardwired to standard logic, but should be controlled by attribute.
;;;    There is currently no support for busses and thus is the BUS symbol
;;;    no being applied. Also, there is currently no static expression
;;;    support, this too may be conveyed using attributes.


;;; this next two functions are writing the generic-clause
;;; in the entity declaration
;;; vams:write-generic-clause requires a list of all generics and
;;; its values, such like ((power 12.2) (velocity 233.34))

(define (vams:write-generic-clause generic-list)
  (define (format-generic generic)
    (if (= 2 (length generic))
	(let ((name (first generic))
	      (value (second generic)))
	  (format #f "~A : REAL := ~A" name value))
	""))
  (if (null? generic-list)
      ""
      (format #f "\t GENERIC (\t~A );\n"
              (string-join (map format-generic generic-list) ";\n\t\t\t"))))



;;; this function writes the port-clause in the entity-declarartion
;;; It requires a list of ports. ports stand for a list of all
;;; pin-attributes.
;;; This little routine writes a single pin on the port-clause.
;;; It requires a list containing (port_name, port_object, port_type, port_mode)
;;; such like
;;; ((heat quantity thermal in) (base terminal electrical unknown) .. )


(define (vams:write-port-clause port-list)
  (define (format-port port)
    (if (equal? (length port) 4)
	  (let ((name (first port))
		(object (second port))
		(type (third port))
		(mode (fourth port)))
	    (format #f "~A \t~A \t: ~A \t~A"
		    object
		    name
		    (if (equal? object 'quantity) mode "")
		    type))
	  ""))
  (if (null? port-list)
      ""
      (format #f "\t PORT (\t\t~A );\n"
              (string-join (map format-port port-list) ";\n\t\t\t"))))



;;;              ENTITY GENERATING PART
;;;                     END

;;; ===================================================================================

;;;           ARCHITECTURE GENERATING PART
;;;                   BEGIN



;; Secondary Unit Section
;;

;;; Architecture Declarative Part
;;;
;;; According to IEEE 1076-1993 1.2.1:
;;;
;;; architecture_declarative_part :=
;;;  { block_declarative_item }
;;;
;;; block_declarative_item :=
;;;    subprogram_declaration
;;;  | subprogram_body
;;;  | type_declaration
;;;  | subtype_declaration
;;;  | constant_declaration
;;;  | signal_declaration
;;;  | shared_variable_declaration
;;;  | file_declaration
;;;  | alias_declaration
;;;  | component_declaration
;;;  | attribute_declaration
;;;  | attribute_specification
;;;  | configuration_specification
;;;  | disconnection_specification
;;;  | use_clause
;;;  | group_template_declaration
;;;  | group_declaration
;;;
;;; Implementation note:
;;;    There is currently no support for programs or procedural handling in
;;;    gEDA, thus will all declarations above involved in thus activites be
;;;    left unused. This applies to subprogram declaration, subprogram body,
;;;    shared variable declaration and file declaration.
;;;
;;;    Further, there is currently no support for type handling and therefore
;;;    will not the type declaration and subtype declaration be used.
;;;
;;;    The is currently no support for constants, aliases, configuration
;;;    and groups so the constant declaration, alias declaration, configuration
;;;    specification, group template declaration and group declaration will not
;;;    be used.
;;;
;;;    The attribute passing from a gEDA netlist into VAMS attributes must
;;;    wait, therefore will the attribute declaration and attribute
;;;    specification not be used.
;;;
;;;    The disconnection specification will not be used.
;;;
;;;    The use clause will not be used since we pass the responsibility to the
;;;    primary unit (where it is not yet supported).
;;;
;;;    The signal declation will be used to convey signals held within the
;;;    architecture.
;;;
;;;    The component declaration will be used to convey the declarations of
;;;    any external entity being used within the architecture.


;;; toplevel-subfunction for architecture generation.
;;; requires architecture and entity name and the port, where
;;; the architecture should wrote to.

(define (vams:write-secondary-unit architecture entity package-list)
  (let ((ports (vams:all-ports-in-list package-list))
        (nets (schematic-nets (toplevel-schematic))))
    (display "-- Structural VAMS generated by lepton-netlist\n")
    (format #t "-- Secondary unit

ARCHITECTURE ~A OF ~A IS
" architecture entity)
    (vams:write-architecture-declarative-part ports nets)
    (display "BEGIN\n")
    (vams:write-architecture-statement-part package-list ports)
    (format #t "END ARCHITECTURE ~A;\n" architecture)))


;;;
;;; at this time, it only calls the signal declarations

(define (vams:write-architecture-declarative-part ports nets)
  ;; Due to my taste will the component declarations go first
  ;; XXX - Broken until someday
  ;; (vams:write-component-declarations packages)
  ;; Then comes the signal declatations
  (vams:write-signal-declarations ports nets))


;;; Signal Declaration
;;;
;;; According to IEEE 1076-1993 4.3.1.2:
;;;
;;; signal_declaration :=
;;;    SIGNAL identifier_list : subtype_indication [ signal_kind ]
;;;    [ := expression ] ;
;;;
;;; signal_kind := REGISTER | BUS
;;;
;;; Implementation note:
;;;    Currently will the identifier list be reduced to a single entry.
;;;    There is no support for either register or bus type of signal kind.
;;;    Further, no default expression is being supported.
;;;    The subtype indication is hardwired to Std_Logic.


;;; the really signal-declaration-writing function
;;; it's something more complex, because it's checking all signals
;;; for consistence. it only needs the output-port as parameter.

(define (vams:write-signal-declarations ports nets)
  (for-each
   (lambda (net)
     (let*((connlist (get-all-connections net))
           (port_object (vams:net-consistence "port_object" connlist))
           (port_type (vams:net-consistence "port_type" connlist))
           )
       (if (and port_object
                port_type
                (if (equal? port_object "quantity")
                    (port_mode (vams:net-consistence 'port_mode connlist))))
           (format #t "\t~A ~A \t:  ~A;\n" port_object net port_type)
           (format #t "-- error in subnet : ~A\n" net))))
   (vams:all-necessary-nets ports nets)))


;;; Architecture Statement Part
;;;
;;; According to IEEE 1076-1993 1.2.2:
;;;
;;; architecture_statement_part :=
;;;  { concurrent_statement }
;;;
;;; According to IEEE 1076-1993 9:
;;;
;;; concurrent_statement :=
;;;    block_statement
;;;  | process_statement
;;;  | concurrent_procedure_call_statement
;;;  | concurrent_assertion_statement
;;;  | concurrent_signal_assignment_statement
;;;  | component_instantiation_statement
;;;  | generate_statement
;;;
;;; Implementation note:
;;;    We currently does not support block statements, process statements,
;;;    concurrent procedure call statements, concurrent assertion statements,
;;;    concurrent signal assignment statements or generarte statements.
;;;
;;;    Thus, we only support component instantiation statements.
;;;
;;; According to IEEE 1076-1993 9.6:
;;;
;;; component_instantiation_statement :=
;;;    instantiation_label : instantiation_unit
;;;  [ generic_map_aspect ] [ port_map_aspect ] ;
;;;
;;; instantiated_unit :=
;;;    [ COMPONENT ] component_name
;;;  | ENTITY entity_name [ ( architecture_identifier ) ]
;;;  | CONFIGURATION configuration_name
;;;
;;; Implementation note:
;;;    Since we are not supporting the generic parameters we will thus not
;;;    suppport the generic map aspect. We will support the port map aspect.
;;;
;;;    Since we do not yeat support the component form we will not yet use
;;;    the component symbol based instantiated unit.
;;;
;;;    Since we do not yeat support configurations we will not support the
;;;    we will not support the configuration symbol based form.
;;;
;;;    This leaves us with the entity form, which we will support initially
;;;    using only the entity name. The architecture identifier could possibly
;;;    be supported by attribute value.

;;; Component Declaration
;;;
;;; According to IEEE 1076-1993 4.5:
;;;
;;; component_declaration :=
;;;    COMPONENT identifier [ IS ]
;;;     [ local_generic_clause ]
;;;     [ local_port_clause ]
;;;    END COMPONENT [ component_simple_name ] ;
;;;
;;; Implementation note:
;;;    The component declaration should match the entity declaration of the
;;;    same name as the component identifier indicates. Since we do not yeat
;;;    support the generic clause in the entity declaration we shall not
;;;    support it here either. We will however support the port clause.
;;;
;;;    In the same fassion as before we will use the conditional IS symbol
;;;    as well as replicating the identifier as component simple name just to
;;;    be in line with good VAMS-93 practice and keep compilers happy.

;;; Checks if VAL is a value of a default generic attribute.  Such
;;; values should start with '?'.
(define (default-generic-value? val)
  (char=? (string-ref val 0) #\?))

;;; writes the architecture body.
;;; required all used packages, which are necessary for netlist-
;;; generation, and the output-port.

(define (vams:write-architecture-statement-part package-list ports)
  (display "-- Architecture statement part\n")
  (for-each (lambda (package)
              (let ((device (get-device package))
                    (architecture
                     (gnetlist:get-package-attribute
                      package
                      "architecture")))
                (if (not (schematic-port-device-string? device))
                    (format #t "\n  ~A : ENTITY ~A~A\n~A~A;\n"
			    package	; writes instance-label
			    (get-device package) ; writes entity
                                        ; name, which should
                                        ; instanciated
			    (if (equal? architecture "unknown")
				""
				;; write the architecture of an
				;; entity in brackets after the
				;; entity, when necessary.
				(format #f "(~A)"
					(if (default-generic-value?
					     (gnetlist:get-package-attribute package
                                                                             "architecture"))
					    (substring architecture 1)
					    architecture))
				)
			    (vams:write-generic-map package)
                            (vams:write-port-map package ports)))))
            (vams:all-necessary-packages package-list ports)))



(define-public (vams-get-package-attributes refdes)
  (let loop ((netlist (schematic-components (toplevel-schematic))))
    (if (null? netlist)
        '()
        (let ((package (car netlist)))
          (if (and (schematic-component-refdes package)
                   (string=? (schematic-component-refdes package) refdes))
              (map symbol->string (map car (schematic-component-attribs package)))
              (loop (cdr netlist)))))))


;; Given a uref, prints all generics attribute => values, without some
;; special attribs, like uref,source and architecture.
;; Don't ask why .... it's not the right place to discuss this.
;; requires the output-port and a uref

(define (vams:write-generic-map uref)
  (define non-generics '(refdes
                         source
                         architecture
                         net
                         slot))

  (define (permitted-attrib->pair attrib)
    (let ((value (gnetlist:get-package-attribute uref attrib)))
      (and (not (or (memq (string->symbol attrib) non-generics)
                    (unknown? value)))
           (cons attrib value))))

  (define (write-attribute attrib-value)
    (let ((attrib (car attrib-value))
          (value (cdr attrib-value)))
      (format #f "\t\t\t~A => ~A" attrib value)))

  ;; Returns all not default-setted generics, i.e. all attribs,
  ;; which values not started with '?'.
  (define (filter-generics uref)
    (filter-map
     (lambda (x)
       (and (not (default-generic-value?
                  (gnetlist:get-package-attribute uref x)))
	    x))
     (vams-get-package-attributes uref)))

  (let ((ls (filter-map permitted-attrib->pair
                        (filter-generics uref))))
    (if (null? ls)
	""
	(format #f "\tGENERIC MAP (\n~A)\n"
		(string-join (map write-attribute ls) ", \n")))))



;;; Port map aspect
;;;
;;; According to IEEE 1076-1993 5.6.1.2:
;;;
;;; port_map_aspect := PORT MAP ( port_association_list )
;;;
;;; According to IEEE 1076-1993 4.3.2.2:
;;;
;;; association_list :=
;;;    association_element { , association_element }

;;; writes the port map of the component.
;;; required output-port and uref.

(define (vams:write-port-map uref ports)
  (let ((pin-list (get-pins-nets uref)))
    (if (null? pin-list)
	""
	(format #f "\tPORT MAP (\t~A)"
                (string-join (map (cut vams:write-association-element <> ports)
				  pin-list)
			     ",\n\t\t\t")))))


;;; Association element
;;;
;;; According to IEEE 1076-1993 4.3.2.2:
;;;
;;; association_element :=
;;;  [ formal_part => ] actual_part
;;;
;;; formal_part :=
;;;    formal_designator
;;;  | function_name ( formal_designator )
;;;  | type_mark ( formal_designator )
;;;
;;; formal_designator :=
;;;    generic_name
;;;  | port_name
;;;  | parameter_name
;;;
;;; actual_part :=
;;;    actual_designator
;;;  | function_name ( actual_designator )
;;;  | type_mark ( actual_designator )
;;;
;;; actual_designator :=
;;;    expression
;;;  | signal_name
;;;  | variable_name
;;;  | file_name
;;;  | OPEN
;;;
;;; Implementation note:
;;;    In the association element one may have a formal part or relly on
;;;    positional association. The later is doomed out as bad VAMS practice
;;;    and thus will the formal part allways be present.
;;;
;;;    The formal part will not support either the function name or type mark
;;;    based forms, thus only the formal designator form is supported.
;;;
;;;    Of the formal designator forms will generic name and port name be used
;;;    as appropriate (this currently means that only port name will be used).
;;;
;;;    The actual part will not support either the function name or type mark
;;;    based forms, thus only the actual designator form is supported.


;;; the purpose of this function is very easy: write OPEN if pin
;;; unconnected and normal output if it connected.

(define (vams:write-association-element pin ports)
  (format #f "~A => ~A"
          (car pin)
          (if (string-prefix-ci? "unconnected_pin" (cdr pin))
              "OPEN"
              (vams:port-test pin ports))))




;;;           ARCHITECTURE GENERATING PART
;;;                       END

;;; ===================================================================================

;;;
;;;           REALLY IMPORTANT HELP FUNCTIONS



;; checks all pins of a net for consistence, under different points
;; of view (pin-attributes).
;; requires: a pin-attribute and the subnet

(define (vams:net-consistence attribute connlist)
  (if (equal? connlist '())
      #f
      (if (= (length connlist) 1)
          (if (equal? attribute 'port_mode)
              (if (equal? (gnetlist:get-attribute-by-pinnumber (car (car connlist))
                                                               (cdr (car connlist))
                                                               attribute)
                          'out)
                  #t
                  #f)
              (append (gnetlist:get-attribute-by-pinnumber (car (car connlist))
                                                           (cdr (car connlist))
                                                           attribute)))
          (if (equal? attribute 'port_mode)
              (if (equal? (gnetlist:get-attribute-by-pinnumber (car (car connlist))
                                                               (cdr (car connlist))
                                                               attribute)
                          'out)
                  #t
                  (vams:net-consistence attribute (cdr connlist)))
              (if (equal? (gnetlist:get-attribute-by-pinnumber (car (car connlist))
                                                               (cdr (car connlist))
                                                               attribute)
                          (vams:net-consistence attribute (cdr connlist)))
                  (append (gnetlist:get-attribute-by-pinnumber (car (car connlist))
                                                               (cdr (car connlist))
                                                               attribute))
                  #f)))))



;; returns a string, where are all whitespaces replaced to underlines
;; requires: a string only

(define (vams:change-all-whitespaces-to-underlines str)
  (if (string-index str #\ )
      (if (= (string-index str #\ ) (- (string-length str) 1))
          (vams:change-all-whitespaces-to-underlines
           (substring str 0 (- (string-length str) 1)))
          (begin
            (string-set! str (string-index str #\ ) #\_ )
            (vams:change-all-whitespaces-to-underlines str)))
      (append str)))


;;; Returns all net names associated with all pins of packages in
;;; REFDES-LIST.
(define (vams:all-packages-nets refdes-list)
  (define (pin-net-names package)
    (map (cut pin-netname package <>)
         (get-pins package)))

  (map pin-net-names refdes-list))


;; returns all ports from a list of urefs.
;; important for hierachical netlists. in our definition ports are
;; special components, which device-attributes a setted to "PORT".
;; The port-attributes are saved on toplevel of this special component.
;; requires: list of urefs

(define (vams:all-ports-in-list refdes-list)
  (define (is-port? refdes)
    (and (equal? "PORT" (get-device refdes))
          refdes))
  (filter-map is-port? refdes-list))



;; returns all nets in the schematic, which not
;; directly connected to a port.

(define (vams:all-necessary-nets ports nets)
  (vams:only-different-nets nets
                            (vams:all-packages-nets ports)))



;; returns all elements from ls, that are not in without-ls.
;; a simple list function.
(define (vams:only-different-nets ls without-ls)
  (lset-difference equal? ls without-ls))


;; sort all port-components out

(define (vams:all-necessary-packages package-list ports)
  (vams:only-different-nets package-list ports))



;; if pin connetected to a port (special component), then return port.
;; else return the net, which the pin is connetcted to.
;; requires: a pin only

(define (vams:port-test pin-net-pair ports)
  (let ((netname (cdr pin-net-pair)))
    (if (member netname (vams:all-packages-nets ports))
        (vams:which-port pin-net-pair ports)
        netname)))



;; returns the port, when is in port-list, which the pin is connected to
;; requires: a pin and a port-list

(define (vams:which-port pin ports)
  (define (first-pin port)
    (car (get-pins port)))

  (define (first-pin-net port)
    (pin-netname port (first-pin port)))

  (if (null? ports)
      '()
      (let ((port (car ports)))
        (if (equal? (cdr pin) (first-pin-net port))
            port
            (vams:which-port pin (cdr ports))))))



;; generate generic list for generic clause
;;((generic value) (generic value) .. ())

(define (vams:generate-generic-list ls)
  (if (null? ls)
      '()
      (append
       (if (not (or (string-prefix? "refdes=" (car ls))
                    (string-prefix? "source=" (car ls))
                    (string-prefix? "architecture=" (car ls))))
           (list
            (if (string-index (car ls) #\=)
                (list
                 (substring (car ls) 0 (string-rindex (car ls) #\= 0))
                 (substring (car ls) (+ (string-rindex (car ls) #\= 0)
                                        (if (equal? (string-ref
                                                     (car ls)
                                                     (1+ (string-rindex (car ls) #\= 0)))
                                                    #\?)
                                            2 1))
                            (string-length (car ls))))
                (car ls)))
           '())
       (vams:generate-generic-list (cdr ls)))))



;;; generates a port list of the current schematic, or returns
;;; a empty list, if no port reachable.

(define (vams:generate-port-list uref)
  (let ((port-list  (list '())))
    (if (null? uref)
        '()
        (begin
          (for-each (lambda (pin)
                      (append! port-list
                               (list (list pin
                                           (gnetlist:get-attribute-by-pinnumber uref pin "port_object")
                                           (gnetlist:get-attribute-by-pinnumber uref pin "port_type")
                                           (gnetlist:get-attribute-by-pinnumber uref pin "port_mode")))))
                    (get-pins uref))
          (append (cdr port-list))))))



;;; gets the uref value from the top-attribs-list, which is assigned from gschem.
;;; only important for automatic-gnetlist-calls from gschem !!!

(define (vams:get-uref liste)
  (if (null? liste)
      '()
      (if (string-prefix? "refdes=" (car liste))
          (append (substring (car liste) 7
                             (string-length (car liste))))
          (vams:get-uref (cdr liste)))))


;;; set generate-mode to default (1), when not defined before.
(define generate-mode (if (defined? 'generate-mode) generate-mode '1))


;;; set to-attribs list empty, when not needed.
(define top-attribs (if (defined? 'top-attribs) top-attribs '()))

(message "loaded gnet-vams.scm\n")
