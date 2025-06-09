;; Lepton EDA library - Scheme API
;; Copyright (C) 2012 Peter Brett <peter@peter-b.co.uk>
;; Copyright (C) 2016 gEDA Contributors
;; Copyright (C) 2017-2022 Lepton EDA Contributors
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
;;

;; This file contains deprecated configuration functions for RC files.
;; The use gEDA RC files for configuration is being replaced by the
;; use of key-value files that are parsed rather than executed. This
;; will allow configuration to be safely written back to disk, and
;; will have security benefits.  The functions defined in this file
;; are intended for use in legacy RC files during the transition
;; process.

;; ===================================================================
;; Utility functions and macros
;; ===================================================================


(define-module (geda deprecated-config)
  #:use-module (lepton config)
  #:use-module (lepton legacy-config))


;; Returns an RC function closure to replace the legacy configuration
;; function OLD-ID.  The returned closure takes an arbitrary number of
;; arguments, and does nothing other than print a deprecation message
;; the first time it is called.
(define (rc-dead-config old-id)

  (define (deprecation-warning)
    (display (warning-option-obsolete old-id) (current-error-port)))

  (let ((warned? #f))
    (lambda args
      (or warned? (begin (deprecation-warning) (set! warned? #t))))))

;; Convenience macro for using rc-dead-config.
;;
;;   define-rc-dead-config OLD-ID
;;
;; Creates a dead rc configuration function called OLD-ID.
(define-syntax define-rc-dead-config
  (syntax-rules ()
    ((_ old-id)
     (define-public old-id (rc-dead-config (quote old-id))))))

;; Returns an RC function closure to replace the legacy configuration
;; function OLD-ID. The returned closure takes an arbitrary number of
;; arguments, and sets the configuration parameter determined by GROUP
;; and KEY to the result of passing its arguments to
;; VALUE-TRANSFORMER.  The first time the closure is called, it prints
;; a deprecation message.
(define (rc-deprecated-config old-id group key value-transformer)

  (define (deprecation-warning)
    (display (warning-option-deprecated old-id group key)
             (current-error-port)))

  (let ((warned? #f))
    (lambda args
      (or warned?
          (begin (deprecation-warning) (set! warned? #t)))
      (set-config!
        (path-config-context (getcwd))
        group
        key
        (apply value-transformer args)
      )
    )
  ) ; let
)

;; Convenience macro for using rc-deprecated-config.
;;
;;   define-rc-deprecated-config OLD-ID GROUP KEY VALUE-TRANSFORMER
;;
;; Creates a deprecated rc configuration function called OLD-ID that
;; uses VALUE-TRANSFORMER to set the configuration parameter by GROUP
;; and KEY.
(define-syntax define-rc-deprecated-config
  (syntax-rules ()
    ((_ old-id group key value-transformer)
     (define-public old-id
       (rc-deprecated-config (quote old-id) group key value-transformer)))))

;; Identity value transformer for define-rc-deprecated-config
(define (rc-deprecated-string-transformer str) str)

(define (rc-deprecated-int-transformer num) num)

; ( list "attr1" "attr2" "attr3" ) => "attr1;attr2;attr3":
;
( define ( rc-deprecated-strlist-transformer strlist )
  ; return:
  ( apply
    string-append
    ( map
      ( lambda( str )
        ( format #f "~a;" str )
      )
      strlist
    )
  )
)

;; Transformer for "enabled"/"disabled" to boolean
(define (rc-deprecated-string-boolean-transformer str)
  (string=? "enabled" str))

;; ===================================================================
;; Deprecated liblepton configuration functions
;; ===================================================================

(define-rc-dead-config postscript-prolog)
(define-rc-dead-config world-size)
(define-rc-dead-config bitmap-directory)
(define-rc-deprecated-config
 attribute-promotion "schematic.attrib" "promote"
 rc-deprecated-string-boolean-transformer)
(define-rc-deprecated-config
 promote-invisible "schematic.attrib" "promote-invisible"
 rc-deprecated-string-boolean-transformer)
(define-rc-deprecated-config
 keep-invisible "schematic.attrib" "keep-invisible"
 rc-deprecated-string-boolean-transformer)
(define-rc-deprecated-config
 make-backup-files "schematic.backup" "create-files"
 rc-deprecated-string-boolean-transformer)
(define-rc-deprecated-config
 always-promote-attributes "schematic.attrib" "always-promote"
 rc-deprecated-strlist-transformer)

;; ===================================================================
;; Deprecated lepton-schematic configuration functions
;; ===================================================================

(define-rc-dead-config output-capstyle)
(define-rc-dead-config output-color)
(define-rc-dead-config output-orientation)
(define-rc-dead-config output-type)
(define-rc-dead-config paper-size)
(define-rc-dead-config paper-sizes)
(define-rc-dead-config print-command)
(define-rc-dead-config setpagedevice-orientation)
(define-rc-dead-config setpagedevice-pagesize)

(define-rc-deprecated-config
 print-paper "schematic.printing" "paper"
 rc-deprecated-string-transformer)

(define-rc-deprecated-config
 print-orientation "schematic.printing" "layout"
 rc-deprecated-string-transformer)

(define-rc-deprecated-config
 print-color "schematic.printing" "monochrome"
 (lambda (x) (not (rc-deprecated-string-boolean-transformer x))))

(define-rc-dead-config net-style)
(define-rc-dead-config bus-style)
(define-rc-dead-config pin-style)
(define-rc-dead-config line-style)
(define-rc-dead-config net-endpoint-mode)
(define-rc-dead-config net-midpoint-mode)
(define-rc-dead-config object-clipping)
(define-rc-dead-config text-origin-marker)
(define-rc-dead-config text-display-zoomfactor)
(define-rc-dead-config text-feedback)

(define-rc-deprecated-config
 untitled-name "schematic" "default-filename"
 rc-deprecated-string-transformer)

(define-rc-dead-config scrollbar-update)

(define-rc-deprecated-config
 sort-component-library "schematic.library" "sort"
 rc-deprecated-string-boolean-transformer)

(define-rc-deprecated-config
 component-dialog-attributes "schematic.library" "component-attributes"
 (lambda (x) x))

(define-rc-dead-config add-attribute-offset)
(define-rc-dead-config logging-destination)
(define-rc-dead-config log-window-type)
(define-rc-dead-config raise-dialog-boxes-on-expose)
(define-rc-dead-config image-size)
(define-rc-dead-config image-color)
(define-rc-dead-config window-size)

;; ===================================================================
;; Deprecated lepton-netlist configuration functions
;; ===================================================================
(define-rc-dead-config gnetlist-version)

(define-rc-deprecated-config
  unnamed-netname "netlist" "default-net-name"
  rc-deprecated-string-transformer)
(define-rc-deprecated-config
  unnamed-busname "netlist" "default-bus-name"
  rc-deprecated-string-transformer)
(define-rc-deprecated-config
  net-naming-priority "netlist" "net-naming-priority"
  (lambda (x) (if (string=? x "netname") "netname-attribute" "net-attribute")))
(define-rc-deprecated-config
  hierarchy-traversal "netlist.hierarchy" "traverse-hierarchy"
  rc-deprecated-string-boolean-transformer)
(define-rc-deprecated-config
  hierarchy-uref-mangle "netlist.hierarchy" "mangle-refdes-attribute"
  rc-deprecated-string-boolean-transformer)
(define-rc-deprecated-config
  hierarchy-uref-order "netlist.hierarchy" "refdes-attribute-order"
  (lambda (x) (string=? "prepend" x)))
(define-rc-deprecated-config
  hierarchy-uref-separator "netlist.hierarchy" "refdes-attribute-separator"
  rc-deprecated-string-transformer)
(define-rc-deprecated-config
  hierarchy-netname-mangle "netlist.hierarchy" "mangle-netname-attribute"
  rc-deprecated-string-boolean-transformer)
(define-rc-deprecated-config
  hierarchy-netname-order "netlist.hierarchy" "netname-attribute-order"
  (lambda (x) (string=? "prepend" x)))
(define-rc-deprecated-config
  hierarchy-netname-separator "netlist.hierarchy" "netname-attribute-separator"
  rc-deprecated-string-transformer)
(define-rc-deprecated-config
  hierarchy-netattrib-mangle "netlist.hierarchy" "mangle-net-attribute"
  rc-deprecated-string-boolean-transformer)
(define-rc-deprecated-config
  hierarchy-netattrib-order "netlist.hierarchy" "net-attribute-order"
  (lambda (x) (string=? "prepend" x)))
(define-rc-deprecated-config
  hierarchy-netattrib-separator "netlist.hierarchy" "net-attribute-separator"
  rc-deprecated-string-transformer)

(define-rc-deprecated-config
 draw-grips "schematic.gui" "draw-grips"
 rc-deprecated-string-boolean-transformer)
(define-rc-deprecated-config
 toolbars "schematic.gui" "toolbars"
 rc-deprecated-string-boolean-transformer)
(define-rc-deprecated-config
 scrollbars "schematic.gui" "scrollbars"
 rc-deprecated-string-boolean-transformer)
(define-rc-deprecated-config
 handleboxes "schematic.gui" "handleboxes"
 rc-deprecated-string-boolean-transformer)
(define-rc-deprecated-config
 zoom-with-pan "schematic.gui" "zoom-with-pan"
 rc-deprecated-string-boolean-transformer)
(define-rc-deprecated-config
 fast-mousepan "schematic.gui" "fast-mousepan"
 rc-deprecated-string-boolean-transformer)
(define-rc-deprecated-config
 continue-component-place "schematic.gui" "continue-component-place"
 rc-deprecated-string-boolean-transformer)
(define-rc-deprecated-config
 file-preview "schematic.gui" "file-preview"
 rc-deprecated-string-boolean-transformer)
(define-rc-deprecated-config
 enforce-hierarchy "schematic.gui" "enforce-hierarchy"
 rc-deprecated-string-boolean-transformer)
(define-rc-deprecated-config
 third-button-cancel "schematic.gui" "third-button-cancel"
 rc-deprecated-string-boolean-transformer)
(define-rc-deprecated-config
 warp-cursor "schematic.gui" "warp-cursor"
 rc-deprecated-string-boolean-transformer)
(define-rc-deprecated-config
 force-boundingbox "schematic.gui" "force-boundingbox"
 rc-deprecated-string-boolean-transformer)
(define-rc-deprecated-config
 net-direction-mode "schematic.gui" "net-direction-mode"
 rc-deprecated-string-boolean-transformer)
(define-rc-deprecated-config
 embed-components "schematic.gui" "embed-components"
 rc-deprecated-string-boolean-transformer)
(define-rc-deprecated-config
 netconn-rubberband "schematic.gui" "netconn-rubberband"
 rc-deprecated-string-boolean-transformer)
(define-rc-deprecated-config
 magnetic-net-mode "schematic.gui" "magnetic-net-mode"
 rc-deprecated-string-boolean-transformer)
(define-rc-deprecated-config
 zoom-gain "schematic.gui" "zoom-gain"
 rc-deprecated-int-transformer)
(define-rc-deprecated-config
 mousepan-gain "schematic.gui" "mousepan-gain"
 rc-deprecated-int-transformer)
(define-rc-deprecated-config
 keyboardpan-gain "schematic.gui" "keyboardpan-gain"
 rc-deprecated-int-transformer)
(define-rc-deprecated-config
 select-slack-pixels "schematic.gui" "select-slack-pixels"
 rc-deprecated-int-transformer)
(define-rc-deprecated-config
 text-size "schematic.gui" "text-size"
 rc-deprecated-int-transformer)
(define-rc-deprecated-config
 snap-size "schematic.gui" "snap-size"
 rc-deprecated-int-transformer)
(define-rc-deprecated-config
 scrollpan-steps "schematic.gui" "scrollpan-steps"
 rc-deprecated-int-transformer)
(define-rc-deprecated-config
 dots-grid-dot-size "schematic.gui" "dots-grid-dot-size"
 rc-deprecated-int-transformer)
(define-rc-deprecated-config
 dots-grid-fixed-threshold "schematic.gui" "dots-grid-fixed-threshold"
 rc-deprecated-int-transformer)
(define-rc-deprecated-config
 mesh-grid-display-threshold "schematic.gui" "mesh-grid-display-threshold"
 rc-deprecated-int-transformer)
(define-rc-deprecated-config
 action-feedback-mode "schematic.gui" "action-feedback-mode"
 rc-deprecated-string-transformer)
(define-rc-deprecated-config
 text-caps-style "schematic.gui" "text-caps-style"
 rc-deprecated-string-transformer)
(define-rc-deprecated-config
 middle-button "schematic.gui" "middle-button"
 rc-deprecated-string-transformer)
(define-rc-deprecated-config
 third-button "schematic.gui" "third-button"
 rc-deprecated-string-transformer)
(define-rc-deprecated-config
 scroll-wheel "schematic.gui" "scroll-wheel"
 rc-deprecated-string-transformer)
(define-rc-deprecated-config
 grid-mode "schematic.gui" "grid-mode"
 rc-deprecated-string-transformer)
(define-rc-deprecated-config
 dots-grid-mode "schematic.gui" "dots-grid-mode"
 rc-deprecated-string-transformer)
(define-rc-deprecated-config
 net-selection-mode "schematic.gui" "net-selection-mode"
 rc-deprecated-string-transformer)
(define-rc-deprecated-config
 undo-control "schematic.undo" "undo-control"
 rc-deprecated-string-boolean-transformer)
(define-rc-deprecated-config
 undo-type "schematic.undo" "undo-type"
 rc-deprecated-string-transformer)
(define-rc-deprecated-config
 undo-levels "schematic.undo" "undo-levels"
 rc-deprecated-int-transformer)
(define-rc-deprecated-config
 undo-panzoom "schematic.undo" "undo-panzoom"
 rc-deprecated-string-boolean-transformer)
(define-rc-deprecated-config
 bus-ripper-size "schematic" "bus-ripper-size"
 rc-deprecated-int-transformer)
(define-rc-deprecated-config
 bus-ripper-type "schematic" "bus-ripper-type"
 rc-deprecated-string-transformer)
(define-rc-deprecated-config
 bus-ripper-rotation "schematic" "bus-ripper-rotation"
 rc-deprecated-string-transformer)
(define-rc-deprecated-config
 net-consolidate "schematic" "net-consolidate"
 rc-deprecated-string-boolean-transformer)
(define-rc-deprecated-config
 logging "schematic" "logging"
 rc-deprecated-string-boolean-transformer)
(define-rc-deprecated-config
 auto-save-interval "schematic" "auto-save-interval"
 rc-deprecated-int-transformer)
(define-rc-deprecated-config
 log-window "schematic" "log-window"
 rc-deprecated-string-transformer)
(define-rc-deprecated-config
 bus-ripper-symname "schematic" "bus-ripper-symname"
 rc-deprecated-string-transformer)
