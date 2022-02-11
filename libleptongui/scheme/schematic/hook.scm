;; Lepton EDA Schematic Capture
;; Scheme API
;; Copyright (C) 2010-2011 Peter Brett
;; Copyright (C) 2010-2016 gEDA Contributors
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

(define-module (schematic hook)

  ;; Import C definitions
  #:use-module (schematic core hook)

  #:export (action-property-hook
            bind-keys-hook))

(define-public add-objects-hook %add-objects-hook)

(define-public copy-objects-hook %copy-objects-hook)

(define-public remove-objects-hook %remove-objects-hook)

(define-public move-objects-hook %move-objects-hook)

(define-public mirror-objects-hook %mirror-objects-hook)

(define-public rotate-objects-hook %rotate-objects-hook)

(define-public paste-objects-hook %paste-objects-hook)

(define-public attach-attribs-hook %attach-attribs-hook)

(define-public detach-attribs-hook %detach-attribs-hook)

(define-public select-objects-hook %select-objects-hook)

(define-public deselect-objects-hook %deselect-objects-hook)

(define-public new-page-hook %new-page-hook)

(define-public open-page-hook %open-page-hook)

(define action-property-hook (make-hook 3))

(define bind-keys-hook (make-hook 3))

(define-public switch-action-mode-hook %switch-action-mode-hook)
