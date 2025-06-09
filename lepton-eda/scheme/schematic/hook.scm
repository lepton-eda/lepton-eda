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
  #:export (add-objects-hook
            copy-objects-hook
            remove-objects-hook
            move-objects-hook
            mirror-objects-hook
            rotate-objects-hook
            paste-objects-hook
            attach-attribs-hook
            detach-attribs-hook
            select-objects-hook
            deselect-objects-hook
            new-page-hook
            open-page-hook
            action-property-hook
            switch-action-mode-hook
            bind-keys-hook
            complex-place-list-changed-hook))

(define add-objects-hook (make-hook 1))

(define copy-objects-hook (make-hook 1))

(define remove-objects-hook (make-hook 1))

(define move-objects-hook (make-hook 1))

(define mirror-objects-hook (make-hook 1))

(define rotate-objects-hook (make-hook 1))

(define paste-objects-hook (make-hook 1))

(define attach-attribs-hook (make-hook 1))

(define detach-attribs-hook (make-hook 1))

(define select-objects-hook (make-hook 1))

(define deselect-objects-hook (make-hook 1))

(define new-page-hook (make-hook 1))

(define open-page-hook (make-hook 1))

(define action-property-hook (make-hook 3))

(define bind-keys-hook (make-hook 3))

(define switch-action-mode-hook (make-hook 1))

(define complex-place-list-changed-hook (make-hook 1))
