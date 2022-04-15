; -*- mode: lisp; -*-
;                                   _            ,__ __
;    ()                            (_|   |   |_//|  |  |
;    /\_|_         _  _  _     _     |   |   |   |  |  |
;   /  \|  |   |  / |/ |/ |  |/ \_   |   |   |   |  |  |
;  /(__/|_/ \_/|_/  |  |  |_/|__/     \_/ \_/    |  |  |_/
;                           /|
;                           \|
;    __
;   / ()  _        |\ o  _,
;  |     / \_/|/|  |/ | / |
;   \___/\_/  | |_/|_/|/\/|/
;                  |)    (|
;
;; Copyright © 2018, 2019 Roch D'amour (notarock)

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; This file define all frame-preferences and groups

;;; Code:
(in-package :stumpwm)

;;;; Frame Preferences

;; (define-frame-preference "Default"
;;     ;; frame raise lock (lock AND raise == jumpto)
;;     (0 t   t :class "Firefox"))

;; (define-frame-preference "MAIL"
;;     ;; frame raise lock (lock AND raise == jumpto)
;;     (0 t   t :class "Thunderbird"))

;; (define-frame-preference "EMACS"
;;     ;; frame raise lock (lock AND raise == jumpto)
;;     (0 t   t :class "Emacs"))

;;;; Define groups

;;      DEFAUT              ;; 1
(gnewbg "ONE")            ;; 2
(gnewbg "TWO")             ;; 3

;; TODO: Move all keybindings in another file
(define-key *top-map* (kbd "s-1") "gselect 1")
(define-key *top-map* (kbd "s-2") "gselect 2")

(define-key *top-map* (kbd "s-m") "gmove")
(define-key *top-map* (kbd "s--") "gprev")
(define-key *top-map* (kbd "s-=") "gnext")

;; Show programs in every workspaces
(define-key *top-map* (kbd "s-0") "vgroups")


;; End of file
