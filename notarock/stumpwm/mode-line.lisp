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

;;; Mode-line format and modules configuration

;;; Code:

(in-package :stumpwm)

(setq stumpwm:*mode-line-position* :top)

(setq *separator-right* " ^7*^b«^7 ")
(setq *separator-left* " ^7*^b»^7 ")

(setf *group-format* "[%t]")
(setf *time-modeline-string* "%m-^B%d ^3^b%R^b^n")
(setf *mode-line-background-color* (nth 10 *colors*))

(setf *no-battery-info* "")

(setf stumpwm:*screen-mode-line-format*
      (list
       "^7*^B%g^b^n"                ; Focused frame
       *separator-left*             ; ---
       "^>"                         ; middle
       *separator-right*            ; ---
       "^9*^b%C"                    ; cpu
       *separator-right*            ; ---
       "^4*^b%M"                    ; Memory
       *separator-right*            ; ---
       "^2*^b%l"                         ; Wifi
       *separator-right*            ; ---
       "%d"                         ; Date
       *separator-right*            ; ---
       "%T"                         ; free space for stumptray
       ))

(dolist (head
         (list (first (screen-heads (current-screen)))))
  (enable-mode-line (current-screen) head
                    t *screen-mode-line-format*))

(stumptray:stumptray)

;; End of file
