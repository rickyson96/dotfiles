;;; +mappings-modal-v2.el --- Custom-made key mapping  -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author:  <rickyson@thinky>
;; Keywords: local

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; I wish to make my own set of keybindings for meow, but it feels
;; like too much hassle without significant improvement. I'll update
;; my current meow configurations with better hotkeys for now (enable
;; easier hotkeys for command I often use)

;;; Code:



(provide '+mappings-modal-v2)
;;; +mappings-modal-v2.el ends here


(meow-normal-define-key
 '("0" . meow-expand-0)
 '("9" . meow-expand-9)
 '("8" . meow-expand-8)
 '("7" . meow-expand-7)
 '("6" . meow-expand-6)
 '("5" . meow-expand-5)
 '("4" . meow-expand-4)
 '("3" . meow-expand-3)
 '("2" . meow-expand-2)
 '("1" . meow-expand-1)

 ;; right hand
 '("h" . meow-left)
 '("t" . meow-prev)
 '("n" . meow-next)
 '("s" . meow-right)
 '("H" . meow-left-expand)
 '("T" . meow-prev-expand)
 '("N" . meow-next-expand)
 '("S" . meow-right-expand)
 '("c" . meow-back-word)
 '("r" . meow-next-word)

 ;; left hand
 '("" . meow-mark-word)

 '("<escape>" . ignore))
