;;; +eshell-alias.el --- Eshell Aliases              -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Ricky Anderson

;; Author: Ricky Anderson <randerson@thinky>
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

;; Eshell aliases as functions

;;; Code:

(defun eshell/dired (&optional dir)
  "Eshell `dired' alias to automatically defaults to current directory"
  (dired (or dir default-directory)))
(defalias 'eshell/d 'eshell/dired)

(provide '+eshell-alias)
;;; +eshell-alias.el ends here
