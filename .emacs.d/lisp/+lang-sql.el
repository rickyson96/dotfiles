;;; +lang-sql.el --- SQL Language Configurations     -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Ricky Anderson

;; Author: Ricky Anderson <randerson@fedora>
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

;;

;;; Code:

(defun sql-comint-spanner (product options &optional buffer)
  "Create comint buffer and connect to spanner"
  (let ((params (append (unless (string= "" sql-server)
                          (list "-p" sql-server))
                        (unless (string= "" sql-user)
                          (list "-i" sql-user))
                        (unless (string= "" sql-database)
                          (list "-d" sql-database)))))
    (sql-comint product params buffer)))

(defun ra/sql-spanner-setup-history ()
  (setq-local sql-input-ring-file-name (file-truename "~/.cache/spanner/history.sql")
              sql-input-ring-separator ";\n"))
(add-hook 'sql-interactive-mode-hook #'ra/sql-spanner-setup-history)

(with-eval-after-load 'sql
  (add-to-list 'sql-product-alist `(spanner :name "Spanner"
                                            :font-lock sql-mode-ansi-font-lock-keywords
                                            :sqli-program "spanner-cli"
                                            :sqli-options nil
                                            :sqli-login (server user database)
                                            :sqli-comint-func sql-comint-spanner
                                            :prompt-regexp ,(rx bol "spanner> ")
                                            :prompt-cont-regexp ,(rx bol "      -> ")
                                            :prompt-length 9
                                            :list-all "SHOW TABLES;"
                                            :list-table "SHOW COLUMNS FROM %s;"
                                            :terminator ";")))
(defun sql-spanner (&optional buffer)
  (interactive "P")
  (sql-product-interactive 'spanner buffer))

(require '+lang-sql-connections)
(provide '+lang-sql)
;;; +lang-sql.el ends here
