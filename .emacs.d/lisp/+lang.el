;;; +lang.el --- Teach Emacs Various Languages       -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Ricky Anderson

;; Author: Ricky Anderson <rickyson@thinky>
;; Keywords: local

;; This file is part of GNU Emacs

;;; Commentary:

;;

;;; Code:

(require 'treesit)
(defmacro ra/treesitter-setup (language url &rest extra)
  "Setup Emacs's treesitter for LANGUAGE"
  (macroexp-progn
   `((add-to-list 'treesit-language-source-alist '(,language ,url ,@extra))
	 (unless (treesit-language-available-p ',language)
	   (treesit-install-language-grammar ',language)))))

(elpaca go-mode)
(elpaca gotest)
(elpaca go-tag)
(elpaca go-stacktracer)
(elpaca go-fill-struct)
(elpaca go-scratch)
(elpaca go-playground)
(ra/treesitter-setup go "https://github.com/tree-sitter/tree-sitter-go")

(require '+lang-jsts)

(require '+lang-org)

(require '+lang-web)

(setopt markdown-fontify-code-blocks-natively t
		markdown-wiki-link-fontify-missing t)

(elpaca jsonian)

(elpaca nix-mode)

(elpaca csv-mode
  (add-hook 'csv-mode-hook #'csv-align-mode)
  (setopt csv-align-style 'auto))

;; Emacs Lisp
(elpaca macrostep)
(elpaca suggest)

(elpaca pdf-tools)

;; sudo file
(elpaca etc-sudoers-mode)

(elpaca fish-mode)
(elpaca fish-completion)

;; literate calc
(elpaca literate-calc-mode)

(elpaca systemd)

(elpaca dotenv-mode)

(provide '+lang)
;;; +lang.el ends here
