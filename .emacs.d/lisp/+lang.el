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
(elpaca (hjson :host github :repo "hjson/hjson-emacs" :main "hjson-mode.el"))

(ra/treesitter-setup yaml "https://github.com/ikatyang/tree-sitter-yaml")
(elpaca yaml-pro
  (autoload #'yaml-pro-ts-mode "yaml-pro" "" t)
  (add-hook 'yaml-ts-mode-hook #'yaml-pro-ts-mode)
  (add-to-list 'auto-mode-alist `(,(rx (or ".yml" ".yaml") eos) . yaml-ts-mode)))

(elpaca dockerfile-mode)

(elpaca nix-mode)

(elpaca csv-mode
  (add-hook 'csv-mode-hook #'csv-align-mode)
  (setopt csv-align-style 'auto))

;; Emacs Lisp
(elpaca macrostep)
(elpaca suggest)

(elpaca (pdf-tools :host github :repo "vedang/pdf-tools"
				   :remotes ("roll" :repo "dalanicolai/pdf-tools" :branch "pdf-roll"))
  (pdf-loader-install)
  (setopt pdf-view-display-size 'fit-page)
  (with-eval-after-load 'pdf-view
	(ra/keymap-set pdf-view-mode-map
	  "n" 'pdf-view-scroll-up-or-next-page
	  "p" 'pdf-view-scroll-down-or-previous-page
	  "v" 'pdf-view-next-page
	  "V" 'pdf-view-previous-page)))

(elpaca (image-roll :host github :repo "dalanicolai/image-roll.el"))

;; sudo file
(elpaca etc-sudoers-mode)

(elpaca	fish-mode)
(elpaca fish-completion)

;; literate calc
(elpaca literate-calc-mode)

(elpaca systemd)

(elpaca dotenv-mode)

(elpaca mermaid-mode)

(elpaca git-modes
  (add-to-list 'auto-mode-alist `(,(rx (or ".ignore" ".rgignore") eos) . gitignore-mode)))

(provide '+lang)
;;; +lang.el ends here
