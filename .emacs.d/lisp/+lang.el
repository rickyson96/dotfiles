;;; +lang.el --- Teach Emacs Various Languages       -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Ricky Anderson

;; Author: Ricky Anderson <rickyson@thinky>
;; Keywords: local

;; This file is part of GNU Emacs

;;; Commentary:

;;

;;; Code:

(require 'treesit)
(setopt treesit-font-lock-level 4)		; more colors
(cl-defmacro ra/treesitter-setup (language url &rest extra &key no-mode-remap &allow-other-keys)
  "Setup Emacs's treesitter for LANGUAGE"
  (macroexp-progn
   `((add-to-list 'treesit-language-source-alist '(,language ,url ,@extra))
	 (unless (treesit-language-available-p ',language)
	   (treesit-install-language-grammar ',language))
	 (unless ,no-mode-remap
	   (add-to-list 'major-mode-remap-alist '(,(intern (concat (symbol-name language) "-mode")) .
											  ,(intern (concat (symbol-name language) "-ts-mode"))))))))

(elpaca go-mode)
(elpaca gotest)
(elpaca go-tag)
(elpaca go-stacktracer)
(elpaca go-fill-struct)
(elpaca go-scratch)
(elpaca go-playground)
(ra/treesitter-setup go "https://github.com/tree-sitter/tree-sitter-go")

(ra/treesitter-setup bash "https://github.com/tree-sitter/tree-sitter-bash")

(require '+lang-jsts)

(ra/treesitter-setup python "https://github.com/tree-sitter/tree-sitter-python")
(elpaca pip-requirements)
(when (executable-find "conda")
  (elpaca conda
	(conda-env-initialize-eshell)))
(elpaca anaconda-mode)

(require '+lang-org)

(require '+lang-web)

(elpaca php-mode)

;; TODO make markdown tab to autocomplete and `markdown-cycle' only works in normal mode
(setopt markdown-fontify-code-blocks-natively t
		markdown-wiki-link-fontify-missing t
		markdown-italic-underscore t
		markdown-asymmetric-header t
		markdown-gfm-additional-languages '("sh")
		markdown-make-gfm-checkboxes-buttons t
		markdown-fontify-whole-heading-line t)

(add-hook 'markdown-mode-hook #'variable-pitch-mode)

(add-to-list 'auto-mode-alist `(,(rx "README" (opt ".md") eos) . gfm-mode))

;; create markdown that starts with markdown-view-mode and integrate with read-only-mode
;; (ra/keymap-set markdown-mode-map
;; )

(with-eval-after-load 'meow
  (add-hook 'markdown-mode-hook (lambda () (markdown-toggle-markup-hiding 1)))
  (add-hook 'meow-insert-enter-hook (lambda ()
									  (when (derived-mode-p 'markdown-mode)
										(markdown-toggle-markup-hiding -1)))))

(elpaca jsonian
  (add-hook 'jsonian-mode-hook #'lsp-deferred))

(elpaca (hjson :host github :repo "hjson/hjson-emacs" :main "hjson-mode.el"))

(elpaca jq-mode
  (setopt jq-interactive-command "yq"
		  jq-interactive-default-options "--prettyPrint --input-format json --output-format json"
		  jq-interactive-font-lock-mode #'jsonian-mode)

  (defun ra/yq-interactively ()
	"`jq-interactively' but using yq for yaml files"
	(interactive)
	(let ((jq-interactive-command "yq")
		  (jq-interactive-font-lock-mode #'yaml-ts-mode)
		  (jq-interactive-default-options "")
		  (jq-interactive-default-prompt "yq: "))
	  (call-interactively #'jq-interactively))))

(ra/treesitter-setup yaml "https://github.com/ikatyang/tree-sitter-yaml"
					 :no-mode-remap t)
(elpaca yaml-pro
  (autoload #'yaml-pro-ts-mode "yaml-pro" "" t)
  (add-hook 'yaml-mode-hook #'yaml-pro-ts-mode)
  (add-to-list 'auto-mode-alist `(,(rx (or ".yml" ".yaml") eos) . yaml-mode)))

(add-hook 'yaml-mode-hook #'indent-bars-mode)
(add-hook 'yaml-mode-hook #'lsp-deferred)

(elpaca dockerfile-mode)
(elpaca docker-compose-mode)
(setopt tramp-docker-program "podman")

(elpaca nix-mode)

(elpaca csv-mode
  (add-hook 'csv-mode-hook #'csv-align-mode)
  (setopt csv-align-style 'auto))

;; Emacs Lisp
(elpaca (elispfl :host github :repo "cireu/elispfl")
  (elispfl-mode 1)
  (elispfl-ielm-mode 1))
(elpaca lisp-extra-font-lock
  (lisp-extra-font-lock-global-mode 1))
(elpaca pair-tree
  (with-eval-after-load 'pair-tree
	(ra/keymap-set pair-tree-mode-map
	  "h" #'pair-tree-nav-up
	  "n" #'pair-tree-nav-down
	  "t" #'pair-tree-nav-right
	  "p" #'pair-tree-nav-up)))
(elpaca macrostep)
(elpaca suggest)
(elpaca elisp-autofmt)

(elpaca (pdf-tools :host github :repo "vedang/pdf-tools"
				   ;; :remotes ("roll" :repo "dalanicolai/pdf-tools" :branch "pdf-roll")
				   )
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
(add-hook 'sh-mode-hook #'lsp-deferred)

;; literate calc
(elpaca literate-calc-mode)

(elpaca systemd)

(elpaca dotenv-mode)

(elpaca mermaid-mode)

(elpaca git-modes
  (add-to-list 'auto-mode-alist `(,(rx "." (0+ nonl) "ignore" eos) . gitignore-mode)))

;; ansible
(elpaca ansible)
(elpaca ansible-doc)
(elpaca ansible-vault)
(elpaca poly-ansible)

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

(elpaca caddyfile-mode
  (add-to-list 'auto-mode-alist `(,(rx "Caddyfile" eos) . caddyfile-mode))
  (add-to-list 'auto-mode-alist `(,(rx "caddy.conf" eos) . caddyfile-mode)))

(elpaca terraform-mode
  (add-hook 'terraform-mode-hook #'lsp-deferred)
  (add-hook 'terraform-mode-hook #'apheleia-mode))
(elpaca terraform-doc)

(provide '+lang)
;;; +lang.el ends here
