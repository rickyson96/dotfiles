;;; +lang-jsts.el --- Javascript and Typescript language setup  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Ricky Anderson

;; Author: Ricky Anderson <rickyson@thinky>
;; Keywords: local

;; This file is not part of GNU Emacs

;;; Commentary:

;;

;;; Code:

(ra/treesitter-setup typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
(ra/treesitter-setup tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
(ra/treesitter-setup javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")

;; (add-hook 'typescript-ts-mode-hook #'lsp-deferred)
;; (add-hook 'tsx-ts-mode-hook #'lsp-deferred)
;; (add-hook 'js-ts-mode-hook #'lsp-deferred)

(add-to-list 'auto-mode-alist `(,(rx "." (or "j" "t") "s" eos) . typescript-ts-mode))
(dolist (actions '(apheleia-mode
				   ;; lsp-deferred
				   eglot-ensure))
  (add-hook 'typescript-ts-base-mode-hook actions))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
			   '(((typescript-ts-mode :language-id "typesrcipt")) .
				 ("typescript-language-server" "--stdio"
				  :initializationOptions
				  ( :preferences
					( :importModuleSpecifierPreference "relative"
					  :includeInlayEnumMemberValueHints t
					  :includeInlayFunctionLikeReturnTypeHints t
					  :includeInlayFunctionParameterTypeHints t
					  :includeInlayParameterNameHints "literals" ; "none" | "literals" | "all"
					  :includeInlayParameterNameHintsWhenArgumentMatchesName t
					  :includeInlayPropertyDeclarationTypeHints t
					  :includeInlayVariableTypeHints t
					  :includeInlayVariableTypeHintsWhenTypeMatchesName t
					  :quotePreference "single"))))))

(with-eval-after-load 'lsp-mode
  (setopt lsp-clients-typescript-preferences '( :importModuleSpecifierPreferences "relative"
												:includeInlayParameterNameHints "literals"
												:includeInlayEnumMemberValueHints t
												:includeInlayFunctionLikeReturnTypeHints t
												:includeInlayFunctionParameterTypeHints t
												:includeInlayParameterNameHintsWhenArgumentMatchesName t
												:includeInlayPropertyDeclarationTypeHints t
												:includeInlayVariableTypeHints t
												:includeInlayVariableTypeHintsWhenTypeMatchesName t
												:quotePreference "single")))

(elpaca npm)
(elpaca nodejs-repl)
(elpaca nvm
  (setopt nvm-dir "/home/randerson/.local/share/nvm/"))
(elpaca (yarn :host github :repo "jmfirth/yarn.el"))
(elpaca yarn-mode)

(elpaca jest-test-mode)

(elpaca flymake-eslint
  (add-hook 'eglot-managed-mode-hook
			(lambda ()
			  (let ((eslint (cond ((executable-find "eslint_d") "eslint_d")
								  ((executable-find "eslint") "eslint"))))
				(when (and eslint
						   (or (locate-dominating-file buffer-file-name ".eslintrc.json")
							   (locate-dominating-file buffer-file-name ".eslintrc.js"))
						   (derived-mode-p 'typescript-ts-base-mode))
				  (setq flymake-eslint-executable-name eslint)
				  (flymake-eslint-enable)))

			  (flymake-flycheck-auto)))

  (defun ra/flymake-eslint_d-restart ()
	"Restart eslint_d and then run `flymake-start'"
	(interactive)
	(shell-command "eslint_d restart")
	(flymake-start)))

(elpaca flycheck-jest
  (setq flycheck-jest '(typescript-ts-mode web-mode js-mode typescript-mode rjsx-mode))
  (add-hook 'typescript-ts-base-mode-hook 'flycheck-jest-setup))

(elpaca jsdoc)

(elpaca ts-comint)

(provide '+lang-jsts)
;;; +lang-jsts.el ends here
