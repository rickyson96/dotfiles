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

(defun ra/setup-extra-pair-typescript ()
  "Add extra pairs for typescript"
  (let ((pairs '((?< . ?>))))
	(setq-local electric-pair-pairs (append electric-pair-pairs pairs))
	(setq-local electric-pair-text-pairs (append electric-pair-text-pairs pairs))))

(add-to-list 'auto-mode-alist `(,(rx "." (or "j" "t") "s" eos) . typescript-ts-mode))
(dolist (actions '(apheleia-mode
				   lsp-deferred
				   indent-bars-mode
				   combobulate-mode
				   ;; ra/setup-extra-pair-typescript
				   ;;eglot-ensure
				   ))
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

(setopt lsp-clients-typescript-preferences '( :importModuleSpecifierPreference "relative")
		lsp-clients-typescript-prefer-use-project-ts-server t
		lsp-javascript-display-return-type-hints nil
		lsp-javascript-display-variable-type-hints nil
		lsp-javascript-display-parameter-name-hints "none"
		lsp-javascript-display-parameter-type-hints	nil
		lsp-javascript-display-enum-member-value-hints nil
		lsp-javascript-display-property-declaration-type-hints nil
		lsp-javascript-display-parameter-name-hints-when-argument-matches-name nil
		lsp-javascript-preferences-import-module-specifier "relative"
		lsp-typescript-preferences-import-module-specifier "relative"
		lsp-typescript-format-enable nil
		lsp-typescript-format-insert-space-after-comma-delimiter nil
		lsp-typescript-format-insert-space-after-semicolon-in-for-statements nil
		lsp-typescript-format-insert-space-before-and-after-binary-operators nil
		lsp-typescript-format-insert-space-after-keywords-in-control-flow-statements nil
		lsp-typescript-format-insert-space-after-function-keyword-for-anonymous-functions nil
		lsp-typescript-format-insert-space-after-opening-and-before-closing-nonempty-braces nil
		lsp-javascript-format-enable nil
		lsp-javascript-format-insert-space-after-comma-delimiter nil
		lsp-javascript-format-insert-space-after-semicolon-in-for-statements nil
		lsp-javascript-format-insert-space-before-and-after-binary-operators nil
		lsp-javascript-format-insert-space-after-keywords-in-control-flow-statements nil
		lsp-javascript-format-insert-space-after-function-keyword-for-anonymous-functions nil
		lsp-javascript-format-insert-space-after-opening-and-before-closing-nonempty-braces nil)

;; (elpaca tide)

(elpaca npm)
(elpaca nodejs-repl)
(elpaca nvm
  (setopt nvm-dir "/home/randerson/.local/share/nvm/"))
(elpaca (yarn :host github :repo "jmfirth/yarn.el"))
(elpaca yarn-mode)
(elpaca (bun-mode :repo "~/opensource/bun-mode"))

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

(ra/imenu-extra-setup-hook
 typescript-ts-mode-hook
 `(("jest.it" ,(rx bol (zero-or-more (any "\t ")) "it('" (group (one-or-more (not (any "'"))))) 1)
   ("jest.desc" ,(rx (seq bol (zero-or-more (any "	 ")) "describe('" (group (one-or-more (not (any "'")))))) 1)))

(with-eval-after-load 'compile
  (add-to-list 'compilation-error-regexp-alist 'yarn)
  (add-to-list 'compilation-error-regexp-alist-alist
			   `(yarn ,(rx (group (0+ (not (or ":" "(" space)))) ; File name
						   ":" (group (1+ (any "0-9"))) ; Line number
						   ":" (group (1+ (any "0-9"))) ; Column number
						   )
					  1 2 3)))
;;       at Object.<anonymous> (library/loyalty-utils/lib/sendCollectNameSMS.test.ts:87:54)

(with-eval-after-load 'smartparens-javascript
  (sp-with-modes sp--javascript-modes
	(sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET")
											("| " "SPC")))))

(provide '+lang-jsts)
;;; +lang-jsts.el ends here

