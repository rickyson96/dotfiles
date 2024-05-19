;;; +intellisense.el --- Intelligent in-buffer code-completions  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Ricky Anderson

;; Author: Ricky Anderson <rickyson@thinky>
;; Keywords: local

;; This file is not part of GNU Emacs

;;; Commentary:

;;

;;; Code:

(setopt completion-ignore-case t
		read-file-name-completion-ignore-case t
		read-buffer-completion-ignore-case t)

(elpaca corfu
  (global-corfu-mode 1)
  (corfu-popupinfo-mode)
  (setopt tab-always-indent 'complete
		  corfu-cycle t
		  corfu-popupinfo-delay (cons nil 0.3)))

(elpaca cape)

(elpaca nerd-icons-corfu
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(elpaca corfu-candidate-overlay
  (corfu-candidate-overlay-mode 1))

(elpaca corfu-terminal
  (unless (display-graphic-p)
	(corfu-terminal-mode +1)))

(elpaca lsp-mode
  (setopt lsp-idle-delay 0.3
		  lsp-completion-provider :none
		  lsp-keymap-prefix "C-c l"
		  lsp-diagnostics-provider t
		  lsp-inlay-hint-enable t
		  lsp-enable-suggest-server-download nil
		  lsp-eldoc-render-all t)

  (with-eval-after-load 'lsp-mode
	(ra/keymap-set lsp-mode-map
	  "<remap> <display-local-help>" #'eldoc))

  (with-eval-after-load 'lsp-mode
	  (set-face-attribute 'lsp-face-highlight-read nil :inherit 'bold :underline t))

  (defun ra/lsp-mode-setup-completion ()
	"Taken from corfu's wiki"
	(setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless)))

  (add-hook 'lsp-completion-mode #'ra/lsp-mode-setup-completion)
  (add-hook 'lsp-mode-hook #'yas-minor-mode)

  ;; LSP Booster
  (defun lsp-booster--advice-json-parse (old-fn &rest args)
	"Try to parse bytecode instead of json."
	(or
	 (when (equal (following-char) ?#)
       (let ((bytecode (read (current-buffer))))
		 (when (byte-code-function-p bytecode)
           (funcall bytecode))))
	 (apply old-fn args)))
  (advice-add (if (progn (require 'json)
						 (fboundp 'json-parse-buffer))
                  'json-parse-buffer
				'json-read)
              :around
              #'lsp-booster--advice-json-parse)

  (defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
	"Prepend emacs-lsp-booster command to lsp CMD."
	(let ((orig-result (funcall old-fn cmd test?)))
      (if (and (not test?)                             ;; for check lsp-server-present?
               (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
               lsp-use-plists
               (not (functionp 'json-rpc-connection))  ;; native json-rpc
               (executable-find "emacs-lsp-booster"))
          (progn
			(message "Using emacs-lsp-booster for %s!" orig-result)
			(cons "emacs-lsp-booster" orig-result))
		orig-result)))
  (advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command))

(with-eval-after-load 'eglot
  (set-face-attribute 'eglot-highlight-symbol-face nil :underline t))

(elpaca (eglot-booster :host github :repo "jdtsmith/eglot-booster")
  (with-eval-after-load 'eglot
	(eglot-booster-mode 1)))

(elpaca (lsp-bridge :host github :repo "manateelazycat/lsp-bridge"
					:files (:defaults "*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources")
					:build (:not elpaca--byte-compile))
  (setopt lsp-bridge-enable-log nil
		  lsp-bridge-enable-inlay-hint t))

(elpaca yasnippet)

(elpaca apheleia
  (with-eval-after-load 'apheleia-formatters
	(add-to-list 'apheleia-formatters '(eslint . ("apheleia-npx" "eslint" "--fix" file)))
	(add-to-list 'apheleia-formatters '(eslintd . ("eslint_d" "--fix-to-stdout" "--stdin" "--stdin-filename" filepath)))
	(setf (alist-get 'typescript-ts-mode apheleia-mode-alist) 'eslintd)))

(elpaca tempel
  (defun ra/tempel-setup-capf ()
	"Setup capf to try tempel first
Taken from tempel's readme"
	(add-hook 'completion-at-point-functions #'tempel-expand -90 t)))

(elpaca tempel-collection)

(elpaca (lsp-snippet :host github :repo "svaante/lsp-snippet")
  ;; (when (featurep 'lsp-mode)
  ;; 	(lsp-snippet-tempel-lsp-mode-init))
  (when (featurep 'eglot)
	(lsp-snippet-tempel-eglot-init)))

;; TODO: need to find a way to partially enable aggressive indent
(elpaca aggressive-indent)

(elpaca flymake-flycheck
  (add-hook 'flymake-mode-hook 'flymake-flycheck-auto)

  (with-eval-after-load 'flymake-flycheck
	(dolist (checkers '(javascript-eslint))
	  (add-to-list 'flycheck-disabled-checkers checkers))))

(elpaca eldoc-box)

(elpaca (copilot :host github :repo "copilot-emacs/copilot.el")
  ;; (add-hook 'prog-mode-hook #'copilot-mode)
  (with-eval-after-load 'copilot
	(ra/keymap-set copilot-completion-map
	"C-g" #'copilot-clear-overlay
	"M-p" #'copilot-previous-completion
	"M-n" #'copilot-next-completion
	"C-e" #'copilot-accept-completion-by-line
	"M-f" #'copilot-accept-completion-by-word
	"C-<return>" #'copilot-accept-completion)))

(elpaca (openai :host github :repo "emacs-openai/openai")

  (defun ra/openai-key-auth-source (&optional base-url)
  "Retrieve the OpenAI API key from auth-source given a BASE-URL.
If BASE-URL is not specified, it defaults to `openai-base-url'."
  (if-let ((auth-info (auth-source-search :max 1
										  :host (url-host (url-generic-parse-url (or nil openai-base-url)))
					  :require '(:secret))))
      (funcall (plist-get (car auth-info) :secret))
    (error "OpenAI API key not found in auth-source")))

  (setopt openai-key #'ra/openai-key-auth-source
		  openai-user "ricky.anderson2696@gmail.com"))
(elpaca (chatgpt :host github :repo "emacs-openai/chatgpt"))
(elpaca (codegpt :host github :repo "emacs-openai/codegpt"))

(provide '+intellisense)
;;; +intellisense.el ends here
