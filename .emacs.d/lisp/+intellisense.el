;;; +intellisense.el --- Intelligent in-buffer code-completions  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Ricky Anderson

;; Author: Ricky Anderson <rickyson@thinky>
;; Keywords: local

;; This file is not part of GNU Emacs

;;; Commentary:

;;

;;; Code:

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

(elpaca lsp-mode
  (setopt lsp-idle-delay 0.3
		  lsp-completion-provider :none)

  (defun ra/lsp-mode-setup-completion ()
	"Taken from corfu's wiki"
	(setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless)))

  (add-hook 'lsp-completion-mode #'ra/lsp-mode-setup-completion))

(elpaca apheleia)

(elpaca tempel
  ;; (defun ra/tempel-setup-capf ()
;; 	"Setup capf to try tempel first
;; Taken from tempel's readme"
  (add-hook 'completion-at-point-functions #'tempel-expand -90 t))

(elpaca tempel-collection)

(elpaca aggressive-indent
  (global-aggressive-indent-mode 1))

(provide '+intellisense)
;;; +intellisense.el ends here
