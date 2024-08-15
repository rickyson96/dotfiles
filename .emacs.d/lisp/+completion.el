;;; +completion.el --- Completion in Emacs           -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Ricky Anderson

;; Author: Ricky Anderson <rickyson@thinky>
;; Keywords: local

;; This file is not part of GNU Emacs

;;; Commentary:

;; 

;;; Code:

(elpaca vertico
  (vertico-mode)

  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  (require 'vertico-directory)
  (ra/keymap-set vertico-map
	"RET" #'vertico-directory-enter
	"DEL" #'vertico-directory-delete-char
	"M-DEL" #'vertico-directory-delete-word
	"C-z" #'vertico-quick-exit)
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy))

(elpaca orderless
  (setopt completion-styles '(substring orderless partial-completion)
		  completion-category-defaults nil
		  completion-category-overrides '((file (styles basic partial-completion)))))

(elpaca marginalia
  (marginalia-mode)
  (add-to-list 'marginalia-prompt-categories '("Callable" . function)))

(elpaca nerd-icons-completion
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(elpaca consult
  (setopt register-preview-delay 0.5
		  register-preview-function #'consult-register-format
		  xref-show-xrefs-function #'consult-xref
		  xref-show-definitions-function #'consult-xref
		  consult-narrow-key "<"
		  consult-ripgrep-args "rg --hidden --null --line-buffered --color=never --max-columns=1000 --path-separator /   --smart-case --no-heading --with-filename --line-number --search-zip")

  (add-hook 'completion-list-mode #'consult-preview-at-point-mode)

  (advice-add #'register-preview :override #'consult-register-window))

;;;###autoload
(defun ra/replace-region-with-kill ()
  "Replace region content with kill-ring"
  (interactive)
  (when (use-region-p)
    (delete-active-region)
    (yank)))

;; TODO add embark elpaca integration?
(elpaca embark
  (setopt prefix-help-command #'embark-prefix-help-command)

  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))

  (with-eval-after-load 'embark
	(ra/keymap-set embark-region-map
	"R" #'reverse-region
	"r" #'ra/replace-region-with-kill)))

(elpaca embark-consult
  (add-hook 'embark-collect-mode #'consult-preview-at-point-mode))

(elpaca wgrep
  (require 'wgrep))

(provide '+completion)
;;; +completion.el ends here
