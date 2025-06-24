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
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)

  (set-face-attribute 'vertico-group-title nil :box nil :italic t))

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
          consult-narrow-key "<")

  (add-hook 'completion-list-mode #'consult-preview-at-point-mode)

  (advice-add #'register-preview :override #'consult-register-window))

(elpaca browser-hist
  (setq browser-hist-default-browser 'qutebrowser))

(elpaca (consult-mu :host github :repo "armindarvish/consult-mu"))

(elpaca (consult-omni :host github :repo "armindarvish/consult-omni" :files (:defaults "sources/*.el"))
  (setopt consult-omni-sources-modules-to-load
          (list 'consult-omni-apps
                'consult-omni-brave-autosuggest
                'consult-omni-brave
                'consult-omni-browser-history
                'consult-omni-buffer
                'consult-omni-calc
                ;; 'consult-omni-consult-notes
                'consult-omni-dict
                'consult-omni-elfeed
                'consult-omni-fd
                'consult-omni-gh
                'consult-omni-git-grep
                'consult-omni-gptel
                'consult-omni-line-multi
                'consult-omni-man
                'consult-omni-mu4e
                'consult-omni-notes
                'consult-omni-org-agenda
                'consult-omni-projects
                'consult-omni-ripgrep
                'consult-omni-ripgrep-all
                'consult-omni-stackoverflow
                'consult-omni-wikipedia
                'consult-omni-youtube))
  ;; (setopt consult-omni-multi-sources '(consult-omni-apps
  ;;                                      consult-omni-file
  ;;                                      consult-omni-buffer
  ;;                                      ;; "Bookmark"
  ;;                                      consult-omni-apps
  ;;                                      ;; "gptel"
  ;;                                      "Brave"
  ;;                                      "Dictionary"
  ;;                                      ;; "Google"
  ;;                                      "Wikipedia"
  ;;                                      "elfeed"
  ;;                                      ;; "mu4e"
  ;;                                      ;; "buffers text search"
  ;;                                      "Notes Search"
  ;;                                      "Org Agenda"
  ;;                                      "GitHub"
  ;;                                      "YouTube"
  ;;                                      "Invidious"))
  (setopt consult-omni-multi-sources (list "Apps"
                                           "File"
                                           "Buffer"
                                           "Notes Search"
                                           "Org Agenda")))
  ;; (require 'consult-omni-brave-autosuggest)
  (with-eval-after-load 'consult-omni
    (require 'consult-omni-sources)
    (require 'consult-omni-embark)
    (consult-omni-sources-load-modules))

;;;###autoload
(defun ra/replace-region-with-kill ()
  "Replace region content with kill-ring"
  (interactive)
  (when (use-region-p)
    (delete-active-region)
    (yank)))

;;;###autoload
(defun ra/replace-region-with-consult-yank-pop ()
  "Replace region content with kill-ring"
  (interactive)
  (when (use-region-p)
    (delete-active-region)
    (consult-yank-pop)))

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
