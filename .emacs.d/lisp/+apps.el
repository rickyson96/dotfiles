;;; +apps.el --- Emacs Apps to Live Inside Emacs     -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Ricky Anderson

;; Author: Ricky Anderson <rickyson@thinky>
;; Keywords: local

;; This file is not part of GNU Emacs

;;; Commentary:

;;

;;; Code:

(elpaca pass
  (defvar-keymap embark-password-store-actions
    :doc "Keymap for actions for password-store."
    "c" #'password-store-copy
    "f" #'password-store-copy-field
    "i" #'password-store-insert
    "I" #'password-store-generate
    "r" #'password-store-rename
    "e" #'password-store-edit
    "k" #'password-store-remove
    "U" #'password-store-url)

  (with-eval-after-load 'embark
    (add-to-list 'embark-keymap-alist '(password . embark-password-store-actions)))

  ;; Either add a prompt classifier or overwrite password-store--completing-read
  (with-eval-after-load 'marginalia
    (add-to-list 'marginalia-prompt-categories '("Password entry" . password-store))))

(defun ra/scan-otp-uri (&optional direct-insert)
  "Scan otp-uri using `hyprshot' and put it in `kill-ring'"
  (interactive "P")
  (let* ((cmd (if direct-insert #'insert #'kill-new))
         (otp-raw (shell-command-to-string "hyprshot -rm region | zbarimg PNG:-"))
         (otp (car (split-string otp-raw)))
         (otp-split (split-string otp ":")))
    (funcall cmd (string-join (cdr otp-split) ":"))))

(auth-source-pass-enable)

(elpaca eat
  (add-hook 'eat-exit-hook #'quit-window)
  (add-hook 'eshell-load-hook #'eat-eshell-mode)

  (defun ra/connect-to-paystone-vm ()
    (interactive)
    (let ((display-buffer-overriding-action '(display-buffer-full-frame . ())))
      (tabspaces-switch-or-create-workspace "paystone-vm")
      (eat-other-window)))

  (with-eval-after-load 'eat
    (ra/keymap-set eat-char-mode-map
      "C-M-g" 'eat-semi-char-mode
      "M-RET" 'eat-self-input
      "C-/" 'eat-self-input)
    (ra/keymap-set eat-semi-char-mode-map
      "C-x 3" (lambda ()
                (interactive)
                (let ((window (split-window nil nil 'right)))
                  (select-window window)
                  (with-selected-window window
                    (eat nil '(4)))))))

  (with-eval-after-load 'meow
    (add-hook 'meow-normal-mode-hook (lambda ()
                                       ;; (hide-mode-line-mode -1)
                                       (when (eq major-mode 'eat-mode)
                                         (eat-emacs-mode))))
    (add-hook 'meow-insert-mode-hook (lambda () (when (eq major-mode 'eat-mode)
                                                  ;; (hide-mode-line-mode -1)
                                                  (eat-semi-char-mode))))
    ;; (add-hook 'eat--char-mode (lambda () (meow-motion-mode 1)))
    ;; (define-advice eat-char-mode (:after (&rest _) hide-mode-line)
    ;;   (meow-motion-mode 1)
    ;;   ;; (hide-mode-line-mode 1)
    ;;   )
    ))

(elpaca eshell-prompt-extras
  (autoload 'epe-theme-multiline-with-status "eshell-prompt-extras")
  (with-eval-after-load 'esh-opt
    (setopt eshell-highlight-prompt nil
            eshell-prompt-function 'epe-theme-multiline-with-status)))

(add-hook 'eshell-exit-hook #'quit-window)
(setopt eshell-modules-list '( eshell-alias eshell-banner
                               eshell-basic eshell-cmpl
                               eshell-dirs eshell-elecslash
                               eshell-extpipe eshell-glob
                               eshell-hist eshell-ls
                               eshell-pred ; eshell-rebind
                               eshell-script eshell-smart
                               eshell-tramp eshell-unix
                               eshell-xtra eshell-prompt)
        eshell-where-to-jump 'begin
        eshell-review-quick-commands nil
        eshell-smart-space-goes-to-end t)

(setopt calc-multiplication-has-precedence nil)

(setopt proced-enable-color-flag t
        proced-auto-update-flag t
        proced-auto-update-interval 1
        proced-format 'short
        proced-show-remote-processes t)

(elpaca daemons
  (add-hook 'daemons-mode-hook #'eldoc-mode))

(elpaca bluetooth)

(elpaca trashed)

(elpaca elfeed)
(elpaca elfeed-org
  (setopt rmh-elfeed-org-files '("~/org/elfeed.org"))
  (elfeed-org))

(elpaca denote
  (require 'denote-journal-extras)
  (setopt denote-journal-extras-title-format 'day-date-month-year)

  (with-eval-after-load 'doct
    (setopt org-capture-templates (doct-add-to org-capture-templates
                                               '(("  Denote" :keys "n"
                                                  :file denote-last-path
                                                  :type plain
                                                  :template denote-org-capture
                                                  :no-save t
                                                  :immediate-finish nil
                                                  :kill-buffer t))
                                               t))))

(elpaca wordel)

(elpaca codespaces
  (require 'codespaces)
  (codespaces-setup))

(elpaca verb
  (with-eval-after-load 'org
    (ra/keymap-set org-mode-map
      "C-c C-r" verb-command-map)))

(elpaca impostman)

(elpaca (mount :host github :repo "zellerin/mount-mode"))

(elpaca (noman :host github :repo "andykuszyk/noman.el")
  (autoload #'noman "noman" "Attempt to parse command line help for the command CMD" (interactive)))

(provide '+apps)
;;; +apps.el ends here
