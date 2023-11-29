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

(auth-source-pass-enable)

(elpaca eat
  (add-hook 'eat-exit-hook #'quit-window))

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
                               eshell-pred eshell-rebind
                               eshell-script eshell-smart
                               eshell-term eshell-tramp
                               eshell-unix eshell-xtra
                               eshell-prompt)
        eshell-where-to-jump 'begin
        eshell-review-quick-commands nil
        eshell-smart-space-goes-to-end t)

(setopt calc-multiplication-has-precedence nil)

(setopt proced-enable-color-flag t
        proced-auto-update-flag t
        proced-auto-update-interval 1
        proced-format 'short
        proced-show-remote-processes t)

(provide '+apps)
;;; +apps.el ends here
