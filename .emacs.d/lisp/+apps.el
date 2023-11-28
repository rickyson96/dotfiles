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

(add-hook 'eshell-exit-hook #'quit-window)
(add-hook 'eat-exit-hook #'quit-window)

(setopt proced-enable-color-flag t
        proced-auto-update-flag t
        proced-auto-update-interval 1
        proced-format 'short
        proced-show-remote-processes t)

(provide '+apps)
;;; +apps.el ends here
