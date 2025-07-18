;;; +defaults.el --- Emacs More Sensible Defaults    -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Ricky Anderson

;; Author: Ricky Anderson <rickyson@thinky>
;; Keywords: local

;; This file is not part of GNU Emacs

;;; Commentary:

;; This configurations are taken from few sources, notably:
;; 1. https://idiomdrottning.org/bad-emacs-defaults
;; 2. https://github.com/hrs/sensible-defaults.el
;; 3. https://git.sr.ht/~technomancy/better-defaults

;;; Code:

(setopt apropos-do-all t        ; apropos include all helps
        mouse-yank-at-point t       ; middle-mouse yank at point instead of at cursor

        custom-file (expand-file-name "custom.el" user-emacs-directory) ; do not clutter init.el

        savehist-additional-variables '(search-ring regexp-search-ring)
        savehist-autosave-interval 60

        recentf-max-saved-items 500
        recentf-max-menu-items 15

        ;; for eat purposes https://codeberg.org/akib/emacs-eat/issues/110
        read-process-output-max (* 4 1024 1024)
        process-adaptive-read-buffering nil

        ;; Make \\[universal-argument] on `browse-url' opens `eww'
        browse-url-secondary-browser-function #'eww-browse-url

        shell-command-prompt-show-cwd t
        find-file-visit-truename t
        enable-recursive-minibuffers t
        inhibit-startup-screen t
        backup-by-copying t
        sentence-end-double-space nil
        require-final-newline t
        frame-inhibit-implied-resize t
        uniquify-buffer-name-style 'forward
        save-interprogram-paste-before-kill t
        visible-bell t
        load-prefer-newer t
        use-dialog-box nil
        global-auto-revert-non-file-buffers t
        auto-revert-check-vc-info t
        auto-revert-interval 1          ; NOTE: use 1 sec interval, might impact performance
        delete-by-moving-to-trash t)

(load custom-file 'noerror 'nomessage)

(fset 'yes-or-no-p 'y-or-n-p)

(save-place-mode 1)
(savehist-mode 1)
(recentf-mode 1)
(global-auto-revert-mode 1)
(indent-tabs-mode 0)

(elpaca dtrt-indent
  (require 'dtrt-indent)
  (setopt dtrt-indent-global-mode t
          tab-width 4))

;; Find a way to non-intrusively show whitespace using `whitespace-mode'
;; \\`C-c T d' should then toggle between the intrusive version.
(elpaca ws-butler
  (ws-butler-global-mode 1))

(elpaca editorconfig
  (editorconfig-mode 1)
  (setopt editorconfig-trim-whitespaces-mode 'ws-butler-mode))

(elpaca super-save
  (super-save-mode 1)
  (setopt super-save-auto-save-when-idle t
          super-save-remote-files nil))

(elpaca undo-fu
  (setopt undo-limit 67108864 ; 64mb.
          undo-strong-limit 100663296 ; 96mb.
          undo-outer-limit 1006632960) ; 960mb.
  (ra/keymap-set (current-global-map)
    "<remap> <undo>" #'undo-fu-only-undo
    "<remap> <undo-redo>" #'undo-fu-only-redo))

(elpaca undo-fu-session
  (undo-fu-session-global-mode 1))

(elpaca vundo
  (setopt vundo-glyph-alist vundo-ascii-symbols)
  (with-eval-after-load 'vundo
    (ra/keymap-set vundo-mode-map
      "h" #'vundo-backward
      "t" #'vundo-forward)))

(provide '+defaults)
;;; +defaults.el ends here
