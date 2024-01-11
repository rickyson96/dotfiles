;;; +mappings-modal.el --- Modal Editing in Emacs    -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Ricky Anderson

;; Author: Ricky Anderson <rickyson@thinky>
;; Keywords: local

;; This file is not part of GNU Emacs

;;; Commentary:

;; Tried:
;;   Meow: very good, especially `beacon-mode', but not working
;;         great with puni, and missing mwim navigation goodies
;;   XFK: trying


;;; Code:

(defun ra/meow-block-to-block (arg)
  "Run `meow-block' initially, and `meow-to-block' for next invocation"
  )

(defun ra/meow-next-word-expand (n)
  "`meow-next-word' but expand selection"
  (interactive "p")
  (unless (equal 'word (cdr (meow--selection-type)))
    (meow--cancel-selection))
  (thread-first (meow--make-selection '(expand . word)
                                      (if (region-active-p) (mark) (point))
                                      (point))
                (meow--select))
  (meow-next-word 1))

(defun meow-xah-adapt ()
  "Meow modal adapted from xah-fly-keys"
  (meow-normal-define-key
   ;; Movement
   '("c" . meow-prev)
   '("C" . meow-prev-expand)
   '("t" . meow-next)
   '("T" . meow-next-expand)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("n" . meow-right)
   '("N" . meow-right-expand)
   '("g" . meow-back-word)
   '("r" . meow-next-word)
   '("R" . ra/meow-next-word-expand)))

(defun meow-setup ()
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-dvorak)
  (ra/keymap-set mode-specific-map
    "?" #'meow-cheatsheet
    "f" #'find-file
    "s" #'save-buffer
    "r" #'bookmark-jump
    "p" project-prefix-map
    "'" #'org-capture)
  (meow-motion-overwrite-define-key
   ;; custom keybinding for motion state
   '("<escape>" . ignore))
  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("d" . meow-delete)
   '("D" . meow-backward-delete)
   '("e" . meow-line)
   '("E" . meow-goto-line)
   `("f" . ,(cmd! (meow--cancel-selection) (call-interactively #'embark-dwim)))
   '("F" . embark-act)
   '("g" . meow-cancel-selection)
   '("G" . meow-grab)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("i" . meow-insert)
   '("I" . meow-open-above)
   '("j" . meow-join)
   `("J" . ,(cmd! (meow-join -1)))
   '("k" . meow-kill)
   '("K" . meow-save)
   '("l" . meow-till)
   '("L" . meow-find)
   '("m" . meow-mark-word)
   '("M" . meow-mark-symbol)
   '("n" . meow-next)
   '("N" . meow-next-expand)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("p" . meow-prev)
   '("P" . meow-prev-expand)
   '("q" . meow-quit)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("s" . meow-search)
   ;;`("S" . ,ra/meow-pair)
   '("t" . meow-right)
   '("T" . meow-right-expand)
   '("u" . meow-undo)
   '("U" . undo-fu-only-redo)
   '("v" . meow-page-down)
   '("V" . meow-page-up)
   '("w" . meow-next-word)
   '("W" . meow-next-symbol)
   '("x" . "C-x")
   '("X" . meow-sync-grab)
   '("y" . meow-yank)
   '("Y" . meow-yank-pop)
   '("z" . meow-reverse)
   '("Z" . meow-pop-selection)
   '("'" . my/meow-kmacro)
   '("/" . meow-visit)
   '("\"" . meow-end-or-call-kmacro)
   '("-" . ngative-argument)
   '("_" . meow-universal-argument)
   '(";" . meow-M-x)
   '(":" . eval-expression)
   '("," . meow-bounds-of-thing)
   '("." . meow-inner-of-thing)
   '("<" . meow-beginning-of-thing)
   '(">" . meow-end-of-thing)
   '("!" . shell-command)
   '("#" . eat-project-other-window)
   '("%" . anzu-query-replace-regexp)
   '("<escape>" . ignore))

  (setopt meow-expand-hint-remove-delay 0
          meow-use-clipboard t))

;; TODO: meow kmacro

(elpaca meow
  (require 'meow)
  (meow-setup)
  (meow-global-mode 1)

  (with-eval-after-load 'eat
    (add-hook 'meow-normal-mode-hook (lambda ()
                                       (when (eq major-mode 'eat-mode)
                                         (eat-emacs-mode))))
    (add-hook 'meow-insert-mode-hook (lambda () (when (eq major-mode 'eat-mode)
                                                  (eat-semi-char-mode))))))

(elpaca xah-fly-keys
  ;; (setopt xah-fly-use-control-key nil
  ;;         xah-fly-use-meta-key nil)
  ;; (require 'xah-fly-keys)
  ;; (xah-fly-keys-set-layout "dvorak")
  ;; (xah-fly-keys 1)
  ;; (ra/keymap-set xah-fly-command-map
  ;;   "F" 'undo-fu-only-redo)
  )

(provide '+mappings-modal)
;;; +mappings-modal.el ends here
