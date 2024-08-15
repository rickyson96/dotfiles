;;; +mappings-modal.el --- Modal Editing in Emacs    -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Ricky Anderson

;; Author: Ricky Anderson <rickyson@thinky>
;; Keywords: local

;; This file is not part of GNU Emacs

;;; Commentary:

;; Tried:
;;   Meow: very good, especially `beacon-mode', but not working
;;         great with puni, and missing mwim navigation goodies
;;   XFK: it's so hard to make it work with my muscle memory. It feels
;;         like `meow' integrates better withemcas ecosystem


;;; Code:

(elpaca embrace
  (add-hook 'org-mode-hook #'embrace-org-mode-hook)
  ;; See https://github.com/meow-edit/meow/discussions/273#discussioncomment-3158654
  (defun embrace-markdown-mode-hook ()
    (require 'embrace)
    (dolist (lst '((?* "*" . "*")
                   (?\ "\\" . "\\")
                   (?$ "$" . "$")
                   (?/ "/" . "/")))
      (embrace-add-pair (car lst) (cadr lst) (cddr lst))))
  (add-hook 'markdown-mode-hook 'embrace-markdown-mode-hook))

;;;###autoload
(defun ra/meow-block-to-block (arg)
  "Run `meow-block' initially, and `meow-to-block' for next invocation"
  (interactive "P")
  (if meow--selection
      (meow-to-block arg)
    (meow-block arg)))

;;;###autoload
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

;;;###autoload
(defun ra/meow-yank-grab ()
  "Yank or replace region with secondary selection"
  (interactive)
  (let* ((rbeg (region-beginning))
         (rend (region-end))
         (str (meow--second-sel-get-string))
         ;; (next-marker (make-marker))
         )
    ;; (move-marker next-marker (overlay-end mouse-secondary-overlay))
    (when (region-active-p) (delete-region rbeg rend))
    (insert str)))

;;;###autoload
(defun meow-setup ()
  (el-patch-defun meow-change-save ()
    (interactive)
    (let ((select-enable-clipboard meow-use-clipboard))
      ((el-patch-swap when if) (and (meow--allow-modify-p) (region-active-p))
       (el-patch-wrap 1 0
         (progn
           (el-patch-add (setq this-command #'meow-change-save))
           (el-patch-wrap 1 0
             (meow--with-selection-fallback
              (kill-region (region-beginning) (region-end))
              (meow--switch-state 'insert)
              (setq-local meow--insert-pos (point))))))
       (el-patch-add
         (kill-region (point) (1+ (point)))
         (meow--switch-state 'insert)))))

  (setq meow-cheatsheet-layout meow-cheatsheet-layout-dvorak)
  (ra/keymap-set mode-specific-map
    "?" #'meow-cheatsheet)
  (meow-motion-overwrite-define-key
   ;; custom keybinding for motion state
   '("<escape>" . ignore)
   '("SPC" . "H-SPC"))
  (ra/keymap-set meow-keypad-state-keymap
    "C-g" #'meow-keypad-quit)
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
   '("c" . meow-change-save)
   '("C" . ra/meow-yank-grab)
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
   '("O" . ra/meow-block-to-block)
   '("p" . meow-prev)
   '("P" . meow-prev-expand)
   '("q" . meow-quit)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("s" . meow-search)
   `("S" . embrace-commander)
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
   '("'" . ra/meow-kmacro)
   '("/" . meow-visit)
   '("\"" . meow-end-or-call-kmacro)
   '("-" . negative-argument)
   '("_" . meow-universal-argument)
   '(";" . meow-M-x)
   '(":" . eval-expression)
   '("," . meow-bounds-of-thing)
   '("." . meow-inner-of-thing)
   '("<" . meow-beginning-of-thing)
   '(">" . meow-end-of-thing)
   '("!" . ra/shell-command)
   '("#" . eat-project-other-window)
   '("$" . ra/eshell-or-project-eshell)
   '("%" . anzu-query-replace-regexp)
   '("<escape>" . ignore))

  (setopt meow-expand-hint-remove-delay 0
          meow-use-clipboard t))

;;;###autoload
(defun ra/meow-kmacro ()
  "Run kmacro function based on mode.
NORMAL: toggle kmacro state using `meow-start-kmacro' and `meow-end-or-call-kmacro'.
BEACON: toggle kmacro state using `meow-beacon-start' and `meow-end-or-call-kmacro'."
  (interactive)
  (cond ((and meow-beacon-mode meow--beacon-defining-kbd-macro) (meow-end-or-call-kmacro))
        (meow-beacon-mode (meow-beacon-start))
        (defining-kbd-macro (meow-end-or-call-kmacro))
        (t (meow-start-kmacro))))

(elpaca meow
  (require 'meow)
  (meow-setup)
  (meow-global-mode 1)

  (defvar ra/meow--macrostep-setup nil)

  (defun ra/meow--macrostep-hook-function ()
    "Switch meow state when entering/leaving macrostep-mode"
    (if (bound-and-true-p macrostep-mode)
        (meow--switch-to-motion)
      (meow--switch-to-normal)))

  (defun ra/meow--setup-macrostep (enable)
    "Setup macrostep. ENABLE non-nil means turn on shim."
    (setq ra/meow--macrostep-setup enable)
    (if enable
        (add-hook 'macrostep-mode-hook #'ra/meow--macrostep-hook-function)
      (remove-hook 'macrostep-mode-hook #'ra/meow--macrostep-hook-function)))

  (with-eval-after-load "macrostep" (ra/meow--setup-macrostep t))
  (add-hook 'ediff-mode-hook #'meow-motion-mode))

;; (elpaca xah-fly-keys
;; (setopt xah-fly-use-control-key nil
;;         xah-fly-use-meta-key nil)
;; (require 'xah-fly-keys)
;; (xah-fly-keys-set-layout "dvorak")
;; (xah-fly-keys 1)
;; (ra/keymap-set xah-fly-command-map
;;   "F" 'undo-fu-only-redo)
;; )

(provide '+mappings-modal)
;;; +mappings-modal.el ends here
