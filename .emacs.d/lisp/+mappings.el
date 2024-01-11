;;; +mappings.el --- My Emacs Mappings               -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Ricky Anderson

;; Author: Ricky Anderson <rickyson@thinky>
;; Keywords: local

;; This file is not part of GNU Emacs

;;; Commentary:

;;

;;; Code:

(elpaca puni
  (dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook sgml-mode-hook nxml-mode-hook tex-mode-hook eval-expression-minibuffer-setup-hook))
    (add-hook hook #'puni-mode))

  (defun ra/my-kill-line (&optional n)
    "Kill a line forward while keeping expressions balanced.
If nothing can be deleted, kill backward.  If still nothing can be
deleted, kill the pairs around point.
Taken from: https://github.com/AmaiKinono/puni/wiki/Useful-commands"
    (interactive "P")
    (require 'puni)
    (if n
        (puni-kill-line (prefix-numeric-value n))
      (let ((bounds (puni-bounds-of-list-around-point)))
        (if (eq (car bounds) (cdr bounds))
            (when-let ((sexp-bounds (puni-bounds-of-sexp-around-point)))
              (puni-delete-region (car sexp-bounds) (cdr sexp-bounds) 'kill))
          (if (eq (point) (cdr bounds))
              (puni-backward-kill-line)
            (puni-kill-line))))))

  (defun ra/puni-C-w (&optional n)
    "Do `puni-backward-kill-word' when no region active. Else, do `puni-kill-region'"
    (interactive "P")
    (if (use-region-p)
        (puni-kill-region)
      (puni-backward-kill-word n)))

  (ra/keymap-set puni-mode-map
    "C-k" #'ra/my-kill-line
    "C-w" #'ra/puni-C-w
    "C-<backspace>" #'puni-force-delete))

(defun ra/narrow-or-widen-dwim (p)
  "Widen if buffer is narrowed, narrow-dwim otherwise.
Dwim means: region, org-src-block, org-subtree, or
defun, whichever applies first. Narrowing to
org-src-block actually calls `org-edit-src-code'.

With prefix P, don't widen, just narrow even if buffer
is already narrowed.

Taken from: http://endlessparentheses.com/emacs-narrow-or-widen-dwim.html"
  (interactive "P")
  (declare (interactive-only))
  (cond ((and (buffer-narrowed-p) (not p)) (widen))
        ((region-active-p)
         (narrow-to-region (region-beginning)
                           (region-end)))
        ((derived-mode-p 'org-mode)
         ;; `org-edit-src-code' is not a real narrowing
         ;; command. Remove this first conditional if
         ;; you don't want it.
         (cond ((ignore-errors (org-edit-src-code) t)
                (delete-other-windows))
               ((ignore-errors (org-narrow-to-block) t))
               (t (org-narrow-to-subtree))))
        ((derived-mode-p 'latex-mode)
         (LaTeX-narrow-to-environment))
        (t (narrow-to-defun))))

(defun ra/C-w-dwim (p)
  "Run `kill-region' on region active, else run `backward-kill-word'"
  (interactive "P")
  (if (use-region-p)
      (kill-region (region-beginning) (region-end))
    (backward-kill-word (prefix-numeric-value p))))

(defun ra/shell-command (p)
  "`shell-command', which on prefix \[universal-argument], got to define `default-directory'"
  (interactive "P")
  (let ((default-directory (if p
                               (read-directory-name "shell-command on dir: ")
                             default-directory)))
    (call-interactively #'shell-command)))

(defun ra/comment-dwim-on-region (&optional p)
  "Dwim style comment on region. Do nothing when region is inactive.

By default, it will comment whole line,
with \\[universal-argument], it will only comment selected region
with \\[universal-argument] -, it will uncomment the region"
  (comment-normalize-vars)
  (when-let ((beg (use-region-beginning))
             (end (use-region-end))
             (comment-fn (if (< (prefix-numeric-value p) 0)
                             #'uncomment-region
                           #'comment-or-uncomment-region)))
    (unless p
      (setq beg (save-excursion
                  (goto-char (region-beginning))
                  (line-beginning-position)))
      (setq end (save-excursion
                  (goto-char (region-end))
                  (line-end-position))))
    (funcall comment-fn beg end)))

(setopt comment-style 'extra-line
        comment-empty-lines 'eol)

(defun ra/comment-dwim (&optional p)
  "Custom comment-dwim style
With region active: call `ra/comment-dwim-on-region'
With \\[universal-argument] -: delete comment (a.k.a `comment-kill')
With \\[universal-argument]  : create comment for this line (a.k.a `comment-indent')
With positive prefix : comment this much line forward.
Without prefix: comment line (a.k.a `comment-line')"
  (interactive "P")
  (comment-normalize-vars)
  (pcase p
    ((guard (use-region-p)) (ra/comment-dwim-on-region p))
    ('- (save-excursion (comment-kill 1)))
    ('(4) (comment-indent t))
    ('nil (if (save-excursion (beginning-of-line) (not (looking-at "\\s-*$")))
              (comment-line 1)
            (comment-dwim nil)))
    ((pred numberp) (comment-line p))))

(ra/keymap-set narrow-map
  "n" #'ra/narrow-or-widen-dwim
  "r" #'narrow-to-region)

(ra/keymap-set goto-map
  "o" #'consult-outline
  "i" #'consult-imenu
  "f" #'consult-flymake
  "g" #'consult-goto-line
  "M-g" #'consult-goto-line)

(ra/keymap-set search-map
  "d" #'consult-fd
  "p" #'consult-ripgrep ; search (p)roject
  "l" #'consult-line
  "L" #'consult-line-multi)

(defvar-keymap ra/manipulate-repeat-map
  :doc "Manipulate map repeatable commnads"
  :repeat t
  "a" #'puni-slurp-backward
  "o" #'puni-barf-backward
  "e" #'puni-barf-forward
  "u" #'puni-slurp-forward)

(defvar-keymap ra/manipulate-map
  :doc "Keymap for text and region manipulation"
  :parent ra/manipulate-repeat-map
  "s" #'puni-splice
  "k" #'puni-squeeze
  "r" #'puni-raise
  "c" #'puni-convolute
  "l" #'puni-split
  "t" #'puni-transpose
  "d" #'crux-duplicate-current-line-or-region
  "D" #'crux-duplicate-and-comment-current-line-or-region)

(defvar-keymap ra/completion-map
  :doc "Keymap for hosting multiple completion type"
  "f" #'cape-file)

(defvar-keymap ra/toggle-map
  :doc "Keymap for toggling emacs state"
  :prefix 'ra/toggle-map
  "*" #'literate-calc-mode
  "c" #'literate-calc-mode
  "d" #'toggle-debug-on-error
  "f" #'auto-fill-mode
  "l" #'visual-line-mode
  "L" #'toggle-truncate-lines
  "q" #'toggle-debug-on-quit
  "r" #'dired-toggle-read-only
  "n" #'ra/narrow-or-widen-dwim
  "w" #'whitespace-mode)

(defvar-keymap ra/open-map
  :doc "Keymap for opening various apps on Emacs"
  :prefix 'ra/open-map
  "p" #'pass
  "t" #'eat
  "f" #'elfeed
  "m" #'consult-man
  "n" #'denote-open-or-create
  "RET" #'mediator-open
  "'" #'proced)

(defvar-keymap ra/eval-map
  :doc "Keymap for evaluating elisp code"
  ":" #'eval-expression
  "b" #'eval-buffer
  "e" #'eval-last-sexp
  "d" #'eval-defun
  "m" #'macrostep-expand
  "r" #'crux-eval-and-replace)

;; C-c keymap
(ra/keymap-set mode-specific-map
  "TAB" ra/completion-map
  "e" ra/eval-map
  "o" #'ra/open-map
  "T" #'ra/toggle-map)

;; C-x keymap
(ra/keymap-set ctl-x-map
  "SPC" #'set-rectangular-region-anchor
  "b" #'consult-buffer
  "d" #'dirvish-dwim
  "o" #'ace-window)

(ra/keymap-set (current-global-map)
  "C-h B" #'embark-bindings

  "C-<tab>" #'corfu-candidate-overlay-complete-at-point
  "C-o" #'crux-smart-open-line
  "C-." #'embark-act
  "C-," #'ra/substitute-map
  "C-w" #'ra/C-w-dwim

  "M-." #'embark-dwim
  "M-'" #'er/expand-region
  "M-!" #'ra/shell-command
  "M-;" #'ra/comment-dwim
  "M-#" #'eat
  "M-$" #'eshell
  "M-y" #'consult-yank-pop
  "M-m" ra/manipulate-map
  "M-o" #'switch-window
  "M-N" #'move-text-down
  "M-P" #'move-text-up)

(require '+mappings-modal)

(provide '+mappings)
;;; +mappings.el ends here
