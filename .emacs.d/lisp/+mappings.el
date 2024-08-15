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

(elpaca (combobulate :host github :repo "mickeynp/combobulate"))

;;;###autoload
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

;;;###autoload
(defun ra/C-w-dwim (p)
  "Run `kill-region' on region active, else run `backward-kill-word'"
  (interactive "P")
  (if (use-region-p)
      (kill-region (region-beginning) (region-end))
    (backward-kill-word (prefix-numeric-value p))))

(defun ra/shell-command (&optional p)
  "Improved `shell-command'! Now using `shell-command+'.
\\[universal-argument] (default on `shell-command+'): Insert output to buffer.
\\[universal-argument] \\[universal-argument]: Select directory to execute.
\\[universal-argument] \\[universal-argument] \\[universal-argument]: Select directory and insert output."
  (interactive "P")
  (let* ((num-prefix (prefix-numeric-value p))
         (default-directory (if (>= num-prefix 16)
                                (read-directory-name "shell-command on dir: ")
                              default-directory))
         (current-prefix-arg (if (= num-prefix 16)
                                 nil
                               p)))
    (call-interactively #'shell-command+)))

;;;###autoload
(defun ra/eshell-command (&optional p)
  "`shell-command', which on prefix \[universal-argument], got to define `default-directory'"
  (interactive "P")
  (let ((default-directory (if p
                               (read-directory-name "shell-command on dir: ")
                             default-directory))
        (eshell-hist--new-items (if eshell-hist--new-items eshell-hist--new-items)))
    (call-interactively #'eshell-command)))

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

;;;###autoload
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

;;;###autoload
(defun ra/cmd-or-project-cmd (cmd project-cmd)
  "Interactively run CMD or PROJECT-CMD based on whether current file is in
project or not"
  (if (project-current)
      (call-interactively project-cmd)
    (call-interactively cmd)))

;;;###autoload
(defun ra/eshell-or-project-eshell ()
  "Run `project-eshell' on project, else run `eshell'"
  (interactive)
  (ra/cmd-or-project-cmd #'eshell #'project-eshell))

;;;###autoload
(defun ra/find-file (&optional arg)
  "Run `project-find-file' when inside a project. If not, run `find-file'.
\\[universal-argument] forces the latter."
  (interactive "P")
  (if (and (project-current) (not arg))
      (project-find-file)
    (call-interactively #'find-file)))

(define-advice forward-page (:after (&rest _) recenter-to-top)
  "Use `recenter-top-bottom' to recenter current page to top"
  (recenter-top-bottom 0))

;; Inspired by `doom/escape' from doomemacs
(defcustom ra/keyboard-quit-hook nil
  "Hook run by `ra/keyboard-quit'."
  :type 'hook)

(defun ra/keyboard-quit (&optional p)
  "Run `ra/keyboard-quit-hook' until one of it succeeds.
With \\[universal-argument], run all the hook and then `keyboard-quit'.
With \\[universal-argument] \\[universal-argument], run `keyboard-escape-quit'"
  (interactive "P")
  (let ((inhibit-quit t)
        (kbd-quit (lambda ()
                    (unwind-protect (keyboard-quit)
                      (setq this-command 'keyboard-quit)))))
    (pcase p
      ('(4) (progn (run-hook-with-args 'ra/keyboard-quit-hook)
                   (funcall kbd-quit)))
      ('(16) (keyboard-escape-quit))
      (_ (cond ((use-region-p) (deactivate-mark))
               ((run-hook-with-args-until-success 'ra/keyboard-quit-hook))
               ((or defining-kbd-macro executing-kbd-macro) nil)
               ((funcall kbd-quit)))))))

(ra/keymap-set (current-global-map)
  "<remap> <keyboard-quit>" #'ra/keyboard-quit)

(elpaca transpose-mark
  (defun ra/transpose-mark-quit-overlay ()
    "Quit `transpose-mark' overlay. Return `t' if it succeeds, and `nil' if it
doesn't find overlay"
    (when (ra/fbound-and-true-p 'transpose-mark-region-overlay-active)
      (transpose-mark-region-abort)
      t))
  (add-hook 'ra/keyboard-quit-hook #'ra/transpose-mark-quit-overlay))

(defun ra/eval-region (beg end &optional p)
  "Run `eval-region' on current region.
\\[universal-argument] to replace the region with result"
  (interactive "r\nP")
  (save-excursion
    (goto-char end)
    (if p
        (crux-eval-and-replace)
      (eval-last-sexp nil)))
  (deactivate-mark))

(elpaca selected
  (selected-global-mode 1)
  (setopt selected-ignore-modes '(magit-status-mode diff-mode magit-revision-mode))
  (ra/keymap-set selected-keymap
    "u" #'crux-upcase-region
    "d" #'crux-downcase-region
    "e" #'ra/eval-region
    "w" #'count-words-region
    "c" #'crux-capitalize-region
    "k" #'kill-region
    "r" #'ra/replace-region-with-kill
    "," #'iedit-mode
    "'" #'er/expand-region
    "t" #'transpose-mark
    "SPC" #'iedit-rectangle-mode))

(with-eval-after-load 'kmacro
  (defalias 'kmacro-insert-macro #'insert-kbd-macro)
  (ra/keymap-set kmacro-keymap
    "I" #'kmacro-insert-macro))

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

(defvar-keymap ra/multi-line-repeat-map
  :doc "`multi-line' repeating map"
  :repeat t
  "m" #'multi-line
  "-" #'multi-line-single-line
  "h" #'multi-line-highlight-current-candidates)

(defvar-keymap ra/manipulate-map
  :doc "Keymap for text and region manipulation"
  :prefix 'ra/manipulate-map
  "a" #'puni-slurp-backward
  "o" #'puni-barf-backward
  "e" #'puni-barf-forward
  "u" #'puni-slurp-forward
  "s" #'puni-splice
  "k" #'puni-squeeze
  "r" #'puni-raise
  "c" #'puni-convolute
  "|" #'puni-split
  "t" #'puni-transpose
  "d" #'crux-duplicate-current-line-or-region
  "D" #'crux-duplicate-and-comment-current-line-or-region
  "j" #'join-line
  "J" #'crux-top-join-line
  "." #'mc/mark-all-like-this-dwim
  "n" #'mc/mark-next-like-this
  "m" #'multi-line)

(defvar-keymap ra/completion-map
  :doc "Keymap for hosting multiple completion type"
  "f" #'cape-file)

(defvar-keymap ra/toggle-map
  :doc "Keymap for toggling emacs state"
  :prefix 'ra/toggle-map
  "*" #'literate-calc-minor-mode
  "c" #'literate-calc-minor-mode
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
  "d" #'dashboard-open
  "D" #'docker
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

(defvar-keymap ra/window-repeat-map
  :doc "Keymap for repeatedly modify emacs' windows"
  :repeat t
  "c" #'enlarge-window
  "t" #'shrink-window
  "h" #'shrink-window-horizontally
  "n" #'enlarge-window-horizontally)

(defun ra/mark-line (&optional p)
  "Mark line"
  (interactive "p")
  (beginning-of-line)
  (set-mark (point))
  (end-of-line p))

(defvar-keymap ra/object-map
  :doc "Keymap for marking objects"
  :prefix 'ra/object-map
  "l" #'ra/mark-line
  "d" #'er/mark-defun
  "g" #'er/mark-inside-python-string
  "s" #'puni-mark-sexp-at-point
  "." #'er/mark-symbol
  "w" #'er/mark-word
  "b" #'er/mark-inside-pairs
  "B" #'er/mark-outside-pairs)

(ra/keymap-set window-prefix-map
  "c" #'enlarge-window
  "t" #'shrink-window
  "h" #'shrink-window-horizontally
  "n" #'enlarge-window-horizontally)

(defvar-keymap ra/eglot-map
  :doc "Keymap for invoking eglot features"
  :prefix 'ra/eglot-map
  "h" #'eglot-inlay-hints-mode
  "d" #'eldoc-box-help-at-point)

;; C-c keymap
(ra/keymap-set mode-specific-map
  "TAB" ra/completion-map
  "a" #'org-agenda
  "e" ra/eval-map
  "o" #'ra/open-map
  "w" #'ra/window-map
  "T" #'ra/toggle-map
  "f" #'ra/find-file
  "s" #'save-buffer
  "r" #'bookmark-jump
  "p" project-prefix-map
  "c" #'org-capture
  "," #'ra/substitute-map)

;; C-x keymap
(ra/keymap-set ctl-x-map
  "SPC" #'set-rectangular-region-anchor
  "b" #'consult-buffer
  "d" #'dirvish-dwim
  "o" #'ace-window
  "O" #'ra/ace-window-prefix)

(ra/keymap-set (current-global-map)
  "C-h B" #'embark-bindings

  "C-<tab>" #'corfu-candidate-overlay-complete-at-point
  "C-o" #'crux-smart-open-line
  "C-." #'embark-act
  "C-'" #'ra/object-map
  "C-w" #'ra/C-w-dwim

  "M-." #'embark-dwim
  "M-'" #'er/expand-region
  "M-!" #'ra/shell-command
  "M-;" #'ra/comment-dwim
  "M-#" #'eat
  "M-$" #'ra/eshell-or-project-eshell
  "M-y" #'consult-yank-pop
  "M-m" 'ra/manipulate-map
  "M-o" #'ra/other-window-alternating
  "M-n" #'flymake-goto-next-error
  "M-p" #'flymake-goto-prev-error
  "M-N" #'move-text-down
  "M-P" #'move-text-up
  "M-l" #'ra/eglot-map
  "M-r" #'re-builder
  "<remap> <Info-goto-emacs-command-node>" #'describe-face)

;; remove modal
;; (require '+mappings-modal)

(provide '+mappings)
;;; +mappings.el ends here
