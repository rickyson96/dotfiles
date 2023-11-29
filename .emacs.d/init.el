;;; init.el --- The coded emacs configuration        -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Ricky Anderson

;; Author: Ricky Anderson <ricky.anderson2696@gmail.com>
;; Maintainer: Ricky Anderson <ricky.anderson2696@gmail.com>
;; Created: July 31, 2023
;; Keywords: local
;; Package-Requires: ((emacs "29.0"))

;; This file is not part of GNU Emacs

;;; Commentary:

;; This configurations mostly influenced by my experience with
;; configuring wasteland emacs, and few others:
;;  - doom-emacs: https://github.com/doomemacs/doomemacs
;;  - case duckworth: https://git.acdw.net/emacs/
;;  - system crafters youtube: https://youtube.com/@SystemCrafters

;; The last bankcruptcy is because I lost my site-lisp files.

;; Bankruptcy: 3

;;; Code:

;; Add our `lisp' folder to load-path so that emacs can search them.
(add-to-list 'load-path (locate-user-emacs-file "lisp"))

(require '+elpaca)

(setopt delete-old-versions t
		version-control t)
(elpaca no-littering
  (require 'no-littering)
  (no-littering-theme-backups) ; setup no littering to theme backups variables
  (with-eval-after-load 'recentf
    (add-to-list 'recentf-exclude
             (recentf-expand-file-name no-littering-var-directory))
    (add-to-list 'recentf-exclude
             (recentf-expand-file-name no-littering-etc-directory))))

(elpaca gcmh
  (gcmh-mode))

;; we need to wait for `no-littering' to install so that further
;; installation can conform to the no-littering directories
(elpaca-wait)

(set-face-attribute 'default nil :font "IosevraRelaxed Nerd Font" :height 110)
(set-face-attribute 'fixed-pitch nil :font "IosevraRelaxed Nerd Font")
(set-face-attribute 'variable-pitch nil :font "IosevraAileRelaxed Nerd Font")

(require '+defaults)

(elpaca xr)
(elpaca pcre2el)
(elpaca 0xc)

(defmacro ra/keymap-set (keymap &rest pairs)
  "Bind multiple pairs of KEY/DEFINITION to KEYMAP using `keymap-set'.

\(fn KEYMAP [KEY DEFINITION]...)"
  (declare (indent defun))
  (unless (zerop (mod (length pairs) 2))
    (error "PAIRS must be pair of KEY/DEFINITION"))
  (macroexp-progn (seq-map (lambda (pair)
							 `(keymap-set ,keymap ,(car pair) ,(cadr pair)))
						   (seq-split pairs 2))))
(defmacro ra/cmd (&rest body)
  "Shorthand for (lambda () (interactive) ,@body)

Taken from https://github.com/doomemacs/doomemacs/blob/844a82c4a0cacbb5a1aa558c88675ba1a9ee80a3/lisp/doom-lib.el#L521"
  (declare (doc-string 1) (pure t) (side-effect-free t))
  `(lambda (&rest _) (interactive) ,@body))

(defalias 'cmd! #'ra/cmd
  "Aliases for `ra/cmd'. Enables short cmd! command inspired from doomemacs.
It's so that if ! is not emacs-lisp friendly anymore, we can just swap for the namespaced variant.")

(elpaca (define-repeat-map :repo "https://tildegit.org/acdw/define-repeat-map.el"))

(require '+ui)

(elpaca (bmkp :host github :repo "emacsmirror/bookmark-plus")
  (require 'bookmark+))

;; (require '+project)

(require '+window)

(require '+completion)

(require '+intellisense)

;; View mode
;; Enable view-mode on read-only buffer
(setopt view-read-only t)

(electric-pair-mode 1)

(elpaca switch-window
  (setopt switch-window-shortcut-style 'qwerty
		  switch-window-qwerty-shortcuts '("a" "o" "e" "u" "h" "t" "n" "s" "i" "d" "'" "," "." "p" "g" "c" "r" "l")))

(elpaca outshine
  (setopt outline-minor-mode-prefix "\C-c o"))

(setopt isearch-lazy-count t)
(elpaca anzu
  (global-anzu-mode 1)
  (ra/keymap-set (current-global-map)
    "<remap> <query-replace>" #'anzu-query-replace
    "<remap> <query-replace-regexp>" #'anzu-query-replace-regexp))
(elpaca substitute
  (require 'substitute)
  (setopt substitute-post-replace-functions #'substitute-report-operation)
  (defvar-keymap ra/substitute-map
	:doc "Keymap for substitute package"
	:prefix 'ra/substitute-map
	"," #'substitute-target-in-buffer
	"C-," #'substitute-target-in-buffer
	"n" #'substitute-target-below-point
	"p" #'substitute-target-above-point
	"d" #'substitute-target-in-defun
	"b" #'substitute-target-in-buffer))

(elpaca kurecolor)
(elpaca rainbow-mode)

(elpaca deadgrep)

(elpaca expand-region)
(elpaca multiple-cursors)
(elpaca crux
  (require 'crux)
  (ra/keymap-set (current-global-map)
	"<remap> <kill-whole-line>" #'crux-kill-whole-line)
  (crux-with-region-or-buffer indent-region)
  (crux-with-region-or-line kill-ring-save))
(elpaca mwim
  (ra/keymap-set (current-global-map)
    "<remap> <move-beginning-of-line>" #'mwim-beginning
	"<remap> <move-end-of-line>" #'mwim-end))
(elpaca golden-ratio-scroll-screen
  (ra/keymap-set (current-global-map)
	"<remap> <scroll-up-command>" #'golden-ratio-scroll-screen-up
	"<remap> <scroll-down-command>" #'golden-ratio-scroll-screen-down)
  (setopt golden-ratio-scroll-highlight-flag nil))

(elpaca string-inflection
  (with-eval-after-load 'embark
    (keymap-set embark-identifier-map "~" #'string-inflection-all-cycle)
    (add-to-list 'embark-repeat-actions #'string-inflection-all-cycle)))

(elpaca move-text
  (defun indent-region-advice (&rest ignored)
	(let ((deactivate deactivate-mark))
      (if (region-active-p)
          (indent-region (region-beginning) (region-end))
		(indent-region (line-beginning-position) (line-end-position)))
      (setq deactivate-mark deactivate)))

  (advice-add 'move-text-up :after 'indent-region-advice)
  (advice-add 'move-text-down :after 'indent-region-advice))

(elpaca multi-line)

(setopt eldoc-documentation-strategy #'eldoc-documentation-enthusiast
		;; Set eldoc to only show single line on minibuffer
		;; I use eldoc mainly to show function signature.
		;; In depth documentation should only be show when requested.
		eldoc-echo-area-use-multiline-p nil)

(elpaca envrc
  (envrc-global-mode 1))

(elpaca makefile-executor
  (add-hook 'makefile-mode-hook #'makefile-executor-mode))

(elpaca num3-mode
  (custom-set-faces '(num3-face-even ((t :weight ultra-bold :background nil)))))

(require '+vc)

(setopt tramp-histfile-override nil
		tramp-default-method "ssh"
		tramp-verbose 0)

(elpaca helpful
  (ra/keymap-set (current-global-map)
    "<remap> <describe-function>" #'helpful-callable
    "<remap> <describe-variable>" #'helpful-variable
    "<remap> <describe-key>" #'helpful-key
    "<remap> <describe-command>" #'helpful-command
    "<remap> <describe-symbol>" #'helpful-symbol))

(elpaca info-colors
  (add-hook 'Info-selection-hook #'info-colors-fontify-node))

(elpaca elisp-demos
  (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update))

(require '+lang)

(require '+apps)

(require '+mappings)

(defun my/get-jira-url (&optional write-to-buffer)
  "Gobit: Copy formatted slack message to inform created MR"
  (interactive "p")
  (let* ((branch (magit-get-current-branch))
         (jira-regex (rx (zero-or-more nonl) (group "CRYPTO-" (zero-or-more digit)) (zero-or-more nonl)))
         (jira-issue-match (string-match jira-regex branch))
         (jira-issue (match-string 1 branch))
         (jira-link (format "https://stockbit.atlassian.net/browse/%s" jira-issue)))
	(when write-to-buffer
	  (insert jira-link))
	jira-link))

(defun my/gobit-copy-mr-message ()
  "Gobit: Copy formatted slack message to inform created MR"
  (interactive)
  (if-let ((pr (or (forge-post-at-point)
		           (forge-current-topic)))
	       (title (oref pr title))
	       (url (forge-get-url pr))
	       (jira-link (my/get-jira-url))
	       (msg-title "Hi guys @crypto-go, please review my MR! :capoo-thank:")
	       (msg (format "%s\n*%s*\n\n:merge: %s\n:jira: %s" msg-title title url jira-link)))
	  (kill-new msg)
	(message "Fail to copy mr message")))

(repeat-mode 1)

;;; init.el ends here
