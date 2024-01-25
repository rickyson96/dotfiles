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
(defun +elpaca-unload-seq (e)
  "Unload seq before continuing the elpaca build, then continue to build the recipe E."
  (and (featurep 'seq) (unload-feature 'seq t))
  (elpaca--continue-build e))
(elpaca `(seq :build ,(append (butlast (if (file-exists-p (expand-file-name "seq" elpaca-builds-directory))
                                           elpaca--pre-built-steps
                                         elpaca-build-steps))
                              (list '+elpaca-unload-seq 'elpaca--activate-package))))

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

(setopt bookmark-save-flag 1) ; auto save bookmark

;; (require '+project)
;; (defun ra/project-remember-default-directory ()
;;   ""
;;   (let ((tabspaces-project-switch-commands #'magit-project-status)
;; 		(project--list (append `(,default-directory) 'project--list))))
;;   (tabspaces-project-switch-project-open-file default-directory))
;; (with-eval-after-load 'magit
;;   (add-hook 'magit-post-clone-hook #'ra/project-remember-default-directory))

(require '+window)

(elpaca tabspaces
  (setopt tabspaces-use-filtered-buffers-as-default t
		  tabspaces-keymap-prefix "C-c t"
		  tab-bar-show nil)

  (tabspaces-mode 1)

  (ra/keymap-set tabspaces-mode-map
	"<remap> <project-switch-project>" #'tabspaces-open-or-create-project-and-workspace)

  (with-eval-after-load 'marginalia
	(add-to-list 'marginalia-command-categories '(tabspaces-switch-or-create-workspace . tab)))

  ;; Filter Buffers for Consult-Buffer
  ;; Taken from https://github.com/mclear-tools/tabspaces

  (with-eval-after-load 'consult
	;; hide full buffer list (still available with "b" prefix)
	(consult-customize consult--source-buffer :hidden t :default nil)
	;; set consult-workspace buffer list
	(defvar consult--source-workspace
	  (list :name     "Workspace Buffers"
			:narrow   ?w
			:history  'buffer-name-history
			:category 'buffer
			:state    #'consult--buffer-state
			:default  t
			:items    (lambda () (consult--buffer-query
								  :predicate #'tabspaces--local-buffer-p
								  :sort 'visibility
								  :as #'buffer-name)))

	  "Set workspace buffer list for consult-buffer.")
	(add-to-list 'consult-buffer-sources 'consult--source-workspace)))


(require '+completion)

(require '+intellisense)

;; Remote development
(setopt tramp-default-remote-shell "/bin/bash"
		enable-remote-dir-locals t
		remote-file-name-inhibit-locks nil
		remote-file-name-inhibit-cache 60
		tramp-ssh-controlmaster-options "")

(with-eval-after-load 'tramp
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  (add-to-list 'tramp-connection-properties '("/sshx:" "remote-shell" "/usr/bin/bash"))
  (add-to-list 'tramp-methods '("sshbash"
								(tramp-login-program "ssh")
								(tramp-login-args
								 (("-l" "%u")
								  ("-p" "%p")
								  ("%c")
								  ("-e" "none")
								  ("%h")))
								(tramp-async-args
								 (("-q")))
								(tramp-direct-async t)
								(tramp-remote-shell "/bin/bash")
								(tramp-remote-shell-login
								 ("-l"))
								(tramp-remote-shell-args
								 ("-c")))))
;; (with-eval-after-load 'eat
;; (add-to-list 'eat-tramp-shells '("ssh" . "/bin/bash")))

;; View mode
;; Enable view-mode on read-only buffer
(setopt view-read-only t)

(electric-pair-mode 1)

(elpaca outshine
  (setopt outline-minor-mode-prefix "\C-c o"))

(setopt isearch-lazy-count t)
(elpaca anzu
  (global-anzu-mode 1)
  (ra/keymap-set (current-global-map)
    "<remap> <query-replace>" #'anzu-query-replace
    "<remap> <query-replace-regexp>" #'anzu-query-replace-regexp))

(elpaca ialign
  (autoload 'ialign "ialign"))

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

(elpaca which-key
  (setopt which-key-show-early-on-C-h t
		  which-key-idle-secondary-delay 0.01)
  (which-key-mode 1))

(elpaca string-inflection
  (with-eval-after-load 'embark
    (keymap-set embark-identifier-map "~" #'string-inflection-all-cycle)
    (add-to-list 'embark-repeat-actions #'string-inflection-all-cycle)))

(elpaca move-text
  (defun indent-region-advice (&rest _)
	(let ((deactivate deactivate-mark))
      (if (region-active-p)
          (indent-region (region-beginning) (region-end))
		(indent-region (line-beginning-position) (line-end-position)))
      (setq deactivate-mark deactivate)))

  (advice-add 'move-text-up :after 'indent-region-advice)
  (advice-add 'move-text-down :after 'indent-region-advice)

  (defun ra/move-text-fix-eol (args)
	"fix move-text not marking eol when already marking the whole line"
	(cl-destructuring-bind (start end n) args
	  (if (let ((point<mark (< (point) (mark)))
				check)
			(when (not point<mark) (exchange-point-and-mark))
			(setq check (bolp))
			(exchange-point-and-mark)
			(and check
				 (eolp)
				 (not (bolp))))
		  (progn
			(forward-char 1)
			(list start (1+ end) n))
		(list start end n))))

  (define-advice move-text-up (:filter-args (args) fix-region-mark-eol)
	(ra/move-text-fix-eol args))
  (define-advice move-text-down (:filter-args (args) fix-region-mark-eol)
	(ra/move-text-fix-eol args)))

(elpaca multi-line)

(setopt eldoc-documentation-strategy #'eldoc-documentation-enthusiast
		;; Set eldoc to only show single line on minibuffer
		;; I use eldoc mainly to show function signature.
		;; In depth documentation should only be show when requested.
		eldoc-echo-area-use-multiline-p nil)

(elpaca (envrc :host github :repo "purcell/envrc"
			   :remotes ("envrc-remote" :host github :repo "siddharthverma314/envrc"))
  (setopt envrc-remote t)
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

(elpaca (noman :host github :repo "andykuszyk/noman.el"))

(elpaca info-colors
  (add-hook 'Info-selection-hook #'info-colors-fontify-node))

(elpaca elisp-demos
  (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update))


(require '+lang)

(elpaca (mediator :host github :repo "dalanicolai/mediator")
  (with-eval-after-load 'embark
	(ra/keymap-set embark-file-map
	  "O" #'mediator-open-file)))

(require '+apps)

(defun ra/get-jira-url (&optional write-to-buffer)
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

(defun ra/gobit-copy-mr-message ()
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

;; TODO: make openvpn run via emacs function
(defun ra/start-openvpn (config)
  "Start openvpn with designated config file"
  (make-process :name "openvpn"
				:buffer "*openvpn*"
				:command '("openvpn")))

(require '+mappings)

(repeat-mode 1)

;;; init.el ends here
