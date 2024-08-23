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

(setopt gc-cons-threshold (* 2000 1000 1000) ; 1GB gc-cons
		gc-cons-percentage 0.8)

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

;; load exec-path-from-shell before configuration, so that our config
;; can rely on newly setup PATH. Some package also depends on
;; environment variable like `lsp-mode'
(elpaca exec-path-from-shell
  (when (daemonp)
	(require 'exec-path-from-shell)
	(dolist (var '("PATH"
				   "SSH_AUTH_SOCK"
				   "SSH_AGENT_PID"
				   "LANG"
				   "LC_ALL"
				   "LSP_USE_PLISTS"
				   "DISPLAY"
				   "WAYLAND_DISPLAY"))
	  (add-to-list 'exec-path-from-shell-variables var))

	(exec-path-from-shell-initialize)))

;; we need to wait for `no-littering' to install so that further
;; installation can conform to the no-littering directories
(elpaca-wait)

(defmacro ra/configure-frame (name &rest body)
  "Macro for creating hook function for frame related configurations, which will not function
correctly when being run directly on startup.
This macro will add the corresponding hooks and then remove them on startup.

See https://emacs.stackexchange.com/questions/59791/font-and-frame-configuration-in-daemon-mode"
  (declare (indent defun))
  (macroexp-progn
   (mapcan (lambda (hook) (let ((name (intern (format "%s@%s" name hook))))
							`((defun ,name (&rest _) ,@body
									 (remove-hook ',hook #',name))
							  (add-hook ',hook #',name))))
		   '(elpaca-after-init-hook server-after-make-frame-hook))))

(ra/configure-frame ra/setup-font
  (set-face-attribute 'default nil :font "monospace" :height 110)
  (set-face-attribute 'fixed-pitch nil :font "monospace")
  (set-face-attribute 'variable-pitch nil :font "sans-serif"))

(require '+defaults)

(elpaca xr)
(elpaca pcre2el)
(elpaca 0xc)
(elpaca el-patch)

(defun ra/keymap-set (keymap &rest pairs)
  "Bind multiple pairs of KEY/DEFINITION to KEYMAP using `keymap-set'.

\(fn KEYMAP [KEY DEFINITION]...)"
  (declare (indent defun))
  (unless (zerop (mod (length pairs) 2))
    (error "PAIRS must be pair of KEY/DEFINITION"))
  (seq-map (lambda (pair)
			 (keymap-set keymap (car pair) (cadr pair)))
		   (seq-split pairs 2)))

(defmacro ra/cmd (&rest body)
  "Shorthand for (lambda () (interactive) ,@body)

Taken from https://github.com/doomemacs/doomemacs/blob/844a82c4a0cacbb5a1aa558c88675ba1a9ee80a3/lisp/doom-lib.el#L521"
  (declare (doc-string 1) (pure t) (side-effect-free t))
  `(lambda (&rest _) (interactive) ,@body))

(defalias 'cmd! #'ra/cmd
  "Aliases for `ra/cmd'. Enables short cmd! command inspired from doomemacs.
It's so that if ! is not emacs-lisp friendly anymore, we can just swap for the namespaced variant.")

(defun ra/fbound-and-true-p (fn &rest args)
  "Return the result of FN called with ARGS."
  (and (fboundp fn) (apply fn args)))

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
		  tab-bar-show nil
		  tabspaces-initialize-project-with-todo nil)

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

(elpaca imenu-list)

(elpaca (treesit-fold :host github :repo "emacs-tree-sitter/treesit-fold"))

(require '+completion)

(require '+intellisense)

;; (elpaca fancy-compilation
;;   (fancy-compilation-mode 1)
;;   (setopt fancy-compilation-override-colors nil
;; 		  fancy-compilation-term "tmux-256color"))

;; (elpaca xterm-color
;;   (setopt compilation-environment '("TERM=xterm-256color"))
;;
;;   (defun ra/advice-compilation-filter (f proc string)
;; 	(funcall f proc (xterm-color-filter string)))
;;
;;   (advice-add 'compilation-filter :around #'ra/advice-compilation-filter))

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

;; Download file using emacs
;; (defun ra/download-file ()
;;   ""
;;   (interactive (let ((the-file-url (read-string "Test: ")))
;; 				 (read-string "Test2: " nil nil (parse the-file-url)))))

;; View mode
;; Enable view-mode on read-only buffer
(setopt view-read-only t)

;; (electric-pair-mode 1)
;; (show-paren-mode 1)
(elpaca smartparens
  (smartparens-global-mode 1)
  ;; still debating this between show-paren-mode
  (show-smartparens-global-mode 1)
  (setopt sp-highlight-pair-overlay nil
		  sp-highlight-wrap-overlay nil
		  sp-highlight-wrap-tag-overlay nil
		  ;; Value taken from doomemacs
		  sp-max-pair-length 10
		  sp-max-prefix-length 100)

  (with-eval-after-load 'smartparens
	(require 'smartparens-config)

	;; Snatched from doomemacs, remove harmless message
	(dolist (key '(:unmatched-expression :no-matching-tag))
      (setf (alist-get key sp-message-alist) nil))

	(add-hook 'eval-expression-minibuffer-setup-hook (defun ra/smartparens-eval-expression ()
													   "Enable smartparens in minibuffer"
													   (when smartparens-global-mode
														 (smartparens-mode 1))))

	;; Disable pairs that don't work well with lisp on minibuffer
	(sp-local-pair '(minibuffer-mode minibuffer-inactive-mode) "'" nil :actions nil)
	(sp-local-pair '(minibuffer-mode minibuffer-inactive-mode) "`" nil :actions nil)))


(elpaca outshine
  (setopt outline-minor-mode-prefix "\C-c o"))

(when (executable-find "asdf")
  (elpaca (asdf :host github :repo "tabfugnic/asdf.el")
	(require 'asdf)
	(asdf-enable)))

(elpaca anzu
  (global-anzu-mode 1)
  (ra/keymap-set (current-global-map)
    "<remap> <query-replace>" #'anzu-query-replace
    "<remap> <query-replace-regexp>" #'anzu-query-replace-regexp)
  (ra/keymap-set isearch-mode-map
	"<remap> <isearch-query-replace>" #'anzu-isearch-query-replace
    "<remap> <isearch-query-replace-regexp>" #'anzu-isearch-query-replace-regexp))

(with-eval-after-load 're-builder
  (setopt reb-re-syntax 'string))

(elpaca casual-re-builder
  (with-eval-after-load 're-builder
	(ra/keymap-set reb-mode-map "M-r" #'casual-re-builder-tmenu)
	(ra/keymap-set reb-lisp-mode-map "M-r" #'casual-re-builder-tmenu)))

(elpaca casual-calc
  (add-hook 'calc-start-hook (defun ra/setup-calc-map ()
							   "Setup `calc-mode-map' on hook so that it doesn't get overwritten."
							   (ra/keymap-set calc-mode-map "C-o" #'casual-calc-tmenu))))

(elpaca ialign
  (autoload 'ialign "ialign")
  (ra/keymap-set (current-global-map)
	"C-c |" #'ialign))

(elpaca substitute
  (require 'substitute)
  (setopt substitute-post-replace-functions #'substitute-report-operation)

  (defun ra/rename-symbol ()
	"Run rename symol based on context. Use `eglot-rename' when eglot is on,
otherwise, use `substitute-target-in-buffer'"
	(interactive)
	(call-interactively
	 (cond ((and (fboundp 'eglot-managed-p) (eglot-managed-p)) #'eglot-rename)
		   ((bound-and-true-p lsp-managed-mode) #'lsp-rename)
		   (t #'substitute-target-in-buffer))))

  (defvar-keymap ra/substitute-map
	:doc "Keymap for substitute package"
	:prefix 'ra/substitute-map
	"," #'ra/rename-symbol
	"C-," #'ra/rename-symbol
	"n" #'substitute-target-below-point
	"p" #'substitute-target-above-point
	"d" #'substitute-target-in-defun
	"b" #'substitute-target-in-buffer))

(elpaca kurecolor)
(elpaca rainbow-mode)

(elpaca deadgrep)

(elpaca expand-region)
(elpaca multiple-cursors
  (with-eval-after-load 'multiple-cursors
	(ra/keymap-set mc/keymap
	  "C->" #'mc/mark-next-like-this
	  "C-<" #'mc/mark-prev-like-this)))
;; (elpaca kmacro-x
;;   (ra/keymap-set (current-global-map)
;; 	"C->" #'kmacro-x-mc-mark-next
;; 	"C-<" #'kmacro-x-mc-mark-previous))
(elpaca iedit
  (defun ra/iedit-down-to-occurrence ()
	"Start `iedit-mode' but only mark current and next occurrence."
	(interactive)
	(unless (bound-and-true-p iedit-mode)
	  (iedit-mode)
	  (iedit-restrict-current-line))
	(iedit-expand-down-to-occurrence))

  (defun ra/iedit-up-to-occurrence ()
	"Start `iedit-mode' but only mark current and previous occurrence."
	(interactive)
	(unless (bound-and-true-p iedit-mode)
	  (iedit-mode)
	  (iedit-restrict-current-line))
	(iedit-expand-up-to-occurrence))

  (ra/keymap-set (current-global-map)
	"C-," #'iedit-mode
	"C->" #'ra/iedit-down-to-occurrence
	"C-<" #'ra/iedit-up-to-occurrence)

  (with-eval-after-load 'isearch
	(ra/keymap-set isearch-mode-map
	  "C-," #'iedit-mode-from-isearch))

  (with-eval-after-load 'iedit-mode
	(with-eval-after-load '+mappings
	  (defun ra/iedit-quit ()
		"Function to quit `iedit-mode'."
		(when iedit-mode
		  (save-excursion
			(deactivate-mark)
			(iedit-mode))
		  t))
	  (add-hook 'ra/keyboard-quit-hook #'ra/iedit-quit 80))))
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

(setopt isearch-lazy-count t
		isearch-repeat-on-direction-change t
		isearch-allow-motion t
		isearch-allow-scroll 'unlimited
		isearch-motion-changes-direction t
		isearch-wrap-pause 'no-ding
		isearch-yank-on-move nil
		search-exit-option nil)

;; See https://karthinks.com/software/avy-can-do-anything
(defun ra/isearch-forward-other-window (prefix)
  "Function to isearch-forward in other-window."
  (interactive "P")
  (unless (one-window-p)
    (save-excursion
      (let ((next (if prefix -1 1)))
        (other-window next)
        (isearch-forward)
        (other-window (- next))))))

(defun ra/isearch-backward-other-window (prefix)
  "Function to isearch-backward in other-window."
  (interactive "P")
  (unless (one-window-p)
    (save-excursion
      (let ((next (if prefix 1 -1)))
        (other-window next)
        (isearch-backward)
        (other-window (- next))))))

(ra/keymap-set (current-global-map)
  "C-M-s" #'ra/isearch-forward-other-window
  "C-M-r" #'ra/isearch-backward-other-window)

(elpaca avy
  (ra/keymap-set (current-global-map)
	"C-z" #'avy-goto-char-timer
	"M-z" (cmd! (let ((avy-action-oneshot #'avy-action-zap-to-char)
					  (avy-all-windows nil))
				  (call-interactively #'avy-goto-char-timer))))
  (ra/keymap-set isearch-mode-map
	"C-z" #'avy-isearch)

  ;; See https://karthinks.com/software/avy-can-do-anything
  (defun ra/avy-action-kill-whole-line (pt)
	"Avy action to kill whole line at PT."
	(save-excursion
	  (goto-char pt)
	  (kill-whole-line))
	(message "Killed: %s" (current-kill 0))
	(select-window (cdr (ring-ref avy-ring 0)))
	t)

  (defun ra/avy-action-copy-whole-line (pt)
	"Avy action to copy whole line at PT."
	(save-excursion
      (goto-char pt)
      (cl-destructuring-bind (start . end)
          (bounds-of-thing-at-point 'line)
		(copy-region-as-kill start end)))
	(select-window
	 (cdr
      (ring-ref avy-ring 0)))
	t)

  (defun ra/avy-action-yank-whole-line (pt)
	"Avy action to yank whole line at PT."
	(ra/avy-action-copy-whole-line pt)
	(save-excursion (yank))
	t)

  (defun ra/avy-action-teleport-whole-line (pt)
	"Avy action to teleport or transpose whole line from PT."
    (ra/avy-action-kill-whole-line pt)
    (save-excursion (yank)) t)

  (defun ra/avy-action-mark-to-char (pt)
	"Avy action to mark from `point' to PT."
	(activate-mark)
	(goto-char pt))

  (defun ra/avy-action-embark (pt)
	"Avy action to run `embark-act' on PT."
	(goto-char pt)
    (embark-act))

  (defun ra/avy-action-embark-dwim (pt)
	"Avy action to run `embark-dwim' on PT."
	(goto-char pt)
    (embark-dwim))

  (setopt avy-keys '(?u ?e ?o ?a ?p ?g ?h ?n ?s)
		  avy-dispatch-alist '((?k . avy-action-kill-stay)
							   (?K . ra/avy-action-kill-whole-line)
							   (?w . avy-action-copy)
							   (?W . ra/avy-action-copy-whole-line)
							   (?y . avy-action-yank)
							   (?Y . ra/avy-action-yank-whole-line)
							   (?t . avy-action-teleport)
							   (?T . ra/avy-action-teleport-whole-line)
							   (?m . avy-action-mark)
							   (?  . ra/avy-action-mark-to-char)
							   (?. . ra/avy-action-embark)
							   (?, . ra/avy-action-embark-dwim)
							   (?i . avy-action-ispell)
							   (?z . avy-action-zap-to-char))))

(elpaca separedit
  (ra/keymap-set (current-global-map)
	"C-c '" #'separedit)
  (setopt separedit-preserve-string-indentation t
		  separedit-continue-fill-column t
		  separedit-write-file-when-execute-save t))

(elpaca golden-ratio-scroll-screen
  (ra/keymap-set (current-global-map)
	"<remap> <scroll-up-command>" #'golden-ratio-scroll-screen-up
	"<remap> <scroll-down-command>" #'golden-ratio-scroll-screen-down)
  (setopt golden-ratio-scroll-highlight-flag nil
		  golden-ratio-scroll-recenter nil)

  (defun ra/scroll-all-golden-scroll-down ()
	"scroll down in all buffers using `golden-ratio-scroll-screen-down'"
	(interactive)
	(scroll-all-function-all #'golden-ratio-scroll-screen-down nil))

  (defun ra/scroll-all-golden-scroll-up ()
	"scroll down in all buffers using `golden-ratio-scroll-screen-up'"
	(interactive)
	(scroll-all-function-all #'golden-ratio-scroll-screen-up nil))

  (define-advice scroll-all-check-to-scroll (:around (orig-fn &rest _) golden-scroll)
	"Enable golden-scroll on `scroll-all-mode'"
	(let ((golden-ratio-scroll-recenter nil))
	  (cond ((eq this-command 'golden-ratio-scroll-screen-up)
			 (call-interactively #'ra/scroll-all-golden-scroll-up))
			((eq this-command 'golden-ratio-scroll-screen-down)
			 (call-interactively #'ra/scroll-all-golden-scroll-down))
			(t (funcall orig-fn)))))) ; Pass through to call orig-fn

(elpaca clipetty
  (global-clipetty-mode))

(elpaca which-key
  (setopt which-key-show-early-on-C-h t
		  which-key-idle-secondary-delay 0.01)
  (which-key-mode 1))

(elpaca hydra)

(elpaca string-inflection
  (with-eval-after-load 'embark
    (keymap-set embark-identifier-map "~" #'ra/transient-string-inflection))

  (with-eval-after-load 'string-inflection
	(require 'transient)
	(transient-define-prefix ra/transient-string-inflection ()
	  "Transient menu for running string-inflection"
	  ["String Inflection"
	   [("s" "snake_case" string-inflection-underscore)
		("k" "kebab-case" string-inflection-kebab-case)]
	   [("p" "PascalCase" string-inflection-camelcase)
		("m" "mixedCase" string-inflection-lower-camelcase)]
	   [("u" "UPPER_CAMEL_CASE" string-inflection-upcase)
		("c" "Capital_Camel_Case" string-inflection-capital-underscore)]])))

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
	  (if (and start
			   end
			   (let ((point<mark (< (point) (mark)))
					 check)
				 (when (not point<mark) (exchange-point-and-mark))
				 (setq check (bolp))
				 (exchange-point-and-mark)
				 (and check
					  (eolp)
					  (not (bolp)))))
		  (progn
			(forward-char 1)
			(list start (1+ end) n))
		(list start end n))))

  (define-advice move-text-up (:filter-args (args) fix-region-mark-eol)
	(ra/move-text-fix-eol args))
  (define-advice move-text-down (:filter-args (args) fix-region-mark-eol)
	(ra/move-text-fix-eol args)))

(elpaca multi-line
  (defvar ra/multi-line-highlighting nil
	"Saves the state of `multi-line-highlight-current-candidates'")
  (define-advice multi-line-highlight-current-candidate (:after (&rest _) setq-highlight-var)
	"Set `ra/multi-line-highlighting' variable so that we know if it's currently active or not"
	(setq ra/multi-line-highlighting t))
  (define-advice multi-line-clear-highlights (:after (&rest _) setq-highlight-var)
	"Set `ra/multi-line-highlighting' variable so that we know if it's currently active or not"
	(setq ra/multi-line-highlighting nil))
  (add-hook 'ra/keyboard-quit-hook #'multi-line-clear-highlights))

(elpaca (envrc :host github :repo "purcell/envrc")
  (setopt envrc-remote t)
  (envrc-global-mode 1))

(elpaca makefile-executor
  (add-hook 'makefile-mode-hook #'makefile-executor-mode))

(elpaca num3-mode
  (custom-set-faces '(num3-face-even ((t :underline t)))
					'(num3-face-odd ((t :underline nil))))
  (add-hook 'calc-start-hook #'num3-mode))

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

(elpaca (mediator :host github :repo "dalanicolai/mediator")
  (with-eval-after-load 'embark
	(ra/keymap-set embark-file-map
	  "O" #'mediator-open-file)))

;; Shell-command extensions
(elpaca shell-command+
  ;; TODO add directory changing for C-u on shell-command+
  ;; TODO integrate `shell-command+' with `dwim-shell-command'
  (ra/keymap-set (current-global-map)
	"M-!" #'shell-command+)

  (setopt shell-command+-prompt "Shell command+ in `%s': "))

(require '+apps)

(require '+toolbox)

(with-eval-after-load 'info
  (add-to-list 'Info-default-directory-list (file-truename "~/books")))

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

(require '+mappings)

(repeat-mode 1)

(elpaca gcmh
  (setopt gcmh-high-cons-threshold (* 500 1000 1000) ; 500MB
		  gcmh-low-cons-threshold (* 16 1000 1000)	 ; 16MB
		  gcmh-idle-delay 5
		  gc-cons-percentage 0.2)
  (gcmh-mode))

;;; init.el ends here
