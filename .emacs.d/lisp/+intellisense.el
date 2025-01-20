;;; +intellisense.el --- Intelligent in-buffer code-completions  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Ricky Anderson

;; Author: Ricky Anderson <rickyson@thinky>
;; Keywords: local

;; This file is not part of GNU Emacs

;;; Commentary:

;;

;;; Code:

(setopt completion-ignore-case t
        read-file-name-completion-ignore-case t
        read-buffer-completion-ignore-case t

        xref-prompt-for-identifier '(not xref-find-definitions
                                         xref-find-definitions-other-window
                                         xref-find-definitions-other-frame
                                         xref-find-references))

(elpaca corfu
  (global-corfu-mode 1)
  (corfu-popupinfo-mode)
  (setopt tab-always-indent 'complete
          corfu-cycle t
          corfu-popupinfo-delay (cons nil 0.3)))

(elpaca cape)

(elpaca nerd-icons-corfu
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(elpaca capf-autosuggest
  (setopt capf-autosuggest-dwim-next-line nil
          capf-autosuggest-capf-functions '(capf-autosuggest-history-capf))
  (add-hook 'eshell-mode-hook #'capf-autosuggest-mode)
  (add-hook 'comint-mode-hook #'capf-autosuggest-mode))

(elpaca corfu-terminal
  (unless (display-graphic-p)
    (corfu-terminal-mode +1)))

(setopt eldoc-documentation-strategy #'eldoc-documentation-default
        ;; Set eldoc to only show single line on minibuffer
        ;; I use eldoc mainly to show function signature.
        ;; In depth documentation should only be show when requested.
        eldoc-echo-area-use-multiline-p 1
        eldoc-echo-area-display-truncation-message nil)

(elpaca lsp-mode
  (setopt lsp-idle-delay 0.3
          lsp-completion-provider :none
          lsp-keymap-prefix "C-c l"
          lsp-diagnostics-provider t
          lsp-inlay-hint-enable t
          lsp-enable-suggest-server-download nil
          lsp-eldoc-render-all t
          lsp-enable-on-type-formatting nil ; It often formats when it doesn't expected. e.g.: Remove recently inserted trailing comma
          lsp-headerline-breadcrumb-enable nil)

  (with-eval-after-load 'lsp-mode
    (ra/keymap-set lsp-mode-map
      "<remap> <display-local-help>" #'eldoc))

  (with-eval-after-load 'lsp-mode
    (set-face-attribute 'lsp-face-highlight-read nil :inherit 'bold :underline t))

  (defun ra/lsp-mode-setup-completion ()
    "Taken from corfu's wiki"
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless))
    (add-hook 'completion-at-point-functions #'lsp-completion-at-point -70 t))

  (add-hook 'lsp-completion-mode-hook #'ra/lsp-mode-setup-completion)
  (add-hook 'lsp-managed-mode-hook #'ra/tempel-setup-capf)
  (add-hook 'lsp-managed-mode-hook #'ra/tiny-setup-capf)

  ;; LSP Booster
  (defun lsp-booster--advice-json-parse (old-fn &rest args)
    "Try to parse bytecode instead of json."
    (or
     (when (equal (following-char) ?#)
       (let ((bytecode (read (current-buffer))))
         (when (byte-code-function-p bytecode)
           (funcall bytecode))))
     (apply old-fn args)))
  (advice-add (if (progn (require 'json)
                         (fboundp 'json-parse-buffer))
                  'json-parse-buffer
                'json-read)
              :around
              #'lsp-booster--advice-json-parse)

  (defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
    "Prepend emacs-lsp-booster command to lsp CMD."
    (let ((orig-result (funcall old-fn cmd test?)))
      (if (and (not test?)                             ;; for check lsp-server-present?
               (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
               lsp-use-plists
               (not (functionp 'json-rpc-connection))  ;; native json-rpc
               (executable-find "emacs-lsp-booster"))
          (progn
            (message "Using emacs-lsp-booster for %s!" orig-result)
            (cons "emacs-lsp-booster" orig-result))
        orig-result)))
  (advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command))

(elpaca consult-lsp)

(with-eval-after-load 'eglot
  (add-hook 'eglot-managed-mode-hook #'ra/tempel-setup-capf)
  (add-hook 'eglot-managed-mode-hook #'ra/tiny-setup-capf)
  (set-face-attribute 'eglot-highlight-symbol-face nil :underline t))

(elpaca (eglot-booster :host github :repo "jdtsmith/eglot-booster")
  (with-eval-after-load 'eglot
    (eglot-booster-mode 1)))

(cl-defun apheleia-lsp-organize-import-formatter (&key buffer scratch callback stdin &allow-other-keys)
  (with-current-buffer buffer
    (if-let ((_ (lsp-feature? "textDocument/codeAction"))
             (action (->> (lsp-get-or-calculate-code-actions "source.organizeImports")
                          lsp--select-action)))
        (-let* ((workspace-edit (cadr action))
                ((&WorkspaceEdit :document-changes?) workspace-edit)
                (document-changes (seq-reverse document-changes?)))
          (lsp--check-document-changes-version document-changes)
          (->> document-changes
               (seq-do (lambda (change)
                         (with-current-buffer scratch
                           (lsp--apply-text-edits (lsp:text-document-edit-edits change) 'code-action)))))))
    (funcall callback)))

(defun apheleia-lsp-formatter-buffer (buffer scratch)
  (with-current-buffer buffer
    (if (lsp-feature? "textDocument/formatting")
        (let ((edits (lsp-request
                      "textDocument/formatting"
                      (lsp--make-document-formatting-params))))
          (unless (seq-empty-p edits)
            (with-current-buffer scratch
              (lsp--apply-text-edits edits 'format)))))))

(cl-defun apheleia-lsp-formatter
    (&key buffer scratch formatter callback &allow-other-keys)
  (apheleia-lsp-formatter-buffer buffer scratch)
  (funcall callback))

(cl-defun apheleia-pyflakes
    (&key buffer scratch formatter callback &allow-other-keys)
  (let ((inhibit-message t))
    (with-current-buffer scratch
      (ignore-errors
       (python-fix-imports))))
  (funcall callback))

(elpaca apheleia
  ;; Apheleia LSP
  (with-eval-after-load 'apheleia-formatters
    (mapc (lambda (x)
            (add-to-list 'apheleia-formatters x))
          '((eslint . ("apheleia-npx" "eslint" "--fix" file))
            (eslintd . ("eslint_d" "--fix-to-stdout" "--stdin" "--stdin-filename" filepath))
            (lsp-format . apheleia-lsp-formatter)
            (lsp-organize . apheleia-lsp-organize-import-formatter)
            (pyflakes . apheleia-pyflakes)))

    (setf (alist-get 'typescript-ts-mode apheleia-mode-alist) '(lsp-organize eslintd))
    (setf (alist-get 'python-ts-mode apheleia-mode-alist) '(pyflakes black isort))))

(elpaca tempel
  (defun ra/tempel-setup-capf ()
    "Setup capf to try tempel first
Taken from tempel's readme"
    (add-hook 'completion-at-point-functions #'tempel-expand -80 t))
  (ra/tempel-setup-capf)
  (add-hook 'conf-mode-hook #'ra/tempel-setup-capf)
  (add-hook 'prog-mode-hook #'ra/tempel-setup-capf)
  (add-hook 'text-mode-hook #'ra/tempel-setup-capf)

  (defun ra/tempel-insert-template (elt)
    "Tempel elements that expands `t' for another template"
    (when (eq (car-safe elt) 't)
      (if-let (template (alist-get (cadr elt) (tempel--templates)))
          (cons 'l template)
        (message "Template %s not found" (cadr elt))
        nil)))

  (with-eval-after-load 'tempel
    (add-to-list 'tempel-user-elements #'ra/tempel-insert-template)

    (ra/keymap-set tempel-map
      "TAB" #'tempel-next
      "<backtab>" #'tempel-previous
      "M-n" #'tempel-next
      "M-p" #'tempel-previous
      "C-g" #'tempel-abort)))

(elpaca tempel-collection)

(elpaca (lsp-snippet :host github :repo "svaante/lsp-snippet")
  (when (featurep 'lsp-mode)
    (lsp-snippet-tempel-lsp-mode-init)))

(elpaca aggressive-indent
  (global-aggressive-indent-mode 0))

(elpaca flymake-flycheck
  (add-hook 'flymake-mode-hook 'flymake-flycheck-auto)

  (with-eval-after-load 'flymake-flycheck
    (dolist (checkers '(javascript-eslint))
      (add-to-list 'flycheck-disabled-checkers checkers))))

(elpaca eldoc-box)

(elpaca imenu-extra)

(defmacro ra/imenu-extra-setup-hook (hook imenu-pattern)
  "Add hook function to HOOK to setup imenu using `imenu-extra-auto-setup'
with IMENU-PATTERN"
  (let* ((hook-name (symbol-name hook))
         (name (format "%s@%s" "imenu-extra" hook-name))
         (sym (intern name)))
    (macroexp-progn `((defun ,sym ()
                        ,(format "Set up imenu-extra on %s" hook-name)
                        (imenu-extra-auto-setup ,imenu-pattern))
                      (add-hook ',hook #',sym)))))

(elpaca (copilot :host github :repo "copilot-emacs/copilot.el")
  ;; (add-hook 'prog-mode-hook #'copilot-mode)
  (with-eval-after-load 'copilot
    (ra/keymap-set copilot-completion-map
      "C-g" #'copilot-clear-overlay
      "M-p" #'copilot-previous-completion
      "M-n" #'copilot-next-completion
      "C-e" #'copilot-accept-completion-by-line
      "M-f" #'copilot-accept-completion-by-word
      "C-<return>" #'copilot-accept-completion)))

(elpaca (openai :host github :repo "emacs-openai/openai")

  (defun ra/openai-key-auth-source (&optional base-url)
    "Retrieve the OpenAI API key from auth-source given a BASE-URL.
If BASE-URL is not specified, it defaults to `openai-base-url'."
    (if-let ((auth-info (auth-source-search :max 1
                                            :host (url-host (url-generic-parse-url (or nil openai-base-url)))
                                            :require '(:secret))))
        (funcall (plist-get (car auth-info) :secret))
      (error "OpenAI API key not found in auth-source")))

  (setopt openai-key #'ra/openai-key-auth-source
          openai-user "ricky.anderson2696@gmail.com"))
(elpaca (chatgpt :host github :repo "emacs-openai/chatgpt"))
(elpaca (codegpt :host github :repo "emacs-openai/codegpt"))

;; Tabby
;; (elpaca (tabby :host github :files ("*.el" "node_scripts") :repo "alan-w-255/tabby.el")
;;   (ra/keymap-set tabby-mode-map
;;     "C-<return>" #'tabby-accept-completion))

(elpaca tiny
  (defun ra/tiny-setup-capf ()
    "Setup capf to try tiny first "
    (add-hook 'completion-at-point-functions #'tiny-expand -90 t))
  (ra/tiny-setup-capf))

(elpaca sideline
  (setopt sideline-order-left 'up
          sideline-order-right 'down))

(elpaca sideline-lsp)
(elpaca sideline-flymake
  (setopt sideline-flymake-display-mode 'point
          sideline-flymake-show-backend-name nil))
(elpaca (sideline-load-cost :host github :repo "emacs-sideline/sideline-load-cost"))

(setopt sideline-backends-left '(sideline-lsp))
(setopt sideline-backends-right '(sideline-flymake sideline-load-cost))

;; (defun ra/compile-multi-parse-package-json ()
;;   "Parse package.json file and format it to use with `compile-multi-config'."
;;   (let* ((json-object-type 'alist)
;;       (json-array-type 'list)
;;       (default-directory (project-root (project-current)))
;;       (buf (find-file-noselect "package.json"))
;;       (content (with-current-buffer buf
;;                  (beginning-of-buffer)
;;                  (json-read)))
;;       (scripts (alist-get 'scripts content)))
;;  (mapcar (lambda (x)
;;            (cons (format "yarn:%s" (car x)) (cdr x)))
;;          scripts)))

(elpaca compile-multi
  (setopt compile-multi-config `(((file-exists-p ".stowrc")
                                  ("stow:stow" . ("stow" ".")))))
  (setopt compile-multi-default-directory (lambda () (project-root (project-current)))
          compile-multi-annotate-limit 120))
(elpaca consult-compile-multi
  (with-eval-after-load 'consult
    (consult-compile-multi-mode 1)))
(elpaca compile-multi-embark
  (compile-multi-embark-mode 1))

(elpaca projection
  (ra/keymap-set ctl-x-map
    "P" projection-map))
(elpaca projection-multi
  (with-eval-after-load 'project
    (ra/keymap-set project-prefix-map
      "c" #'projection-multi-compile
      "g" #'compile-multi-recompile)))
(elpaca projection-multi-embark
  (projection-multi-embark-setup-command-map))

(provide '+intellisense)
;;; +intellisense.el ends here
