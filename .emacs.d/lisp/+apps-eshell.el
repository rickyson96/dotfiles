;;; +apps-eshell.el --- Eshell configuration         -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Ricky Anderson

;; Author: Ricky Anderson <randerson@thinky>
;; Keywords: local

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(add-hook 'eshell-exit-hook #'quit-window)
(setopt eshell-modules-list '( eshell-alias eshell-banner
                               eshell-basic eshell-cmpl
                               eshell-dirs eshell-elecslash
                               eshell-extpipe eshell-glob
                               eshell-hist eshell-ls
                               eshell-pred eshell-rebind
                               eshell-script ; eshell-smart
                               eshell-tramp eshell-unix
                               eshell-xtra eshell-prompt)
        eshell-where-to-jump 'after
        eshell-review-quick-commands 'not-even-short-output
        eshell-smart-space-goes-to-end t
        eshell-history-size 2048
        eshell-hist-ignoredups t)

(with-eval-after-load 'consult
  (ra/keymap-set eshell-mode-map
    "C-r" #'consult-history))

(elpaca eshell-syntax-highlighting
  (eshell-syntax-highlighting-global-mode 1))

(elpaca eshell-did-you-mean
  (eshell-did-you-mean-setup))

(elpaca eshell-info-banner
  (require 'eshell-info-banner)
  (add-hook 'eshell-banner-load-hook 'eshell-info-banner-update-banner))

(elpaca eshell-fringe-status
  ;; (add-hook 'eshell-mode-hook #'eshell-fringe-status-mode)
  )

(elpaca esh-help
  (setup-esh-help-eldoc))

;; Completions
;; (elpaca bash-completion)

(elpaca fish-completion
  (setopt fish-completion-fallback-on-bash-p nil)
  (autoload #'global-fish-completion-mode "fish-completion")
  (global-fish-completion-mode 1))

;; (elpaca eshell-atuin
;;   (setopt eshell-atuin-history-format "%->110c %-<50i %t + %d (%r ago) - %e"
;;           eshell-atuin-search-fields '(time duration command relativetime directory exit))
;;   (eshell-atuin-mode 1)
;;   (with-eval-after-load 'em-hist
;;     (ra/keymap-set eshell-hist-mode-map
;;       "<remap> <eshell-previous-matching-input>"  #'eshell-atuin-history)))

;; Prompts

(elpaca eshell-prompt-extras
  (autoload 'epe-theme-multiline-with-status "eshell-prompt-extras")
  ;; (with-eval-after-load 'esh-opt
  ;;   (setopt eshell-highlight-prompt t
  ;;           eshell-prompt-function 'epe-theme-multiline-with-status))
  )

(elpaca eshell-git-prompt
  (eshell-git-prompt-use-theme "multiline2")
  (ef-themes-with-colors
    (mapc (lambda (x)
            (set-face-attribute (car x) nil :foreground (cdr x)))
          `((eshell-git-prompt-multiline2-dir-face . ,fg-main)
            (eshell-git-prompt-multiline2-git-face . ,red)
            (eshell-git-prompt-multiline2-usr-face . ,blue)
            (eshell-git-prompt-multiline2-fail-face . ,red-warmer)
            (eshell-git-prompt-multiline2-host-face . ,green)
            (eshell-git-prompt-multiline2-command-face . ,magenta-cooler)
            (eshell-git-prompt-multiline2-secondary-face . ,cyan-cooler)))))

;; Eshell meow integration
(with-eval-after-load 'meow
  (setopt eshell-rebind-keys-alist '(([(control ?d)] . eshell-delchar-or-maybe-eof)
                                     ([backspace] . eshell-delete-backward-char)
                                     ([delete] . eshell-delete-backward-char)
                                     ([(control ?w)] . backward-kill-word)
                                     ([(control ?u)] . eshell-kill-input)
                                     ([(control ?r)] . consult-history)))
  (setopt eshell-hist-rebind-keys-alist '(([(control ?p)]   . eshell-previous-input)
                                          ([(control ?n)]   . eshell-next-input)
                                          ([(control up)]   . eshell-previous-input)
                                          ([(control down)] . eshell-next-input)
                                          ([(control ?r)]   . consult-history)
                                          ([(control ?s)]   . consult-history)
                                          ([(meta ?r)]      . eshell-atuin-history)
                                          ([(meta ?s)]      . eshell-next-matching-input)
                                          ([(meta ?p)]      . eshell-previous-matching-input-from-input)
                                          ([(meta ?n)]      . eshell-next-matching-input-from-input)
                                          ([up]             . eshell-previous-matching-input-from-input)
                                          ([down]           . eshell-next-matching-input-from-input)))
  (add-hook 'meow-insert-enter-hook (lambda () (when (eq major-mode 'eshell-mode)
                                                 (eshell-lock-local-map nil))))
  (add-hook 'meow-insert-exit-hook (lambda () (when (eq major-mode 'eshell-mode)
                                                (eshell-lock-local-map t))))

  (defun ra/eat-eshell-meow-insert-enter-hook ()
    (when (eq major-mode 'eshell-mode)
      (eat-eshell-semi-char-mode)
      (when buffer-read-only
        (setq buffer-read-only nil))))
  (defun ra/eat-eshell-meow-insert-exit-hook ()
    (when (eq major-mode 'eshell-mode)
      (eat-eshell-emacs-mode)))

  (defun ra/eat-eshell-toggle-mode-enable ()
    (add-hook 'meow-insert-enter-hook #'ra/eat-eshell-meow-insert-enter-hook)
    (add-hook 'meow-insert-exit-hook #'ra/eat-eshell-meow-insert-exit-hook))
  (defun ra/eat-eshell-toggle-mode-disable ()
    (remove-hook 'meow-insert-enter-hook #'ra/eat-eshell-meow-insert-enter-hook)
    (remove-hook 'meow-insert-exit-hook #'ra/eat-eshell-meow-insert-exit-hook))

  ;; TODO use same keybind for `eat-eshell-emacs-mode' and `eshell-lock-local-map'
  (define-advice eat-eshell-emacs-mode (:after (&rest _) disable-lock-local-map)
    "Disable `eshell-lock-local-map'."
    (let ((inhibit-message t))
      (eshell-lock-local-map nil)))

  (defun ra/enable-lock-local-map ()
    "Enable `eshell-lock-local-map'."
    (let ((inhibit-message t))
      (eshell-lock-local-map t)))

  (add-hook 'eat--eshell-semi-char-mode-hook #'ra/enable-lock-local-map)
  (add-hook 'eat--eshell-char-mode-hook #'ra/enable-lock-local-map)

  (add-hook 'eat-eshell-exec-hook #'ra/eat-eshell-toggle-mode-enable)
  (add-hook 'eat-eshell-exit-hook #'ra/eat-eshell-toggle-mode-disable)
  (add-hook 'eat-eshell-exit-hook (lambda ()
                                    (when buffer-read-only
                                      (setq buffer-read-only nil))))

  (add-to-list 'meow-mode-state-list '(eshell-mode . insert))
  (add-hook 'eshell-first-time-mode-hook (lambda ()
                                           (let ((inhibit-message t))
                                             (eshell-lock-local-map nil)))))

;; TODO: create starship eshell integration
(defun ra/eshell-starship ()
  )

;; Eshell alias
(defun eshell/dired (&optional dir)
  (dired (or dir default-directory)))
(defalias 'eshell/d 'eshell/dired)

;; Mappings
(with-eval-after-load 'eshell
(keymap-unset 'eshell-command-map "C-w" t)
(keymap-unset eshell-mode-map "C-w" t))

(provide '+apps-eshell)
;;; +apps-eshell.el ends here
