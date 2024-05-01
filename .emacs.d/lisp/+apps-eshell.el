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
                               eshell-pred ; eshell-rebind
                               eshell-script eshell-smart
                               eshell-tramp eshell-unix
                               eshell-xtra eshell-prompt)
        eshell-where-to-jump 'after
        eshell-review-quick-commands 'not-even-short-output
        eshell-smart-space-goes-to-end t)

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
  (add-hook 'eshell-mode-hook #'eshell-fringe-status-mode))

(elpaca esh-help
  (setup-esh-help-eldoc))

;; Completions
(elpaca bash-completion)

(elpaca fish-completion
  (autoload #'global-fish-completion-mode "fish-completion")
  (global-fish-completion-mode 1))

;; Prompts

(elpaca eshell-prompt-extras
  ;; (autoload 'epe-theme-multiline-with-status "eshell-prompt-extras")
  ;; (with-eval-after-load 'esh-opt
  ;; (setopt eshell-highlight-prompt nil
  ;; eshell-prompt-function 'epe-theme-multiline-with-status))
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

;; TODO: create starship eshell integration
(defun ra/eshell-starship ()
  )

(provide '+apps-eshell)
;;; +apps-eshell.el ends here
