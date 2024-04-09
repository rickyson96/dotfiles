;;; +ui.el --- UI Theming and Configuration           -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Ricky Anderson

;; Author: Ricky Anderson <rickyson@thinky>
;; Keywords: local

;; This file is not part of GNU Emacs

;;; Commentary:

;;

;;; Code:

(setopt cursor-type 'bar
		truncate-lines t)

(line-number-mode 1)
(column-number-mode 1)
(size-indication-mode 1)

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(horizontal-scroll-bar-mode -1)

(elpaca nerd-icons
  (setopt nerd-icons-scale-factor 1
		  nerd-icons-font-family "IosevraRelaxed Nerd Font"))

(elpaca hl-todo
  (hl-todo-mode 1))

(elpaca ef-themes
  (setopt ef-themes-mixed-fonts t
		  ef-themes-variable-pitch-ui t
		  ef-themes-headings ; read the manual's entry or the doc string
		  '((0 variable-pitch 1.5)
			(1 light variable-pitch 1.5)
            (2 regular 1.3)
            (3 1.1)))

  (with-eval-after-load 'org
	(set-face-attribute 'org-quote nil :slant 'italic))

  (defun my-ef-themes-hl-todo-faces ()
	"Configure `hl-todo-keyword-faces' with Ef themes colors.
The exact color values are taken from the active Ef theme.
Taken from: https://protesilaos.com/emacs/ef-themes#h:19c549dc-d13f-45c4-a727-3618591d5c4f"
	(ef-themes-with-colors
      (setopt hl-todo-keyword-faces
			  `(("HOLD" . ,yellow)
				("TODO" . ,red)
				("NEXT" . ,blue)
				("THEM" . ,magenta)
				("PROG" . ,cyan-warmer)
				("OKAY" . ,green-warmer)
				("DONT" . ,yellow-warmer)
				("FAIL" . ,red-warmer)
				("BUG" . ,red-warmer)
				("DONE" . ,green)
				("NOTE" . ,blue-warmer)
				("KLUDGE" . ,cyan)
				("HACK" . ,cyan)
				("TEMP" . ,red)
				("FIXME" . ,red-warmer)
				("XXX+" . ,red-warmer)
				("REVIEW" . ,red)
				("DEPRECATED" . ,yellow-faint)))))

  (add-hook 'ef-themes-post-load-hook #'my-ef-themes-hl-todo-faces)

  (mapc #'disable-theme custom-enabled-themes)
  (ef-themes-select 'ef-cyprus))

(elpaca doom-modeline
  (doom-modeline-mode 1)
  (setopt doom-modeline-height 24
		  doom-modeline-bar-width 4
		  doom-modeline-buffer-file-name-style 'truncate-with-project
		  doom-modeline-indent-info t
		  doom-modeline-total-line-number t
		  doom-modeline-buffer-encoding 'nondefault))

(elpaca spacious-padding
  (setopt spacious-padding-widths '( :internal-border-width 15
									 :header-line-width 4
									 :mode-line-width 3
									 :tab-width 4
									 :right-divider-width 10
									 :scroll-bar-width 8))

  (ra/configure-frame "spacious-padding" (elpaca-after-init-hook server-after-make-frame-hook)
	(spacious-padding-mode 1)))

(elpaca (dired-plus :host github :repo "emacsmirror/dired-plus" :main "dired+.el"))

(elpaca dirvish
  (dirvish-override-dired-mode)
  (dirvish-peek-mode)
  (setopt dirvish-quick-access-entries '(("h" "~/"                          "Home")
										 ("d" "~/Downloads/"                "Downloads")
										 ("m" "/mnt/"                       "Drives")
										 ("t" "~/.local/share/Trash/files/" "TrashCan")
										 (". ." "~/.dotfiles"               "Dotfiles")
										 (". r" "~/.root-dotfiles"          "Root Dotfiles")))
  (setopt dirvish-mode-line-format '(:left (sort symlink) :right (omit yank index))
		  dirvish-header-line-format '(:left (path) :right (vc-info file-user))
		  dirvish-attributes '(nerd-icons file-time file-size collapse subtree-state vc-state git-msg)
		  dired-listing-switches "-l --almost-all --human-readable --group-directories-first --no-group"
		  dirvish-reuse-session nil
		  dirvish-header-line-height 25)

  (ra/keymap-set dirvish-mode-map
	"b" #'dired-up-directory
	"a" #'dirvish-quick-access
	"TAB" #'dirvish-subtree-toggle
	"M-f" #'dirvish-history-go-forward
	"M-b" #'dirvish-history-go-backward
	"M-l" #'dirvish-ls-switches-menu
	"M-m" #'dirvish-mark-menu
	"M-t" #'dirvish-layout-toggle
	"M-s" #'dirvish-setup-menu
	"M-e" #'dirvish-emerge-menu
	"M-j" #'dirvish-fd-jump

	"$" #'eshell
	"M-$" #'eshell

	"y" #'dirvish-yank-menu ; `dired-show-file-type' is accessible from `dirfish-file-info-menu'
	"N" #'dirvish-narrow    ; `dired-do-man' is less used command
	"^" #'dirvish-history-last
	"h" #'dirvish-history-jump ; `describe-mode' can be accessed from `help-map'
	"s" #'dirvish-quicksort    ; supersedes `dired-sort-toggle-or-edit'
	"v" #'dirvish-vc-menu      ; `dired-view-file' is superseded by preview and `dired-find-file'

	;; if no marked files, similar to `dired-find-file'. We can have
	;; both "f" and "F" key on single key, thus freeing one for
	;; `dirvish-file-info-menu'.
	;; Other than that, "RET" is also `dired-find-file'.
	"f" #'dired-do-find-marked-files
	"F" #'dirvish-file-info-menu))

(elpaca diredfl
  (add-hook 'dired-mode-hook #'diredfl-mode)
  (add-hook 'dirvish-directory-view-mode #'diredfl-mode))

(elpaca iscroll
  (add-hook 'text-mode-hook #'iscroll-mode))

(elpaca page-break-lines)

(elpaca dashboard
  (setopt dashboard-display-icons-p t
		  dashboard-icon-type 'nerd-icons
		  dashboard-set-navigator t
		  dashboard-filter-agenda-entry #'dashboard-filter-agenda-by-todo)

  (add-hook 'elpaca-after-init-hook #'dashboard-insert-startupify-lists)
  (add-hook 'elpaca-after-init-hook #'dashboard-initialize)
  (dashboard-setup-startup-hook)

  (dashboard-modify-heading-icons '((recents . "nf-oct-file_text")
									(bookmarks . "nf-oct-book")))


  (ra/keymap-set dashboard-mode-map
	"n" #'dashboard-next-line
	"p" #'dashboard-previous-line)

  (setopt dashboard-center-content t
		  dashboard-vertically-center-content t)

  ;; to work with daemon
  ;; https://github.com/emacs-dashboard/emacs-dashboard?tab=readme-ov-file#emacs-daemon
  (setopt initial-buffer-choice (lambda () (get-buffer-create dashboard-buffer-name))))

(elpaca hide-mode-line)

(provide '+ui)
;;; +ui.el ends here
