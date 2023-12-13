;;; +lang-org.el --- Org-mode Configurations         -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Ricky Anderson

;; Author: Ricky Anderson <rickyson@thinky>
;; Keywords: local

;; This file is not part of GNU Emacs

;;; Commentary:

;;

;;; Code:

(elpaca org-modern
  (with-eval-after-load 'org
	(global-org-modern-mode 1)))

(setopt org-catch-invisible-edits 'show-and-error
		org-special-ctrl-a/e t
		org-hide-emphasis-markers t
		org-pretty-entities t
		org-ellipsis "…"
		org-tags-column 0)

;; Agenda styling
(setopt org-agenda-tags-column 'auto
		org-agenda-block-separator ?─
		org-agenda-time-grid
		'((daily today require-timed)
		  (800 1000 1200 1400 1600 1800 2000)
		  " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
		org-agenda-current-time-string
		"◀── now ─────────────────────────────────────────────────")

(elpaca org-appear
  (add-hook 'org-mode-hook #'org-appear-mode)
  (setopt org-appear-autosubmarkers t
		  org-appear-autokeywords t
		  org-appear-autolinks t
		  org-appear-delay 0.5))

(provide '+lang-org)
;;; +lang-org.el ends here
