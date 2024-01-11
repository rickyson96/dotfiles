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

(elpaca (org-modern-indent :host github :repo "jdtsmith/org-modern-indent")
  (add-hook 'org-mode-hook #'org-modern-indent-mode 90))

(setopt org-catch-invisible-edits 'show-and-error
		org-special-ctrl-a/e t
		org-hide-emphasis-markers t
		org-pretty-entities t
		org-ellipsis "…"
		org-tags-column 0
		org-startup-with-inline-images t
		org-startup-indented t
		org-fontify-quote-and-verse-blocks t
		org-link-abbrev-alist '(("youtube" . "https://youtube.com/watch?v=%s")
								("github" . "https://github.com/%s")))

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
		  org-appear-delay 0.0
		  org-appear-trigger 'manual)

  (add-hook 'org-mode-hook (lambda ()
							 (add-hook 'meow-normal-mode-hook #'org-appear-manual-stop nil t)
							 (add-hook 'meow-insert-mode-hook #'org-appear-manual-start nil t))))

(elpaca org-pdftools
  (add-hook 'org-mode-hook #'org-pdftools-setup-link))

(elpaca (org-yt :host github :repo "TobiasZawada/org-yt")
  (setopt org-yt-cache-directory (no-littering-expand-var-file-name "yt-cache"))
  (with-eval-after-load 'org
	(require 'org-yt)))

(elpaca org-gtd
  (setopt org-gtd-directory "~/org/gtd"
		  org-edna-use-inheritance t
		  org-gtd-update-ack "3.0.0")
  (ra/keymap-set org-gtd-clarify-map
	"C-c c" #'org-gtd-organize)

  (with-eval-after-load 'org
	(org-edna-mode 1)))

(elpaca doct
  (setopt org-capture-templates
		  (doct '(("  Inbox" :keys "i"
				   :file org-gtd-inbox-path
				   :template ("* %^{Title: }"
							  "%U"
							  ""
							  ""
							  "%?"
							  " %i")
				   :kill-buffer t)
				  ("  Inbox with link" :keys "l"
				   :file org-gtd-inbox-path
				   :template ("* %^{Title: }"
							  "%U"
							  ""
							  ""
							  "%?"
							  " %i"
							  " %a")
				   :kill-buffer t)))))

(provide '+lang-org)
;;; +lang-org.el ends here
