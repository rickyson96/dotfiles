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
		org-tags-column 0
		org-startup-with-inline-images t
		org-startup-indented t
		org-fontify-quote-and-verse-blocks t
		org-link-abbrev-alist '(("youtube" . "https://youtube.com/watch?v=%s")
								("github" . "https://github.com/%s")))

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
		  org-gtd-update-ack "3.0.0"

		  org-agenda-files (list org-gtd-directory)
		  org-gtd-engage-prefix-width 20)
  (ra/keymap-set org-gtd-clarify-map
	"C-c c" #'org-gtd-organize)

  (with-eval-after-load 'org
	(org-edna-mode 1)
	(require 'org-gtd)))

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

(elpaca org-super-agenda
  (org-super-agenda-mode 1))

;; Agenda styling
(setopt org-agenda-tags-column 'auto
		org-agenda-block-separator ?─
		org-agenda-time-grid
		'((daily today require-timed)
		  (800 1000 1200 1400 1600 1800 2000)
		  " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
		org-agenda-current-time-string
		"◀── now ─────────────────────────────────────────────────"
		org-agenda-window-setup 'only-window
		org-agenda-restore-windows-after-quit t)

(setopt org-agenda-custom-commands
		'(("u" "My GTD Agenda"
		   ((agenda "" ((org-agenda-span 'day)
						(org-agenda-breadcrumbs-separator " ❱ ")
						(org-agenda-current-time-string "ᐊ┈┈┈┈┈┈┈┈┈┈┈┈┈┈ Now")
						(org-agenda-time-grid '((today require-timed remove-match)
												(800 1000 1200 1400 1600 1800 2000 2200)
												"      " "┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈"))
						;; (org-agenda-prefix-format "  %-3i  %-15b%t  %s")
						(org-agenda-prefix-format "   %i %?-2 t%s")
						(org-agenda-start-day "+0")
						;; (org-agenda-prefix-format " %i %-12:c%?-12t% s")
						(org-agenda-overriding-header "󰃭 Calendar")
						(org-agenda-files '("~/org" "~/org/gtd"))
						(org-super-agenda-groups '((:time-grid t)))))
			(todo "NEXT" ((org-agenda-overriding-header "⚡ Next Action")
						  ;; (org-agenda-prefix-format " %i %-30:(my/org-gtd-agenda-prefix-format 30) ")
						  (org-agenda-prefix-format '((todo . " %i %-12:(org-gtd-agenda--prefix-format)")))
						  ;; (org-agenda-prefix-format " %i %-12:(org-gtd--agenda-prefix-format)")
						  (org-agenda-files `(,org-gtd-directory))))
			(todo "WAIT" ((org-agenda-overriding-header " Delegated / Blocked")
						  (org-agenda-todo-ignore-with-date t)
						  (org-agenda-prefix-format " %i %-12:(org-gtd--agenda-prefix-format)")
						  (org-agenda-files `(,org-gtd-directory))))))))

(provide '+lang-org)
;;; +lang-org.el ends here
