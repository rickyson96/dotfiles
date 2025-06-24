;;; +lang-org.el --- Org-mode Configurations         -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Ricky Anderson

;; Author: Ricky Anderson <rickyson@thinky>
;; Keywords: local

;; This file is not part of GNU Emacs

;;; Commentary:

;;

;;; Code:

(elpaca org-modern
  (setopt org-modern-list '((?+ . "•")
                            (?- . "›")
                            (?* . "◦")))
  (with-eval-after-load 'org
    (global-org-modern-mode 1)))

(defun ra/verb-var-link (t)
  (let* ((split (split-string t ":"))
         (name (car split))
         (content (nth 1 split)))
    (if content
        (format "(verb-set-var \"%s\" \"%s\")" name content)
      (format "(verb-set-var \"%s\")" name))))

(setopt org-catch-invisible-edits 'show-and-error
        org-special-ctrl-a/e t
        org-hide-emphasis-markers t
        org-pretty-entities t
        org-ellipsis "…"
        org-tags-column 0
        org-startup-with-inline-images t
        org-startup-indented t
        org-fontify-quote-and-verse-blocks t
        org-use-sub-superscripts '{}
        org-list-demote-modify-bullet '(("-" . "+")
                                        ("+" . "-")
                                        ("*" . "+"))
        org-use-property-inheritance t
        org-edit-src-content-indentation 0
        org-link-abbrev-alist '(("github" . "https://github.com/%s")
                                ("verb-var" . "elisp:%(ra/verb-var-link)")
                                ("youtube" . "https://youtube.com/watch?v=%s")))

(put 'ra/verb-var-link 'org-link-abbrev-safe t)

(with-eval-after-load 'org
  (setopt org-src-lang-modes (append org-src-lang-modes '(("json" . jsonian)
                                                          ("jsx" . tsx-ts)
                                                          ("ts" . typescript-ts)
                                                          ("typescript" . typescript-ts)
                                                          ("tsx" . tsx-ts)))))


(setopt org-babel-load-languages '((emacs-lisp . t)
                                   (js . t)
                                   (shell . t)
                                   (sql . t)))

(with-eval-after-load 'verb
  (add-to-list 'org-babel-load-languages '(verb . t)))

(elpaca org-appear
  (add-hook 'org-mode-hook #'org-appear-mode)
  (setopt org-appear-autosubmarkers t
          org-appear-autokeywords t
          org-appear-autolinks t
          org-appear-delay 0.0
          org-appear-trigger 'on-change)

  (add-hook 'org-mode-hook (lambda ()
                             (add-hook 'meow-normal-mode-hook #'org-appear-manual-stop nil t)
                             (add-hook 'meow-insert-mode-hook #'org-appear-manual-start nil t))))

;; (elpaca org-pdftools
;;   (add-hook 'org-mode-hook #'org-pdftools-setup-link))

(elpaca (org-yt :host github :repo "TobiasZawada/org-yt")
  (setopt org-yt-cache-directory (no-littering-expand-var-file-name "yt-cache"))
  (with-eval-after-load 'org
    (require 'org-yt)))

(setopt org-directory "~/org"
        org-gtd-directory (file-name-concat org-directory "gtd"))

(require 'org-protocol)

(elpaca org-edna
  (setopt org-edna-use-inheritance t)

  (with-eval-after-load 'org
    (org-edna-mode 1)))

(elpaca org-gtd
  (setopt org-gtd-update-ack "3.0.0"

          org-gtd-engage-prefix-width 20)
  (ra/keymap-set org-gtd-clarify-map
    "C-c c" #'org-gtd-organize)
  (with-eval-after-load 'org
    (require 'org-gtd)))

(elpaca alert
  (setopt alert-default-style 'libnotify))

(elpaca (elnode :depth nil))
(elpaca org-gcal
  (require 'password-store)
  (require 'plstore)
  ;; TODO move this to separate repo
  (add-to-list 'plstore-encrypt-to "E8D54E788BB19898860D357F7A564DFD29D70526")
  (let* ((pass-entry (password-store-parse-entry "org-gcal/ricky.anderson2696@gmail.com"))
         (client-id (alist-get "client_id" pass-entry nil nil 'string=))
         (client-secret (alist-get 'secret pass-entry)))
    (setopt org-gcal-client-id client-id
            org-gcal-client-secret client-secret
            org-gcal-fetch-file-alist `(("ricky.anderson2696@gmail.com" . ,(file-name-concat org-directory "schedule/personal.org"))
                                        ("ricky.anderson@xendit.co" . ,(file-name-concat org-directory "schedule/work.org")))
            org-gcal-recurring-events-mode 'top-level
            org-gcal-notify-p nil
            org-gcal-remove-api-cancelled-events t))
  (require 'org-gcal)
  (run-at-time t (* 30 60) #'org-gcal-sync))

(elpaca org-ql)
(elpaca org-alert
  (setopt org-alert-interval (* 10 60)
          org-alert-notify-after-event-cutoff 0
          org-alert-notify-cutoff 10)
  (require 'org-alert)
  (org-alert-enable))

(elpaca doct
  (setopt org-capture-templates
          (doct `(("🖉 Todo" :keys "t"
                   :file ,(file-name-concat org-directory "todos.org")
                   :template ("* TODO %?"
                              "%U"
                              ""
                              ""
                              " %i")
                   :kill-buffer t)
                  ("󰏪 Work Todo" :keys "w"
                   :file ,(file-name-concat org-directory "todos.org")
                   :template ("* TODO %? :work:"
                              "%U"
                              ""
                              ""
                              " %i")
                   :kill-buffer t)
                  ("  Todo with link" :keys "l"
                   :file ,(file-name-concat org-directory "schedule/private.org")
                   :template ("* TODO %?"
                              "%U"
                              ""
                              ""
                              " %i"
                              " %a")
                   :kill-buffer t)
                  ("🗓 Schedule" :keys "s"
                   :file ,(file-name-concat org-directory "schedule/private.org")
                   :template ("* TODO %^{Title}"
                              "SCHEDULED: %^t"
                              ""
                              "%?"
                              " %i"))
                  ("🗓 Work Schedule" :keys "z"
                   :file ,(file-name-concat org-directory "schedule/private.org")
                   :template ("* TODO %^{Title} :work:"
                              "SCHEDULED: %^t"
                              ""
                              "%?"
                              " %i"))
                  ("🗓 Agenda" :keys "a"
                   :file ,(file-name-concat org-directory "schedule/private.org")
                   :template ("* %^{Title}"
                              "%^T"
                              ""
                              "%?"
                              " %i"))
                  (" Bookmark" :keys "b"
                   :file ,(file-name-concat org-directory "linkmarks.org")
                   :template ("* %^{Title}"
                              "%(with-temp-buffer (org-insert-link '(16) nil \"\") (buffer-string))"
                              "  added: %U"))
                  ("Protocols" :keys "p"
                   :children (("Capture" :keys "c"
                               :file ,(file-name-concat org-directory "todos.org")
                               :template ("* TODO"))
                              ("Linkmarks" :keys "l"
                               :file ,(file-name-concat org-directory "linkmarks.org")
                               :template ("* %:description %?"
                                          "[[%:link]]"
                                          "  added: %U"))
                              ("Linkmarks BSSD" :keys "b"
                               :file ,(file-name-concat org-directory "linkmarks.org")
                               :template ("* Xendit: BSSD %:description %?"
                                          "[[%:link]]"
                                          "  added: %U"))))))))

(elpaca org-super-agenda
  (org-super-agenda-mode 1))

;; Agenda styling
(add-hook 'org-agenda-mode-hook #'hl-line-mode)

(setopt org-agenda-tags-column 'auto
        org-agenda-block-separator ?─
        org-agenda-time-grid
        '((daily today require-timed)
          (800 1000 1200 1400 1600 1800 2000)
          " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
        org-agenda-current-time-string
        "◀── now ─────────────────────────────────────────────────"
        org-agenda-window-setup 'only-window
        org-agenda-restore-windows-after-quit t
        org-link-elisp-confirm-function #'y-or-n-p
        org-log-done 'time
        org-log-reschedule 'note
        org-log-redeadline 'note
        org-log-into-drawer t
        org-return-follows-link t
        org-extend-today-until 4
        org-todo-keywords '((sequence "TODO" "CHECK" "|" "DONE(d!)" "CNCL(c@)"))
        org-refile-targets '(("recurring.org" :maxlevel . 1)
                             (org-agenda-files :maxlevel . 3))
        org-refile-use-outline-path 'file
        org-use-speed-commands t)

;; https://github.com/minad/vertico?tab=readme-ov-file#org-refile 
(with-eval-after-load 'vertico
  (defun vertico-enforce-basic-completion (&rest args)
    (minibuffer-with-setup-hook
        (:append
         (lambda ()
           (let ((map (make-sparse-keymap)))
             (define-key map [tab] #'minibuffer-complete)
             (use-local-map (make-composed-keymap (list map) (current-local-map))))
           (setq-local completion-styles (cons 'basic completion-styles)
                       vertico-preselect 'prompt)))
      (apply args)))
  
  (advice-add #'org-olpath-completing-read :around #'vertico-enforce-basic-completion))

(with-eval-after-load 'ef-themes
  (setopt org-modern-tag-faces (ef-themes-with-colors
                               `(("personal" :background ,bg-cyan-subtle)
                                 ("work" :background ,bg-red-subtle)))
        org-modern-todo-faces (ef-themes-with-colors
                               `(("CHECK" :background ,yellow :foreground ,bg-main)))))

(setopt denote-directory "/home/rickyson/org/notes")

(let ((agenda (list org-directory denote-directory))
      (schedule (file-name-concat org-directory "schedule"))
      (work-schedule (file-name-concat org-directory "schedule/work.org"))
      (work-recur (file-name-concat org-directory "schedule/work-recurring.org")))
  (setopt org-agenda-files (flatten-list (list agenda schedule)))
  (setopt ra/org-work-agenda-files (flatten-list (list agenda work-schedule work-recur))))

(setopt org-tag-alist '((:startgroup . nil)
                        ("personal" . ?p)
                        ("work" . ?w)
                        (:endgroup . nil))
        org-agenda-current-time-string "◀═════ now ═════▶"
        org-agenda-default-appointment-duration 60
        org-priority-highest ?A
        org-priority-lowest ?E
        org-priority-default ?D
        org-agenda-custom-commands
        '(("u" "My GTD Agenda"
           ((agenda "" ((org-agenda-span 'day)
                        (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                        (org-agenda-breadcrumbs-separator " ❱ ")
                        (org-agenda-current-time-string "◀═════ now ═════▶")
                        (org-agenda-time-grid '((today require-timed remove-match)
                                                (800 1000 1200 1400 1600 1800 2000 2200 2400)
                                                "      " "┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈"))
                        ;; (org-agenda-prefix-format "  %-3i  %-15b%t  %s")
                        (org-agenda-prefix-format "   %i %?-2 t%s")
                        (org-agenda-start-day "+0")
                        ;; (org-agenda-prefix-format " %i %-12:c%?-12t% s")
                        (org-agenda-overriding-header "󰃭 Calendar")
                        (org-super-agenda-header-separator "")
                        (org-super-agenda-groups '((:time-grid t)))))
            (tags-todo "PRIORITY=\"A\"&TODO=\"TODO\"" ((org-agenda-overriding-header " Urgent")
                                                       (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                                                       (org-agenda-prefix-format "%8b %i ")))
            (tags-todo "-PRIORITY=\"A\"&-TODO=\"DONE\""
                       ((org-agenda-overriding-header "⚡ New TODO")
                        (org-agenda-prefix-format "%8b %i ")
                        (org-agenda-skip-function '(org-agenda-skip-entry-if 'timestamp))
                        (org-agenda-sorting-strategy '(priority-down))))
            (todo "NEXT" ((org-agenda-overriding-header "⚡ Next Action")
                          ;; (org-agenda-prefix-format " %i %-30:(my/org-gtd-agenda-prefix-format 30) ")
                          (org-agenda-prefix-format '((todo . " %i %-30:(org-gtd-agenda--prefix-format)")))
                          ;; (org-agenda-prefix-format " %i %-12:(org-gtd--agenda-prefix-format)")
                          (org-agenda-files `(,org-gtd-directory))))
            (todo "WAIT" ((org-agenda-overriding-header " Delegated / Blocked")
                          (org-agenda-todo-ignore-with-date t)
                          ;; (org-agenda-prefix-format " %i %-12:(org-gtd--agenda-prefix-format)")
                          (org-agenda-files `(,org-gtd-directory))))))
          ("w" "Work"
           ((agenda "" ((org-agenda-span 'day)
                        (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                        (org-agenda-breadcrumbs-separator " ❱ ")
                        (org-agenda-current-time-string "◀═════ now ═════▶")
                        (org-agenda-time-grid '((today require-timed remove-match)
                                                (800 1000 1200 1400 1600 1800 2000 2200 2400)
                                                "      " "┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈"))
                        ;; (org-agenda-prefix-format "  %-3i  %-15b%t  %s")
                        (org-agenda-prefix-format "   %i %?-2 t%s")
                        (org-agenda-start-day "+0")
                        ;; (org-agenda-prefix-format " %i %-12:c%?-12t% s")
                        (org-agenda-overriding-header "󰃭 Calendar")
                        (org-super-agenda-header-separator "")
                        (org-super-agenda-groups '((:time-grid t)))
                        (org-agenda-files ra/org-work-agenda-files)))
            (tags-todo "work&PRIORITY=\"A\"&TODO=\"TODO\"" ((org-agenda-overriding-header " Urgent")
                                    (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                                    (org-agenda-prefix-format "%8b %i ")
                                    (org-agenda-files ra/org-work-agenda-files)))
            (tags-todo "work&-PRIORITY=\"A\"&-TODO=\"DONE\""
                       ((org-agenda-overriding-header "⚡ New TODO")
                        (org-agenda-prefix-format "%8b %i ")
                        (org-agenda-skip-function '(org-agenda-skip-entry-if 'timestamp))
                        (org-agenda-sorting-strategy '(priority-down))
                        (org-agenda-files ra/org-work-agenda-files)))))))

(add-hook 'org-agenda-finalize-hook #'beginning-of-buffer 90)

;; https://www.reddit.com/r/emacs/comments/1h532zl/how_to_automatically_narrow_to_subtree_when/?rdt=57629
(defun ra/org-agenda-goto-narrow ()
  "`org-agenda-goto' and then narrow that subtree"
  (interactive)
  (org-agenda-goto)
  (org-narrow-to-subtree))

(with-eval-after-load 'org-agenda
  (ra/keymap-set org-agenda-mode-map
    "<remap> <org-agenda-goto>" #'ra/org-agenda-goto-narrow))

(elpaca org-agenda-property
  (setopt org-agenda-property-list '("LOCATION" "DELEGATED_TO")))

(elpaca org-fragtog
  (add-hook 'org-mode-hook #'org-fragtog-mode))

;; (defun ra/org-convert-link (link)
;;   "Convert links to org-")

(with-eval-after-load 'org
  (ra/keymap-set org-mode-map
    "C-c C-o" #'org-open-at-point
    "C-'" nil))

(setopt org-format-latex-options '( :foreground default
                                    :background default
                                    :scale 0.3
                                    :html-foreground "Black"
                                    :html-background "Transparent"
                                    :html-scale 1.0
			                        :matchers ("begin" "$1" "$" "$$" "\\(" "\\[")))

(elpaca (org-cv :host github :repo "Titan-C/org-cv"
                :main "ox-moderncv.el" 
                :files ("ox-moderncv.el" "org-cv-utils.el")))

(defun ra/denote-mermaid-file (&optional name ext)
  "Return the denote file name for a mermaid file, appended with NAME and
using EXT as extension."
  (let* ((current-file-name (file-name-sans-extension (file-name-nondirectory (buffer-file-name))))
         (file-name (if name
                        (concat current-file-name "." name)
                      current-file-name)))
    (concat file-name (or ext ".svg"))))

(elpaca ob-mermaid
  (add-to-list 'org-babel-load-languages '(mermaid . t)))

(elpaca ox-hugo
  (with-eval-after-load 'ox
    (require 'ox-hugo))
  (setopt org-hugo-base-dir "~/projects/rickyson96.github.io"))

(elpaca ob-mongo)

(provide '+lang-org)
;;; +lang-org.el ends here
