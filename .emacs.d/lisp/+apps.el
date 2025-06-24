;;; +apps.el --- Emacs Apps to Live Inside Emacs     -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Ricky Anderson

;; Author: Ricky Anderson <rickyson@thinky>
;; Keywords: local

;; This file is not part of GNU Emacs

;;; Commentary:

;;

;;; Code:

(defun ra/generate-totp-url (account key issuer)
  (interactive (let* ((account-raw (split-string (read-string "Account: ") ":"))
                      (key (string-replace " " "" (read-string "Key: ")))
                      (account (car (last account-raw)))
                      (issuer-from-acc (when (length= account-raw 2)
                                         (car account-raw)))
                      (issuer (read-string "Issuer: " issuer-from-acc)))
                 (list account key issuer)))
  (kill-new (format "otpauth://totp/%s?secret=%s&issuer=%s"
                    (string-join (remove "" (list issuer account)) ":")
                    key
                    issuer)))

(elpaca pass
  (defvar-keymap embark-password-store-actions
    :doc "Keymap for actions for password-store."
    "c" #'password-store-copy
    "f" #'password-store-copy-field
    "i" #'password-store-insert
    "I" #'password-store-generate
    "r" #'password-store-rename
    "e" #'password-store-edit
    "k" #'password-store-remove
    "U" #'password-store-url)

  (with-eval-after-load 'embark
    (add-to-list 'embark-keymap-alist '(password . embark-password-store-actions)))

  ;; Either add a prompt classifier or overwrite password-store--completing-read
  (with-eval-after-load 'marginalia
    (add-to-list 'marginalia-prompt-categories '("Password entry" . password-store)))

  (with-eval-after-load 'pass
    (ra/keymap-set pass-view-mode-map
      "<remap> <meow-quit>" #'kill-current-buffer)
    (ra/keymap-set pass-mode-map
      "S" #'ra/pass-symlink)))

;; TODO upstream this?
(defun ra/pass-symlink (edit)
  "Relatively generate symlink entry at point.
By default, it'll try to symlink the parent directory.
EDIT means edit which directory or file to be symlinked."
  (interactive "P")
  (if-let* ((pass-entry (pass-closest-entry))
            (pass-dir (file-name-concat default-directory (file-name-directory pass-entry)))
            (entry (if edit
                       (read-file-name "Select dir/file to symlink: " pass-dir nil t (concat "/" pass-entry))
                     (file-name-concat default-directory (file-name-parent-directory pass-entry))))
            (symlink-name (read-file-name (format "Symlink %s to: " (propertize entry 'face 'bold)) (file-name-parent-directory entry))))
      (make-symbolic-link entry symlink-name 1)
    (message "No entry at point")))

(defun ra/scan-otp-uri (&optional direct-insert)
  "Scan otp-uri using `hyprshot' and put it in `kill-ring'"
  (interactive "P")
  (let* ((cmd (if direct-insert #'insert #'kill-new))
         (otp-raw (shell-command-to-string "hyprshot -rm region | zbarimg PNG:-"))
         (otp (car (split-string otp-raw)))
         (otp-split (split-string otp ":")))
    (funcall cmd (string-join (cdr otp-split) ":"))))

(auth-source-pass-enable)

(elpaca eat
  ;; (add-hook 'eat-exit-hook #'quit-window)
  (add-hook 'eshell-load-hook #'eat-eshell-mode)
  (eat-eshell-mode 1)
  (setopt eat-term-scrollback-size nil)

  (defun ra/connect-to-paystone-vm ()
    (interactive)
    (let ((display-buffer-overriding-action '(display-buffer-full-frame . ())))
      (tabspaces-switch-or-create-workspace "paystone-vm")
      (eat-other-window)))

  (with-eval-after-load 'eat
    (ra/keymap-set eat-char-mode-map
      "C-M-g" 'eat-semi-char-mode
      "M-RET" 'eat-self-input
      "C-/" 'eat-self-input)
    (ra/keymap-set eat-semi-char-mode-map
      "C-x 3" (lambda ()
                (interactive)
                (let ((window (split-window nil nil 'right)))
                  (select-window window)
                  (with-selected-window window
                    (eat nil '(4)))))))

  (with-eval-after-load 'meow
    (add-hook 'meow-insert-exit-hook (lambda ()
                                       ;; (hide-mode-line-mode -1)
                                       (when (eq major-mode 'eat-mode)
                                         (eat-emacs-mode))))
    (add-hook 'meow-insert-enter-hook (lambda () (when (eq major-mode 'eat-mode)
                                                   ;; (hide-mode-line-mode -1)
                                                   (eat-semi-char-mode)))))

  ;; Taken from https://codeberg.org/akib/emacs-eat/issues/33#issuecomment-1127805
  (define-advice compilation-start (:override (command &optional mode name-function highlight-regexp continue) eat-compilation-start)
    (let ((name-of-mode "compilation")
          (dir default-directory)
          outbuf)
      (if (or (not mode) (eq mode t))
          (setq mode #'compilation-minor-mode)
        (setq name-of-mode (replace-regexp-in-string "-mode\\'" "" (symbol-name mode))))
      (with-current-buffer
          (setq outbuf
                (get-buffer-create
                 (compilation-buffer-name name-of-mode mode name-function)))
        (setq default-directory dir)
        (setq buffer-read-only nil)
        (erase-buffer)
        (compilation-insert-annotation
         "-*- mode: " name-of-mode
         "; default-directory: "
         (prin1-to-string (abbreviate-file-name default-directory))
         " -*-\n")
        (compilation-insert-annotation
         (format "%s started at %s\n\n"
                 mode-name
                 (substring (current-time-string) 0 19))
         command "\n")
        (eat-mode)
        (eat-exec outbuf "*compile*" shell-file-name nil (list "-lc" command))
        (run-hook-with-args 'compilation-start-hook (get-buffer-process outbuf))
        (eat-emacs-mode)
        (funcall mode)
        (setq next-error-last-buffer outbuf)
        (display-buffer outbuf '(nil (allow-no-window . t)))
        (when-let (w (get-buffer-window outbuf))
          (set-window-start w (point-min)))))))

(elpaca coterm)

(require '+apps-eshell)

(setopt calc-multiplication-has-precedence nil)

(setopt proced-enable-color-flag t
        proced-auto-update-flag t
        proced-auto-update-interval 1
        proced-format 'short
        proced-show-remote-processes t)

(elpaca daemons
  (add-hook 'daemons-mode-hook #'eldoc-mode))

(elpaca journalctl-mode)

(elpaca bluetooth)

(elpaca trashed)

(elpaca elfeed
  (setopt elfeed-search-filter "@6-months-ago +unread -bulk"))
(elpaca elfeed-org
  (setopt rmh-elfeed-org-files '("~/org/elfeed.org"))
  (elfeed-org))
(elpaca elfeed-goodies
  (with-eval-after-load 'elfeed
    (elfeed-goodies/setup)))

(defun ra/denote-meeting-template ()
  "Command to create a meeting note for denote on `org-capture-template'"
  (lambda () (let ((denote-commands-for-new-notes (append denote-commands-for-new-notes '(denote-org-capture denote-org-capture-with-prompts)))
                   (denote-use-keywords '("paystone"))
                   (denote-org-capture-specifiers (string-join '(":Created: %T"
                                                                 ""
                                                                 "* Attendees%?"
                                                                 "** Me"
                                                                 "** "
                                                                 ""
                                                                 "* Notes"
                                                                 ""
                                                                 "* Action Items"
                                                                 "** TODO [#A] ")
                                                               "\n")))
               (denote-org-capture-with-prompts (not denote-use-title)))))

(defun ra/start-meeting ()
  "Use at org-agenda. Tries to open link at point and create a meeting note after that."
  (interactive)
  (unless (eq major-mode 'org-agenda-mode)
    (user-error "Need to be in org-agenda."))

  (org-agenda-open-link)
  (let* ((meeting-name (substring-no-properties (get-text-property (point) 'txt)))
         (denote-use-title (format "Meeting: %s" meeting-name)))
    (org-capture nil "m")))

(elpaca denote
  ;; (require 'denote-journal-extras)
  ;; (setopt denote-journal-extras-title-format 'day-date-month-year)

  (with-eval-after-load 'doct
    (setopt org-capture-templates (doct-add-to org-capture-templates
                                               '(("  Denote" :keys "n"
                                                  :file denote-last-path
                                                  :type plain
                                                  :template denote-org-capture
                                                  :no-save t
                                                  :immediate-finish nil
                                                  :kill-buffer t)
                                                 ("󰢧 Denote Meeting" :keys "m"
                                                  :file denote-last-path
                                                  :type plain
                                                  :template ra/denote-meeting-template
                                                  :no-save t
                                                  :immediate-finish nil
                                                  :kill-buffer t
                                                  :jump-to-captured t
                                                  :clock-in t
                                                  :clock-resume t))))))

(elpaca wordel)

(elpaca codespaces
  (require 'codespaces)
  (codespaces-setup))

(defmacro ra/verb-stored-body (name &optional jsonpath)
  "Get JSONPATH value from `verb-stored-response' with NAME.
Essentially does what `verb-json-get' but with `let-alist' syntax"
  `(let ((body (json-read-from-string
                (oref (verb-stored-response ,name) body))))
     (if ,(macroexp-quote jsonpath)
         (let-alist body ,jsonpath)
       body)))

(defun ra/verb-view-stored-response (name)
  "View stored response with NAME."
  (interactive (list (completing-read "Stored response: "
                                       (mapcar #'car verb--stored-responses))))
  (with-current-buffer-window (format "*verb response: %s*" name)
      #'display-buffer-other-window
      nil
    (insert (oref (verb-stored-response name) body))
    (jsonian-mode)))


(elpaca verb
  (setopt verb-json-use-mode 'jsonian-mode
          verb-auto-show-headers-buffer nil
          verb-suppress-load-unsecure-prelude-warning t)
  (with-eval-after-load 'org
    (ra/keymap-set org-mode-map
      "C-c C-r" verb-command-map))
  (add-hook 'verb-mode-hook (##require 'uuidgen))

  (with-eval-after-load 'mise
    (advice-add #'verb-send-request-on-point-other-window-stay :around #'mise-propagate-env)))

(elpaca impostman)

(elpaca (mount :host github :repo "zellerin/mount-mode"))

(elpaca (noman :host github :repo "andykuszyk/noman.el")
  (autoload #'noman "noman" "Attempt to parse command line help for the command CMD" t))

(when (executable-find "wakatime-cli")
  (elpaca wakatime-mode
    (global-wakatime-mode 1)))

(setopt calc-algebraic-mode 1)
(setopt math-additional-units '((cfm "ft^3/min" "Cubic feet / meter")
                                (b nil "bit")
                                (B "8 * b" "byte")
                                (KiB "1024 * B" "KibiByte")
                                (MiB "1024 * KiB" "MibiByte")
                                (GiB "1024 * MiB" "GibiByte")
                                (TiB "1024 * GiB" "TebiByte")
                                (PiB "1024 * TiB" "PebiByte")
                                (EiB "1024 * PiB" "ExbiByte")
                                (ZiB "1024 * EiB" "ZebiByte")
                                (YiB "1024 * ZiB" "YobiByte")
                                (Kib "1024 * b" "KibiBit")
                                (Mib "1024 * Kib" "MibiBit")
                                (Gib "1024 * Mib" "GibiBit")
                                (Tib "1024 * Gib" "TebiBit")
                                (Pib "1024 * Tib" "PebiBit")
                                (Eib "1024 * Pib" "ExbiBit")
                                (Zib "1024 * Eib" "ZebiBit")
                                (Yib "1024 * Zib" "YobiBit")))
(setq math-units-table nil)
(elpaca (calc-currency :host github :repo "jws85/calc-currency")
  (autoload #'calc-currency-load "calc-currency")
  (add-hook 'calc-start-hook #'calc-currency-load))

(elpaca plz
  (autoload #'plz "plz"))
(elpaca plz-see
  (with-eval-after-load 'plz-see
    (add-to-list 'plz-see-content-type-alist `("\\`application/json" . ,(lambda ()
                                                                          (jsonian-format-region (point-min) (point-max))
                                                                          (jsonian-mode))))))
(defun ra/get-redirect-location (url)
  "Get the Location header on an URL, if any."
  (interactive "sURL: ")
  (unwind-protect
      (progn
        (advice-add 'plz--skip-redirect-headers :override #'ignore)
        (condition-case err
	        (let ((plz-curl-default-args '("--silent" "--compressed")))
	          (plz 'get url))
          (plz-error (alist-get 'location (plz-response-headers
								           (plz-error-response (caddr err)))))))
    (advice-remove 'plz--skip-redirect-headers #'ignore)))

(elpaca devdocs)

(elpaca (mu4e :host github :files ("mu4e/*.el" "build/mu4e/mu4e-meta.el" "build/mu4e/mu4e-config.el" "build/mu4e/mu4e.info") :repo "djcb/mu"
              :main "mu4e/mu4e.el"
              :pre-build (("./autogen.sh" "-Dtests=disabled")
                          ("ninja" "-C" "build")
                          (make-symbolic-link (expand-file-name "./build/mu/mu")
                                              (expand-file-name "~/.local/bin/mu") 'ok-if-exists))
              :build (:not elpaca--compile-info)
              :post-build (("mu" "init" "--quiet" "--maildir" "/home/mail"
                            "--my-address=" "ricky.anderson2696@gmail.com"
                            "--my-address=" "ricky.anderson@xendit.co")
                           ("mu" "index" "--quiet")))
  ;; (setopt message-kill-buffer-on-exit t
  ;;         mu4e-update-interval 900
  ;;         mail-user-agent 'mu4e-user-agent
  ;;         mu4e-org-support t
  ;;         mu4e-maildir (expand-file-name "/home/mail")
  ;;         mu4e-attachment-dir "~/Downloads"
  ;;         mu4e-completing-read-function 'completing-read
  ;;         mu4e-compose-signature-auto-include nil
  ;;         mu4e-use-fancy-chars t
  ;;         mu4e-view-show-addresses t
  ;;         mu4e-view-show-images t
  ;;         mu4e-sent-messages-behavior 'sent
  ;;         mu4e-get-mail-command "mbsync -a"
  ;;         mu4e-change-filenames-when-moving t
  ;;         mu4e-confirm-quit nil
  ;;         mu4e-html2text-command  'mu4e-shr2text
  ;;         mu4e-context-policy 'pick-first
  ;;         mu4e-compose-context-policy 'always-ask
  ;;         mu4e-contexts (list
  ;;                        (make-mu4e-context
  ;;                         :name "personal"
  ;;                         :enter-func (lambda () (mu4e-message "Entering personal context"))
  ;;                         :leave-func (lambda () (mu4e-message "Leaving personal context"))
  ;;                         :match-func (lambda (msg)
  ;;                                       (when msg
  ;;                                         (mu4e-message-contact-field-matches
  ;;                                          msg '(:from :to :cc :bcc) "ricky.anderson2696@gmail.com")))
  ;;                         :vars `((user-mail-address .  "ricky.anderson2696@gmail.com")
  ;;                                 (user-full-name . "Ricky Anderson")
  ;;                                 (mu4e-compose-format-flowed . t)
  ;;                                 (mu4e-maildir . (file-name-concat (mu4e-root-maildir) user-mail-address))
  ;;                                 (mu4e-drafts-folder . "/[Gmail].Drafts")
  ;;                                 (mu4e-sent-folder . "/[Gmail].Sent Mail")
  ;;                                 (mu4e-trash-folder . "/[Gmail].Trash")
  ;;                                 (message-send-mail-function . smtpmail-send-it)
  ;;                                 (smtpmail-smtp-user . "ricky.anderson2696@gmail.com")
  ;;                                 ;; (smtpmail-auth-credentials . (expand-file-name "~/.authinfo.gpg"))
  ;;                                 (smtpmail-smtp-server . "smtp.gmail.com")
  ;;                                 (smtpmail-smtp-service . 587)
  ;;                                 (smtpmail-debug-info . t)
  ;;                                 (smtpmail-debug-verbose . t)))
  ;;                        (make-mu4e-context
  ;;                         :name "work"
  ;;                         :enter-func (lambda () (mu4e-message "Entering work context"))
  ;;                         :leave-func (lambda () (mu4e-message "Leaving work context"))
  ;;                         :match-func (lambda (msg)
  ;;                                       (when msg
  ;;                                         (mu4e-message-contact-field-matches
  ;;                                          msg '(:from :to :cc :bcc) "ricky.anderson@xendit.co")))
  ;;                         :vars `((user-mail-address .  "ricky.anderson@xendit.co")
  ;;                                 (user-full-name . "Ricky Anderson")
  ;;                                 (mu4e-drafts-folder . "/[Gmail].Drafts")
  ;;                                 (mu4e-sent-folder . "/[Gmail].Sent Mail")
  ;;                                 (mu4e-trash-folder . "/[Gmail].Trash")
  ;;                                 (mu4e-compose-format-flowed . t)
  ;;                                 (message-send-mail-function . smtpmail-send-it)
  ;;                                 (smtpmail-smtp-user . "ricky.anderson@xendit.co")
  ;;                                 ;; (smtpmail-auth-credentials . (expand-file-name "~/.authinfo.gpg"))
  ;;                                 (smtpmail-smtp-server . "smtp.gmail.com")
  ;;                                 (smtpmail-smtp-service . 587)
  ;;                                 (smtpmail-debug-info . t)
  ;;                                 (smtpmail-debug-verbose . t)))))
  )

(elpaca mu4easy
  (with-eval-after-load 'mu4e
    (setf (alist-get 'refile mu4e-marks)
      '(:char ("r" . "▶")
              :prompt "refile"
              :dyn-target (lambda (target msg) (mu4e-get-refile-folder msg))
              ;; Notice the special treatment for Gmail.
              :action (lambda (docid msg target)
                        (let ((maildir (mu4e-message-field msg :maildir)))
                          (if (string-match-p "Gmail\\|Google" maildir)
                              (mu4e--server-remove docid)
                            (mu4e--server-move docid (mu4e--mark-check-target target) "+S-u-N")))))))

  (cl-defmacro mu4easy-context (&key c-name maildir mail smtp
                                     (smtp-mail mail)
                                     (smtp-port 587)
                                     (smtp-type 'starttls)
                                     (sent-action 'sent)
                                     (name user-full-name)
                                     (sig user-full-name))
    (let ((inbox      (concat "/" maildir "/Inbox"))
          (sent       (concat "/" maildir "/Sent"))
          (trash      (concat "/" maildir "/Trash"))
          (refile     (concat "/" maildir "/Archive"))
          (draft      (concat "/" maildir "/Drafts")))

    `(make-mu4e-context
      :name ,c-name
      :match-func (lambda (msg)
                    (when msg
                      (string-match-p (concat "^/" ,maildir "/")
                                      (mu4e-message-field msg :maildir))))
      :vars '((user-mail-address . ,mail)
              (user-full-name . ,name)
              (mu4e-sent-folder . ,sent)
              (mu4e-drafts-folder . ,draft)
              (mu4e-trash-folder . ,trash)
              (mu4e-refile-folder . ,refile)
              (mu4e-sent-messages-behavior . ,sent-action)
              (smtpmail-stream-type . ,smtp-type)
              (smtpmail-smtp-service . ,smtp-port)
              (smtpmail-smtp-user . ,smtp-mail)
              (smtpmail-smtp-server . ,smtp)
              (smtpmail-debug-info . t)
              (smtpmail-debug-verbose . t)
              (org-msg-signature . ,sig)
              (mu4e-maildir-shortcuts .
                                      ((,inbox   . ?i)
                                       (,sent    . ?s)
                                       (,trash   . ?t)
                                       (,refile  . ?a)
                                       (,draft   . ?d)))))))

  (setopt mu4easy-contexts '((mu4easy-context
                              :c-name  "personal"
                              :maildir "ricky.anderson2696@gmail.com"
                              :mail    "ricky.anderson2696@gmail.com"
                              :smtp    "smtp.gmail.com"
                              :sent-action delete)
                             (mu4easy-context
                              :c-name  "work"
                              :maildir "ricky.anderson@xendit.co"
                              :mail    "ricky.anderson@xendit.co"
                              :smtp    "smtp.gmail.com"
                              :sent-action delete)))
  (mu4easy-mode 1))

(setopt zoneinfo-style-world-list '(("Asia/Jakarta" "Indonesia")
                                    ("Asia/Makassar" "Bali")
                                    ("America/Halifax" "Atlantic")
                                    ("America/Los_Angeles" "Pacific")
                                    ("UTC" "UTC"))
        world-clock-time-format "%R %z (%Z) %t%A %d %B"
        world-clock-buffer-name "*world clock*")

(elpaca docker
  (setopt docker-command "podman"))

(elpaca (ready-player :host github :repo "xenodium/ready-player")
  (ready-player-add-to-auto-mode-alist))

(elpaca calfw-org)
(elpaca (calfw-blocks :host github :repo "ml729/calfw-blocks"))
(elpaca calfw
  (with-eval-after-load 'calfw
    (setq cfw:calendar-mode-map (make-sparse-keymap))
    (ra/keymap-set cfw:calendar-mode-map
      "<mouse-1>" 'cfw:navi-on-click
      "n" 'cfw:navi-next-item-command
      "p" 'cfw:navi-prev-item-command
      "f" 'cfw:navi-next-day-command
      "b" 'cfw:navi-previous-day-command
      "." 'cfw:navi-goto-today-command
      "g" 'cfw:refresh-calendar-buffer
      "q" 'quit-window))


  (defun ra/calendar ()
    "open calendar using calfw"
    (interactive)
    (require 'calfw)
    (require 'calfw-org)
    (require 'calfw-blocks)

    (set-face-attribute 'cfw:face-today nil :background nil)

    (cfw:open-calendar-buffer
     :contents-sources
     (list (cfw:org-create-file-source "personal" (file-name-concat org-directory "schedule/personal.org") "White")
           (cfw:org-create-file-source "work" (file-name-concat org-directory "schedule/work.org") "White")
           (cfw:org-create-file-source "work" (file-name-concat org-directory "schedule/private.org") "White"))
     :view 'block-day)))

(elpaca (show-font :host github :repo "protesilaos/show-font"))

(elpaca tmr)

(elpaca leetcode)

(elpaca go-translate
  (require 'go-translate)
  (setq gt-langs '(en id zh)
        gt-default-translator (gt-translator :taker (gt-taker :text 'word :prompt t)
                                             :engines (gt-google-engine)
                                             :render (gt-buffer-render))))

(elpaca (syncthing :host github :repo "KeyWeeUsr/emacs-syncthing")
  (setq syncthing-default-server-token "fWEeWbbvkqXXgixeTUWGCfcPxGyDhY9T"))
(elpaca (emacs-conflict :host github :repo "ibizaman/emacs-conflict")
  (autoload 'emacs-conflict-resolve-conflicts "emacs-conflict" "" t)
  (autoload 'emacs-conflict-show-conflicts-dired "emacs-conflict" "" t)
  (autoload 'emacs-conflict-resolve-conflict-dired "emacs-conflict" "" t))

(elpaca (iwd :host sourcehut :repo "leon_plickat/iwd-el")
  (autoload #'iwd "iwd" "Control iwd WLAN connections." t))

(elpaca esup)

(elpaca (hexrgb :host github :repo "emacsmirror/hexrgb"))
(elpaca (palette :host github :repo "emacsmirror/palette"))

(defun ra/linkmarks-capture ()
  "`linkmarks-capture' but directly select the template. TODO upstream this"
  (interactive)
  (linkmarks--setup)
  (org-capture nil "b"))
(elpaca (linkmarks :host github :repo "dustinlacewell/linkmarks")
  (setopt linkmarks-file (file-name-concat org-directory "linkmarks.org"))
  (ra/keymap-set (current-global-map)
    "<remap> <bookmark-jump>" #'linkmarks-select
    "<remap> <bookmark-set>" #'ra/linkmarks-capture))

(elpaca org-jira
  (setopt jiralib-url "https://banksampoerna.atlassian.net"
          jiralib-user "it.di2@banksampoerna.com"
          org-jira-custom-jqls
          '((:jql "project IN (BSSD) AND assignee = 'Ricky Anderson' AND status NOT IN (DONE) AND Sprint != EMPTY order by priority, created DESC"
                  :limit 20
                  :filename "bssd-sprint-assigned")
            (:jql "project IN (BSSD) AND assignee = 'Ricky Anderson' AND status NOT IN (DONE) AND Sprint = EMPTY order by priority, created DESC"
                  :limit 20
                  :filename "bssd-backlog-assigned"))
          org-jira-jira-status-to-org-keyword-alist '(("To Do" . "TODO")
                                                      ("Blocked" . "BLOCKED")
                                                      ("In Progress" . "IN-PROGRESS")
                                                      ("QA FEEDBACK" . "TODO")
                                                      ("READY FOR TEST" . "QA")
                                                      ("CODE REVIEW" . "PR")
                                                      ("Testing" . "TESTING")
                                                      ("Done" . "DONE")
                                                      ("Won't Do" . "CNCL"))
          org-jira-progress-issue-flow '(("To Do" . "In Progress")
                                         ("In Progress" . "CODE REVIEW")
                                         ("CODE REVIEW" . "READY FOR TEST")
                                         ("QA FEEDBACK" . "In Progress"))))


(provide '+apps)
;;; +apps.el ends here
