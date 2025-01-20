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
      "<remap> <meow-quit>" #'kill-current-buffer)))

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
  (require 'denote-journal-extras)
  (setopt denote-journal-extras-title-format 'day-date-month-year)

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

(elpaca verb
  (setopt verb-json-use-mode 'jsonian-mode
          verb-auto-show-headers-buffer nil)
  (with-eval-after-load 'org
    (ra/keymap-set org-mode-map
      "C-c C-r" verb-command-map))
  (defun ra/verb-generate-url-encoded-body (&rest pairs)
    "Turn pairs of KEY VALUE into url-encoded format

\(fn [KEY VALUE]...)"
    (unless (zerop (mod (length pairs) 2))
      (error "PAIRS must be pair of KEY VALUE"))
    (string-join (seq-map (lambda (pair)
                            (format "%s=%s" (car pair) (cadr pair)))
                          (seq-split pairs 2))
                 "&"))
  (defun ra/verb-url-encoded-body-parser (rs)
    "`Verb-Map-Request' compatible version to convert newline into &"
    (when-let (body (oref rs body))
      (thread-last
        (string-trim body)
        (replace-regexp-in-string "\n" "&")
        (oset rs body)))
    rs))

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

(elpaca devdocs)

(elpaca (emacs-conflict :host github :repo "ibizaman/emacs-conflict"))

;; (elpaca mu4e)

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

(provide '+apps)
;;; +apps.el ends here
