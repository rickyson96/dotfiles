;;; +elpaca.el --- Elpaca Package Manager            -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Ricky Anderson

;; Author: Ricky Anderson <rickyson@thinky>
;; Keywords: local

;; This file is not part of GNU Emacs

;;; Commentary:

;; 

;;; Code:

(defvar elpaca-installer-version 0.7)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                 ,@(when-let ((depth (plist-get order :depth)))
                                                     (list (format "--depth=%d" depth) "--no-single-branch"))
                                                 ,(plist-get order :repo) ,repo))))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

(defun +elpaca-unload-seq (e) "Unload seq before continuing the elpaca build, then continue to build the recipe E."
       (and (featurep 'seq) (unload-feature 'seq t))
       (elpaca--continue-build e))

(elpaca `(seq :build ,(append (butlast (if (file-exists-p (expand-file-name "seq" elpaca-builds-directory))
                                           elpaca--pre-built-steps
                                         elpaca-build-steps))
                              (list '+elpaca-unload-seq 'elpaca--activate-package))))

;; I put system-packages here because it's also package manager but systemwide.
(elpaca system-packages
  (with-eval-after-load 'system-packages
    (add-to-list 'system-packages-supported-package-managers
                 '(yay . ((default-sudo . nil)
                          (install . "yay -S")
                          (search . "yay -Ss")
                          (uninstall . "yay -Rns")
                          (update . "yay -Syu")
                          (clean-cache . "yay -Sc")
                          (change-log . "yay -Qc")
                          (log . "cat /var/log/pacman.log")
                          (get-info . "yay -Qi")
                          (get-info-remote . "yay -Si")
                          (list-files-provided-by . "yay -qQl")
                          (owning-file . "yay -Qo")
                          (owning-file-remote . "yay -F")
                          (verify-all-packages . "yay -Qkk")
                          (verify-all-dependencies . "yay -Dk")
                          (remove-orphaned . "yay -Yc")
                          (list-installed-packages . "yay -Qe")
                          (list-installed-packages-all . "yay -Q")
                          (list-dependencies-of . "yay -Qi")
                          (noconfirm . "--noconfirm"))))
    (setq system-packages-use-sudo nil
          system-packages-package-manager 'yay
          system-packages-noconfirm t))  )

(provide '+elpaca)
;;; +elpaca.el ends here
