;;; +vc.el --- Controlling code versions inside Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Ricky Anderson

;; Author: Ricky Anderson <rickyson@thinky>
;; Keywords: local

;; This file is not part of GNU Emacs

;;; Commentary:

;;

;;; Code:

(elpaca magit
  (setopt magit-branch-prefer-remote-upstream '("master" "develop" "production" "development" "staging" "dev" "main")
		  magit-branch-adjust-remote-upstream-alist '(("origin/master" . "/")
													  ("origin/main" . "/"))
		  magit-revision-show-gravatars '("^Author:     " . "^Commit:     "))

  (with-eval-after-load 'magit
	(transient-replace-suffix 'magit-branch 'magit-checkout
	  '("b" "dwim" magit-branch-or-checkout))
	(transient-append-suffix 'magit-branch 'magit-branch-checkout
	  '("B" "branch/revision" magit-checkout))))

(elpaca magit-imerge)

(elpaca agitate
  (add-hook 'diff-mode-hook #'agitate-diff-enable-outline-minor-mode)
  (advice-add #'vc-git-push :override #'agitate-vc-git-push-prompt-for-remote)
  (agitate-log-edit-informative-mode 1))

(elpaca diff-hl
  (global-diff-hl-mode 1)
  (diff-hl-flydiff-mode 1))

(elpaca git-timemachine)

(elpaca browse-at-remote)

(provide '+vc)
;;; +vc.el ends here
