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
		  magit-revision-show-gravatars '("^Author:     " . "^Commit:     ")
		  magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1
		  auto-revert-buffer-list-filter #'magit-auto-revert-repository-buffer-p)

  (with-eval-after-load 'magit
	(transient-replace-suffix 'magit-branch 'magit-checkout
	  '("b" "dwim" magit-branch-or-checkout))
	(transient-append-suffix 'magit-branch 'magit-branch-checkout
	  '("B" "branch/revision" magit-checkout)))

  ;; Taken from https://github.com/alphapapa/unpackaged.el#hydra
  (with-eval-after-load 'hydra
	(defhydra smerge-hydra
	  (:color pink :hint nil :post (smerge-auto-leave))
	  "
^Move^       ^Keep^               ^Diff^                 ^Other^
^^-----------^^-------------------^^---------------------^^-------
_n_ext       _b_ase               _<_: upper/base        _C_ombine
_p_rev       _u_pper              _=_: upper/lower       _r_esolve
^^           _l_ower              _>_: base/lower        _k_ill current
^^           _a_ll                _R_efine
^^           _RET_: current       _E_diff
"
	  ("n" smerge-next)
      ("p" smerge-prev)
      ("b" smerge-keep-base)
      ("u" smerge-keep-upper)
      ("l" smerge-keep-lower)
      ("a" smerge-keep-all)
      ("RET" smerge-keep-current)
      ("\C-m" smerge-keep-current)
      ("<" smerge-diff-base-upper)
      ("=" smerge-diff-upper-lower)
      (">" smerge-diff-base-lower)
      ("R" smerge-refine)
      ("E" smerge-ediff)
      ("C" smerge-combine-with-next)
      ("r" smerge-resolve)
      ("k" smerge-kill-current)
      ("ZZ" (lambda ()
              (interactive)
              (save-buffer)
              (bury-buffer))
       "Save and bury buffer" :color blue)
	  ("q" nil "cancel" :color blue))

	(add-hook 'smerge-mode-hook (lambda () (when smerge-mode
											 (smerge-hydra))))))

(elpaca magit-imerge)

(elpaca forge)

(elpaca agitate
  (add-hook 'diff-mode-hook #'agitate-diff-enable-outline-minor-mode)
  (advice-add #'vc-git-push :override #'agitate-vc-git-push-prompt-for-remote)
  (agitate-log-edit-informative-mode 1))

(elpaca diff-hl
  (global-diff-hl-mode 1))

(elpaca git-timemachine)

(elpaca browse-at-remote)

(setopt vc-handled-backends '(Git))

(provide '+vc)
;;; +vc.el ends here
