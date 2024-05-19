;;; +vc.el --- Controlling code versions inside Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Ricky Anderson

;; Author: Ricky Anderson <rickyson@thinky>
;; Keywords: local

;; This file is not part of GNU Emacs

;;; Commentary:

;;

;;; Code:

(elpaca transient)
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
										   (smerge-hydra/body)))))

(setopt ediff-window-setup-function 'ediff-setup-windows-plain
		ediff-split-window-function 'split-window-horizontally
		ediff-diff-options "-w")

(defun ra/ediff-copy-both-to-C ()
  "`ediff' function to combine both on conflict merge.

Taken from: https://stackoverflow.com/a/29757750"
  (interactive)
  (ediff-copy-diff ediff-current-difference nil 'C nil
                   (concat
                    (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer)
                    (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer))))

(add-hook 'ediff-keymap-setup-hook (lambda ()
									 (keymap-set ediff-mode-map
									   "B" 'ra/ediff-copy-both-to-C)))

(add-hook 'ediff-startup-hook (lambda ()
								(setopt ediff-long-help-message-merge "
p,DEL -previous diff |     | -vert/horiz split   |  x -copy buf X's region to C
n,SPC -next diff     |     h -highlighting       |  B -copy both region to C
    j -jump to diff  |     @ -auto-refinement    |  r -restore buf C's old diff
   gx -goto X's point|    ## -ignore whitespace  |  * -refine current region
  C-l -recenter      | #f/#h -focus/hide regions |  ! -update diff regions
  v/V -scroll up/dn  |     X -read-only in buf X |  + -combine diff regions
  </> -scroll lt/rt  |     m -wide display       | wx -save buf X
    ~ -swap variants |     s -shrink window C    | wd -save diff output
                     |  $$ -show clashes only    |  / -show/hide ancestor buff
                     |  $* -skip changed regions |  & -merge w/new default
")))

(elpaca magit-imerge)

(elpaca forge
  (autoload #'forge-read-pullreq "forge")
  (with-eval-after-load 'forge
	(transient-append-suffix 'forge-topic-menu 'forge-create-pullreq-from-issue
	  '("R" "Review Pull Request" ra/checkout-and-review-forge-pr-topic
		:transient transient--do-exit
		:if (lambda () (forge-pullreq-p (forge-current-topic t)))))))

(elpaca agitate
  (add-hook 'diff-mode-hook #'agitate-diff-enable-outline-minor-mode)
  (advice-add #'vc-git-push :override #'agitate-vc-git-push-prompt-for-remote)
  (agitate-log-edit-informative-mode 1))

(elpaca diff-hl
  (global-diff-hl-mode 1))

(elpaca magit-todos
  (with-eval-after-load 'magit
	(magit-todos-mode 1)))

(elpaca git-timemachine)

(elpaca browse-at-remote)

(setopt vc-handled-backends '(Git))

(elpaca (closql :depth nil))

(elpaca (code-review :host github :repo "doomelpa/code-review")
  (defun ra/checkout-and-review-forge-pr-topic ()
	"Checkout using `forge-checkout-pullreq' and immediately start code-review session.
This ensures that we can visit correct pullreq file when reviewing.
This is used from `forge-topic-menu', so it's safe to assume we are on a topic."
	(interactive)
	(let ((pullreq (forge-current-topic t)))
	  (forge-checkout-pullreq pullreq)
	  (magit-fetch-all-no-prune)
	  (call-interactively #'code-review-forge-pr-at-point)))

  (defun ra/checkout-and-review-forge-pr (pullreq)
	"Checkout using `forge-checkout-pullreq' and immediately start code-review session.
This ensures that we can visit correct pullreq file when reviewing."
	(interactive (list (forge-read-pullreq "Review pull-request")))
	(forge-checkout-pullreq pullreq)
	(magit-fetch-all-no-prune)
	(forge-visit-pullreq pullreq)
	(call-interactively #'code-review-forge-pr-at-point)))

(provide '+vc)
;;; +vc.el ends here
