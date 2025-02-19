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
  (setopt magit-prefer-remote-upstream t
          magit-branch-prefer-remote-upstream '("master" "develop" "production" "development" "staging" "dev" "main")
          magit-branch-adjust-remote-upstream-alist '(("origin/develop" . ("master" "main" "develop"))
                                                      ("origin/master" . ("master" "main"))
                                                      ("origin/main" . ("master" "main")))
          magit-revision-show-gravatars '("^Author:     " . "^Commit:     ")
          magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1
          auto-revert-buffer-list-filter #'magit-auto-revert-repository-buffer-p
          magit-diff-refine-hunk t
          magit-commit-squash-confirm nil
          magit-clone-name-alist `(("\\`\\(?:github:\\|gh:\\)?\\([^:]+\\)\\'" "github.com" "github.user")
                                   ("\\`\\(?:gitlab:\\|gl:\\)\\([^:]+\\)\\'" "gitlab.com" "gitlab.user")
                                   ("\\`\\(?:sourcehut:\\|sh:\\)\\([^:]+\\)\\'" "git.sr.ht" "sourcehut.user")
                                   (,(rx bos "dc:" (group (1+ (not (any ":")))) eos) "git.datacandy.com" "gitlab.git.datacandy.com/api.user")
                                   (,(rx bos (or "nicejob:" "nj:" "nicejobinc:") (group (1+ (not (any ":")))) eos) "git.datacandy.com" "nicejob")
                                   (,(rx bos (or "atom:" "a:") (group (1+ (not (any ":")))) eos) "git.datacandy.com" "atom")
                                   (,(rx bos (or "zomaron:" "zm:") (group (1+ (not (any ":")))) eos) "git.datacandy.com" "zomaron")))

  (ra/keymap-set project-prefix-map
    "m" #'magit-project-status)

  (with-eval-after-load 'magit
    (require 'forge))

  (define-advice magit-whitespace-disallowed (:override (&rest _) use-dash-instead)
    "Use `-' instead of beeping when space is not allowed"
    (insert "-"))

  (magit-wip-mode 1)

  (with-eval-after-load 'magit
    (transient-replace-suffix 'magit-branch 'magit-checkout
      '("b" "dwim" magit-branch-or-checkout))
    (transient-append-suffix 'magit-branch 'magit-branch-checkout
      '("y" "branch/revision" magit-checkout))
    (transient-append-suffix 'magit-fetch "-t"
      '("-u" "Unshallow repository" "--unshallow"))

    (ra/keymap-set magit-status-mode-map
      "M-RET" #'magit-diff-visit-file-other-window))

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
    ("q" nil "cancel" :color blue)))

(when (executable-find "delta")
  (elpaca magit-delta
    ;; (add-hook 'magit-mode-hook #'magit-delta-mode)
    (setopt magit-delta-hide-plus-minus-markers nil)))

(setopt ediff-window-setup-function 'ediff-setup-windows-plain
        ediff-split-window-function 'split-window-horizontally
        ediff-diff-options "-w"
        ediff-show-ancestor nil)

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
                                       "c" 'ra/ediff-copy-both-to-C)))

(add-hook 'ediff-startup-hook (lambda ()
                                (setopt ediff-long-help-message-merge "
p,DEL -previous diff |     | -vert/horiz split   |  x -copy buf X's region to C
n,SPC -next diff     |     h -highlighting       |  c -copy both region to C
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

  (defconst ra/transient-review-pr
    '("/r" "Review Pull Request" ra/checkout-and-review-forge-pr
      :transient transient--do-exit))

  (with-eval-after-load 'forge-commands
    (transient-append-suffix 'forge-dispatch 'forge-add-repository ra/transient-review-pr))
  (with-eval-after-load 'forge-topic
    (transient-append-suffix 'forge-topic-menu 'forge-browse-this-topic ra/transient-review-pr))

  ;; TODO probably upstream this?
  (defun ra/forge-post-eldoc-function (callback)
    "use `bug-reference--url-at-point' to display the bug url at point"
    (if-let* ((topic (forge-topic-at-point))
              (title (oref topic title))
              (num (oref topic number)))
      (format "%s %s"
              (propertize (format "#%s:" num) 'face 'bold)
              title)
      (bug-reference--url-at-point)))

  (defun ra/forge-post-setup ()
    "extra setup for `forge-post-mode'"
    (add-hook 'eldoc-documentation-functions #'ra/forge-post-eldoc-function nil t))

  (add-hook 'forge-post-mode-hook #'ra/forge-post-setup)

  (with-eval-after-load 'forge-core
    (push '("git.datacandy.com"               ; GITHOST
            "git.datacandy.com/api/v4"        ; APIHOST
            "git.datacandy.com"               ; WEBHOST and INSTANCE-ID
            forge-gitlab-repository)    ; CLASS
          forge-alist)))

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
  (defun ra/checkout-and-review-forge-pr (&optional pullreq)
    "Checkout using `forge-checkout-pullreq' and immediately start code-review session.
This ensures that we can visit correct pullreq file when reviewing."
    (interactive)
    (let ((pullreq (or pullreq
                       (forge-current-pullreq)
                       (forge-read-pullreq "Review pull-request: "))))
      (magit-fetch-all-no-prune)
      (magit-checkout (forge--pullreq-ref pullreq))
      (forge-visit-pullreq pullreq)
      (call-interactively #'code-review-forge-pr-at-point)))

  (setopt code-review-auth-login-marker 'forge
          code-review-gitlab-host "git.datacandy.com/api"
          code-review-gitlab-graphql-host "git.datacandy.com/api"))

(elpaca (emsg-blame :host github :repo "ISouthRain/emsg-blame")
  (setopt emsg-blame-background t))

(elpaca dumb-diff)

(provide '+vc)
;;; +vc.el ends here
