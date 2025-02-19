;;; +ui.el --- UI Theming and Configuration           -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Ricky Anderson

;; Author: Ricky Anderson <rickyson@thinky>
;; Keywords: local

;; This file is not part of GNU Emacs

;;; Commentary:

;;

;;; Code:

(setopt cursor-type 'bar
        truncate-lines t)

(line-number-mode 1)
(column-number-mode 1)
(size-indication-mode 1)

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(horizontal-scroll-bar-mode -1)

(setopt use-default-font-for-symbols nil
        face-font-rescale-alist '(("-cdac$" . 1.3)
                                  ("Sarasa" . 1.2)))

;; Test fonts
;; aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
;; iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii
;; 云云云云云云云云云云云云云云云云云云云云云云云云云云云云云云云云云云云云云云云云
;; 雲雲雲雲雲雲雲雲雲雲雲雲雲雲雲雲雲雲雲雲雲雲雲雲雲雲雲雲雲雲雲雲雲雲雲雲雲雲雲雲
;; ㄞㄞㄞㄞㄞㄞㄞㄞㄞㄞㄞㄞㄞㄞㄞㄞㄞㄞㄞㄞㄞㄞㄞㄞㄞㄞㄞㄞㄞㄞㄞㄞㄞㄞㄞㄞㄞㄞㄞㄞ
;; ああああああああああああああああああああああああああああああああああああああああ
;; 가가가가가가가가가가가가가가가가가가가가가가가가가가가가가가가가가가가가가가가가
;; aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
;; ●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●
;; ○○○○○○○○○○○○○○○○○○○○○○○○○○○○○○○○○○○○○○○○
;; ────────────────────────────────────────────────────────────────────────────────
;; ││││││││││││││││││││││││││││││││││││││││││││││││││││││││││││││││││││││││││││││││
;; ├├├├├├├├├├├├├├├├├├├├├├├├├├├├├├├├├├├├├├├├├├├├├├├├├├├├├├├├├├├├├├├├├├├├├├├├├├├├├├├├
;; └└└└└└└└└└└└└└└└└└└└└└└└└└└└└└└└└└└└└└└└└└└└└└└└└└└└└└└└└└└└└└└└└└└└└└└└└└└└└└└└
;; The quick brown fox jumps over the lazy dog.‘’“”
;; ABC.DEF.GHI.JKL.MNO.PQRS.TUV.WXYZ abc.def.ghi.jkl.mno.pqrs.tuv.wxyz
;; !iIlL17|¦ ¢coO08BDQ $5SZ2zs ∂96µm float il1[]={1-2/3.4,5+6=7/8%90};
;; 1234567890 ,._-+= >< «¯-¬_» ~–÷+× {*}[]()<>`+-=$/#_%^@\&|~?'" !,.;:
;; G6Qg9q¶ Þẞðþſß ΓΔΛαβγδιλμνξπτυφχψ ЖЗКНРУЭЯавжзклмнруфчьыэя <= != ==

(defun ra/set-font (&optional frame)
  "Set font configurations. FRAME determine the frame to set the font"
  (set-face-font 'default "monospace-11" frame)
  ;; (set-fontset-font "fontset-default" 'unicode (font-spec :family "monospace") frame 'prepend)
  ;; (dolist (charset '(kana han cjk-misc hangul kanbun bopomofo))
  ;;    (set-fontset-font "fontset-default" charset
  ;;                      (font-spec :family "Sarasa Term SC")
  ;;                      frame 'prepend))
  )

(ra/set-font)

(add-hook 'after-make-frame-functions #'ra/set-font)

(elpaca nerd-icons
  (setopt nerd-icons-scale-factor 1
          nerd-icons-font-family "IosevraRelaxed Nerd Font"))

(elpaca (hl-todo :depth nil)
  (global-hl-todo-mode 1))
(elpaca consult-todo)

(elpaca ef-themes
  (setopt ef-themes-mixed-fonts t
          ef-themes-variable-pitch-ui t
          ef-themes-headings ; read the manual's entry or the doc string
          (quote ((0 bold variable-pitch 1.2)
                  (1 bold variable-pitch 1.2)
                  (2 bold variable-pitch 1.1)
                  (t bold variable-pitch))))

  (with-eval-after-load 'org
    (set-face-attribute 'org-quote nil :slant 'italic))

  (defun my-ef-themes-hl-todo-faces ()
    "Configure `hl-todo-keyword-faces' with Ef themes colors.
The exact color values are taken from the active Ef theme.
Taken from: https://protesilaos.com/emacs/ef-themes#h:19c549dc-d13f-45c4-a727-3618591d5c4f"
    (ef-themes-with-colors
      (setopt hl-todo-keyword-faces
              `(("HOLD" . ,yellow)
                ("TODO" . ,red)
                ("NEXT" . ,blue)
                ("THEM" . ,magenta)
                ("PROG" . ,cyan-warmer)
                ("OKAY" . ,green-warmer)
                ("DONT" . ,yellow-warmer)
                ("FAIL" . ,red-warmer)
                ("BUG" . ,red-warmer)
                ("DONE" . ,green)
                ("NOTE" . ,blue-warmer)
                ("KLUDGE" . ,cyan)
                ("HACK" . ,cyan)
                ("TEMP" . ,red)
                ("FIXME" . ,red-warmer)
                ("XXX+" . ,red-warmer)
                ("REVIEW" . ,red)
                ("DEPRECATED" . ,yellow-faint)))))

  (defun ra/ef-themes-overrides ()
    "Override ef-themes face definitions."
    (my-ef-themes-hl-todo-faces)
    (ef-themes-with-colors
      (custom-set-faces `(header-line ((,c ( :family "monospace"
                                             :box ( :line-width 4
                                                    :color ,bg-dim
                                                    :style nil))))))))

  (add-hook 'ef-themes-post-load-hook #'ra/ef-themes-overrides)
  (add-hook 'csv-mode-hook #'ra/ef-themes-overrides)
  (add-hook 'sqlite-mode-hook #'ra/ef-themes-overrides)

  (mapc #'disable-theme custom-enabled-themes)
  (ef-themes-select 'ef-cyprus))

(elpaca doom-modeline
  (doom-modeline-mode 1)
  (setopt doom-modeline-height 24
          doom-modeline-bar-width 4
          doom-modeline-buffer-file-name-style 'truncate-with-project
          doom-modeline-indent-info t
          doom-modeline-total-line-number t
          doom-modeline-buffer-encoding 'nondefault))

(elpaca spacious-padding
  (setopt spacious-padding-widths '( :internal-border-width 15
                                     :header-line-width 4
                                     :mode-line-width 3
                                     :tab-width 4
                                     :right-divider-width 10
                                     :scroll-bar-width 8))

  (ra/configure-frame ra/spacious-padding
    (spacious-padding-mode 1)))

(elpaca (dired-plus :host github :repo "emacsmirror/dired-plus" :main "dired+.el"))

(elpaca (dirvish :host github :repo "alexluigit/dirvish"
                 :remotes ("hlissner" :repo "hlissner/dirvish"))
  (dirvish-override-dired-mode)
  (dirvish-peek-mode)
  (setopt dirvish-quick-access-entries '(("h" "~/"                          "Home")
                                         ("d" "~/Downloads/"                "Downloads")
                                         ("m" "/mnt/"                       "Drives")
                                         ("t" "~/.local/share/Trash/files/" "TrashCan")
                                         (". ." "~/.dotfiles"               "Dotfiles")
                                         (". r" "~/.root-dotfiles"          "Root Dotfiles")))
  (setopt dirvish-mode-line-format '(:left (sort symlink) :right (omit yank index))
          dirvish-header-line-format '(:left (path) :right (vc-info file-user))
          dirvish-attributes '(nerd-icons file-time file-size collapse subtree-state vc-state git-msg)
          dired-listing-switches "-l --almost-all --human-readable --group-directories-first --no-group"
          dirvish-reuse-session t
          dirvish-header-line-height '(20 . 25)
          dirvish-use-header-line 'global)

  (ra/keymap-set dirvish-mode-map
    "b" #'dired-up-directory
    "a" #'dirvish-quick-access
    "TAB" #'dirvish-subtree-toggle
    "M-f" #'dirvish-history-go-forward
    "M-b" #'dirvish-history-go-backward
    "M-l" #'dirvish-ls-switches-menu
    "M-m" #'dirvish-mark-menu
    "M-t" #'dirvish-layout-toggle
    "M-s" #'dirvish-setup-menu
    "M-e" #'dirvish-emerge-menu
    "M-j" #'dirvish-fd-jump

    "$" #'eshell
    "M-$" #'eshell

    "y" #'dirvish-yank-menu ; `dired-show-file-type' is accessible from `dirfish-file-info-menu'
    "N" #'dirvish-narrow    ; `dired-do-man' is less used command
    "^" #'dirvish-history-last
    "h" #'dirvish-history-jump ; `describe-mode' can be accessed from `help-map'
    "s" #'dirvish-quicksort    ; supersedes `dired-sort-toggle-or-edit'
    "v" #'dirvish-vc-menu      ; `dired-view-file' is superseded by preview and `dired-find-file'

    ;; if no marked files, similar to `dired-find-file'. We can have
    ;; both "f" and "F" key on single key, thus freeing one for
    ;; `dirvish-file-info-menu'.
    ;; Other than that, "RET" is also `dired-find-file'.
    "f" #'dired-do-find-marked-files
    "F" #'dirvish-file-info-menu))

(elpaca diredfl
  (add-hook 'dired-mode-hook #'diredfl-mode)
  (add-hook 'dirvish-directory-view-mode #'diredfl-mode)
  (with-eval-after-load 'diredfl (set-face-attribute 'diredfl-dir-name nil :bold t)))

(elpaca iscroll
  (mapc (lambda (x)
          (add-hook x #'iscroll-mode))
        '(org-mode-hook)))

(elpaca page-break-lines
  (setopt page-break-lines-modes '( emacs-lisp-mode lisp-mode scheme-mode compilation-mode outline-mode
                                    help-mode lisp-data-mode emacs-news-mode))
  (global-page-break-lines-mode 1))

(require '+ui-dashboard)

(elpaca hide-mode-line)

(elpaca breadcrumb
  (setopt breadcrumb-imenu-max-length 1.0)
  (with-eval-after-load 'breadcrumb
    (set-face-attribute 'breadcrumb-face nil :height 0.9)
    (set-face-attribute 'breadcrumb-imenu-leaf-face nil :height 0.9))

  (define-minor-mode ra/breadcrumb-local-mode ""
    :init-value nil
    (if ra/breadcrumb-local-mode (add-to-list 'header-line-format '(:eval (breadcrumb-imenu-crumbs)))
      (setq header-line-format (delete '(:eval (breadcrumb-imenu-crumbs)) header-line-format))))

  (define-globalized-minor-mode ra/breadcrumb-mode ra/breadcrumb-local-mode
    (lambda ()
      (unless (or (minibufferp)
                  (not (buffer-file-name))
                  (null (breadcrumb-project-crumbs)))
        (ra/breadcrumb-local-mode 1)))))

;; Zone (screensaver)
(define-advice zone (:around (orig-fn &rest _) "zone-all-buffer")
  (save-window-excursion
    (let ((op-win (car (window-list))))
      (mapc (lambda (w)
              (with-selected-window w
                (set-window-dedicated-p w nil)
                (switch-to-buffer "*zone*")))
            (cdr (window-list)))
      (with-selected-window op-win
        (set-window-dedicated-p op-win nil)
        (funcall orig-fn)))))

;; Disable zone
;; (add-hook 'server-after-make-frame-hook (lambda () (require 'zone) (zone-when-idle 900)))

(elpaca (commentize-conflict :host github :repo "zk-phi/commentize-conflict")
  (add-hook 'prog-mode-hook #'commentize-conflict-mode))

(elpaca (indent-bars :host github :repo "jdtsmith/indent-bars")
  (setopt indent-bars-treesit-support t
          indent-bars-treesit-ignore-blank-lines-types '("module")
          indent-bars-treesit-scope '((python function_definition class_definition for_statement
                                              if_statement with_statement while_statement)
                                      (yaml block_mapping))

          indent-bars-color '(highlight :face-bg t :blend 1.0)
          indent-bars-pattern ". . . . "
          indent-bars-width-frac 0.1
          indent-bars-pad-frac 0.1
          indent-bars-zigzag nil
          indent-bars-color-by-depth '(:regexp "outline-\\([0-9]+\\)" :blend 0.2)
          indent-bars-highlight-current-depth '(:pattern "." :blend 1) ; pump up the BG blend on current
          indent-bars-display-on-blank-lines t))

(elpaca emojify)
(elpaca emojify-logos)

(provide '+ui)
;;; +ui.el ends here
