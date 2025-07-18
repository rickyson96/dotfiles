;;; +lang-web.el --- Web UI Language Support         -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Ricky Anderson

;; Author: Ricky Anderson <rickyson@thinky>
;; Keywords: local

;; This file is not part of GNU Emacs

;;; Commentary:

;;

;;; Code:

(ra/treesitter-setup css "https://github.com/tree-sitter/tree-sitter-css")
(add-to-list 'auto-mode-alist `(,(rx ".css" eos) . css-ts-mode))

(elpaca web-mode
  (setopt web-mode-enable-html-entities-fontification t)

  (add-to-list 'auto-mode-alist '(".html\\'" . web-mode))
  (add-hook 'web-mode-hook #'lsp-deferred))

(ra/treesitter-setup vue "https://github.com/ikatyang/tree-sitter-vue")

(elpaca (vue-ts-mode :host github :repo "8uff3r/vue-ts-mode")
  (require 'vue-ts-mode)
  (add-hook 'vue-ts-mode-hook #'lsp-deferred)
  (setopt lsp-volar-take-over-mode nil
          lsp-volar-hybrid-mode t)
  (ef-themes-with-colors
    (set-face-attribute 'vue-ts-mode-template-tag-bracket-face nil :foreground cyan-faint))

  (with-eval-after-load 'smartparens
    ;; (add-to-list 'sp-sexp-suffix (list 'vue-ts-mode 'regexp ""))
    ;; (sp-local-pair 'vue-ts-mode "<" ">" :skip-match 'sp-javascript-skip-match-angle-bracket)
    ;; (sp-local-tag 'vue-ts-mode "<" "<_>" "</_>" :transform 'sp-match-sgml-tags :post-handlers '(sp-html-post-handler))
    ;; (add-to-list 'sp-navigate-consider-sgml-tags 'vue-ts-mode)
    ;; (eval-after-load 'vue-ts-mode '(require 'smartparens-html))
    ))

(elpaca lsp-tailwindcss
  (setopt lsp-tailwindcss-add-on-mode t
          lsp-tailwindcss-skip-config-check t
          lsp-tailwindcss-major-modes '(vue-ts-mode))
  ;; (with-eval-after-load 'lsp
  ;;   (add-to-list 'lsp-language-id-configuration `(,(rx ".vue" eol) . "")))
  )

(provide '+lang-web)
;;; +lang-web.el ends here
