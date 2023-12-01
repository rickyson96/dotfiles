;;; +lang-jsts.el --- Javascript and Typescript language setup  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Ricky Anderson

;; Author: Ricky Anderson <rickyson@thinky>
;; Keywords: local

;; This file is not part of GNU Emacs

;;; Commentary:

;;

;;; Code:

(ra/treesitter-setup typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
(ra/treesitter-setup tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
(ra/treesitter-setup javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")

;; (add-hook 'typescript-ts-mode-hook #'lsp-deferred)
;; (add-hook 'tsx-ts-mode-hook #'lsp-deferred)
;; (add-hook 'js-ts-mode-hook #'lsp-deferred)

(add-hook 'typescript-ts-mode-hook #'eglot)

(elpaca npm)
(elpaca nodejs-repl)
(elpaca nvm)

(provide '+lang-jsts)
;;; +lang-jsts.el ends here
