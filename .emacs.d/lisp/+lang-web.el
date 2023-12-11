;;; +lang-web.el --- Web UI Language Support         -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Ricky Anderson

;; Author: Ricky Anderson <rickyson@thinky>
;; Keywords: local

;; This file is not part of GNU Emacs

;;; Commentary:

;;

;;; Code:

(elpaca web-mode
  (setopt web-mode-enable-html-entities-fontification t)

  (add-to-list 'auto-mode-alist '(".html\\'" . web-mode))
  (add-hook 'web-mode-hook #'lsp-deferred))

(provide '+lang-web)
;;; +lang-web.el ends here
