;;; +ai.el --- AI Integration to Emacs               -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author:  <rickyson@thinky>
;; Keywords: local

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(elpaca (copilot :host github :repo "copilot-emacs/copilot.el")
  ;; (add-hook 'prog-mode-hook #'copilot-mode)
  (with-eval-after-load 'copilot
    (ra/keymap-set copilot-completion-map
      "C-g" #'copilot-clear-overlay
      "M-p" #'copilot-previous-completion
      "M-n" #'copilot-next-completion
      "C-e" #'copilot-accept-completion-by-line
      "M-f" #'copilot-accept-completion-by-word
      "C-<return>" #'copilot-accept-completion)))

(elpaca (openai :host github :repo "emacs-openai/openai")

  (defun ra/openai-key-auth-source (&optional base-url)
    "Retrieve the OpenAI API key from auth-source given a BASE-URL.
If BASE-URL is not specified, it defaults to `openai-base-url'."
    (if-let ((auth-info (auth-source-search :max 1
                                            :host (url-host (url-generic-parse-url (or nil openai-base-url)))
                                            :require '(:secret))))
        (funcall (plist-get (car auth-info) :secret))
      (error "OpenAI API key not found in auth-source")))

  (setopt openai-key #'ra/openai-key-auth-source
          openai-user "ricky.anderson2696@gmail.com"))
(elpaca (chatgpt :host github :repo "emacs-openai/chatgpt"))
(elpaca (codegpt :host github :repo "emacs-openai/codegpt"))

;; Tabby
;; (elpaca (tabby :host github :files ("*.el" "node_scripts") :repo "alan-w-255/tabby.el")
;;   (ra/keymap-set tabby-mode-map
;;     "C-<return>" #'tabby-accept-completion))

(elpaca gptel)


(provide '+ai)
;;; +ai.el ends here
