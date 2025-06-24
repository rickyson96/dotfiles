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
  (add-hook 'prog-mode-hook #'copilot-mode)
  (setopt copilot-idle-delay 1)
  (with-eval-after-load 'copilot
    (ra/keymap-set copilot-completion-map
      "C-g" #'copilot-clear-overlay
      "M-p" #'copilot-previous-completion
      "M-n" #'copilot-next-completion
      "C-e" #'copilot-accept-completion-by-line
      "M-f" #'copilot-accept-completion-by-word
      "C-j" #'copilot-accept-completion)))

(elpaca gptel
  (with-eval-after-load 'gptel
    (require 'gptel-integrations))

  (setq ra/gptel-backends `((openrouter . ,(gptel-make-openai "OpenRouter"               ;Any name you want
                                            :host "openrouter.ai"
                                            :endpoint "/api/v1/chat/completions"
                                            :stream t
                                            :key (plist-get (car (auth-source-search :max 1
                                                                                     :host "openrouter.ai"
                                                                                     :user "andersonricky@proton.me^api-key"))
                                                            :secret)
                                            :models '(deepseek/deepseek-chat-v3-0324:free
                                                      mistralai/devstral-small:free)))
                            (gemini . ,(gptel-make-gemini "Gemini Pro Xendit"
                                        :key (plist-get (car (auth-source-search :host "gemini.google.com"
                                                                                 :user "ricky.anderson@xendit.co^api-key"))
                                                        :secret)
                                        :stream t))
                            (copilot . ,(gptel-make-gh-copilot "Copilot"))))
  (setopt gptel-default-mode #'org-mode
          gptel-model #'claude-3.7-sonnet-thought
          gptel-prompt-prefix-alist '((markdown-mode . "### ")
                                      (org-mode . "*** ")
                                      (text-mode . "### "))

          gptel-backend (alist-get 'copilot ra/gptel-backends))

  ;; Tools taken from https://github.com/karthink/gptel/wiki/Tools-collection
  (defun my-gptel--edit_file (file-path file-edits)
    "In FILE-PATH, apply FILE-EDITS with pattern matching and replacing."
    (if (and file-path (not (string= file-path "")) file-edits)
        (with-current-buffer (get-buffer-create "*edit-file*")
          (erase-buffer)
          (insert-file-contents (expand-file-name file-path))
          (let ((inhibit-read-only t)
                (case-fold-search nil)
                (file-name (expand-file-name file-path))
                (edit-success nil))
            ;; apply changes
            (dolist (file-edit (seq-into file-edits 'list))
              (when-let ((line-number (plist-get file-edit :line_number))
                         (old-string (plist-get file-edit :old_string))
                         (new-string (plist-get file-edit :new_string))
                         (is-valid-old-string (not (string= old-string ""))))
                (goto-char (point-min))
                (forward-line (1- line-number))
                (when (search-forward old-string nil t)
                  (replace-match new-string t t)
                  (setq edit-success t))))
            ;; return result to gptel
            (if edit-success
                (progn
                  ;; show diffs
                  (ediff-buffers (find-file-noselect file-name) (current-buffer))
                  (format "Successfully edited %s" file-name))
              (format "Failed to edited %s" file-name))))
      (format "Failed to edited %s" file-path)))

  (add-to-list 'gptel-tools
               (gptel-make-tool
                :function #'my-gptel--edit_file
                :name "edit_file"
                :description "Edit file with a list of edits, each edit contains a line-number,
a old-string and a new-string, new-string will replace the old-string at the specified line."
                :args (list '(:name "file-path"
                                    :type string
                                    :description "The full path of the file to edit")
                            '(:name "file-edits"
                                    :type array
                                    :items (:type object
                                                  :properties
                                                  (:line_number
                                                   (:type integer :description "The line number of the file where edit starts.")
                                                   :old_string
                                                   (:type string :description "The old-string to be replaced.")
                                                   :new_string
                                                   (:type string :description "The new-string to replace old-string.")))
                                    :description "The list of edits to apply on the file"))
                :category "filesystem"))

  (add-to-list 'gptel-tools
               (gptel-make-tool
                :function (lambda (command &optional working_dir)
                            (with-temp-message (format "Executing command: `%s`" command)
                              (let ((default-directory (if (and working_dir (not (string= working_dir "")))
                                                           (expand-file-name working_dir)
                                                         default-directory)))
                                (shell-command-to-string command))))
                :name "run_command"
                :description "Executes a shell command and returns the output as a string. IMPORTANT: This tool allows execution of arbitrary code; user confirmation will be required before any command is run."
                :args (list
                       '(:name "command"
                               :type string
                               :description "The complete shell command to execute.")
                       '(:name "working_dir"
                               :type string
                               :description "Optional: The directory in which to run the command. Defaults to the current directory if not specified."))
                :category "command"
                :confirm t
                :include t))

  (defun run_async_command (callback command)
    "Run COMMAND asynchronously and pass output to CALLBACK."
    (condition-case error
        (let ((buffer (generate-new-buffer " *async output*")))
          (with-temp-message (format "Running async command: %s" command)
            (async-shell-command command buffer nil))
          (let ((proc (get-buffer-process buffer)))
            (when proc
              (set-process-sentinel
               proc
               (lambda (process _event)
                 (unless (process-live-p process)
                   (with-current-buffer (process-buffer process)
                     (let ((output (buffer-substring-no-properties (point-min) (point-max))))
                       (kill-buffer (current-buffer))
                       (funcall callback output)))))))))
      (t
       ;; Handle any kind of error
       (funcall callback (format "An error occurred: %s" error)))))

  (add-to-list 'gptel-tools
               (gptel-make-tool
                :function #'run_async_command
                :name "run_async_command"
                :description "Run an async command."
                :args (list
                       '(:name "command"
                               :type "string"
                               :description "Command to run."))
                :category "command"
                :async t
                :include t))

  (defun gptel-read-documentation (symbol)
    "Read the documentation for SYMBOL, which can be a function or variable."
    (let ((sym (intern symbol)))
      (cond
       ((fboundp sym)
        (documentation sym))
       ((boundp sym)
        (documentation-property sym 'variable-documentation))
       (t
        (format "No documentation found for %s" symbol)))))

  (add-to-list 'gptel-tools
               (gptel-make-tool
                :name "read_documentation"
                :function #'gptel-read-documentation
                :description "Read the documentation for a given function or variable"
                :args (list '(:name "name"
                                    :type string
                                    :description "The name of the function or variable whose documentation is to be retrieved"))
                :category "emacs"))

  (gptel-make-preset 'gpt4coding
                     :description "A preset optimized for coding tasks"
                     :backend "Copilot"
                     :model 'claude-3.7-sonnet-thought
                     :system "You are an expert coding assistant. Your role is to provide high-quality code solutions, refactorings, and explanations."
                     :tools `(mapcar (lambda (tool) (oref tool name)) gptel-tools))
  (gptel-make-preset 'websearch
                     :description "A preset for quick web searches"
                     :backend "Copilot"
                     :model 'gemini-2.5-pro-preview-06-05
                     :system "You are a web search assistant. Your role is to quickly find and summarize information from the web.")

  (with-eval-after-load 'gptel
    (add-to-list 'gptel-directives '(mentor . "You are a large language model, a seasoned programmer, and a mentor. Provide succinct yet thorough explanations and point out caveats where applicable."))
    (add-to-list 'gptel-directives '(tech-compare . "I want you to act as a software developer tech reviewer. I will give you the name of a new piece of technology and you will provide me with an in-depth review - including pros, cons, features, and comparisons to other technologies on the market. I want you to also research on how overall market feels about the product. Please also outline any alternatives that's better, why it's better, and why it's percieved better by the market."))
    (ra/keymap-set gptel-mode-map
      "C-c C-g" #'gptel-abort)))

(elpaca (llm-tool-collection :host github :repo "skissue/llm-tool-collection")
  (with-eval-after-load 'gptel
    (require 'llm-tool-collection)
    (mapcar (lambda (x) (add-to-list 'gptel-tools (apply #'gptel-make-tool x)))
          (llm-tool-collection-get-all))))

;; (elpaca (codeium :host github :repo "Exafunction/codeium.el")
;;   (setopt codeium/metadata/api_key (funcall (plist-get (car (auth-source-search :max 1
;;                                                                                 :host "windsurf.com"
;;                                                                                 :user "andersonricky@proton.me^api-key"))
;;                                                        :secret))))

;; (elpaca (supermaven :host github :repo "crazywolf132/supermaven.el")
;;   (setopt supermaven-keymaps '((clear-suggestion . "C-]")
;;                                (accept-suggestion . "C-j"))))

(provide '+ai)
;;; +ai.el ends here
