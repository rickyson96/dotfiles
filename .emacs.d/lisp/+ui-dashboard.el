;;; +ui-dashboard.el --- Dashboard setup             -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Ricky Anderson

;; Author: Ricky Anderson <randerson@thinky>
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

(elpaca dashboard
  (setopt dashboard-display-icons-p t
          dashboard-icon-type 'nerd-icons
          dashboard-set-navigator t
          dashboard-navigation-cycle t
          dashboard-filter-agenda-entry #'dashboard-filter-agenda-by-time
          dashboard-week-agenda nil
          dashboard-set-heading-icons t
          dashboard-set-file-icons t)

  (setopt dashboard-navigator-buttons
          `((("󰃭" "Agenda" "Org GTD Agenda" ,(cmd! (org-agenda nil "u")))
             ("󱖟" "Process" "Org GTD Process Inbox" ,(cmd! (org-gtd-process-inbox)))
             ("" "Edit" "Edit gtd inbox" ,(cmd! (find-file (file-name-concat org-directory "gtd/inbox.org" nil)))))
            (("" "Elfeed" "Browse elfeed" ,(cmd! (elfeed)))
             ("" "Edit" "Edit elfeed-org" ,(cmd! (find-file (file-name-concat org-directory "elfeed.org" nil)))))))

  (setopt dashboard-items '((projects . 5)
                            (agenda . 15)))

  (setopt dashboard-startupify-list `( dashboard-insert-banner-title
                                       dashboard-insert-newline
                                       dashboard-insert-navigator
                                       dashboard-insert-newline
                                       dashboard-insert-items
                                       dashboard-insert-newline
                                       dashboard-insert-footer
                                       dashboard-insert-newline
                                       dashboard-insert-init-info
                                       dashboard-insert-newline))


  (add-hook 'elpaca-after-init-hook #'dashboard-insert-startupify-lists)
  (add-hook 'elpaca-after-init-hook #'dashboard-initialize)
  (dashboard-setup-startup-hook)

  (dashboard-modify-heading-icons '((recents . "nf-oct-file")
                                    (bookmarks . "nf-oct-book")))


  (ra/keymap-set dashboard-mode-map
    "n" #'dashboard-next-line
    "p" #'dashboard-previous-line)

  (setopt dashboard-center-content t
          dashboard-vertically-center-content t)

  ;; to work with daemon
  ;; https://github.com/emacs-dashboard/emacs-dashboard?tab=readme-ov-file#emacs-daemon
  (setopt initial-buffer-choice (lambda () (get-buffer-create dashboard-buffer-name)))

  ;; Add more quotes
  (setopt dashboard-footer-messages '("The one true editor, Emacs!"
                                      "Who the hell uses VIM anyway? Go Evil!"
                                      "Free as free speech, free as free Beer" "Happy coding!"
                                      "Vi Vi Vi, the editor of the beast" "Welcome to the church of Emacs"
                                      "While any text editor can save your files, only Emacs can save your soul"
                                      "I showed you my source code, pls respond"
                                      "There's no magic, there's just some f-ing sand, shoved together into silicone, and that's the chip. - DHH"
                                      "You should be your biggest fan, bcs everybody else will \"boo\" you, don't join them! - GaryVee")))

(provide '+ui-dashboard)
;;; +ui-dashboard.el ends here
