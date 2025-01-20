;;; +toolbox.el --- Toolbox                          -*- lexical-binding: t; -*-

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

(elpaca dwim-shell-command
  (defun ra/sc-log-handpoint-terminal ()
    "Run \"adb logcat | grep App-Detailed-Logger\" on a separate buffer."
    (interactive)
    (dwim-shell-command-on-marked-files
     "Handpoint: App-Detailed-Logger"
     "adb logcat | rg App-Detailed-Logger"
     :utils '("adb" "rg")
     :shell-util "sh"
     :shell-args "-c"
     :focus-now t
     :no-progress t))

  (defun ra/sc-connect-vpn ()
    "Connect to paystone vpn on emacs background instance."
    (interactive)
    (dwim-shell-command-on-marked-files
     "OpenVPN: Paystone"
     "pass show jumpcloud.com/randerson@paystone.com | sudo -S openvpn --config /home/randerson/Downloads/randerson.conf --auth-user-pass (echo -e \"randerson\n$(pass show jumpcloud.com/randerson@paystone.com | head -n1)\" | psub)"
     :utils '("fish" "openvpn" "pass")
     :shell-util "fish"
     :shell-args "-c"
     :no-progress t
     :error-autofocus nil))

  (defun ra/sc-stop-vpn ()
    "Stop vpn."
    (interactive)
    (with-current-buffer (get-buffer "*OpenVPN: Paystone*")
      (comint-interrupt-subjob)
      (kill-buffer)))

  (defun ra/copy-qr-from-clipboard-image ()
    "Copy qr content from clipboard qr image"
    (interactive)
    (dwim-shell-command-on-marked-files
     "Copy QR Content"
     "wl-paste | zbarimg PNG:- | cut -d: -f2- | wl-copy"
     :utils '("fish" "zbarimg" "wl-paste" "wl-copy")
     :shell-util "fish"
     :shell-args "-c"
     :error-autofocus nil))

  (defun ra/convert-video-to-mp4 ()
    "Convert video to mp4 using ffmpeg."
    (interactive)
    (dwim-shell-command-on-marked-files
     "Convert Video to mp4"
     "ffmpeg -i '<<f>>' '<<fne>>.mp4'"
     :utils '("ffmpeg")))

  (defun ra/dragon ()
    "run `dragon' on current file"
    (interactive)
    (dwim-shell-command-on-marked-files
     "Drag and Drop on Files"
     "dragon '<<*>>'"
     :utils '("dragon")))

  (autoload #'dwim-shell-command "dwim-shell-command")
  (autoload #'dwim-shell-command-on-marked-files "dwim-shell-command"))

(elpaca (emacs-everywhere :host github :repo "tecosaur/emacs-everywhere"
                          :remotes ("wayland" :repo "zauster/emacs-everywhere")))

(defun ra/parse-date-from-unix (time &optional zone)
  "Parse time from unix to human readable format. `universal-argument' to define zone"
  (interactive (list (read-number "Unix Millis: ")
                     (when current-prefix-arg (read-string "Zone: " "UTC" nil '("UTC-7" "Asia/Jakarta")))))
  (when (> (/ time (expt 10 10)) 0)
    (setq time (/ time 1000)))
  (let ((result (format-time-string "%Y-%m-%d %H:%M:%S %z" time zone)))
    (kill-new result)
    (message result)))

(provide '+toolbox)
;;; +toolbox.el ends here
