;; -*- geiser-scheme-implementation: guile -*-
;; This "home-environment" file can be passed to 'guix home reconfigure'
;; to reproduce the content of your profile.  This is "symbolic": it only
;; specifies package names.  To reproduce the exact same profile, you also
;; need to capture the channels being used, as returned by "guix describe".
;; See the "Replicating Guix" section in the manual.

(use-modules (gnu home)
             (gnu home services)
             (gnu home services shells)
             (gnu home services guix)
             (gnu packages)
             (gnu packages admin)
             (gnu services)
             (guix gexp)
             (guix channels))

(home-environment
  ;; Below is the list of packages that will show up in your
  ;; Home profile, under ~/.guix-home/profile.
 (packages (specifications->packages '("htop" "notify-send")
            ;; '("emacs-native-comp"
            ;;                            "emacs-better-defaults"
            ;;                            "emacs-dash"
            ;;                            "emacs-modus-themes"
            ;;                            "emacs-geiser"
            ;;                            "emacs-geiser-guile"
            ;;                            "emacs-guix"
            ;;                            "emacs-with-editor"
            ;;                            "emacs-pdf-tools")
                                     ))

  ;; Below is the list of Home services.  To search for available
  ;; services, run 'guix home search KEYWORD' in a terminal.
  (services
   (list (simple-service 'variant-packages-service
                home-channels-service-type
                (list
                 (channel
                  (name 'variant-packages)
                  (url "https://github.com/flatwhatson/guix-channel"))))
         (service home-bash-service-type
                  (home-bash-configuration
                   (aliases '(("grep" . "grep --color=auto")
                              ("ls" . "ls --color=auto")))
                   (bashrc (list (local-file
                                  "/home/rickyson/.dotfiles/.bashrc" "bashrc")))
                   (bash-profile (list (local-file
                                        "/home/rickyson/.dotfiles/.bash_profile"
                                        "bash_profile")))
                   (bash-logout (list (local-file
                                       "/home/rickyson/.dotfiles/.bash_logout"
                                       "bash_logout"))))))))
