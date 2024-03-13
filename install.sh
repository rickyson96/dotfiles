export DEBIAN_FRONTEND=noninteractive
sudo apt-add-repository ppa:ubuntu-elisp/ppa
sudo apt update
sudo apt upgrade -y
sudo apt install -y fish emacs-snapshot ripgrep stow

stow -t "$HOME" . 
