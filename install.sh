export DEBIAN_FRONTEND=noninteractive
sudo apt update
sudo apt upgrade -y
sudo apt install -y fish emacs ripgrep stow

echo "STOWING"
ls -la .
stow -v 3 -t "$HOME" . 
