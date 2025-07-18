#
# ~/.bash_profile
#

[[ -f ~/.bashrc ]] && . ~/.bashrc
[[ -f ~/.profile ]] && . ~/.profile

export SSH_AGENT_PID=""
export SSH_AUTH_SOCK="$XDG_RUNTIME_DIR/gnupg/S.gpg-agent.ssh"
export LSP_USE_PLISTS=true
# export DOCKER_HOST="unix://$XDG_RUNTIME_DIR/podman/podman.sock"

if [ -e /home/rickyson/.nix-profile/etc/profile.d/nix.sh ]; then . /home/rickyson/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer

export GUIX_PROFILE="/home/rickyson/.guix-profile"
source "$GUIX_PROFILE/etc/profile"
source "$HOME/.config/guix/current/etc/profile"

export PATH=$PATH:~/bin
export PATH=$PATH:~/.local/bin
export PATH=$PATH:~/go/bin
export PATH=$PATH:/Users/rickyson/Library/Application\ Support/Coursier/bin
export PATH=$PATH:/usr/local/opt/libpq/bin
export PATH=$PATH:/usr/local/opt/openssl@1.1/bin
export PATH=$PATH:/Users/rickyson/opensource/kotlin-language-server/server/build/install/server/bin
export PATH=$PATH:~/.cargo/bin
export PATH=$PATH:/Users/rickyson/opensource/mermaid-cli/node_modules/.bin

export SDKMAN_DIR="$HOME/.sdkman"
export EDITOR='emacsclient -r'

# pnpm
export PNPM_HOME="/home/randerson/.local/share/pnpm"
case ":$PATH:" in
*":$PNPM_HOME:"*) ;;
*) export PATH="$PNPM_HOME:$PATH" ;;
esac
# pnpm end
