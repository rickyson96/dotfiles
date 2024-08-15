# nix specific locale setting
# taken from: https://gist.github.com/peti/2c818d6cb49b0b0f2fd7c300f8386bc3
if [ -e /home/rickylson/.nix-profile/etc/profile.d/nix.fish ]
    source /home/rickyson/.nix-profile/etc/profile.d/nix.fish
    export LOCALE_ARCHIVE_2_27="$(nix-build --no-out-link "<nixpkgs>" -A glibcLocales)/lib/locale/locale-archive"
    export LOCALE_ARCHIVE="/usr/lib/locale/locale-archive"
    export LOCALE_ARCHIVE_2_11="/etc/locale.gen"
end

# Fundle
if not functions -q fundle; eval (curl -sfL https://git.io/fundle-install); end

fundle plugin 'edc/bass'
fundle plugin 'jorgebucaran/autopair.fish'
fundle plugin 'PatrickF1/fzf.fish'
fundle plugin 'jorgebucaran/nvm.fish'
fundle plugin 'oh-my-fish/plugin-pyenv'
fundle plugin 'reitzig/sdkman-for-fish'

fundle init

fish_add_path --universal ~/bin
fish_add_path --universal ~/.local/bin
fish_add_path --universal ~/go/bin
fish_add_path --universal /Users/rickyson/Library/Application\ Support/Coursier/bin
fish_add_path --universal /usr/local/opt/libpq/bin
fish_add_path --universal /usr/local/opt/openssl@1.1/bin
fish_add_path --universal /Users/rickyson/opensource/kotlin-language-server/server/build/install/server/bin
fish_add_path --universal ~/.cargo/bin
fish_add_path --universal /Users/rickyson/opensource/mermaid-cli/node_modules/.bin

export YDOTOOL_SOCKET=/tmp/.ydotool_socket
export SSH_AGENT_PID=""
export SSH_AUTH_SOCK="$XDG_RUNTIME_DIR/gnupg/S.gpg-agent.ssh"
export LANG='en_US.UTF-8'
export LC_ALL='en_US.UTF-8'
export EDITOR='emacsclient -r'
export LESS="-g -i -M -R -w -z-4"
export BAT_PAGER="less -RF"
export STARSHIP_CONFIG="~/.config/starship/starship.toml"
export LSP_USE_PLISTS=true
export npm_config_prefix="$HOME/.local"
export NODE_VERSION_PREFIX=v
export NODE_VERSIONS={$HOME/.local/share/nvm}
export NODE_OPTIONS="--max-old-space-size=2048"

alias grep=rg
alias ls="exa --icons"

abbr --add rm rmtrash
abbr --add rmdir rmdirtrash

if status is-interactive
    # Commands to run in interactive sessions can go here
    starship init fish | source
    direnv hook fish | source
    atuin init fish | source
end

# Created by `pipx` on 2023-11-23 18:52:18
set PATH $PATH /home/rickyson/.local/bin

if test -e ~/.asdf/asdf.fish
    source ~/.asdf/asdf.fish
    if not test -e ~/.config/fish/completions/asdf.fish
        mkdir -p ~/.config/fish/completions; and ln -s ~/.asdf/completions/asdf.fish ~/.config/fish/completions
    end
end
