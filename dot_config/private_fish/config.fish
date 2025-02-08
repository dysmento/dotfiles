if status is-interactive
    # Commands to run in interactive sessions can go here
end
source ~/.asdf/asdf.fish

set -x PATH {$HOME}/bin {$HOME}/.local/bin $PATH /opt/homebrew/opt/postgresql@16/bin /opt/homebrew/bin {$HOME}/infra/bin
set -x LC_ALL en_US.UTF-8
starship init fish | source


test -e {$HOME}/.iterm2_shell_integration.fish ; and source {$HOME}/.iterm2_shell_integration.fish
direnv hook fish | source

alias ls='eza'
alias tree='lsd --tree'
