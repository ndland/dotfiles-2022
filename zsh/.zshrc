autoload -U compinit promptinit
promptinit
compinit

prompt spaceship

HISTFILE=~/.zsh_history
HISTSIZE=10000
SAVEHIST=10000

export EDITOR="code -rw"

# Vim bindings for zsh
bindkey -v

alias code="code -r"
alias vim="nvim"
alias reload="source ~/.zshrc"
alias zshrc="code ~/.zshrc"
alias vimrc="code ~/.config/nvim/init.vim"
alias ls="exa --color=always"
alias ll="exa --color=always --long --git --header"
alias la="exa --color=always --long --all --git --header"
alias l="exa --color=always --git --header"
alias cat="bat"
alias mkdir="mkdir -p"

# Git aliases
alias grv="git remote -v"
alias gst="git status"
alias gc="git commit"
alias ga="git add"
alias gaa="git add ."
alias ggpush="git push"
alias gd="git diff"
alias gco="git checkout"
alias gcob="git checkout -b"

git config --global commit.template "$HOME/.git_commit_message_template"

# Brew aliases
alias bubc="brew upgrade && brew cask upgrade && brew cleanup"

# Stow alias
alias stow="stow --target=/Users/$USER"

alias df="df -h"

# Function to grep a search term including the header
function lag() {
    la | { head -1; grep $1; }
}

# Function to return a tree of n length || default to 3
function tree() {
    if [ "$1" ]; then
        exa --tree --level=$1 --long --git --header
    else
        exa --tree --level=3 --long --git --header
    fi
}

export PATH=~/.npm-global/bin:$PATH

# 0 -- vanilla completion (abc => abc)
# 1 -- smart case completion (abc => Abc)
# 2 -- word flex completion (abc => A-big-Car)
# 3 -- full flex completion (abc => ABraCadabra)
zstyle ':completion:*' matcher-list '' \
    'm:{a-z\-}={A-Z\_}' \
    'r:[^[:alpha:]]||[[:alpha:]]=** r:|=* m:{a-z\-}={A-Z\_}' \
    'r:|?=** m:{a-z\-}={A-Z\_}'

source /usr/local/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
source /usr/local/share/zsh-history-substring-search/zsh-history-substring-search.zsh

bindkey '^[[A' history-substring-search-up
bindkey '^[[B' history-substring-search-down

function gi() { curl -sLw n https://www.gitignore.io/api/$@ ;}