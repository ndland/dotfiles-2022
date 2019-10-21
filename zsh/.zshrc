autoload -U compinit promptinit
promptinit
compinit

prompt spaceship

HISTFILE=~/.zsh_history
HISTSIZE=10000
SAVEHIST=10000

# Vim bindings for zsh
bindkey -v

alias code="code -r"
alias vim="nvim"
alias reload="source ~/.zshrc"
alias zshrc="code ~/.zshrc"
alias vimrc="code ~/.config/nvim/init.vim"
alias ls="exa"
alias ll="exa --long --git --header"
alias la="exa --long --all --git --header"
alias l="exa --git --header"
alias tree="exa --tree --level=3 --long --git --header"
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

# Brew aliases
alias bubc="brew upgrade && brew cask upgrade && brew cleanup"

# Stow alias
alias stow="stow --target=/Users/nland"

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
