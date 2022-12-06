eval "$(starship init zsh)"

zstyle ':completion:*' matcher-list '' 'm:{a-zA-Z}={A-Za-z}' 'r:|=*' 'l:|=* r:|=*'
autoload -Uz compinit
compinit

source ~/.zplug/init.zsh

zplug "zsh-users/zsh-completions",                defer:0
zplug "zsh-users/zsh-autosuggestions",            defer:1
zplug "zsh-users/zsh-syntax-highlighting",        defer:2
zplug "zsh-users/zsh-history-substring-search",   defer:3

zplug "chrissicool/zsh-256color"
zplug "changyuheng/fz"
zplug "rupa/z", use:z.sh
zplug "zplug/zplug", hook-build:'zplug --self-manage'

if ! zplug check --verbose; then
  printf "Install? [y/N]: "
  if read -q; then
    echo; zplug install
  fi
fi

zplug load

bindkey '^[[A' history-substring-search-up
bindkey '^[[B' history-substring-search-down

eval $(thefuck --alias)

# source /usr/local/share/zsh-you-should-use/you-should-use.plugin.zsh

# Aliases
alias zl="source ~/.zshrc"

alias ls="exa"
alias la="exa -a"
alias ll="exa -la"

alias vim="nvim"

# fnm
export PATH=$HOME/.fnm:$PATH
eval "`fnm env`"
eval "$(fnm env --use-on-cd)"

