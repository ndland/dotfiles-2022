# Executes commands at the start of an interactive session.
#
# Authors:
#   Sorin Ionescu <sorin.ionescu@gmail.com>
#

# Source Prezto.
if [[ -s "${ZDOTDIR:-$HOME}/.zprezto/init.zsh" ]]; then
  source "${ZDOTDIR:-$HOME}/.zprezto/init.zsh"
fi

# Customize to your needs...
# zplug
source ~/.zplug/init.zsh

zplug "changyuheng/fz", defer:1
zplug "rupa/z", use:z.sh
zplug "modules/prompt", from:prezto

eval $(thefuck --alias)

alias vim="nvim"

source /usr/local/share/zsh-you-should-use/you-should-use.plugin.zsh

alias vim="nvim"

# fnm
export PATH=/home/nland/.fnm:$PATH
eval "`fnm env`"
