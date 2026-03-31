source ~/.config/agnosticrc/agnosticrc

autoload -U select-word-style
select-word-style bash

autoload -Uz compinit bashcompinit && compinit && bashcompinit

# aerolab zsh completion
source /home/kporter/.aerolab.completion.zsh
