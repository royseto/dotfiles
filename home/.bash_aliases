alias h='history | tail -80 | less'
alias l='ls -CF --color'
alias ls='ls --color'
alias ll='ls -l --color'
alias lt='ls -lt|head'
alias p='parallel'
alias k=kubectl

if [ -f ~/.bash_aliases.site ]; then
    . ~/.bash_aliases.site
fi
