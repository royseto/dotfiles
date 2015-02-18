# ZSH configuration file.
# This may be helpful: http://wiki.gentoo.org/wiki/Zsh/HOWTO

# Load tab completion and enable auto-correction.

autoload -U compinit
compinit

setopt correctall

# Load aliases.

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# Set shell prompt.

autoload -U promptinit
promptinit
export PS1='%m:%1~$ '

# Configure shell history.

export HISTSIZE=2000
export HISTFILE="$HOME/.history"
export SAVEHIST=$HISTSIZE

setopt hist_ignore_all_dups

# Ensure ssh-agent is running and has loaded private key id_rsa.  Set
# up env vars to use ssh-agent in this shell.  Run ssh-keygen to
# generate SSH key pair and install keychain utility using "sudo
# apt-get install keychain" before using this.

function ssh-init()
{
    /usr/bin/keychain id_rsa
    [ -z "$HOSTNAME" ] && HOSTNAME=`/bin/uname -n`
    [ -f $HOME/.keychain/$HOSTNAME-sh ] && . $HOME/.keychain/$HOSTNAME-sh
}

ssh-init

export PATH="$PATH:$HOME/.rvm/bin" # Add RVM to PATH for scripting

source $HOME/.homesick/repos/homeshick/homeshick.sh

if [ -f ~/.zshrc.site ]; then
    . ~/.zshrc.site
fi
