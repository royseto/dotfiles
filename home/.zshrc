# ZSH configuration file.
# This may be helpful: http://wiki.gentoo.org/wiki/Zsh/HOWTO

# Path to your oh-my-zsh installation.
export ZSH=/home/dev/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="robbyrussell"

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(git)

# User configuration

# export MANPATH="/usr/local/man:$MANPATH"

source $ZSH/oh-my-zsh.sh

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='vim'
# else
#   export EDITOR='mvim'
# fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# ssh
# export SSH_KEY_PATH="~/.ssh/dsa_id"

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"
# Load tab completion and enable auto-correction.

autoload -U compinit
compinit

setopt correctall

# Load aliases.

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# Set shell prompt.

# autoload -U promptinit
# promptinit
# export PS1='%m:%1~$ '

# Configure shell history.

# export HISTSIZE=2000
# export HISTFILE="$HOME/.history"
# export SAVEHIST=$HISTSIZE

# setopt hist_ignore_all_dups

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
