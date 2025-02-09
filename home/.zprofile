# Path to your oh-my-zsh installation.
export ZSH=${HOME}/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="robbyrussell"

# On Mac, activate homebrew. TODO: disable on Linux.
eval "$(/opt/homebrew/bin/brew shellenv)"

# TODO: Load pgsql and rvm in PATH and LD_LIBRARY_PATH only if they exist.

export PATH="/usr/local/pgsql/bin:$HOME/bin:$PATH:$HOME/.rvm/bin" # Add RVM to PATH for scripting
export PATH="$HOME/.local/bin:$PATH" # For Python poetry on Mac  TODO: clean up for Linux
export LD_LIBRARY_PATH=/usr/local/pgsql/lib
source $HOME/.homesick/repos/homeshick/homeshick.sh
export KEYTIMEOUT=1
