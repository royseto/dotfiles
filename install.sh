git clone https://github.com/andsens/homeshick.git $HOME/.homesick/repos/homeshick && \
    source $HOME/.homesick/repos/homeshick/homeshick.sh && \
    homeshick -b clone https://github.com/royseto/dotfiles.git && \
    homeshick -f link && \
    source $HOME/.zshrc
