#!/bin/bash

# This scrip should set up neovim
if [[ $EUID -ne 0 ]]; then
    echo -e "\n\x1b[31;01m Run this script with 'sudo -E' \x1b[39;49;00m\n"
    exit 1
fi
if [ $HOME  = '/root' ]; then
    echo -e "\n\x1b[31;01m Run this script with 'sudo -E' \x1b[39;49;00m\n"
    exit 1
fi
if [ -z ${SUDO_USER} ]; then
    echo -e "\n\x1b[31;01m No \$SUDO_USER available, quiting ... \x1b[39;49;00m\n"
    exit 1
fi

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
ST_INSTALL_DIR=${HOME}/.local/share/
ST_DIR=${ST_INSTALL_DIR}/st

# git clone and install the 'st' terminal emulator
if [ ! -d "$ST_DIR" ];
then
    apt-get -y install libx11-dev libxft-dev
    sudo -u ${SUDO_USER} mkdir -p $ST_INSTALL_DIR
    cd $ST_INSTALL_DIR
    sudo -u ${SUDO_USER} git clone git://git.suckless.org/st
    cd "$ST_DIR"
    # sudo -u ${SUDO_USER} git checkout -b master
    sudo -u ${SUDO_USER} git checkout 041912a
    sudo -u ${SUDO_USER} git checkout -b custom
    # make clean config.h
    make config.h
    # ignore files created by 'make'
    sudo -u ${SUDO_USER} echo -e "st\n*.o" > .gitignore
    sudo -u ${SUDO_USER} git add --all
    sudo -u ${SUDO_USER} git commit -m 'Copied config, Added gitignore file'
    # build patches on a separate branch
    sudo -u ${SUDO_USER} git checkout master
    sudo -u ${SUDO_USER} git checkout 041912a
    sudo -u ${SUDO_USER} git checkout -b patched
    sudo -u ${SUDO_USER} mkdir -p "$ST_DIR/patches"
    cd "$ST_DIR/patches"
    # sudo -u ${SUDO_USER} curl https://st.suckless.org/patches/scrollback/st-scrollback-0.8.diff -O
    sudo -u ${SUDO_USER} curl https://st.suckless.org/patches/solarized/st-no_bold_colors-0.8.1.diff -O
    sudo -u ${SUDO_USER} curl https://st.suckless.org/patches/solarized/st-solarized-dark-20180411-041912a.diff -O
    cd ..
    # apply 1st patch
    echo -e "\n\x1b[33;01m Applying 1st patch ... \x1b[39;49;00m\n" && sleep 1
    sudo -u ${SUDO_USER} git apply "$ST_DIR/patches/st-no_bold_colors-0.8.1.diff"
    # echo -e "\n\x1b[33;01m \t running make clean config.h ... \x1b[39;49;00m\n" && sleep 1
    make clean config.h
    # echo -e "\n\x1b[33;01m \t running git add --all ... \x1b[39;49;00m\n" && sleep 1
    sudo -u ${SUDO_USER} git add --all
    # echo -e "\n\x1b[33;01m \t commiting changes ... \x1b[39;49;00m\n" && sleep 1
    sudo -u ${SUDO_USER} git commit -m 'patched st-no_bold_colors-0.8.1.diff'
    # echo -e "\n\x1b[33;01m \t rebasing ... \x1b[39;49;00m\n" && sleep 1
    sudo -u ${SUDO_USER} git checkout custom
    sudo -u ${SUDO_USER} git rebase patched
    # apply 2nd patch
    echo -e "\n\x1b[33;01m Applying 2nd patch ... \x1b[39;49;00m\n" && sleep 1
    sudo -u ${SUDO_USER} git checkout patched
    sudo -u ${SUDO_USER} git apply "$ST_DIR/patches/st-solarized-dark-20180411-041912a.diff"
    rm config.h
    make clean config.h
    sudo -u ${SUDO_USER} git add --all
    sudo -u ${SUDO_USER} git commit -m 'patched st-solarized-dark-20180411-041912a.diff'
    sudo -u ${SUDO_USER} git checkout custom
    sudo -u ${SUDO_USER} git rebase patched
    make clean install
    echo -e "\n\x1b[32;01m Built patch ... success ... \x1b[39;49;00m\n"
else
    echo -e "\n\x1b[33;01m Dir $ST_INSTALL_DIR already exits, not installing 'st' \x1b[39;49;00m\n"
fi

# FONTS_SOURCE_DIR="${SCRIPT_DIR}/../fonts"
# FONTS_DEST_DIR="${HOME}/.local/share/fonts"
# # install fonts
# sudo -u ${SUDO_USER} mkdir -p $FONTS_DEST_DIR
# sudo -u ${SUDO_USER} cp -a "${FONTS_SOURCE_DIR}/." $FONTS_DEST_DIR
# echo -e "\n\x1b[33;01m Copying $FONTS_SOURCE_DIR to $FONTS_DEST_DIR ... \x1b[39;49;00m\n"
