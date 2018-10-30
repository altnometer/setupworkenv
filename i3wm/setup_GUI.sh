#!/bin/bash

# This scrip should set up a GUI

# check sudo -E -----------------------------------------------------------{{{
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
# }}}

apt-get install -y xorg xinput firefox-esr i3 suckless-tools conky-all
apt-get install -y silversearcher-ag ripgrep pass

# link .XResources --------------------------------------------------------{{{
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
XRESOURCES_SOURCE="${SCRIPT_DIR}/XResources"
XRESOURCES_DEST="${HOME}/.XResources"
if [ -f $XRESOURCES_SOURCE ];
then
    echo -e "\n\x1b[33;01m Linking $XRESOURCES_SOURCE to $XRESOURCES_DEST ... \x1b[39;49;00m\n"
    if [ -f "$XRESOURCES_DEST" ]; then
        rm $XRESOURCES_DEST
    fi
    if [ -h "$XRESOURCES_DEST" ]; then # -h, true if file exist and a symbolic link.
        rm $XRESOURCES_DEST
    fi
	sudo -u ${SUDO_USER} ln -s $XRESOURCES_SOURCE $XRESOURCES_DEST
else
    echo -e "\n\x1b[31;01m ${XRESOURCES_SOURCE} does not exist. Quiting ... \x1b[39;49;00m\n"
	exit 1
fi
# }}}

# link .Xmodmap -----------------------------------------------------------{{{
XMODMAP_SOURCE="${SCRIPT_DIR}/XResources"
XMODMAP_DEST="${HOME}/.Xresources"
if [ -f $XMODMAP_SOURCE ];
then
    echo -e "\n\x1b[33;01m Linking $XMODMAP_SOURCE to $XMODMAP_DEST ... \x1b[39;49;00m\n"
    if [ -f "$XMODMAP_DEST" ]; then
        rm $XMODMAP_DEST
    fi
    if [ -h "$XMODMAP_DEST" ]; then # -h, true if file exist and a symbolic link.
        rm $XMODMAP_DEST
    fi
	sudo -u ${SUDO_USER} ln -s $XMODMAP_SOURCE $XMODMAP_DEST
else
    echo -e "\n\x1b[31;01m ${XMODMAP_SOURCE} does not exist. Quiting ... \x1b[39;49;00m\n"
	exit 1
fi
# }}}

# link .xinitrc -----------------------------------------------------------{{{
XINIT_SOURCE="${SCRIPT_DIR}/xinitrc"
XINIT_DEST="${HOME}/.xinitrc"
if [ -f $XINIT_SOURCE ];
then
    echo -e "\n\x1b[33;01m Linking $XINIT_SOURCE to $XINIT_DEST ... \x1b[39;49;00m\n"
    if [ -f "$XINIT_DEST" ]; then
        rm $XINIT_DEST
    fi
    if [ -h "$XINIT_DEST" ]; then # -h, true if file exist and a symbolic link.
        rm $XINIT_DEST
    fi
	sudo -u ${SUDO_USER} ln -s $XINIT_SOURCE $XINIT_DEST
else
    echo -e "\n\x1b[31;01m ${XINIT_SOURCE} does not exist. Quiting ... \x1b[39;49;00m\n"
	exit 1
fi
# }}}

# setup color theme
COLOR_THEME_DEST=${HOME}/.Xresources.d/colors
sudo -u ${SUDO_USER} wget -P $COLOR_THEME_DEST https://raw.githubusercontent.com/solarized/xresources/master/Xresources.dark
