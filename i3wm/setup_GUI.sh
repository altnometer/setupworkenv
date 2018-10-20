#!/bin/bash

# This scrip should set up a GUI
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
# run apt-get update
# if [ "$(( $(date +%s) - $(stat -c %Z /var/cache/apt/pkgcache.bin 2>/dev/null || echo '0') ))" -ge 600 ]; then
#     echo -e "\n\x1b[33;01m Running apt-get update ... \x1b[39;49;00m\n" && sleep 1
#     apt-get update
# fi

# install packages
# apt-get install -y vim-nox curl git wgepython-pip exuberant-ctags
# if hash nvim 2>/dev/null; then
#     echo -e "\n\x1b[33;01m neovim is installed, not installing or upgrading. \x1b[39;49;00m\n" && sleep 1
# else
#     echo -e "\n\x1b[33;01m Installing neovim ...  \x1b[39;49;00m\n" && sleep 1
#     # TODO: python-neovim python3-neovim are available only in 'unstable' deb
#     # repository
#     apt-get install -y neovim curl python-neovim python3-neovim
# fi

# link .XResources file
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

# link .Xmodmap file
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

# link .xinitrc file
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

# setup urxvt
URXVT_EXT_DIR=${HOME}/.urxvt/ext
# sudo -u ${SUDO_USER} mkdir -p $URXVT_EXT_DIR
sudo -u ${SUDO_USER} wget -P $URXVT_EXT_DIR https://raw.githubusercontent.com/simmel/urxvt-resize-font/master/resize-font

# setup color theme
COLOR_THEME_DEST=${HOME}/.Xresources.d/colors
sudo -u ${SUDO_USER} wget -P $COLOR_THEME_DEST https://raw.githubusercontent.com/solarized/xresources/master/Xresources.dark
