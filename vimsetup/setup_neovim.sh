#!/bin/bash
set -e

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
# run apt-get update
# if [ "$(( $(date +%s) - $(stat -c %Z /var/cache/apt/pkgcache.bin 2>/dev/null || echo '0') ))" -ge 600 ]; then
#     echo -e "\n\x1b[33;01m Running apt-get update ... \x1b[39;49;00m\n" && sleep 1
#     apt-get update
# fi

# setup neovim ------------------------------------------------------------{{{
# install packages
# apt-get install -y vim-nox curl git wgepython-pip exuberant-ctags
# NVIM_INST_DIR="${HOME}/.local/share/nvim"
NVIM_INST_DIR="${HOME}/Repos/neovim"
# NVIM_EXEC_FILE="${NVIM_INST_DIR}/nvim.appimage"
NVIM_EXEC_FILE_SOURCE="${NVIM_INST_DIR}/nvim"
NVIM_EXEC_FILE_DEST="/usr/local/bin/nvim"
if hash nvim 2>/dev/null; then
    echo -e "\n\x1b[33;01m neovim is installed, not installing or upgrading. \x1b[39;49;00m\n" && sleep 1
else
    echo -e "\n\x1b[33;01m Installing neovim ...  \x1b[39;49;00m\n" && sleep 1
    apt-get install -y curl python-neovim python3-neovim silversearcher-ag ripgrep
    # apt-get install -y fuse
    apt-get install -y exuberant-ctags
    apt-get install -y ninja-build gettext libtool libtool-bin autoconf automake cmake g++ pkg-config unzip
    # sudo -u ${SUDO_USER} mkdir -p $NVIM_INST_DIR
    # sudo -u ${SUDO_USER} curl -Lo $NVIM_EXEC_FILE_SOURCE https://github.com/neovim/neovim/releases/download/nightly/nvim.appimage
    if [ ! -d "$NVIM_INST_DIR" ] ; then
        sudo -u ${SUDO_USER} git clone https://github.com/neovim/neovim $NVIM_INST_DIR
    fi
    cd $NVIM_INST_DIR
    sudo -u ${SUDO_USER} git checkout v0.3.1
    # sudo -u ${SUDO_USER} make
    make install
    # chmod u+x $NVIM_EXEC_FILE_SOURCE
    # rm -f $NVIM_EXEC_FILE_DEST
    # ln -s $NVIM_EXEC_FILE_SOURCE $NVIM_EXEC_FILE_DEST
fi
# }}}

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
VIMRC_PATH="${SCRIPT_DIR}/init.vim"
VIMRC_DEST_DIR="${HOME}/.config/nvim"
VIMRC_DEST="${VIMRC_DEST_DIR}/init.vim"
echo -e "\n\x1b[33;01m Creating $VIMRC_DEST_DIR dir ... \x1b[39;49;00m\n"
sudo -u ${SUDO_USER} mkdir -p $VIMRC_DEST_DIR
if [ ! -f ~/.local/share/nvim/site/autoload/plug.vim  ];
then
    # Setup vim-plug to manage your plugins
    sudo -u ${SUDO_USER} curl -fLo ${HOME}/.local/share/nvim/site/autoload/plug.vim --create-dirs \
        https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
fi
if [ -f $VIMRC_PATH ];
then
    echo -e "\n\x1b[33;01m Linking $VIMRC_PATH to $VIMRC_DEST ... \x1b[39;49;00m\n"
    if [ -f "$VIMRC_DEST" ]; then
        rm $VIMRC_DEST
    fi
    if [ -h "$VIMRC_DEST" ]; then # -h, true if file exist and a symbolic link.
        rm $VIMRC_DEST
    fi
	sudo -u ${SUDO_USER} ln -s $VIMRC_PATH $VIMRC_DEST
else
    echo -e "\n\x1b[31;01m vimrc does not exist. Quiting ... \x1b[39;49;00m\n"
	exit 1
fi

