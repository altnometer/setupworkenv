#!/bin/bash

# This scrip should set up vim with options in .vimrc
# .vimrc file should be copied to ~/ folder.
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
VIMRC_PATH="${SCRIPT_DIR}/vimrc"
if [ -f $VIMRC_PATH ];
then
    echo -e "\n\x1b[33;01m Linking $VIMRC_PATH to $HOME/.vimrc ... \x1b[39;49;00m\n"
    if [ -f "$HOME/.vimrc" ]; then
        rm $HOME/.vimrc
    fi
    if [ -h "$HOME/.vimrc" ]; then  # -h, true if file exist and a symbolic link.
        rm $HOME/.vimrc
    fi
	sudo -u ${SUDO_USER} ln -s $VIMRC_PATH $HOME/.vimrc
else
    echo -e "\n\x1b[31;01m vimrc does not exist. Quiting ... \x1b[39;49;00m\n"
	exit 1
fi

if [ "$(( $(date +%s) - $(stat -c %Z /var/cache/apt/pkgcache.bin 2>/dev/null || echo '0') ))" -ge 600 ]; then
    echo -e "\n\x1b[33;01m Running apt-get update ... \x1b[39;49;00m\n" && sleep 1
    apt-get update
fi
apt-get install -y vim-nox curl git wget python-pip exuberant-ctags
#pip install pep8
pip install pyflakes
pip install flake8
#pip install pylint
# # Setup Pathogen to manage your plugins
# PATHOGEN_PATH=$HOME/redmoo/auct/setup/vimsetup/pathogen.vim
# sudo -u ${SUDO_USER} mkdir -pv ~/.vim/autoload ~/.vim/bundle
# #sudo -u ${SUDO_USER} curl -LSso ~/.vim/autoload/pathogen.vim https://tpo.pe/pathogen.vim
# sudo -u ${SUDO_USER} cp -f ${PATHOGEN_PATH} ~/.vim/autoload/pathogen.vim
# # Now you can install any plugin into a .vim/bundle/plugin-name/ folder

# Setup vim-plug to manage your plugins
sudo -u ${SUDO_USER} curl -fLo ${HOME}/.vim/autoload/plug.vim --create-dirs \
    https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim

COLOR_SCHEME_SEOUL256_PATH="${SCRIPT_DIR}/seoul256.vim"
echo -e "\n\x1b[33;01m linking $HOME/.vim/colors/seoul256.vim to ${COLOR_SCHEME_SEOUL256_PATH}. \x1b[39;49;00m\n" && sleep 1
sudo -u ${SUDO_USER} mkdir -p ~/.vim/colors
sudo -u ${SUDO_USER} ln -sf ${COLOR_SCHEME_SEOUL256_PATH} $HOME/.vim/colors/seoul256.vim
