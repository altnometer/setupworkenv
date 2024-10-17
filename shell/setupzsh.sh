#!/bin/bash

set -e

# cd to the folder before running
#https://github.com/altercation/solarized

# check if the script is run with 'sudo -E'
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

SCRIPT_DIR_OLD=$SCRIPT_DIR
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
ZSH_OH_MY_ZSH_DIR="${HOME}/.oh-my-zsh"
ZSH_CUSTOM_PLUG_DIR="${ZSH_OH_MY_ZSH_DIR}/custom/plugins"
BACKUP_DIR="${HOME}/backup"

if hash zsh 2>/dev/null;
then
    echo -e "\n\x1b[33;01m zsh is installed, not installing or upgrading. \x1b[39;49;00m\n" && sleep 1
else
   echo -e "\n\x1b[33;01m installing zsh ... \x1b[39;49;00m\n" && sleep 1

    apt-get install -y zsh curl
    chsh --shell $(which zsh) ${SUDO_USER}
    echo -e "\n\x1b[33;01m installing oh-my-zsh ... \x1b[39;49;00m\n" && sleep 1
    sudo -u ${SUDO_USER} rm -rf ${ZSH_OH_MY_ZSH_DIR}
    sudo -u ${SUDO_USER} sh -c "$(curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"
fi

if [ ! -d "${ZSH_CUSTOM_PLUG_DIR}/zsh-autosuggestions" ];
then
    sudo -u ${SUDO_USER} mkdir -p $ZSH_CUSTOM_PLUG_DIR
    sudo -u ${SUDO_USER} git clone \
         https://github.com/zsh-users/zsh-autosuggestions ${ZSH_CUSTOM_PLUG_DIR}/zsh-autosuggestions
fi

if [ ! -d "${ZSH_CUSTOM_PLUG_DIR}/zsh-syntax-highlighting" ];
then
    sudo -u ${SUDO_USER} mkdir -p $ZSH_CUSTOM_PLUG_DIR
    sudo -u ${SUDO_USER} git clone \
         https://github.com/zsh-users/zsh-syntax-highlighting.git ${ZSH_CUSTOM_PLUG_DIR}/zsh-syntax-highlighting
fi
if [ ! -d "${ZSH_CUSTOM_PLUG_DIR}/zsh-completions" ];
then
    sudo -u ${SUDO_USER} mkdir -p $ZSH_CUSTOM_PLUG_DIR
    sudo -u ${SUDO_USER} git clone \
         https://github.com/zsh-users/zsh-completions.git ${ZSH_CUSTOM_PLUG_DIR}/zsh-completions
fi

## install oh-my-zsh

# if [ -z "$DESKTOP_SESSION" -a "$DESKTOP_SESSION" == "lightdm-xsession" ];
# then
#     echo -e "$COL_GREEN seting up color scheme for xfce4 $COL_RESET"
#     file_conf_orig="${SCRIPT_DIR}/terminalrc"
#     dir_conf_dest="${HOME}/.config/Terminal"
#     file_conf_dest="${dir_conf_dest}/terminalrc"
#     sudo -u ${SUDO_USER} mkdir -pv "${dir_conf_dest}"
#     sudo -u ${SUDO_USER} cp "$file_conf_orig"  "$file_conf_dest"
# fi
ZSH_PRE_OH_MY_ZSH_SOURCE=${SCRIPT_DIR}/zshrc.pre-oh-my-zsh
ZSH_PRE_OH_MY_ZSH_DEST=${HOME}/.zshrc.pre-oh-my-zsh
echo -e "\n\x1b[33;01m linking .zshrc and .profile \x1b[39;49;00m\n" && sleep 1
sudo -u ${SUDO_USER} ln -fs ${SCRIPT_DIR}/zshrc ${HOME}/.zshrc
sudo -u ${SUDO_USER} ln -fs ${SCRIPT_DIR}/profile ${HOME}/.profile
echo -e "\n\x1b[33;01m linking ${ZSH_PRE_OH_MY_ZSH_SOURCE} to ${ZSH_PRE_OH_MY_ZSH_DEST}\x1b[39;49;00m\n" && sleep 1
sudo -u ${SUDO_USER} ln -fs ${ZSH_PRE_OH_MY_ZSH_SOURCE} ${ZSH_PRE_OH_MY_ZSH_DEST}
ZSH_CUSTOM_THEME_SOURCE=${SCRIPT_DIR}/my256.zsh-theme
ZSH_CUSTOM_THEME_DEST=${HOME}/.oh-my-zsh/custom/themes
sudo -u ${SUDO_USER} mkdir -p $ZSH_CUSTOM_THEME_DEST
echo -e "\n\x1b[33;01m linking ${ZSH_CUSTOM_THEME_SOURCE} to ${ZSH_CUSTOM_THEME_DEST}\x1b[39;49;00m\n" && sleep 1
sudo -u ${SUDO_USER} ln -fs ${ZSH_CUSTOM_THEME_SOURCE} ${ZSH_CUSTOM_THEME_DEST}

ZSH_HISTORY_SOURCE=${BACKUP_DIR}/zsh_history
ZSH_HISTORY_DEST=${HOME}/.zsh_history
sudo -u ${SUDO_USER} rm -f $ZSH_HISTORY_DEST
echo -e "\n\x1b[33;01m linking ${ZSH_HISTORY_SOURCE} to ${ZSH_HISTORY_DEST}\x1b[39;49;00m\n" && sleep 1
sudo -u ${SUDO_USER} ln -fs ${ZSH_HISTORY_SOURCE} ${ZSH_HISTORY_DEST}


SCRIPT_DIR=$SCRIPT_DIR_OLD
